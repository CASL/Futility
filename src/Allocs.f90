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
!> @brief Utility module for dynamic memory allocation.
!>
!> The point of this module is to assist in tracking memory usage for
!> dynamically allocated pointer arrays and allocatable arrays used in the
!> program. The module provides 6 interfaces to several subroutines for the 
!> programmer to use elsewhere in the code. The interfaces are intended to
!> reduce the burden on programmer of writing different allocation statements
!> for variables of different types, and to simplify the syntax for variables
!> of any rank. Variables allocated by this module are initialized to zero
!> (or false).
!>
!> The @ref Allocs::dmallocP "dmallocP" interface is used for allocating pointers
!> from index 1 to \e n for all dimensions (e.g. 1 to 10). The dmalloc0P
!> interface is used to allocate from any starting number to any ending number
!> (e.g. 0 to 9 or -4 to 2). Similarly the @ref Allocs::dmallocA "dmallocA" and
!> dmalloc0A are used for @c ALLOCATABLE arrays. The demallocP and demallocA
!> can be used to deallocate the variable.
!>
!> This module provides support for all intrinsic kinds defined by @ref IntrType
!> "IntrType". The generic interfaces can be expanded in other modules by using
!> creating another generic interface within the other module.
!>
!> This module is tested using @c testAllocs.f90. An example of how to use this
!> module is given below, the unit test also shows how to use the module.
!> Code coverage documentation can be found on the @ref CodeCoverageReports 
!> page.
!>
!> @note Regarding parallelism, this module is also not thread safe. Although, 
!> typically memory allocation of array variables does not need to be done 
!> in parallel. Regarding usage within distributed memory programs 
!> (e.g. MPI based programs), @ref Allocs::Alloc_nbytes "Alloc_nbytes" will be
!> tracked independently by each process. So, if one wants to know the total
!> memory used on all processes they will need to perform a REDUCE operation
!> collecting the sum of all values of @ref Allocs::Alloc_nbytes "Alloc_nbytes".
!>
!> @par Module Dependencies
!>  - @ref IntrType "IntrType": @copybrief IntrType
!>  - @ref ExceptionHandler "ExceptionHandler": @copybrief ExceptionHandler
!>
!> @par EXAMPLES
!> @code
!> PROGRAM ExampleAllocs
!> 
!>   USE IntrType
!>   USE Allocs
!>   IMPLICIT NONE
!>
!>   REAL(SRK),POINTER :: myVector(:)
!>   INTEGER(SIK),ALLOCATABLE :: myMatrix(:,:)
!>
!>   WRITE(*,*) ALLOC_MEMSTRING_LENGTH
!> 
!>   CALL eAllocs%setStopOnError(.FALSE.)
!>
!>   CALL dmallocA(myMatrix,500,500)
!>   CALL dmallocP(myVector,500)
!>   WRITE(*,*) Alloc_bytes
!>   WRITE(*,*) 'Using '//TRIM(getMemUsageChar())//' of memory.'
!>   WRITE(*,*) 'myVector lower bound is ',LBOUND(myVector)
!>   WRITE(*,*) 'myVector upper bound is ',UBOUND(myVector)
!>   CALL demalloc(myVector)
!>
!>   CALL dmalloc0P(myVector,0,499)
!>   WRITE(*,*) Alloc_bytes
!>   WRITE(*,*) 'Using '//TRIM(getMemUsageChar())//' of memory.'
!>   WRITE(*,*) 'Lower bound is ',LBOUND(myVector)
!>   WRITE(*,*) 'Upper bound is ',UBOUND(myVector)
!>   CALL demalloc(myVector)
!>
!> END PROGRAM
!> @endcode
!>
!> @author Brendan Kochunas
!>   @date 03/19/2009
!>
!> @par Revisions:
!> (01/17/2011) - Brendan Kochunas
!>   - Added documentation for doxygen. Set private public access to variables
!>     and routines. Added module error handling and support for up to rank 7
!>     arrays of all data types.
!> @par
!> (04/26/2011) - Brendan Kochunas
!>   - Added explicit routines for INTEGER(4) and INTEGER(8).
!>   - Changed the naming of public members to include "Allocs" to avoid 
!>     conflicts in global name space.
!>   - Added @ref Allocs::demalloc "demalloc" interface for deallocating pointer
!>     arrays.
!>   - Changed query methods from subroutines to functions and added enumerated
!>     data types for return string lengths to list of public members.
!>   - Put under test.
!> @par
!> (06/22/2011) - Brendan Kochunas
!>   - Removed dependency on ErrorMod module, and replaced with ExceptionHandler
!>   - Removed SetAllocsErrorMode, getAllcosErrorCode, getAllocsErrorMessage
!>   - Removed private variables errFileUnit, ALLOC_ERRMESG_LENGTH, errorMode,
!>     and errorCode. alloc_mesg moved to AllocsError
!>   - Updated documentation and format.
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE Allocs

  USE IntrType
  USE ExceptionHandler
     
  IMPLICIT NONE
  PRIVATE !Default private for module contents
!
! List of Public items
  PUBLIC :: dmallocP
  PUBLIC :: dmallocA
  PUBLIC :: dmalloc0P
  PUBLIC :: dmalloc0A
  PUBLIC :: demallocP
  PUBLIC :: demallocA
  PUBLIC :: getMemUsageChar
  PUBLIC :: getMemUsage
  PUBLIC :: Alloc_nbytes
  PUBLIC :: ALLOC_MEMSTRING_LENGTH
  PUBLIC :: eAllocs
!
! Variables
  !> Maximum length of character string returned by 
  !> @ref Allocs::getMemUsageChar "getMemUsageChar"
  INTEGER(SIK),PARAMETER :: ALLOC_MEMSTRING_LENGTH=14_SIK
  !> Global variable for tracking amount of memory allocated by module.
  REAL(SRK),SAVE :: Alloc_nbytes=0.0_SRK
  !> Exception handler for module
  TYPE(ExceptionHandlerType),SAVE :: eAllocs
!
! Module Interfaces
  !> @brief Generic interface for returning the memory usage as a
  !> formatted string.
  INTERFACE getMemUsageChar
    !> @copybrief Allocs::getMemUsageChar_default
    !> @copydetails Allocs::getMemUsageChar_default
    MODULE PROCEDURE getMemUsageChar_default
    !> @copybrief Allocs::getMemUsageChar_bytes
    !> @copydetails Allocs::getMemUsageChar_bytes
    MODULE PROCEDURE getMemUsageChar_bytes
  ENDINTERFACE
!
  !> @brief Generic interface for allocating a pointer array from 1 to \e n
  !>
  !> Specifically, it allocates a pointer array of any rank 1 - 7 from 1 to 
  !> \e ndim for each dimension of the array for single precision, double
  !> precision, integer, and logical data types.
  !>
  !> The suffix s,d,i,l refers to data type: single precision, double 
  !> precision, integer, logical. The number refers to the rank.
  INTERFACE dmallocP
    !> @copybrief Allocs::malloc_ps1 @copydetails Allocs::malloc_ps1
    MODULE PROCEDURE malloc_ps1
    !> @copybrief Allocs::malloc_pd1 @copydetails Allocs::malloc_pd1
    MODULE PROCEDURE malloc_pd1
    !> @copybrief Allocs::malloc_pi1 @copydetails Allocs::malloc_pi1
    MODULE PROCEDURE malloc_pi1
    !> @copybrief Allocs::malloc_pl1 @copydetails Allocs::malloc_pl1
    MODULE PROCEDURE malloc_pl1
    !> @copybrief Allocs::malloc_pb1 @copydetails Allocs::malloc_pb1
    MODULE PROCEDURE malloc_pb1
    !> @copybrief Allocs::malloc_ps2 @copydetails Allocs::malloc_ps2
    MODULE PROCEDURE malloc_ps2
    !> @copybrief Allocs::malloc_pd2 @copydetails Allocs::malloc_pd2
    MODULE PROCEDURE malloc_pd2
    !> @copybrief Allocs::malloc_pi2 @copydetails Allocs::malloc_pi2
    MODULE PROCEDURE malloc_pi2
    !> @copybrief Allocs::malloc_pl2 @copydetails Allocs::malloc_pl2
    MODULE PROCEDURE malloc_pl2
    !> @copybrief Allocs::malloc_pb2 @copydetails Allocs::malloc_pb2
    MODULE PROCEDURE malloc_pb2
    !> @copybrief Allocs::malloc_ps3 @copydetails Allocs::malloc_ps3
    MODULE PROCEDURE malloc_ps3
    !> @copybrief Allocs::malloc_pd3 @copydetails Allocs::malloc_pd3
    MODULE PROCEDURE malloc_pd3
    !> @copybrief Allocs::malloc_pi3 @copydetails Allocs::malloc_pi3
    MODULE PROCEDURE malloc_pi3
    !> @copybrief Allocs::malloc_pl3 @copydetails Allocs::malloc_pl3
    MODULE PROCEDURE malloc_pl3
    !> @copybrief Allocs::malloc_pb3 @copydetails Allocs::malloc_pb3
    MODULE PROCEDURE malloc_pb3
    !> @copybrief Allocs::malloc_ps4 @copydetails Allocs::malloc_ps4
    MODULE PROCEDURE malloc_ps4
    !> @copybrief Allocs::malloc_pd4 @copydetails Allocs::malloc_pd4
    MODULE PROCEDURE malloc_pd4
    !> @copybrief Allocs::malloc_pi4 @copydetails Allocs::malloc_pi4
    MODULE PROCEDURE malloc_pi4
    !> @copybrief Allocs::malloc_pl4 @copydetails Allocs::malloc_pl4
    MODULE PROCEDURE malloc_pl4
    !> @copybrief Allocs::malloc_pb4 @copydetails Allocs::malloc_pb4
    MODULE PROCEDURE malloc_pb4
    !> @copybrief Allocs::malloc_ps5 @copydetails Allocs::malloc_ps5
    MODULE PROCEDURE malloc_ps5
    !> @copybrief Allocs::malloc_pd5 @copydetails Allocs::malloc_pd5
    MODULE PROCEDURE malloc_pd5
    !> @copybrief Allocs::malloc_pi5 @copydetails Allocs::malloc_pi5
    MODULE PROCEDURE malloc_pi5
    !> @copybrief Allocs::malloc_pl5 @copydetails Allocs::malloc_pl5
    MODULE PROCEDURE malloc_pl5
    !> @copybrief Allocs::malloc_pb5 @copydetails Allocs::malloc_pb5
    MODULE PROCEDURE malloc_pb5
    !> @copybrief Allocs::malloc_ps6 @copydetails Allocs::malloc_ps6
    MODULE PROCEDURE malloc_ps6
    !> @copybrief Allocs::malloc_pd6 @copydetails Allocs::malloc_pd6
    MODULE PROCEDURE malloc_pd6
    !> @copybrief Allocs::malloc_pi6 @copydetails Allocs::malloc_pi6
    MODULE PROCEDURE malloc_pi6
    !> @copybrief Allocs::malloc_pl6 @copydetails Allocs::malloc_pl6
    MODULE PROCEDURE malloc_pl6
    !> @copybrief Allocs::malloc_pb6 @copydetails Allocs::malloc_pb6
    MODULE PROCEDURE malloc_pb6
    !> @copybrief Allocs::malloc_ps7 @copydetails Allocs::malloc_ps7
    MODULE PROCEDURE malloc_ps7
    !> @copybrief Allocs::malloc_pd7 @copydetails Allocs::malloc_pd7
    MODULE PROCEDURE malloc_pd7
    !> @copybrief Allocs::malloc_pi7 @copydetails Allocs::malloc_pi7
    MODULE PROCEDURE malloc_pi7
    !> @copybrief Allocs::malloc_pl7 @copydetails Allocs::malloc_pl7
    MODULE PROCEDURE malloc_pl7
    !> @copybrief Allocs::malloc_pb7 @copydetails Allocs::malloc_pb7
    MODULE PROCEDURE malloc_pb7
  ENDINTERFACE
!
  !> @brief Generic interface for allocating an array from 1 to \e n
  !>
  !> Specifically, it allocates an array of any rank 1 - 7 from 1 to 
  !> \e ndim for each dimension of the array for single precision, double
  !> precision, integer, and logical data types.
  !>
  !> The suffix s,d,i,l refers to data type: single precision, double 
  !> precision, integer, logical. The number refers to the rank.
  INTERFACE dmallocA
    !> @copybrief Allocs::malloc_as1 @copydetails Allocs::malloc_as1
    MODULE PROCEDURE malloc_as1
    !> @copybrief Allocs::malloc_ad1 @copydetails Allocs::malloc_ad1
    MODULE PROCEDURE malloc_ad1
    !> @copybrief Allocs::malloc_ai1 @copydetails Allocs::malloc_ai1
    MODULE PROCEDURE malloc_ai1
    !> @copybrief Allocs::malloc_al1 @copydetails Allocs::malloc_al1
    MODULE PROCEDURE malloc_al1
    !> @copybrief Allocs::malloc_ab1 @copydetails Allocs::malloc_ab1
    MODULE PROCEDURE malloc_ab1
    !> @copybrief Allocs::malloc_as2 @copydetails Allocs::malloc_as2
    MODULE PROCEDURE malloc_as2
    !> @copybrief Allocs::malloc_ad2 @copydetails Allocs::malloc_ad2
    MODULE PROCEDURE malloc_ad2
    !> @copybrief Allocs::malloc_ai2 @copydetails Allocs::malloc_ai2
    MODULE PROCEDURE malloc_ai2
    !> @copybrief Allocs::malloc_al2 @copydetails Allocs::malloc_al2
    MODULE PROCEDURE malloc_al2
    !> @copybrief Allocs::malloc_ab2 @copydetails Allocs::malloc_ab2
    MODULE PROCEDURE malloc_ab2
    !> @copybrief Allocs::malloc_as3 @copydetails Allocs::malloc_as3
    MODULE PROCEDURE malloc_as3
    !> @copybrief Allocs::malloc_ad3 @copydetails Allocs::malloc_ad3
    MODULE PROCEDURE malloc_ad3
    !> @copybrief Allocs::malloc_ai3 @copydetails Allocs::malloc_ai3
    MODULE PROCEDURE malloc_ai3
    !> @copybrief Allocs::malloc_al3 @copydetails Allocs::malloc_al3
    MODULE PROCEDURE malloc_al3
    !> @copybrief Allocs::malloc_ab3 @copydetails Allocs::malloc_ab3
    MODULE PROCEDURE malloc_ab3
    !> @copybrief Allocs::malloc_as4 @copydetails Allocs::malloc_as4
    MODULE PROCEDURE malloc_as4
    !> @copybrief Allocs::malloc_ad4 @copydetails Allocs::malloc_ad4
    MODULE PROCEDURE malloc_ad4
    !> @copybrief Allocs::malloc_ai4 @copydetails Allocs::malloc_ai4
    MODULE PROCEDURE malloc_ai4
    !> @copybrief Allocs::malloc_al4 @copydetails Allocs::malloc_al4
    MODULE PROCEDURE malloc_al4
    !> @copybrief Allocs::malloc_ab4 @copydetails Allocs::malloc_ab4
    MODULE PROCEDURE malloc_ab4
    !> @copybrief Allocs::malloc_as5 @copydetails Allocs::malloc_as5
    MODULE PROCEDURE malloc_as5
    !> @copybrief Allocs::malloc_ad5 @copydetails Allocs::malloc_ad5
    MODULE PROCEDURE malloc_ad5
    !> @copybrief Allocs::malloc_ai5 @copydetails Allocs::malloc_ai5
    MODULE PROCEDURE malloc_ai5
    !> @copybrief Allocs::malloc_al5 @copydetails Allocs::malloc_al5
    MODULE PROCEDURE malloc_al5
    !> @copybrief Allocs::malloc_ab5 @copydetails Allocs::malloc_ab5
    MODULE PROCEDURE malloc_ab5
    !> @copybrief Allocs::malloc_as6 @copydetails Allocs::malloc_as6
    MODULE PROCEDURE malloc_as6
    !> @copybrief Allocs::malloc_ad6 @copydetails Allocs::malloc_ad6
    MODULE PROCEDURE malloc_ad6
    !> @copybrief Allocs::malloc_ai6 @copydetails Allocs::malloc_ai6
    MODULE PROCEDURE malloc_ai6
    !> @copybrief Allocs::malloc_al6 @copydetails Allocs::malloc_al6
    MODULE PROCEDURE malloc_al6
    !> @copybrief Allocs::malloc_ab6 @copydetails Allocs::malloc_ab6
    MODULE PROCEDURE malloc_ab6
    !> @copybrief Allocs::malloc_as7 @copydetails Allocs::malloc_as7
    MODULE PROCEDURE malloc_as7
    !> @copybrief Allocs::malloc_ad7 @copydetails Allocs::malloc_ad7
    MODULE PROCEDURE malloc_ad7
    !> @copybrief Allocs::malloc_ai7 @copydetails Allocs::malloc_ai7
    MODULE PROCEDURE malloc_ai7
    !> @copybrief Allocs::malloc_al7 @copydetails Allocs::malloc_al7
    MODULE PROCEDURE malloc_al7
    !> @copybrief Allocs::malloc_ab7 @copydetails Allocs::malloc_ab7
    MODULE PROCEDURE malloc_ab7
  ENDINTERFACE
!
  !> @brief Generic interface for allocating a pointer array from \e n to \e m
  !>
  !>Specifically, it allocates a pointer array of any rank 1 - 7 from \e nbeg to \e nend 
  !>for each dimension of the array for single precision, double precision, 
  !>integer, and logical data types.
  !>
  !>The suffix s,d,i,l refers to data type: single precision, double precision,
  !>integer, logical. The number refers to the rank.
  INTERFACE dmalloc0P
    !> @copybrief Allocs::malloc_ps01 @copydetails Allocs::malloc_ps01
    MODULE PROCEDURE malloc_ps01
    !> @copybrief Allocs::malloc_pd01 @copydetails Allocs::malloc_pd01
    MODULE PROCEDURE malloc_pd01
    !> @copybrief Allocs::malloc_pi01 @copydetails Allocs::malloc_pi01
    MODULE PROCEDURE malloc_pi01
    !> @copybrief Allocs::malloc_pl01 @copydetails Allocs::malloc_pl01
    MODULE PROCEDURE malloc_pl01
    !> @copybrief Allocs::malloc_pb01 @copydetails Allocs::malloc_pb01
    MODULE PROCEDURE malloc_pb01
    !> @copybrief Allocs::malloc_ps02 @copydetails Allocs::malloc_ps02
    MODULE PROCEDURE malloc_ps02
    !> @copybrief Allocs::malloc_pd02 @copydetails Allocs::malloc_pd02
    MODULE PROCEDURE malloc_pd02
    !> @copybrief Allocs::malloc_pi02 @copydetails Allocs::malloc_pi02
    MODULE PROCEDURE malloc_pi02
    !> @copybrief Allocs::malloc_pl02 @copydetails Allocs::malloc_pl02
    MODULE PROCEDURE malloc_pl02
    !> @copybrief Allocs::malloc_pb02 @copydetails Allocs::malloc_pb02
    MODULE PROCEDURE malloc_pb02
    !> @copybrief Allocs::malloc_ps03 @copydetails Allocs::malloc_ps03
    MODULE PROCEDURE malloc_ps03
    !> @copybrief Allocs::malloc_pd03 @copydetails Allocs::malloc_pd03
    MODULE PROCEDURE malloc_pd03
    !> @copybrief Allocs::malloc_pi03 @copydetails Allocs::malloc_pi03
    MODULE PROCEDURE malloc_pi03
    !> @copybrief Allocs::malloc_pl03 @copydetails Allocs::malloc_pl03
    MODULE PROCEDURE malloc_pl03
    !> @copybrief Allocs::malloc_pb03 @copydetails Allocs::malloc_pb03
    MODULE PROCEDURE malloc_pb03
    !> @copybrief Allocs::malloc_ps04 @copydetails Allocs::malloc_ps04
    MODULE PROCEDURE malloc_ps04
    !> @copybrief Allocs::malloc_pd04 @copydetails Allocs::malloc_pd04
    MODULE PROCEDURE malloc_pd04
    !> @copybrief Allocs::malloc_pi04 @copydetails Allocs::malloc_pi04
    MODULE PROCEDURE malloc_pi04
    !> @copybrief Allocs::malloc_pl04 @copydetails Allocs::malloc_pl04
    MODULE PROCEDURE malloc_pl04
    !> @copybrief Allocs::malloc_pb04 @copydetails Allocs::malloc_pb04
    MODULE PROCEDURE malloc_pb04
    !> @copybrief Allocs::malloc_ps05 @copydetails Allocs::malloc_ps05
    MODULE PROCEDURE malloc_ps05
    !> @copybrief Allocs::malloc_pd05 @copydetails Allocs::malloc_pd05
    MODULE PROCEDURE malloc_pd05
    !> @copybrief Allocs::malloc_pi05 @copydetails Allocs::malloc_pi05
    MODULE PROCEDURE malloc_pi05
    !> @copybrief Allocs::malloc_pl05 @copydetails Allocs::malloc_pl05
    MODULE PROCEDURE malloc_pl05
    !> @copybrief Allocs::malloc_pb05 @copydetails Allocs::malloc_pb05
    MODULE PROCEDURE malloc_pb05
    !> @copybrief Allocs::malloc_ps06 @copydetails Allocs::malloc_ps06
    MODULE PROCEDURE malloc_ps06
    !> @copybrief Allocs::malloc_pd06 @copydetails Allocs::malloc_pd06
    MODULE PROCEDURE malloc_pd06
    !> @copybrief Allocs::malloc_pi06 @copydetails Allocs::malloc_pi06
    MODULE PROCEDURE malloc_pi06
    !> @copybrief Allocs::malloc_pl06 @copydetails Allocs::malloc_pl06
    MODULE PROCEDURE malloc_pl06
    !> @copybrief Allocs::malloc_pb06 @copydetails Allocs::malloc_pb06
    MODULE PROCEDURE malloc_pb06
    !> @copybrief Allocs::malloc_ps07 @copydetails Allocs::malloc_ps07
    MODULE PROCEDURE malloc_ps07
    !> @copybrief Allocs::malloc_pd07 @copydetails Allocs::malloc_pd07
    MODULE PROCEDURE malloc_pd07
    !> @copybrief Allocs::malloc_pi07 @copydetails Allocs::malloc_pi07
    MODULE PROCEDURE malloc_pi07
    !> @copybrief Allocs::malloc_pl07 @copydetails Allocs::malloc_pl07
    MODULE PROCEDURE malloc_pl07
    !> @copybrief Allocs::malloc_pb07 @copydetails Allocs::malloc_pb07
    MODULE PROCEDURE malloc_pb07
  ENDINTERFACE
!
  !> @brief Generic interface for allocating an array from \e n to \e m
  !>
  !>Specifically, it allocates an array of any rank 1 - 7 from \e nbeg to \e nend 
  !>for each dimension of the array for single precision, double precision, 
  !>integer, and logical data types.
  !>
  !>The suffix s,d,i,l refers to data type: single precision, double precision,
  !>integer, logical. The number refers to the rank.
  INTERFACE dmalloc0A
    !> @copybrief Allocs::malloc_as01 @copydetails Allocs::malloc_as01
    MODULE PROCEDURE malloc_as01
    !> @copybrief Allocs::malloc_ad01 @copydetails Allocs::malloc_ad01
    MODULE PROCEDURE malloc_ad01
    !> @copybrief Allocs::malloc_ai01 @copydetails Allocs::malloc_ai01
    MODULE PROCEDURE malloc_ai01
    !> @copybrief Allocs::malloc_al01 @copydetails Allocs::malloc_al01
    MODULE PROCEDURE malloc_al01
    !> @copybrief Allocs::malloc_ab01 @copydetails Allocs::malloc_ab01
    MODULE PROCEDURE malloc_ab01
    !> @copybrief Allocs::malloc_as02 @copydetails Allocs::malloc_as02
    MODULE PROCEDURE malloc_as02
    !> @copybrief Allocs::malloc_ad02 @copydetails Allocs::malloc_ad02
    MODULE PROCEDURE malloc_ad02
    !> @copybrief Allocs::malloc_ai02 @copydetails Allocs::malloc_ai02
    MODULE PROCEDURE malloc_ai02
    !> @copybrief Allocs::malloc_al02 @copydetails Allocs::malloc_al02
    MODULE PROCEDURE malloc_al02
    !> @copybrief Allocs::malloc_ab02 @copydetails Allocs::malloc_ab02
    MODULE PROCEDURE malloc_ab02
    !> @copybrief Allocs::malloc_as03 @copydetails Allocs::malloc_as03
    MODULE PROCEDURE malloc_as03
    !> @copybrief Allocs::malloc_ad03 @copydetails Allocs::malloc_ad03
    MODULE PROCEDURE malloc_ad03
    !> @copybrief Allocs::malloc_ai03 @copydetails Allocs::malloc_ai03
    MODULE PROCEDURE malloc_ai03
    !> @copybrief Allocs::malloc_al03 @copydetails Allocs::malloc_al03
    MODULE PROCEDURE malloc_al03
    !> @copybrief Allocs::malloc_ab03 @copydetails Allocs::malloc_ab03
    MODULE PROCEDURE malloc_ab03
    !> @copybrief Allocs::malloc_as04 @copydetails Allocs::malloc_as04
    MODULE PROCEDURE malloc_as04
    !> @copybrief Allocs::malloc_ad04 @copydetails Allocs::malloc_ad04
    MODULE PROCEDURE malloc_ad04
    !> @copybrief Allocs::malloc_ai04 @copydetails Allocs::malloc_ai04
    MODULE PROCEDURE malloc_ai04
    !> @copybrief Allocs::malloc_al04 @copydetails Allocs::malloc_al04
    MODULE PROCEDURE malloc_al04
    !> @copybrief Allocs::malloc_ab04 @copydetails Allocs::malloc_ab04
    MODULE PROCEDURE malloc_ab04
    !> @copybrief Allocs::malloc_as05 @copydetails Allocs::malloc_as05
    MODULE PROCEDURE malloc_as05
    !> @copybrief Allocs::malloc_ad05 @copydetails Allocs::malloc_ad05
    MODULE PROCEDURE malloc_ad05
    !> @copybrief Allocs::malloc_ai05 @copydetails Allocs::malloc_ai05
    MODULE PROCEDURE malloc_ai05
    !> @copybrief Allocs::malloc_al05 @copydetails Allocs::malloc_al05
    MODULE PROCEDURE malloc_al05
    !> @copybrief Allocs::malloc_ab05 @copydetails Allocs::malloc_ab05
    MODULE PROCEDURE malloc_ab05
    !> @copybrief Allocs::malloc_as06 @copydetails Allocs::malloc_as06
    MODULE PROCEDURE malloc_as06
    !> @copybrief Allocs::malloc_ad06 @copydetails Allocs::malloc_ad06
    MODULE PROCEDURE malloc_ad06
    !> @copybrief Allocs::malloc_ai06 @copydetails Allocs::malloc_ai06
    MODULE PROCEDURE malloc_ai06
    !> @copybrief Allocs::malloc_al06 @copydetails Allocs::malloc_al06
    MODULE PROCEDURE malloc_al06
    !> @copybrief Allocs::malloc_ab06 @copydetails Allocs::malloc_ab06
    MODULE PROCEDURE malloc_ab06
    !> @copybrief Allocs::malloc_as07 @copydetails Allocs::malloc_as07
    MODULE PROCEDURE malloc_as07
    !> @copybrief Allocs::malloc_ad07 @copydetails Allocs::malloc_ad07
    MODULE PROCEDURE malloc_ad07
    !> @copybrief Allocs::malloc_ai07 @copydetails Allocs::malloc_ai07
    MODULE PROCEDURE malloc_ai07
    !> @copybrief Allocs::malloc_al07 @copydetails Allocs::malloc_al07
    MODULE PROCEDURE malloc_al07
    !> @copybrief Allocs::malloc_ab07 @copydetails Allocs::malloc_ab07
    MODULE PROCEDURE malloc_ab07
  ENDINTERFACE
!
  !> @brief Generic interface for deallocating a pointer array. 
  !> 
  !> Defined for all intrinsic types of the kinds defined by IntrType
  INTERFACE demallocP
    !> @copybrief Allocs::demalloc_ps1 @copydetails Allocs::demalloc_ps1
    MODULE PROCEDURE demalloc_ps1
    !> @copybrief Allocs::demalloc_pd1 @copydetails Allocs::demalloc_pd1
    MODULE PROCEDURE demalloc_pd1
    !> @copybrief Allocs::demalloc_pi1 @copydetails Allocs::demalloc_pi1
    MODULE PROCEDURE demalloc_pi1
    !> @copybrief Allocs::demalloc_pl1 @copydetails Allocs::demalloc_pl1
    MODULE PROCEDURE demalloc_pl1
    !> @copybrief Allocs::demalloc_pb1 @copydetails Allocs::demalloc_pb1
    MODULE PROCEDURE demalloc_pb1
    !> @copybrief Allocs::demalloc_ps2 @copydetails Allocs::demalloc_ps2
    MODULE PROCEDURE demalloc_ps2
    !> @copybrief Allocs::demalloc_pd2 @copydetails Allocs::demalloc_pd2
    MODULE PROCEDURE demalloc_pd2
    !> @copybrief Allocs::demalloc_pi2 @copydetails Allocs::demalloc_pi2
    MODULE PROCEDURE demalloc_pi2
     !> @copybrief Allocs::demalloc_pl2 @copydetails Allocs::demalloc_pl2
    MODULE PROCEDURE demalloc_pl2
    !> @copybrief Allocs::demalloc_pb2 @copydetails Allocs::demalloc_pb2
    MODULE PROCEDURE demalloc_pb2
    !> @copybrief Allocs::demalloc_ps3 @copydetails Allocs::demalloc_ps3
    MODULE PROCEDURE demalloc_ps3
    !> @copybrief Allocs::demalloc_pd3 @copydetails Allocs::demalloc_pd3
    MODULE PROCEDURE demalloc_pd3
    !> @copybrief Allocs::demalloc_pi3 @copydetails Allocs::demalloc_pi3
    MODULE PROCEDURE demalloc_pi3
     !> @copybrief Allocs::demalloc_pl3 @copydetails Allocs::demalloc_pl3
    MODULE PROCEDURE demalloc_pl3
    !> @copybrief Allocs::demalloc_pb3 @copydetails Allocs::demalloc_pb3
    MODULE PROCEDURE demalloc_pb3
    !> @copybrief Allocs::demalloc_ps4 @copydetails Allocs::demalloc_ps4
    MODULE PROCEDURE demalloc_ps4
    !> @copybrief Allocs::demalloc_pd4 @copydetails Allocs::demalloc_pd4
    MODULE PROCEDURE demalloc_pd4
    !> @copybrief Allocs::demalloc_pi4 @copydetails Allocs::demalloc_pi4
    MODULE PROCEDURE demalloc_pi4
    !> @copybrief Allocs::demalloc_pl4 @copydetails Allocs::demalloc_pl4
    MODULE PROCEDURE demalloc_pl4
    !> @copybrief Allocs::demalloc_pb4 @copydetails Allocs::demalloc_pb4
    MODULE PROCEDURE demalloc_pb4
    !> @copybrief Allocs::demalloc_ps5 @copydetails Allocs::demalloc_ps5
    MODULE PROCEDURE demalloc_ps5
    !> @copybrief Allocs::demalloc_pd5 @copydetails Allocs::demalloc_pd5
    MODULE PROCEDURE demalloc_pd5
    !> @copybrief Allocs::demalloc_pi5 @copydetails Allocs::demalloc_pi5
    MODULE PROCEDURE demalloc_pi5
    !> @copybrief Allocs::demalloc_pl5 @copydetails Allocs::demalloc_pl5
    MODULE PROCEDURE demalloc_pl5
    !> @copybrief Allocs::demalloc_pb5 @copydetails Allocs::demalloc_pb5
    MODULE PROCEDURE demalloc_pb5
    !> @copybrief Allocs::demalloc_ps6 @copydetails Allocs::demalloc_ps6
    MODULE PROCEDURE demalloc_ps6
    !> @copybrief Allocs::demalloc_pd6 @copydetails Allocs::demalloc_pd6
    MODULE PROCEDURE demalloc_pd6
    !> @copybrief Allocs::demalloc_pi6 @copydetails Allocs::demalloc_pi6
    MODULE PROCEDURE demalloc_pi6
    !> @copybrief Allocs::demalloc_pl6 @copydetails Allocs::demalloc_pl6
    MODULE PROCEDURE demalloc_pl6
    !> @copybrief Allocs::demalloc_pb6 @copydetails Allocs::demalloc_pb6
    MODULE PROCEDURE demalloc_pb6
    !> @copybrief Allocs::demalloc_ps7 @copydetails Allocs::demalloc_ps7
    MODULE PROCEDURE demalloc_ps7
    !> @copybrief Allocs::demalloc_pd7 @copydetails Allocs::demalloc_pd7
    MODULE PROCEDURE demalloc_pd7
    !> @copybrief Allocs::demalloc_pi7 @copydetails Allocs::demalloc_pi7
    MODULE PROCEDURE demalloc_pi7
    !> @copybrief Allocs::demalloc_pl7 @copydetails Allocs::demalloc_pl7
    MODULE PROCEDURE demalloc_pl7
    !> @copybrief Allocs::demalloc_pb7 @copydetails Allocs::demalloc_pb7
    MODULE PROCEDURE demalloc_pb7
  ENDINTERFACE
!
  !> @brief Generic interface for deallocating an allocatable array. 
  !> 
  !> Defined for all intrinsic types of the kinds defined by IntrType
  INTERFACE demallocA
    !> @copybrief Allocs::demalloc_as1 @copydetails Allocs::demalloc_as1
    MODULE PROCEDURE demalloc_as1
    !> @copybrief Allocs::demalloc_ad1 @copydetails Allocs::demalloc_ad1
    MODULE PROCEDURE demalloc_ad1
    !> @copybrief Allocs::demalloc_ai1 @copydetails Allocs::demalloc_ai1
    MODULE PROCEDURE demalloc_ai1
    !> @copybrief Allocs::demalloc_al1 @copydetails Allocs::demalloc_al1
    MODULE PROCEDURE demalloc_al1
    !> @copybrief Allocs::demalloc_ab1 @copydetails Allocs::demalloc_ab1
    MODULE PROCEDURE demalloc_ab1
    !> @copybrief Allocs::demalloc_as2 @copydetails Allocs::demalloc_as2
    MODULE PROCEDURE demalloc_as2
    !> @copybrief Allocs::demalloc_ad2 @copydetails Allocs::demalloc_ad2
    MODULE PROCEDURE demalloc_ad2
    !> @copybrief Allocs::demalloc_ai2 @copydetails Allocs::demalloc_ai2
    MODULE PROCEDURE demalloc_ai2
     !> @copybrief Allocs::demalloc_al2 @copydetails Allocs::demalloc_al2
    MODULE PROCEDURE demalloc_al2
    !> @copybrief Allocs::demalloc_ab2 @copydetails Allocs::demalloc_ab2
    MODULE PROCEDURE demalloc_ab2
    !> @copybrief Allocs::demalloc_as3 @copydetails Allocs::demalloc_as3
    MODULE PROCEDURE demalloc_as3
    !> @copybrief Allocs::demalloc_ad3 @copydetails Allocs::demalloc_ad3
    MODULE PROCEDURE demalloc_ad3
    !> @copybrief Allocs::demalloc_ai3 @copydetails Allocs::demalloc_ai3
    MODULE PROCEDURE demalloc_ai3
    !> @copybrief Allocs::demalloc_al3 @copydetails Allocs::demalloc_al3
    MODULE PROCEDURE demalloc_al3
    !> @copybrief Allocs::demalloc_ab3 @copydetails Allocs::demalloc_ab3
    MODULE PROCEDURE demalloc_ab3
    !> @copybrief Allocs::demalloc_as4 @copydetails Allocs::demalloc_as4
    MODULE PROCEDURE demalloc_as4
    !> @copybrief Allocs::demalloc_ad4 @copydetails Allocs::demalloc_ad4
    MODULE PROCEDURE demalloc_ad4
    !> @copybrief Allocs::demalloc_ai4 @copydetails Allocs::demalloc_ai4
    MODULE PROCEDURE demalloc_ai4
    !> @copybrief Allocs::demalloc_al4 @copydetails Allocs::demalloc_al4
    MODULE PROCEDURE demalloc_al4
    !> @copybrief Allocs::demalloc_ab4 @copydetails Allocs::demalloc_ab4
    MODULE PROCEDURE demalloc_ab4
    !> @copybrief Allocs::demalloc_as5 @copydetails Allocs::demalloc_as5
    MODULE PROCEDURE demalloc_as5
    !> @copybrief Allocs::demalloc_ad5 @copydetails Allocs::demalloc_ad5
    MODULE PROCEDURE demalloc_ad5
    !> @copybrief Allocs::demalloc_ai5 @copydetails Allocs::demalloc_ai5
    MODULE PROCEDURE demalloc_ai5
    !> @copybrief Allocs::demalloc_al5 @copydetails Allocs::demalloc_al5
    MODULE PROCEDURE demalloc_al5
    !> @copybrief Allocs::demalloc_ab5 @copydetails Allocs::demalloc_ab5
    MODULE PROCEDURE demalloc_ab5
    !> @copybrief Allocs::demalloc_as6 @copydetails Allocs::demalloc_as6
    MODULE PROCEDURE demalloc_as6
    !> @copybrief Allocs::demalloc_ad6 @copydetails Allocs::demalloc_ad6
    MODULE PROCEDURE demalloc_ad6
    !> @copybrief Allocs::demalloc_ai6 @copydetails Allocs::demalloc_ai6
    MODULE PROCEDURE demalloc_ai6
    !> @copybrief Allocs::demalloc_al6 @copydetails Allocs::demalloc_al6
    MODULE PROCEDURE demalloc_al6
    !> @copybrief Allocs::demalloc_ab6 @copydetails Allocs::demalloc_ab6
    MODULE PROCEDURE demalloc_ab6
    !> @copybrief Allocs::demalloc_as7 @copydetails Allocs::demalloc_as7
    MODULE PROCEDURE demalloc_as7
    !> @copybrief Allocs::demalloc_ad7 @copydetails Allocs::demalloc_ad7
    MODULE PROCEDURE demalloc_ad7
    !> @copybrief Allocs::demalloc_ai7 @copydetails Allocs::demalloc_ai7
    MODULE PROCEDURE demalloc_ai7
    !> @copybrief Allocs::demalloc_al7 @copydetails Allocs::demalloc_al7
    MODULE PROCEDURE demalloc_al7
    !> @copybrief Allocs::demalloc_ab7 @copydetails Allocs::demalloc_ab7
    MODULE PROCEDURE demalloc_ab7
  ENDINTERFACE
!
!===============================================================================
  CONTAINS
!
!> @brief Function to get the memory usage as a string
!>
!> To see the current memory usage use the module global variable \e Alloc_nbytes
!> when calling getMemUsageChar.
    FUNCTION getMemUsageChar_default() RESULT(memstring)
      CHARACTER(LEN=ALLOC_MEMSTRING_LENGTH) :: memstring
      CHARACTER(LEN=5) :: unit
      REAL(SRK) :: mem,bytes
      REAL(SRK),PARAMETER :: KB2bytes=1024_SRK
      REAL(SRK),PARAMETER :: MB2bytes=1048576_SRK
      REAL(SRK),PARAMETER :: GB2bytes=1073741824_SRK
      
      bytes=Alloc_nbytes
      mem=bytes
      unit='bytes'
      IF(bytes >= KB2bytes .AND. bytes < MB2bytes) THEN
        mem=bytes/KB2bytes
        unit='KB'
      ELSEIF(bytes >= MB2bytes .AND. bytes < GB2bytes) THEN
        mem=bytes/MB2bytes
        unit='MB'
      ELSEIF(Alloc_nbytes >= GB2bytes) THEN
        mem=bytes/GB2bytes
        unit='GB'
      ENDIF
      WRITE(memstring,'(f8.2,a)') mem,' '//unit
    ENDFUNCTION getMemUsageChar_default
!
!===============================================================================
!> @brief Function to get the memory usage for input argument bytes as a string
!> @param bytes the number of bytes
!>
!> To see the current memory usage use the module global variable \e Alloc_nbytes
!> when calling getMemUsageChar.
    FUNCTION getMemUsageChar_bytes(bytes) RESULT(memstring)
      CHARACTER(LEN=ALLOC_MEMSTRING_LENGTH) :: memstring
      CHARACTER(LEN=5) :: unit
      REAL(SRK),INTENT(IN) :: bytes
      REAL(SRK) :: mem
      REAL(SRK),PARAMETER :: KB2bytes=1024_SRK
      REAL(SRK),PARAMETER :: MB2bytes=1048576_SRK
      REAL(SRK),PARAMETER :: GB2bytes=1073741824_SRK

      mem=0.0_SRK
      unit='bytes'
      IF(bytes >= KB2bytes .AND. bytes < MB2bytes) THEN
        mem=bytes/KB2bytes
        unit='KB'
      ELSEIF(bytes >= MB2bytes .AND. bytes < GB2bytes) THEN
        mem=bytes/MB2bytes
        unit='MB'
      ELSEIF(bytes >= GB2bytes) THEN
        mem=bytes/GB2bytes
        unit='GB'
      ENDIF
      WRITE(memstring,'(f8.2,a)') mem,' '//unit
    ENDFUNCTION getMemUsageChar_bytes
!
!-------------------------------------------------------------------------------
!> @brief Function to get the memory usage for input argument bytes as a string
!> @param bytes the number of bytes
!>
!> To see the current memory usage use the module global variable \e Alloc_nbytes
!> when calling getMemUsageChar.
    SUBROUTINE getMemUsage(memory,units)
      CHARACTER(LEN=*),INTENT(IN) :: units
      REAL(SRK),INTENT(INOUT) :: memory
      CHARACTER(LEN=32) :: mem_string,mem_unit
      REAL(SRK),PARAMETER :: KB2bytes=1024_SRK
      REAL(SRK),PARAMETER :: MB2bytes=1048576_SRK
      REAL(SRK),PARAMETER :: GB2bytes=1073741824_SRK
      
      ! Get memory usage for current process, then convert to bytes
      mem_string=getMemUsageChar_default()
      READ(mem_string,FMT='(f8.2,a)') memory,mem_unit
      IF(TRIM(ADJUSTL(mem_unit))=='KB') THEN
        memory=memory*KB2bytes
      ELSEIF(TRIM(ADJUSTL(mem_unit))=='MB') THEN
        memory=memory*MB2bytes
      ELSEIF(TRIM(ADJUSTL(mem_unit))=='GB') THEN
        memory=memory*GB2bytes
      ENDIF

      ! Total memory as bytes, convert to whatever was requrested
      IF(TRIM(ADJUSTL(units))=='KB') THEN
        memory=memory/KB2bytes
      ELSEIF(TRIM(ADJUSTL(units))=='MB') THEN
        memory=memory/MB2bytes
      ELSEIF(TRIM(ADJUSTL(units))=='GB') THEN
        memory=memory/GB2bytes
      ENDIF
    ENDSUBROUTINE getMemUsage
!
!-------------------------------------------------------------------------------
!> @name Private Routines
!< @{
!> @brief Raises an error with the module exception handler.
!> @param err input argument for the error value
!> @param req input argument for the memory requested
!>
!> For more about the exception handler see the module @ref ExceptionHandler
!> "ExceptionHandler"
    SUBROUTINE AllocsError(err,req)
      INTEGER(SIK),INTENT(IN) :: err
      REAL(SRK),INTENT(IN) :: req
      CHARACTER(LEN=EXCEPTION_MAX_MESG_LENGTH) :: alloc_mesg
      
      alloc_mesg=''
      IF(err /= 0_SIK) THEN
        WRITE(alloc_mesg,'(a,i4,a)') &
          'ALLOCS: Memory Allocation Error. Attempted to allocate '// &
          TRIM(getMemUsageChar_bytes(req))//' while using '// &
          TRIM(getMemUsageChar_default())//'. ALLOCATE statement returned'// &
          ' status code ',err,'.'
        CALL eAllocs%raiseError(alloc_mesg)
      ENDIF
    ENDSUBROUTINE AllocsError
!
!===============================================================================
! The following section contains all procedures for allocating pointers from 1
! to n.
!
!> @brief Allocates rank 1 SINGLE precision pointer array.
!> @param a dummy argument for pointer array to be allocated
!> @param n1 size of first dimension
!>
!>Routine for allocating a rank 1 pointer array of type SINGLE 
!>precision from 1 to \e n1.
    SUBROUTINE malloc_ps1(a,n1)
      INTEGER(SIK),INTENT(IN) :: n1
      REAL(SSK),POINTER :: a(:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ASSOCIATED(a)) THEN
        NULLIFY(a)
        IF(n1 >= 1_SIK) THEN
          ALLOCATE(a(n1),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=0.0_SSK
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SSK,SRK)
          ELSE
            bsize=REAL(SSK,SRK)
            mysize=REAL(REAL(n1,SRK)*bsize,SRK)
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_ps1
!
!----------------------------------------
!> @brief Allocates rank 1 DOUBLE precision pointer array.
!> @param a dummy argument for pointer array to be allocated
!> @param n1 size of first dimension
!>
!> Routine for allocating a rank 1 pointer array of type DOUBLE 
!> precision from 1 to \e n1.
    SUBROUTINE malloc_pd1(a,n1)
      INTEGER(SIK),INTENT(IN) :: n1
      REAL(SDK),POINTER :: a(:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ASSOCIATED(a)) THEN
        NULLIFY(a)
        IF(n1 >= 1_SIK) THEN
          ALLOCATE(a(n1),STAT=alloc_err)
           IF(alloc_err == 0_SIK) THEN
            a=0.0_SDK
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SDK,SRK)
          ELSE
            bsize=REAL(SDK,SRK)
            mysize=REAL(REAL(n1,SRK)*bsize,SRK)
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_pd1
!
!----------------------------------------
!> @brief Allocates rank 1 INTEGER pointer array.
!> @param a dummy argument for pointer array to be allocated
!> @param n1 size of first dimension
!>
!> Routine for allocating a rank 1 pointer array of type INTEGER 
!> from 1 to \e n1.
    SUBROUTINE malloc_pi1(a,n1)
      INTEGER(SIK),INTENT(IN) :: n1
      INTEGER(SNK),POINTER :: a(:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ASSOCIATED(a)) THEN
        NULLIFY(a)
        IF(n1 >= 1_SIK) THEN
          ALLOCATE(a(n1),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=0_SNK
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SNK,SRK)
          ELSE
            bsize=REAL(SNK,SRK)
            mysize=REAL(REAL(n1,SRK)*bsize,SRK)
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_pi1
!
!----------------------------------------
!> @brief Allocates rank 1 LONG integer pointer array.
!> @param a dummy argument for pointer array to be allocated
!> @param n1 size of first dimension
!>
!> Routine for allocating a rank 1 pointer array of type LONG integer 
!> from 1 to \e n1.
    SUBROUTINE malloc_pl1(a,n1)
      INTEGER(SIK),INTENT(IN) :: n1
      INTEGER(SLK),POINTER :: a(:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ASSOCIATED(a)) THEN
        NULLIFY(a)
        IF(n1 >= 1_SIK) THEN
          ALLOCATE(a(n1),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=0_SLK
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SLK,SRK)
          ELSE
            bsize=REAL(SLK,SRK)
            mysize=REAL(REAL(n1,SRK)*bsize,SRK)
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_pl1
!
!----------------------------------------
!> @brief Allocates rank 1 LOGICAL pointer array.
!> @param a dummy argument for pointer array to be allocated
!> @param n1 size of first dimension
!>
!> Routine for allocating a rank 1 pointer array of type LOGICAL 
!> from 1 to \e n1.
    SUBROUTINE malloc_pb1(a,n1)
      INTEGER(SIK),INTENT(IN) :: n1
      LOGICAL(SBK),POINTER :: a(:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ASSOCIATED(a)) THEN
        NULLIFY(a)
        IF(n1 >= 1_SIK) THEN
          ALLOCATE(a(n1),STAT=alloc_err)
           IF(alloc_err == 0_SIK) THEN
            a=.FALSE.
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SBK,SRK)
          ELSE
            bsize=REAL(SBK,SRK)
            mysize=REAL(REAL(n1,SRK)*bsize,SRK)
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_pb1
!
!-------------------------------------------------------------------------------
! Allocate two dimensional pointers
!
!> @brief Allocates rank 2 SINGLE precision pointer array.
!> @param a dummy argument for pointer array to be allocated
!> @param n1 size of first dimension
!> @param n2 size of second dimension
!>
!> Routine for allocating a rank 2 pointer array of type SINGLE 
!> precision from 1 to \e n1 and 1 to \e n2
    SUBROUTINE malloc_ps2(a,n1,n2)
      INTEGER(SIK),INTENT(IN) :: n1,n2
      REAL(SSK),POINTER :: a(:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ASSOCIATED(a)) THEN
        NULLIFY(a)
        IF(n1 >= 1_SIK .AND. n2 >= 1_SIK) THEN
          ALLOCATE(a(n1,n2),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=0.0_SSK
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SSK,SRK)
          ELSE
            bsize=REAL(SSK,SRK)
            mysize=REAL(REAL(n1,SRK)*REAL(n2,SRK)*bsize,SRK)
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_ps2
!
!----------------------------------------
!> @brief Allocates rank 2 DOUBLE precision pointer array.
!> @param a dummy argument for pointer array to be allocated
!> @param n1 size of first dimension
!> @param n2 size of second dimension
!>
!> Routine for allocating a rank 2 pointer array of type DOUBLE 
!> precision from 1 to \e n1 and 1 to \e n2
    SUBROUTINE malloc_pd2(a,n1,n2)
      INTEGER(SIK),INTENT(IN) :: n1,n2
      REAL(SDK),POINTER :: a(:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ASSOCIATED(a)) THEN
        NULLIFY(a)
        IF(n1 >= 1_SIK .AND. n2 >= 1_SIK) THEN
          ALLOCATE(a(n1,n2),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=0.0_SDK
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SDK,SRK)
          ELSE
            bsize=REAL(SDK,SRK)
            mysize=REAL(REAL(n1,SRK)*REAL(n2,SRK)*bsize,SRK)
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_pd2
!
!----------------------------------------
!> @brief Allocates rank 2 INTEGER pointer array.
!> @param a dummy argument for pointer array to be allocated
!> @param n1 size of first dimension
!> @param n2 size of second dimension
!>
!> Routine for allocating a rank 2 pointer array of type INTEGER
!> from 1 to \e n1 and 1 to \e n2
    SUBROUTINE malloc_pi2(a,n1,n2)
      INTEGER(SIK),INTENT(IN) :: n1,n2
      INTEGER(SNK),POINTER :: a(:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ASSOCIATED(a)) THEN
        NULLIFY(a)
        IF(n1 >= 1_SIK .AND. n2 >= 1_SIK) THEN
          ALLOCATE(a(n1,n2),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=0_SNK
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SNK,SRK)
          ELSE
            bsize=REAL(SNK,SRK)
            mysize=REAL(REAL(n1,SRK)*REAL(n2,SRK)*bsize,SRK)
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_pi2
!
!----------------------------------------
!> @brief Allocates rank 2 LONG integer pointer array.
!> @param a dummy argument for pointer array to be allocated
!> @param n1 size of first dimension
!> @param n2 size of second dimension
!>
!> Routine for allocating a rank 2 pointer array of type LONG integer
!> from 1 to \e n1 and 1 to \e n2
    SUBROUTINE malloc_pl2(a,n1,n2)
      INTEGER(SIK),INTENT(IN) :: n1,n2
      INTEGER(SLK),POINTER :: a(:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ASSOCIATED(a)) THEN
        NULLIFY(a)
        IF(n1 >= 1_SIK .AND. n2 >= 1_SIK) THEN
          ALLOCATE(a(n1,n2),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=0_SLK
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SLK,SRK)
          ELSE
            bsize=REAL(SlK,SRK)
            mysize=REAL(REAL(n1,SRK)*REAL(n2,SRK)*bsize,SRK)
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_pl2
!
!----------------------------------------
!> @brief Allocates rank 2 LOGICAL pointer array.
!> @param a dummy argument for pointer array to be allocated
!> @param n1 size of first dimension
!> @param n2 size of second dimension
!>
!> Routine for allocating a rank 2 pointer array of type LOGICAL
!> from 1 to \e n1 and 1 to \e n2
    SUBROUTINE malloc_pb2(a,n1,n2)
      INTEGER(SIK),INTENT(IN) :: n1,n2
      LOGICAL(SBK),POINTER :: a(:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ASSOCIATED(a)) THEN
        NULLIFY(a)
        IF(n1 >= 1_SIK .AND. n2 >= 1_SIK) THEN
          ALLOCATE(a(n1,n2),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=.FALSE.
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SBK,SRK)
          ELSE
            bsize=REAL(SBK,SRK)
            mysize=REAL(REAL(n1,SRK)*REAL(n2,SRK)*bsize,SRK)
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_pb2
!
!-------------------------------------------------------------------------------
! Allocate three dimensional pointers
!
!> @brief Allocates rank 3 SINGLE precision pointer array.
!> @param a dummy argument for pointer array to be allocated
!> @param n1 size of first dimension
!> @param n2 size of second dimension
!> @param n3 size of third dimension
!>
!> Routine for allocating a rank 3 pointer array of type SINGLE
!> precision from 1 to \e n1, 1 to \e n2, and 1 to \e n3
    SUBROUTINE malloc_ps3(a,n1,n2,n3)
      INTEGER(SIK),INTENT(IN) :: n1,n2,n3
      REAL(SSK),POINTER :: a(:,:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ASSOCIATED(a)) THEN
        NULLIFY(a)
        IF(n1 >= 1_SIK .AND. n2 >= 1_SIK .AND. n3 >= 1_SIK) THEN
          ALLOCATE(a(n1,n2,n3),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=0.0_SSK
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SSK,SRK)
          ELSE
            bsize=REAL(SSK,SRK)
            mysize=REAL(REAL(n1,SRK)*REAL(n2,SRK)*REAL(n3,SRK)*bsize,SRK)
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_ps3
!
!----------------------------------------
!> @brief Allocates rank 3 DOUBLE precision pointer array.
!> @param a dummy argument for pointer array to be allocated
!> @param n1 size of first dimension
!> @param n2 size of second dimension
!> @param n3 size of third dimension
!>
!> Routine for allocating a rank 3 pointer array of type DOUBLE
!> precision from 1 to \e n1, 1 to \e n2, and 1 to \e n3
    SUBROUTINE malloc_pd3(a,n1,n2,n3)
      INTEGER(SIK),INTENT(IN) :: n1,n2,n3
      REAL(SDK),POINTER :: a(:,:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ASSOCIATED(a)) THEN
        NULLIFY(a)
        IF(n1 >= 1_SIK .AND. n2 >= 1_SIK .AND. n3 >= 1_SIK) THEN
          ALLOCATE(a(n1,n2,n3),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=0.0_SDK
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SDK,SRK)
          ELSE
            bsize=REAL(SDK,SRK)
            mysize=REAL(REAL(n1,SRK)*REAL(n2,SRK)*REAL(n3,SRK)*bsize,SRK)
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_pd3
!
!----------------------------------------
!> @brief Allocates rank 3 INTEGER pointer array.
!> @param a dummy argument for pointer array to be allocated
!> @param n1 size of first dimension
!> @param n2 size of second dimension
!> @param n3 size of third dimension
!>
!> Routine for allocating a rank 3 pointer array of type INTEGER
!> from 1 to \e n1, 1 to \e n2, and 1 to \e n3
    SUBROUTINE malloc_pi3(a,n1,n2,n3)
      INTEGER(SIK),INTENT(IN) :: n1,n2,n3
      INTEGER(SNK),POINTER :: a(:,:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ASSOCIATED(a)) THEN
        NULLIFY(a)
        IF(n1 >= 1_SIK .AND. n2 >= 1_SIK .AND. n3 >= 1_SIK) THEN
          ALLOCATE(a(n1,n2,n3),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=0_SNK
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SNK,SRK)
          ELSE
            bsize=REAL(SNK,SRK)
            mysize=REAL(REAL(n1,SRK)*REAL(n2,SRK)*REAL(n3,SRK)*bsize,SRK)
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_pi3
!
!----------------------------------------
!> @brief Allocates rank 3 LONG integer pointer array.
!> @param a dummy argument for pointer array to be allocated
!> @param n1 size of first dimension
!> @param n2 size of second dimension
!> @param n3 size of third dimension
!>
!> Routine for allocating a rank 3 pointer array of type LONG integer
!> from 1 to \e n1, 1 to \e n2, and 1 to \e n3
    SUBROUTINE malloc_pl3(a,n1,n2,n3)
      INTEGER(SIK),INTENT(IN) :: n1,n2,n3
      INTEGER(SLK),POINTER :: a(:,:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ASSOCIATED(a)) THEN
        NULLIFY(a)
        IF(n1 >= 1_SIK .AND. n2 >= 1_SIK .AND. n3 >= 1_SIK) THEN
          ALLOCATE(a(n1,n2,n3),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=0_SLK
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SLK,SRK)
          ELSE
            bsize=REAL(SLK,SRK)
            mysize=REAL(REAL(n1,SRK)*REAL(n2,SRK)*REAL(n3,SRK)*bsize,SRK)
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_pl3
!
!----------------------------------------
!> @brief Allocates rank 3 LOGICAL pointer array.
!> @param a dummy argument for pointer array to be allocated
!> @param n1 size of first dimension
!> @param n2 size of second dimension
!> @param n3 size of third dimension
!>
!> Routine for allocating a rank 3 pointer array of type LOGICAL
!> from 1 to \e n1, 1 to \e n2, and 1 to \e n3
    SUBROUTINE malloc_pb3(a,n1,n2,n3)
      INTEGER(SIK),INTENT(IN) :: n1,n2,n3
      LOGICAL(SBK),POINTER :: a(:,:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ASSOCIATED(a)) THEN
        NULLIFY(a)
        IF(n1 >= 1_SIK .AND. n2 >= 1_SIK .AND. n3 >= 1_SIK) THEN
          ALLOCATE(a(n1,n2,n3),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=.FALSE.
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SBK,SRK)
          ELSE
            bsize=REAL(SBK,SRK)
            mysize=REAL(REAL(n1,SRK)*REAL(n2,SRK)*REAL(n3,SRK)*bsize,SRK)
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_pb3
!
!-------------------------------------------------------------------------------
! Allocate four dimensional pointers
!
!> @brief Allocates rank 4 SINGLE precision pointer array.
!> @param a dummy argument for pointer array to be allocated
!> @param n1 size of first dimension
!> @param n2 size of second dimension
!> @param n3 size of third dimension
!> @param n4 size of fourth dimension
!>
!> Routine for allocating a rank 4 pointer array of type SINGLE
!> precision from 1 to \e n1, 1 to \e n2, 1 to \e n3, and 1 to \e n4
    SUBROUTINE malloc_ps4(a,n1,n2,n3,n4)
      INTEGER(SIK),INTENT(IN) :: n1,n2,n3,n4
      REAL(SSK),POINTER :: a(:,:,:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ASSOCIATED(a)) THEN
        NULLIFY(a)
        IF(n1 >= 1_SIK .AND. n2 >= 1_SIK .AND. &
                             n3 >= 1_SIK .AND. n4 >= 1_SIK) THEN
          ALLOCATE(a(n1,n2,n3,n4),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=0.0_SSK
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SSK,SRK)
          ELSE
            bsize=REAL(SSK,SRK)
            mysize=REAL(REAL(n1,SRK)*REAL(n2,SRK)*REAL(n3,SRK)* &
                        REAL(n4,SRK)*bsize,SRK)
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_ps4
!
!----------------------------------------
!> @brief Allocates rank 4 DOUBLE precision pointer array.
!> @param a dummy argument for pointer array to be allocated
!> @param n1 size of first dimension
!> @param n2 size of second dimension
!> @param n3 size of third dimension
!> @param n4 size of fourth dimension
!>
!> Routine for allocating a rank 4 pointer array of type DOUBLE
!> precision from 1 to \e n1, 1 to \e n2, 1 to \e n3, and 1 to \e n4
    SUBROUTINE malloc_pd4(a,n1,n2,n3,n4)
      INTEGER(SIK),INTENT(IN) :: n1,n2,n3,n4
      REAL(SDK),POINTER :: a(:,:,:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ASSOCIATED(a)) THEN
        NULLIFY(a)
        IF(n1 >= 1_SIK .AND. n2 >= 1_SIK .AND. &
                             n3 >= 1_SIK .AND. n4 >= 1_SIK) THEN
          ALLOCATE(a(n1,n2,n3,n4),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=0.0_SDK
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SDK,SRK)
          ELSE
            bsize=REAL(SDK,SRK)
            mysize=REAL(REAL(n1,SRK)*REAL(n2,SRK)*REAL(n3,SRK)* &
                        REAL(n4,SRK)*bsize,SRK)
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_pd4
!
!----------------------------------------
!> @brief Allocates rank 4 INTEGER pointer array.
!> @param a dummy argument for pointer array to be allocated
!> @param n1 size of first dimension
!> @param n2 size of second dimension
!> @param n3 size of third dimension
!> @param n4 size of fourth dimension
!>
!> Routine for allocating a rank 4 pointer array of type INTEGER
!> from 1 to \e n1, 1 to \e n2, 1 to \e n3, and 1 to \e n4
    SUBROUTINE malloc_pi4(a,n1,n2,n3,n4)
      INTEGER(SIK),INTENT(IN) :: n1,n2,n3,n4
      INTEGER(SNK),POINTER :: a(:,:,:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ASSOCIATED(a)) THEN
        NULLIFY(a)
        IF(n1 >= 1_SIK .AND. n2 >= 1_SIK .AND. &
                             n3 >= 1_SIK .AND. n4 >= 1_SIK) THEN
          ALLOCATE(a(n1,n2,n3,n4),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=0_SNK
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SNK,SRK)
          ELSE
            bsize=REAL(SNK,SRK)
            mysize=REAL(REAL(n1,SRK)*REAL(n2,SRK)*REAL(n3,SRK)* &
                        REAL(n4,SRK)*bsize,SRK)
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_pi4
!
!----------------------------------------
!> @brief Allocates rank 4 LONG integer pointer array.
!> @param a dummy argument for pointer array to be allocated
!> @param n1 size of first dimension
!> @param n2 size of second dimension
!> @param n3 size of third dimension
!> @param n4 size of fourth dimension
!>
!> Routine for allocating a rank 4 pointer array of type LONG integer
!> from 1 to \e n1, 1 to \e n2, 1 to \e n3, and 1 to \e n4
    SUBROUTINE malloc_pl4(a,n1,n2,n3,n4)
      INTEGER(SIK),INTENT(IN) :: n1,n2,n3,n4
      INTEGER(SLK),POINTER :: a(:,:,:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ASSOCIATED(a)) THEN
        NULLIFY(a)
        IF(n1 >= 1_SIK .AND. n2 >= 1_SIK .AND. &
                             n3 >= 1_SIK .AND. n4 >= 1_SIK) THEN
          ALLOCATE(a(n1,n2,n3,n4),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=0_SLK
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SLK,SRK)
          ELSE
            bsize=REAL(SLK,SRK)
            mysize=REAL(REAL(n1,SRK)*REAL(n2,SRK)*REAL(n3,SRK)* &
                        REAL(n4,SRK)*bsize,SRK)
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_pl4
!
!----------------------------------------
!> @brief Allocates rank 4 LOGICAL pointer array.
!> @param a dummy argument for pointer array to be allocated
!> @param n1 size of first dimension
!> @param n2 size of second dimension
!> @param n3 size of third dimension
!> @param n4 size of fourth dimension
!>
!> Routine for allocating a rank 4 pointer array of type LOGICAL
!> from 1 to \e n1, 1 to \e n2, 1 to \e n3, and 1 to \e n4
    SUBROUTINE malloc_pb4(a,n1,n2,n3,n4)
      INTEGER(SIK),INTENT(IN) :: n1,n2,n3,n4
      LOGICAL(SBK),POINTER :: a(:,:,:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ASSOCIATED(a)) THEN
        NULLIFY(a)
        IF(n1 >= 1_SIK .AND. n2 >= 1_SIK .AND. &
                             n3 >= 1_SIK .AND. n4 >= 1_SIK) THEN
          ALLOCATE(a(n1,n2,n3,n4),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=.FALSE.
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SBK,SRK)
          ELSE
            bsize=REAL(SBK,SRK)
            mysize=REAL(REAL(n1,SRK)*REAL(n2,SRK)*REAL(n3,SRK)* &
                        REAL(n4,SRK)*bsize,SRK)
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_pb4
!
!-------------------------------------------------------------------------------
! Allocate five dimensional pointers
!
!> @brief Allocates rank 5 SINGLE precision pointer array.
!> @param a dummy argument for pointer array to be allocated
!> @param n1 size of first dimension
!> @param n2 size of second dimension
!> @param n3 size of third dimension
!> @param n4 size of fourth dimension
!> @param n5 size of fifth dimension
!>
!> Routine for allocating a rank 5 pointer array of type SINGLE
!> precision from 1 to \e n1, 1 to \e n2, 1 to \e n3, 1 to \e n3, 
!> 1 to \e n4, and 1 to \e n5
    SUBROUTINE malloc_ps5(a,n1,n2,n3,n4,n5)
      INTEGER(SIK),INTENT(IN) :: n1,n2,n3,n4,n5
      REAL(SSK),POINTER :: a(:,:,:,:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ASSOCIATED(a)) THEN
        NULLIFY(a)
        IF(n1 >= 1_SIK .AND. n2 >= 1_SIK &
                       .AND. n3 >= 1_SIK .AND. n4 >= 1_SIK &
                                         .AND. n5 >= 1_SIK) THEN
          ALLOCATE(a(n1,n2,n3,n4,n5),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=0.0_SSK
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SSK,SRK)
          ELSE
            bsize=REAL(SSK,SRK)
            mysize=REAL(REAL(n1,SRK)*REAL(n2,SRK)*REAL(n3,SRK)* &
                        REAL(n4,SRK)*REAL(n5,SRK)*bsize,SRK)
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_ps5
!
!----------------------------------------
!> @brief Allocates rank 5 DOUBLE precision pointer array.
!> @param a dummy argument for pointer array to be allocated
!> @param n1 size of first dimension
!> @param n2 size of second dimension
!> @param n3 size of third dimension
!> @param n4 size of fourth dimension
!> @param n5 size of fifth dimension
!>
!> Routine for allocating a rank 5 pointer array of type DOUBLE
!> precision from 1 to \e n1, 1 to \e n2, 1 to \e n3, 1 to \e n3,
!> 1 to \e n4, and 1 to \e n5
    SUBROUTINE malloc_pd5(a,n1,n2,n3,n4,n5)
      INTEGER(SIK),INTENT(IN) :: n1,n2,n3,n4,n5
      REAL(SDK),POINTER :: a(:,:,:,:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ASSOCIATED(a)) THEN
        NULLIFY(a)
        IF(n1 >= 1_SIK .AND. n2 >= 1_SIK &
                       .AND. n3 >= 1_SIK .AND. n4 >= 1_SIK &
                                         .AND. n5 >= 1_SIK) THEN
          ALLOCATE(a(n1,n2,n3,n4,n5),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=0.0_SDK
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SDK,SRK)
          ELSE
            bsize=REAL(SDK,SRK)
            mysize=REAL(REAL(n1,SRK)*REAL(n2,SRK)*REAL(n3,SRK)* &
                        REAL(n4,SRK)*REAL(n5,SRK)*bsize,SRK)
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_pd5
!
!----------------------------------------
!> @brief Allocates rank 5 INTEGER pointer array.
!> @param a dummy argument for pointer array to be allocated
!> @param n1 size of first dimension
!> @param n2 size of second dimension
!> @param n3 size of third dimension
!> @param n4 size of fourth dimension
!> @param n5 size of fifth dimension
!>
!> Routine for allocating a rank 5 pointer array of type INTEGER
!> from 1 to \e n1, 1 to \e n2, 1 to \e n3, 1 to \e n3, 1 to \e n4,
!> and 1 to \e n5
    SUBROUTINE malloc_pi5(a,n1,n2,n3,n4,n5)
      INTEGER(SIK),INTENT(IN) :: n1,n2,n3,n4,n5
      INTEGER(SNK),POINTER :: a(:,:,:,:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ASSOCIATED(a)) THEN
        NULLIFY(a)
        IF(n1 >= 1_SIK .AND. n2 >= 1_SIK &
                       .AND. n3 >= 1_SIK .AND. n4 >= 1_SIK &
                                         .AND. n5 >= 1_SIK) THEN
          ALLOCATE(a(n1,n2,n3,n4,n5),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=0_SNK
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SNK,SRK)
          ELSE
            bsize=REAL(SNK,SRK)
            mysize=REAL(REAL(n1,SRK)*REAL(n2,SRK)*REAL(n3,SRK)* &
                        REAL(n4,SRK)*REAL(n5,SRK)*bsize,SRK)
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_pi5
!
!----------------------------------------
!> @brief Allocates rank 5 LONG integer pointer array.
!> @param a dummy argument for pointer array to be allocated
!> @param n1 size of first dimension
!> @param n2 size of second dimension
!> @param n3 size of third dimension
!> @param n4 size of fourth dimension
!> @param n5 size of fifth dimension
!>
!> Routine for allocating a rank 5 pointer array of type LONG integer
!> from 1 to \e n1, 1 to \e n2, 1 to \e n3, 1 to \e n3, 1 to \e n4,
!> and 1 to \e n5
    SUBROUTINE malloc_pl5(a,n1,n2,n3,n4,n5)
      INTEGER(SIK),INTENT(IN) :: n1,n2,n3,n4,n5
      INTEGER(SLK),POINTER :: a(:,:,:,:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ASSOCIATED(a)) THEN
        NULLIFY(a)
        IF(n1 >= 1_SIK .AND. n2 >= 1_SIK &
                       .AND. n3 >= 1_SIK .AND. n4 >= 1_SIK &
                                         .AND. n5 >= 1_SIK) THEN
          ALLOCATE(a(n1,n2,n3,n4,n5),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=0_SLK
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SLK,SRK)
          ELSE
            bsize=REAL(SLK,SRK)
            mysize=REAL(REAL(n1,SRK)*REAL(n2,SRK)*REAL(n3,SRK)* &
                        REAL(n4,SRK)*REAL(n5,SRK)*bsize,SRK)
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_pl5
!
!----------------------------------------
!> @brief Allocates rank 5 LOGICAL pointer array.
!> @param a dummy argument for pointer array to be allocated
!> @param n1 size of first dimension
!> @param n2 size of second dimension
!> @param n3 size of third dimension
!> @param n4 size of fourth dimension
!> @param n5 size of fifth dimension
!>
!> Routine for allocating a rank 5 pointer array of type LOGICAL
!> from 1 to \e n1, 1 to \e n2, 1 to \e n3, 1 to \e n3, 1 to \e n4,
!> and 1 to \e n5
    SUBROUTINE malloc_pb5(a,n1,n2,n3,n4,n5)
      INTEGER(SIK),INTENT(IN) :: n1,n2,n3,n4,n5
      LOGICAL(SBK),POINTER :: a(:,:,:,:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ASSOCIATED(a)) THEN
        NULLIFY(a)
        IF(n1 >= 1_SIK .AND. n2 >= 1_SIK &
                       .AND. n3 >= 1_SIK .AND. n4 >= 1_SIK &
                                         .AND. n5 >= 1_SIK) THEN
          ALLOCATE(a(n1,n2,n3,n4,n5),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=.FALSE.
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SBK,SRK)
          ELSE
            bsize=REAL(SBK,SRK)
            mysize=REAL(REAL(n1,SRK)*REAL(n2,SRK)*REAL(n3,SRK)* &
                        REAL(n4,SRK)*REAL(n5,SRK)*bsize,SRK)
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_pb5
!
!-------------------------------------------------------------------------------
! Allocate six dimensional pointers
!
!> @brief Allocates rank 6 SINGLE precision pointer array.
!> @param a dummy argument for pointer array to be allocated
!> @param n1 size of first dimension
!> @param n2 size of second dimension
!> @param n3 size of third dimension
!> @param n4 size of fourth dimension
!> @param n5 size of fifth dimension
!> @param n6 size of sixth dimension
!>
!> Routine for allocating a rank 6 pointer array of type SINGLE
!> precision from 1 to \e n1, 1 to \e n2, 1 to \e n3, 1 to \e n3,
!> 1 to \e n4, 1 to \e n5, and 1 to \e n6.
    SUBROUTINE malloc_ps6(a,n1,n2,n3,n4,n5,n6)
      INTEGER(SIK),INTENT(IN) :: n1,n2,n3,n4,n5,n6
      REAL(SSK),POINTER :: a(:,:,:,:,:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ASSOCIATED(a)) THEN
        NULLIFY(a)
        IF(n1 >= 1_SIK .AND. n2 >= 1_SIK &
                       .AND. n3 >= 1_SIK .AND. n4 >= 1_SIK &
                       .AND. n5 >= 1_SIK .AND. n6 >= 1_SIK) THEN
          ALLOCATE(a(n1,n2,n3,n4,n5,n6),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=0.0_SSK
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SSK,SRK)
          ELSE
            bsize=REAL(SSK,SRK)
            mysize=REAL(REAL(n1,SRK)*REAL(n2,SRK)*REAL(n3,SRK)* &
                        REAL(n4,SRK)*REAL(n5,SRK)*REAL(n6,SRK)*bsize,SRK)
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_ps6
!
!----------------------------------------
!> @brief Allocates rank 6 DOUBLE precision pointer array.
!> @param a dummy argument for pointer array to be allocated
!> @param n1 size of first dimension
!> @param n2 size of second dimension
!> @param n3 size of third dimension
!> @param n4 size of fourth dimension
!> @param n5 size of fifth dimension
!> @param n6 size of sixth dimension
!>
!> Routine for allocating a rank 6 pointer array of type DOUBLE
!> precision from 1 to \e n1, 1 to \e n2, 1 to \e n3, 1 to \e n3,
!> 1 to \e n4, 1 to \e n5, and 1 to \e n6.
    SUBROUTINE malloc_pd6(a,n1,n2,n3,n4,n5,n6)
      INTEGER(SIK),INTENT(IN) :: n1,n2,n3,n4,n5,n6
      REAL(SDK),POINTER :: a(:,:,:,:,:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ASSOCIATED(a)) THEN
        NULLIFY(a)
        IF(n1 >= 1_SIK .AND. n2 >= 1_SIK &
                       .AND. n3 >= 1_SIK .AND. n4 >= 1_SIK &
                       .AND. n5 >= 1_SIK .AND. n6 >= 1_SIK) THEN
          ALLOCATE(a(n1,n2,n3,n4,n5,n6),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=0.0_SDK
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SDK,SRK)
          ELSE
            bsize=REAL(SDK,SRK)
            mysize=REAL(REAL(n1,SRK)*REAL(n2,SRK)*REAL(n3,SRK)* &
                        REAL(n4,SRK)*REAL(n5,SRK)*REAL(n6,SRK)*bsize,SRK)
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_pd6
!
!----------------------------------------
!> @brief Allocates rank 6 INTEGER pointer array.
!> @param a dummy argument for pointer array to be allocated
!> @param n1 size of first dimension
!> @param n2 size of second dimension
!> @param n3 size of third dimension
!> @param n4 size of fourth dimension
!> @param n5 size of fifth dimension
!> @param n6 size of sixth dimension
!>
!> Routine for allocating a rank 6 pointer array of type INTEGER
!> from 1 to \e n1, 1 to \e n2, 1 to \e n3, 1 to \e n3, 1 to \e n4,
!> 1 to \e n5, and 1 to \e n6.
    SUBROUTINE malloc_pi6(a,n1,n2,n3,n4,n5,n6)
      INTEGER(SIK),INTENT(IN) :: n1,n2,n3,n4,n5,n6
      INTEGER(SNK),POINTER :: a(:,:,:,:,:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ASSOCIATED(a)) THEN
        NULLIFY(a)
        IF(n1 >= 1_SIK .AND. n2 >= 1_SIK &
                       .AND. n3 >= 1_SIK .AND. n4 >= 1_SIK &
                       .AND. n5 >= 1_SIK .AND. n6 >= 1_SIK) THEN
          ALLOCATE(a(n1,n2,n3,n4,n5,n6),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=0_SNK
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SNK,SRK)
          ELSE
            bsize=REAL(SNK,SRK)
            mysize=REAL(REAL(n1,SRK)*REAL(n2,SRK)*REAL(n3,SRK)* &
                        REAL(n4,SRK)*REAL(n5,SRK)*REAL(n6,SRK)*bsize,SRK)
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_pi6
!
!----------------------------------------
!> @brief Allocates rank 6 LONG integer pointer array.
!> @param a dummy argument for pointer array to be allocated
!> @param n1 size of first dimension
!> @param n2 size of second dimension
!> @param n3 size of third dimension
!> @param n4 size of fourth dimension
!> @param n5 size of fifth dimension
!> @param n6 size of sixth dimension
!>
!> Routine for allocating a rank 6 pointer array of type LONG integer
!> from 1 to \e n1, 1 to \e n2, 1 to \e n3, 1 to \e n3, 1 to \e n4,
!> 1 to \e n5, and 1 to \e n6.
    SUBROUTINE malloc_pl6(a,n1,n2,n3,n4,n5,n6)
      INTEGER(SIK),INTENT(IN) :: n1,n2,n3,n4,n5,n6
      INTEGER(SLK),POINTER :: a(:,:,:,:,:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ASSOCIATED(a)) THEN
        NULLIFY(a)
        IF(n1 >= 1_SIK .AND. n2 >= 1_SIK &
                       .AND. n3 >= 1_SIK .AND. n4 >= 1_SIK &
                       .AND. n5 >= 1_SIK .AND. n6 >= 1_SIK) THEN
          ALLOCATE(a(n1,n2,n3,n4,n5,n6),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=0_SLK
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SLK,SRK)
          ELSE
            bsize=REAL(SLK,SRK)
            mysize=REAL(REAL(n1,SRK)*REAL(n2,SRK)*REAL(n3,SRK)* &
                        REAL(n4,SRK)*REAL(n5,SRK)*REAL(n6,SRK)*bsize,SRK)
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_pl6
!
!----------------------------------------
!> @brief Allocates rank 6 LOGICAL pointer array.
!> @param a dummy argument for pointer array to be allocated
!> @param n1 size of first dimension
!> @param n2 size of second dimension
!> @param n3 size of third dimension
!> @param n4 size of fourth dimension
!> @param n5 size of fifth dimension
!> @param n6 size of sixth dimension
!>
!> Routine for allocating a rank 6 pointer array of type LOGICAL
!> from 1 to \e n1, 1 to \e n2, 1 to \e n3, 1 to \e n3, 1 to \e n4,
!> 1 to \e n5, and 1 to \e n6.
    SUBROUTINE malloc_pb6(a,n1,n2,n3,n4,n5,n6)
      INTEGER(SIK),INTENT(IN) :: n1,n2,n3,n4,n5,n6
      LOGICAL(SBK),POINTER :: a(:,:,:,:,:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ASSOCIATED(a)) THEN
        NULLIFY(a)
        IF(n1 >= 1_SIK .AND. n2 >= 1_SIK &
                       .AND. n3 >= 1_SIK .AND. n4 >= 1_SIK &
                       .AND. n5 >= 1_SIK .AND. n6 >= 1_SIK) THEN
          ALLOCATE(a(n1,n2,n3,n4,n5,n6),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=.FALSE.
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SBK,SRK)
          ELSE
            bsize=REAL(SBK,SRK)
            mysize=REAL(REAL(n1,SRK)*REAL(n2,SRK)*REAL(n3,SRK)* &
                        REAL(n4,SRK)*REAL(n5,SRK)*REAL(n6,SRK)*bsize,SRK)
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_pb6
!
!-------------------------------------------------------------------------------
! Allocate seven dimensional pointers
!
!> @brief Allocates rank 7 SINGLE precision pointer array.
!> @param a dummy argument for pointer array to be allocated
!> @param n1 size of first dimension
!> @param n2 size of second dimension
!> @param n3 size of third dimension
!> @param n4 size of fourth dimension
!> @param n5 size of fifth dimension
!> @param n6 size of sixth dimension
!> @param n7 size of seventh dimension
!>
!> Routine for allocating a rank 7 pointer array of type SINGLE
!> precision from 1 to \e n1, 1 to \e n2, 1 to \e n3, 1 to \e n3,
!> 1 to \e n4, 1 to \e n5,  1 to \e n6, and 1 to \e n7.
    SUBROUTINE malloc_ps7(a,n1,n2,n3,n4,n5,n6,n7)
      INTEGER(SIK),INTENT(IN) :: n1,n2,n3,n4,n5,n6,n7
      REAL(SSK),POINTER :: a(:,:,:,:,:,:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ASSOCIATED(a)) THEN
        NULLIFY(a)
        IF(n1 >= 1_SIK .AND. n2 >= 1_SIK &
                       .AND. n3 >= 1_SIK .AND. n4 >= 1_SIK &
                       .AND. n5 >= 1_SIK .AND. n6 >= 1_SIK &
                                         .AND. n7 >= 1_SIK) THEN
          ALLOCATE(a(n1,n2,n3,n4,n5,n6,n7),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=0.0_SSK
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SSK,SRK)
          ELSE
            bsize=REAL(SSK,SRK)
            mysize=REAL(REAL(n1,SRK)*REAL(n2,SRK)*REAL(n3,SRK)* &
                        REAL(n4,SRK)*REAL(n5,SRK)*REAL(n6,SRK)* &
                        REAL(n7,SRK)*bsize,SRK)
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_ps7
!
!----------------------------------------
!> @brief Allocates rank 7 DOUBLE precision pointer array.
!> @param a dummy argument for pointer array to be allocated
!> @param n1 size of first dimension
!> @param n2 size of second dimension
!> @param n3 size of third dimension
!> @param n4 size of fourth dimension
!> @param n5 size of fifth dimension
!> @param n6 size of sixth dimension
!> @param n7 size of seventh dimension
!>
!> Routine for allocating a rank 7 pointer array of type DOUBLE
!> precision from 1 to \e n1, 1 to \e n2, 1 to \e n3, 1 to \e n3,
!> 1 to \e n4, 1 to \e n5,  1 to \e n6, and 1 to \e n7.
    SUBROUTINE malloc_pd7(a,n1,n2,n3,n4,n5,n6,n7)
      INTEGER(SIK),INTENT(IN) :: n1,n2,n3,n4,n5,n6,n7
      REAL(SDK),POINTER :: a(:,:,:,:,:,:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ASSOCIATED(a)) THEN
        NULLIFY(a)
        IF(n1 >= 1_SIK .AND. n2 >= 1_SIK &
                       .AND. n3 >= 1_SIK .AND. n4 >= 1_SIK &
                       .AND. n5 >= 1_SIK .AND. n6 >= 1_SIK &
                                         .AND. n7 >= 1_SIK) THEN
          ALLOCATE(a(n1,n2,n3,n4,n5,n6,n7),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=0.0_SDK
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SDK,SRK)
          ELSE
            bsize=REAL(SDK,SRK)
            mysize=REAL(REAL(n1,SRK)*REAL(n2,SRK)*REAL(n3,SRK)* &
                        REAL(n4,SRK)*REAL(n5,SRK)*REAL(n6,SRK)* &
                        REAL(n7,SRK)*bsize,SRK)
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_pd7
!
!----------------------------------------
!> @brief Allocates rank 7 INTEGER pointer array.
!> @param a dummy argument for pointer array to be allocated
!> @param n1 size of first dimension
!> @param n2 size of second dimension
!> @param n3 size of third dimension
!> @param n4 size of fourth dimension
!> @param n5 size of fifth dimension
!> @param n6 size of sixth dimension
!> @param n7 size of seventh dimension
!>
!> Routine for allocating a rank 7 pointer array of type INTEGER
!> from 1 to \e n1, 1 to \e n2, 1 to \e n3, 1 to \e n3, 1 to \e n4,
!> 1 to \e n5, 1 to \e n6, and 1 to \e n7.
    SUBROUTINE malloc_pi7(a,n1,n2,n3,n4,n5,n6,n7)
      INTEGER(SIK),INTENT(IN) :: n1,n2,n3,n4,n5,n6,n7
      INTEGER(SNK),POINTER :: a(:,:,:,:,:,:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ASSOCIATED(a)) THEN
        NULLIFY(a)
        IF(n1 >= 1_SIK .AND. n2 >= 1_SIK &
                       .AND. n3 >= 1_SIK .AND. n4 >= 1_SIK &
                       .AND. n5 >= 1_SIK .AND. n6 >= 1_SIK &
                                         .AND. n7 >= 1_SIK) THEN
          ALLOCATE(a(n1,n2,n3,n4,n5,n6,n7),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=0_SNK
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SNK,SRK)
          ELSE
            bsize=REAL(SNK,SRK)
            mysize=REAL(REAL(n1,SRK)*REAL(n2,SRK)*REAL(n3,SRK)* &
                        REAL(n4,SRK)*REAL(n5,SRK)*REAL(n6,SRK)* &
                        REAL(n7,SRK)*bsize,SRK)
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_pi7
!
!----------------------------------------
!> @brief Allocates rank 7 LONG integer pointer array.
!> @param a dummy argument for pointer array to be allocated
!> @param n1 size of first dimension
!> @param n2 size of second dimension
!> @param n3 size of third dimension
!> @param n4 size of fourth dimension
!> @param n5 size of fifth dimension
!> @param n6 size of sixth dimension
!> @param n7 size of seventh dimension
!>
!> Routine for allocating a rank 7 pointer array of type LONG integer
!> from 1 to \e n1, 1 to \e n2, 1 to \e n3, 1 to \e n3, 1 to \e n4,
!> 1 to \e n5, 1 to \e n6, and 1 to \e n7.
    SUBROUTINE malloc_pl7(a,n1,n2,n3,n4,n5,n6,n7)
      INTEGER(SIK),INTENT(IN) :: n1,n2,n3,n4,n5,n6,n7
      INTEGER(SLK),POINTER :: a(:,:,:,:,:,:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ASSOCIATED(a)) THEN
        NULLIFY(a)
        IF(n1 >= 1_SIK .AND. n2 >= 1_SIK &
                       .AND. n3 >= 1_SIK .AND. n4 >= 1_SIK &
                       .AND. n5 >= 1_SIK .AND. n6 >= 1_SIK &
                                         .AND. n7 >= 1_SIK) THEN
          ALLOCATE(a(n1,n2,n3,n4,n5,n6,n7),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=0_SLK
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SLK,SRK)
          ELSE
            bsize=REAL(SLK,SRK)
            mysize=REAL(REAL(n1,SRK)*REAL(n2,SRK)*REAL(n3,SRK)* &
                        REAL(n4,SRK)*REAL(n5,SRK)*REAL(n6,SRK)* &
                        REAL(n7,SRK)*bsize,SRK)
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_pl7
!
!----------------------------------------
!> @brief Allocates rank 7 LOGICAL pointer array.
!> @param a dummy argument for pointer array to be allocated
!> @param n1 size of first dimension
!> @param n2 size of second dimension
!> @param n3 size of third dimension
!> @param n4 size of fourth dimension
!> @param n5 size of fifth dimension
!> @param n6 size of sixth dimension
!> @param n7 size of seventh dimension
!>
!> Routine for allocating a rank 7 pointer array of type LOGICAL from 1
!> to \e n1, 1 to \e n2, 1 to \e n3, 1 to \e n3, 1 to \e n4, 1 to \e n5, 
!> 1 to \e n6, and 1 to \e n7.
    SUBROUTINE malloc_pb7(a,n1,n2,n3,n4,n5,n6,n7)
      INTEGER(SIK),INTENT(IN) :: n1,n2,n3,n4,n5,n6,n7
      LOGICAL(SBK),POINTER :: a(:,:,:,:,:,:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ASSOCIATED(a)) THEN
        NULLIFY(a)
        IF(n1 >= 1_SIK .AND. n2 >= 1_SIK &
                       .AND. n3 >= 1_SIK .AND. n4 >= 1_SIK &
                       .AND. n5 >= 1_SIK .AND. n6 >= 1_SIK &
                                         .AND. n7 >= 1_SIK) THEN
          ALLOCATE(a(n1,n2,n3,n4,n5,n6,n7),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=.FALSE.
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SBK,SRK)
          ELSE
            bsize=REAL(SBK,SRK)
            mysize=REAL(REAL(n1,SRK)*REAL(n2,SRK)*REAL(n3,SRK)* &
                        REAL(n4,SRK)*REAL(n5,SRK)*REAL(n6,SRK)* &
                        REAL(n7,SRK)*bsize,SRK)
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_pb7
!
!===============================================================================
! The following section contains all procedures for allocating arrays from 1
! to n.
!
!> @brief Allocates rank 1 SINGLE precision allocatable array.
!> @param a dummy argument for allocatable array to be allocated
!> @param n1 size of first dimension
!>
!>Routine for allocating a rank 1 allocatable array of type SINGLE 
!>precision from 1 to \e n1.
    SUBROUTINE malloc_as1(a,n1)
      INTEGER(SIK),INTENT(IN) :: n1
      REAL(SSK),ALLOCATABLE,INTENT(INOUT) :: a(:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ALLOCATED(a)) THEN
        IF(n1 >= 1_SIK) THEN
          ALLOCATE(a(n1),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=0.0_SSK
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SSK,SRK)
          ELSE
            bsize=REAL(SSK,SRK)
            mysize=REAL(REAL(n1,SRK)*bsize,SRK)
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_as1
!
!----------------------------------------
!> @brief Allocates rank 1 DOUBLE precision allocatable array.
!> @param a dummy argument for allocatable array to be allocated
!> @param n1 size of first dimension
!>
!> Routine for allocating a rank 1 allocatable array of type DOUBLE 
!> precision from 1 to \e n1.
    SUBROUTINE malloc_ad1(a,n1)
      INTEGER(SIK),INTENT(IN) :: n1
      REAL(SDK),ALLOCATABLE,INTENT(INOUT) :: a(:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ALLOCATED(a)) THEN
        IF(n1 >= 1_SIK) THEN
          ALLOCATE(a(n1),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=0.0_SDK
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SDK,SRK)
          ELSE
            bsize=REAL(SDK,SRK)
            mysize=REAL(REAL(n1,SRK)*bsize,SRK)
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_ad1
!
!----------------------------------------
!> @brief Allocates rank 1 INTEGER allocatable array.
!> @param a dummy argument for allocatable array to be allocated
!> @param n1 size of first dimension
!>
!> Routine for allocating a rank 1 allocatable array of type INTEGER 
!> from 1 to \e n1.
    SUBROUTINE malloc_ai1(a,n1)
      INTEGER(SIK),INTENT(IN) :: n1
      INTEGER(SNK),ALLOCATABLE,INTENT(INOUT) :: a(:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ALLOCATED(a)) THEN
        IF(n1 >= 1_SIK) THEN
          ALLOCATE(a(n1),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=0_SNK
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SNK,SRK)
          ELSE
            bsize=REAL(SNK,SRK)
            mysize=REAL(REAL(n1,SRK)*bsize,SRK)
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_ai1
!
!----------------------------------------
!> @brief Allocates rank 1 LONG integer allocatable array.
!> @param a dummy argument for allocatable array to be allocated
!> @param n1 size of first dimension
!>
!> Routine for allocating a rank 1 allocatable array of type LONG integer 
!> from 1 to \e n1.
    SUBROUTINE malloc_al1(a,n1)
      INTEGER(SIK),INTENT(IN) :: n1
      INTEGER(SLK),ALLOCATABLE,INTENT(INOUT) :: a(:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ALLOCATED(a)) THEN
        IF(n1 >= 1_SIK) THEN
          ALLOCATE(a(n1),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=0_SLK
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SLK,SRK)
          ELSE
            bsize=REAL(SLK,SRK)
            mysize=REAL(REAL(n1,SRK)*bsize,SRK)
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_al1
!
!----------------------------------------
!> @brief Allocates rank 1 LOGICAL allocatable array.
!> @param a dummy argument for allocatable array to be allocated
!> @param n1 size of first dimension
!>
!> Routine for allocating a rank 1 allocatable array of type LOGICAL 
!> from 1 to \e n1.
    SUBROUTINE malloc_ab1(a,n1)
      INTEGER(SIK),INTENT(IN) :: n1
      LOGICAL(SBK),ALLOCATABLE,INTENT(INOUT) :: a(:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ALLOCATED(a)) THEN
        IF(n1 >= 1_SIK) THEN
          ALLOCATE(a(n1),STAT=alloc_err)
           IF(alloc_err == 0_SIK) THEN
            a=.FALSE.
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SBK,SRK)
          ELSE
            bsize=REAL(SBK,SRK)
            mysize=REAL(REAL(n1,SRK)*bsize,SRK)
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_ab1
!
!-------------------------------------------------------------------------------
! Allocate two dimensional pointers
!
!> @brief Allocates rank 2 SINGLE precision allocatable array.
!> @param a dummy argument for allocatable array to be allocated
!> @param n1 size of first dimension
!> @param n2 size of second dimension
!>
!> Routine for allocating a rank 2 allocatable array of type SINGLE 
!> precision from 1 to \e n1 and 1 to \e n2
    SUBROUTINE malloc_as2(a,n1,n2)
      INTEGER(SIK),INTENT(IN) :: n1,n2
      REAL(SSK),ALLOCATABLE,INTENT(INOUT) :: a(:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ALLOCATED(a)) THEN
        IF(n1 >= 1_SIK .AND. n2 >= 1_SIK) THEN
          ALLOCATE(a(n1,n2),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=0.0_SSK
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SSK,SRK)
          ELSE
            bsize=REAL(SSK,SRK)
            mysize=REAL(REAL(n1,SRK)*REAL(n2,SRK)*bsize,SRK)
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_as2
!
!----------------------------------------
!> @brief Allocates rank 2 DOUBLE precision allocatable array.
!> @param a dummy argument for allocatable array to be allocated
!> @param n1 size of first dimension
!> @param n2 size of second dimension
!>
!> Routine for allocating a rank 2 allocatable array of type DOUBLE 
!> precision from 1 to \e n1 and 1 to \e n2
    SUBROUTINE malloc_ad2(a,n1,n2)
      INTEGER(SIK),INTENT(IN) :: n1,n2
      REAL(SDK),ALLOCATABLE,INTENT(INOUT) :: a(:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ALLOCATED(a)) THEN
        IF(n1 >= 1_SIK .AND. n2 >= 1_SIK) THEN
          ALLOCATE(a(n1,n2),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=0.0_SDK
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SDK,SRK)
          ELSE
            bsize=REAL(SDK,SRK)
            mysize=REAL(REAL(n1,SRK)*REAL(n2,SRK)*bsize,SRK)
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_ad2
!
!----------------------------------------
!> @brief Allocates rank 2 INTEGER allocatable array.
!> @param a dummy argument for allocatable array to be allocated
!> @param n1 size of first dimension
!> @param n2 size of second dimension
!>
!> Routine for allocating a rank 2 allocatable array of type INTEGER
!> from 1 to \e n1 and 1 to \e n2
    SUBROUTINE malloc_ai2(a,n1,n2)
      INTEGER(SIK),INTENT(IN) :: n1,n2
      INTEGER(SNK),ALLOCATABLE,INTENT(INOUT) :: a(:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ALLOCATED(a)) THEN
        IF(n1 >= 1_SIK .AND. n2 >= 1_SIK) THEN
          ALLOCATE(a(n1,n2),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=0_SNK
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SNK,SRK)
          ELSE
            bsize=REAL(SNK,SRK)
            mysize=REAL(REAL(n1,SRK)*REAL(n2,SRK)*bsize,SRK)
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_ai2
!
!----------------------------------------
!> @brief Allocates rank 2 LONG integer allocatable array.
!> @param a dummy argument for allocatable array to be allocated
!> @param n1 size of first dimension
!> @param n2 size of second dimension
!>
!> Routine for allocating a rank 2 allocatable array of type LONG integer
!> from 1 to \e n1 and 1 to \e n2
    SUBROUTINE malloc_al2(a,n1,n2)
      INTEGER(SIK),INTENT(IN) :: n1,n2
      INTEGER(SLK),ALLOCATABLE,INTENT(INOUT) :: a(:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ALLOCATED(a)) THEN
        IF(n1 >= 1_SIK .AND. n2 >= 1_SIK) THEN
          ALLOCATE(a(n1,n2),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=0_SLK
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SLK,SRK)
          ELSE
            bsize=REAL(SlK,SRK)
            mysize=REAL(REAL(n1,SRK)*REAL(n2,SRK)*bsize,SRK)
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_al2
!
!----------------------------------------
!> @brief Allocates rank 2 LOGICAL allocatable array.
!> @param a dummy argument for allocatable array to be allocated
!> @param n1 size of first dimension
!> @param n2 size of second dimension
!>
!> Routine for allocating a rank 2 allocatable array of type LOGICAL
!> from 1 to \e n1 and 1 to \e n2
    SUBROUTINE malloc_ab2(a,n1,n2)
      INTEGER(SIK),INTENT(IN) :: n1,n2
      LOGICAL(SBK),ALLOCATABLE,INTENT(INOUT) :: a(:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ALLOCATED(a)) THEN
        IF(n1 >= 1_SIK .AND. n2 >= 1_SIK) THEN
          ALLOCATE(a(n1,n2),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=.FALSE.
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SBK,SRK)
          ELSE
            bsize=REAL(SBK,SRK)
            mysize=REAL(REAL(n1,SRK)*REAL(n2,SRK)*bsize,SRK)
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_ab2
!
!-------------------------------------------------------------------------------
! Allocate three dimensional pointers
!
!> @brief Allocates rank 3 SINGLE precision allocatable array.
!> @param a dummy argument for allocatable array to be allocated
!> @param n1 size of first dimension
!> @param n2 size of second dimension
!> @param n3 size of third dimension
!>
!> Routine for allocating a rank 3 allocatable array of type SINGLE
!> precision from 1 to \e n1, 1 to \e n2, and 1 to \e n3
    SUBROUTINE malloc_as3(a,n1,n2,n3)
      INTEGER(SIK),INTENT(IN) :: n1,n2,n3
      REAL(SSK),ALLOCATABLE,INTENT(INOUT) :: a(:,:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ALLOCATED(a)) THEN
        IF(n1 >= 1_SIK .AND. n2 >= 1_SIK .AND. n3 >= 1_SIK) THEN
          ALLOCATE(a(n1,n2,n3),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=0.0_SSK
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SSK,SRK)
          ELSE
            bsize=REAL(SSK,SRK)
            mysize=REAL(REAL(n1,SRK)*REAL(n2,SRK)*REAL(n3,SRK)*bsize,SRK)
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_as3
!
!----------------------------------------
!> @brief Allocates rank 3 DOUBLE precision allocatable array.
!> @param a dummy argument for allocatable array to be allocated
!> @param n1 size of first dimension
!> @param n2 size of second dimension
!> @param n3 size of third dimension
!>
!> Routine for allocating a rank 3 allocatable array of type DOUBLE
!> precision from 1 to \e n1, 1 to \e n2, and 1 to \e n3
    SUBROUTINE malloc_ad3(a,n1,n2,n3)
      INTEGER(SIK),INTENT(IN) :: n1,n2,n3
      REAL(SDK),ALLOCATABLE,INTENT(INOUT) :: a(:,:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ALLOCATED(a)) THEN
        IF(n1 >= 1_SIK .AND. n2 >= 1_SIK .AND. n3 >= 1_SIK) THEN
          ALLOCATE(a(n1,n2,n3),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=0.0_SDK
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SDK,SRK)
          ELSE
            bsize=REAL(SDK,SRK)
            mysize=REAL(REAL(n1,SRK)*REAL(n2,SRK)*REAL(n3,SRK)*bsize,SRK)
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_ad3
!
!----------------------------------------
!> @brief Allocates rank 3 INTEGER allocatable array.
!> @param a dummy argument for allocatable array to be allocated
!> @param n1 size of first dimension
!> @param n2 size of second dimension
!> @param n3 size of third dimension
!>
!> Routine for allocating a rank 3 allocatable array of type INTEGER
!> from 1 to \e n1, 1 to \e n2, and 1 to \e n3
    SUBROUTINE malloc_ai3(a,n1,n2,n3)
      INTEGER(SIK),INTENT(IN) :: n1,n2,n3
      INTEGER(SNK),ALLOCATABLE,INTENT(INOUT) :: a(:,:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ALLOCATED(a)) THEN
        IF(n1 >= 1_SIK .AND. n2 >= 1_SIK .AND. n3 >= 1_SIK) THEN
          ALLOCATE(a(n1,n2,n3),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=0_SNK
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SNK,SRK)
          ELSE
            bsize=REAL(SNK,SRK)
            mysize=REAL(REAL(n1,SRK)*REAL(n2,SRK)*REAL(n3,SRK)*bsize,SRK)
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_ai3
!
!----------------------------------------
!> @brief Allocates rank 3 LONG integer allocatable array.
!> @param a dummy argument for allocatable array to be allocated
!> @param n1 size of first dimension
!> @param n2 size of second dimension
!> @param n3 size of third dimension
!>
!> Routine for allocating a rank 3 allocatable array of type LONG integer
!> from 1 to \e n1, 1 to \e n2, and 1 to \e n3
    SUBROUTINE malloc_al3(a,n1,n2,n3)
      INTEGER(SIK),INTENT(IN) :: n1,n2,n3
      INTEGER(SLK),ALLOCATABLE,INTENT(INOUT) :: a(:,:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ALLOCATED(a)) THEN
        IF(n1 >= 1_SIK .AND. n2 >= 1_SIK .AND. n3 >= 1_SIK) THEN
          ALLOCATE(a(n1,n2,n3),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=0_SLK
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SLK,SRK)
          ELSE
            bsize=REAL(SLK,SRK)
            mysize=REAL(REAL(n1,SRK)*REAL(n2,SRK)*REAL(n3,SRK)*bsize,SRK)
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_al3
!
!----------------------------------------
!> @brief Allocates rank 3 LOGICAL allocatable array.
!> @param a dummy argument for allocatable array to be allocated
!> @param n1 size of first dimension
!> @param n2 size of second dimension
!> @param n3 size of third dimension
!>
!> Routine for allocating a rank 3 allocatable array of type LOGICAL
!> from 1 to \e n1, 1 to \e n2, and 1 to \e n3
    SUBROUTINE malloc_ab3(a,n1,n2,n3)
      INTEGER(SIK),INTENT(IN) :: n1,n2,n3
      LOGICAL(SBK),ALLOCATABLE,INTENT(INOUT) :: a(:,:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ALLOCATED(a)) THEN
        IF(n1 >= 1_SIK .AND. n2 >= 1_SIK .AND. n3 >= 1_SIK) THEN
          ALLOCATE(a(n1,n2,n3),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=.FALSE.
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SBK,SRK)
          ELSE
            bsize=REAL(SBK,SRK)
            mysize=REAL(REAL(n1,SRK)*REAL(n2,SRK)*REAL(n3,SRK)*bsize,SRK)
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_ab3
!
!-------------------------------------------------------------------------------
! Allocate four dimensional pointers
!
!> @brief Allocates rank 4 SINGLE precision allocatable array.
!> @param a dummy argument for allocatable array to be allocated
!> @param n1 size of first dimension
!> @param n2 size of second dimension
!> @param n3 size of third dimension
!> @param n4 size of fourth dimension
!>
!> Routine for allocating a rank 4 allocatable array of type SINGLE
!> precision from 1 to \e n1, 1 to \e n2, 1 to \e n3, and 1 to \e n4
    SUBROUTINE malloc_as4(a,n1,n2,n3,n4)
      INTEGER(SIK),INTENT(IN) :: n1,n2,n3,n4
      REAL(SSK),ALLOCATABLE,INTENT(INOUT) :: a(:,:,:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ALLOCATED(a)) THEN
        IF(n1 >= 1_SIK .AND. n2 >= 1_SIK .AND. &
                             n3 >= 1_SIK .AND. n4 >= 1_SIK) THEN
          ALLOCATE(a(n1,n2,n3,n4),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=0.0_SSK
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SSK,SRK)
          ELSE
            bsize=REAL(SSK,SRK)
            mysize=REAL(REAL(n1,SRK)*REAL(n2,SRK)*REAL(n3,SRK)* &
                        REAL(n4,SRK)*bsize,SRK)
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_as4
!
!----------------------------------------
!> @brief Allocates rank 4 DOUBLE precision allocatable array.
!> @param a dummy argument for allocatable array to be allocated
!> @param n1 size of first dimension
!> @param n2 size of second dimension
!> @param n3 size of third dimension
!> @param n4 size of fourth dimension
!>
!> Routine for allocating a rank 4 allocatable array of type DOUBLE
!> precision from 1 to \e n1, 1 to \e n2, 1 to \e n3, and 1 to \e n4
    SUBROUTINE malloc_ad4(a,n1,n2,n3,n4)
      INTEGER(SIK),INTENT(IN) :: n1,n2,n3,n4
      REAL(SDK),ALLOCATABLE,INTENT(INOUT) :: a(:,:,:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ALLOCATED(a)) THEN
        IF(n1 >= 1_SIK .AND. n2 >= 1_SIK .AND. &
                             n3 >= 1_SIK .AND. n4 >= 1_SIK) THEN
          ALLOCATE(a(n1,n2,n3,n4),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=0.0_SDK
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SDK,SRK)
          ELSE
            bsize=REAL(SDK,SRK)
            mysize=REAL(REAL(n1,SRK)*REAL(n2,SRK)*REAL(n3,SRK)* &
                        REAL(n4,SRK)*bsize,SRK)
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_ad4
!
!----------------------------------------
!> @brief Allocates rank 4 INTEGER allocatable array.
!> @param a dummy argument for allocatable array to be allocated
!> @param n1 size of first dimension
!> @param n2 size of second dimension
!> @param n3 size of third dimension
!> @param n4 size of fourth dimension
!>
!> Routine for allocating a rank 4 allocatable array of type INTEGER
!> from 1 to \e n1, 1 to \e n2, 1 to \e n3, and 1 to \e n4
    SUBROUTINE malloc_ai4(a,n1,n2,n3,n4)
      INTEGER(SIK),INTENT(IN) :: n1,n2,n3,n4
      INTEGER(SNK),ALLOCATABLE,INTENT(INOUT) :: a(:,:,:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ALLOCATED(a)) THEN
        IF(n1 >= 1_SIK .AND. n2 >= 1_SIK .AND. &
                             n3 >= 1_SIK .AND. n4 >= 1_SIK) THEN
          ALLOCATE(a(n1,n2,n3,n4),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=0_SNK
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SNK,SRK)
          ELSE
            bsize=REAL(SNK,SRK)
            mysize=REAL(REAL(n1,SRK)*REAL(n2,SRK)*REAL(n3,SRK)* &
                        REAL(n4,SRK)*bsize,SRK)
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_ai4
!
!----------------------------------------
!> @brief Allocates rank 4 LONG integer allocatable array.
!> @param a dummy argument for allocatable array to be allocated
!> @param n1 size of first dimension
!> @param n2 size of second dimension
!> @param n3 size of third dimension
!> @param n4 size of fourth dimension
!>
!> Routine for allocating a rank 4 allocatable array of type LONG integer
!> from 1 to \e n1, 1 to \e n2, 1 to \e n3, and 1 to \e n4
    SUBROUTINE malloc_al4(a,n1,n2,n3,n4)
      INTEGER(SIK),INTENT(IN) :: n1,n2,n3,n4
      INTEGER(SLK),ALLOCATABLE,INTENT(INOUT) :: a(:,:,:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ALLOCATED(a)) THEN
        IF(n1 >= 1_SIK .AND. n2 >= 1_SIK .AND. &
                             n3 >= 1_SIK .AND. n4 >= 1_SIK) THEN
          ALLOCATE(a(n1,n2,n3,n4),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=0_SLK
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SLK,SRK)
          ELSE
            bsize=REAL(SLK,SRK)
            mysize=REAL(REAL(n1,SRK)*REAL(n2,SRK)*REAL(n3,SRK)* &
                        REAL(n4,SRK)*bsize,SRK)
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_al4
!
!----------------------------------------
!> @brief Allocates rank 4 LOGICAL allocatable array.
!> @param a dummy argument for allocatable array to be allocated
!> @param n1 size of first dimension
!> @param n2 size of second dimension
!> @param n3 size of third dimension
!> @param n4 size of fourth dimension
!>
!> Routine for allocating a rank 4 allocatable array of type LOGICAL
!> from 1 to \e n1, 1 to \e n2, 1 to \e n3, and 1 to \e n4
    SUBROUTINE malloc_ab4(a,n1,n2,n3,n4)
      INTEGER(SIK),INTENT(IN) :: n1,n2,n3,n4
      LOGICAL(SBK),ALLOCATABLE,INTENT(INOUT) :: a(:,:,:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ALLOCATED(a)) THEN
        IF(n1 >= 1_SIK .AND. n2 >= 1_SIK .AND. &
                             n3 >= 1_SIK .AND. n4 >= 1_SIK) THEN
          ALLOCATE(a(n1,n2,n3,n4),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=.FALSE.
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SBK,SRK)
          ELSE
            bsize=REAL(SBK,SRK)
            mysize=REAL(REAL(n1,SRK)*REAL(n2,SRK)*REAL(n3,SRK)* &
                        REAL(n4,SRK)*bsize,SRK)
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_ab4
!
!-------------------------------------------------------------------------------
! Allocate five dimensional pointers
!
!> @brief Allocates rank 5 SINGLE precision allocatable array.
!> @param a dummy argument for allocatable array to be allocated
!> @param n1 size of first dimension
!> @param n2 size of second dimension
!> @param n3 size of third dimension
!> @param n4 size of fourth dimension
!> @param n5 size of fifth dimension
!>
!> Routine for allocating a rank 5 allocatable array of type SINGLE
!> precision from 1 to \e n1, 1 to \e n2, 1 to \e n3, 1 to \e n3, 
!> 1 to \e n4, and 1 to \e n5
    SUBROUTINE malloc_as5(a,n1,n2,n3,n4,n5)
      INTEGER(SIK),INTENT(IN) :: n1,n2,n3,n4,n5
      REAL(SSK),ALLOCATABLE,INTENT(INOUT) :: a(:,:,:,:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ALLOCATED(a)) THEN
        IF(n1 >= 1_SIK .AND. n2 >= 1_SIK &
                       .AND. n3 >= 1_SIK .AND. n4 >= 1_SIK &
                                         .AND. n5 >= 1_SIK) THEN
          ALLOCATE(a(n1,n2,n3,n4,n5),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=0.0_SSK
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SSK,SRK)
          ELSE
            bsize=REAL(SSK,SRK)
            mysize=REAL(REAL(n1,SRK)*REAL(n2,SRK)*REAL(n3,SRK)* &
                        REAL(n4,SRK)*REAL(n5,SRK)*bsize,SRK)
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_as5
!
!----------------------------------------
!> @brief Allocates rank 5 DOUBLE precision allocatable array.
!> @param a dummy argument for allocatable array to be allocated
!> @param n1 size of first dimension
!> @param n2 size of second dimension
!> @param n3 size of third dimension
!> @param n4 size of fourth dimension
!> @param n5 size of fifth dimension
!>
!> Routine for allocating a rank 5 allocatable array of type DOUBLE
!> precision from 1 to \e n1, 1 to \e n2, 1 to \e n3, 1 to \e n3,
!> 1 to \e n4, and 1 to \e n5
    SUBROUTINE malloc_ad5(a,n1,n2,n3,n4,n5)
      INTEGER(SIK),INTENT(IN) :: n1,n2,n3,n4,n5
      REAL(SDK),ALLOCATABLE,INTENT(INOUT) :: a(:,:,:,:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ALLOCATED(a)) THEN
        IF(n1 >= 1_SIK .AND. n2 >= 1_SIK &
                       .AND. n3 >= 1_SIK .AND. n4 >= 1_SIK &
                                         .AND. n5 >= 1_SIK) THEN
          ALLOCATE(a(n1,n2,n3,n4,n5),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=0.0_SDK
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SDK,SRK)
          ELSE
            bsize=REAL(SDK,SRK)
            mysize=REAL(REAL(n1,SRK)*REAL(n2,SRK)*REAL(n3,SRK)* &
                        REAL(n4,SRK)*REAL(n5,SRK)*bsize,SRK)
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_ad5
!
!----------------------------------------
!> @brief Allocates rank 5 INTEGER allocatable array.
!> @param a dummy argument for allocatable array to be allocated
!> @param n1 size of first dimension
!> @param n2 size of second dimension
!> @param n3 size of third dimension
!> @param n4 size of fourth dimension
!> @param n5 size of fifth dimension
!>
!> Routine for allocating a rank 5 allocatable array of type INTEGER
!> from 1 to \e n1, 1 to \e n2, 1 to \e n3, 1 to \e n3, 1 to \e n4,
!> and 1 to \e n5
    SUBROUTINE malloc_ai5(a,n1,n2,n3,n4,n5)
      INTEGER(SIK),INTENT(IN) :: n1,n2,n3,n4,n5
      INTEGER(SNK),ALLOCATABLE,INTENT(INOUT) :: a(:,:,:,:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ALLOCATED(a)) THEN
        IF(n1 >= 1_SIK .AND. n2 >= 1_SIK &
                       .AND. n3 >= 1_SIK .AND. n4 >= 1_SIK &
                                         .AND. n5 >= 1_SIK) THEN
          ALLOCATE(a(n1,n2,n3,n4,n5),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=0_SNK
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SNK,SRK)
          ELSE
            bsize=REAL(SNK,SRK)
            mysize=REAL(REAL(n1,SRK)*REAL(n2,SRK)*REAL(n3,SRK)* &
                        REAL(n4,SRK)*REAL(n5,SRK)*bsize,SRK)
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_ai5
!
!----------------------------------------
!> @brief Allocates rank 5 LONG integer allocatable array.
!> @param a dummy argument for allocatable array to be allocated
!> @param n1 size of first dimension
!> @param n2 size of second dimension
!> @param n3 size of third dimension
!> @param n4 size of fourth dimension
!> @param n5 size of fifth dimension
!>
!> Routine for allocating a rank 5 allocatable array of type LONG integer
!> from 1 to \e n1, 1 to \e n2, 1 to \e n3, 1 to \e n3, 1 to \e n4,
!> and 1 to \e n5
    SUBROUTINE malloc_al5(a,n1,n2,n3,n4,n5)
      INTEGER(SIK),INTENT(IN) :: n1,n2,n3,n4,n5
      INTEGER(SLK),ALLOCATABLE,INTENT(INOUT) :: a(:,:,:,:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ALLOCATED(a)) THEN
        IF(n1 >= 1_SIK .AND. n2 >= 1_SIK &
                       .AND. n3 >= 1_SIK .AND. n4 >= 1_SIK &
                                         .AND. n5 >= 1_SIK) THEN
          ALLOCATE(a(n1,n2,n3,n4,n5),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=0_SLK
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SLK,SRK)
          ELSE
            bsize=REAL(SLK,SRK)
            mysize=REAL(REAL(n1,SRK)*REAL(n2,SRK)*REAL(n3,SRK)* &
                        REAL(n4,SRK)*REAL(n5,SRK)*bsize,SRK)
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_al5
!
!----------------------------------------
!> @brief Allocates rank 5 LOGICAL allocatable array.
!> @param a dummy argument for allocatable array to be allocated
!> @param n1 size of first dimension
!> @param n2 size of second dimension
!> @param n3 size of third dimension
!> @param n4 size of fourth dimension
!> @param n5 size of fifth dimension
!>
!> Routine for allocating a rank 5 allocatable array of type LOGICAL
!> from 1 to \e n1, 1 to \e n2, 1 to \e n3, 1 to \e n3, 1 to \e n4,
!> and 1 to \e n5
    SUBROUTINE malloc_ab5(a,n1,n2,n3,n4,n5)
      INTEGER(SIK),INTENT(IN) :: n1,n2,n3,n4,n5
      LOGICAL(SBK),ALLOCATABLE,INTENT(INOUT) :: a(:,:,:,:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ALLOCATED(a)) THEN
        IF(n1 >= 1_SIK .AND. n2 >= 1_SIK &
                       .AND. n3 >= 1_SIK .AND. n4 >= 1_SIK &
                                         .AND. n5 >= 1_SIK) THEN
          ALLOCATE(a(n1,n2,n3,n4,n5),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=.FALSE.
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SBK,SRK)
          ELSE
            bsize=REAL(SBK,SRK)
            mysize=REAL(REAL(n1,SRK)*REAL(n2,SRK)*REAL(n3,SRK)* &
                        REAL(n4,SRK)*REAL(n5,SRK)*bsize,SRK)
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_ab5
!
!-------------------------------------------------------------------------------
! Allocate six dimensional pointers
!
!> @brief Allocates rank 6 SINGLE precision allocatable array.
!> @param a dummy argument for allocatable array to be allocated
!> @param n1 size of first dimension
!> @param n2 size of second dimension
!> @param n3 size of third dimension
!> @param n4 size of fourth dimension
!> @param n5 size of fifth dimension
!> @param n6 size of sixth dimension
!>
!> Routine for allocating a rank 6 allocatable array of type SINGLE
!> precision from 1 to \e n1, 1 to \e n2, 1 to \e n3, 1 to \e n3,
!> 1 to \e n4, 1 to \e n5, and 1 to \e n6.
    SUBROUTINE malloc_as6(a,n1,n2,n3,n4,n5,n6)
      INTEGER(SIK),INTENT(IN) :: n1,n2,n3,n4,n5,n6
      REAL(SSK),ALLOCATABLE,INTENT(INOUT) :: a(:,:,:,:,:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ALLOCATED(a)) THEN
        IF(n1 >= 1_SIK .AND. n2 >= 1_SIK &
                       .AND. n3 >= 1_SIK .AND. n4 >= 1_SIK &
                       .AND. n5 >= 1_SIK .AND. n6 >= 1_SIK) THEN
          ALLOCATE(a(n1,n2,n3,n4,n5,n6),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=0.0_SSK
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SSK,SRK)
          ELSE
            bsize=REAL(SSK,SRK)
            mysize=REAL(REAL(n1,SRK)*REAL(n2,SRK)*REAL(n3,SRK)* &
                        REAL(n4,SRK)*REAL(n5,SRK)*REAL(n6,SRK)*bsize,SRK)
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_as6
!
!----------------------------------------
!> @brief Allocates rank 6 DOUBLE precision allocatable array.
!> @param a dummy argument for allocatable array to be allocated
!> @param n1 size of first dimension
!> @param n2 size of second dimension
!> @param n3 size of third dimension
!> @param n4 size of fourth dimension
!> @param n5 size of fifth dimension
!> @param n6 size of sixth dimension
!>
!> Routine for allocating a rank 6 allocatable array of type DOUBLE
!> precision from 1 to \e n1, 1 to \e n2, 1 to \e n3, 1 to \e n3,
!> 1 to \e n4, 1 to \e n5, and 1 to \e n6.
    SUBROUTINE malloc_ad6(a,n1,n2,n3,n4,n5,n6)
      INTEGER(SIK),INTENT(IN) :: n1,n2,n3,n4,n5,n6
      REAL(SDK),ALLOCATABLE,INTENT(INOUT) :: a(:,:,:,:,:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ALLOCATED(a)) THEN
        IF(n1 >= 1_SIK .AND. n2 >= 1_SIK &
                       .AND. n3 >= 1_SIK .AND. n4 >= 1_SIK &
                       .AND. n5 >= 1_SIK .AND. n6 >= 1_SIK) THEN
          ALLOCATE(a(n1,n2,n3,n4,n5,n6),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=0.0_SDK
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SDK,SRK)
          ELSE
            bsize=REAL(SDK,SRK)
            mysize=REAL(REAL(n1,SRK)*REAL(n2,SRK)*REAL(n3,SRK)* &
                        REAL(n4,SRK)*REAL(n5,SRK)*REAL(n6,SRK)*bsize,SRK)
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_ad6
!
!----------------------------------------
!> @brief Allocates rank 6 INTEGER allocatable array.
!> @param a dummy argument for allocatable array to be allocated
!> @param n1 size of first dimension
!> @param n2 size of second dimension
!> @param n3 size of third dimension
!> @param n4 size of fourth dimension
!> @param n5 size of fifth dimension
!> @param n6 size of sixth dimension
!>
!> Routine for allocating a rank 6 allocatable array of type INTEGER
!> from 1 to \e n1, 1 to \e n2, 1 to \e n3, 1 to \e n3, 1 to \e n4,
!> 1 to \e n5, and 1 to \e n6.
    SUBROUTINE malloc_ai6(a,n1,n2,n3,n4,n5,n6)
      INTEGER(SIK),INTENT(IN) :: n1,n2,n3,n4,n5,n6
      INTEGER(SNK),ALLOCATABLE,INTENT(INOUT) :: a(:,:,:,:,:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ALLOCATED(a)) THEN
        IF(n1 >= 1_SIK .AND. n2 >= 1_SIK &
                       .AND. n3 >= 1_SIK .AND. n4 >= 1_SIK &
                       .AND. n5 >= 1_SIK .AND. n6 >= 1_SIK) THEN
          ALLOCATE(a(n1,n2,n3,n4,n5,n6),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=0_SNK
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SNK,SRK)
          ELSE
            bsize=REAL(SNK,SRK)
            mysize=REAL(REAL(n1,SRK)*REAL(n2,SRK)*REAL(n3,SRK)* &
                        REAL(n4,SRK)*REAL(n5,SRK)*REAL(n6,SRK)*bsize,SRK)
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_ai6
!
!----------------------------------------
!> @brief Allocates rank 6 LONG integer allocatable array.
!> @param a dummy argument for allocatable array to be allocated
!> @param n1 size of first dimension
!> @param n2 size of second dimension
!> @param n3 size of third dimension
!> @param n4 size of fourth dimension
!> @param n5 size of fifth dimension
!> @param n6 size of sixth dimension
!>
!> Routine for allocating a rank 6 allocatable array of type LONG integer
!> from 1 to \e n1, 1 to \e n2, 1 to \e n3, 1 to \e n3, 1 to \e n4,
!> 1 to \e n5, and 1 to \e n6.
    SUBROUTINE malloc_al6(a,n1,n2,n3,n4,n5,n6)
      INTEGER(SIK),INTENT(IN) :: n1,n2,n3,n4,n5,n6
      INTEGER(SLK),ALLOCATABLE,INTENT(INOUT) :: a(:,:,:,:,:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ALLOCATED(a)) THEN
        IF(n1 >= 1_SIK .AND. n2 >= 1_SIK &
                       .AND. n3 >= 1_SIK .AND. n4 >= 1_SIK &
                       .AND. n5 >= 1_SIK .AND. n6 >= 1_SIK) THEN
          ALLOCATE(a(n1,n2,n3,n4,n5,n6),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=0_SLK
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SLK,SRK)
          ELSE
            bsize=REAL(SLK,SRK)
            mysize=REAL(REAL(n1,SRK)*REAL(n2,SRK)*REAL(n3,SRK)* &
                        REAL(n4,SRK)*REAL(n5,SRK)*REAL(n6,SRK)*bsize,SRK)
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_al6
!
!----------------------------------------
!> @brief Allocates rank 6 LOGICAL allocatable array.
!> @param a dummy argument for allocatable array to be allocated
!> @param n1 size of first dimension
!> @param n2 size of second dimension
!> @param n3 size of third dimension
!> @param n4 size of fourth dimension
!> @param n5 size of fifth dimension
!> @param n6 size of sixth dimension
!>
!> Routine for allocating a rank 6 allocatable array of type LOGICAL
!> from 1 to \e n1, 1 to \e n2, 1 to \e n3, 1 to \e n3, 1 to \e n4,
!> 1 to \e n5, and 1 to \e n6.
    SUBROUTINE malloc_ab6(a,n1,n2,n3,n4,n5,n6)
      INTEGER(SIK),INTENT(IN) :: n1,n2,n3,n4,n5,n6
      LOGICAL(SBK),ALLOCATABLE,INTENT(INOUT) :: a(:,:,:,:,:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ALLOCATED(a)) THEN
        IF(n1 >= 1_SIK .AND. n2 >= 1_SIK &
                       .AND. n3 >= 1_SIK .AND. n4 >= 1_SIK &
                       .AND. n5 >= 1_SIK .AND. n6 >= 1_SIK) THEN
          ALLOCATE(a(n1,n2,n3,n4,n5,n6),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=.FALSE.
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SBK,SRK)
          ELSE
            bsize=REAL(SBK,SRK)
            mysize=REAL(REAL(n1,SRK)*REAL(n2,SRK)*REAL(n3,SRK)* &
                        REAL(n4,SRK)*REAL(n5,SRK)*REAL(n6,SRK)*bsize,SRK)
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_ab6
!
!-------------------------------------------------------------------------------
! Allocate seven dimensional pointers
!
!> @brief Allocates rank 7 SINGLE precision allocatable array.
!> @param a dummy argument for allocatable array to be allocated
!> @param n1 size of first dimension
!> @param n2 size of second dimension
!> @param n3 size of third dimension
!> @param n4 size of fourth dimension
!> @param n5 size of fifth dimension
!> @param n6 size of sixth dimension
!> @param n7 size of seventh dimension
!>
!> Routine for allocating a rank 7 allocatable array of type SINGLE
!> precision from 1 to \e n1, 1 to \e n2, 1 to \e n3, 1 to \e n3,
!> 1 to \e n4, 1 to \e n5,  1 to \e n6, and 1 to \e n7.
    SUBROUTINE malloc_as7(a,n1,n2,n3,n4,n5,n6,n7)
      INTEGER(SIK),INTENT(IN) :: n1,n2,n3,n4,n5,n6,n7
      REAL(SSK),ALLOCATABLE,INTENT(INOUT) :: a(:,:,:,:,:,:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ALLOCATED(a)) THEN
        IF(n1 >= 1_SIK .AND. n2 >= 1_SIK &
                       .AND. n3 >= 1_SIK .AND. n4 >= 1_SIK &
                       .AND. n5 >= 1_SIK .AND. n6 >= 1_SIK &
                                         .AND. n7 >= 1_SIK) THEN
          ALLOCATE(a(n1,n2,n3,n4,n5,n6,n7),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=0.0_SSK
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SSK,SRK)
          ELSE
            bsize=REAL(SSK,SRK)
            mysize=REAL(REAL(n1,SRK)*REAL(n2,SRK)*REAL(n3,SRK)* &
                        REAL(n4,SRK)*REAL(n5,SRK)*REAL(n6,SRK)* &
                        REAL(n7,SRK)*bsize,SRK)
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_as7
!
!----------------------------------------
!> @brief Allocates rank 7 DOUBLE precision allocatable array.
!> @param a dummy argument for allocatable array to be allocated
!> @param n1 size of first dimension
!> @param n2 size of second dimension
!> @param n3 size of third dimension
!> @param n4 size of fourth dimension
!> @param n5 size of fifth dimension
!> @param n6 size of sixth dimension
!> @param n7 size of seventh dimension
!>
!> Routine for allocating a rank 7 allocatable array of type DOUBLE
!> precision from 1 to \e n1, 1 to \e n2, 1 to \e n3, 1 to \e n3,
!> 1 to \e n4, 1 to \e n5,  1 to \e n6, and 1 to \e n7.
    SUBROUTINE malloc_ad7(a,n1,n2,n3,n4,n5,n6,n7)
      INTEGER(SIK),INTENT(IN) :: n1,n2,n3,n4,n5,n6,n7
      REAL(SDK),ALLOCATABLE,INTENT(INOUT) :: a(:,:,:,:,:,:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ALLOCATED(a)) THEN
        IF(n1 >= 1_SIK .AND. n2 >= 1_SIK &
                       .AND. n3 >= 1_SIK .AND. n4 >= 1_SIK &
                       .AND. n5 >= 1_SIK .AND. n6 >= 1_SIK &
                                         .AND. n7 >= 1_SIK) THEN
          ALLOCATE(a(n1,n2,n3,n4,n5,n6,n7),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=0.0_SDK
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SDK,SRK)
          ELSE
            bsize=REAL(SDK,SRK)
            mysize=REAL(REAL(n1,SRK)*REAL(n2,SRK)*REAL(n3,SRK)* &
                        REAL(n4,SRK)*REAL(n5,SRK)*REAL(n6,SRK)* &
                        REAL(n7,SRK)*bsize,SRK)
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_ad7
!
!----------------------------------------
!> @brief Allocates rank 7 INTEGER allocatable array.
!> @param a dummy argument for allocatable array to be allocated
!> @param n1 size of first dimension
!> @param n2 size of second dimension
!> @param n3 size of third dimension
!> @param n4 size of fourth dimension
!> @param n5 size of fifth dimension
!> @param n6 size of sixth dimension
!> @param n7 size of seventh dimension
!>
!> Routine for allocating a rank 7 allocatable array of type INTEGER
!> from 1 to \e n1, 1 to \e n2, 1 to \e n3, 1 to \e n3, 1 to \e n4,
!> 1 to \e n5, 1 to \e n6, and 1 to \e n7.
    SUBROUTINE malloc_ai7(a,n1,n2,n3,n4,n5,n6,n7)
      INTEGER(SIK),INTENT(IN) :: n1,n2,n3,n4,n5,n6,n7
      INTEGER(SNK),ALLOCATABLE,INTENT(INOUT) :: a(:,:,:,:,:,:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ALLOCATED(a)) THEN
        IF(n1 >= 1_SIK .AND. n2 >= 1_SIK &
                       .AND. n3 >= 1_SIK .AND. n4 >= 1_SIK &
                       .AND. n5 >= 1_SIK .AND. n6 >= 1_SIK &
                                         .AND. n7 >= 1_SIK) THEN
          ALLOCATE(a(n1,n2,n3,n4,n5,n6,n7),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=0_SNK
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SNK,SRK)
          ELSE
            bsize=REAL(SNK,SRK)
            mysize=REAL(REAL(n1,SRK)*REAL(n2,SRK)*REAL(n3,SRK)* &
                        REAL(n4,SRK)*REAL(n5,SRK)*REAL(n6,SRK)* &
                        REAL(n7,SRK)*bsize,SRK)
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_ai7
!
!----------------------------------------
!> @brief Allocates rank 7 LONG integer allocatable array.
!> @param a dummy argument for allocatable array to be allocated
!> @param n1 size of first dimension
!> @param n2 size of second dimension
!> @param n3 size of third dimension
!> @param n4 size of fourth dimension
!> @param n5 size of fifth dimension
!> @param n6 size of sixth dimension
!> @param n7 size of seventh dimension
!>
!> Routine for allocating a rank 7 allocatable array of type LONG integer
!> from 1 to \e n1, 1 to \e n2, 1 to \e n3, 1 to \e n3, 1 to \e n4,
!> 1 to \e n5, 1 to \e n6, and 1 to \e n7.
    SUBROUTINE malloc_al7(a,n1,n2,n3,n4,n5,n6,n7)
      INTEGER(SIK),INTENT(IN) :: n1,n2,n3,n4,n5,n6,n7
      INTEGER(SLK),ALLOCATABLE,INTENT(INOUT) :: a(:,:,:,:,:,:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ALLOCATED(a)) THEN
        IF(n1 >= 1_SIK .AND. n2 >= 1_SIK &
                       .AND. n3 >= 1_SIK .AND. n4 >= 1_SIK &
                       .AND. n5 >= 1_SIK .AND. n6 >= 1_SIK &
                                         .AND. n7 >= 1_SIK) THEN
          ALLOCATE(a(n1,n2,n3,n4,n5,n6,n7),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=0_SLK
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SLK,SRK)
          ELSE
            bsize=REAL(SLK,SRK)
            mysize=REAL(REAL(n1,SRK)*REAL(n2,SRK)*REAL(n3,SRK)* &
                        REAL(n4,SRK)*REAL(n5,SRK)*REAL(n6,SRK)* &
                        REAL(n7,SRK)*bsize,SRK)
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_al7
!
!----------------------------------------
!> @brief Allocates rank 7 LOGICAL allocatable array.
!> @param a dummy argument for allocatable array to be allocated
!> @param n1 size of first dimension
!> @param n2 size of second dimension
!> @param n3 size of third dimension
!> @param n4 size of fourth dimension
!> @param n5 size of fifth dimension
!> @param n6 size of sixth dimension
!> @param n7 size of seventh dimension
!>
!> Routine for allocating a rank 7 allocatable array of type LOGICAL from 1
!> to \e n1, 1 to \e n2, 1 to \e n3, 1 to \e n3, 1 to \e n4, 1 to \e n5, 
!> 1 to \e n6, and 1 to \e n7.
    SUBROUTINE malloc_ab7(a,n1,n2,n3,n4,n5,n6,n7)
      INTEGER(SIK),INTENT(IN) :: n1,n2,n3,n4,n5,n6,n7
      LOGICAL(SBK),ALLOCATABLE,INTENT(INOUT) :: a(:,:,:,:,:,:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ALLOCATED(a)) THEN
        IF(n1 >= 1_SIK .AND. n2 >= 1_SIK &
                       .AND. n3 >= 1_SIK .AND. n4 >= 1_SIK &
                       .AND. n5 >= 1_SIK .AND. n6 >= 1_SIK &
                                         .AND. n7 >= 1_SIK) THEN
          ALLOCATE(a(n1,n2,n3,n4,n5,n6,n7),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=.FALSE.
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SBK,SRK)
          ELSE
            bsize=REAL(SBK,SRK)
            mysize=REAL(REAL(n1,SRK)*REAL(n2,SRK)*REAL(n3,SRK)* &
                        REAL(n4,SRK)*REAL(n5,SRK)*REAL(n6,SRK)* &
                        REAL(n7,SRK)*bsize,SRK)
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_ab7
!
!===============================================================================
! The following section contains all procedures for allocating pointers from nb
! to ne.
!
!> @brief Allocates rank 1 SINGLE precision pointer array.
!> @param a dummy argument for pointer array to be allocated
!> @param nb1 beginning index of first dimension
!> @param ne1 ending index of first dimension
!>
!> Routine for allocating a rank 1 pointer array of type SINGLE precision from
!> \e nb1 to \e ne1.
    SUBROUTINE malloc_ps01(a,nb1,ne1)
      INTEGER(SIK),INTENT(IN) :: nb1,ne1
      REAL(SSK),POINTER :: a(:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ASSOCIATED(a)) THEN
        NULLIFY(a)
        IF(nb1 <= ne1) THEN
          ALLOCATE(a(nb1:ne1),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=0.0_SSK
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SSK,SRK)
          ELSE
            bsize=REAL(SSK,SRK)
            mysize=(REAL(ne1,SRK)-REAL(nb1,SRK)+1_SRK)*bsize
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_ps01
!
!----------------------------------------
!> @brief Allocates rank 1 DOUBLE precision pointer array.
!> @param a dummy argument for pointer array to be allocated
!> @param nb1 beginning index of first dimension
!> @param ne1 ending index of first dimension
!>
!> Routine for allocating a rank 1 pointer array of type DOUBLE precision from
!> \e nb1 to \e ne1.
    SUBROUTINE malloc_pd01(a,nb1,ne1)
      INTEGER(SIK),INTENT(IN) :: nb1,ne1
      REAL(SDK),POINTER :: a(:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ASSOCIATED(a)) THEN
        NULLIFY(a)
        IF(nb1 <= ne1) THEN
          ALLOCATE(a(nb1:ne1),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=0.0_SDK
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SDK,SRK)
          ELSE
            bsize=REAL(SDK,SRK)
            mysize=(REAL(ne1,SRK)-REAL(nb1,SRK)+1_SRK)*bsize
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_pd01
!
!----------------------------------------
!> @brief Allocates rank 1 INTEGER pointer array.
!> @param a dummy argument for pointer array to be allocated
!> @param nb1 beginning index of first dimension
!> @param ne1 ending index of first dimension
!>
!> Routine for allocating a rank 1 pointer array of type INTEGER from
!> \e nb1 to \e ne1.
    SUBROUTINE malloc_pi01(a,nb1,ne1)
      INTEGER(SIK),INTENT(IN) :: nb1,ne1
      INTEGER(SNK),POINTER :: a(:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ASSOCIATED(a)) THEN
        NULLIFY(a)
        IF(nb1 <= ne1) THEN
          ALLOCATE(a(nb1:ne1),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=0_SNK
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SNK,SRK)
          ELSE
            bsize=REAL(SNK,SRK)
            mysize=(REAL(ne1,SRK)-REAL(nb1,SRK)+1_SRK)*bsize
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_pi01
!
!----------------------------------------
!> @brief Allocates rank 1 LONG integer pointer array.
!> @param a dummy argument for pointer array to be allocated
!> @param nb1 beginning index of first dimension
!> @param ne1 ending index of first dimension
!>
!> Routine for allocating a rank 1 pointer array of type LONG integer from
!> \e nb1 to \e ne1.
    SUBROUTINE malloc_pl01(a,nb1,ne1)
      INTEGER(SIK),INTENT(IN) :: nb1,ne1
      INTEGER(SLK),POINTER :: a(:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ASSOCIATED(a)) THEN
        NULLIFY(a)
        IF(nb1 <= ne1) THEN
          ALLOCATE(a(nb1:ne1),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=0_SLK
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SLK,SRK)
          ELSE
            bsize=REAL(SLK,SRK)
            mysize=(REAL(ne1,SRK)-REAL(nb1,SRK)+1_SRK)*bsize
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_pl01
!
!----------------------------------------
!> @brief Allocates rank 1 LOGICAL pointer array.
!> @param a dummy argument for pointer array to be allocated
!> @param nb1 beginning index of first dimension
!> @param ne1 ending index of first dimension
!>
!> Routine for allocating a rank 1 pointer array of type LOGICAL from
!> \e nb1 to \e ne1.
    SUBROUTINE malloc_pb01(a,nb1,ne1)
      INTEGER(SIK),INTENT(IN) :: nb1,ne1
      LOGICAL(SBK),POINTER :: a(:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ASSOCIATED(a)) THEN
        NULLIFY(a)
        IF(nb1 <= ne1) THEN
          ALLOCATE(a(nb1:ne1),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=.FALSE.
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SBK,SRK)
          ELSE
            bsize=REAL(SBK,SRK)
            mysize=(REAL(ne1,SRK)-REAL(nb1,SRK)+1_SRK)*bsize
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_pb01
!
!-------------------------------------------------------------------------------
! Allocate two dimensional pointers
!
!> @brief Allocates rank 2 SINGLE precision pointer array.
!> @param a dummy argument for pointer array to be allocated
!> @param nb1 beginning index of first dimension
!> @param ne1 ending index of first dimension
!> @param nb2 beginning index of second dimension
!> @param ne2 ending index of second dimension
!>
!> Routine for allocating a rank 2 pointer array of type SINGLE precision from
!> \e nb1 to \e ne1 and \e nb2 to \e ne2.
    SUBROUTINE malloc_ps02(a,nb1,ne1,nb2,ne2)
      INTEGER(SIK),INTENT(IN) :: nb1,ne1,nb2,ne2
      REAL(SSK),POINTER :: a(:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ASSOCIATED(a)) THEN
        NULLIFY(a)
        IF(nb1 <= ne1 .AND. nb2 <= ne2) THEN
          ALLOCATE(a(nb1:ne1,nb2:ne2),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=0.0_SSK
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SSK,SRK)
          ELSE
            bsize=REAL(SSK,SRK)
            mysize=(REAL(ne1,SRK)-REAL(nb1,SRK)+1_SRK)+ &
                   (REAL(ne2,SRK)-REAL(nb2,SRK)+1_SRK)*bsize
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_ps02
!
!----------------------------------------
!> @brief Allocates rank 2 DOUBLE precision pointer array.
!> @param a dummy argument for pointer array to be allocated
!> @param nb1 beginning index of first dimension
!> @param ne1 ending index of first dimension
!> @param nb2 beginning index of second dimension
!> @param ne2 ending index of second dimension
!>
!> Routine for allocating a rank 2 pointer array of type DOUBLE precision from
!> \e nb1 to \e ne1 and \e nb2 to \e ne2.
    SUBROUTINE malloc_pd02(a,nb1,ne1,nb2,ne2)
      INTEGER(SIK),INTENT(IN) :: nb1,ne1,nb2,ne2
      REAL(SDK),POINTER :: a(:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ASSOCIATED(a)) THEN
        NULLIFY(a)
        IF(nb1 <= ne1 .AND. nb2 <= ne2) THEN
          ALLOCATE(a(nb1:ne1,nb2:ne2),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=0.0_SDK
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SDK,SRK)
          ELSE
            bsize=REAL(SDK,SRK)
            mysize=(REAL(ne1,SRK)-REAL(nb1,SRK)+1_SRK)+ &
                   (REAL(ne2,SRK)-REAL(nb2,SRK)+1_SRK)*bsize
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_pd02
!
!----------------------------------------
!> @brief Allocates rank 2 INTEGER pointer array.
!> @param a dummy argument for pointer array to be allocated
!> @param nb1 beginning index of first dimension
!> @param ne1 ending index of first dimension
!> @param nb2 beginning index of second dimension
!> @param ne2 ending index of second dimension
!>
!> Routine for allocating a rank 2 pointer array of type INTEGER from
!> \e nb1 to \e ne1 and \e nb2 to \e ne2.
    SUBROUTINE malloc_pi02(a,nb1,ne1,nb2,ne2)
      INTEGER(SIK),INTENT(IN) :: nb1,ne1,nb2,ne2
      INTEGER(SNK),POINTER :: a(:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ASSOCIATED(a)) THEN
        NULLIFY(a)
        IF(nb1 <= ne1 .AND. nb2 <= ne2) THEN
          ALLOCATE(a(nb1:ne1,nb2:ne2),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=0_SNK
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SNK,SRK)
          ELSE
            bsize=REAL(SNK,SRK)
            mysize=(REAL(ne1,SRK)-REAL(nb1,SRK)+1_SRK)+ &
                   (REAL(ne2,SRK)-REAL(nb2,SRK)+1_SRK)*bsize
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_pi02
!
!----------------------------------------
!> @brief Allocates rank 2 LONG integer pointer array.
!> @param a dummy argument for pointer array to be allocated
!> @param nb1 beginning index of first dimension
!> @param ne1 ending index of first dimension
!> @param nb2 beginning index of second dimension
!> @param ne2 ending index of second dimension
!>
!> Routine for allocating a rank 2 pointer array of type LONG integer from
!> \e nb1 to \e ne1 and \e nb2 to \e ne2.
    SUBROUTINE malloc_pl02(a,nb1,ne1,nb2,ne2)
      INTEGER(SIK),INTENT(IN) :: nb1,ne1,nb2,ne2
      INTEGER(SLK),POINTER :: a(:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ASSOCIATED(a)) THEN
        NULLIFY(a)
        IF(nb1 <= ne1 .AND. nb2 <= ne2) THEN
          ALLOCATE(a(nb1:ne1,nb2:ne2),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=0_SLK
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SLK,SRK)
          ELSE
            bsize=REAL(SLK,SRK)
            mysize=(REAL(ne1,SRK)-REAL(nb1,SRK)+1_SRK)+ &
                   (REAL(ne2,SRK)-REAL(nb2,SRK)+1_SRK)*bsize
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_pl02
!
!----------------------------------------
!> @brief Allocates rank 2 LOGICAL pointer array.
!> @param a dummy argument for pointer array to be allocated
!> @param nb1 beginning index of first dimension
!> @param ne1 ending index of first dimension
!> @param nb2 beginning index of second dimension
!> @param ne2 ending index of second dimension
!>
!> Routine for allocating a rank 2 pointer array of type LOGICAL from
!> \e nb1 to \e ne1 and \e nb2 to \e ne2.
    SUBROUTINE malloc_pb02(a,nb1,ne1,nb2,ne2)
      INTEGER(SIK),INTENT(IN) :: nb1,ne1,nb2,ne2
      LOGICAL(SBK),POINTER :: a(:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ASSOCIATED(a)) THEN
        NULLIFY(a)
        IF(nb1 <= ne1 .AND. nb2 <= ne2) THEN
          ALLOCATE(a(nb1:ne1,nb2:ne2),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=.FALSE.
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SBK,SRK)
          ELSE
            bsize=REAL(SBK,SRK)
            mysize=(REAL(ne1,SRK)-REAL(nb1,SRK)+1_SRK)+ &
                   (REAL(ne2,SRK)-REAL(nb2,SRK)+1_SRK)*bsize
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_pb02
!
!-------------------------------------------------------------------------------
! Allocate three dimensional pointers
!
!> @brief Allocates rank 3 SINGLE precision pointer array.
!> @param a dummy argument for pointer array to be allocated
!> @param nb1 beginning index of first dimension
!> @param ne1 ending index of first dimension
!> @param nb2 beginning index of second dimension
!> @param ne2 ending index of second dimension
!> @param nb3 beginning index of third dimension
!> @param ne3 ending index of third dimension
!>
!> Routine for allocating a rank 3 pointer array of type SINGLE precision from
!> \e nb1 to \e ne1, \e nb2 to \e ne2, and \e nb3 to \e ne3.
    SUBROUTINE malloc_ps03(a,nb1,ne1,nb2,ne2,nb3,ne3)
      INTEGER(SIK),INTENT(IN) :: nb1,ne1,nb2,ne2,nb3,ne3
      REAL(SSK),POINTER :: a(:,:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ASSOCIATED(a)) THEN
        NULLIFY(a)
        IF(nb1 <= ne1 .AND. nb2 <= ne2 .AND. nb3 <= ne3) THEN
          ALLOCATE(a(nb1:ne1,nb2:ne2,nb3:ne3),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=0.0_SSK
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SSK,SRK)
          ELSE
            bsize=REAL(SSK,SRK)
            mysize=(REAL(ne1,SRK)-REAL(nb1,SRK)+1_SRK)+ &
                   (REAL(ne2,SRK)-REAL(nb2,SRK)+1_SRK)+ &
                   (REAL(ne3,SRK)-REAL(nb3,SRK)+1_SRK)*bsize
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_ps03
!
!----------------------------------------
!> @brief Allocates rank 3 DOUBLE precision pointer array.
!> @param a dummy argument for pointer array to be allocated
!> @param nb1 beginning index of first dimension
!> @param ne1 ending index of first dimension
!> @param nb2 beginning index of second dimension
!> @param ne2 ending index of second dimension
!> @param nb3 beginning index of third dimension
!> @param ne3 ending index of third dimension
!>
!> Routine for allocating a rank 3 pointer array of type DOUBLE precision from
!> \e nb1 to \e ne1, \e nb2 to \e ne2, and \e nb3 to \e ne3.
    SUBROUTINE malloc_pd03(a,nb1,ne1,nb2,ne2,nb3,ne3)
      INTEGER(SIK),INTENT(IN) :: nb1,ne1,nb2,ne2,nb3,ne3
      REAL(SDK),POINTER :: a(:,:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ASSOCIATED(a)) THEN
        NULLIFY(a)
        IF(nb1 <= ne1 .AND. nb2 <= ne2 .AND. nb3 <= ne3) THEN
          ALLOCATE(a(nb1:ne1,nb2:ne2,nb3:ne3),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=0.0_SDK
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SDK,SRK)
          ELSE
            bsize=REAL(SDK,SRK)
            mysize=(REAL(ne1,SRK)-REAL(nb1,SRK)+1_SRK)+ &
                   (REAL(ne2,SRK)-REAL(nb2,SRK)+1_SRK)+ &
                   (REAL(ne3,SRK)-REAL(nb3,SRK)+1_SRK)*bsize
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_pd03
!
!----------------------------------------
!> @brief Allocates rank 3 INTEGER pointer array.
!> @param a dummy argument for pointer array to be allocated
!> @param nb1 beginning index of first dimension
!> @param ne1 ending index of first dimension
!> @param nb2 beginning index of second dimension
!> @param ne2 ending index of second dimension
!> @param nb3 beginning index of third dimension
!> @param ne3 ending index of third dimension
!>
!> Routine for allocating a rank 3 pointer array of type INTEGER from
!> \e nb1 to \e ne1, \e nb2 to \e ne2, and \e nb3 to \e ne3.
    SUBROUTINE malloc_pi03(a,nb1,ne1,nb2,ne2,nb3,ne3)
      INTEGER(SIK),INTENT(IN) :: nb1,ne1,nb2,ne2,nb3,ne3
      INTEGER(SNK),POINTER :: a(:,:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ASSOCIATED(a)) THEN
        NULLIFY(a)
        IF(nb1 <= ne1 .AND. nb2 <= ne2 .AND. nb3 <= ne3) THEN
          ALLOCATE(a(nb1:ne1,nb2:ne2,nb3:ne3),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=0_SNK
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SNK,SRK)
          ELSE
            bsize=REAL(SNK,SRK)
            mysize=(REAL(ne1,SRK)-REAL(nb1,SRK)+1_SRK)+ &
                   (REAL(ne2,SRK)-REAL(nb2,SRK)+1_SRK)+ &
                   (REAL(ne3,SRK)-REAL(nb3,SRK)+1_SRK)*bsize
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_pi03
!
!----------------------------------------
!> @brief Allocates rank 3 LONG integer pointer array.
!> @param a dummy argument for pointer array to be allocated
!> @param nb1 beginning index of first dimension
!> @param ne1 ending index of first dimension
!> @param nb2 beginning index of second dimension
!> @param ne2 ending index of second dimension
!> @param nb3 beginning index of third dimension
!> @param ne3 ending index of third dimension
!>
!> Routine for allocating a rank 3 pointer array of type LONG integer from
!> \e nb1 to \e ne1, \e nb2 to \e ne2, and \e nb3 to \e ne3.
    SUBROUTINE malloc_pl03(a,nb1,ne1,nb2,ne2,nb3,ne3)
      INTEGER(SIK),INTENT(IN) :: nb1,ne1,nb2,ne2,nb3,ne3
      INTEGER(SLK),POINTER :: a(:,:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ASSOCIATED(a)) THEN
        NULLIFY(a)
        IF(nb1 <= ne1 .AND. nb2 <= ne2 .AND. nb3 <= ne3) THEN
          ALLOCATE(a(nb1:ne1,nb2:ne2,nb3:ne3),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=0_SLK
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SLK,SRK)
          ELSE
            bsize=REAL(SLK,SRK)
            mysize=(REAL(ne1,SRK)-REAL(nb1,SRK)+1_SRK)+ &
                   (REAL(ne2,SRK)-REAL(nb2,SRK)+1_SRK)+ &
                   (REAL(ne3,SRK)-REAL(nb3,SRK)+1_SRK)*bsize
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_pl03
!
!----------------------------------------
!> @brief Allocates rank 3 LOGICAL pointer array.
!> @param a dummy argument for pointer array to be allocated
!> @param nb1 beginning index of first dimension
!> @param ne1 ending index of first dimension
!> @param nb2 beginning index of second dimension
!> @param ne2 ending index of second dimension
!> @param nb3 beginning index of third dimension
!> @param ne3 ending index of third dimension
!>
!> Routine for allocating a rank 3 pointer array of type LOGICAL from
!> \e nb1 to \e ne1, \e nb2 to \e ne2, and \e nb3 to \e ne3.
    SUBROUTINE malloc_pb03(a,nb1,ne1,nb2,ne2,nb3,ne3)
      INTEGER(SIK),INTENT(IN) :: nb1,ne1,nb2,ne2,nb3,ne3
      LOGICAL(SBK),POINTER :: a(:,:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ASSOCIATED(a)) THEN
        NULLIFY(a)
        IF(nb1 <= ne1 .AND. nb2 <= ne2 .AND. nb3 <= ne3) THEN
          ALLOCATE(a(nb1:ne1,nb2:ne2,nb3:ne3),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=.FALSE.
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SBK,SRK)
          ELSE
            bsize=REAL(SBK,SRK)
            mysize=(REAL(ne1,SRK)-REAL(nb1,SRK)+1_SRK)+ &
                   (REAL(ne2,SRK)-REAL(nb2,SRK)+1_SRK)+ &
                   (REAL(ne3,SRK)-REAL(nb3,SRK)+1_SRK)*bsize
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_pb03
!
!-------------------------------------------------------------------------------
! Allocate four dimensional pointers
!
!> @brief Allocates rank 4 SINGLE precision pointer array.
!> @param a dummy argument for pointer array to be allocated
!> @param nb1 beginning index of first dimension
!> @param ne1 ending index of first dimension
!> @param nb2 beginning index of second dimension
!> @param ne2 ending index of second dimension
!> @param nb3 beginning index of third dimension
!> @param ne3 ending index of third dimension
!> @param nb4 beginning index of fourth dimension
!> @param ne4 ending index of fourth dimension
!>
!> Routine for allocating a rank 4 pointer array of type SINGLE precision from
!> \e nb1 to \e ne1, \e nb2 to \e ne2, \e nb3 to \e ne3, and \e nb4 to \e ne4.
    SUBROUTINE malloc_ps04(a,nb1,ne1,nb2,ne2,nb3,ne3,nb4,ne4)
      INTEGER(SIK),INTENT(IN) :: nb1,ne1,nb2,ne2,nb3,ne3,nb4,ne4
      REAL(SSK),POINTER :: a(:,:,:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ASSOCIATED(a)) THEN
        NULLIFY(a)
        IF(nb1 <= ne1 .AND. nb2 <= ne2 .AND. nb3 <= ne3 .AND. nb4 <= ne4) THEN
          ALLOCATE(a(nb1:ne1,nb2:ne2,nb3:ne3,nb4:ne4),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=0.0_SSK
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SSK,SRK)
          ELSE
            bsize=REAL(SSK,SRK)
            mysize=(REAL(ne1,SRK)-REAL(nb1,SRK)+1_SRK)+ &
                   (REAL(ne2,SRK)-REAL(nb2,SRK)+1_SRK)+ &
                   (REAL(ne3,SRK)-REAL(nb3,SRK)+1_SRK)+ &
                   (REAL(ne4,SRK)-REAL(nb4,SRK)+1_SRK)*bsize
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_ps04
!
!----------------------------------------
!> @brief Allocates rank 4 DOUBLE precision pointer array.
!> @param a dummy argument for pointer array to be allocated
!> @param nb1 beginning index of first dimension
!> @param ne1 ending index of first dimension
!> @param nb2 beginning index of second dimension
!> @param ne2 ending index of second dimension
!> @param nb3 beginning index of third dimension
!> @param ne3 ending index of third dimension
!> @param nb4 beginning index of fourth dimension
!> @param ne4 ending index of fourth dimension
!>
!> Routine for allocating a rank 4 pointer array of type DOUBLE precision from
!> \e nb1 to \e ne1, \e nb2 to \e ne2, \e nb3 to \e ne3, and \e nb4 to \e ne4.
    SUBROUTINE malloc_pd04(a,nb1,ne1,nb2,ne2,nb3,ne3,nb4,ne4)
      INTEGER(SIK),INTENT(IN) :: nb1,ne1,nb2,ne2,nb3,ne3,nb4,ne4
      REAL(SDK),POINTER :: a(:,:,:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ASSOCIATED(a)) THEN
        NULLIFY(a)
        IF(nb1 <= ne1 .AND. nb2 <= ne2 .AND. nb3 <= ne3 .AND. nb4 <= ne4) THEN
          ALLOCATE(a(nb1:ne1,nb2:ne2,nb3:ne3,nb4:ne4),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=0.0_SDK
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SDK,SRK)
          ELSE
            bsize=REAL(SDK,SRK)
            mysize=(REAL(ne1,SRK)-REAL(nb1,SRK)+1_SRK)+ &
                   (REAL(ne2,SRK)-REAL(nb2,SRK)+1_SRK)+ &
                   (REAL(ne3,SRK)-REAL(nb3,SRK)+1_SRK)+ &
                   (REAL(ne4,SRK)-REAL(nb4,SRK)+1_SRK)*bsize
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_pd04
!
!----------------------------------------
!> @brief Allocates rank 4 INTEGER pointer array.
!> @param a dummy argument for pointer array to be allocated
!> @param nb1 beginning index of first dimension
!> @param ne1 ending index of first dimension
!> @param nb2 beginning index of second dimension
!> @param ne2 ending index of second dimension
!> @param nb3 beginning index of third dimension
!> @param ne3 ending index of third dimension
!> @param nb4 beginning index of fourth dimension
!> @param ne4 ending index of fourth dimension
!>
!> Routine for allocating a rank 4 pointer array of type INTEGER from
!> \e nb1 to \e ne1, \e nb2 to \e ne2, \e nb3 to \e ne3, and \e nb4 to \e ne4.
    SUBROUTINE malloc_pi04(a,nb1,ne1,nb2,ne2,nb3,ne3,nb4,ne4)
      INTEGER(SIK),INTENT(IN) :: nb1,ne1,nb2,ne2,nb3,ne3,nb4,ne4
      INTEGER(SNK),POINTER :: a(:,:,:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ASSOCIATED(a)) THEN
        NULLIFY(a)
        IF(nb1 <= ne1 .AND. nb2 <= ne2 .AND. nb3 <= ne3 .AND. nb4 <= ne4) THEN
          ALLOCATE(a(nb1:ne1,nb2:ne2,nb3:ne3,nb4:ne4),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=0_SNK
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SNK,SRK)
          ELSE
            bsize=REAL(SNK,SRK)
            mysize=(REAL(ne1,SRK)-REAL(nb1,SRK)+1_SRK)+ &
                   (REAL(ne2,SRK)-REAL(nb2,SRK)+1_SRK)+ &
                   (REAL(ne3,SRK)-REAL(nb3,SRK)+1_SRK)+ &
                   (REAL(ne4,SRK)-REAL(nb4,SRK)+1_SRK)*bsize
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_pi04
!
!----------------------------------------
!> @brief Allocates rank 4 LONG integer pointer array.
!> @param a dummy argument for pointer array to be allocated
!> @param nb1 beginning index of first dimension
!> @param ne1 ending index of first dimension
!> @param nb2 beginning index of second dimension
!> @param ne2 ending index of second dimension
!> @param nb3 beginning index of third dimension
!> @param ne3 ending index of third dimension
!> @param nb4 beginning index of fourth dimension
!> @param ne4 ending index of fourth dimension
!>
!> Routine for allocating a rank 4 pointer array of type LONG integer from
!> \e nb1 to \e ne1, \e nb2 to \e ne2, \e nb3 to \e ne3, and \e nb4 to \e ne4.
    SUBROUTINE malloc_pl04(a,nb1,ne1,nb2,ne2,nb3,ne3,nb4,ne4)
      INTEGER(SIK),INTENT(IN) :: nb1,ne1,nb2,ne2,nb3,ne3,nb4,ne4
      INTEGER(SLK),POINTER :: a(:,:,:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ASSOCIATED(a)) THEN
        NULLIFY(a)
        IF(nb1 <= ne1 .AND. nb2 <= ne2 .AND. nb3 <= ne3 .AND. nb4 <= ne4) THEN
          ALLOCATE(a(nb1:ne1,nb2:ne2,nb3:ne3,nb4:ne4),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=0_SLK
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SLK,SRK)
          ELSE
            bsize=REAL(SLK,SRK)
            mysize=(REAL(ne1,SRK)-REAL(nb1,SRK)+1_SRK)+ &
                   (REAL(ne2,SRK)-REAL(nb2,SRK)+1_SRK)+ &
                   (REAL(ne3,SRK)-REAL(nb3,SRK)+1_SRK)+ &
                   (REAL(ne4,SRK)-REAL(nb4,SRK)+1_SRK)*bsize
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_pl04
!
!----------------------------------------
!> @brief Allocates rank 4 INTEGER pointer array.
!> @param a dummy argument for pointer array to be allocated
!> @param nb1 beginning index of first dimension
!> @param ne1 ending index of first dimension
!> @param nb2 beginning index of second dimension
!> @param ne2 ending index of second dimension
!> @param nb3 beginning index of third dimension
!> @param ne3 ending index of third dimension
!> @param nb4 beginning index of fourth dimension
!> @param ne4 ending index of fourth dimension
!>
!> Routine for allocating a rank 4 pointer array of type INTEGER from
!> \e nb1 to \e ne1, \e nb2 to \e ne2, \e nb3 to \e ne3, and \e nb4 to \e ne4.
    SUBROUTINE malloc_pb04(a,nb1,ne1,nb2,ne2,nb3,ne3,nb4,ne4)
      INTEGER(SIK),INTENT(IN) :: nb1,ne1,nb2,ne2,nb3,ne3,nb4,ne4
      LOGICAL(SBK),POINTER :: a(:,:,:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ASSOCIATED(a)) THEN
        NULLIFY(a)
        IF(nb1 <= ne1 .AND. nb2 <= ne2 .AND. nb3 <= ne3 .AND. nb4 <= ne4) THEN
          ALLOCATE(a(nb1:ne1,nb2:ne2,nb3:ne3,nb4:ne4),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=.FALSE.
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SBK,SRK)
          ELSE
            bsize=REAL(SBK,SRK)
            mysize=(REAL(ne1,SRK)-REAL(nb1,SRK)+1_SRK)+ &
                   (REAL(ne2,SRK)-REAL(nb2,SRK)+1_SRK)+ &
                   (REAL(ne3,SRK)-REAL(nb3,SRK)+1_SRK)+ &
                   (REAL(ne4,SRK)-REAL(nb4,SRK)+1_SRK)*bsize
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_pb04
!
!-------------------------------------------------------------------------------
! Allocate five dimensional pointers
!
!> @brief Allocates rank 5 SINGLE precision pointer array.
!> @param a dummy argument for pointer array to be allocated
!> @param nb1 beginning index of first dimension
!> @param ne1 ending index of first dimension
!> @param nb2 beginning index of second dimension
!> @param ne2 ending index of second dimension
!> @param nb3 beginning index of third dimension
!> @param ne3 ending index of third dimension
!> @param nb4 beginning index of fourth dimension
!> @param ne4 ending index of fourth dimension
!> @param nb5 beginning index of fifth dimension
!> @param ne5 ending index of fifth dimension
!>
!> Routine for allocating a rank 5 pointer array of type SINGLE precision from
!> \e nb1 to \e ne1, \e nb2 to \e ne2, \e nb3 to \e ne3, \e nb4 to \e ne4, 
!> and \e nb5 to \e ne5.
    SUBROUTINE malloc_ps05(a,nb1,ne1,nb2,ne2,nb3,ne3,nb4,ne4,nb5,ne5)
      INTEGER(SIK),INTENT(IN) :: nb1,ne1,nb2,ne2,nb3,ne3,nb4,ne4,nb5,ne5
      REAL(SSK),POINTER :: a(:,:,:,:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ASSOCIATED(a)) THEN
        NULLIFY(a)
        IF(nb1 <= ne1 .AND. nb2 <= ne2 .AND. nb3 <= ne3 .AND. nb4 <= ne4 &
                                                       .AND. nb5 <= ne5) THEN
          ALLOCATE(a(nb1:ne1,nb2:ne2,nb3:ne3,nb4:ne4,nb5:ne5),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=0.0_SSK
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SSK,SRK)
          ELSE
            bsize=REAL(SSK,SRK)
            mysize=(REAL(ne1,SRK)-REAL(nb1,SRK)+1_SRK)+ &
                   (REAL(ne2,SRK)-REAL(nb2,SRK)+1_SRK)+ &
                   (REAL(ne3,SRK)-REAL(nb3,SRK)+1_SRK)+ &
                   (REAL(ne4,SRK)-REAL(nb4,SRK)+1_SRK)+ &
                   (REAL(ne5,SRK)-REAL(nb5,SRK)+1_SRK)*bsize
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_ps05
!
!----------------------------------------
!> @brief Allocates rank 5 DOUBLE precision pointer array.
!> @param a dummy argument for pointer array to be allocated
!> @param nb1 beginning index of first dimension
!> @param ne1 ending index of first dimension
!> @param nb2 beginning index of second dimension
!> @param ne2 ending index of second dimension
!> @param nb3 beginning index of third dimension
!> @param ne3 ending index of third dimension
!> @param nb4 beginning index of fourth dimension
!> @param ne4 ending index of fourth dimension
!> @param nb5 beginning index of fifth dimension
!> @param ne5 ending index of fifth dimension
!>
!> Routine for allocating a rank 5 pointer array of type DOUBLE precision from
!> \e nb1 to \e ne1, \e nb2 to \e ne2, \e nb3 to \e ne3, \e nb4 to \e ne4, 
!> and \e nb5 to \e ne5.
    SUBROUTINE malloc_pd05(a,nb1,ne1,nb2,ne2,nb3,ne3,nb4,ne4,nb5,ne5)
      INTEGER(SIK),INTENT(IN) :: nb1,ne1,nb2,ne2,nb3,ne3,nb4,ne4,nb5,ne5
      REAL(SDK),POINTER :: a(:,:,:,:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ASSOCIATED(a)) THEN
        NULLIFY(a)
        IF(nb1 <= ne1 .AND. nb2 <= ne2 .AND. nb3 <= ne3 .AND. nb4 <= ne4 &
                                                       .AND. nb5 <= ne5) THEN
          ALLOCATE(a(nb1:ne1,nb2:ne2,nb3:ne3,nb4:ne4,nb5:ne5),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=0.0_SDK
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SDK,SRK)
          ELSE
            bsize=REAL(SDK,SRK)
            mysize=(REAL(ne1,SRK)-REAL(nb1,SRK)+1_SRK)+ &
                   (REAL(ne2,SRK)-REAL(nb2,SRK)+1_SRK)+ &
                   (REAL(ne3,SRK)-REAL(nb3,SRK)+1_SRK)+ &
                   (REAL(ne4,SRK)-REAL(nb4,SRK)+1_SRK)+ &
                   (REAL(ne5,SRK)-REAL(nb5,SRK)+1_SRK)*bsize
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_pd05
!
!----------------------------------------
!> @brief Allocates rank 5 INTEGER pointer array.
!> @param a dummy argument for pointer array to be allocated
!> @param nb1 beginning index of first dimension
!> @param ne1 ending index of first dimension
!> @param nb2 beginning index of second dimension
!> @param ne2 ending index of second dimension
!> @param nb3 beginning index of third dimension
!> @param ne3 ending index of third dimension
!> @param nb4 beginning index of fourth dimension
!> @param ne4 ending index of fourth dimension
!> @param nb5 beginning index of fifth dimension
!> @param ne5 ending index of fifth dimension
!>
!> Routine for allocating a rank 5 pointer array of type INTEGER from
!> \e nb1 to \e ne1, \e nb2 to \e ne2, \e nb3 to \e ne3, \e nb4 to \e ne4, 
!> and \e nb5 to \e ne5.
    SUBROUTINE malloc_pi05(a,nb1,ne1,nb2,ne2,nb3,ne3,nb4,ne4,nb5,ne5)
      INTEGER(SIK),INTENT(IN) :: nb1,ne1,nb2,ne2,nb3,ne3,nb4,ne4,nb5,ne5
      INTEGER(SNK),POINTER :: a(:,:,:,:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

        IF(.NOT.ASSOCIATED(a)) THEN
          NULLIFY(a)
        IF(nb1 <= ne1 .AND. nb2 <= ne2 .AND. nb3 <= ne3 .AND. nb4 <= ne4 &
                                                       .AND. nb5 <= ne5) THEN
          ALLOCATE(a(nb1:ne1,nb2:ne2,nb3:ne3,nb4:ne4,nb5:ne5),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=0_SNK
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SNK,SRK)
          ELSE
            bsize=REAL(SNK,SRK)
            mysize=(REAL(ne1,SRK)-REAL(nb1,SRK)+1_SRK)+ &
                   (REAL(ne2,SRK)-REAL(nb2,SRK)+1_SRK)+ &
                   (REAL(ne3,SRK)-REAL(nb3,SRK)+1_SRK)+ &
                   (REAL(ne4,SRK)-REAL(nb4,SRK)+1_SRK)+ &
                   (REAL(ne5,SRK)-REAL(nb5,SRK)+1_SRK)*bsize
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_pi05
!
!----------------------------------------
!> @brief Allocates rank 5 LONG integer pointer array.
!> @param a dummy argument for pointer array to be allocated
!> @param nb1 beginning index of first dimension
!> @param ne1 ending index of first dimension
!> @param nb2 beginning index of second dimension
!> @param ne2 ending index of second dimension
!> @param nb3 beginning index of third dimension
!> @param ne3 ending index of third dimension
!> @param nb4 beginning index of fourth dimension
!> @param ne4 ending index of fourth dimension
!> @param nb5 beginning index of fifth dimension
!> @param ne5 ending index of fifth dimension
!>
!> Routine for allocating a rank 5 pointer array of type LONG integer from
!> \e nb1 to \e ne1, \e nb2 to \e ne2, \e nb3 to \e ne3, \e nb4 to \e ne4, 
!> and \e nb5 to \e ne5.
    SUBROUTINE malloc_pl05(a,nb1,ne1,nb2,ne2,nb3,ne3,nb4,ne4,nb5,ne5)
      INTEGER(SIK),INTENT(IN) :: nb1,ne1,nb2,ne2,nb3,ne3,nb4,ne4,nb5,ne5
      INTEGER(SLK),POINTER :: a(:,:,:,:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

        IF(.NOT.ASSOCIATED(a)) THEN
          NULLIFY(a)
          IF(nb1 <= ne1 .AND. nb2 <= ne2 .AND. nb3 <= ne3 .AND. nb4 <= ne4 &
                                                       .AND. nb5 <= ne5) THEN
          ALLOCATE(a(nb1:ne1,nb2:ne2,nb3:ne3,nb4:ne4,nb5:ne5),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=0_SLK
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SLK,SRK)
          ELSE
            bsize=REAL(SLK,SRK)
            mysize=(REAL(ne1,SRK)-REAL(nb1,SRK)+1_SRK)+ &
                   (REAL(ne2,SRK)-REAL(nb2,SRK)+1_SRK)+ &
                   (REAL(ne3,SRK)-REAL(nb3,SRK)+1_SRK)+ &
                   (REAL(ne4,SRK)-REAL(nb4,SRK)+1_SRK)+ &
                   (REAL(ne5,SRK)-REAL(nb5,SRK)+1_SRK)*bsize
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_pl05
!
!----------------------------------------
!> @brief Allocates rank 5 LOGICAL pointer array.
!> @param a dummy argument for pointer array to be allocated
!> @param nb1 beginning index of first dimension
!> @param ne1 ending index of first dimension
!> @param nb2 beginning index of second dimension
!> @param ne2 ending index of second dimension
!> @param nb3 beginning index of third dimension
!> @param ne3 ending index of third dimension
!> @param nb4 beginning index of fourth dimension
!> @param ne4 ending index of fourth dimension
!> @param nb5 beginning index of fifth dimension
!> @param ne5 ending index of fifth dimension
!>
!> Routine for allocating a rank 5 pointer array of type LOGICAL from
!> \e nb1 to \e ne1, \e nb2 to \e ne2, \e nb3 to \e ne3, \e nb4 to \e ne4, 
!> and \e nb5 to \e ne5.
    SUBROUTINE malloc_pb05(a,nb1,ne1,nb2,ne2,nb3,ne3,nb4,ne4,nb5,ne5)
      INTEGER(SIK),INTENT(IN) :: nb1,ne1,nb2,ne2,nb3,ne3,nb4,ne4,nb5,ne5
      LOGICAL(SBK),POINTER :: a(:,:,:,:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ASSOCIATED(a)) THEN
        NULLIFY(a)
        IF(nb1 <= ne1 .AND. nb2 <= ne2 .AND. nb3 <= ne3 .AND. nb4 <= ne4 &
                                                       .AND. nb5 <= ne5) THEN
          ALLOCATE(a(nb1:ne1,nb2:ne2,nb3:ne3,nb4:ne4,nb5:ne5),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=.FALSE.
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SBK,SRK)
          ELSE
            bsize=REAL(SBK,SRK)
            mysize=(REAL(ne1,SRK)-REAL(nb1,SRK)+1_SRK)+ &
                   (REAL(ne2,SRK)-REAL(nb2,SRK)+1_SRK)+ &
                   (REAL(ne3,SRK)-REAL(nb3,SRK)+1_SRK)+ &
                   (REAL(ne4,SRK)-REAL(nb4,SRK)+1_SRK)+ &
                   (REAL(ne5,SRK)-REAL(nb5,SRK)+1_SRK)*bsize
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_pb05
!
!-------------------------------------------------------------------------------
! Allocate six dimensional pointers
!
!> @brief Allocates rank 6 SINGLE precision pointer array.
!> @param a dummy argument for pointer array to be allocated
!> @param nb1 beginning index of first dimension
!> @param ne1 ending index of first dimension
!> @param nb2 beginning index of second dimension
!> @param ne2 ending index of second dimension
!> @param nb3 beginning index of third dimension
!> @param ne3 ending index of third dimension
!> @param nb4 beginning index of fourth dimension
!> @param ne4 ending index of fourth dimension
!> @param nb5 beginning index of fifth dimension
!> @param ne5 ending index of fifth dimension
!> @param nb6 beginning index of sixth dimension
!> @param ne6 ending index of sixth dimension
!>
!> Routine for allocating a rank 6 pointer array of type SINGLE precision from
!> \e nb1 to \e ne1, \e nb2 to \e ne2, \e nb3 to \e ne3, \e nb4 to \e ne4, 
!> \e nb5 to \e ne5, and \e nb6 to \e ne6.
    SUBROUTINE malloc_ps06(a,nb1,ne1,nb2,ne2,nb3,ne3,nb4,ne4,nb5,ne5,nb6,ne6)
      INTEGER(SIK),INTENT(IN) :: nb1,ne1,nb2,ne2,nb3,ne3,nb4,ne4,nb5,ne5
      INTEGER(SIK),INTENT(IN) :: nb6,ne6
      REAL(SSK),POINTER :: a(:,:,:,:,:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ASSOCIATED(a)) THEN
        NULLIFY(a)
        IF(nb1 <= ne1 .AND. nb2 <= ne2 .AND. nb3 <= ne3 .AND. nb4 <= ne4 &
                                       .AND. nb5 <= ne5 .AND. nb6 <= ne6) THEN
          ALLOCATE(a(nb1:ne1,nb2:ne2,nb3:ne3,nb4:ne4,nb5:ne5, &
                     nb6:ne6),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=0.0_SSK
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SSK,SRK)
          ELSE
            bsize=REAL(SSK,SRK)
            mysize=(REAL(ne1,SRK)-REAL(nb1,SRK)+1_SRK)+ &
                   (REAL(ne2,SRK)-REAL(nb2,SRK)+1_SRK)+ &
                   (REAL(ne3,SRK)-REAL(nb3,SRK)+1_SRK)+ &
                   (REAL(ne4,SRK)-REAL(nb4,SRK)+1_SRK)+ &
                   (REAL(ne5,SRK)-REAL(nb5,SRK)+1_SRK)+ &
                   (REAL(ne6,SRK)-REAL(nb6,SRK)+1_SRK)*bsize
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_ps06
!
!----------------------------------------
!> @brief Allocates rank 6 DOUBLE precision pointer array.
!> @param a dummy argument for pointer array to be allocated
!> @param nb1 beginning index of first dimension
!> @param ne1 ending index of first dimension
!> @param nb2 beginning index of second dimension
!> @param ne2 ending index of second dimension
!> @param nb3 beginning index of third dimension
!> @param ne3 ending index of third dimension
!> @param nb4 beginning index of fourth dimension
!> @param ne4 ending index of fourth dimension
!> @param nb5 beginning index of fifth dimension
!> @param ne5 ending index of fifth dimension
!> @param nb6 beginning index of sixth dimension
!> @param ne6 ending index of sixth dimension
!>
!> Routine for allocating a rank 6 pointer array of type DOUBLE precision from
!> \e nb1 to \e ne1, \e nb2 to \e ne2, \e nb3 to \e ne3, \e nb4 to \e ne4, 
!> \e nb5 to \e ne5, and \e nb6 to \e ne6.
    SUBROUTINE malloc_pd06(a,nb1,ne1,nb2,ne2,nb3,ne3,nb4,ne4,nb5,ne5,nb6,ne6)
      INTEGER(SIK),INTENT(IN) :: nb1,ne1,nb2,ne2,nb3,ne3,nb4,ne4,nb5,ne5
      INTEGER(SIK),INTENT(IN) :: nb6,ne6
      REAL(SDK),POINTER :: a(:,:,:,:,:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ASSOCIATED(a)) THEN
        NULLIFY(a)
        IF(nb1 <= ne1 .AND. nb2 <= ne2 .AND. nb3 <= ne3 .AND. nb4 <= ne4 &
                                       .AND. nb5 <= ne5 .AND. nb6 <= ne6) THEN
          ALLOCATE(a(nb1:ne1,nb2:ne2,nb3:ne3,nb4:ne4,nb5:ne5, &
                     nb6:ne6),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=0.0_SDK
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SDK,SRK)
          ELSE
            bsize=REAL(SDK,SRK)
            mysize=(REAL(ne1,SRK)-REAL(nb1,SRK)+1_SRK)+ &
                   (REAL(ne2,SRK)-REAL(nb2,SRK)+1_SRK)+ &
                   (REAL(ne3,SRK)-REAL(nb3,SRK)+1_SRK)+ &
                   (REAL(ne4,SRK)-REAL(nb4,SRK)+1_SRK)+ &
                   (REAL(ne5,SRK)-REAL(nb5,SRK)+1_SRK)+ &
                   (REAL(ne6,SRK)-REAL(nb6,SRK)+1_SRK)*bsize
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_pd06
!
!----------------------------------------
!> @brief Allocates rank 6 INTEGER pointer array.
!> @param a dummy argument for pointer array to be allocated
!> @param nb1 beginning index of first dimension
!> @param ne1 ending index of first dimension
!> @param nb2 beginning index of second dimension
!> @param ne2 ending index of second dimension
!> @param nb3 beginning index of third dimension
!> @param ne3 ending index of third dimension
!> @param nb4 beginning index of fourth dimension
!> @param ne4 ending index of fourth dimension
!> @param nb5 beginning index of fifth dimension
!> @param ne5 ending index of fifth dimension
!> @param nb6 beginning index of sixth dimension
!> @param ne6 ending index of sixth dimension
!>
!> Routine for allocating a rank 6 pointer array of type INTEGER from
!> \e nb1 to \e ne1, \e nb2 to \e ne2, \e nb3 to \e ne3, \e nb4 to \e ne4, 
!> \e nb5 to \e ne5, and \e nb6 to \e ne6.
    SUBROUTINE malloc_pi06(a,nb1,ne1,nb2,ne2,nb3,ne3,nb4,ne4,nb5,ne5,nb6,ne6)
      INTEGER(SIK),INTENT(IN) :: nb1,ne1,nb2,ne2,nb3,ne3,nb4,ne4,nb5,ne5
      INTEGER(SIK),INTENT(IN) :: nb6,ne6
      INTEGER(SNK),POINTER :: a(:,:,:,:,:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ASSOCIATED(a)) THEN
        NULLIFY(a)
        IF(nb1 <= ne1 .AND. nb2 <= ne2 .AND. nb3 <= ne3 .AND. nb4 <= ne4 &
                                       .AND. nb5 <= ne5 .AND. nb6 <= ne6) THEN
          ALLOCATE(a(nb1:ne1,nb2:ne2,nb3:ne3,nb4:ne4,nb5:ne5, &
                     nb6:ne6),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=0_SNK
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SNK,SRK)
          ELSE
            bsize=REAL(SNK,SRK)
            mysize=(REAL(ne1,SRK)-REAL(nb1,SRK)+1_SRK)+ &
                   (REAL(ne2,SRK)-REAL(nb2,SRK)+1_SRK)+ &
                   (REAL(ne3,SRK)-REAL(nb3,SRK)+1_SRK)+ &
                   (REAL(ne4,SRK)-REAL(nb4,SRK)+1_SRK)+ &
                   (REAL(ne5,SRK)-REAL(nb5,SRK)+1_SRK)+ &
                   (REAL(ne6,SRK)-REAL(nb6,SRK)+1_SRK)*bsize
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_pi06
!
!----------------------------------------
!> @brief Allocates rank 6 INTEGER pointer array.
!> @param a dummy argument for pointer array to be allocated
!> @param nb1 beginning index of first dimension
!> @param ne1 ending index of first dimension
!> @param nb2 beginning index of second dimension
!> @param ne2 ending index of second dimension
!> @param nb3 beginning index of third dimension
!> @param ne3 ending index of third dimension
!> @param nb4 beginning index of fourth dimension
!> @param ne4 ending index of fourth dimension
!> @param nb5 beginning index of fifth dimension
!> @param ne5 ending index of fifth dimension
!> @param nb6 beginning index of sixth dimension
!> @param ne6 ending index of sixth dimension
!>
!> Routine for allocating a rank 6 pointer array of type INTEGER from
!> \e nb1 to \e ne1, \e nb2 to \e ne2, \e nb3 to \e ne3, \e nb4 to \e ne4, 
!> \e nb5 to \e ne5, and \e nb6 to \e ne6.
    SUBROUTINE malloc_pl06(a,nb1,ne1,nb2,ne2,nb3,ne3,nb4,ne4,nb5,ne5,nb6,ne6)
      INTEGER(SIK),INTENT(IN) :: nb1,ne1,nb2,ne2,nb3,ne3,nb4,ne4,nb5,ne5
      INTEGER(SIK),INTENT(IN) :: nb6,ne6
      INTEGER(SLK),POINTER :: a(:,:,:,:,:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ASSOCIATED(a)) THEN
        NULLIFY(a)
        IF(nb1 <= ne1 .AND. nb2 <= ne2 .AND. nb3 <= ne3 .AND. nb4 <= ne4 &
                                       .AND. nb5 <= ne5 .AND. nb6 <= ne6) THEN
          ALLOCATE(a(nb1:ne1,nb2:ne2,nb3:ne3,nb4:ne4,nb5:ne5, &
                     nb6:ne6),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=0_SLK
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SLK,SRK)
          ELSE
            bsize=REAL(SLK,SRK)
            mysize=(REAL(ne1,SRK)-REAL(nb1,SRK)+1_SRK)+ &
                   (REAL(ne2,SRK)-REAL(nb2,SRK)+1_SRK)+ &
                   (REAL(ne3,SRK)-REAL(nb3,SRK)+1_SRK)+ &
                   (REAL(ne4,SRK)-REAL(nb4,SRK)+1_SRK)+ &
                   (REAL(ne5,SRK)-REAL(nb5,SRK)+1_SRK)+ &
                   (REAL(ne6,SRK)-REAL(nb6,SRK)+1_SRK)*bsize
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_pl06
!
!----------------------------------------
!> @brief Allocates rank 6 LOGICAL pointer array.
!> @param a dummy argument for pointer array to be allocated
!> @param nb1 beginning index of first dimension
!> @param ne1 ending index of first dimension
!> @param nb2 beginning index of second dimension
!> @param ne2 ending index of second dimension
!> @param nb3 beginning index of third dimension
!> @param ne3 ending index of third dimension
!> @param nb4 beginning index of fourth dimension
!> @param ne4 ending index of fourth dimension
!> @param nb5 beginning index of fifth dimension
!> @param ne5 ending index of fifth dimension
!> @param nb6 beginning index of sixth dimension
!> @param ne6 ending index of sixth dimension
!>
!> Routine for allocating a rank 6 pointer array of type LOGICAL from
!> \e nb1 to \e ne1, \e nb2 to \e ne2, \e nb3 to \e ne3, \e nb4 to \e ne4, 
!> \e nb5 to \e ne5, and \e nb6 to \e ne6.
    SUBROUTINE malloc_pb06(a,nb1,ne1,nb2,ne2,nb3,ne3,nb4,ne4,nb5,ne5,nb6,ne6)
      INTEGER(SIK),INTENT(IN) :: nb1,ne1,nb2,ne2,nb3,ne3,nb4,ne4,nb5,ne5
      INTEGER(SIK),INTENT(IN) :: nb6,ne6
      LOGICAL(SBK),POINTER :: a(:,:,:,:,:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ASSOCIATED(a)) THEN
        NULLIFY(a)
        IF(nb1 <= ne1 .AND. nb2 <= ne2 .AND. nb3 <= ne3 .AND. nb4 <= ne4 &
                                       .AND. nb5 <= ne5 .AND. nb6 <= ne6) THEN
          ALLOCATE(a(nb1:ne1,nb2:ne2,nb3:ne3,nb4:ne4,nb5:ne5, &
                     nb6:ne6),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=.FALSE.
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SBK,SRK)
          ELSE
            bsize=REAL(SBK,SRK)
            mysize=(REAL(ne1,SRK)-REAL(nb1,SRK)+1_SRK)+ &
                   (REAL(ne2,SRK)-REAL(nb2,SRK)+1_SRK)+ &
                   (REAL(ne3,SRK)-REAL(nb3,SRK)+1_SRK)+ &
                   (REAL(ne4,SRK)-REAL(nb4,SRK)+1_SRK)+ &
                   (REAL(ne5,SRK)-REAL(nb5,SRK)+1_SRK)+ &
                   (REAL(ne6,SRK)-REAL(nb6,SRK)+1_SRK)*bsize
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_pb06
!
!-------------------------------------------------------------------------------
! Allocate seven dimensional pointers
!
!> @brief Allocates rank 7 SINGLE precision pointer array.
!> @param a dummy argument for pointer array to be allocated
!> @param nb1 beginning index of first dimension
!> @param ne1 ending index of first dimension
!> @param nb2 beginning index of second dimension
!> @param ne2 ending index of second dimension
!> @param nb3 beginning index of third dimension
!> @param ne3 ending index of third dimension
!> @param nb4 beginning index of fourth dimension
!> @param ne4 ending index of fourth dimension
!> @param nb5 beginning index of fifth dimension
!> @param ne5 ending index of fifth dimension
!> @param nb6 beginning index of sixth dimension
!> @param ne6 ending index of sixth dimension
!> @param nb7 beginning index of seventh dimension
!> @param ne7 ending index of seventh dimension
!>
!> Routine for allocating a rank 7 pointer array of type SINGLE precision from
!> \e nb1 to \e ne1, \e nb2 to \e ne2, \e nb3 to \e ne3, \e nb4 to \e ne4, 
!> \e nb5 to \e ne5, \e nb6 to \e ne6, and \e nb7 to \e ne7.
    SUBROUTINE malloc_ps07(a,nb1,ne1,nb2,ne2,nb3,ne3,nb4,ne4,nb5,ne5, &
                           nb6,ne6,nb7,ne7)
      INTEGER(SIK),INTENT(IN) :: nb1,ne1,nb2,ne2,nb3,ne3,nb4,ne4,nb5,ne5
      INTEGER(SIK),INTENT(IN) :: nb6,ne6,nb7,ne7
      REAL(SSK),POINTER :: a(:,:,:,:,:,:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ASSOCIATED(a)) THEN
        NULLIFY(a)
        IF(nb1 <= ne1 .AND. nb2 <= ne2 .AND. nb3 <= ne3 .AND. nb4 <= ne4 &
                      .AND. nb5 <= ne5 .AND. nb6 <= ne6 .AND. nb7 <= ne7) THEN
          ALLOCATE(a(nb1:ne1,nb2:ne2,nb3:ne3,nb4:ne4,nb5:ne5, &
                     nb6:ne6,nb7:ne7),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=0.0_SSK
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SSK,SRK)
          ELSE
            bsize=REAL(SSK,SRK)
            mysize=(REAL(ne1,SRK)-REAL(nb1,SRK)+1_SRK)+ &
                   (REAL(ne2,SRK)-REAL(nb2,SRK)+1_SRK)+ &
                   (REAL(ne3,SRK)-REAL(nb3,SRK)+1_SRK)+ &
                   (REAL(ne4,SRK)-REAL(nb4,SRK)+1_SRK)+ &
                   (REAL(ne5,SRK)-REAL(nb5,SRK)+1_SRK)+ &
                   (REAL(ne6,SRK)-REAL(nb6,SRK)+1_SRK)+ &
                   (REAL(ne7,SRK)-REAL(nb7,SRK)+1_SRK)*bsize
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_ps07
!
!----------------------------------------
!> @brief Allocates rank 7 DOUBLE precision pointer array.
!> @param a dummy argument for pointer array to be allocated
!> @param nb1 beginning index of first dimension
!> @param ne1 ending index of first dimension
!> @param nb2 beginning index of second dimension
!> @param ne2 ending index of second dimension
!> @param nb3 beginning index of third dimension
!> @param ne3 ending index of third dimension
!> @param nb4 beginning index of fourth dimension
!> @param ne4 ending index of fourth dimension
!> @param nb5 beginning index of fifth dimension
!> @param ne5 ending index of fifth dimension
!> @param nb6 beginning index of sixth dimension
!> @param ne6 ending index of sixth dimension
!> @param nb7 beginning index of seventh dimension
!> @param ne7 ending index of seventh dimension
!>
!> Routine for allocating a rank 7 pointer array of type DOUBLE precision from
!> \e nb1 to \e ne1, \e nb2 to \e ne2, \e nb3 to \e ne3, \e nb4 to \e ne4, 
!> \e nb5 to \e ne5, \e nb6 to \e ne6, and \e nb7 to \e ne7.
    SUBROUTINE malloc_pd07(a,nb1,ne1,nb2,ne2,nb3,ne3,nb4,ne4,nb5,ne5, &
                           nb6,ne6,nb7,ne7)
      INTEGER(SIK),INTENT(IN) :: nb1,ne1,nb2,ne2,nb3,ne3,nb4,ne4,nb5,ne5
      INTEGER(SIK),INTENT(IN) :: nb6,ne6,nb7,ne7
      REAL(SDK),POINTER :: a(:,:,:,:,:,:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ASSOCIATED(a)) THEN
        NULLIFY(a)
        IF(nb1 <= ne1 .AND. nb2 <= ne2 .AND. nb3 <= ne3 .AND. nb4 <= ne4 &
                      .AND. nb5 <= ne5 .AND. nb6 <= ne6 .AND. nb7 <= ne7) THEN
          ALLOCATE(a(nb1:ne1,nb2:ne2,nb3:ne3,nb4:ne4,nb5:ne5, &
                     nb6:ne6,nb7:ne7),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=0.0_SDK
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SDK,SRK)
          ELSE
            bsize=REAL(SDK,SRK)
            mysize=(REAL(ne1,SRK)-REAL(nb1,SRK)+1_SRK)+ &
                   (REAL(ne2,SRK)-REAL(nb2,SRK)+1_SRK)+ &
                   (REAL(ne3,SRK)-REAL(nb3,SRK)+1_SRK)+ &
                   (REAL(ne4,SRK)-REAL(nb4,SRK)+1_SRK)+ &
                   (REAL(ne5,SRK)-REAL(nb5,SRK)+1_SRK)+ &
                   (REAL(ne6,SRK)-REAL(nb6,SRK)+1_SRK)+ &
                   (REAL(ne7,SRK)-REAL(nb7,SRK)+1_SRK)*bsize
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_pd07
!
!----------------------------------------
!> @brief Allocates rank 7 INTEGER pointer array.
!> @param a dummy argument for pointer array to be allocated
!> @param nb1 beginning index of first dimension
!> @param ne1 ending index of first dimension
!> @param nb2 beginning index of second dimension
!> @param ne2 ending index of second dimension
!> @param nb3 beginning index of third dimension
!> @param ne3 ending index of third dimension
!> @param nb4 beginning index of fourth dimension
!> @param ne4 ending index of fourth dimension
!> @param nb5 beginning index of fifth dimension
!> @param ne5 ending index of fifth dimension
!> @param nb6 beginning index of sixth dimension
!> @param ne6 ending index of sixth dimension
!> @param nb7 beginning index of seventh dimension
!> @param ne7 ending index of seventh dimension
!>
!> Routine for allocating a rank 7 pointer array of type INTEGER from
!> \e nb1 to \e ne1, \e nb2 to \e ne2, \e nb3 to \e ne3, \e nb4 to \e ne4, 
!> \e nb5 to \e ne5, \e nb6 to \e ne6, and \e nb7 to \e ne7.
    SUBROUTINE malloc_pi07(a,nb1,ne1,nb2,ne2,nb3,ne3,nb4,ne4,nb5,ne5, &
                           nb6,ne6,nb7,ne7)
      INTEGER(SIK),INTENT(IN) :: nb1,ne1,nb2,ne2,nb3,ne3,nb4,ne4,nb5,ne5
      INTEGER(SIK),INTENT(IN) :: nb6,ne6,nb7,ne7
      INTEGER(SNK),POINTER :: a(:,:,:,:,:,:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ASSOCIATED(a)) THEN
        NULLIFY(a)
        IF(nb1 <= ne1 .AND. nb2 <= ne2 .AND. nb3 <= ne3 .AND. nb4 <= ne4 &
                      .AND. nb5 <= ne5 .AND. nb6 <= ne6 .AND. nb7 <= ne7) THEN
          ALLOCATE(a(nb1:ne1,nb2:ne2,nb3:ne3,nb4:ne4,nb5:ne5, &
                     nb6:ne6,nb7:ne7),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=0_SNK
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SNK,SRK)
          ELSE
            bsize=REAL(SNK,SRK)
            mysize=(REAL(ne1,SRK)-REAL(nb1,SRK)+1_SRK)+ &
                   (REAL(ne2,SRK)-REAL(nb2,SRK)+1_SRK)+ &
                   (REAL(ne3,SRK)-REAL(nb3,SRK)+1_SRK)+ &
                   (REAL(ne4,SRK)-REAL(nb4,SRK)+1_SRK)+ &
                   (REAL(ne5,SRK)-REAL(nb5,SRK)+1_SRK)+ &
                   (REAL(ne6,SRK)-REAL(nb6,SRK)+1_SRK)+ &
                   (REAL(ne7,SRK)-REAL(nb7,SRK)+1_SRK)*bsize
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_pi07
!
!----------------------------------------
!> @brief Allocates rank 7 LONG integer pointer array.
!> @param a dummy argument for pointer array to be allocated
!> @param nb1 beginning index of first dimension
!> @param ne1 ending index of first dimension
!> @param nb2 beginning index of second dimension
!> @param ne2 ending index of second dimension
!> @param nb3 beginning index of third dimension
!> @param ne3 ending index of third dimension
!> @param nb4 beginning index of fourth dimension
!> @param ne4 ending index of fourth dimension
!> @param nb5 beginning index of fifth dimension
!> @param ne5 ending index of fifth dimension
!> @param nb6 beginning index of sixth dimension
!> @param ne6 ending index of sixth dimension
!> @param nb7 beginning index of seventh dimension
!> @param ne7 ending index of seventh dimension
!>
!> Routine for allocating a rank 7 pointer array of type LONG integer from
!> \e nb1 to \e ne1, \e nb2 to \e ne2, \e nb3 to \e ne3, \e nb4 to \e ne4, 
!> \e nb5 to \e ne5, \e nb6 to \e ne6, and \e nb7 to \e ne7.
    SUBROUTINE malloc_pl07(a,nb1,ne1,nb2,ne2,nb3,ne3,nb4,ne4,nb5,ne5, &
                           nb6,ne6,nb7,ne7)
      INTEGER(SIK),INTENT(IN) :: nb1,ne1,nb2,ne2,nb3,ne3,nb4,ne4,nb5,ne5
      INTEGER(SIK),INTENT(IN) :: nb6,ne6,nb7,ne7
      INTEGER(SLK),POINTER :: a(:,:,:,:,:,:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ASSOCIATED(a)) THEN
        NULLIFY(a)
        IF(nb1 <= ne1 .AND. nb2 <= ne2 .AND. nb3 <= ne3 .AND. nb4 <= ne4 &
                      .AND. nb5 <= ne5 .AND. nb6 <= ne6 .AND. nb7 <= ne7) THEN
          ALLOCATE(a(nb1:ne1,nb2:ne2,nb3:ne3,nb4:ne4,nb5:ne5, &
                     nb6:ne6,nb7:ne7),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=0_SLK
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SLK,SRK)
          ELSE
            bsize=REAL(SLK,SRK)
            mysize=(REAL(ne1,SRK)-REAL(nb1,SRK)+1_SRK)+ &
                   (REAL(ne2,SRK)-REAL(nb2,SRK)+1_SRK)+ &
                   (REAL(ne3,SRK)-REAL(nb3,SRK)+1_SRK)+ &
                   (REAL(ne4,SRK)-REAL(nb4,SRK)+1_SRK)+ &
                   (REAL(ne5,SRK)-REAL(nb5,SRK)+1_SRK)+ &
                   (REAL(ne6,SRK)-REAL(nb6,SRK)+1_SRK)+ &
                   (REAL(ne7,SRK)-REAL(nb7,SRK)+1_SRK)*bsize
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_pl07
!
!----------------------------------------
!> @brief Allocates rank 7 LOGICAL pointer array.
!> @param a dummy argument for pointer array to be allocated
!> @param nb1 beginning index of first dimension
!> @param ne1 ending index of first dimension
!> @param nb2 beginning index of second dimension
!> @param ne2 ending index of second dimension
!> @param nb3 beginning index of third dimension
!> @param ne3 ending index of third dimension
!> @param nb4 beginning index of fourth dimension
!> @param ne4 ending index of fourth dimension
!> @param nb5 beginning index of fifth dimension
!> @param ne5 ending index of fifth dimension
!> @param nb6 beginning index of sixth dimension
!> @param ne6 ending index of sixth dimension
!> @param nb7 beginning index of seventh dimension
!> @param ne7 ending index of seventh dimension
!>
!> Routine for allocating a rank 7 pointer array of type LOGICAL from
!> \e nb1 to \e ne1, \e nb2 to \e ne2, \e nb3 to \e ne3, \e nb4 to \e ne4, 
!> \e nb5 to \e ne5, \e nb6 to \e ne6, and \e nb7 to \e ne7.
    SUBROUTINE malloc_pb07(a,nb1,ne1,nb2,ne2,nb3,ne3,nb4,ne4,nb5,ne5, &
                           nb6,ne6,nb7,ne7)
      INTEGER(SIK),INTENT(IN) :: nb1,ne1,nb2,ne2,nb3,ne3,nb4,ne4,nb5,ne5
      INTEGER(SIK),INTENT(IN) :: nb6,ne6,nb7,ne7
      LOGICAL(SBK),POINTER :: a(:,:,:,:,:,:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ASSOCIATED(a)) THEN
        NULLIFY(a)
        IF(nb1 <= ne1 .AND. nb2 <= ne2 .AND. nb3 <= ne3 .AND. nb4 <= ne4 &
                      .AND. nb5 <= ne5 .AND. nb6 <= ne6 .AND. nb7 <= ne7) THEN
          ALLOCATE(a(nb1:ne1,nb2:ne2,nb3:ne3,nb4:ne4,nb5:ne5, &
                     nb6:ne6,nb7:ne7),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=.FALSE.
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SBK,SRK)
          ELSE
            bsize=REAL(SBK,SRK)
            mysize=(REAL(ne1,SRK)-REAL(nb1,SRK)+1_SRK)+ &
                   (REAL(ne2,SRK)-REAL(nb2,SRK)+1_SRK)+ &
                   (REAL(ne3,SRK)-REAL(nb3,SRK)+1_SRK)+ &
                   (REAL(ne4,SRK)-REAL(nb4,SRK)+1_SRK)+ &
                   (REAL(ne5,SRK)-REAL(nb5,SRK)+1_SRK)+ &
                   (REAL(ne6,SRK)-REAL(nb6,SRK)+1_SRK)+ &
                   (REAL(ne7,SRK)-REAL(nb7,SRK)+1_SRK)*bsize
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_pb07
!
!===============================================================================
! The following section contains all procedures for allocating arrays from nb
! to ne.
!
!> @brief Allocates rank 1 SINGLE precision allocatable array.
!> @param a dummy argument for allocatable array to be allocated
!> @param nb1 beginning index of first dimension
!> @param ne1 ending index of first dimension
!>
!> Routine for allocating a rank 1 allocatable array of type SINGLE precision from
!> \e nb1 to \e ne1.
    SUBROUTINE malloc_as01(a,nb1,ne1)
      INTEGER(SIK),INTENT(IN) :: nb1,ne1
      REAL(SSK),ALLOCATABLE,INTENT(INOUT) :: a(:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ALLOCATED(a)) THEN
        IF(nb1 <= ne1) THEN
          ALLOCATE(a(nb1:ne1),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=0.0_SSK
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SSK,SRK)
          ELSE
            bsize=REAL(SSK,SRK)
            mysize=(REAL(ne1,SRK)-REAL(nb1,SRK)+1_SRK)*bsize
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_as01
!
!----------------------------------------
!> @brief Allocates rank 1 DOUBLE precision allocatable array.
!> @param a dummy argument for allocatable array to be allocated
!> @param nb1 beginning index of first dimension
!> @param ne1 ending index of first dimension
!>
!> Routine for allocating a rank 1 allocatable array of type DOUBLE precision from
!> \e nb1 to \e ne1.
    SUBROUTINE malloc_ad01(a,nb1,ne1)
      INTEGER(SIK),INTENT(IN) :: nb1,ne1
      REAL(SDK),ALLOCATABLE,INTENT(INOUT) :: a(:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ALLOCATED(a)) THEN
        IF(nb1 <= ne1) THEN
          ALLOCATE(a(nb1:ne1),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=0.0_SDK
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SDK,SRK)
          ELSE
            bsize=REAL(SDK,SRK)
            mysize=(REAL(ne1,SRK)-REAL(nb1,SRK)+1_SRK)*bsize
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_ad01
!
!----------------------------------------
!> @brief Allocates rank 1 INTEGER allocatable array.
!> @param a dummy argument for allocatable array to be allocated
!> @param nb1 beginning index of first dimension
!> @param ne1 ending index of first dimension
!>
!> Routine for allocating a rank 1 allocatable array of type INTEGER from
!> \e nb1 to \e ne1.
    SUBROUTINE malloc_ai01(a,nb1,ne1)
      INTEGER(SIK),INTENT(IN) :: nb1,ne1
      INTEGER(SNK),ALLOCATABLE,INTENT(INOUT) :: a(:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ALLOCATED(a)) THEN
        IF(nb1 <= ne1) THEN
          ALLOCATE(a(nb1:ne1),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=0_SNK
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SNK,SRK)
          ELSE
            bsize=REAL(SNK,SRK)
            mysize=(REAL(ne1,SRK)-REAL(nb1,SRK)+1_SRK)*bsize
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_ai01
!
!----------------------------------------
!> @brief Allocates rank 1 LONG integer allocatable array.
!> @param a dummy argument for allocatable array to be allocated
!> @param nb1 beginning index of first dimension
!> @param ne1 ending index of first dimension
!>
!> Routine for allocating a rank 1 allocatable array of type LONG integer from
!> \e nb1 to \e ne1.
    SUBROUTINE malloc_al01(a,nb1,ne1)
      INTEGER(SIK),INTENT(IN) :: nb1,ne1
      INTEGER(SLK),ALLOCATABLE,INTENT(INOUT) :: a(:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ALLOCATED(a)) THEN
        IF(nb1 <= ne1) THEN
          ALLOCATE(a(nb1:ne1),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=0_SLK
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SLK,SRK)
          ELSE
            bsize=REAL(SLK,SRK)
            mysize=(REAL(ne1,SRK)-REAL(nb1,SRK)+1_SRK)*bsize
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_al01
!
!----------------------------------------
!> @brief Allocates rank 1 LOGICAL allocatable array.
!> @param a dummy argument for allocatable array to be allocated
!> @param nb1 beginning index of first dimension
!> @param ne1 ending index of first dimension
!>
!> Routine for allocating a rank 1 allocatable array of type LOGICAL from
!> \e nb1 to \e ne1.
    SUBROUTINE malloc_ab01(a,nb1,ne1)
      INTEGER(SIK),INTENT(IN) :: nb1,ne1
      LOGICAL(SBK),ALLOCATABLE,INTENT(INOUT) :: a(:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ALLOCATED(a)) THEN
        IF(nb1 <= ne1) THEN
          ALLOCATE(a(nb1:ne1),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=.FALSE.
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SBK,SRK)
          ELSE
            bsize=REAL(SBK,SRK)
            mysize=(REAL(ne1,SRK)-REAL(nb1,SRK)+1_SRK)*bsize
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_ab01
!
!-------------------------------------------------------------------------------
! Allocate two dimensional pointers
!
!> @brief Allocates rank 2 SINGLE precision allocatable array.
!> @param a dummy argument for allocatable array to be allocated
!> @param nb1 beginning index of first dimension
!> @param ne1 ending index of first dimension
!> @param nb2 beginning index of second dimension
!> @param ne2 ending index of second dimension
!>
!> Routine for allocating a rank 2 allocatable array of type SINGLE precision from
!> \e nb1 to \e ne1 and \e nb2 to \e ne2.
    SUBROUTINE malloc_as02(a,nb1,ne1,nb2,ne2)
      INTEGER(SIK),INTENT(IN) :: nb1,ne1,nb2,ne2
      REAL(SSK),ALLOCATABLE,INTENT(INOUT) :: a(:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ALLOCATED(a)) THEN
        IF(nb1 <= ne1 .AND. nb2 <= ne2) THEN
          ALLOCATE(a(nb1:ne1,nb2:ne2),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=0.0_SSK
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SSK,SRK)
          ELSE
            bsize=REAL(SSK,SRK)
            mysize=(REAL(ne1,SRK)-REAL(nb1,SRK)+1_SRK)+ &
                   (REAL(ne2,SRK)-REAL(nb2,SRK)+1_SRK)*bsize
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_as02
!
!----------------------------------------
!> @brief Allocates rank 2 DOUBLE precision allocatable array.
!> @param a dummy argument for allocatable array to be allocated
!> @param nb1 beginning index of first dimension
!> @param ne1 ending index of first dimension
!> @param nb2 beginning index of second dimension
!> @param ne2 ending index of second dimension
!>
!> Routine for allocating a rank 2 allocatable array of type DOUBLE precision from
!> \e nb1 to \e ne1 and \e nb2 to \e ne2.
    SUBROUTINE malloc_ad02(a,nb1,ne1,nb2,ne2)
      INTEGER(SIK),INTENT(IN) :: nb1,ne1,nb2,ne2
      REAL(SDK),ALLOCATABLE,INTENT(INOUT) :: a(:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ALLOCATED(a)) THEN
        IF(nb1 <= ne1 .AND. nb2 <= ne2) THEN
          ALLOCATE(a(nb1:ne1,nb2:ne2),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=0.0_SDK
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SDK,SRK)
          ELSE
            bsize=REAL(SDK,SRK)
            mysize=(REAL(ne1,SRK)-REAL(nb1,SRK)+1_SRK)+ &
                   (REAL(ne2,SRK)-REAL(nb2,SRK)+1_SRK)*bsize
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_ad02
!
!----------------------------------------
!> @brief Allocates rank 2 INTEGER allocatable array.
!> @param a dummy argument for allocatable array to be allocated
!> @param nb1 beginning index of first dimension
!> @param ne1 ending index of first dimension
!> @param nb2 beginning index of second dimension
!> @param ne2 ending index of second dimension
!>
!> Routine for allocating a rank 2 allocatable array of type INTEGER from
!> \e nb1 to \e ne1 and \e nb2 to \e ne2.
    SUBROUTINE malloc_ai02(a,nb1,ne1,nb2,ne2)
      INTEGER(SIK),INTENT(IN) :: nb1,ne1,nb2,ne2
      INTEGER(SNK),ALLOCATABLE,INTENT(INOUT) :: a(:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ALLOCATED(a)) THEN
        IF(nb1 <= ne1 .AND. nb2 <= ne2) THEN
          ALLOCATE(a(nb1:ne1,nb2:ne2),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=0_SNK
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SNK,SRK)
          ELSE
            bsize=REAL(SNK,SRK)
            mysize=(REAL(ne1,SRK)-REAL(nb1,SRK)+1_SRK)+ &
                   (REAL(ne2,SRK)-REAL(nb2,SRK)+1_SRK)*bsize
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_ai02
!
!----------------------------------------
!> @brief Allocates rank 2 LONG integer allocatable array.
!> @param a dummy argument for allocatable array to be allocated
!> @param nb1 beginning index of first dimension
!> @param ne1 ending index of first dimension
!> @param nb2 beginning index of second dimension
!> @param ne2 ending index of second dimension
!>
!> Routine for allocating a rank 2 allocatable array of type LONG integer from
!> \e nb1 to \e ne1 and \e nb2 to \e ne2.
    SUBROUTINE malloc_al02(a,nb1,ne1,nb2,ne2)
      INTEGER(SIK),INTENT(IN) :: nb1,ne1,nb2,ne2
      INTEGER(SLK),ALLOCATABLE,INTENT(INOUT) :: a(:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ALLOCATED(a)) THEN
        IF(nb1 <= ne1 .AND. nb2 <= ne2) THEN
          ALLOCATE(a(nb1:ne1,nb2:ne2),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=0_SLK
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SLK,SRK)
          ELSE
            bsize=REAL(SLK,SRK)
            mysize=(REAL(ne1,SRK)-REAL(nb1,SRK)+1_SRK)+ &
                   (REAL(ne2,SRK)-REAL(nb2,SRK)+1_SRK)*bsize
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_al02
!
!----------------------------------------
!> @brief Allocates rank 2 LOGICAL allocatable array.
!> @param a dummy argument for allocatable array to be allocated
!> @param nb1 beginning index of first dimension
!> @param ne1 ending index of first dimension
!> @param nb2 beginning index of second dimension
!> @param ne2 ending index of second dimension
!>
!> Routine for allocating a rank 2 allocatable array of type LOGICAL from
!> \e nb1 to \e ne1 and \e nb2 to \e ne2.
    SUBROUTINE malloc_ab02(a,nb1,ne1,nb2,ne2)
      INTEGER(SIK),INTENT(IN) :: nb1,ne1,nb2,ne2
      LOGICAL(SBK),ALLOCATABLE,INTENT(INOUT) :: a(:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ALLOCATED(a)) THEN
        IF(nb1 <= ne1 .AND. nb2 <= ne2) THEN
          ALLOCATE(a(nb1:ne1,nb2:ne2),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=.FALSE.
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SBK,SRK)
          ELSE
            bsize=REAL(SBK,SRK)
            mysize=(REAL(ne1,SRK)-REAL(nb1,SRK)+1_SRK)+ &
                   (REAL(ne2,SRK)-REAL(nb2,SRK)+1_SRK)*bsize
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_ab02
!
!-------------------------------------------------------------------------------
! Allocate three dimensional pointers
!
!> @brief Allocates rank 3 SINGLE precision allocatable array.
!> @param a dummy argument for allocatable array to be allocated
!> @param nb1 beginning index of first dimension
!> @param ne1 ending index of first dimension
!> @param nb2 beginning index of second dimension
!> @param ne2 ending index of second dimension
!> @param nb3 beginning index of third dimension
!> @param ne3 ending index of third dimension
!>
!> Routine for allocating a rank 3 allocatable array of type SINGLE precision from
!> \e nb1 to \e ne1, \e nb2 to \e ne2, and \e nb3 to \e ne3.
    SUBROUTINE malloc_as03(a,nb1,ne1,nb2,ne2,nb3,ne3)
      INTEGER(SIK),INTENT(IN) :: nb1,ne1,nb2,ne2,nb3,ne3
      REAL(SSK),ALLOCATABLE,INTENT(INOUT) :: a(:,:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ALLOCATED(a)) THEN
        IF(nb1 <= ne1 .AND. nb2 <= ne2 .AND. nb3 <= ne3) THEN
          ALLOCATE(a(nb1:ne1,nb2:ne2,nb3:ne3),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=0.0_SSK
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SSK,SRK)
          ELSE
            bsize=REAL(SSK,SRK)
            mysize=(REAL(ne1,SRK)-REAL(nb1,SRK)+1_SRK)+ &
                   (REAL(ne2,SRK)-REAL(nb2,SRK)+1_SRK)+ &
                   (REAL(ne3,SRK)-REAL(nb3,SRK)+1_SRK)*bsize
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_as03
!
!----------------------------------------
!> @brief Allocates rank 3 DOUBLE precision allocatable array.
!> @param a dummy argument for allocatable array to be allocated
!> @param nb1 beginning index of first dimension
!> @param ne1 ending index of first dimension
!> @param nb2 beginning index of second dimension
!> @param ne2 ending index of second dimension
!> @param nb3 beginning index of third dimension
!> @param ne3 ending index of third dimension
!>
!> Routine for allocating a rank 3 allocatable array of type DOUBLE precision from
!> \e nb1 to \e ne1, \e nb2 to \e ne2, and \e nb3 to \e ne3.
    SUBROUTINE malloc_ad03(a,nb1,ne1,nb2,ne2,nb3,ne3)
      INTEGER(SIK),INTENT(IN) :: nb1,ne1,nb2,ne2,nb3,ne3
      REAL(SDK),ALLOCATABLE,INTENT(INOUT) :: a(:,:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ALLOCATED(a)) THEN
        IF(nb1 <= ne1 .AND. nb2 <= ne2 .AND. nb3 <= ne3) THEN
          ALLOCATE(a(nb1:ne1,nb2:ne2,nb3:ne3),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=0.0_SDK
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SDK,SRK)
          ELSE
            bsize=REAL(SDK,SRK)
            mysize=(REAL(ne1,SRK)-REAL(nb1,SRK)+1_SRK)+ &
                   (REAL(ne2,SRK)-REAL(nb2,SRK)+1_SRK)+ &
                   (REAL(ne3,SRK)-REAL(nb3,SRK)+1_SRK)*bsize
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_ad03
!
!----------------------------------------
!> @brief Allocates rank 3 INTEGER allocatable array.
!> @param a dummy argument for allocatable array to be allocated
!> @param nb1 beginning index of first dimension
!> @param ne1 ending index of first dimension
!> @param nb2 beginning index of second dimension
!> @param ne2 ending index of second dimension
!> @param nb3 beginning index of third dimension
!> @param ne3 ending index of third dimension
!>
!> Routine for allocating a rank 3 allocatable array of type INTEGER from
!> \e nb1 to \e ne1, \e nb2 to \e ne2, and \e nb3 to \e ne3.
    SUBROUTINE malloc_ai03(a,nb1,ne1,nb2,ne2,nb3,ne3)
      INTEGER(SIK),INTENT(IN) :: nb1,ne1,nb2,ne2,nb3,ne3
      INTEGER(SNK),ALLOCATABLE,INTENT(INOUT) :: a(:,:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ALLOCATED(a)) THEN
        IF(nb1 <= ne1 .AND. nb2 <= ne2 .AND. nb3 <= ne3) THEN
          ALLOCATE(a(nb1:ne1,nb2:ne2,nb3:ne3),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=0_SNK
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SNK,SRK)
          ELSE
            bsize=REAL(SNK,SRK)
            mysize=(REAL(ne1,SRK)-REAL(nb1,SRK)+1_SRK)+ &
                   (REAL(ne2,SRK)-REAL(nb2,SRK)+1_SRK)+ &
                   (REAL(ne3,SRK)-REAL(nb3,SRK)+1_SRK)*bsize
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_ai03
!
!----------------------------------------
!> @brief Allocates rank 3 LONG integer allocatable array.
!> @param a dummy argument for allocatable array to be allocated
!> @param nb1 beginning index of first dimension
!> @param ne1 ending index of first dimension
!> @param nb2 beginning index of second dimension
!> @param ne2 ending index of second dimension
!> @param nb3 beginning index of third dimension
!> @param ne3 ending index of third dimension
!>
!> Routine for allocating a rank 3 allocatable array of type LONG integer from
!> \e nb1 to \e ne1, \e nb2 to \e ne2, and \e nb3 to \e ne3.
    SUBROUTINE malloc_al03(a,nb1,ne1,nb2,ne2,nb3,ne3)
      INTEGER(SIK),INTENT(IN) :: nb1,ne1,nb2,ne2,nb3,ne3
      INTEGER(SLK),ALLOCATABLE,INTENT(INOUT) :: a(:,:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ALLOCATED(a)) THEN
        IF(nb1 <= ne1 .AND. nb2 <= ne2 .AND. nb3 <= ne3) THEN
          ALLOCATE(a(nb1:ne1,nb2:ne2,nb3:ne3),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=0_SLK
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SLK,SRK)
          ELSE
            bsize=REAL(SLK,SRK)
            mysize=(REAL(ne1,SRK)-REAL(nb1,SRK)+1_SRK)+ &
                   (REAL(ne2,SRK)-REAL(nb2,SRK)+1_SRK)+ &
                   (REAL(ne3,SRK)-REAL(nb3,SRK)+1_SRK)*bsize
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_al03
!
!----------------------------------------
!> @brief Allocates rank 3 LOGICAL allocatable array.
!> @param a dummy argument for allocatable array to be allocated
!> @param nb1 beginning index of first dimension
!> @param ne1 ending index of first dimension
!> @param nb2 beginning index of second dimension
!> @param ne2 ending index of second dimension
!> @param nb3 beginning index of third dimension
!> @param ne3 ending index of third dimension
!>
!> Routine for allocating a rank 3 allocatable array of type LOGICAL from
!> \e nb1 to \e ne1, \e nb2 to \e ne2, and \e nb3 to \e ne3.
    SUBROUTINE malloc_ab03(a,nb1,ne1,nb2,ne2,nb3,ne3)
      INTEGER(SIK),INTENT(IN) :: nb1,ne1,nb2,ne2,nb3,ne3
      LOGICAL(SBK),ALLOCATABLE,INTENT(INOUT) :: a(:,:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ALLOCATED(a)) THEN
        IF(nb1 <= ne1 .AND. nb2 <= ne2 .AND. nb3 <= ne3) THEN
          ALLOCATE(a(nb1:ne1,nb2:ne2,nb3:ne3),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=.FALSE.
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SBK,SRK)
          ELSE
            bsize=REAL(SBK,SRK)
            mysize=(REAL(ne1,SRK)-REAL(nb1,SRK)+1_SRK)+ &
                   (REAL(ne2,SRK)-REAL(nb2,SRK)+1_SRK)+ &
                   (REAL(ne3,SRK)-REAL(nb3,SRK)+1_SRK)*bsize
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_ab03
!
!-------------------------------------------------------------------------------
! Allocate four dimensional pointers
!
!> @brief Allocates rank 4 SINGLE precision allocatable array.
!> @param a dummy argument for allocatable array to be allocated
!> @param nb1 beginning index of first dimension
!> @param ne1 ending index of first dimension
!> @param nb2 beginning index of second dimension
!> @param ne2 ending index of second dimension
!> @param nb3 beginning index of third dimension
!> @param ne3 ending index of third dimension
!> @param nb4 beginning index of fourth dimension
!> @param ne4 ending index of fourth dimension
!>
!> Routine for allocating a rank 4 allocatable array of type SINGLE precision from
!> \e nb1 to \e ne1, \e nb2 to \e ne2, \e nb3 to \e ne3, and \e nb4 to \e ne4.
    SUBROUTINE malloc_as04(a,nb1,ne1,nb2,ne2,nb3,ne3,nb4,ne4)
      INTEGER(SIK),INTENT(IN) :: nb1,ne1,nb2,ne2,nb3,ne3,nb4,ne4
      REAL(SSK),ALLOCATABLE,INTENT(INOUT) :: a(:,:,:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ALLOCATED(a)) THEN
        IF(nb1 <= ne1 .AND. nb2 <= ne2 .AND. nb3 <= ne3 .AND. nb4 <= ne4) THEN
          ALLOCATE(a(nb1:ne1,nb2:ne2,nb3:ne3,nb4:ne4),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=0.0_SSK
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SSK,SRK)
          ELSE
            bsize=REAL(SSK,SRK)
            mysize=(REAL(ne1,SRK)-REAL(nb1,SRK)+1_SRK)+ &
                   (REAL(ne2,SRK)-REAL(nb2,SRK)+1_SRK)+ &
                   (REAL(ne3,SRK)-REAL(nb3,SRK)+1_SRK)+ &
                   (REAL(ne4,SRK)-REAL(nb4,SRK)+1_SRK)*bsize
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_as04
!
!----------------------------------------
!> @brief Allocates rank 4 DOUBLE precision allocatable array.
!> @param a dummy argument for allocatable array to be allocated
!> @param nb1 beginning index of first dimension
!> @param ne1 ending index of first dimension
!> @param nb2 beginning index of second dimension
!> @param ne2 ending index of second dimension
!> @param nb3 beginning index of third dimension
!> @param ne3 ending index of third dimension
!> @param nb4 beginning index of fourth dimension
!> @param ne4 ending index of fourth dimension
!>
!> Routine for allocating a rank 4 allocatable array of type DOUBLE precision from
!> \e nb1 to \e ne1, \e nb2 to \e ne2, \e nb3 to \e ne3, and \e nb4 to \e ne4.
    SUBROUTINE malloc_ad04(a,nb1,ne1,nb2,ne2,nb3,ne3,nb4,ne4)
      INTEGER(SIK),INTENT(IN) :: nb1,ne1,nb2,ne2,nb3,ne3,nb4,ne4
      REAL(SDK),ALLOCATABLE,INTENT(INOUT) :: a(:,:,:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ALLOCATED(a)) THEN
        IF(nb1 <= ne1 .AND. nb2 <= ne2 .AND. nb3 <= ne3 .AND. nb4 <= ne4) THEN
          ALLOCATE(a(nb1:ne1,nb2:ne2,nb3:ne3,nb4:ne4),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=0.0_SDK
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SDK,SRK)
          ELSE
            bsize=REAL(SDK,SRK)
            mysize=(REAL(ne1,SRK)-REAL(nb1,SRK)+1_SRK)+ &
                   (REAL(ne2,SRK)-REAL(nb2,SRK)+1_SRK)+ &
                   (REAL(ne3,SRK)-REAL(nb3,SRK)+1_SRK)+ &
                   (REAL(ne4,SRK)-REAL(nb4,SRK)+1_SRK)*bsize
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_ad04
!
!----------------------------------------
!> @brief Allocates rank 4 INTEGER allocatable array.
!> @param a dummy argument for allocatable array to be allocated
!> @param nb1 beginning index of first dimension
!> @param ne1 ending index of first dimension
!> @param nb2 beginning index of second dimension
!> @param ne2 ending index of second dimension
!> @param nb3 beginning index of third dimension
!> @param ne3 ending index of third dimension
!> @param nb4 beginning index of fourth dimension
!> @param ne4 ending index of fourth dimension
!>
!> Routine for allocating a rank 4 allocatable array of type INTEGER from
!> \e nb1 to \e ne1, \e nb2 to \e ne2, \e nb3 to \e ne3, and \e nb4 to \e ne4.
    SUBROUTINE malloc_ai04(a,nb1,ne1,nb2,ne2,nb3,ne3,nb4,ne4)
      INTEGER(SIK),INTENT(IN) :: nb1,ne1,nb2,ne2,nb3,ne3,nb4,ne4
      INTEGER(SNK),ALLOCATABLE,INTENT(INOUT) :: a(:,:,:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ALLOCATED(a)) THEN
        IF(nb1 <= ne1 .AND. nb2 <= ne2 .AND. nb3 <= ne3 .AND. nb4 <= ne4) THEN
          ALLOCATE(a(nb1:ne1,nb2:ne2,nb3:ne3,nb4:ne4),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=0_SNK
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SNK,SRK)
          ELSE
            bsize=REAL(SNK,SRK)
            mysize=(REAL(ne1,SRK)-REAL(nb1,SRK)+1_SRK)+ &
                   (REAL(ne2,SRK)-REAL(nb2,SRK)+1_SRK)+ &
                   (REAL(ne3,SRK)-REAL(nb3,SRK)+1_SRK)+ &
                   (REAL(ne4,SRK)-REAL(nb4,SRK)+1_SRK)*bsize
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_ai04
!
!----------------------------------------
!> @brief Allocates rank 4 LONG integer allocatable array.
!> @param a dummy argument for allocatable array to be allocated
!> @param nb1 beginning index of first dimension
!> @param ne1 ending index of first dimension
!> @param nb2 beginning index of second dimension
!> @param ne2 ending index of second dimension
!> @param nb3 beginning index of third dimension
!> @param ne3 ending index of third dimension
!> @param nb4 beginning index of fourth dimension
!> @param ne4 ending index of fourth dimension
!>
!> Routine for allocating a rank 4 allocatable array of type LONG integer from
!> \e nb1 to \e ne1, \e nb2 to \e ne2, \e nb3 to \e ne3, and \e nb4 to \e ne4.
    SUBROUTINE malloc_al04(a,nb1,ne1,nb2,ne2,nb3,ne3,nb4,ne4)
      INTEGER(SIK),INTENT(IN) :: nb1,ne1,nb2,ne2,nb3,ne3,nb4,ne4
      INTEGER(SLK),ALLOCATABLE,INTENT(INOUT) :: a(:,:,:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ALLOCATED(a)) THEN
        IF(nb1 <= ne1 .AND. nb2 <= ne2 .AND. nb3 <= ne3 .AND. nb4 <= ne4) THEN
          ALLOCATE(a(nb1:ne1,nb2:ne2,nb3:ne3,nb4:ne4),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=0_SLK
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SLK,SRK)
          ELSE
            bsize=REAL(SLK,SRK)
            mysize=(REAL(ne1,SRK)-REAL(nb1,SRK)+1_SRK)+ &
                   (REAL(ne2,SRK)-REAL(nb2,SRK)+1_SRK)+ &
                   (REAL(ne3,SRK)-REAL(nb3,SRK)+1_SRK)+ &
                   (REAL(ne4,SRK)-REAL(nb4,SRK)+1_SRK)*bsize
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_al04
!
!----------------------------------------
!> @brief Allocates rank 4 INTEGER allocatable array.
!> @param a dummy argument for allocatable array to be allocated
!> @param nb1 beginning index of first dimension
!> @param ne1 ending index of first dimension
!> @param nb2 beginning index of second dimension
!> @param ne2 ending index of second dimension
!> @param nb3 beginning index of third dimension
!> @param ne3 ending index of third dimension
!> @param nb4 beginning index of fourth dimension
!> @param ne4 ending index of fourth dimension
!>
!> Routine for allocating a rank 4 allocatable array of type INTEGER from
!> \e nb1 to \e ne1, \e nb2 to \e ne2, \e nb3 to \e ne3, and \e nb4 to \e ne4.
    SUBROUTINE malloc_ab04(a,nb1,ne1,nb2,ne2,nb3,ne3,nb4,ne4)
      INTEGER(SIK),INTENT(IN) :: nb1,ne1,nb2,ne2,nb3,ne3,nb4,ne4
      LOGICAL(SBK),ALLOCATABLE,INTENT(INOUT) :: a(:,:,:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ALLOCATED(a)) THEN
        IF(nb1 <= ne1 .AND. nb2 <= ne2 .AND. nb3 <= ne3 .AND. nb4 <= ne4) THEN
          ALLOCATE(a(nb1:ne1,nb2:ne2,nb3:ne3,nb4:ne4),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=.FALSE.
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SBK,SRK)
          ELSE
            bsize=REAL(SBK,SRK)
            mysize=(REAL(ne1,SRK)-REAL(nb1,SRK)+1_SRK)+ &
                   (REAL(ne2,SRK)-REAL(nb2,SRK)+1_SRK)+ &
                   (REAL(ne3,SRK)-REAL(nb3,SRK)+1_SRK)+ &
                   (REAL(ne4,SRK)-REAL(nb4,SRK)+1_SRK)*bsize
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_ab04
!
!-------------------------------------------------------------------------------
! Allocate five dimensional pointers
!
!> @brief Allocates rank 5 SINGLE precision allocatable array.
!> @param a dummy argument for allocatable array to be allocated
!> @param nb1 beginning index of first dimension
!> @param ne1 ending index of first dimension
!> @param nb2 beginning index of second dimension
!> @param ne2 ending index of second dimension
!> @param nb3 beginning index of third dimension
!> @param ne3 ending index of third dimension
!> @param nb4 beginning index of fourth dimension
!> @param ne4 ending index of fourth dimension
!> @param nb5 beginning index of fifth dimension
!> @param ne5 ending index of fifth dimension
!>
!> Routine for allocating a rank 5 allocatable array of type SINGLE precision from
!> \e nb1 to \e ne1, \e nb2 to \e ne2, \e nb3 to \e ne3, \e nb4 to \e ne4, 
!> and \e nb5 to \e ne5.
    SUBROUTINE malloc_as05(a,nb1,ne1,nb2,ne2,nb3,ne3,nb4,ne4,nb5,ne5)
      INTEGER(SIK),INTENT(IN) :: nb1,ne1,nb2,ne2,nb3,ne3,nb4,ne4,nb5,ne5
      REAL(SSK),ALLOCATABLE,INTENT(INOUT) :: a(:,:,:,:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ALLOCATED(a)) THEN
        IF(nb1 <= ne1 .AND. nb2 <= ne2 .AND. nb3 <= ne3 .AND. nb4 <= ne4 &
                                                       .AND. nb5 <= ne5) THEN
          ALLOCATE(a(nb1:ne1,nb2:ne2,nb3:ne3,nb4:ne4,nb5:ne5),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=0.0_SSK
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SSK,SRK)
          ELSE
            bsize=REAL(SSK,SRK)
            mysize=(REAL(ne1,SRK)-REAL(nb1,SRK)+1_SRK)+ &
                   (REAL(ne2,SRK)-REAL(nb2,SRK)+1_SRK)+ &
                   (REAL(ne3,SRK)-REAL(nb3,SRK)+1_SRK)+ &
                   (REAL(ne4,SRK)-REAL(nb4,SRK)+1_SRK)+ &
                   (REAL(ne5,SRK)-REAL(nb5,SRK)+1_SRK)*bsize
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_as05
!
!----------------------------------------
!> @brief Allocates rank 5 DOUBLE precision allocatable array.
!> @param a dummy argument for allocatable array to be allocated
!> @param nb1 beginning index of first dimension
!> @param ne1 ending index of first dimension
!> @param nb2 beginning index of second dimension
!> @param ne2 ending index of second dimension
!> @param nb3 beginning index of third dimension
!> @param ne3 ending index of third dimension
!> @param nb4 beginning index of fourth dimension
!> @param ne4 ending index of fourth dimension
!> @param nb5 beginning index of fifth dimension
!> @param ne5 ending index of fifth dimension
!>
!> Routine for allocating a rank 5 allocatable array of type DOUBLE precision from
!> \e nb1 to \e ne1, \e nb2 to \e ne2, \e nb3 to \e ne3, \e nb4 to \e ne4, 
!> and \e nb5 to \e ne5.
    SUBROUTINE malloc_ad05(a,nb1,ne1,nb2,ne2,nb3,ne3,nb4,ne4,nb5,ne5)
      INTEGER(SIK),INTENT(IN) :: nb1,ne1,nb2,ne2,nb3,ne3,nb4,ne4,nb5,ne5
      REAL(SDK),ALLOCATABLE,INTENT(INOUT) :: a(:,:,:,:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ALLOCATED(a)) THEN
        IF(nb1 <= ne1 .AND. nb2 <= ne2 .AND. nb3 <= ne3 .AND. nb4 <= ne4 &
                                                       .AND. nb5 <= ne5) THEN
          ALLOCATE(a(nb1:ne1,nb2:ne2,nb3:ne3,nb4:ne4,nb5:ne5),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=0.0_SDK
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SDK,SRK)
          ELSE
            bsize=REAL(SDK,SRK)
            mysize=(REAL(ne1,SRK)-REAL(nb1,SRK)+1_SRK)+ &
                   (REAL(ne2,SRK)-REAL(nb2,SRK)+1_SRK)+ &
                   (REAL(ne3,SRK)-REAL(nb3,SRK)+1_SRK)+ &
                   (REAL(ne4,SRK)-REAL(nb4,SRK)+1_SRK)+ &
                   (REAL(ne5,SRK)-REAL(nb5,SRK)+1_SRK)*bsize
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_ad05
!
!----------------------------------------
!> @brief Allocates rank 5 INTEGER allocatable array.
!> @param a dummy argument for allocatable array to be allocated
!> @param nb1 beginning index of first dimension
!> @param ne1 ending index of first dimension
!> @param nb2 beginning index of second dimension
!> @param ne2 ending index of second dimension
!> @param nb3 beginning index of third dimension
!> @param ne3 ending index of third dimension
!> @param nb4 beginning index of fourth dimension
!> @param ne4 ending index of fourth dimension
!> @param nb5 beginning index of fifth dimension
!> @param ne5 ending index of fifth dimension
!>
!> Routine for allocating a rank 5 allocatable array of type INTEGER from
!> \e nb1 to \e ne1, \e nb2 to \e ne2, \e nb3 to \e ne3, \e nb4 to \e ne4, 
!> and \e nb5 to \e ne5.
    SUBROUTINE malloc_ai05(a,nb1,ne1,nb2,ne2,nb3,ne3,nb4,ne4,nb5,ne5)
      INTEGER(SIK),INTENT(IN) :: nb1,ne1,nb2,ne2,nb3,ne3,nb4,ne4,nb5,ne5
      INTEGER(SNK),ALLOCATABLE,INTENT(INOUT) :: a(:,:,:,:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ALLOCATED(a)) THEN
        IF(nb1 <= ne1 .AND. nb2 <= ne2 .AND. nb3 <= ne3 .AND. nb4 <= ne4 &
                                                        .AND. nb5 <= ne5) THEN
          ALLOCATE(a(nb1:ne1,nb2:ne2,nb3:ne3,nb4:ne4,nb5:ne5),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=0_SNK
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SNK,SRK)
          ELSE
            bsize=REAL(SNK,SRK)
            mysize=(REAL(ne1,SRK)-REAL(nb1,SRK)+1_SRK)+ &
                   (REAL(ne2,SRK)-REAL(nb2,SRK)+1_SRK)+ &
                   (REAL(ne3,SRK)-REAL(nb3,SRK)+1_SRK)+ &
                   (REAL(ne4,SRK)-REAL(nb4,SRK)+1_SRK)+ &
                   (REAL(ne5,SRK)-REAL(nb5,SRK)+1_SRK)*bsize
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_ai05
!
!----------------------------------------
!> @brief Allocates rank 5 LONG integer allocatable array.
!> @param a dummy argument for allocatable array to be allocated
!> @param nb1 beginning index of first dimension
!> @param ne1 ending index of first dimension
!> @param nb2 beginning index of second dimension
!> @param ne2 ending index of second dimension
!> @param nb3 beginning index of third dimension
!> @param ne3 ending index of third dimension
!> @param nb4 beginning index of fourth dimension
!> @param ne4 ending index of fourth dimension
!> @param nb5 beginning index of fifth dimension
!> @param ne5 ending index of fifth dimension
!>
!> Routine for allocating a rank 5 allocatable array of type LONG integer from
!> \e nb1 to \e ne1, \e nb2 to \e ne2, \e nb3 to \e ne3, \e nb4 to \e ne4, 
!> and \e nb5 to \e ne5.
    SUBROUTINE malloc_al05(a,nb1,ne1,nb2,ne2,nb3,ne3,nb4,ne4,nb5,ne5)
      INTEGER(SIK),INTENT(IN) :: nb1,ne1,nb2,ne2,nb3,ne3,nb4,ne4,nb5,ne5
      INTEGER(SLK),ALLOCATABLE,INTENT(INOUT) :: a(:,:,:,:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

        IF(.NOT.ALLOCATED(a)) THEN
          IF(nb1 <= ne1 .AND. nb2 <= ne2 .AND. nb3 <= ne3 .AND. nb4 <= ne4 &
                                                       .AND. nb5 <= ne5) THEN
          ALLOCATE(a(nb1:ne1,nb2:ne2,nb3:ne3,nb4:ne4,nb5:ne5),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=0_SLK
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SLK,SRK)
          ELSE
            bsize=REAL(SLK,SRK)
            mysize=(REAL(ne1,SRK)-REAL(nb1,SRK)+1_SRK)+ &
                   (REAL(ne2,SRK)-REAL(nb2,SRK)+1_SRK)+ &
                   (REAL(ne3,SRK)-REAL(nb3,SRK)+1_SRK)+ &
                   (REAL(ne4,SRK)-REAL(nb4,SRK)+1_SRK)+ &
                   (REAL(ne5,SRK)-REAL(nb5,SRK)+1_SRK)*bsize
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_al05
!
!----------------------------------------
!> @brief Allocates rank 5 LOGICAL allocatable array.
!> @param a dummy argument for allocatable array to be allocated
!> @param nb1 beginning index of first dimension
!> @param ne1 ending index of first dimension
!> @param nb2 beginning index of second dimension
!> @param ne2 ending index of second dimension
!> @param nb3 beginning index of third dimension
!> @param ne3 ending index of third dimension
!> @param nb4 beginning index of fourth dimension
!> @param ne4 ending index of fourth dimension
!> @param nb5 beginning index of fifth dimension
!> @param ne5 ending index of fifth dimension
!>
!> Routine for allocating a rank 5 allocatable array of type LOGICAL from
!> \e nb1 to \e ne1, \e nb2 to \e ne2, \e nb3 to \e ne3, \e nb4 to \e ne4, 
!> and \e nb5 to \e ne5.
    SUBROUTINE malloc_ab05(a,nb1,ne1,nb2,ne2,nb3,ne3,nb4,ne4,nb5,ne5)
      INTEGER(SIK),INTENT(IN) :: nb1,ne1,nb2,ne2,nb3,ne3,nb4,ne4,nb5,ne5
      LOGICAL(SBK),ALLOCATABLE,INTENT(INOUT) :: a(:,:,:,:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ALLOCATED(a)) THEN
        IF(nb1 <= ne1 .AND. nb2 <= ne2 .AND. nb3 <= ne3 .AND. nb4 <= ne4 &
                                                       .AND. nb5 <= ne5) THEN
          ALLOCATE(a(nb1:ne1,nb2:ne2,nb3:ne3,nb4:ne4,nb5:ne5),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=.FALSE.
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SBK,SRK)
          ELSE
            bsize=REAL(SBK,SRK)
            mysize=(REAL(ne1,SRK)-REAL(nb1,SRK)+1_SRK)+ &
                   (REAL(ne2,SRK)-REAL(nb2,SRK)+1_SRK)+ &
                   (REAL(ne3,SRK)-REAL(nb3,SRK)+1_SRK)+ &
                   (REAL(ne4,SRK)-REAL(nb4,SRK)+1_SRK)+ &
                   (REAL(ne5,SRK)-REAL(nb5,SRK)+1_SRK)*bsize
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_ab05
!
!-------------------------------------------------------------------------------
! Allocate six dimensional pointers
!
!> @brief Allocates rank 6 SINGLE precision allocatable array.
!> @param a dummy argument for allocatable array to be allocated
!> @param nb1 beginning index of first dimension
!> @param ne1 ending index of first dimension
!> @param nb2 beginning index of second dimension
!> @param ne2 ending index of second dimension
!> @param nb3 beginning index of third dimension
!> @param ne3 ending index of third dimension
!> @param nb4 beginning index of fourth dimension
!> @param ne4 ending index of fourth dimension
!> @param nb5 beginning index of fifth dimension
!> @param ne5 ending index of fifth dimension
!> @param nb6 beginning index of sixth dimension
!> @param ne6 ending index of sixth dimension
!>
!> Routine for allocating a rank 6 allocatable array of type SINGLE precision from
!> \e nb1 to \e ne1, \e nb2 to \e ne2, \e nb3 to \e ne3, \e nb4 to \e ne4, 
!> \e nb5 to \e ne5, and \e nb6 to \e ne6.
    SUBROUTINE malloc_as06(a,nb1,ne1,nb2,ne2,nb3,ne3,nb4,ne4,nb5,ne5,nb6,ne6)
      INTEGER(SIK),INTENT(IN) :: nb1,ne1,nb2,ne2,nb3,ne3,nb4,ne4,nb5,ne5
      INTEGER(SIK),INTENT(IN) :: nb6,ne6
      REAL(SSK),ALLOCATABLE,INTENT(INOUT) :: a(:,:,:,:,:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ALLOCATED(a)) THEN
        IF(nb1 <= ne1 .AND. nb2 <= ne2 .AND. nb3 <= ne3 .AND. nb4 <= ne4 &
                                       .AND. nb5 <= ne5 .AND. nb6 <= ne6) THEN
          ALLOCATE(a(nb1:ne1,nb2:ne2,nb3:ne3,nb4:ne4,nb5:ne5, &
                     nb6:ne6),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=0.0_SSK
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SSK,SRK)
          ELSE
            bsize=REAL(SSK,SRK)
            mysize=(REAL(ne1,SRK)-REAL(nb1,SRK)+1_SRK)+ &
                   (REAL(ne2,SRK)-REAL(nb2,SRK)+1_SRK)+ &
                   (REAL(ne3,SRK)-REAL(nb3,SRK)+1_SRK)+ &
                   (REAL(ne4,SRK)-REAL(nb4,SRK)+1_SRK)+ &
                   (REAL(ne5,SRK)-REAL(nb5,SRK)+1_SRK)+ &
                   (REAL(ne6,SRK)-REAL(nb6,SRK)+1_SRK)*bsize
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_as06
!
!----------------------------------------
!> @brief Allocates rank 6 DOUBLE precision allocatable array.
!> @param a dummy argument for allocatable array to be allocated
!> @param nb1 beginning index of first dimension
!> @param ne1 ending index of first dimension
!> @param nb2 beginning index of second dimension
!> @param ne2 ending index of second dimension
!> @param nb3 beginning index of third dimension
!> @param ne3 ending index of third dimension
!> @param nb4 beginning index of fourth dimension
!> @param ne4 ending index of fourth dimension
!> @param nb5 beginning index of fifth dimension
!> @param ne5 ending index of fifth dimension
!> @param nb6 beginning index of sixth dimension
!> @param ne6 ending index of sixth dimension
!>
!> Routine for allocating a rank 6 allocatable array of type DOUBLE precision from
!> \e nb1 to \e ne1, \e nb2 to \e ne2, \e nb3 to \e ne3, \e nb4 to \e ne4, 
!> \e nb5 to \e ne5, and \e nb6 to \e ne6.
    SUBROUTINE malloc_ad06(a,nb1,ne1,nb2,ne2,nb3,ne3,nb4,ne4,nb5,ne5,nb6,ne6)
      INTEGER(SIK),INTENT(IN) :: nb1,ne1,nb2,ne2,nb3,ne3,nb4,ne4,nb5,ne5
      INTEGER(SIK),INTENT(IN) :: nb6,ne6
      REAL(SDK),ALLOCATABLE,INTENT(INOUT) :: a(:,:,:,:,:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ALLOCATED(a)) THEN
        IF(nb1 <= ne1 .AND. nb2 <= ne2 .AND. nb3 <= ne3 .AND. nb4 <= ne4 &
                                       .AND. nb5 <= ne5 .AND. nb6 <= ne6) THEN
          ALLOCATE(a(nb1:ne1,nb2:ne2,nb3:ne3,nb4:ne4,nb5:ne5, &
                     nb6:ne6),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=0.0_SDK
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SDK,SRK)
          ELSE
            bsize=REAL(SDK,SRK)
            mysize=(REAL(ne1,SRK)-REAL(nb1,SRK)+1_SRK)+ &
                   (REAL(ne2,SRK)-REAL(nb2,SRK)+1_SRK)+ &
                   (REAL(ne3,SRK)-REAL(nb3,SRK)+1_SRK)+ &
                   (REAL(ne4,SRK)-REAL(nb4,SRK)+1_SRK)+ &
                   (REAL(ne5,SRK)-REAL(nb5,SRK)+1_SRK)+ &
                   (REAL(ne6,SRK)-REAL(nb6,SRK)+1_SRK)*bsize
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_ad06
!
!----------------------------------------
!> @brief Allocates rank 6 INTEGER allocatable array.
!> @param a dummy argument for allocatable array to be allocated
!> @param nb1 beginning index of first dimension
!> @param ne1 ending index of first dimension
!> @param nb2 beginning index of second dimension
!> @param ne2 ending index of second dimension
!> @param nb3 beginning index of third dimension
!> @param ne3 ending index of third dimension
!> @param nb4 beginning index of fourth dimension
!> @param ne4 ending index of fourth dimension
!> @param nb5 beginning index of fifth dimension
!> @param ne5 ending index of fifth dimension
!> @param nb6 beginning index of sixth dimension
!> @param ne6 ending index of sixth dimension
!>
!> Routine for allocating a rank 6 allocatable array of type INTEGER from
!> \e nb1 to \e ne1, \e nb2 to \e ne2, \e nb3 to \e ne3, \e nb4 to \e ne4, 
!> \e nb5 to \e ne5, and \e nb6 to \e ne6.
    SUBROUTINE malloc_ai06(a,nb1,ne1,nb2,ne2,nb3,ne3,nb4,ne4,nb5,ne5,nb6,ne6)
      INTEGER(SIK),INTENT(IN) :: nb1,ne1,nb2,ne2,nb3,ne3,nb4,ne4,nb5,ne5
      INTEGER(SIK),INTENT(IN) :: nb6,ne6
      INTEGER(SNK),ALLOCATABLE,INTENT(INOUT) :: a(:,:,:,:,:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ALLOCATED(a)) THEN
        IF(nb1 <= ne1 .AND. nb2 <= ne2 .AND. nb3 <= ne3 .AND. nb4 <= ne4 &
                                       .AND. nb5 <= ne5 .AND. nb6 <= ne6) THEN
          ALLOCATE(a(nb1:ne1,nb2:ne2,nb3:ne3,nb4:ne4,nb5:ne5, &
                     nb6:ne6),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=0_SNK
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SNK,SRK)
          ELSE
            bsize=REAL(SNK,SRK)
            mysize=(REAL(ne1,SRK)-REAL(nb1,SRK)+1_SRK)+ &
                   (REAL(ne2,SRK)-REAL(nb2,SRK)+1_SRK)+ &
                   (REAL(ne3,SRK)-REAL(nb3,SRK)+1_SRK)+ &
                   (REAL(ne4,SRK)-REAL(nb4,SRK)+1_SRK)+ &
                   (REAL(ne5,SRK)-REAL(nb5,SRK)+1_SRK)+ &
                   (REAL(ne6,SRK)-REAL(nb6,SRK)+1_SRK)*bsize
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_ai06
!
!----------------------------------------
!> @brief Allocates rank 6 INTEGER allocatable array.
!> @param a dummy argument for allocatable array to be allocated
!> @param nb1 beginning index of first dimension
!> @param ne1 ending index of first dimension
!> @param nb2 beginning index of second dimension
!> @param ne2 ending index of second dimension
!> @param nb3 beginning index of third dimension
!> @param ne3 ending index of third dimension
!> @param nb4 beginning index of fourth dimension
!> @param ne4 ending index of fourth dimension
!> @param nb5 beginning index of fifth dimension
!> @param ne5 ending index of fifth dimension
!> @param nb6 beginning index of sixth dimension
!> @param ne6 ending index of sixth dimension
!>
!> Routine for allocating a rank 6 allocatable array of type INTEGER from
!> \e nb1 to \e ne1, \e nb2 to \e ne2, \e nb3 to \e ne3, \e nb4 to \e ne4, 
!> \e nb5 to \e ne5, and \e nb6 to \e ne6.
    SUBROUTINE malloc_al06(a,nb1,ne1,nb2,ne2,nb3,ne3,nb4,ne4,nb5,ne5,nb6,ne6)
      INTEGER(SIK),INTENT(IN) :: nb1,ne1,nb2,ne2,nb3,ne3,nb4,ne4,nb5,ne5
      INTEGER(SIK),INTENT(IN) :: nb6,ne6
      INTEGER(SLK),ALLOCATABLE,INTENT(INOUT) :: a(:,:,:,:,:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ALLOCATED(a)) THEN
        IF(nb1 <= ne1 .AND. nb2 <= ne2 .AND. nb3 <= ne3 .AND. nb4 <= ne4 &
                                       .AND. nb5 <= ne5 .AND. nb6 <= ne6) THEN
          ALLOCATE(a(nb1:ne1,nb2:ne2,nb3:ne3,nb4:ne4,nb5:ne5, &
                     nb6:ne6),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=0_SLK
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SLK,SRK)
          ELSE
            bsize=REAL(SLK,SRK)
            mysize=(REAL(ne1,SRK)-REAL(nb1,SRK)+1_SRK)+ &
                   (REAL(ne2,SRK)-REAL(nb2,SRK)+1_SRK)+ &
                   (REAL(ne3,SRK)-REAL(nb3,SRK)+1_SRK)+ &
                   (REAL(ne4,SRK)-REAL(nb4,SRK)+1_SRK)+ &
                   (REAL(ne5,SRK)-REAL(nb5,SRK)+1_SRK)+ &
                   (REAL(ne6,SRK)-REAL(nb6,SRK)+1_SRK)*bsize
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_al06
!
!----------------------------------------
!> @brief Allocates rank 6 LOGICAL allocatable array.
!> @param a dummy argument for allocatable array to be allocated
!> @param nb1 beginning index of first dimension
!> @param ne1 ending index of first dimension
!> @param nb2 beginning index of second dimension
!> @param ne2 ending index of second dimension
!> @param nb3 beginning index of third dimension
!> @param ne3 ending index of third dimension
!> @param nb4 beginning index of fourth dimension
!> @param ne4 ending index of fourth dimension
!> @param nb5 beginning index of fifth dimension
!> @param ne5 ending index of fifth dimension
!> @param nb6 beginning index of sixth dimension
!> @param ne6 ending index of sixth dimension
!>
!> Routine for allocating a rank 6 allocatable array of type LOGICAL from
!> \e nb1 to \e ne1, \e nb2 to \e ne2, \e nb3 to \e ne3, \e nb4 to \e ne4, 
!> \e nb5 to \e ne5, and \e nb6 to \e ne6.
    SUBROUTINE malloc_ab06(a,nb1,ne1,nb2,ne2,nb3,ne3,nb4,ne4,nb5,ne5,nb6,ne6)
      INTEGER(SIK),INTENT(IN) :: nb1,ne1,nb2,ne2,nb3,ne3,nb4,ne4,nb5,ne5
      INTEGER(SIK),INTENT(IN) :: nb6,ne6
      LOGICAL(SBK),ALLOCATABLE,INTENT(INOUT) :: a(:,:,:,:,:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ALLOCATED(a)) THEN
        IF(nb1 <= ne1 .AND. nb2 <= ne2 .AND. nb3 <= ne3 .AND. nb4 <= ne4 &
                                       .AND. nb5 <= ne5 .AND. nb6 <= ne6) THEN
          ALLOCATE(a(nb1:ne1,nb2:ne2,nb3:ne3,nb4:ne4,nb5:ne5, &
                     nb6:ne6),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=.FALSE.
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SBK,SRK)
          ELSE
            bsize=REAL(SBK,SRK)
            mysize=(REAL(ne1,SRK)-REAL(nb1,SRK)+1_SRK)+ &
                   (REAL(ne2,SRK)-REAL(nb2,SRK)+1_SRK)+ &
                   (REAL(ne3,SRK)-REAL(nb3,SRK)+1_SRK)+ &
                   (REAL(ne4,SRK)-REAL(nb4,SRK)+1_SRK)+ &
                   (REAL(ne5,SRK)-REAL(nb5,SRK)+1_SRK)+ &
                   (REAL(ne6,SRK)-REAL(nb6,SRK)+1_SRK)*bsize
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_ab06
!
!-------------------------------------------------------------------------------
! Allocate seven dimensional pointers
!
!> @brief Allocates rank 7 SINGLE precision allocatable array.
!> @param a dummy argument for allocatable array to be allocated
!> @param nb1 beginning index of first dimension
!> @param ne1 ending index of first dimension
!> @param nb2 beginning index of second dimension
!> @param ne2 ending index of second dimension
!> @param nb3 beginning index of third dimension
!> @param ne3 ending index of third dimension
!> @param nb4 beginning index of fourth dimension
!> @param ne4 ending index of fourth dimension
!> @param nb5 beginning index of fifth dimension
!> @param ne5 ending index of fifth dimension
!> @param nb6 beginning index of sixth dimension
!> @param ne6 ending index of sixth dimension
!> @param nb7 beginning index of seventh dimension
!> @param ne7 ending index of seventh dimension
!>
!> Routine for allocating a rank 7 allocatable array of type SINGLE precision from
!> \e nb1 to \e ne1, \e nb2 to \e ne2, \e nb3 to \e ne3, \e nb4 to \e ne4, 
!> \e nb5 to \e ne5, \e nb6 to \e ne6, and \e nb7 to \e ne7.
    SUBROUTINE malloc_as07(a,nb1,ne1,nb2,ne2,nb3,ne3,nb4,ne4,nb5,ne5, &
                           nb6,ne6,nb7,ne7)
      INTEGER(SIK),INTENT(IN) :: nb1,ne1,nb2,ne2,nb3,ne3,nb4,ne4,nb5,ne5
      INTEGER(SIK),INTENT(IN) :: nb6,ne6,nb7,ne7
      REAL(SSK),ALLOCATABLE,INTENT(INOUT) :: a(:,:,:,:,:,:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ALLOCATED(a)) THEN
        IF(nb1 <= ne1 .AND. nb2 <= ne2 .AND. nb3 <= ne3 .AND. nb4 <= ne4 &
                      .AND. nb5 <= ne5 .AND. nb6 <= ne6 .AND. nb7 <= ne7) THEN
          ALLOCATE(a(nb1:ne1,nb2:ne2,nb3:ne3,nb4:ne4,nb5:ne5, &
                     nb6:ne6,nb7:ne7),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=0.0_SSK
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SSK,SRK)
          ELSE
            bsize=REAL(SSK,SRK)
            mysize=(REAL(ne1,SRK)-REAL(nb1,SRK)+1_SRK)+ &
                   (REAL(ne2,SRK)-REAL(nb2,SRK)+1_SRK)+ &
                   (REAL(ne3,SRK)-REAL(nb3,SRK)+1_SRK)+ &
                   (REAL(ne4,SRK)-REAL(nb4,SRK)+1_SRK)+ &
                   (REAL(ne5,SRK)-REAL(nb5,SRK)+1_SRK)+ &
                   (REAL(ne6,SRK)-REAL(nb6,SRK)+1_SRK)+ &
                   (REAL(ne7,SRK)-REAL(nb7,SRK)+1_SRK)*bsize
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_as07
!
!----------------------------------------
!> @brief Allocates rank 7 DOUBLE precision allocatable array.
!> @param a dummy argument for allocatable array to be allocated
!> @param nb1 beginning index of first dimension
!> @param ne1 ending index of first dimension
!> @param nb2 beginning index of second dimension
!> @param ne2 ending index of second dimension
!> @param nb3 beginning index of third dimension
!> @param ne3 ending index of third dimension
!> @param nb4 beginning index of fourth dimension
!> @param ne4 ending index of fourth dimension
!> @param nb5 beginning index of fifth dimension
!> @param ne5 ending index of fifth dimension
!> @param nb6 beginning index of sixth dimension
!> @param ne6 ending index of sixth dimension
!> @param nb7 beginning index of seventh dimension
!> @param ne7 ending index of seventh dimension
!>
!> Routine for allocating a rank 7 allocatable array of type DOUBLE precision from
!> \e nb1 to \e ne1, \e nb2 to \e ne2, \e nb3 to \e ne3, \e nb4 to \e ne4, 
!> \e nb5 to \e ne5, \e nb6 to \e ne6, and \e nb7 to \e ne7.
    SUBROUTINE malloc_ad07(a,nb1,ne1,nb2,ne2,nb3,ne3,nb4,ne4,nb5,ne5, &
                           nb6,ne6,nb7,ne7)
      INTEGER(SIK),INTENT(IN) :: nb1,ne1,nb2,ne2,nb3,ne3,nb4,ne4,nb5,ne5
      INTEGER(SIK),INTENT(IN) :: nb6,ne6,nb7,ne7
      REAL(SDK),ALLOCATABLE,INTENT(INOUT) :: a(:,:,:,:,:,:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ALLOCATED(a)) THEN
        IF(nb1 <= ne1 .AND. nb2 <= ne2 .AND. nb3 <= ne3 .AND. nb4 <= ne4 &
                      .AND. nb5 <= ne5 .AND. nb6 <= ne6 .AND. nb7 <= ne7) THEN
          ALLOCATE(a(nb1:ne1,nb2:ne2,nb3:ne3,nb4:ne4,nb5:ne5, &
                     nb6:ne6,nb7:ne7),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=0.0_SDK
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SDK,SRK)
          ELSE
            bsize=REAL(SDK,SRK)
            mysize=(REAL(ne1,SRK)-REAL(nb1,SRK)+1_SRK)+ &
                   (REAL(ne2,SRK)-REAL(nb2,SRK)+1_SRK)+ &
                   (REAL(ne3,SRK)-REAL(nb3,SRK)+1_SRK)+ &
                   (REAL(ne4,SRK)-REAL(nb4,SRK)+1_SRK)+ &
                   (REAL(ne5,SRK)-REAL(nb5,SRK)+1_SRK)+ &
                   (REAL(ne6,SRK)-REAL(nb6,SRK)+1_SRK)+ &
                   (REAL(ne7,SRK)-REAL(nb7,SRK)+1_SRK)*bsize
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_ad07
!
!----------------------------------------
!> @brief Allocates rank 7 INTEGER allocatable array.
!> @param a dummy argument for allocatable array to be allocated
!> @param nb1 beginning index of first dimension
!> @param ne1 ending index of first dimension
!> @param nb2 beginning index of second dimension
!> @param ne2 ending index of second dimension
!> @param nb3 beginning index of third dimension
!> @param ne3 ending index of third dimension
!> @param nb4 beginning index of fourth dimension
!> @param ne4 ending index of fourth dimension
!> @param nb5 beginning index of fifth dimension
!> @param ne5 ending index of fifth dimension
!> @param nb6 beginning index of sixth dimension
!> @param ne6 ending index of sixth dimension
!> @param nb7 beginning index of seventh dimension
!> @param ne7 ending index of seventh dimension
!>
!> Routine for allocating a rank 7 allocatable array of type INTEGER from
!> \e nb1 to \e ne1, \e nb2 to \e ne2, \e nb3 to \e ne3, \e nb4 to \e ne4, 
!> \e nb5 to \e ne5, \e nb6 to \e ne6, and \e nb7 to \e ne7.
    SUBROUTINE malloc_ai07(a,nb1,ne1,nb2,ne2,nb3,ne3,nb4,ne4,nb5,ne5, &
                           nb6,ne6,nb7,ne7)
      INTEGER(SIK),INTENT(IN) :: nb1,ne1,nb2,ne2,nb3,ne3,nb4,ne4,nb5,ne5
      INTEGER(SIK),INTENT(IN) :: nb6,ne6,nb7,ne7
      INTEGER(SNK),ALLOCATABLE,INTENT(INOUT) :: a(:,:,:,:,:,:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ALLOCATED(a)) THEN
        IF(nb1 <= ne1 .AND. nb2 <= ne2 .AND. nb3 <= ne3 .AND. nb4 <= ne4 &
                      .AND. nb5 <= ne5 .AND. nb6 <= ne6 .AND. nb7 <= ne7) THEN
          ALLOCATE(a(nb1:ne1,nb2:ne2,nb3:ne3,nb4:ne4,nb5:ne5, &
                     nb6:ne6,nb7:ne7),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=0_SNK
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SNK,SRK)
          ELSE
            bsize=REAL(SNK,SRK)
            mysize=(REAL(ne1,SRK)-REAL(nb1,SRK)+1_SRK)+ &
                   (REAL(ne2,SRK)-REAL(nb2,SRK)+1_SRK)+ &
                   (REAL(ne3,SRK)-REAL(nb3,SRK)+1_SRK)+ &
                   (REAL(ne4,SRK)-REAL(nb4,SRK)+1_SRK)+ &
                   (REAL(ne5,SRK)-REAL(nb5,SRK)+1_SRK)+ &
                   (REAL(ne6,SRK)-REAL(nb6,SRK)+1_SRK)+ &
                   (REAL(ne7,SRK)-REAL(nb7,SRK)+1_SRK)*bsize
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_ai07
!
!----------------------------------------
!> @brief Allocates rank 7 LONG integer allocatable array.
!> @param a dummy argument for allocatable array to be allocated
!> @param nb1 beginning index of first dimension
!> @param ne1 ending index of first dimension
!> @param nb2 beginning index of second dimension
!> @param ne2 ending index of second dimension
!> @param nb3 beginning index of third dimension
!> @param ne3 ending index of third dimension
!> @param nb4 beginning index of fourth dimension
!> @param ne4 ending index of fourth dimension
!> @param nb5 beginning index of fifth dimension
!> @param ne5 ending index of fifth dimension
!> @param nb6 beginning index of sixth dimension
!> @param ne6 ending index of sixth dimension
!> @param nb7 beginning index of seventh dimension
!> @param ne7 ending index of seventh dimension
!>
!> Routine for allocating a rank 7 allocatable array of type LONG integer from
!> \e nb1 to \e ne1, \e nb2 to \e ne2, \e nb3 to \e ne3, \e nb4 to \e ne4, 
!> \e nb5 to \e ne5, \e nb6 to \e ne6, and \e nb7 to \e ne7.
    SUBROUTINE malloc_al07(a,nb1,ne1,nb2,ne2,nb3,ne3,nb4,ne4,nb5,ne5, &
                           nb6,ne6,nb7,ne7)
      INTEGER(SIK),INTENT(IN) :: nb1,ne1,nb2,ne2,nb3,ne3,nb4,ne4,nb5,ne5
      INTEGER(SIK),INTENT(IN) :: nb6,ne6,nb7,ne7
      INTEGER(SLK),ALLOCATABLE,INTENT(INOUT) :: a(:,:,:,:,:,:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ALLOCATED(a)) THEN
        IF(nb1 <= ne1 .AND. nb2 <= ne2 .AND. nb3 <= ne3 .AND. nb4 <= ne4 &
                      .AND. nb5 <= ne5 .AND. nb6 <= ne6 .AND. nb7 <= ne7) THEN
          ALLOCATE(a(nb1:ne1,nb2:ne2,nb3:ne3,nb4:ne4,nb5:ne5, &
                     nb6:ne6,nb7:ne7),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=0_SLK
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SLK,SRK)
          ELSE
            bsize=REAL(SLK,SRK)
            mysize=(REAL(ne1,SRK)-REAL(nb1,SRK)+1_SRK)+ &
                   (REAL(ne2,SRK)-REAL(nb2,SRK)+1_SRK)+ &
                   (REAL(ne3,SRK)-REAL(nb3,SRK)+1_SRK)+ &
                   (REAL(ne4,SRK)-REAL(nb4,SRK)+1_SRK)+ &
                   (REAL(ne5,SRK)-REAL(nb5,SRK)+1_SRK)+ &
                   (REAL(ne6,SRK)-REAL(nb6,SRK)+1_SRK)+ &
                   (REAL(ne7,SRK)-REAL(nb7,SRK)+1_SRK)*bsize
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_al07
!
!----------------------------------------
!> @brief Allocates rank 7 LOGICAL allocatable array.
!> @param a dummy argument for allocatable array to be allocated
!> @param nb1 beginning index of first dimension
!> @param ne1 ending index of first dimension
!> @param nb2 beginning index of second dimension
!> @param ne2 ending index of second dimension
!> @param nb3 beginning index of third dimension
!> @param ne3 ending index of third dimension
!> @param nb4 beginning index of fourth dimension
!> @param ne4 ending index of fourth dimension
!> @param nb5 beginning index of fifth dimension
!> @param ne5 ending index of fifth dimension
!> @param nb6 beginning index of sixth dimension
!> @param ne6 ending index of sixth dimension
!> @param nb7 beginning index of seventh dimension
!> @param ne7 ending index of seventh dimension
!>
!> Routine for allocating a rank 7 allocatable array of type LOGICAL from
!> \e nb1 to \e ne1, \e nb2 to \e ne2, \e nb3 to \e ne3, \e nb4 to \e ne4, 
!> \e nb5 to \e ne5, \e nb6 to \e ne6, and \e nb7 to \e ne7.
    SUBROUTINE malloc_ab07(a,nb1,ne1,nb2,ne2,nb3,ne3,nb4,ne4,nb5,ne5, &
                           nb6,ne6,nb7,ne7)
      INTEGER(SIK),INTENT(IN) :: nb1,ne1,nb2,ne2,nb3,ne3,nb4,ne4,nb5,ne5
      INTEGER(SIK),INTENT(IN) :: nb6,ne6,nb7,ne7
      LOGICAL(SBK),ALLOCATABLE,INTENT(INOUT) :: a(:,:,:,:,:,:,:)
      INTEGER(SIK) :: alloc_err
      REAL(SRK) :: mysize,bsize

      IF(.NOT.ALLOCATED(a)) THEN
        IF(nb1 <= ne1 .AND. nb2 <= ne2 .AND. nb3 <= ne3 .AND. nb4 <= ne4 &
                      .AND. nb5 <= ne5 .AND. nb6 <= ne6 .AND. nb7 <= ne7) THEN
          ALLOCATE(a(nb1:ne1,nb2:ne2,nb3:ne3,nb4:ne4,nb5:ne5, &
                     nb6:ne6,nb7:ne7),STAT=alloc_err)
          IF(alloc_err == 0_SIK) THEN
            a=.FALSE.
            Alloc_nbytes=Alloc_nbytes+REAL(SIZE(a)*SBK,SRK)
          ELSE
            bsize=REAL(SBK,SRK)
            mysize=(REAL(ne1,SRK)-REAL(nb1,SRK)+1_SRK)+ &
                   (REAL(ne2,SRK)-REAL(nb2,SRK)+1_SRK)+ &
                   (REAL(ne3,SRK)-REAL(nb3,SRK)+1_SRK)+ &
                   (REAL(ne4,SRK)-REAL(nb4,SRK)+1_SRK)+ &
                   (REAL(ne5,SRK)-REAL(nb5,SRK)+1_SRK)+ &
                   (REAL(ne6,SRK)-REAL(nb6,SRK)+1_SRK)+ &
                   (REAL(ne7,SRK)-REAL(nb7,SRK)+1_SRK)*bsize
            CALL AllocsError(alloc_err,mysize)
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE malloc_ab07
!
!===============================================================================
! The following section contains all procedures for deallocating pointers from 1
! to n.
!
!> @brief Deallocates rank 1 SINGLE precision pointer array.
!> @param a dummy argument for pointer array to be deallocated
!>
!> Routine for deallocating a rank 1 pointer array of type SINGLE precision.
!> Only useful for modifying Alloc_nbytes.
    SUBROUTINE demalloc_ps1(a)
      REAL(SSK),POINTER :: a(:)
      INTEGER(SIK) :: alloc_err,mysize

      IF(ASSOCIATED(a)) THEN
        mysize=SIZE(a)
        DEALLOCATE(a,STAT=alloc_err)
        IF(alloc_err == 0_SIK) THEN
          Alloc_nbytes=Alloc_nbytes-mysize*SSK
        ELSE
          CALL AllocsError(alloc_err,REAL(-mysize,SRK))
        ENDIF
      ENDIF
    ENDSUBROUTINE demalloc_ps1
!
!----------------------------------------
!> @brief Deallocates rank 1 DOUBLE precision pointer array.
!> @param a dummy argument for pointer array to be deallocated
!>
!> Routine for deallocating a rank 1 pointer array of type DOUBLE precision.
!> Only useful for modifying Alloc_nbytes.
    SUBROUTINE demalloc_pd1(a)
      REAL(SDK),POINTER :: a(:)
      INTEGER(SIK) :: alloc_err,mysize

      IF(ASSOCIATED(a)) THEN
        mysize=SIZE(a)
        DEALLOCATE(a,STAT=alloc_err)
        IF(alloc_err == 0_SIK) THEN
          Alloc_nbytes=Alloc_nbytes-mysize*SDK
        ELSE
          CALL AllocsError(alloc_err,REAL(-mysize,SRK))
        ENDIF
      ENDIF
    ENDSUBROUTINE demalloc_pd1
!
!----------------------------------------
!> @brief Deallocates rank 1 INTEGER pointer array.
!> @param a dummy argument for pointer array to be deallocated
!>
!> Routine for deallocating a rank 1 pointer array of type INTEGER.
!> Only useful for modifying Alloc_nbytes.
    SUBROUTINE demalloc_pi1(a)
      INTEGER(SNK),POINTER :: a(:)
      INTEGER(SIK) :: alloc_err,mysize

      IF(ASSOCIATED(a)) THEN
        mysize=SIZE(a)
        DEALLOCATE(a,STAT=alloc_err)
        IF(alloc_err == 0_SIK) THEN
          Alloc_nbytes=Alloc_nbytes-mysize*SNK
        ELSE
          CALL AllocsError(alloc_err,REAL(-mysize,SRK))
        ENDIF
      ENDIF
    ENDSUBROUTINE demalloc_pi1
!
!----------------------------------------
!> @brief Deallocates rank 1 LONG integer pointer array.
!> @param a dummy argument for pointer array to be deallocated
!>
!> Routine for deallocating a rank 1 pointer array of type LONG integer.
!> Only useful for modifying Alloc_nbytes.
    SUBROUTINE demalloc_pl1(a)
      INTEGER(SLK),POINTER :: a(:)
      INTEGER(SIK) :: alloc_err,mysize

      IF(ASSOCIATED(a)) THEN
        mysize=SIZE(a)
        DEALLOCATE(a,STAT=alloc_err)
        IF(alloc_err == 0_SIK) THEN
          Alloc_nbytes=Alloc_nbytes-mysize*SLK
        ELSE
          CALL AllocsError(alloc_err,REAL(-mysize,SRK))
        ENDIF
      ENDIF
    ENDSUBROUTINE demalloc_pl1
!
!----------------------------------------
!> @brief Deallocates rank 1 LOGICAL pointer array.
!> @param a dummy argument for pointer array to be deallocated
!>
!> Routine for deallocating a rank 1 pointer array of type LOGICAL.
!> Only useful for modifying Alloc_nbytes.
    SUBROUTINE demalloc_pb1(a)
      LOGICAL(SBK),POINTER :: a(:)
      INTEGER(SIK) :: alloc_err,mysize

      IF(ASSOCIATED(a)) THEN
        mysize=SIZE(a)
        DEALLOCATE(a,STAT=alloc_err)
        IF(alloc_err == 0_SIK) THEN
          Alloc_nbytes=Alloc_nbytes-mysize*SBK
        ELSE
          CALL AllocsError(alloc_err,REAL(-mysize,SRK))
        ENDIF
      ENDIF
    ENDSUBROUTINE demalloc_pb1
!
!-------------------------------------------------------------------------------
! Allocate two dimensional pointers
!
!> @brief Deallocates rank 2 SINGLE precision pointer array.
!> @param a dummy argument for pointer array to be deallocated
!>
!> Routine for deallocating a rank 2 pointer array of type SINGLE precision.
!> Only useful for modifying Alloc_nbytes.
    SUBROUTINE demalloc_ps2(a)
      REAL(SSK),POINTER :: a(:,:)
      INTEGER(SIK) :: alloc_err,mysize

      IF(ASSOCIATED(a)) THEN
        mysize=SIZE(a)
        DEALLOCATE(a,STAT=alloc_err)
        IF(alloc_err == 0_SIK) THEN
          Alloc_nbytes=Alloc_nbytes-mysize*SSK
        ELSE
          CALL AllocsError(alloc_err,REAL(-mysize,SRK))
        ENDIF
      ENDIF
    ENDSUBROUTINE demalloc_ps2
!
!----------------------------------------
!> @brief Deallocates rank 2 DOUBLE precision pointer array.
!> @param a dummy argument for pointer array to be deallocated
!>
!> Routine for deallocating a rank 2 pointer array of type DOUBLE precision.
!> Only useful for modifying Alloc_nbytes.
    SUBROUTINE demalloc_pd2(a)
      REAL(SDK),POINTER :: a(:,:)
      INTEGER(SIK) :: alloc_err,mysize

      IF(ASSOCIATED(a)) THEN
        mysize=SIZE(a)
        DEALLOCATE(a,STAT=alloc_err)
        IF(alloc_err == 0_SIK) THEN
          Alloc_nbytes=Alloc_nbytes-mysize*SDK
        ELSE
          CALL AllocsError(alloc_err,REAL(-mysize,SRK))
        ENDIF
      ENDIF
    ENDSUBROUTINE demalloc_pd2
!
!----------------------------------------
!> @brief Deallocates rank 2 INTEGER pointer array.
!> @param a dummy argument for pointer array to be deallocated
!>
!> Routine for deallocating a rank 2 pointer array of type INTEGER.
!> Only useful for modifying Alloc_nbytes.
    SUBROUTINE demalloc_pi2(a)
      INTEGER(SNK),POINTER :: a(:,:)
      INTEGER(SIK) :: alloc_err,mysize

      IF(ASSOCIATED(a)) THEN
        mysize=SIZE(a)
        DEALLOCATE(a,STAT=alloc_err)
        IF(alloc_err == 0_SIK) THEN
          Alloc_nbytes=Alloc_nbytes-mysize*SNK
        ELSE
          CALL AllocsError(alloc_err,REAL(-mysize,SRK))
        ENDIF
      ENDIF
    ENDSUBROUTINE demalloc_pi2
!
!----------------------------------------
!> @brief Deallocates rank 2 LONG integer pointer array.
!> @param a dummy argument for pointer array to be deallocated
!>
!> Routine for deallocating a rank 2 pointer array of type LONG integer.
!> Only useful for modifying Alloc_nbytes.
    SUBROUTINE demalloc_pl2(a)
      INTEGER(SLK),POINTER :: a(:,:)
      INTEGER(SIK) :: alloc_err,mysize

      IF(ASSOCIATED(a)) THEN
        mysize=SIZE(a)
        DEALLOCATE(a,STAT=alloc_err)
        IF(alloc_err == 0_SIK) THEN
          Alloc_nbytes=Alloc_nbytes-mysize*SLK
        ELSE
          CALL AllocsError(alloc_err,REAL(-mysize,SRK))
        ENDIF
      ENDIF
    ENDSUBROUTINE demalloc_pl2
!
!----------------------------------------
!> @brief Deallocates rank 2 LOGICAL pointer array.
!> @param a dummy argument for pointer array to be deallocated
!>
!> Routine for deallocating a rank 2 pointer array of type LOGICAL.
!> Only useful for modifying Alloc_nbytes.
    SUBROUTINE demalloc_pb2(a)
      LOGICAL(SBK),POINTER :: a(:,:)
      INTEGER(SIK) :: alloc_err,mysize

      IF(ASSOCIATED(a)) THEN
        mysize=SIZE(a)
        DEALLOCATE(a,STAT=alloc_err)
        IF(alloc_err == 0_SIK) THEN
          Alloc_nbytes=Alloc_nbytes-mysize*SBK
        ELSE
          CALL AllocsError(alloc_err,REAL(-mysize,SRK))
        ENDIF
      ENDIF
    ENDSUBROUTINE demalloc_pb2
!
!-------------------------------------------------------------------------------
! Allocate three dimensional pointers
!
!> @brief Deallocates rank 3 SINGLE precision pointer array.
!> @param a dummy argument for pointer array to be deallocated
!>
!> Routine for deallocating a rank 3 pointer array of type SINGLE precision.
!> Only useful for modifying Alloc_nbytes.
    SUBROUTINE demalloc_ps3(a)
      REAL(SSK),POINTER :: a(:,:,:)
      INTEGER(SIK) :: alloc_err,mysize

      IF(ASSOCIATED(a)) THEN
        mysize=SIZE(a)
        DEALLOCATE(a,STAT=alloc_err)
        IF(alloc_err == 0_SIK) THEN
          Alloc_nbytes=Alloc_nbytes-mysize*SSK
        ELSE
          CALL AllocsError(alloc_err,REAL(-mysize,SRK))
        ENDIF
      ENDIF
    ENDSUBROUTINE demalloc_ps3
!
!----------------------------------------
!> @brief Deallocates rank 3 DOUBLE precision pointer array.
!> @param a dummy argument for pointer array to be deallocated
!>
!> Routine for deallocating a rank 3 pointer array of type DOUBLE precision.
!> Only useful for modifying Alloc_nbytes.
    SUBROUTINE demalloc_pd3(a)
      REAL(SDK),POINTER :: a(:,:,:)
      INTEGER(SIK) :: alloc_err,mysize

      IF(ASSOCIATED(a)) THEN
        mysize=SIZE(a)
        DEALLOCATE(a,STAT=alloc_err)
        IF(alloc_err == 0_SIK) THEN
          Alloc_nbytes=Alloc_nbytes-mysize*SDK
        ELSE
          CALL AllocsError(alloc_err,REAL(-mysize,SRK))
        ENDIF
      ENDIF
    ENDSUBROUTINE demalloc_pd3
!
!----------------------------------------
!> @brief Deallocates rank 3 INTEGER pointer array.
!> @param a dummy argument for pointer array to be deallocated
!>
!> Routine for deallocating a rank 3 pointer array of type INTEGER.
!> Only useful for modifying Alloc_nbytes.
    SUBROUTINE demalloc_pi3(a)
      INTEGER(SNK),POINTER :: a(:,:,:)
      INTEGER(SIK) :: alloc_err,mysize

      IF(ASSOCIATED(a)) THEN
        mysize=SIZE(a)
        DEALLOCATE(a,STAT=alloc_err)
        IF(alloc_err == 0_SIK) THEN
          Alloc_nbytes=Alloc_nbytes-mysize*SNK
        ELSE
          CALL AllocsError(alloc_err,REAL(-mysize,SRK))
        ENDIF
      ENDIF
    ENDSUBROUTINE demalloc_pi3
!
!----------------------------------------
!> @brief Deallocates rank 3 LONG integer pointer array.
!> @param a dummy argument for pointer array to be deallocated
!>
!> Routine for deallocating a rank 3 pointer array of type LONG integer.
!> Only useful for modifying Alloc_nbytes.
    SUBROUTINE demalloc_pl3(a)
      INTEGER(SLK),POINTER :: a(:,:,:)
      INTEGER(SIK) :: alloc_err,mysize

      IF(ASSOCIATED(a)) THEN
        mysize=SIZE(a)
        DEALLOCATE(a,STAT=alloc_err)
        IF(alloc_err == 0_SIK) THEN
          Alloc_nbytes=Alloc_nbytes-mysize*SLK
        ELSE
          CALL AllocsError(alloc_err,REAL(-mysize,SRK))
        ENDIF
      ENDIF
    ENDSUBROUTINE demalloc_pl3
!
!----------------------------------------
!> @brief Deallocates rank 3 LOGICAL pointer array.
!> @param a dummy argument for pointer array to be deallocated
!>
!> Routine for deallocating a rank 3 pointer array of type LOGICAL.
!> Only useful for modifying Alloc_nbytes.
    SUBROUTINE demalloc_pb3(a)
      LOGICAL(SBK),POINTER :: a(:,:,:)
      INTEGER(SIK) :: alloc_err,mysize

      IF(ASSOCIATED(a)) THEN
        mysize=SIZE(a)
        DEALLOCATE(a,STAT=alloc_err)
        IF(alloc_err == 0_SIK) THEN
          Alloc_nbytes=Alloc_nbytes-mysize*SBK
        ELSE
          CALL AllocsError(alloc_err,REAL(-mysize,SRK))
        ENDIF
      ENDIF
    ENDSUBROUTINE demalloc_pb3
!
!-------------------------------------------------------------------------------
! Allocate four dimensional pointers
!
!> @brief Deallocates rank 4 SINGLE precision pointer array.
!> @param a dummy argument for pointer array to be deallocated
!>
!> Routine for deallocating a rank 4 pointer array of type SINGLE precision.
!> Only useful for modifying Alloc_nbytes.
    SUBROUTINE demalloc_ps4(a)
      REAL(SSK),POINTER :: a(:,:,:,:)
      INTEGER(SIK) :: alloc_err,mysize

      IF(ASSOCIATED(a)) THEN
        mysize=SIZE(a)
        DEALLOCATE(a,STAT=alloc_err)
        IF(alloc_err == 0_SIK) THEN
          Alloc_nbytes=Alloc_nbytes-mysize*SSK
        ELSE
          CALL AllocsError(alloc_err,REAL(-mysize,SRK))
        ENDIF
      ENDIF
    ENDSUBROUTINE demalloc_ps4
!
!----------------------------------------
!> @brief Deallocates rank 4 DOUBLE precision pointer array.
!> @param a dummy argument for pointer array to be deallocated
!>
!> Routine for deallocating a rank 4 pointer array of type DOUBLE precision.
!> Only useful for modifying Alloc_nbytes.
    SUBROUTINE demalloc_pd4(a)
      REAL(SDK),POINTER :: a(:,:,:,:)
      INTEGER(SIK) :: alloc_err,mysize

      IF(ASSOCIATED(a)) THEN
        mysize=SIZE(a)
        DEALLOCATE(a,STAT=alloc_err)
        IF(alloc_err == 0_SIK) THEN
          Alloc_nbytes=Alloc_nbytes-mysize*SDK
        ELSE
          CALL AllocsError(alloc_err,REAL(-mysize,SRK))
        ENDIF
      ENDIF
    ENDSUBROUTINE demalloc_pd4
!
!----------------------------------------
!> @brief Deallocates rank 4 INTEGER pointer array.
!> @param a dummy argument for pointer array to be deallocated
!>
!> Routine for deallocating a rank 4 pointer array of type INTEGER.
!> Only useful for modifying Alloc_nbytes.
    SUBROUTINE demalloc_pi4(a)
      INTEGER(SNK),POINTER :: a(:,:,:,:)
      INTEGER(SIK) :: alloc_err,mysize

      IF(ASSOCIATED(a)) THEN
        mysize=SIZE(a)
        DEALLOCATE(a,STAT=alloc_err)
        IF(alloc_err == 0_SIK) THEN
          Alloc_nbytes=Alloc_nbytes-mysize*SNK
        ELSE
          CALL AllocsError(alloc_err,REAL(-mysize,SRK))
        ENDIF
      ENDIF
    ENDSUBROUTINE demalloc_pi4
!
!----------------------------------------
!> @brief Deallocates rank 4 LONG integer pointer array.
!> @param a dummy argument for pointer array to be deallocated
!>
!> Routine for deallocating a rank 4 pointer array of type LONG integer.
!> Only useful for modifying Alloc_nbytes.
    SUBROUTINE demalloc_pl4(a)
      INTEGER(SLK),POINTER :: a(:,:,:,:)
      INTEGER(SIK) :: alloc_err,mysize

      IF(ASSOCIATED(a)) THEN
        mysize=SIZE(a)
        DEALLOCATE(a,STAT=alloc_err)
        IF(alloc_err == 0_SIK) THEN
          Alloc_nbytes=Alloc_nbytes-mysize*SLK
        ELSE
          CALL AllocsError(alloc_err,REAL(-mysize,SRK))
        ENDIF
      ENDIF
    ENDSUBROUTINE demalloc_pl4
!
!----------------------------------------
!> @brief Deallocates rank 4 LOGICAL pointer array.
!> @param a dummy argument for pointer array to be deallocated
!>
!> Routine for deallocating a rank 4 pointer array of type LOGICAL.
!> Only useful for modifying Alloc_nbytes.
    SUBROUTINE demalloc_pb4(a)
      LOGICAL(SBK),POINTER :: a(:,:,:,:)
      INTEGER(SIK) :: alloc_err,mysize

      IF(ASSOCIATED(a)) THEN
        mysize=SIZE(a)
        DEALLOCATE(a,STAT=alloc_err)
        IF(alloc_err == 0_SIK) THEN
          Alloc_nbytes=Alloc_nbytes-mysize*SBK
        ELSE
          CALL AllocsError(alloc_err,REAL(-mysize,SRK))
        ENDIF
      ENDIF
    ENDSUBROUTINE demalloc_pb4
!
!-------------------------------------------------------------------------------
! Allocate five dimensional pointers
!
!> @brief Deallocates rank 5 SINGLE precision pointer array.
!> @param a dummy argument for pointer array to be deallocated
!>
!> Routine for deallocating a rank 5 pointer array of type SINGLE precision.
!> Only useful for modifying Alloc_nbytes.
    SUBROUTINE demalloc_ps5(a)
      REAL(SSK),POINTER :: a(:,:,:,:,:)
      INTEGER(SIK) :: alloc_err,mysize

      IF(ASSOCIATED(a)) THEN
        mysize=SIZE(a)
        DEALLOCATE(a,STAT=alloc_err)
        IF(alloc_err == 0_SIK) THEN
          Alloc_nbytes=Alloc_nbytes-mysize*SSK
        ELSE
          CALL AllocsError(alloc_err,REAL(-mysize,SRK))
        ENDIF
      ENDIF
    ENDSUBROUTINE demalloc_ps5
!
!----------------------------------------
!> @brief Deallocates rank 5 DOUBLE precision pointer array.
!> @param a dummy argument for pointer array to be deallocated
!>
!> Routine for deallocating a rank 5 pointer array of type DOUBLE precision.
!> Only useful for modifying Alloc_nbytes.
    SUBROUTINE demalloc_pd5(a)
      REAL(SDK),POINTER :: a(:,:,:,:,:)
      INTEGER(SIK) :: alloc_err,mysize

      IF(ASSOCIATED(a)) THEN
        mysize=SIZE(a)
        DEALLOCATE(a,STAT=alloc_err)
        IF(alloc_err == 0_SIK) THEN
          Alloc_nbytes=Alloc_nbytes-mysize*SDK
        ELSE
          CALL AllocsError(alloc_err,REAL(-mysize,SRK))
        ENDIF
      ENDIF
    ENDSUBROUTINE demalloc_pd5
!
!----------------------------------------
!> @brief Deallocates rank 5 INTEGER pointer array.
!> @param a dummy argument for pointer array to be deallocated
!>
!> Routine for deallocating a rank 5 pointer array of type INTEGER.
!> Only useful for modifying Alloc_nbytes.
    SUBROUTINE demalloc_pi5(a)
      INTEGER(SNK),POINTER :: a(:,:,:,:,:)
      INTEGER(SIK) :: alloc_err,mysize

      IF(ASSOCIATED(a)) THEN
        mysize=SIZE(a)
        DEALLOCATE(a,STAT=alloc_err)
        IF(alloc_err == 0_SIK) THEN
          Alloc_nbytes=Alloc_nbytes-mysize*SNK
        ELSE
          CALL AllocsError(alloc_err,REAL(-mysize,SRK))
        ENDIF
      ENDIF
    ENDSUBROUTINE demalloc_pi5
!
!----------------------------------------
!> @brief Deallocates rank 5 LONG integer pointer array.
!> @param a dummy argument for pointer array to be deallocated
!>
!> Routine for deallocating a rank 5 pointer array of type LONG integer.
!> Only useful for modifying Alloc_nbytes.
    SUBROUTINE demalloc_pl5(a)
      INTEGER(SLK),POINTER :: a(:,:,:,:,:)
      INTEGER(SIK) :: alloc_err,mysize

      IF(ASSOCIATED(a)) THEN
        mysize=SIZE(a)
        DEALLOCATE(a,STAT=alloc_err)
        IF(alloc_err == 0_SIK) THEN
          Alloc_nbytes=Alloc_nbytes-mysize*SLK
        ELSE
          CALL AllocsError(alloc_err,REAL(-mysize,SRK))
        ENDIF
      ENDIF
    ENDSUBROUTINE demalloc_pl5
!
!----------------------------------------
!> @brief Deallocates rank 5 LOGICAL pointer array.
!> @param a dummy argument for pointer array to be deallocated
!>
!> Routine for deallocating a rank 5 pointer array of type LOGICAL.
!> Only useful for modifying Alloc_nbytes.
    SUBROUTINE demalloc_pb5(a)
      LOGICAL(SBK),POINTER :: a(:,:,:,:,:)
      INTEGER(SIK) :: alloc_err,mysize

      IF(ASSOCIATED(a)) THEN
        mysize=SIZE(a)
        DEALLOCATE(a,STAT=alloc_err)
        IF(alloc_err == 0_SIK) THEN
          Alloc_nbytes=Alloc_nbytes-mysize*SBK
        ELSE
          CALL AllocsError(alloc_err,REAL(-mysize,SRK))
        ENDIF
      ENDIF
    ENDSUBROUTINE demalloc_pb5
!
!-------------------------------------------------------------------------------
! Allocate six dimensional pointers
!
!> @brief Deallocates rank 6 SINGLE precision pointer array.
!> @param a dummy argument for pointer array to be deallocated
!>
!> Routine for deallocating a rank 6 pointer array of type SINGLE precision.
!> Only useful for modifying Alloc_nbytes.
    SUBROUTINE demalloc_ps6(a)
      REAL(SSK),POINTER :: a(:,:,:,:,:,:)
      INTEGER(SIK) :: alloc_err,mysize

      IF(ASSOCIATED(a)) THEN
        mysize=SIZE(a)
        DEALLOCATE(a,STAT=alloc_err)
        IF(alloc_err == 0_SIK) THEN
          Alloc_nbytes=Alloc_nbytes-mysize*SSK
        ELSE
          CALL AllocsError(alloc_err,REAL(-mysize,SRK))
        ENDIF
      ENDIF
    ENDSUBROUTINE demalloc_ps6
!
!----------------------------------------
!> @brief Deallocates rank 6 DOUBLE precision pointer array.
!> @param a dummy argument for pointer array to be deallocated
!>
!> Routine for deallocating a rank 6 pointer array of type DOUBLE precision.
!> Only useful for modifying Alloc_nbytes.
    SUBROUTINE demalloc_pd6(a)
      REAL(SDK),POINTER :: a(:,:,:,:,:,:)
      INTEGER(SIK) :: alloc_err,mysize

      IF(ASSOCIATED(a)) THEN
        mysize=SIZE(a)
        DEALLOCATE(a,STAT=alloc_err)
        IF(alloc_err == 0_SIK) THEN
          Alloc_nbytes=Alloc_nbytes-mysize*SDK
        ELSE
          CALL AllocsError(alloc_err,REAL(-mysize,SRK))
        ENDIF
      ENDIF
    ENDSUBROUTINE demalloc_pd6
!
!----------------------------------------
!> @brief Deallocates rank 6 INTEGER pointer array.
!> @param a dummy argument for pointer array to be deallocated
!>
!> Routine for deallocating a rank 6 pointer array of type INTEGER.
!> Only useful for modifying Alloc_nbytes.
    SUBROUTINE demalloc_pi6(a)
      INTEGER(SNK),POINTER :: a(:,:,:,:,:,:)
      INTEGER(SIK) :: alloc_err,mysize

      IF(ASSOCIATED(a)) THEN
        mysize=SIZE(a)
        DEALLOCATE(a,STAT=alloc_err)
        IF(alloc_err == 0_SIK) THEN
          Alloc_nbytes=Alloc_nbytes-mysize*SNK
        ELSE
          CALL AllocsError(alloc_err,REAL(-mysize,SRK))
        ENDIF
      ENDIF
    ENDSUBROUTINE demalloc_pi6
!
!----------------------------------------
!> @brief Deallocates rank 6 LONG integer pointer array.
!> @param a dummy argument for pointer array to be deallocated
!>
!> Routine for deallocating a rank 6 pointer array of type LONG integer.
!> Only useful for modifying Alloc_nbytes.
    SUBROUTINE demalloc_pl6(a)
      INTEGER(SLK),POINTER :: a(:,:,:,:,:,:)
      INTEGER(SIK) :: alloc_err,mysize

      IF(ASSOCIATED(a)) THEN
        mysize=SIZE(a)
        DEALLOCATE(a,STAT=alloc_err)
        IF(alloc_err == 0_SIK) THEN
          Alloc_nbytes=Alloc_nbytes-mysize*SLK
        ELSE
          CALL AllocsError(alloc_err,REAL(-mysize,SRK))
        ENDIF
      ENDIF
    ENDSUBROUTINE demalloc_pl6
!
!----------------------------------------
!> @brief Deallocates rank 6 LOGICAL pointer array.
!> @param a dummy argument for pointer array to be deallocated
!>
!> Routine for deallocating a rank 6 pointer array of type LOGICAL.
!> Only useful for modifying Alloc_nbytes.
    SUBROUTINE demalloc_pb6(a)
      LOGICAL(SBK),POINTER :: a(:,:,:,:,:,:)
      INTEGER(SIK) :: alloc_err,mysize

      IF(ASSOCIATED(a)) THEN
        mysize=SIZE(a)
        DEALLOCATE(a,STAT=alloc_err)
        IF(alloc_err == 0_SIK) THEN
          Alloc_nbytes=Alloc_nbytes-mysize*SBK
        ELSE
          CALL AllocsError(alloc_err,REAL(-mysize,SRK))
        ENDIF
      ENDIF
    ENDSUBROUTINE demalloc_pb6
!
!-------------------------------------------------------------------------------
! Allocate seven dimensional pointers
!
!> @brief Deallocates rank 7 SINGLE precision pointer array.
!> @param a dummy argument for pointer array to be deallocated
!>
!> Routine for deallocating a rank 7 pointer array of type SINGLE precision.
!> Only useful for modifying Alloc_nbytes.
    SUBROUTINE demalloc_ps7(a)
      REAL(SSK),POINTER :: a(:,:,:,:,:,:,:)
      INTEGER(SIK) :: alloc_err,mysize

      IF(ASSOCIATED(a)) THEN
        mysize=SIZE(a)
        DEALLOCATE(a,STAT=alloc_err)
        IF(alloc_err == 0_SIK) THEN
          Alloc_nbytes=Alloc_nbytes-mysize*SSK
        ELSE
          CALL AllocsError(alloc_err,REAL(-mysize,SRK))
        ENDIF
      ENDIF
    ENDSUBROUTINE demalloc_ps7
!
!----------------------------------------
!> @brief Deallocates rank 7 DOUBLE precision pointer array.
!> @param a dummy argument for pointer array to be deallocated
!>
!> Routine for deallocating a rank 7 pointer array of type DOUBLE precision.
!> Only useful for modifying Alloc_nbytes.
    SUBROUTINE demalloc_pd7(a)
      REAL(SDK),POINTER :: a(:,:,:,:,:,:,:)
      INTEGER(SIK) :: alloc_err,mysize

      IF(ASSOCIATED(a)) THEN
        mysize=SIZE(a)
        DEALLOCATE(a,STAT=alloc_err)
        IF(alloc_err == 0_SIK) THEN
          Alloc_nbytes=Alloc_nbytes-mysize*SDK
        ELSE
          CALL AllocsError(alloc_err,REAL(-mysize,SRK))
        ENDIF
      ENDIF
    ENDSUBROUTINE demalloc_pd7
!
!----------------------------------------
!> @brief Deallocates rank 7 INTEGER pointer array.
!> @param a dummy argument for pointer array to be deallocated
!>
!> Routine for deallocating a rank 7 pointer array of type INTEGER.
!> Only useful for modifying Alloc_nbytes.
    SUBROUTINE demalloc_pi7(a)
      INTEGER(SNK),POINTER :: a(:,:,:,:,:,:,:)
      INTEGER(SIK) :: alloc_err,mysize

      IF(ASSOCIATED(a)) THEN
        mysize=SIZE(a)
        DEALLOCATE(a,STAT=alloc_err)
        IF(alloc_err == 0_SIK) THEN
          Alloc_nbytes=Alloc_nbytes-mysize*SNK
        ELSE
          CALL AllocsError(alloc_err,REAL(-mysize,SRK))
        ENDIF
      ENDIF
    ENDSUBROUTINE demalloc_pi7
!
!----------------------------------------
!> @brief Deallocates rank 7 LONG integer pointer array.
!> @param a dummy argument for pointer array to be deallocated
!>
!> Routine for deallocating a rank 7 pointer array of type LONG integer.
!> Only useful for modifying Alloc_nbytes.
    SUBROUTINE demalloc_pl7(a)
      INTEGER(SLK),POINTER :: a(:,:,:,:,:,:,:)
      INTEGER(SIK) :: alloc_err,mysize

      IF(ASSOCIATED(a)) THEN
        mysize=SIZE(a)
        DEALLOCATE(a,STAT=alloc_err)
        IF(alloc_err == 0_SIK) THEN
          Alloc_nbytes=Alloc_nbytes-mysize*SLK
        ELSE
          CALL AllocsError(alloc_err,REAL(-mysize,SRK))
        ENDIF
      ENDIF
    ENDSUBROUTINE demalloc_pl7
!
!----------------------------------------
!> @brief Deallocates rank 7 LOGICAL pointer array.
!> @param a dummy argument for pointer array to be deallocated
!>
!> Routine for deallocating a rank 7 pointer array of type LOGICAL.
!> Only useful for modifying Alloc_nbytes.
    SUBROUTINE demalloc_pb7(a)
      LOGICAL(SBK),POINTER :: a(:,:,:,:,:,:,:)
      INTEGER(SIK) :: alloc_err,mysize

      IF(ASSOCIATED(a)) THEN
        mysize=SIZE(a)
        DEALLOCATE(a,STAT=alloc_err)
        IF(alloc_err == 0_SIK) THEN
          Alloc_nbytes=Alloc_nbytes-mysize*SBK
        ELSE
          CALL AllocsError(alloc_err,REAL(-mysize,SRK))
        ENDIF
      ENDIF
    ENDSUBROUTINE demalloc_pb7
!
!===============================================================================
! The following section contains all procedures for deallocating arrays from 1
! to n.
!
!> @brief Deallocates rank 1 SINGLE precision allocatable array.
!> @param a dummy argument for pointer array to be deallocated
!>
!> Routine for deallocating a rank 1 allocatable array of type SINGLE precision.
!> Only useful for modifying Alloc_nbytes.
    SUBROUTINE demalloc_as1(a)
      REAL(SSK),ALLOCATABLE,INTENT(INOUT) :: a(:)
      INTEGER(SIK) :: alloc_err,mysize

      IF(ALLOCATED(a)) THEN
        mysize=SIZE(a)
        DEALLOCATE(a,STAT=alloc_err)
        IF(alloc_err == 0_SIK) THEN
          Alloc_nbytes=Alloc_nbytes-mysize*SSK
        ELSE
          CALL AllocsError(alloc_err,REAL(-mysize,SRK))
        ENDIF
      ENDIF
    ENDSUBROUTINE demalloc_as1
!
!----------------------------------------
!> @brief Deallocates rank 1 DOUBLE precision allocatable array.
!> @param a dummy argument for allocatable array to be deallocated
!>
!> Routine for deallocating a rank 1 allocatable array of type DOUBLE precision.
!> Only useful for modifying Alloc_nbytes.
    SUBROUTINE demalloc_ad1(a)
      REAL(SDK),ALLOCATABLE,INTENT(INOUT) :: a(:)
      INTEGER(SIK) :: alloc_err,mysize

      IF(ALLOCATED(a)) THEN
        mysize=SIZE(a)
        DEALLOCATE(a,STAT=alloc_err)
        IF(alloc_err == 0_SIK) THEN
          Alloc_nbytes=Alloc_nbytes-mysize*SDK
        ELSE
          CALL AllocsError(alloc_err,REAL(-mysize,SRK))
        ENDIF
      ENDIF
    ENDSUBROUTINE demalloc_ad1
!
!----------------------------------------
!> @brief Deallocates rank 1 INTEGER allocatable array.
!> @param a dummy argument for allocatable array to be deallocated
!>
!> Routine for deallocating a rank 1 allocatable array of type INTEGER.
!> Only useful for modifying Alloc_nbytes.
    SUBROUTINE demalloc_ai1(a)
      INTEGER(SNK),ALLOCATABLE,INTENT(INOUT) :: a(:)
      INTEGER(SIK) :: alloc_err,mysize

      IF(ALLOCATED(a)) THEN
        mysize=SIZE(a)
        DEALLOCATE(a,STAT=alloc_err)
        IF(alloc_err == 0_SIK) THEN
          Alloc_nbytes=Alloc_nbytes-mysize*SNK
        ELSE
          CALL AllocsError(alloc_err,REAL(-mysize,SRK))
        ENDIF
      ENDIF
    ENDSUBROUTINE demalloc_ai1
!
!----------------------------------------
!> @brief Deallocates rank 1 LONG integer allocatable array.
!> @param a dummy argument for allocatable array to be deallocated
!>
!> Routine for deallocating a rank 1 allocatable array of type LONG integer.
!> Only useful for modifying Alloc_nbytes.
    SUBROUTINE demalloc_al1(a)
      INTEGER(SLK),ALLOCATABLE,INTENT(INOUT) :: a(:)
      INTEGER(SIK) :: alloc_err,mysize

      IF(ALLOCATED(a)) THEN
        mysize=SIZE(a)
        DEALLOCATE(a,STAT=alloc_err)
        IF(alloc_err == 0_SIK) THEN
          Alloc_nbytes=Alloc_nbytes-mysize*SLK
        ELSE
          CALL AllocsError(alloc_err,REAL(-mysize,SRK))
        ENDIF
      ENDIF
    ENDSUBROUTINE demalloc_al1
!
!----------------------------------------
!> @brief Deallocates rank 1 LOGICAL allocatable array.
!> @param a dummy argument for allocatable array to be deallocated
!>
!> Routine for deallocating a rank 1 allocatable array of type LOGICAL.
!> Only useful for modifying Alloc_nbytes.
    SUBROUTINE demalloc_ab1(a)
      LOGICAL(SBK),ALLOCATABLE,INTENT(INOUT) :: a(:)
      INTEGER(SIK) :: alloc_err,mysize

      IF(ALLOCATED(a)) THEN
        mysize=SIZE(a)
        DEALLOCATE(a,STAT=alloc_err)
        IF(alloc_err == 0_SIK) THEN
          Alloc_nbytes=Alloc_nbytes-mysize*SBK
        ELSE
          CALL AllocsError(alloc_err,REAL(-mysize,SRK))
        ENDIF
      ENDIF
    ENDSUBROUTINE demalloc_ab1
!
!-------------------------------------------------------------------------------
! Allocate two dimensional pointers
!
!> @brief Deallocates rank 2 SINGLE precision allocatable array.
!> @param a dummy argument for allocatable array to be deallocated
!>
!> Routine for deallocating a rank 2 allocatable array of type SINGLE precision.
!> Only useful for modifying Alloc_nbytes.
    SUBROUTINE demalloc_as2(a)
      REAL(SSK),ALLOCATABLE,INTENT(INOUT) :: a(:,:)
      INTEGER(SIK) :: alloc_err,mysize

      IF(ALLOCATED(a)) THEN
        mysize=SIZE(a)
        DEALLOCATE(a,STAT=alloc_err)
        IF(alloc_err == 0_SIK) THEN
          Alloc_nbytes=Alloc_nbytes-mysize*SSK
        ELSE
          CALL AllocsError(alloc_err,REAL(-mysize,SRK))
        ENDIF
      ENDIF
    ENDSUBROUTINE demalloc_as2
!
!----------------------------------------
!> @brief Deallocates rank 2 DOUBLE precision allocatable array.
!> @param a dummy argument for allocatable array to be deallocated
!>
!> Routine for deallocating a rank 2 allocatable array of type DOUBLE precision.
!> Only useful for modifying Alloc_nbytes.
    SUBROUTINE demalloc_ad2(a)
      REAL(SDK),ALLOCATABLE,INTENT(INOUT) :: a(:,:)
      INTEGER(SIK) :: alloc_err,mysize

      IF(ALLOCATED(a)) THEN
        mysize=SIZE(a)
        DEALLOCATE(a,STAT=alloc_err)
        IF(alloc_err == 0_SIK) THEN
          Alloc_nbytes=Alloc_nbytes-mysize*SDK
        ELSE
          CALL AllocsError(alloc_err,REAL(-mysize,SRK))
        ENDIF
      ENDIF
    ENDSUBROUTINE demalloc_ad2
!
!----------------------------------------
!> @brief Deallocates rank 2 INTEGER allocatable array.
!> @param a dummy argument for allocatable array to be deallocated
!>
!> Routine for deallocating a rank 2 allocatable array of type INTEGER.
!> Only useful for modifying Alloc_nbytes.
    SUBROUTINE demalloc_ai2(a)
      INTEGER(SNK),ALLOCATABLE,INTENT(INOUT) :: a(:,:)
      INTEGER(SIK) :: alloc_err,mysize

      IF(ALLOCATED(a)) THEN
        mysize=SIZE(a)
        DEALLOCATE(a,STAT=alloc_err)
        IF(alloc_err == 0_SIK) THEN
          Alloc_nbytes=Alloc_nbytes-mysize*SNK
        ELSE
          CALL AllocsError(alloc_err,REAL(-mysize,SRK))
        ENDIF
      ENDIF
    ENDSUBROUTINE demalloc_ai2
!
!----------------------------------------
!> @brief Deallocates rank 2 LONG integer allocatable array.
!> @param a dummy argument for allocatable array to be deallocated
!>
!> Routine for deallocating a rank 2 allocatable array of type LONG integer.
!> Only useful for modifying Alloc_nbytes.
    SUBROUTINE demalloc_al2(a)
      INTEGER(SLK),ALLOCATABLE,INTENT(INOUT) :: a(:,:)
      INTEGER(SIK) :: alloc_err,mysize

      IF(ALLOCATED(a)) THEN
        mysize=SIZE(a)
        DEALLOCATE(a,STAT=alloc_err)
        IF(alloc_err == 0_SIK) THEN
          Alloc_nbytes=Alloc_nbytes-mysize*SLK
        ELSE
          CALL AllocsError(alloc_err,REAL(-mysize,SRK))
        ENDIF
      ENDIF
    ENDSUBROUTINE demalloc_al2
!
!----------------------------------------
!> @brief Deallocates rank 2 LOGICAL allocatable array.
!> @param a dummy argument for allocatable array to be deallocated
!>
!> Routine for deallocating a rank 2 allocatable array of type LOGICAL.
!> Only useful for modifying Alloc_nbytes.
    SUBROUTINE demalloc_ab2(a)
      LOGICAL(SBK),ALLOCATABLE,INTENT(INOUT) :: a(:,:)
      INTEGER(SIK) :: alloc_err,mysize

      IF(ALLOCATED(a)) THEN
        mysize=SIZE(a)
        DEALLOCATE(a,STAT=alloc_err)
        IF(alloc_err == 0_SIK) THEN
          Alloc_nbytes=Alloc_nbytes-mysize*SBK
        ELSE
          CALL AllocsError(alloc_err,REAL(-mysize,SRK))
        ENDIF
      ENDIF
    ENDSUBROUTINE demalloc_ab2
!
!-------------------------------------------------------------------------------
! Allocate three dimensional pointers
!
!> @brief Deallocates rank 3 SINGLE precision allocatable array.
!> @param a dummy argument for allocatable array to be deallocated
!>
!> Routine for deallocating a rank 3 allocatable array of type SINGLE precision.
!> Only useful for modifying Alloc_nbytes.
    SUBROUTINE demalloc_as3(a)
      REAL(SSK),ALLOCATABLE,INTENT(INOUT) :: a(:,:,:)
      INTEGER(SIK) :: alloc_err,mysize

      IF(ALLOCATED(a)) THEN
        mysize=SIZE(a)
        DEALLOCATE(a,STAT=alloc_err)
        IF(alloc_err == 0_SIK) THEN
          Alloc_nbytes=Alloc_nbytes-mysize*SSK
        ELSE
          CALL AllocsError(alloc_err,REAL(-mysize,SRK))
        ENDIF
      ENDIF
    ENDSUBROUTINE demalloc_as3
!
!----------------------------------------
!> @brief Deallocates rank 3 DOUBLE precision allocatable array.
!> @param a dummy argument for allocatable array to be deallocated
!>
!> Routine for deallocating a rank 3 allocatable array of type DOUBLE precision.
!> Only useful for modifying Alloc_nbytes.
    SUBROUTINE demalloc_ad3(a)
      REAL(SDK),ALLOCATABLE,INTENT(INOUT) :: a(:,:,:)
      INTEGER(SIK) :: alloc_err,mysize

      IF(ALLOCATED(a)) THEN
        mysize=SIZE(a)
        DEALLOCATE(a,STAT=alloc_err)
        IF(alloc_err == 0_SIK) THEN
          Alloc_nbytes=Alloc_nbytes-mysize*SDK
        ELSE
          CALL AllocsError(alloc_err,REAL(-mysize,SRK))
        ENDIF
      ENDIF
    ENDSUBROUTINE demalloc_ad3
!
!----------------------------------------
!> @brief Deallocates rank 3 INTEGER allocatable array.
!> @param a dummy argument for allocatable array to be deallocated
!>
!> Routine for deallocating a rank 3 allocatable array of type INTEGER.
!> Only useful for modifying Alloc_nbytes.
    SUBROUTINE demalloc_ai3(a)
      INTEGER(SNK),ALLOCATABLE,INTENT(INOUT) :: a(:,:,:)
      INTEGER(SIK) :: alloc_err,mysize

      IF(ALLOCATED(a)) THEN
        mysize=SIZE(a)
        DEALLOCATE(a,STAT=alloc_err)
        IF(alloc_err == 0_SIK) THEN
          Alloc_nbytes=Alloc_nbytes-mysize*SNK
        ELSE
          CALL AllocsError(alloc_err,REAL(-mysize,SRK))
        ENDIF
      ENDIF
    ENDSUBROUTINE demalloc_ai3
!
!----------------------------------------
!> @brief Deallocates rank 3 LONG integer allocatable array.
!> @param a dummy argument for allocatable array to be deallocated
!>
!> Routine for deallocating a rank 3 allocatable array of type LONG integer.
!> Only useful for modifying Alloc_nbytes.
    SUBROUTINE demalloc_al3(a)
      INTEGER(SLK),ALLOCATABLE,INTENT(INOUT) :: a(:,:,:)
      INTEGER(SIK) :: alloc_err,mysize

      IF(ALLOCATED(a)) THEN
        mysize=SIZE(a)
        DEALLOCATE(a,STAT=alloc_err)
        IF(alloc_err == 0_SIK) THEN
          Alloc_nbytes=Alloc_nbytes-mysize*SLK
        ELSE
          CALL AllocsError(alloc_err,REAL(-mysize,SRK))
        ENDIF
      ENDIF
    ENDSUBROUTINE demalloc_al3
!
!----------------------------------------
!> @brief Deallocates rank 3 LOGICAL allocatable array.
!> @param a dummy argument for allocatable array to be deallocated
!>
!> Routine for deallocating a rank 3 allocatable array of type LOGICAL.
!> Only useful for modifying Alloc_nbytes.
    SUBROUTINE demalloc_ab3(a)
      LOGICAL(SBK),ALLOCATABLE,INTENT(INOUT) :: a(:,:,:)
      INTEGER(SIK) :: alloc_err,mysize

      IF(ALLOCATED(a)) THEN
        mysize=SIZE(a)
        DEALLOCATE(a,STAT=alloc_err)
        IF(alloc_err == 0_SIK) THEN
          Alloc_nbytes=Alloc_nbytes-mysize*SBK
        ELSE
          CALL AllocsError(alloc_err,REAL(-mysize,SRK))
        ENDIF
      ENDIF
    ENDSUBROUTINE demalloc_ab3
!
!-------------------------------------------------------------------------------
! Allocate four dimensional pointers
!
!> @brief Deallocates rank 4 SINGLE precision allocatable array.
!> @param a dummy argument for allocatable array to be deallocated
!>
!> Routine for deallocating a rank 4 allocatable array of type SINGLE precision.
!> Only useful for modifying Alloc_nbytes.
    SUBROUTINE demalloc_as4(a)
      REAL(SSK),ALLOCATABLE,INTENT(INOUT) :: a(:,:,:,:)
      INTEGER(SIK) :: alloc_err,mysize

      IF(ALLOCATED(a)) THEN
        mysize=SIZE(a)
        DEALLOCATE(a,STAT=alloc_err)
        IF(alloc_err == 0_SIK) THEN
          Alloc_nbytes=Alloc_nbytes-mysize*SSK
        ELSE
          CALL AllocsError(alloc_err,REAL(-mysize,SRK))
        ENDIF
      ENDIF
    ENDSUBROUTINE demalloc_as4
!
!----------------------------------------
!> @brief Deallocates rank 4 DOUBLE precision allocatable array.
!> @param a dummy argument for allocatable array to be deallocated
!>
!> Routine for deallocating a rank 4 allocatable array of type DOUBLE precision.
!> Only useful for modifying Alloc_nbytes.
    SUBROUTINE demalloc_ad4(a)
      REAL(SDK),ALLOCATABLE,INTENT(INOUT) :: a(:,:,:,:)
      INTEGER(SIK) :: alloc_err,mysize

      IF(ALLOCATED(a)) THEN
        mysize=SIZE(a)
        DEALLOCATE(a,STAT=alloc_err)
        IF(alloc_err == 0_SIK) THEN
          Alloc_nbytes=Alloc_nbytes-mysize*SDK
        ELSE
          CALL AllocsError(alloc_err,REAL(-mysize,SRK))
        ENDIF
      ENDIF
    ENDSUBROUTINE demalloc_ad4
!
!----------------------------------------
!> @brief Deallocates rank 4 INTEGER allocatable array.
!> @param a dummy argument for allocatable array to be deallocated
!>
!> Routine for deallocating a rank 4 allocatable array of type INTEGER.
!> Only useful for modifying Alloc_nbytes.
    SUBROUTINE demalloc_ai4(a)
      INTEGER(SNK),ALLOCATABLE,INTENT(INOUT) :: a(:,:,:,:)
      INTEGER(SIK) :: alloc_err,mysize

      IF(ALLOCATED(a)) THEN
        mysize=SIZE(a)
        DEALLOCATE(a,STAT=alloc_err)
        IF(alloc_err == 0_SIK) THEN
          Alloc_nbytes=Alloc_nbytes-mysize*SNK
        ELSE
          CALL AllocsError(alloc_err,REAL(-mysize,SRK))
        ENDIF
      ENDIF
    ENDSUBROUTINE demalloc_ai4
!
!----------------------------------------
!> @brief Deallocates rank 4 LONG integer allocatable array.
!> @param a dummy argument for allocatable array to be deallocated
!>
!> Routine for deallocating a rank 4 allocatable array of type LONG integer.
!> Only useful for modifying Alloc_nbytes.
    SUBROUTINE demalloc_al4(a)
      INTEGER(SLK),ALLOCATABLE,INTENT(INOUT) :: a(:,:,:,:)
      INTEGER(SIK) :: alloc_err,mysize

      IF(ALLOCATED(a)) THEN
        mysize=SIZE(a)
        DEALLOCATE(a,STAT=alloc_err)
        IF(alloc_err == 0_SIK) THEN
          Alloc_nbytes=Alloc_nbytes-mysize*SLK
        ELSE
          CALL AllocsError(alloc_err,REAL(-mysize,SRK))
        ENDIF
      ENDIF
    ENDSUBROUTINE demalloc_al4
!
!----------------------------------------
!> @brief Deallocates rank 4 LOGICAL allocatable array.
!> @param a dummy argument for allocatable array to be deallocated
!>
!> Routine for deallocating a rank 4 allocatable array of type LOGICAL.
!> Only useful for modifying Alloc_nbytes.
    SUBROUTINE demalloc_ab4(a)
      LOGICAL(SBK),ALLOCATABLE,INTENT(INOUT) :: a(:,:,:,:)
      INTEGER(SIK) :: alloc_err,mysize

      IF(ALLOCATED(a)) THEN
        mysize=SIZE(a)
        DEALLOCATE(a,STAT=alloc_err)
        IF(alloc_err == 0_SIK) THEN
          Alloc_nbytes=Alloc_nbytes-mysize*SBK
        ELSE
          CALL AllocsError(alloc_err,REAL(-mysize,SRK))
        ENDIF
      ENDIF
    ENDSUBROUTINE demalloc_ab4
!
!-------------------------------------------------------------------------------
! Allocate five dimensional pointers
!
!> @brief Deallocates rank 5 SINGLE precision allocatable array.
!> @param a dummy argument for allocatable array to be deallocated
!>
!> Routine for deallocating a rank 5 allocatable array of type SINGLE precision.
!> Only useful for modifying Alloc_nbytes.
    SUBROUTINE demalloc_as5(a)
      REAL(SSK),ALLOCATABLE,INTENT(INOUT) :: a(:,:,:,:,:)
      INTEGER(SIK) :: alloc_err,mysize

      IF(ALLOCATED(a)) THEN
        mysize=SIZE(a)
        DEALLOCATE(a,STAT=alloc_err)
        IF(alloc_err == 0_SIK) THEN
          Alloc_nbytes=Alloc_nbytes-mysize*SSK
        ELSE
          CALL AllocsError(alloc_err,REAL(-mysize,SRK))
        ENDIF
      ENDIF
    ENDSUBROUTINE demalloc_as5
!
!----------------------------------------
!> @brief Deallocates rank 5 DOUBLE precision allocatable array.
!> @param a dummy argument for allocatable array to be deallocated
!>
!> Routine for deallocating a rank 5 allocatable array of type DOUBLE precision.
!> Only useful for modifying Alloc_nbytes.
    SUBROUTINE demalloc_ad5(a)
      REAL(SDK),ALLOCATABLE,INTENT(INOUT) :: a(:,:,:,:,:)
      INTEGER(SIK) :: alloc_err,mysize

      IF(ALLOCATED(a)) THEN
        mysize=SIZE(a)
        DEALLOCATE(a,STAT=alloc_err)
        IF(alloc_err == 0_SIK) THEN
          Alloc_nbytes=Alloc_nbytes-mysize*SDK
        ELSE
          CALL AllocsError(alloc_err,REAL(-mysize,SRK))
        ENDIF
      ENDIF
    ENDSUBROUTINE demalloc_ad5
!
!----------------------------------------
!> @brief Deallocates rank 5 INTEGER allocatable array.
!> @param a dummy argument for allocatable array to be deallocated
!>
!> Routine for deallocating a rank 5 allocatable array of type INTEGER.
!> Only useful for modifying Alloc_nbytes.
    SUBROUTINE demalloc_ai5(a)
      INTEGER(SNK),ALLOCATABLE,INTENT(INOUT) :: a(:,:,:,:,:)
      INTEGER(SIK) :: alloc_err,mysize

      IF(ALLOCATED(a)) THEN
        mysize=SIZE(a)
        DEALLOCATE(a,STAT=alloc_err)
        IF(alloc_err == 0_SIK) THEN
          Alloc_nbytes=Alloc_nbytes-mysize*SNK
        ELSE
          CALL AllocsError(alloc_err,REAL(-mysize,SRK))
        ENDIF
      ENDIF
    ENDSUBROUTINE demalloc_ai5
!
!----------------------------------------
!> @brief Deallocates rank 5 LONG integer allocatable array.
!> @param a dummy argument for allocatable array to be deallocated
!>
!> Routine for deallocating a rank 5 allocatable array of type LONG integer.
!> Only useful for modifying Alloc_nbytes.
    SUBROUTINE demalloc_al5(a)
      INTEGER(SLK),ALLOCATABLE,INTENT(INOUT) :: a(:,:,:,:,:)
      INTEGER(SIK) :: alloc_err,mysize

      IF(ALLOCATED(a)) THEN
        mysize=SIZE(a)
        DEALLOCATE(a,STAT=alloc_err)
        IF(alloc_err == 0_SIK) THEN
          Alloc_nbytes=Alloc_nbytes-mysize*SLK
        ELSE
          CALL AllocsError(alloc_err,REAL(-mysize,SRK))
        ENDIF
      ENDIF
    ENDSUBROUTINE demalloc_al5
!
!----------------------------------------
!> @brief Deallocates rank 5 LOGICAL allocatable array.
!> @param a dummy argument for allocatable array to be deallocated
!>
!> Routine for deallocating a rank 5 allocatable array of type LOGICAL.
!> Only useful for modifying Alloc_nbytes.
    SUBROUTINE demalloc_ab5(a)
      LOGICAL(SBK),ALLOCATABLE,INTENT(INOUT) :: a(:,:,:,:,:)
      INTEGER(SIK) :: alloc_err,mysize

      IF(ALLOCATED(a)) THEN
        mysize=SIZE(a)
        DEALLOCATE(a,STAT=alloc_err)
        IF(alloc_err == 0_SIK) THEN
          Alloc_nbytes=Alloc_nbytes-mysize*SBK
        ELSE
          CALL AllocsError(alloc_err,REAL(-mysize,SRK))
        ENDIF
      ENDIF
    ENDSUBROUTINE demalloc_ab5
!
!-------------------------------------------------------------------------------
! Allocate six dimensional pointers
!
!> @brief Deallocates rank 6 SINGLE precision allocatable array.
!> @param a dummy argument for allocatable array to be deallocated
!>
!> Routine for deallocating a rank 6 allocatable array of type SINGLE precision.
!> Only useful for modifying Alloc_nbytes.
    SUBROUTINE demalloc_as6(a)
      REAL(SSK),ALLOCATABLE,INTENT(INOUT) :: a(:,:,:,:,:,:)
      INTEGER(SIK) :: alloc_err,mysize

      IF(ALLOCATED(a)) THEN
        mysize=SIZE(a)
        DEALLOCATE(a,STAT=alloc_err)
        IF(alloc_err == 0_SIK) THEN
          Alloc_nbytes=Alloc_nbytes-mysize*SSK
        ELSE
          CALL AllocsError(alloc_err,REAL(-mysize,SRK))
        ENDIF
      ENDIF
    ENDSUBROUTINE demalloc_as6
!
!----------------------------------------
!> @brief Deallocates rank 6 DOUBLE precision allocatable array.
!> @param a dummy argument for allocatable array to be deallocated
!>
!> Routine for deallocating a rank 6 allocatable array of type DOUBLE precision.
!> Only useful for modifying Alloc_nbytes.
    SUBROUTINE demalloc_ad6(a)
      REAL(SDK),ALLOCATABLE,INTENT(INOUT) :: a(:,:,:,:,:,:)
      INTEGER(SIK) :: alloc_err,mysize

      IF(ALLOCATED(a)) THEN
        mysize=SIZE(a)
        DEALLOCATE(a,STAT=alloc_err)
        IF(alloc_err == 0_SIK) THEN
          Alloc_nbytes=Alloc_nbytes-mysize*SDK
        ELSE
          CALL AllocsError(alloc_err,REAL(-mysize,SRK))
        ENDIF
      ENDIF
    ENDSUBROUTINE demalloc_ad6
!
!----------------------------------------
!> @brief Deallocates rank 6 INTEGER allocatable array.
!> @param a dummy argument for allocatable array to be deallocated
!>
!> Routine for deallocating a rank 6 allocatable array of type INTEGER.
!> Only useful for modifying Alloc_nbytes.
    SUBROUTINE demalloc_ai6(a)
      INTEGER(SNK),ALLOCATABLE,INTENT(INOUT) :: a(:,:,:,:,:,:)
      INTEGER(SIK) :: alloc_err,mysize

      IF(ALLOCATED(a)) THEN
        mysize=SIZE(a)
        DEALLOCATE(a,STAT=alloc_err)
        IF(alloc_err == 0_SIK) THEN
          Alloc_nbytes=Alloc_nbytes-mysize*SNK
        ELSE
          CALL AllocsError(alloc_err,REAL(-mysize,SRK))
        ENDIF
      ENDIF
    ENDSUBROUTINE demalloc_ai6
!
!----------------------------------------
!> @brief Deallocates rank 6 LONG integer allocatable array.
!> @param a dummy argument for allocatable array to be deallocated
!>
!> Routine for deallocating a rank 6 allocatable array of type LONG integer.
!> Only useful for modifying Alloc_nbytes.
    SUBROUTINE demalloc_al6(a)
      INTEGER(SLK),ALLOCATABLE,INTENT(INOUT) :: a(:,:,:,:,:,:)
      INTEGER(SIK) :: alloc_err,mysize

      IF(ALLOCATED(a)) THEN
        mysize=SIZE(a)
        DEALLOCATE(a,STAT=alloc_err)
        IF(alloc_err == 0_SIK) THEN
          Alloc_nbytes=Alloc_nbytes-mysize*SLK
        ELSE
          CALL AllocsError(alloc_err,REAL(-mysize,SRK))
        ENDIF
      ENDIF
    ENDSUBROUTINE demalloc_al6
!
!----------------------------------------
!> @brief Deallocates rank 6 LOGICAL allocatable array.
!> @param a dummy argument for allocatable array to be deallocated
!>
!> Routine for deallocating a rank 6 allocatable array of type LOGICAL.
!> Only useful for modifying Alloc_nbytes.
    SUBROUTINE demalloc_ab6(a)
      LOGICAL(SBK),ALLOCATABLE,INTENT(INOUT) :: a(:,:,:,:,:,:)
      INTEGER(SIK) :: alloc_err,mysize

      IF(ALLOCATED(a)) THEN
        mysize=SIZE(a)
        DEALLOCATE(a,STAT=alloc_err)
        IF(alloc_err == 0_SIK) THEN
          Alloc_nbytes=Alloc_nbytes-mysize*SBK
        ELSE
          CALL AllocsError(alloc_err,REAL(-mysize,SRK))
        ENDIF
      ENDIF
    ENDSUBROUTINE demalloc_ab6
!
!-------------------------------------------------------------------------------
! Allocate seven dimensional pointers
!
!> @brief Deallocates rank 7 SINGLE precision allocatable array.
!> @param a dummy argument for allocatable array to be deallocated
!>
!> Routine for deallocating a rank 7 allocatable array of type SINGLE precision.
!> Only useful for modifying Alloc_nbytes.
    SUBROUTINE demalloc_as7(a)
      REAL(SSK),ALLOCATABLE,INTENT(INOUT) :: a(:,:,:,:,:,:,:)
      INTEGER(SIK) :: alloc_err,mysize

      IF(ALLOCATED(a)) THEN
        mysize=SIZE(a)
        DEALLOCATE(a,STAT=alloc_err)
        IF(alloc_err == 0_SIK) THEN
          Alloc_nbytes=Alloc_nbytes-mysize*SSK
        ELSE
          CALL AllocsError(alloc_err,REAL(-mysize,SRK))
        ENDIF
      ENDIF
    ENDSUBROUTINE demalloc_as7
!
!----------------------------------------
!> @brief Deallocates rank 7 DOUBLE precision allocatable array.
!> @param a dummy argument for allocatable array to be deallocated
!>
!> Routine for deallocating a rank 7 allocatable array of type DOUBLE precision.
!> Only useful for modifying Alloc_nbytes.
    SUBROUTINE demalloc_ad7(a)
      REAL(SDK),ALLOCATABLE,INTENT(INOUT) :: a(:,:,:,:,:,:,:)
      INTEGER(SIK) :: alloc_err,mysize

      IF(ALLOCATED(a)) THEN
        mysize=SIZE(a)
        DEALLOCATE(a,STAT=alloc_err)
        IF(alloc_err == 0_SIK) THEN
          Alloc_nbytes=Alloc_nbytes-mysize*SDK
        ELSE
          CALL AllocsError(alloc_err,REAL(-mysize,SRK))
        ENDIF
      ENDIF
    ENDSUBROUTINE demalloc_ad7
!
!----------------------------------------
!> @brief Deallocates rank 7 INTEGER allocatable array.
!> @param a dummy argument for allocatable array to be deallocated
!>
!> Routine for deallocating a rank 7 allocatable array of type INTEGER.
!> Only useful for modifying Alloc_nbytes.
    SUBROUTINE demalloc_ai7(a)
      INTEGER(SNK),ALLOCATABLE,INTENT(INOUT) :: a(:,:,:,:,:,:,:)
      INTEGER(SIK) :: alloc_err,mysize

      IF(ALLOCATED(a)) THEN
        mysize=SIZE(a)
        DEALLOCATE(a,STAT=alloc_err)
        IF(alloc_err == 0_SIK) THEN
          Alloc_nbytes=Alloc_nbytes-mysize*SNK
        ELSE
          CALL AllocsError(alloc_err,REAL(-mysize,SRK))
        ENDIF
      ENDIF
    ENDSUBROUTINE demalloc_ai7
!
!----------------------------------------
!> @brief Deallocates rank 7 LONG integer allocatable array.
!> @param a dummy argument for allocatable array to be deallocated
!>
!> Routine for deallocating a rank 7 allocatable array of type LONG integer.
!> Only useful for modifying Alloc_nbytes.
    SUBROUTINE demalloc_al7(a)
      INTEGER(SLK),ALLOCATABLE,INTENT(INOUT) :: a(:,:,:,:,:,:,:)
      INTEGER(SIK) :: alloc_err,mysize

      IF(ALLOCATED(a)) THEN
        mysize=SIZE(a)
        DEALLOCATE(a,STAT=alloc_err)
        IF(alloc_err == 0_SIK) THEN
          Alloc_nbytes=Alloc_nbytes-mysize*SLK
        ELSE
          CALL AllocsError(alloc_err,REAL(-mysize,SRK))
        ENDIF
      ENDIF
    ENDSUBROUTINE demalloc_al7
!
!----------------------------------------
!> @brief Deallocates rank 7 LOGICAL allocatable array.
!> @param a dummy argument for allocatable array to be deallocated
!>
!> Routine for deallocating a rank 7 allocatable array of type LOGICAL.
!> Only useful for modifying Alloc_nbytes.
    SUBROUTINE demalloc_ab7(a)
      LOGICAL(SBK),ALLOCATABLE,INTENT(INOUT) :: a(:,:,:,:,:,:,:)
      INTEGER(SIK) :: alloc_err,mysize

      IF(ALLOCATED(a)) THEN
        mysize=SIZE(a)
        DEALLOCATE(a,STAT=alloc_err)
        IF(alloc_err == 0_SIK) THEN
          Alloc_nbytes=Alloc_nbytes-mysize*SBK
        ELSE
          CALL AllocsError(alloc_err,REAL(-mysize,SRK))
        ENDIF
      ENDIF
    ENDSUBROUTINE demalloc_ab7
!> @}
ENDMODULE Allocs
