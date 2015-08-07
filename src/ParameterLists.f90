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
!> @brief This module defines a parameter type object.
!>
!> The purpose of this object is to facilitate encapsulation of an arbitrary
!> number of parameters that may have varying types of values into one object
!> which can be more easily passed around between different code.
!>
!> The object is self-referential which means it can contain objects of the
!> same type as itself. This allows for nested lists of parameters. A parameter
!> is defined by a name, a datatype, an optional description, and a value.
!> The value attribute is polymorphic so that it can be a single integer
!> or an array of double precision reals or a list of other parameters.
!>
!> This module only makes public the base parameter type and it's assignment
!> operation and the exception handler for the module. The base parameter
!> type includes methods for initialization to a specific type, editing the
!> contents of parameter, clearing the contents of a parameter, getting or
!> setting values of existing parameters (or subparameters) and the ability to
!> add or delete parameters nested within other parameters.
!>
!> The parameters are searched and matched using the name attribute and the
!> matching is not case sensitive.
!>
!> The supported extended parameter types accept the following types of values:
!>  - a list of parameters
!>  - scalar logicals
!>  - @ref Strings::StringType "StringTypes"
!>  - scalar 32-bit integers
!>  - scalar 64-bit integers
!>  - scalar single precision reals
!>  - scalar double precision reals
!>  - 1-D arrays of logicals
!>  - 1-D arrays of 32-bit integers
!>  - 1-D arrays of 64-bit integers
!>  - 1-D arrays of single precision reals
!>  - 1-D arrays of double precision reals
!>  - 1-D arrays of "StringTypes"
!>  - 2-D arrays of 32-bit integers
!>  - 2-D arrays of 64-bit integers
!>  - 2-D arrays of single precision reals
!>  - 2-D arrays of double precision reals
!>  - 2-D arrays of "StringTypes"
!>  - 3-D arrays of 32-bit integers
!>  - 3-D arrays of 64-bit integers
!>  - 3-D arrays of single precision reals
!>  - 3-D arrays of double precision reals
!>
!> @par Module Dependencies
!>  - @ref IntrType "IntrType": @copybrief IntrType
!>  - @ref Strings "Strings": @copybrief Strings
!>  - @ref ExceptionHandler "ExceptionHandler": @copybrief ExceptionHandler
!>  - @ref IO_Strings "IO_Strings": @copybrief IO_Strings
!>  - @ref UnitTest "UnitTest": @copybrief UnitTest
!>
!> @par EXAMPLE
!> @code
!> PROGRAM
!>
!>   IMPLICIT NONE
!>
!>
!> END PROGRAM
!> @endcode
!>
!> @author Brendan Kochunas and Dan Jabaay and Benjamin Collins
!>   @date 07/26/2012
!> @par Revisions:
!>   (08/14/2012) - Dan Jabaay
!>   - Expanded functionality to scalar, one, two, and three dimensional arrays
!>     of the parameter types listed above.
!>   (03/07/2013) - Benjamin Collins
!>   - Added has function to return if the parameter list has a given parameter
!>   (10/22/2013) - Dan Jabaay
!>   - Added the %verify subroutine for validating two parameter lists and then
!>     checking that all of the values in the parameter list are equal.
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE ParameterLists
#include "UnitTest.h"
  USE ISO_FORTRAN_ENV
  USE UnitTest
  USE IntrType
  USE Strings
  USE ExceptionHandler
  USE IO_Strings
  USE FileType_XML

  IMPLICIT NONE
  PRIVATE !Default private for module contents
!
! List of Public items
  PUBLIC :: eParams
  PUBLIC :: ParamType
  PUBLIC :: ASSIGNMENT(=)
  PUBLIC :: OPERATOR(==)

  !> The module name
  CHARACTER(LEN=*),PARAMETER :: modName='PARAMETERLISTS'
  INTEGER(SIK),PARAMETER :: MAX_1D_LEN=10

  !> Exception handler for the module
  TYPE(ExceptionHandlerType),SAVE :: eParams

  !> @brief Derived type for a parameter object
  !>
  !> This is an object which encapsulates a polymorphic value which so that the
  !> parameter value can be any number of things e.g.  a real scalar, an integer
  !> array, a logical, a string, etc.
  !>
  !> This type should not have an @c init operation as it logically does not
  !> make any sense. For all extended types the @c edit and @c clear methods
  !> should be overwritten.
  TYPE :: ParamType
    !> @brief The name of the parameter
    !>
    !> Set through input arguments
    TYPE(StringType) :: name
    !> @brief The data type for the parameter
    !>
    !> Set internally.
    TYPE(StringType) :: dataType
    !> An optional description for the parameter
    TYPE(StringType) :: description
    !> @brief The parameter value
    !>
    !> This is only allocated for variables of TYPE(ParamType) for any
    !> extended type it is the additional attribute that should be
    !> accessed.
    CLASS(ParamType),POINTER :: pdat => NULL()
!
!List of type bound procedures
    CONTAINS
      !> @copybrief ParameterLists::init_ParamType_List
      !> @copydoc ParameterLists::init_ParamType_List
      PROCEDURE,PASS,PRIVATE :: initParamList => init_ParamType_List
      !> @copybrief ParameterLists::init_ParamType_SSK
      !> @copydoc ParameterLists::init_ParamType_SSK
      PROCEDURE,PASS,PRIVATE :: initSSK => init_ParamType_SSK
      !> @copybrief ParameterLists::init_ParamType_SDK
      !> @copydoc ParameterLists::init_ParamType_SDK
      PROCEDURE,PASS,PRIVATE :: initSDK => init_ParamType_SDK
      !> @copybrief ParameterLists::init_ParamType_SNK
      !> @copydoc ParameterLists::init_ParamType_SNK
      PROCEDURE,PASS,PRIVATE :: initSNK => init_ParamType_SNK
      !> @copybrief ParameterLists::init_ParamType_SLK
      !> @copydoc ParameterLists::init_ParamType_SLK
      PROCEDURE,PASS,PRIVATE :: initSLK => init_ParamType_SLK
      !> @copybrief ParameterLists::init_ParamType_SBK
      !> @copydoc ParameterLists::init_ParamType_SBK
      PROCEDURE,PASS,PRIVATE :: initSBK => init_ParamType_SBK
      !> @copybrief ParameterLists::init_ParamType_STR
      !> @copydoc ParameterLists::init_ParamType_STR
      PROCEDURE,PASS,PRIVATE :: initSTR => init_ParamType_STR
      !> @copybrief ParameterLists::init_ParamType_CHAR
      !> @copydoc ParameterLists::init_ParamType_CHAR
      PROCEDURE,PASS,PRIVATE :: initCHAR => init_ParamType_CHAR
      !> @copybrief ParameterLists::init_ParamType_SSK_a1
      !> @copydoc ParameterLists::init_ParamType_SSK_a1
      PROCEDURE,PASS,PRIVATE :: initSSKa1 => init_ParamType_SSK_a1
      !> @copybrief ParameterLists::init_ParamType_SDK_a1
      !> @copydoc ParameterLists::init_ParamType_SDK_a1
      PROCEDURE,PASS,PRIVATE :: initSDKa1 => init_ParamType_SDK_a1
      !> @copybrief ParameterLists::init_ParamType_SNK_a1
      !> @copydoc ParameterLists::init_ParamType_SNK_a1
      PROCEDURE,PASS,PRIVATE :: initSNKa1 => init_ParamType_SNK_a1
      !> @copybrief ParameterLists::init_ParamType_SLK_a1
      !> @copydoc ParameterLists::init_ParamType_SLK_a1
      PROCEDURE,PASS,PRIVATE :: initSLKa1 => init_ParamType_SLK_a1
      !> @copybrief ParameterLists::init_ParamType_SBK_a1
      !> @copydoc ParameterLists::init_ParamType_SBK_a1
      PROCEDURE,PASS,PRIVATE :: initSBKa1 => init_ParamType_SBK_a1
      !> @copybrief ParameterLists::init_ParamType_STR_a1
      !> @copydoc ParameterLists::init_ParamType_STR_a1
      PROCEDURE,PASS,PRIVATE :: initSTRa1 => init_ParamType_STR_a1
      !> @copybrief ParameterLists::init_ParamType_SSK_a2
      !> @copydoc ParameterLists::init_ParamType_SSK_a2
      PROCEDURE,PASS,PRIVATE :: initSSKa2 => init_ParamType_SSK_a2
      !> @copybrief ParameterLists::init_ParamType_SDK_a2
      !> @copydoc ParameterLists::init_ParamType_SDK_a2
      PROCEDURE,PASS,PRIVATE :: initSDKa2 => init_ParamType_SDK_a2
      !> @copybrief ParameterLists::init_ParamType_SNK_a2
      !> @copydoc ParameterLists::init_ParamType_SNK_a2
      PROCEDURE,PASS,PRIVATE :: initSNKa2 => init_ParamType_SNK_a2
      !> @copybrief ParameterLists::init_ParamType_SLK_a2
      !> @copydoc ParameterLists::init_ParamType_SLK_a2
      PROCEDURE,PASS,PRIVATE :: initSLKa2 => init_ParamType_SLK_a2
      !> @copybrief ParameterLists::init_ParamType_STR_a2
      !> @copydoc ParameterLists::init_ParamType_STR_a2
      PROCEDURE,PASS,PRIVATE :: initSTRa2 => init_ParamType_STR_a2
      !> @copybrief ParameterLists::init_ParamType_SSK_a3
      !> @copydoc ParameterLists::init_ParamType_SSK_a3
      PROCEDURE,PASS,PRIVATE :: initSSKa3 => init_ParamType_SSK_a3
      !> @copybrief ParameterLists::init_ParamType_SDK_a3
      !> @copydoc ParameterLists::init_ParamType_SDK_a3
      PROCEDURE,PASS,PRIVATE :: initSDKa3 => init_ParamType_SDK_a3
      !> @copybrief ParameterLists::init_ParamType_SNK_a3
      !> @copydoc ParameterLists::init_ParamType_SNK_a3
      PROCEDURE,PASS,PRIVATE :: initSNKa3 => init_ParamType_SNK_a3
      !> @copybrief ParameterLists::init_ParamType_SLK_a3
      !> @copydoc ParameterLists::init_ParamType_SLK_a3
      PROCEDURE,PASS,PRIVATE :: initSLKa3 => init_ParamType_SLK_a3
      !> Generic type bound interface for all @c init operations
      GENERIC :: init => initParamList,initSSK,initSDK,initSNK,initSLK, &
                 initSBK,initSTR,initCHAR,initSSKa1,initSDKa1,initSNKa1, &
                 initSLKa1,initSBKa1,initSTRa1,initSSKa2,initSDKa2,initSNKa2, &
                 initSLKa2,initSTRa2,initSSKa3,initSDKa3,initSNKa3,initSLKa3
      PROCEDURE,PASS :: initFromXML
      !> @copybrief ParameterLists::set_ParamType_List
      !> @copydoc ParameterLists::set_ParamType_List
      PROCEDURE,PASS,PRIVATE :: setParamList => set_ParamType_List
      !> @copybrief ParameterLists::set_ParamType_SSK
      !> @copydoc ParameterLists::set_ParamType_SSK
      PROCEDURE,PASS,PRIVATE :: setSSK => set_ParamType_SSK
      !> @copybrief ParameterLists::set_ParamType_SDK
      !> @copydoc ParameterLists::set_ParamType_SDK
      PROCEDURE,PASS,PRIVATE :: setSDK => set_ParamType_SDK
      !> @copybrief ParameterLists::set_ParamType_SNK
      !> @copydoc ParameterLists::set_ParamType_SNK
      PROCEDURE,PASS,PRIVATE :: setSNK => set_ParamType_SNK
      !> @copybrief ParameterLists::set_ParamType_SLK
      !> @copydoc ParameterLists::set_ParamType_SLK
      PROCEDURE,PASS,PRIVATE :: setSLK => set_ParamType_SLK
      !> @copybrief ParameterLists::set_ParamType_SBK
      !> @copydoc ParameterLists::set_ParamType_SBK
      PROCEDURE,PASS,PRIVATE :: setSBK => set_ParamType_SBK
      !> @copybrief ParameterLists::set_ParamType_STR
      !> @copydoc ParameterLists::set_ParamType_STR
      PROCEDURE,PASS,PRIVATE :: setSTR => set_ParamType_STR
      !> @copybrief ParameterLists::set_ParamType_CHAR
      !> @copydoc ParameterLists::set_ParamType_CHAR
      PROCEDURE,PASS,PRIVATE :: setCHAR => set_ParamType_CHAR
      !> @copybrief ParameterLists::set_ParamType_SSK_a1
      !> @copydoc ParameterLists::set_ParamType_SSK_a1
      PROCEDURE,PASS,PRIVATE :: setSSKa1 => set_ParamType_SSK_a1
      !> @copybrief ParameterLists::set_ParamType_SDK_a1
      !> @copydoc ParameterLists::set_ParamType_SDK_a1
      PROCEDURE,PASS,PRIVATE :: setSDKa1 => set_ParamType_SDK_a1
      !> @copybrief ParameterLists::set_ParamType_SNK_a1
      !> @copydoc ParameterLists::set_ParamType_SNK_a1
      PROCEDURE,PASS,PRIVATE :: setSNKa1 => set_ParamType_SNK_a1
      !> @copybrief ParameterLists::set_ParamType_SLK_a1
      !> @copydoc ParameterLists::set_ParamType_SLK_a1
      PROCEDURE,PASS,PRIVATE :: setSLKa1 => set_ParamType_SLK_a1
      !> @copybrief ParameterLists::set_ParamType_SBK_a1
      !> @copydoc ParameterLists::set_ParamType_SBK_a1
      PROCEDURE,PASS,PRIVATE :: setSBKa1 => set_ParamType_SBK_a1
      !> @copybrief ParameterLists::set_ParamType_STR_a1
      !> @copydoc ParameterLists::set_ParamType_STR_a1
      PROCEDURE,PASS,PRIVATE :: setSTRa1 => set_ParamType_STR_a1
      !> @copybrief ParameterLists::set_ParamType_SSK_a2
      !> @copydoc ParameterLists::set_ParamType_SSK_a2
      PROCEDURE,PASS,PRIVATE :: setSSKa2 => set_ParamType_SSK_a2
      !> @copybrief ParameterLists::set_ParamType_SDK_a2
      !> @copydoc ParameterLists::set_ParamType_SDK_a2
      PROCEDURE,PASS,PRIVATE :: setSDKa2 => set_ParamType_SDK_a2
      !> @copybrief ParameterLists::set_ParamType_SNK_a2
      !> @copydoc ParameterLists::set_ParamType_SNK_a2
      PROCEDURE,PASS,PRIVATE :: setSNKa2 => set_ParamType_SNK_a2
      !> @copybrief ParameterLists::set_ParamType_SLK_a2
      !> @copydoc ParameterLists::set_ParamType_SLK_a2
      PROCEDURE,PASS,PRIVATE :: setSLKa2 => set_ParamType_SLK_a2
      !> @copybrief ParameterLists::set_ParamType_STR_a2
      !> @copydoc ParameterLists::set_ParamType_STR_a2
      PROCEDURE,PASS,PRIVATE :: setSTRa2 => set_ParamType_STR_a2
      !> @copybrief ParameterLists::set_ParamType_SSK_a3
      !> @copydoc ParameterLists::set_ParamType_SSK_a3
      PROCEDURE,PASS,PRIVATE :: setSSKa3 => set_ParamType_SSK_a3
      !> @copybrief ParameterLists::set_ParamType_SDK_a3
      !> @copydoc ParameterLists::set_ParamType_SDK_a3
      PROCEDURE,PASS,PRIVATE :: setSDKa3 => set_ParamType_SDK_a3
      !> @copybrief ParameterLists::set_ParamType_SNK_a3
      !> @copydoc ParameterLists::set_ParamType_SNK_a3
      PROCEDURE,PASS,PRIVATE :: setSNKa3 => set_ParamType_SNK_a3
      !> @copybrief ParameterLists::set_ParamType_SLK_a3
      !> @copydoc ParameterLists::set_ParamType_SLK_a3
      PROCEDURE,PASS,PRIVATE :: setSLKa3 => set_ParamType_SLK_a3
      !> Generic type bound interface for all @c set operations
      GENERIC :: set => setParamList,setSSK,setSDK,setSNK,setSLK, &
                 setSBK,setSTR,setCHAR,setSSKa1,setSDKa1,setSNKa1, &
                 setSLKa1,setSBKa1,setSTRa1,setSSKa2,setSDKa2,setSNKa2, &
                 setSLKa2,setSTRa2,setSSKa3,setSDKa3,setSNKa3,setSLKa3
      !> @copybrief ParameterLists::get_ParamType
      !> @copydoc ParameterLists::get_ParamType
      PROCEDURE,PASS,PRIVATE :: getParam => get_ParamType
      !> @copybrief ParameterLists::get_ParamType_List
      !> @copydoc ParameterLists::get_ParamType_List
      PROCEDURE,PASS,PRIVATE :: getParamList => get_ParamType_List
      !> @copybrief ParameterLists::get_ParamType_SSK
      !> @copydoc ParameterLists::get_ParamType_SSK
      PROCEDURE,PASS,PRIVATE :: getSSK => get_ParamType_SSK
      !> @copybrief ParameterLists::get_ParamType_SDK
      !> @copydoc ParameterLists::get_ParamType_SDK
      PROCEDURE,PASS,PRIVATE :: getSDK => get_ParamType_SDK
      !> @copybrief ParameterLists::get_ParamType_SNK
      !> @copydoc ParameterLists::get_ParamType_SNK
      PROCEDURE,PASS,PRIVATE :: getSNK => get_ParamType_SNK
      !> @copybrief ParameterLists::get_ParamType_SLK
      !> @copydoc ParameterLists::get_ParamType_SLK
      PROCEDURE,PASS,PRIVATE :: getSLK => get_ParamType_SLK
      !> @copybrief ParameterLists::get_ParamType_SBK
      !> @copydoc ParameterLists::get_ParamType_SBK
      PROCEDURE,PASS,PRIVATE :: getSBK => get_ParamType_SBK
      !> @copybrief ParameterLists::get_ParamType_STR
      !> @copydoc ParameterLists::get_ParamType_STR
      PROCEDURE,PASS,PRIVATE :: getSTR => get_ParamType_STR
      !> @copybrief ParameterLists::get_ParamType_CHAR
      !> @copydoc ParameterLists::get_ParamType_CHAR
      PROCEDURE,PASS,PRIVATE :: getCHAR => get_ParamType_CHAR
      !> @copybrief ParameterLists::get_ParamType_SSK_a1
      !> @copydoc ParameterLists::get_ParamType_SSK_a1
      PROCEDURE,PASS,PRIVATE :: getSSKa1 => get_ParamType_SSK_a1
      !> @copybrief ParameterLists::get_ParamType_SDK_a1
      !> @copydoc ParameterLists::get_ParamType_SDK_a1
      PROCEDURE,PASS,PRIVATE :: getSDKa1 => get_ParamType_SDK_a1
      !> @copybrief ParameterLists::get_ParamType_SNK_a1
      !> @copydoc ParameterLists::get_ParamType_SNK_a1
      PROCEDURE,PASS,PRIVATE :: getSNKa1 => get_ParamType_SNK_a1
      !> @copybrief ParameterLists::get_ParamType_SLK_a1
      !> @copydoc ParameterLists::get_ParamType_SLK_a1
      PROCEDURE,PASS,PRIVATE :: getSLKa1 => get_ParamType_SLK_a1
      !> @copybrief ParameterLists::get_ParamType_SBK_a1
      !> @copydoc ParameterLists::get_ParamType_SBK_a1
      PROCEDURE,PASS,PRIVATE :: getSBKa1 => get_ParamType_SBK_a1
      !> @copybrief ParameterLists::get_ParamType_STR_a1
      !> @copydoc ParameterLists::get_ParamType_STR_a1
      PROCEDURE,PASS,PRIVATE :: getSTRa1 => get_ParamType_STR_a1
      !> @copybrief ParameterLists::get_ParamType_SSK_a2
      !> @copydoc ParameterLists::get_ParamType_SSK_a2
      PROCEDURE,PASS,PRIVATE :: getSSKa2 => get_ParamType_SSK_a2
      !> @copybrief ParameterLists::get_ParamType_SDK_a2
      !> @copydoc ParameterLists::get_ParamType_SDK_a2
      PROCEDURE,PASS,PRIVATE :: getSDKa2 => get_ParamType_SDK_a2
      !> @copybrief ParameterLists::get_ParamType_SNK_a2
      !> @copydoc ParameterLists::get_ParamType_SNK_a2
      PROCEDURE,PASS,PRIVATE :: getSNKa2 => get_ParamType_SNK_a2
      !> @copybrief ParameterLists::get_ParamType_SLK_a2
      !> @copydoc ParameterLists::get_ParamType_SLK_a2
      PROCEDURE,PASS,PRIVATE :: getSLKa2 => get_ParamType_SLK_a2
      !> @copybrief ParameterLists::get_ParamType_STR_a2
      !> @copydoc ParameterLists::get_ParamType_STR_a2
      PROCEDURE,PASS,PRIVATE :: getSTRa2 => get_ParamType_STR_a2
      !> @copybrief ParameterLists::get_ParamType_SSK_a3
      !> @copydoc ParameterLists::get_ParamType_SSK_a3
      PROCEDURE,PASS,PRIVATE :: getSSKa3 => get_ParamType_SSK_a3
      !> @copybrief ParameterLists::get_ParamType_SDK_a3
      !> @copydoc ParameterLists::get_ParamType_SDK_a3
      PROCEDURE,PASS,PRIVATE :: getSDKa3 => get_ParamType_SDK_a3
      !> @copybrief ParameterLists::get_ParamType_SNK_a3
      !> @copydoc ParameterLists::get_ParamType_SNK_a3
      PROCEDURE,PASS,PRIVATE :: getSNKa3 => get_ParamType_SNK_a3
      !> @copybrief ParameterLists::get_ParamType_SLK_a3
      !> @copydoc ParameterLists::get_ParamType_SLK_a3
      PROCEDURE,PASS,PRIVATE :: getSLKa3 => get_ParamType_SLK_a3
      !> Generic type bound interface for all @c get operations
      GENERIC :: get => getParam,getParamList,getSSK,getSDK,getSNK, &
                 getSLK,getSBK,getSTR,getCHAR,getSSKa1,getSDKa1,getSNKa1, &
                 getSBKa1,getSLKa1,getSTRa1,getSSKa2,getSDKa2,getSNKa2, &
                 getSLKa2,getSTRa2,getSSKa3,getSDKa3,getSNKa3,getSLKa3
      !> @copybrief ParameterLists::add_ParamType
      !> @copydoc ParameterLists::add_ParamType
      PROCEDURE,PASS,PRIVATE :: addParam => add_ParamType
      !> @copybrief ParameterLists::add_ParamType_List
      !> @copydoc ParameterLists::add_ParamType_List
      PROCEDURE,PASS,PRIVATE :: addList => add_ParamType_List
      !> @copybrief ParameterLists::add_ParamType_SSK
      !> @copydoc ParameterLists::add_ParamType_SSK
      PROCEDURE,PASS,PRIVATE :: addSSK => add_ParamType_SSK
      !> @copybrief ParameterLists::add_ParamType_SDK
      !> @copydoc ParameterLists::add_ParamType_SDK
      PROCEDURE,PASS,PRIVATE :: addSDK => add_ParamType_SDK
      !> @copybrief ParameterLists::add_ParamType_SNK
      !> @copydoc ParameterLists::add_ParamType_SNK
      PROCEDURE,PASS,PRIVATE :: addSNK => add_ParamType_SNK
      !> @copybrief ParameterLists::add_ParamType_SLK
      !> @copydoc ParameterLists::add_ParamType_SLK
      PROCEDURE,PASS,PRIVATE :: addSLK => add_ParamType_SLK
      !> @copybrief ParameterLists::add_ParamType_SBK
      !> @copydoc ParameterLists::add_ParamType_SBK
      PROCEDURE,PASS,PRIVATE :: addSBK => add_ParamType_SBK
      !> @copybrief ParameterLists::add_ParamType_STR
      !> @copydoc ParameterLists::add_ParamType_STR
      PROCEDURE,PASS,PRIVATE :: addSTR => add_ParamType_STR
      !> @copybrief ParameterLists::add_ParamType_CHAR
      !> @copydoc ParameterLists::add_ParamType_CHAR
      PROCEDURE,PASS,PRIVATE :: addCHAR => add_ParamType_CHAR
      !> @copybrief ParameterLists::add_ParamType_SSK_a1
      !> @copydoc ParameterLists::add_ParamType_SSK_a1
      PROCEDURE,PASS,PRIVATE :: addSSKa1 => add_ParamType_SSK_a1
      !> @copybrief ParameterLists::add_ParamType_SDK_a1
      !> @copydoc ParameterLists::add_ParamType_SDK_a1
      PROCEDURE,PASS,PRIVATE :: addSDKa1 => add_ParamType_SDK_a1
      !> @copybrief ParameterLists::add_ParamType_SNK_a1
      !> @copydoc ParameterLists::add_ParamType_SNK_a1
      PROCEDURE,PASS,PRIVATE :: addSNKa1 => add_ParamType_SNK_a1
      !> @copybrief ParameterLists::add_ParamType_SLK_a1
      !> @copydoc ParameterLists::add_ParamType_SLK_a1
      PROCEDURE,PASS,PRIVATE :: addSLKa1 => add_ParamType_SLK_a1
      !> @copybrief ParameterLists::add_ParamType_SBK_a1
      !> @copydoc ParameterLists::add_ParamType_SBK_a1
      PROCEDURE,PASS,PRIVATE :: addSBKa1 => add_ParamType_SBK_a1
      !> @copybrief ParameterLists::add_ParamType_STR_a1
      !> @copydoc ParameterLists::add_ParamType_STR_a1
      PROCEDURE,PASS,PRIVATE :: addSTRa1 => add_ParamType_STR_a1
      !> @copybrief ParameterLists::add_ParamType_SSK_a2
      !> @copydoc ParameterLists::add_ParamType_SSK_a2
      PROCEDURE,PASS,PRIVATE :: addSSKa2 => add_ParamType_SSK_a2
      !> @copybrief ParameterLists::add_ParamType_SDK_a2
      !> @copydoc ParameterLists::add_ParamType_SDK_a2
      PROCEDURE,PASS,PRIVATE :: addSDKa2 => add_ParamType_SDK_a2
      !> @copybrief ParameterLists::add_ParamType_SNK_a2
      !> @copydoc ParameterLists::add_ParamType_SNK_a2
      PROCEDURE,PASS,PRIVATE :: addSNKa2 => add_ParamType_SNK_a2
      !> @copybrief ParameterLists::add_ParamType_SLK_a2
      !> @copydoc ParameterLists::add_ParamType_SLK_a2
      PROCEDURE,PASS,PRIVATE :: addSLKa2 => add_ParamType_SLK_a2
      !> @copybrief ParameterLists::add_ParamType_STR_a2
      !> @copydoc ParameterLists::add_ParamType_STR_a2
      PROCEDURE,PASS,PRIVATE :: addSTRa2 => add_ParamType_STR_a2
      !> @copybrief ParameterLists::add_ParamType_SSK_a3
      !> @copydoc ParameterLists::add_ParamType_SSK_a3
      PROCEDURE,PASS,PRIVATE :: addSSKa3 => add_ParamType_SSK_a3
      !> @copybrief ParameterLists::add_ParamType_SDK_a3
      !> @copydoc ParameterLists::add_ParamType_SDK_a3
      PROCEDURE,PASS,PRIVATE :: addSDKa3 => add_ParamType_SDK_a3
      !> @copybrief ParameterLists::add_ParamType_SNK_a3
      !> @copydoc ParameterLists::add_ParamType_SNK_a3
      PROCEDURE,PASS,PRIVATE :: addSNKa3 => add_ParamType_SNK_a3
      !> @copybrief ParameterLists::add_ParamType_SLK_a3
      !> @copydoc ParameterLists::add_ParamType_SLK_a3
      PROCEDURE,PASS,PRIVATE :: addSLKa3 => add_ParamType_SLK_a3
      !> Generic type bound interface for all @c add operations
      GENERIC :: add => addParam,addList,addSSK,addSDK, &
         addSNK,addSLK,addSBK,addSTR,addCHAR,addSSKa1,addSDKa1, &
         addSNKa1,addSLKa1,addSBKa1,addSTRa1,addSSKa2,addSDKa2, &
         addSNKa2,addSLKa2,addSTRa2,addSSKa3,addSDKa3,addSNKa3,addSLKa3
      !> @copybrief ParameterLists::remove_ParamType
      !> @copydoc ParameterLists::remove_ParamType
      PROCEDURE,PASS :: remove => remove_ParamType
      !> @copybrief ParameterLists::has_ParamType
      !> @copydoc ParameterLists::has_ParamType
      PROCEDURE,PASS :: getNextParam => getNextParam_ParamType
      PROCEDURE,PASS :: has => has_ParamType
      !> @copybrief ParameterLists::validate_ParamType
      !> @copydoc ParameterLists::validate_ParamType
      PROCEDURE,PASS :: validate => validate_ParamType
      !> @copybrief ParameterLists::verify_ParamType
      !> @copydoc ParameterLists::verify_ParamType
      PROCEDURE,PASS :: verify => verify_ParamType
      !> @copybrief ParameterLists::edit_ParamType
      !> @copydoc ParameterLists::edit_ParamType
      PROCEDURE,PASS :: edit => edit_ParamType
      !> @copybrief ParameterLists::clear_ParamType
      !> @copydoc ParameterLists::clear_ParamType
      PROCEDURE,PASS :: clear => clear_ParamType
  PROCEDURE :: procXMLTree
  ENDTYPE ParamType

  !> @brief Extended type of a ParamType for defining a list of parameters
  TYPE,EXTENDS(ParamType) :: ParamType_List
    !> The list of parameters
    TYPE(ParamType),ALLOCATABLE :: pList(:)
!
!List of type bound procedures
    CONTAINS
      !> @copybrief ParameterLists::edit_ParamType_List
      !> @copydoc ParameterLists::edit_ParamType_List
      PROCEDURE,PASS :: edit => edit_ParamType_List
      !> @copybrief ParameterLists::clear_ParamType_List
      !> @copydoc ParameterLists::clear_ParamType_List
      PROCEDURE,PASS :: clear => clear_ParamType_List
  ENDTYPE ParamType_List

  !> @brief Extended type of a ParamType for defining a parameter that
  !> is a single precision real scalar
  TYPE,EXTENDS(ParamType) :: ParamType_SSK
    !> The value of the parameter
    REAL(SSK) :: val=0.0_SSK
!
!List of type bound procedures
    CONTAINS
      !> @copybrief ParameterLists::edit_ParamType_SSK
      !> @copydoc ParameterLists::edit_ParamType_SSK
      PROCEDURE,PASS :: edit => edit_ParamType_SSK
      !> @copybrief ParameterLists::clear_ParamType_SSK
      !> @copydoc ParameterLists::clear_ParamType_SSK
      PROCEDURE,PASS :: clear => clear_ParamType_SSK
  ENDTYPE ParamType_SSK

  !> @brief Extended type of a ParamType for defining a parameter that
  !> is a double precision real scalar
  TYPE,EXTENDS(ParamType) :: ParamType_SDK
    !> The value of the parameter
    REAL(SDK) :: val=0.0_SDK
!
!List of type bound procedures
    CONTAINS
      !> @copybrief ParameterLists::edit_ParamType_SDK
      !> @copydoc ParameterLists::edit_ParamType_SDK
      PROCEDURE,PASS :: edit => edit_ParamType_SDK
      !> @copybrief ParameterLists::clear_ParamType_SDK
      !> @copydoc ParameterLists::clear_ParamType_SDK
      PROCEDURE,PASS :: clear => clear_ParamType_SDK
  ENDTYPE ParamType_SDK

  !> @brief Extended type of a ParamType for defining a parameter that
  !> is a 32-bit integer scalar
  TYPE,EXTENDS(ParamType) :: ParamType_SNK
    !> The value of the parameter
    INTEGER(SNK) :: val=0_SNK
!
!List of type bound procedures
    CONTAINS
      !> @copybrief ParameterLists::edit_ParamType_SNK
      !> @copydoc ParameterLists::edit_ParamType_SNK
      PROCEDURE,PASS :: edit => edit_ParamType_SNK
      !> @copybrief ParameterLists::clear_ParamType_SNK
      !> @copydoc ParameterLists::clear_ParamType_SNK
      PROCEDURE,PASS :: clear => clear_ParamType_SNK
  ENDTYPE ParamType_SNK

  !> @brief Extended type of a ParamType for defining a parameter that
  !> is a 64-bit integer scalar
  TYPE,EXTENDS(ParamType) :: ParamType_SLK
    !> The value of the parameter
    INTEGER(SLK) :: val=0_SLK
!
!List of type bound procedures
    CONTAINS
      !> @copybrief ParameterLists::edit_ParamType_SLK
      !> @copydoc ParameterLists::edit_ParamType_SLK
      PROCEDURE,PASS :: edit => edit_ParamType_SLK
      !> @copybrief ParameterLists::clear_ParamType_SLK
      !> @copydoc ParameterLists::clear_ParamType_SLK
      PROCEDURE,PASS :: clear => clear_ParamType_SLK
  ENDTYPE ParamType_SLK

  !> @brief Extended type of a ParamType for defining a parameter that
  !> is a logical scalar
  TYPE,EXTENDS(ParamType) :: ParamType_SBK
    !> The value of the parameter
    LOGICAL(SBK) :: val=.FALSE.
!
!List of type bound procedures
    CONTAINS
      !> @copybrief ParameterLists::edit_ParamType_SBK
      !> @copydoc ParameterLists::edit_ParamType_SBK
      PROCEDURE,PASS :: edit => edit_ParamType_SBK
      !> @copybrief ParameterLists::clear_ParamType_SBK
      !> @copydoc ParameterLists::clear_ParamType_SBK
      PROCEDURE,PASS :: clear => clear_ParamType_SBK
  ENDTYPE ParamType_SBK

  !> @brief Extended type of a ParamType for defining a parameter that
  !> is a string derived type
  TYPE,EXTENDS(ParamType) :: ParamType_STR
    !> The value of the parameter
    TYPE(StringType) :: val
!
!List of type bound procedures
    CONTAINS
      !> @copybrief ParameterLists::edit_ParamType_STR
      !> @copydoc ParameterLists::edit_ParamType_STR
      PROCEDURE,PASS :: edit => edit_ParamType_STR
      !> @copybrief ParameterLists::clear_ParamType_STR
      !> @copydoc ParameterLists::clear_ParamType_STR
      PROCEDURE,PASS :: clear => clear_ParamType_STR
  ENDTYPE ParamType_STR
!----------------------------------------------------------------------
!     One-Dimensional Arrays
!----------------------------------------------------------------------
  !> @brief Extended type of a ParamType for defining a one dimensional
  !> array parameter of single precision reals
  TYPE,EXTENDS(ParamType) :: ParamType_SSK_a1
    !> The value of the parameter
    REAL(SSK),ALLOCATABLE :: val(:)
!
!List of type bound procedures
    CONTAINS
      !> @copybrief ParameterLists::edit_ParamType_SSK_a1
      !> @copydoc ParameterLists::edit_ParamType_SSK_a1
      PROCEDURE,PASS :: edit => edit_ParamType_SSK_a1
      !> @copybrief ParameterLists::clear_ParamType_SSK_a1
      !> @copydoc ParameterLists::clear_ParamType_SSK_a1
      PROCEDURE,PASS :: clear => clear_ParamType_SSK_a1
  ENDTYPE ParamType_SSK_a1

  !> @brief Extended type of a ParamType for defining a one dimensional
  !> array parameter of double precision reals
  TYPE,EXTENDS(ParamType) :: ParamType_SDK_a1
    !> The value of the parameter
    REAL(SDK),ALLOCATABLE :: val(:)
!
!List of type bound procedures
    CONTAINS
      !> @copybrief ParameterLists::edit_ParamType_SDK_a1
      !> @copydoc ParameterLists::edit_ParamType_SDK_a1
      PROCEDURE,PASS :: edit => edit_ParamType_SDK_a1
      !> @copybrief ParameterLists::clear_ParamType_SDK_a1
      !> @copydoc ParameterLists::clear_ParamType_SDK_a1
      PROCEDURE,PASS :: clear => clear_ParamType_SDK_a1
  ENDTYPE ParamType_SDK_a1

  !> @brief Extended type of a ParamType for defining a one dimensional
  !> array parameter of 32-bit integers
  TYPE,EXTENDS(ParamType) :: ParamType_SNK_a1
    !> The value of the parameter
    INTEGER(SNK),ALLOCATABLE :: val(:)
!
!List of type bound procedures
    CONTAINS
      !> @copybrief ParameterLists::edit_ParamType_SNK_a1
      !> @copydoc ParameterLists::edit_ParamType_SNK_a1
      PROCEDURE,PASS :: edit => edit_ParamType_SNK_a1
      !> @copybrief ParameterLists::clear_ParamType_SNK_a1
      !> @copydoc ParameterLists::clear_ParamType_SNK_a1
      PROCEDURE,PASS :: clear => clear_ParamType_SNK_a1
  ENDTYPE ParamType_SNK_a1

  !> @brief Extended type of a ParamType for defining a one dimensional
  !> array parameter of 64-bit integers
  TYPE,EXTENDS(ParamType) :: ParamType_SLK_a1
    !> The value of the parameter
    INTEGER(SLK),ALLOCATABLE :: val(:)
!
!List of type bound procedures
    CONTAINS
      !> @copybrief ParameterLists::edit_ParamType_SLK_a1
      !> @copydoc ParameterLists::edit_ParamType_SLK_a1
      PROCEDURE,PASS :: edit => edit_ParamType_SLK_a1
      !> @copybrief ParameterLists::clear_ParamType_SLK_a1
      !> @copydoc ParameterLists::clear_ParamType_SLK_a1
      PROCEDURE,PASS :: clear => clear_ParamType_SLK_a1
  ENDTYPE ParamType_SLK_a1

  !> @brief Extended type of a ParamType for defining a one dimensional
  !> array parameter of single precision reals
  TYPE,EXTENDS(ParamType) :: ParamType_SBK_a1
    !> The value of the parameter
    LOGICAL(SBK),ALLOCATABLE :: val(:)
!
!List of type bound procedures
    CONTAINS
      !> @copybrief ParameterLists::edit_ParamType_SBK_a1
      !> @copydoc ParameterLists::edit_ParamType_SBK_a1
      PROCEDURE,PASS :: edit => edit_ParamType_SBK_a1
      !> @copybrief ParameterLists::clear_ParamType_SBK_a1
      !> @copydoc ParameterLists::clear_ParamType_SBK_a1
      PROCEDURE,PASS :: clear => clear_ParamType_SBK_a1
  ENDTYPE ParamType_SBK_a1

  !> @brief Extended type of a ParamType for defining a one dimensional
  !> array parameter that is a string derived type
  TYPE,EXTENDS(ParamType) :: ParamType_STR_a1
    !> The value of the parameter
    TYPE(StringType),ALLOCATABLE :: val(:)
!
!List of type bound procedures
    CONTAINS
      !> @copybrief ParameterLists::edit_ParamType_STR_a1
      !> @copydoc ParameterLists::edit_ParamType_STR_a1
      PROCEDURE,PASS :: edit => edit_ParamType_STR_a1
      !> @copybrief ParameterLists::clear_ParamType_STR_a1
      !> @copydoc ParameterLists::clear_ParamType_STR_a1
      PROCEDURE,PASS :: clear => clear_ParamType_STR_a1
  ENDTYPE ParamType_STR_a1

!----------------------------------------------------------------------
!     Two-Dimensional Arrays
!----------------------------------------------------------------------
  !> @brief Extended type of a ParamType for defining a two dimensional
  !> array parameter of single precision reals
  TYPE,EXTENDS(ParamType) :: ParamType_SSK_a2
    !> The value of the parameter
    REAL(SSK),ALLOCATABLE :: val(:,:)
!
!List of type bound procedures
    CONTAINS
      !> @copybrief ParameterLists::edit_ParamType_SSK_a2
      !> @copydoc ParameterLists::edit_ParamType_SSK_a2
      PROCEDURE,PASS :: edit => edit_ParamType_SSK_a2
      !> @copybrief ParameterLists::clear_ParamType_SSK_a2
      !> @copydoc ParameterLists::clear_ParamType_SSK_a2
      PROCEDURE,PASS :: clear => clear_ParamType_SSK_a2
  ENDTYPE ParamType_SSK_a2

  !> @brief Extended type of a ParamType for defining a two dimensional
  !> array parameter of double precision reals
  TYPE,EXTENDS(ParamType) :: ParamType_SDK_a2
    !> The value of the parameter
    REAL(SDK),ALLOCATABLE :: val(:,:)
!
!List of type bound procedures
    CONTAINS
      !> @copybrief ParameterLists::edit_ParamType_SDK_a2
      !> @copydoc ParameterLists::edit_ParamType_SDK_a2
      PROCEDURE,PASS :: edit => edit_ParamType_SDK_a2
      !> @copybrief ParameterLists::clear_ParamType_SDK_a2
      !> @copydoc ParameterLists::clear_ParamType_SDK_a2
      PROCEDURE,PASS :: clear => clear_ParamType_SDK_a2
  ENDTYPE ParamType_SDK_a2

  !> @brief Extended type of a ParamType for defining a two dimensional
  !> array parameter of 32-bit integers
  TYPE,EXTENDS(ParamType) :: ParamType_SNK_a2
    !> The value of the parameter
    INTEGER(SNK),ALLOCATABLE :: val(:,:)
!
!List of type bound procedures
    CONTAINS
      !> @copybrief ParameterLists::edit_ParamType_SNK_a2
      !> @copydoc ParameterLists::edit_ParamType_SNK_a2
      PROCEDURE,PASS :: edit => edit_ParamType_SNK_a2
      !> @copybrief ParameterLists::clear_ParamType_SNK_a2
      !> @copydoc ParameterLists::clear_ParamType_SNK_a2
      PROCEDURE,PASS :: clear => clear_ParamType_SNK_a2
  ENDTYPE ParamType_SNK_a2

  !> @brief Extended type of a ParamType for defining a two dimensional
  !> array parameter of 64-bit integers
  TYPE,EXTENDS(ParamType) :: ParamType_SLK_a2
    !> The value of the parameter
    INTEGER(SLK),ALLOCATABLE :: val(:,:)
!
!List of type bound procedures
    CONTAINS
      !> @copybrief ParameterLists::edit_ParamType_SLK_a2
      !> @copydoc ParameterLists::edit_ParamType_SLK_a2
      PROCEDURE,PASS :: edit => edit_ParamType_SLK_a2
      !> @copybrief ParameterLists::clear_ParamType_SLK_a2
      !> @copydoc ParameterLists::clear_ParamType_SLK_a2
      PROCEDURE,PASS :: clear => clear_ParamType_SLK_a2
  ENDTYPE ParamType_SLK_a2

  !> @brief Extended type of a ParamType for defining a two dimensional
  !> array parameter that is a string derived type
  TYPE,EXTENDS(ParamType) :: ParamType_STR_a2
    !> The value of the parameter
    TYPE(StringType),ALLOCATABLE :: val(:,:)
!
!List of type bound procedures
    CONTAINS
      !> @copybrief ParameterLists::edit_ParamType_STR_a2
      !> @copydoc ParameterLists::edit_ParamType_STR_a2
      PROCEDURE,PASS :: edit => edit_ParamType_STR_a2
      !> @copybrief ParameterLists::clear_ParamType_STR_a2
      !> @copydoc ParameterLists::clear_ParamType_STR_a2
      PROCEDURE,PASS :: clear => clear_ParamType_STR_a2
  ENDTYPE ParamType_STR_a2
!----------------------------------------------------------------------
!     Three-Dimensional Arrays
!----------------------------------------------------------------------
  !> @brief Extended type of a ParamType for defining a three dimensional
  !> array parameter of single precision reals
  TYPE,EXTENDS(ParamType) :: ParamType_SSK_a3
    !> The value of the parameter
    REAL(SSK),ALLOCATABLE :: val(:,:,:)
!
!List of type bound procedures
    CONTAINS
      !> @copybrief ParameterLists::edit_ParamType_SSK_a3
      !> @copydoc ParameterLists::edit_ParamType_SSK_a3
      PROCEDURE,PASS :: edit => edit_ParamType_SSK_a3
      !> @copybrief ParameterLists::clear_ParamType_SSK_a3
      !> @copydoc ParameterLists::clear_ParamType_SSK_a3
      PROCEDURE,PASS :: clear => clear_ParamType_SSK_a3
  ENDTYPE ParamType_SSK_a3

  !> @brief Extended type of a ParamType for defining a three dimensional
  !> array parameter of double precision reals
  TYPE,EXTENDS(ParamType) :: ParamType_SDK_a3
    !> The value of the parameter
    REAL(SDK),ALLOCATABLE :: val(:,:,:)
!
!List of type bound procedures
    CONTAINS
      !> @copybrief ParameterLists::edit_ParamType_SDK_a3
      !> @copydoc ParameterLists::edit_ParamType_SDK_a3
      PROCEDURE,PASS :: edit => edit_ParamType_SDK_a3
      !> @copybrief ParameterLists::clear_ParamType_SDK_a3
      !> @copydoc ParameterLists::clear_ParamType_SDK_a3
      PROCEDURE,PASS :: clear => clear_ParamType_SDK_a3
  ENDTYPE ParamType_SDK_a3

  !> @brief Extended type of a ParamType for defining a three dimensional
  !> array parameter of 32-bit integers
  TYPE,EXTENDS(ParamType) :: ParamType_SNK_a3
    !> The value of the parameter
    INTEGER(SNK),ALLOCATABLE :: val(:,:,:)
!
!List of type bound procedures
    CONTAINS
      !> @copybrief ParameterLists::edit_ParamType_SNK_a3
      !> @copydoc ParameterLists::edit_ParamType_SNK_a3
      PROCEDURE,PASS :: edit => edit_ParamType_SNK_a3
      !> @copybrief ParameterLists::clear_ParamType_SNK_a3
      !> @copydoc ParameterLists::clear_ParamType_SNK_a3
      PROCEDURE,PASS :: clear => clear_ParamType_SNK_a3
  ENDTYPE ParamType_SNK_a3

  !> @brief Extended type of a ParamType for defining a three dimensional
  !> array parameter of 64-bit integers
  TYPE,EXTENDS(ParamType) :: ParamType_SLK_a3
    !> The value of the parameter
    INTEGER(SLK),ALLOCATABLE :: val(:,:,:)
!
!List of type bound procedures
    CONTAINS
      !> @copybrief ParameterLists::edit_ParamType_SLK_a3
      !> @copydoc ParameterLists::edit_ParamType_SLK_a3
      PROCEDURE,PASS :: edit => edit_ParamType_SLK_a3
      !> @copybrief ParameterLists::clear_ParamType_SLK_a3
      !> @copydoc ParameterLists::clear_ParamType_SLK_a3
      PROCEDURE,PASS :: clear => clear_ParamType_SLK_a3
  ENDTYPE ParamType_SLK_a3
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !> Generic interface adds a procedure to overload the intrinsic assignment
  !> operator with the given procedure
  INTERFACE ASSIGNMENT(=)
    !> @copybrief ParameterLists::assign_ParamType
    !> @copydoc ParameterLists::assign_ParamType
    MODULE PROCEDURE assign_ParamType
  ENDINTERFACE

  INTERFACE OPERATOR(==)
    !> @copybrief ParameterLists::isEqual_ParamType
    !> @copydoc ParameterLists::isEqual_ParamType
    MODULE PROCEDURE isEqual_ParamType
  ENDINTERFACE
!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief Defines the assignment operation two @c ParamType objects.
!> @param thisParam the ParamType object to be assigned
!> @param param the ParamType object to assign
!>
!> This routine clears the @c thisParam which must be a declared as a
!> <TT>TYPE(ParamType)</TT> in the client code. @c param may be anything. As
!> new extended types of the @c ParamType are defined in this module this
!> will need to be updated with a new <TT>TYPE IS()</TT> block. This should
!> be the only routine that knows about all the extended types of @c ParamType.
!>
    RECURSIVE SUBROUTINE assign_ParamType(thisParam,param)
      CHARACTER(LEN=*),PARAMETER :: myName='assign_ParamType'
      CLASS(ParamType),INTENT(INOUT) :: thisParam
      CLASS(ParamType),INTENT(IN) :: param

      SELECTTYPE(thisParam)
        TYPE IS(ParamType)
          IF(ASSOCIATED(thisParam%pdat)) CALL thisParam%clear()
          SELECTTYPE(p=>param)
            TYPE IS(ParamType)
              !Assign the parameter value using a recursive call
              IF(ASSOCIATED(p%pdat)) CALL assign_ParamType(thisParam,p%pdat)
            TYPE IS(ParamType_SSK)
              CALL thisParam%init(CHAR(p%name),p%val, &
                CHAR(p%description))
            TYPE IS(ParamType_SDK)
              CALL thisParam%init(CHAR(p%name),p%val, &
                CHAR(p%description))
            TYPE IS(ParamType_SNK)
              CALL thisParam%init(CHAR(p%name),p%val, &
                CHAR(p%description))
            TYPE IS(ParamType_SLK)
              CALL thisParam%init(CHAR(p%name),p%val, &
                CHAR(p%description))
            TYPE IS(ParamType_SBK)
              CALL thisParam%init(CHAR(p%name),p%val, &
                CHAR(p%description))
            TYPE IS(ParamType_STR)
              CALL thisParam%init(CHAR(p%name),p%val, &
                CHAR(p%description))
            TYPE IS(ParamType_SSK_a1)
              CALL thisParam%init(CHAR(p%name),p%val, &
                CHAR(p%description))
            TYPE IS(ParamType_SDK_a1)
              CALL thisParam%init(CHAR(p%name),p%val, &
                CHAR(p%description))
            TYPE IS(ParamType_SNK_a1)
              CALL thisParam%init(CHAR(p%name),p%val, &
                CHAR(p%description))
            TYPE IS(ParamType_SLK_a1)
              CALL thisParam%init(CHAR(p%name),p%val, &
                CHAR(p%description))
            TYPE IS(ParamType_SBK_a1)
              CALL thisParam%init(CHAR(p%name),p%val, &
                CHAR(p%description))
            TYPE IS(ParamType_STR_a1)
              CALL thisParam%init(CHAR(p%name),p%val, &
                CHAR(p%description))
            TYPE IS(ParamType_SSK_a2)
              CALL thisParam%init(CHAR(p%name),p%val, &
                CHAR(p%description))
            TYPE IS(ParamType_SDK_a2)
              CALL thisParam%init(CHAR(p%name),p%val, &
                CHAR(p%description))
            TYPE IS(ParamType_SNK_a2)
              CALL thisParam%init(CHAR(p%name),p%val, &
                CHAR(p%description))
            TYPE IS(ParamType_SLK_a2)
              CALL thisParam%init(CHAR(p%name),p%val, &
                CHAR(p%description))
            TYPE IS(ParamType_STR_a2)
              CALL thisParam%init(CHAR(p%name),p%val, &
                CHAR(p%description))
            TYPE IS(ParamType_SSK_a3)
              CALL thisParam%init(CHAR(p%name),p%val, &
                CHAR(p%description))
            TYPE IS(ParamType_SDK_a3)
              CALL thisParam%init(CHAR(p%name),p%val, &
                CHAR(p%description))
            TYPE IS(ParamType_SNK_a3)
              CALL thisParam%init(CHAR(p%name),p%val, &
                CHAR(p%description))
            TYPE IS(ParamType_SLK_a3)
              CALL thisParam%init(CHAR(p%name),p%val, &
                CHAR(p%description))
            TYPE IS(ParamType_List)
              IF(ALLOCATED(p%pList)) THEN
                CALL thisParam%init(CHAR(p%name),p%pList, &
                  CHAR(p%description))
              ELSE
                !Allocate an empty list
                ALLOCATE(ParamType_List :: thisParam%pdat)
                thisParam%pdat%dataType='TYPE(ParamType_List)'
                thisParam%pdat%name=p%name
                thisParam%pdat%description=p%description
              ENDIF
          ENDSELECT
        CLASS DEFAULT
          CALL eParams%raiseError(modName//'::'//myName// &
            ' - cannot assign parameter data to a type extension of ParamType!')
      ENDSELECT
    ENDSUBROUTINE assign_ParamType
!
!-------------------------------------------------------------------------------
!> @brief
!> @param p1
!> @param p2
!> @returns bool
!>
    RECURSIVE PURE FUNCTION isEqual_ParamType(p1,p2) RESULT(bool)
      CLASS(ParamType),INTENT(IN) :: p1
      CLASS(ParamType),INTENT(IN) :: p2
      LOGICAL(SBK) :: bool
      INTEGER(SIK) :: i,j,dims1(7),dims2(7)
      bool=.FALSE.
      IF(p1%name == p2%name) THEN
        IF(SAME_TYPE_AS(p1,p2)) THEN
          dims1=0
          dims2=0
          SELECTTYPE(p1)
            TYPE IS(ParamType)
              IF(ASSOCIATED(p1%pdat) .AND. ASSOCIATED(p2%pdat)) THEN
                bool=isEqual_ParamType(p1%pdat,p2%pdat)
              ELSE
                bool=(ASSOCIATED(p1%pdat) .EQV. ASSOCIATED(p2%pdat))
              ENDIF
            TYPE IS(ParamType_List)
              SELECTTYPE(p2); TYPE IS(ParamType_List)
                bool=(ALLOCATED(p1%pList) .EQV. ALLOCATED(p2%pList))
                IF(ALLOCATED(p1%pList) .AND. ALLOCATED(p2%pList)) THEN
                  IF(SIZE(p1%pList) == SIZE(p2%pList)) THEN
                    DO i=1,SIZE(p1%pList)
                      bool=(isEqual_ParamType(p1%pList(i),p2%pList(i)).AND.bool)
                      IF(.NOT.bool) EXIT
                    ENDDO
                  ENDIF
                ENDIF
              ENDSELECT
            TYPE IS(ParamType_SSK)
              SELECTTYPE(p2); TYPE IS(ParamType_SSK)
                bool=(p1%val == p2%val)
              ENDSELECT
            TYPE IS(ParamType_SDK)
              SELECTTYPE(p2); TYPE IS(ParamType_SDK)
                bool=(p1%val == p2%val)
              ENDSELECT
            TYPE IS(ParamType_SNK)
              SELECTTYPE(p2); TYPE IS(ParamType_SNK)
                bool=(p1%val == p2%val)
              ENDSELECT
            TYPE IS(ParamType_SLK)
              SELECTTYPE(p2); TYPE IS(ParamType_SLK)
                bool=(p1%val == p2%val)
              ENDSELECT
            TYPE IS(ParamType_SBK)
              SELECTTYPE(p2); TYPE IS(ParamType_SBK)
                bool=(p1%val == p2%val)
              ENDSELECT
            TYPE IS(ParamType_STR)
              SELECTTYPE(p2); TYPE IS(ParamType_STR)
                bool=(p1%val == p2%val)
              ENDSELECT
            TYPE IS(ParamType_SSK_a1)
              SELECTTYPE(p2); TYPE IS(ParamType_SSK_a1)
                bool=(ALLOCATED(p1%val) .EQV. ALLOCATED(p2%val))
                IF(ALLOCATED(p1%val) .AND. ALLOCATED(p1%val)) THEN
                  IF(SIZE(p1%val) == SIZE(p2%val)) bool=ALL(p1%val == p2%val)
                ENDIF
              ENDSELECT
            TYPE IS(ParamType_SDK_a1)
              SELECTTYPE(p2); TYPE IS(ParamType_SDK_a1)
                bool=(ALLOCATED(p1%val) .EQV. ALLOCATED(p2%val))
                IF(ALLOCATED(p1%val) .AND. ALLOCATED(p1%val)) THEN
                  IF(SIZE(p1%val) == SIZE(p2%val)) bool=ALL(p1%val == p2%val)
                ENDIF
              ENDSELECT
            TYPE IS(ParamType_SNK_a1)
              SELECTTYPE(p2); TYPE IS(ParamType_SNK_a1)
                bool=(ALLOCATED(p1%val) .EQV. ALLOCATED(p2%val))
                IF(ALLOCATED(p1%val) .AND. ALLOCATED(p1%val)) THEN
                  IF(SIZE(p1%val) == SIZE(p2%val)) bool=ALL(p1%val == p2%val)
                ENDIF
              ENDSELECT
            TYPE IS(ParamType_SLK_a1)
              SELECTTYPE(p2); TYPE IS(ParamType_SLK_a1)
                bool=(ALLOCATED(p1%val) .EQV. ALLOCATED(p2%val))
                IF(ALLOCATED(p1%val) .AND. ALLOCATED(p1%val)) THEN
                  IF(SIZE(p1%val) == SIZE(p2%val)) bool=ALL(p1%val == p2%val)
                ENDIF
              ENDSELECT
            TYPE IS(ParamType_SBK_a1)
              SELECTTYPE(p2); TYPE IS(ParamType_SBK_a1)
                bool=(ALLOCATED(p1%val) .EQV. ALLOCATED(p2%val))
                IF(ALLOCATED(p1%val) .AND. ALLOCATED(p1%val)) THEN
                  IF(SIZE(p1%val) == SIZE(p2%val)) bool=ALL(p1%val .EQV. p2%val)
                ENDIF
              ENDSELECT
            TYPE IS(ParamType_STR_a1)
              SELECTTYPE(p2); TYPE IS(ParamType_STR_a1)
                bool=(ALLOCATED(p1%val) .EQV. ALLOCATED(p2%val))
                IF(ALLOCATED(p1%val) .AND. ALLOCATED(p1%val)) THEN
                  IF(SIZE(p1%val) == SIZE(p2%val)) THEN
                    DO i=1,SIZE(p1%val)
                      bool=(bool .AND. p1%val(i) == p2%val(i))
                      IF(.NOT.bool) EXIT
                    ENDDO
                  ENDIF
                ENDIF
              ENDSELECT
            TYPE IS(ParamType_SSK_a2)
              SELECTTYPE(p2); TYPE IS(ParamType_SSK_a2)
                bool=(ALLOCATED(p1%val) .EQV. ALLOCATED(p2%val))
                IF(ALLOCATED(p1%val) .AND. ALLOCATED(p1%val)) THEN
                  dims1(1:2)=SHAPE(p1%val)
                  dims2(1:2)=SHAPE(p2%val)
                  IF(ALL(dims1(1:2) == dims2(1:2))) bool=ALL(p1%val == p2%val)
                ENDIF
              ENDSELECT
            TYPE IS(ParamType_SDK_a2)
              SELECTTYPE(p2); TYPE IS(ParamType_SDK_a2)
                bool=(ALLOCATED(p1%val) .EQV. ALLOCATED(p2%val))
                IF(ALLOCATED(p1%val) .AND. ALLOCATED(p1%val)) THEN
                  dims1(1:2)=SHAPE(p1%val)
                  dims2(1:2)=SHAPE(p2%val)
                  IF(ALL(dims1(1:2) == dims2(1:2))) bool=ALL(p1%val == p2%val)
                ENDIF
              ENDSELECT
            TYPE IS(ParamType_SNK_a2)
              SELECTTYPE(p2); TYPE IS(ParamType_SNK_a2)
                bool=(ALLOCATED(p1%val) .EQV. ALLOCATED(p2%val))
                IF(ALLOCATED(p1%val) .AND. ALLOCATED(p1%val)) THEN
                  dims1(1:2)=SHAPE(p1%val)
                  dims2(1:2)=SHAPE(p2%val)
                  IF(ALL(dims1(1:2) == dims2(1:2))) bool=ALL(p1%val == p2%val)
                ENDIF
              ENDSELECT
            TYPE IS(ParamType_SLK_a2)
              SELECTTYPE(p2); TYPE IS(ParamType_SLK_a2)
                bool=(ALLOCATED(p1%val) .EQV. ALLOCATED(p2%val))
                IF(ALLOCATED(p1%val) .AND. ALLOCATED(p1%val)) THEN
                  dims1(1:2)=SHAPE(p1%val)
                  dims2(1:2)=SHAPE(p2%val)
                  IF(ALL(dims1(1:2) == dims2(1:2))) bool=ALL(p1%val == p2%val)
                ENDIF
              ENDSELECT
            TYPE IS(ParamType_STR_a2)
              SELECTTYPE(p2); TYPE IS(ParamType_STR_a2)
                bool=(ALLOCATED(p1%val) .EQV. ALLOCATED(p2%val))
                IF(ALLOCATED(p1%val) .AND. ALLOCATED(p1%val)) THEN
                  dims1(1:2)=SHAPE(p1%val)
                  dims2(1:2)=SHAPE(p2%val)
                  IF(ALL(dims1(1:2) == dims2(1:2))) THEN
                    DO j=1,SIZE(p1%val,DIM=2)
                      DO i=1,SIZE(p1%val,DIM=1)
                        bool=(bool .AND. p1%val(i,j) == p2%val(i,j))
                        IF(.NOT.bool) EXIT
                      ENDDO
                      IF(.NOT.bool) EXIT
                    ENDDO
                  ENDIF
                ENDIF
              ENDSELECT
            TYPE IS(ParamType_SSK_a3)
              SELECTTYPE(p2); TYPE IS(ParamType_SSK_a3)
                bool=(ALLOCATED(p1%val) .EQV. ALLOCATED(p2%val))
                IF(ALLOCATED(p1%val) .AND. ALLOCATED(p1%val)) THEN
                  dims1(1:3)=SHAPE(p1%val)
                  dims2(1:3)=SHAPE(p2%val)
                  IF(ALL(dims1(1:3) == dims2(1:3))) bool=ALL(p1%val == p2%val)
                ENDIF
              ENDSELECT
            TYPE IS(ParamType_SDK_a3)
              SELECTTYPE(p2); TYPE IS(ParamType_SDK_a3)
                bool=(ALLOCATED(p1%val) .EQV. ALLOCATED(p2%val))
                IF(ALLOCATED(p1%val) .AND. ALLOCATED(p1%val)) THEN
                  dims1(1:3)=SHAPE(p1%val)
                  dims2(1:3)=SHAPE(p2%val)
                  IF(ALL(dims1(1:3) == dims2(1:3))) bool=ALL(p1%val == p2%val)
                ENDIF
              ENDSELECT
            TYPE IS(ParamType_SNK_a3)
              SELECTTYPE(p2); TYPE IS(ParamType_SNK_a3)
                bool=(ALLOCATED(p1%val) .EQV. ALLOCATED(p2%val))
                IF(ALLOCATED(p1%val) .AND. ALLOCATED(p1%val)) THEN
                  dims1(1:3)=SHAPE(p1%val)
                  dims2(1:3)=SHAPE(p2%val)
                  IF(ALL(dims1(1:3) == dims2(1:3))) bool=ALL(p1%val == p2%val)
                ENDIF
              ENDSELECT
            TYPE IS(ParamType_SLK_a3)
              SELECTTYPE(p2); TYPE IS(ParamType_SLK_a3)
                bool=(ALLOCATED(p1%val) .EQV. ALLOCATED(p2%val))
                IF(ALLOCATED(p1%val) .AND. ALLOCATED(p1%val)) THEN
                  dims1(1:3)=SHAPE(p1%val)
                  dims2(1:3)=SHAPE(p2%val)
                  IF(ALL(dims1(1:3) == dims2(1:3))) bool=ALL(p1%val == p2%val)
                ENDIF
              ENDSELECT
          ENDSELECT
        ENDIF
      ENDIF
    ENDFUNCTION isEqual_ParamType
!
!-------------------------------------------------------------------------------
!> @brief Routine can be used as an "iterator". It takes an absolute list
!>        address and returns the next parameter encountered from the address.
!> @param thisParam the parameter list to obtain the next parameter from
!> @param addr the absolute address from which to find the next parameter
!> @param param a pointer to the next parameter, value is null if the next
!>        parameter does not exist.
!>
!> When @c addr is passed in empty then the root address is returned. To use
!> This routine as an iterator, a loop of the following form should be written
!> in the client code:
!> @code
!> TYPE(StringType) :: addr
!> TYPE(ParameType) :: paramList
!> CLASS(ParamType),POINTER :: nextParam
!> addr=''
!> CALL paramList%getNextParam(addr,nextParam)
!> DO WHILE(ASSOCIATED(nextParam))
!>   !Do stuff with nextParam
!>   !...
!>   CALL paramList%getNextParam(addr,nextParam)
!> ENDDO
!> @endcode
!>
    SUBROUTINE getNextParam_ParamType(thisParam,addr,param)
      CLASS(ParamType),TARGET,INTENT(IN) :: thisParam
      TYPE(StringType),INTENT(INOUT) :: addr
      CLASS(ParamType),POINTER,INTENT(OUT) :: param

      CHARACTER(LEN=addr%ntrim) :: addrIn,newAddr
      INTEGER(SIK) :: istp,ip
      TYPE(StringType) :: tmpAddr
      CLASS(ParamType),POINTER :: tmpParam,nextParam,parentParam

      nextParam => NULL()
      tmpAddr=''
      addrIn=TRIM(addr)
      IF(LEN_TRIM(addrIn) > 0) THEN
        CALL get_ParamType(thisParam,TRIM(addrIn),tmpParam)
        IF(ASSOCIATED(tmpParam)) THEN
          !Check to make sure param is in thisParam
          !if param is null that's ok too because we're guaranteed to
          !be within thisParam

          SELECTTYPE(tp => tmpParam)
            TYPE IS(ParamType_List)
              !Return the first entry in the list
              IF(ALLOCATED(tp%pList)) THEN
                nextParam => tp%pList(1)%pdat
                tmpAddr=TRIM(addrIn)//'->'//nextParam%name
              ELSE
                !This could be a null list within a list that still has
                !entries so get the parent and
                !Get the parent list
                newAddr=''
                istp=INDEX(addrIn,'->',.TRUE.)-1
                parentParam => NULL()
                IF(istp > 0) THEN
                  newAddr=addrIn(1:istp)
                  CALL get_ParamType(thisParam,TRIM(newAddr),parentParam)
                ENDIF
                !Search for the next parameter or parameter list
                parentSearch1: DO WHILE(ASSOCIATED(parentParam))

                  !Search the parent list
                  SELECTTYPE(pp => parentParam); TYPE IS(ParamType_List)
                    DO ip=1,SIZE(pp%pList)-1
                      IF(ASSOCIATED(pp%pList(ip)%pdat,tmpParam)) THEN
                        !Get the next parameter in the list
                        nextParam => pp%pList(ip+1)%pdat
                        tmpAddr=TRIM(newAddr)//'->'//nextParam%name
                        EXIT parentSearch1
                      ENDIF
                    ENDDO

                    !Special case for when the current parameter is the
                    !last parameter in the list
                    IF(ASSOCIATED(pp%pList(ip)%pdat,tmpParam)) THEN
                      !Go up another level and update the search
                      tmpParam => parentParam
                      istp=INDEX(newAddr,'->',.TRUE.)-1
                      parentParam => NULL()
                      IF(istp > 0) THEN
                        newAddr=addrIn(1:istp)
                        CALL get_ParamType(thisParam,TRIM(newAddr),parentParam)
                      ENDIF
                    ENDIF
                  ENDSELECT
                ENDDO parentSearch1
              ENDIF
            CLASS DEFAULT
              !All other types
              IF(ASSOCIATED(tp%pdat)) THEN
                !Append the address and return the next parameter
                nextParam => tp%pdat
                tmpAddr=TRIM(addrIn)//'->'//nextParam%name
              ELSE
                !This was a leaf parameter, so move up one level in the list

                !Get the parent list
                istp=INDEX(addrIn,'->',.TRUE.)-1
                parentParam => NULL()
                IF(istp > 0) THEN
                  newAddr=addrIn(1:istp)
                  CALL get_ParamType(thisParam,TRIM(newAddr),parentParam)
                ENDIF

                !Search for the next parameter or parameter list
                parentSearch2: DO WHILE(ASSOCIATED(parentParam))

                  !Search the parent list
                  SELECTTYPE(pp => parentParam); TYPE IS(ParamType_List)
                    DO ip=1,SIZE(pp%pList)-1
                      IF(ASSOCIATED(pp%pList(ip)%pdat,tmpParam)) THEN
                        !Get the next parameter in the list
                        nextParam => pp%pList(ip+1)%pdat
                        tmpAddr=TRIM(newAddr)//'->'//nextParam%name
                        EXIT parentSearch2
                      ENDIF
                    ENDDO

                    !Special case for when the current parameter is the
                    !last parameter in the list
                    IF(ASSOCIATED(pp%pList(ip)%pdat,tmpParam)) THEN
                      !Go up another level and update the search
                      tmpParam => parentParam
                      istp=INDEX(newAddr,'->',.TRUE.)-1
                      parentParam => NULL()
                      IF(istp > 0) THEN
                        newAddr=addrIn(1:istp)
                        CALL get_ParamType(thisParam,TRIM(newAddr),parentParam)
                      ENDIF
                    ENDIF
                  ENDSELECT
                ENDDO parentSearch2
              ENDIF
          ENDSELECT
        ENDIF
      ELSE
        !No address is given so assume the client wants to start at the root
        IF(LEN_TRIM(thisParam%name) > 0) THEN
          tmpAddr=thisParam%name
          nextParam => thisParam
        ELSE
          IF(ASSOCIATED(thisParam%pdat)) THEN
            tmpAddr=thisParam%pdat%name
            nextParam => thisParam%pdat
          ENDIF
        ENDIF
      ENDIF
      addr=tmpAddr
      param => nextParam
    ENDSUBROUTINE getNextParam_ParamType
!
!-------------------------------------------------------------------------------
!> @brief Returns a pointer to a parameter whose name matches the given input
!>        name.
!> @param thisParam the parameter object to search for @c name
!> @param name the name to locate in the parameter object
!> @param param the pointer to the parameter object whose name matches @c name
!>
!> If the name cannot be matched then @c param is returned as null. The search
!> name can be any full or partial path to a parameter object name. If it is
!> a partial path then the first occurence of this name is returned.
!>
!> To indicate accessing a parameter in a sublist the symbol "->" is used. For
!> example "Some list -> some parameter". Name matching is @b not case sensitive
!> and names can have spaces and leading or trailing whitespace. Input names to
!> this procedure cannot be blank or "->somename" or
!> "firstname -> -> secondname".
!>
!> This routine is primarily used by all @c set and @c get routines of the
!> extended types.
!>
    RECURSIVE SUBROUTINE get_ParamType(thisParam,name,param)
      CHARACTER(LEN=*),PARAMETER :: myName='get_ParamType'
      CLASS(ParamType),TARGET,INTENT(IN) :: thisParam
      CHARACTER(LEN=*),INTENT(IN) :: name
      CLASS(ParamType),POINTER,INTENT(INOUT) :: param
      CHARACTER(LEN=LEN(name)) :: thisname,nextname,pname
      INTEGER(SIK) :: ipos,i
      CLASS(ParamType),POINTER :: tmpParam
      LOGICAL(SBK),SAVE :: partial_match=.TRUE.

      ipos=INDEX(name,'->')
      thisname=name
      nextname=''
      IF(ipos > 0) THEN
        thisname=ADJUSTL(name(1:ipos-1))
        nextname=ADJUSTL(name(ipos+2:LEN(name)))
      ENDIF
      pname=''

      param => NULL()
      IF(LEN_TRIM(thisname) > 0) THEN
        SELECTTYPE(thisParam)
          TYPE IS(ParamType_List)
            CALL toUPPER(thisname)
            IF(LEN_TRIM(nextname) > 0) THEN
              !Set names to upper case for matching
              IF(LEN(pname) >= LEN_TRIM(thisParam%name)) pname=thisParam%name
              CALL toUPPER(pname)

              !Search the list for nextname (thisname must match parameter name)
              IF(TRIM(pname) == TRIM(thisname) .AND. &
                ALLOCATED(thisParam%pList)) THEN
                DO i=1,SIZE(thisParam%pList)
                  !CALL thisParam%pList(i)%getParam(TRIM(nextname),param)
                  IF(ASSOCIATED(thisParam%pList(i)%pdat)) &
                    CALL get_ParamType(thisParam%pList(i)%pdat, &
                      TRIM(nextname),param)
                  IF(ASSOCIATED(param)) EXIT !Found it, stop searching
                ENDDO
              ENDIF
            ELSE
              !End of search list, check search name against list name
              IF(LEN(pname) >= LEN_TRIM(thisParam%name)) &
                pname=thisParam%name
              CALL toUPPER(pname)
              IF(TRIM(pname) == TRIM(thisname)) THEN
                !Search name is thisParam's name
                param => thisParam
              ELSE
                !Search for thisname within the list
                IF(ALLOCATED(thisParam%pList) .AND. partial_match) THEN
                  DO i=1,SIZE(thisParam%pList)
                    IF(ASSOCIATED(thisParam%pList(i)%pdat)) &
                      CALL get_ParamType(thisParam%pList(i)%pdat, &
                        TRIM(thisname),param)
                    IF(ASSOCIATED(param)) EXIT !Found it, stop searching
                  ENDDO
                ENDIF
              ENDIF
            ENDIF
          CLASS DEFAULT
            CALL toUPPER(thisname)
            IF(ASSOCIATED(thisParam%pdat)) THEN
              !Set names to upper case for matching
              IF(LEN(pname) >= LEN_TRIM(thisParam%pdat%name)) &
                pname=thisParam%pdat%name
              CALL toUPPER(pname)
              
              IF(TRIM(pname) == TRIM(thisname)) THEN
                !Found the match
                tmpParam => thisParam%pdat
                IF(LEN_TRIM(nextname) > 0) THEN
                  !Set partial matching to off
                  partial_match=.FALSE.
                  CALL get_ParamType(tmpParam,name,param)
                  partial_match=.TRUE.
                ELSE
                  param => tmpParam
                  NULLIFY(tmpParam)
                ENDIF
              ELSE
                !Search 1-level down
                CALL thisParam%pdat%getParam(thisname,param)
                IF(ASSOCIATED(param) .AND. LEN_TRIM(nextname) > 0) THEN
                  tmpParam => param
                  param => NULL()
                  CALL get_ParamType(tmpParam,name,param)
                ENDIF
              ENDIF
            ELSE
              IF(LEN(pname) >= LEN_TRIM(thisParam%name)) pname=thisParam%name
              CALL toUPPER(pname)
              IF(TRIM(pname) == TRIM(thisname)) param => thisParam
            ENDIF
        ENDSELECT
      ELSE
        CALL eParams%raiseError(modName//'::'//myName// &
          ' - cannot search for a blank name!')
      ENDIF
    ENDSUBROUTINE get_ParamType
!
!-------------------------------------------------------------------------------
!> @brief Adds a new parameter to a parameter object
!> @param thisParam the parameter object to add a parameter to
!> @param name the name of the parameter list within @c thisParam to put the new
!>        parameter
!> @param newParam the new parameter to add to thisParam
!>
!> If @c name contains directions to sublists that do not exist these parameter
!> lists will automatically be created. Full paths should be used with this
!> routine to avoid unintentional matching or partial paths of parameters that
!> may have the same names in different lists. When accessing sublists names can
!> be repeated, so long as the full path is still unique. If @c name matches a
!> parameter name that is not a list it will produce an error. When @c name is
!> blank it is added to the list at the current level.
!>
!> This routine is primarily used by all the @c add routines of the extended
!> types.
!>
    RECURSIVE SUBROUTINE add_ParamType(thisParam,name,newParam)
      CHARACTER(LEN=*),PARAMETER :: myName='add_ParamType'
      CLASS(ParamType),INTENT(INOUT) :: thisParam
      CHARACTER(LEN=*),INTENT(IN) :: name
      CLASS(ParamType),INTENT(IN) :: newParam
      LOGICAL(SBK),SAVE :: lsubListSearch=.TRUE.
      CHARACTER(LEN=LEN(name)) :: thisname,nextname,pname,listName
      INTEGER(SIK) :: ipos,i,np
      TYPE(ParamType),ALLOCATABLE :: tmpList(:)
      CLASS(ParamType),POINTER :: tmpParam

      SELECTTYPE(thisParam)
        TYPE IS(ParamType)
          IF(ASSOCIATED(thisParam%pdat)) THEN
            CALL add_ParamType(thisParam%pdat,name,newParam)
          ELSE
            !thisParam is not initialized
            IF(LEN_TRIM(name) > 0) THEN
              !Create a new list on thisParam
              ALLOCATE(ParamType_List :: thisParam%pdat)
              thisParam%pdat%datatype='TYPE(ParamType_List)'

              !Determine the name for the list and the next name
              ipos=INDEX(name,'->')
              IF(ipos > 0) THEN
                thisParam%pdat%name=TRIM(ADJUSTL(name(1:ipos-1)))
                nextname=ADJUSTL(name(ipos+2:LEN(name)))
              ELSE
                thisParam%pdat%name=TRIM(ADJUSTL(name))
                nextname=''
              ENDIF
              CALL add_ParamType(thisParam%pdat,nextname,newParam)
            ELSE
              !assign newParam to thisParam
              CALL assign_ParamType(thisParam,newParam)
            ENDIF
          ENDIF
        TYPE IS(ParamType_List)
          np=0
          IF(ALLOCATED(thisParam%pList)) np=SIZE(thisParam%pList)
          IF(LEN_TRIM(name) > 0) THEN
            !Check if the name matches this list
            ipos=INDEX(name,'->')
            IF(ipos > 0) THEN
              thisname=TRIM(ADJUSTL(name(1:ipos-1)))
              nextname=ADJUSTL(name(ipos+2:LEN(name)))
            ELSE
              thisname=TRIM(ADJUSTL(name))
              nextname=''
            ENDIF

            pname=thisParam%name
            CALL toUPPER(thisname)
            CALL toUPPER(pname)
            IF(TRIM(pname) == TRIM(thisname)) THEN
              !only search if it's not the last name in the
              !full address. last name is guaranteed not to exist
              !and this prevents accidental partial matching in sublists.
              lsubListSearch=.FALSE.
              CALL add_ParamType(thisParam,nextname,newParam)
              lsubListSearch=.TRUE.
            ELSE
              !Search for thisname within...
              NULLIFY(tmpParam)
              IF(lsubListSearch) THEN
                !...all sub-entries.
                CALL get_ParamType(thisParam,TRIM(thisname),tmpParam)
              ELSE
                !...just this list
                DO i=1,np
                  listName=''
                  IF(ASSOCIATED(thisParam%pList(i)%pdat)) &
                    listName=TRIM(thisParam%pList(i)%pdat%name)
                  CALL toUPPER(listName)
                  IF(TRIM(listName) == TRIM(thisName)) THEN
                    tmpParam => thisParam%pList(i)%pdat
                    EXIT
                  ENDIF
                ENDDO
              ENDIF

              IF(ASSOCIATED(tmpParam)) THEN
                !Found parameter with matching name
                CALL add_ParamType(tmpParam,nextname,newParam)
              ELSE
                !Create a new entry in the list for the new parameter
                IF(np > 0) THEN
                  !Copy the parameter list to a temporary
                  ALLOCATE(tmpList(np))
                  DO i=1,np
                    CALL assign_ParamType(tmpList(i),thisParam%pList(i))
                    CALL clear_ParamType(thisParam%pList(i))
                  ENDDO

                  !Reallocate the parameter list and copy everything back
                  DEALLOCATE(thisParam%pList)
                  ALLOCATE(thisParam%pList(np+1))
                  DO i=1,np
                    CALL assign_ParamType(thisParam%pList(i),tmpList(i))
                    CALL clear_ParamType(tmpList(i))
                  ENDDO
                  DEALLOCATE(tmpList)
                  i=np+1
                ELSE
                  !Allocate the list to 1 element
                  ALLOCATE(thisParam%pList(1))
                  i=1
                ENDIF

                !Make recursive call to add the parameter in the new empty parameter
                CALL add_ParamType(thisParam%pList(i),name,newParam)
              ENDIF
            ENDIF
          ELSE
            !Create a new entry in the list for the new parameter
            IF(np > 0) THEN

              !Search within the list to avoid duplicates.
              IF(LEN_TRIM(newParam%name) == 0 .AND. ASSOCIATED(newParam%pdat)) THEN
                thisname=newParam%pdat%name
              ELSE
                thisname=newParam%name
              ENDIF
              CALL toUPPER(thisName)
              NULLIFY(tmpParam)
              DO i=1,np
                listName=''
                IF(ASSOCIATED(thisParam%pList(i)%pdat)) &
                  listName=TRIM(thisParam%pList(i)%pdat%name)
                CALL toUPPER(listName)
                IF(TRIM(listName) == TRIM(thisName)) THEN
                  tmpParam => thisParam%pList(i)%pdat
                  EXIT
                ENDIF
              ENDDO
              
              IF(.NOT.ASSOCIATED(tmpParam)) THEN
                !Copy the parameter list to a temporary
                ALLOCATE(tmpList(np))
                DO i=1,np
                  CALL assign_ParamType(tmpList(i),thisParam%pList(i))
                  CALL thisParam%pList(i)%clear()
                ENDDO

                !Reallocate the parameter list and copy everything back
                DEALLOCATE(thisParam%pList)
                ALLOCATE(thisParam%pList(np+1))
                DO i=1,np
                  CALL assign_ParamType(thisParam%pList(i),tmpList(i))
                  CALL tmpList(i)%clear()
                ENDDO
                DEALLOCATE(tmpList)
                i=np+1
                CALL add_ParamType(thisParam%pList(i),name,newParam)
              ELSE
                CALL eParams%raiseError(modName//'::'//myName// &
                  ' - parameter name "'//TRIM(thisname)// &
                  '" already exists! Use set method!')
              ENDIF
            ELSE
              !Allocate the list to 1 element
              ALLOCATE(thisParam%pList(1))
              CALL add_ParamType(thisParam%pList(1),name,newParam)
            ENDIF
          ENDIF
        CLASS DEFAULT
          CALL eParams%raiseError(modName//'::'//myName// &
            ' - cannot add parameter to type "'//thisParam%datatype//'"!')
      ENDSELECT
    ENDSUBROUTINE add_ParamType
!
!-------------------------------------------------------------------------------
!> @brief Removes a parameter with a given name from a parameter object.
!> @param thisParam the host parameter to remove the parameter (whose name
!>        matches @c name) from
!> @param name the name of the parameter to be removed from @c thisParam
!>
!> If @c name cannot be matched then nothing is removed. The @c name cannot
!> be blank or contain entries like "->somename" or
!> "firstname -> -> secondname". If partial paths are used for @c name then
!> the first parameter encountered with a matching name is removed.
!>
    RECURSIVE SUBROUTINE remove_ParamType(thisParam,name)
      CHARACTER(LEN=*),PARAMETER :: myName='remove_ParamType'
      CLASS(ParamType),INTENT(INOUT) :: thisParam
      CHARACTER(LEN=*),INTENT(IN) :: name
      CHARACTER(LEN=LEN(name)) :: thisname,nextname,pname
      INTEGER(SIK) :: i,ipos,np,npnew
      TYPE(ParamType),ALLOCATABLE :: tmpList(:)

      ipos=INDEX(name,'->')
      thisname=name
      nextname=''
      IF(ipos > 0) THEN
        thisname=ADJUSTL(name(1:ipos-1))
        nextname=ADJUSTL(name(ipos+2:LEN(name)))
      ENDIF
      pname=''

      IF(LEN_TRIM(thisname) > 0) THEN
        SELECTTYPE(thisParam)
          TYPE IS(ParamType_List)
            IF(LEN_TRIM(nextname) > 0) THEN
              !Set names to upper case for matching
              IF(LEN(pname) >= LEN_TRIM(thisParam%name)) pname=thisParam%name
              CALL toUPPER(pname)
              CALL toUPPER(thisname)

              !Search the list for nextname (thisname must match parameter name)
              IF(TRIM(pname) == TRIM(thisname)) THEN
                IF(ALLOCATED(thisParam%pList)) THEN
                  DO i=1,SIZE(thisParam%pList)
                    !Try to remove the next name
                    IF(ASSOCIATED(thisParam%pList(i)%pdat)) THEN
                      CALL remove_ParamType(thisParam%pList(i),TRIM(nextname))
                      IF(.NOT.ASSOCIATED(thisParam%pList(i)%pdat)) EXIT !success
                    ENDIF
                  ENDDO
                ENDIF
              ELSE
                !Try another level down, this is not so efficient because
                !there is no way to tell in which element the name might've
                !been matched.
                IF(ALLOCATED(thisParam%pList)) THEN
                  DO i=1,SIZE(thisParam%pList)
                    IF(ASSOCIATED(thisParam%pList(i)%pdat)) THEN
                      SELECTTYPE(p=>thisParam%pList(i)%pdat)
                         TYPE IS(ParamType_List); CALL remove_ParamType(p,name)
                      ENDSELECT
                    ENDIF
                  ENDDO
                ENDIF
              ENDIF
            ELSE
              !Search for thisname within the list
              IF(ALLOCATED(thisParam%pList)) THEN
                DO i=1,SIZE(thisParam%pList)
                  !Try to remove the next name
                  IF(ASSOCIATED(thisParam%pList(i)%pdat)) THEN
                    CALL remove_ParamType(thisParam%pList(i),TRIM(thisname))
                    IF(.NOT.ASSOCIATED(thisParam%pList(i)%pdat)) EXIT !success
                  ENDIF
                ENDDO
              ENDIF
            ENDIF

            !Garbage collection, shrink the current list to remove
            !empty values
            IF(ALLOCATED(thisParam%pList)) THEN

              !Create temporary
              np=SIZE(thisParam%pList)
              ALLOCATE(tmpList(np))

              !Copy to temporary
              npnew=0
              DO i=1,np
                IF(ASSOCIATED(thisParam%pList(i)%pdat)) THEN
                  npnew=npnew+1
                  CALL assign_ParamType(tmpList(npnew),thisParam%pList(i))
                  CALL thisParam%pList(i)%clear()
                ENDIF
              ENDDO

              !Reallocate list
              DEALLOCATE(thisParam%pList)
              IF(npnew > 0) THEN
                ALLOCATE(thisParam%pList(npnew))

                !Copy non-empty values back to list
                DO i=1,npnew
                  CALL assign_ParamType(thisParam%pList(i),tmpList(i))
                  CALL tmpList(i)%clear()
                ENDDO
              ENDIF
              DEALLOCATE(tmpList)
            ENDIF
          CLASS DEFAULT
            IF(ASSOCIATED(thisParam%pdat)) THEN
              !Set names to upper case for matching
              IF(LEN(pname) >= LEN_TRIM(thisParam%pdat%name)) &
                pname=thisParam%pdat%name
              CALL toUPPER(pname)
              CALL toUPPER(thisname)
              IF(TRIM(pname) == TRIM(thisname)) THEN
                IF(LEN_TRIM(nextname) > 0) THEN
                  CALL remove_ParamType(thisParam%pdat,name)
                ELSE
                  CALL thisParam%clear()
                ENDIF
              ELSE
                !Search 1-level down
                CALL remove_ParamType(thisParam%pdat,name)
              ENDIF
            ENDIF
        ENDSELECT
      ELSE
        CALL eParams%raiseError(modName//'::'//myName// &
          ' - cannot search for a blank name!')
      ENDIF
    ENDSUBROUTINE remove_ParamType
!
!-------------------------------------------------------------------------------
!> @brief Determines if a parameter with a given name exists in a parameter object.
!> @param thisParam the host parameter to search for the parameter (whose name
!>        matches @c name) from
!> @param name the name of the parameter to be searched for in @c thisParam
!> @param hasname the logical which returns if @c name is present
!>
!> If @c name cannot be matched then FALSE is returned. The @c name cannot
!> be blank or contain entries like "->somename" or
!> "firstname -> -> secondname".
!>
    FUNCTION has_ParamType(thisParam,name) RESULT(hasname)
      CHARACTER(LEN=*),PARAMETER :: myName='has_ParamType'
      CLASS(ParamType),TARGET,INTENT(IN) :: thisParam
      CHARACTER(LEN=*),INTENT(IN) :: name
      LOGICAL(SBK) :: hasname
      CHARACTER(LEN=LEN(name)) :: tmpname
      INTEGER(SIK) :: ipos
      TYPE(ParamType) :: listContainer
      CLASS(ParamType),POINTER :: tmpParam => NULL()

      hasname=.FALSE.
      tmpname=name
      ipos=INDEX(tmpname,'->')
      DO WHILE (ipos > 0)
        IF((ipos == 1) .OR. (ipos == LEN_TRIM(tmpname)-1)) THEN
          CALL eParams%raiseError(modName//'::'//myName// &
            ' - cannot search for a blank name!')
          RETURN
        ENDIF
        tmpname=ADJUSTL(tmpname(ipos+2:LEN(tmpname)))
        ipos=INDEX(tmpname,'->')
      ENDDO

      !Search for the parameter name
      SELECTTYPE(thisParam)
        TYPE IS(ParamType_List)
          listContainer%pdat => thisParam
          CALL get_ParamType(listContainer,name,tmpParam)
        CLASS DEFAULT
          CALL get_ParamType(thisParam,name,tmpParam)
      ENDSELECT
      hasname=ASSOCIATED(tmpParam)

      tmpParam => NULL()
    ENDFUNCTION has_ParamType
!
!-------------------------------------------------------------------------------
!> @brief Edits the information of a parameter
!> @param thisParam the parameter to edit
!> @param funit the unit number to edit the parameter to
!> @param indent optional indicates the number of blank spaces to precede the
!>        beginning of text to edit.
!>
!> This routine is basically a wrapper routine to call the @c edit method on
!> the parameter value which is overriden by another edit routine defined within
!> this module.
!>
    RECURSIVE SUBROUTINE edit_ParamType(thisParam,funit,indent)
      CLASS(ParamType),INTENT(IN) :: thisParam
      INTEGER(SIK),INTENT(IN) :: funit
      INTEGER(SIK),INTENT(IN),OPTIONAL :: indent
      INTEGER(SIK) :: i

      i=3
      IF(PRESENT(indent)) i=i+indent
      IF(ASSOCIATED(thisParam%pdat)) &
        CALL thisParam%pdat%edit(funit,i)
    ENDSUBROUTINE edit_ParamType
!
!-------------------------------------------------------------------------------
!> @brief Clears a parameter object and all its sub-objects
!> @param thisParam the parameter to clear
!>
!> This routine is basically a wrapper routine to call the @c edit method on
!> the parameter value which is overriden by another edit routine defined within
!> this module.
!>
    RECURSIVE SUBROUTINE clear_ParamType(thisParam)
      CLASS(ParamType),INTENT(INOUT) :: thisParam
      IF(ASSOCIATED(thisParam%pdat)) THEN
        CALL thisParam%pdat%clear()
        DEALLOCATE(thisParam%pdat)
      ENDIF
      thisParam%name=''
      thisParam%dataType=''
      thisParam%description=''
    ENDSUBROUTINE clear_ParamType
!
!-------------------------------------------------------------------------------
!> @brief Searches a parameter (thisParam) for a set of required parameters
!> (reqParams) and determines if all the required parameters are present and
!> of the correct type.
!> @param thisParam the parameter to validate against reqParams
!> @param reqParams the set of required parameters that must appear in
!>        @c thisParam
!> @param prefix a prefix path for the parameter's full path name
!> @returns isValid logical indicating that all the required parameters exist
!>          in @c thisParam and are of the correct type.
!>
    RECURSIVE FUNCTION validateReq_ParamType(thisParam,reqParams,prefix,isMatch) &
      RESULT(isValid)
      CHARACTER(LEN=*),PARAMETER :: myName='validateReq_ParamType'
      CLASS(ParamType),INTENT(INOUT) :: thisParam
      CLASS(ParamType),INTENT(IN) :: reqParams
      CHARACTER(LEN=*),INTENT(IN) :: prefix
      LOGICAL(SBK),INTENT(OUT),OPTIONAL :: isMatch
      LOGICAL(SBK) :: isValid
      INTEGER(SIK) :: i,ntrue
      CLASS(ParamType),POINTER :: tmpParam

      isValid=.FALSE.
!
!Loop over all required params in reqParams and search thisParam for
!each parameter and check type
      SELECTTYPE(p=>reqParams)
        TYPE IS(ParamType)
          !Call validate on the required parameter's value
          IF(PRESENT(isMatch)) THEN
            IF(ASSOCIATED(p%pdat)) &
              isValid=validateReq_ParamType(thisParam,p%pdat,prefix,isMatch)
          ELSE
            IF(ASSOCIATED(p%pdat)) &
              isValid=validateReq_ParamType(thisParam,p%pdat,prefix)
          ENDIF
        TYPE IS(ParamType_List)
          !Loop over all parameters in the list and check each
          IF(ALLOCATED(p%pList)) THEN
            ntrue=0
            DO i=1,SIZE(p%pList)
              IF(PRESENT(isMatch)) THEN
                IF(validateReq_ParamType(thisParam,p%pList(i), &
                  prefix//p%name//'->',isMatch)) ntrue=ntrue+1
              ELSE
                IF(validateReq_ParamType(thisParam,p%pList(i), &
                  prefix//p%name//'->')) ntrue=ntrue+1
              ENDIF
            ENDDO
            IF(ntrue == SIZE(p%pList)) isValid=.TRUE.
          ELSE
            !The required list is not allocated, which means we do not
            !check any of it's possible subparameters, but we must at least
            !check that the list exists
            CALL thisParam%getParam(prefix//p%name,tmpParam)
            IF(.NOT.ASSOCIATED(tmpParam)) THEN
              CALL eParams%raiseError(modName//'::'//myName// &
                ' - Failed to locate required parameter "'//prefix// &
                  p%name//'"!')
            ELSE
              IF(SAME_TYPE_AS(tmpParam,p)) THEN
                isValid=.TRUE.
                IF(PRESENT(isMatch)) isMatch=match_ParamType(tmpParam,p,prefix)
              ELSE
                CALL eParams%raiseError(modName//'::'//myName// &
                  ' - Required parameter "'//prefix//p%name//'" has type "'// &
                    tmpParam%dataType//'" and must be type "'//p%dataType//'"!')
              ENDIF
            ENDIF
          ENDIF
        CLASS DEFAULT
          !This is a meaningful parameter so search thisParam for the
          !required parameter's name and check its type
          CALL thisParam%getParam(prefix//p%name,tmpParam)
          IF(.NOT.ASSOCIATED(tmpParam)) THEN
            CALL eParams%raiseError(modName//'::'//myName// &
              ' - Failed to locate required parameter "'//prefix//p%name//'"!')
          ELSE
            IF(SAME_TYPE_AS(tmpParam,p)) THEN
              isValid=.TRUE.
              IF(PRESENT(isMatch)) isMatch=match_ParamType(tmpParam,p,prefix)
            ELSE
              CALL eParams%raiseError(modName//'::'//myName// &
                ' - Required parameter "'//prefix//p%name//'" has type "'// &
                  tmpParam%dataType//'" and must be type "'//p%dataType//'"!')
            ENDIF
          ENDIF
      ENDSELECT
    ENDFUNCTION validateReq_ParamType
!
!-------------------------------------------------------------------------------
!> @brief Searches a parameter (thisParam) for a set of optional parameters
!> (optParams) when an optional parameter is present. It also checks the type.
!> If an optional parameter is not present or has the wrong type it is reset
!> with the default value.
!> @param thisParam the parameter to validate against reqParams
!> @param optParams the set of optional parameters that must appear in
!>        @c thisParam
!> @param prefix a prefix path for the parameter's full path name
!>
   RECURSIVE SUBROUTINE validateOpt_ParamType(thisParam,optParams,prefix)
      CHARACTER(LEN=*),PARAMETER :: myName='validateOpt_ParamType'
      CLASS(ParamType),INTENT(INOUT) :: thisParam
      CLASS(ParamType),INTENT(IN) :: optParams
      CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: prefix
      INTEGER(SIK) :: i,nprefix
      CLASS(ParamType),POINTER :: tmpParam

      nprefix=LEN(prefix)
      IF(nprefix > 1) THEN
        IF('->' == prefix(LEN(prefix)-1:LEN(prefix)) ) &
          nprefix=LEN(prefix)-2
      ENDIF
!
!Loop over all optional params in optParams and search thisParam for
!each parameter and check type
      SELECTTYPE(p=>optParams)
        TYPE IS(ParamType)
          !Call validate on the required parameter's value
          IF(ASSOCIATED(p%pdat)) &
            CALL validateOpt_Paramtype(thisParam,p%pdat,prefix)
        TYPE IS(ParamType_List)
          !Loop over all parameters in the list and check each
          IF(ALLOCATED(p%pList)) THEN
            DO i=1,SIZE(p%pList)
              CALL validateOpt_Paramtype(thisParam,p%pList(i), &
                prefix//p%name//'->')
            ENDDO
          ELSE
            !The optional list is not allocated, which means we do not
            !have any default values for it's possible subparameters, but we
            !must at least check that the list exists
            CALL thisParam%getParam(prefix//p%name,tmpParam)
            IF(.NOT.ASSOCIATED(tmpParam)) THEN
              CALL eParams%raiseDebug(modName//'::'//myName// &
                ' - Failed to locate optional parameter "'//prefix// &
                  p%name//'"! It is being added with no default value!')
              CALL add_ParamType(thisParam,prefix(1:nprefix),p)
            ELSE
              IF(.NOT.SAME_TYPE_AS(tmpParam,p)) THEN
                CALL eParams%raiseWarning(modName//'::'//myName// &
                  ' - Optional parameter "'//prefix//p%name//'" has type "'// &
                    tmpParam%dataType//'" and should be type "'//p%dataType// &
                      '"!  Since has no default value, it will remain unset!')
                CALL remove_ParamType(thisParam,prefix//p%name)
                CALL add_ParamType(thisParam,prefix(1:nprefix),p)
              ENDIF
            ENDIF
          ENDIF
        CLASS DEFAULT
          !This is a meaningful parameter so search thisParam for the
          !optional parameter's name and check its type
          CALL thisParam%getParam(prefix//p%name,tmpParam)
          IF(.NOT.ASSOCIATED(tmpParam)) THEN
            CALL eParams%raiseInformation(modName//'::'//myName// &
              ' - Failed to locate optional parameter "'//prefix//p%name//'"!'// &
                'It is being added with default value.')
            CALL add_ParamType(thisParam,prefix(1:nprefix),p)
          ELSE
            IF(.NOT.SAME_TYPE_AS(tmpParam,p)) THEN
              CALL eParams%raiseWarning(modName//'::'//myName// &
                ' - Optional parameter "'//prefix//p%name//'" has type "'// &
                  tmpParam%dataType//'" and should be type "'//p%dataType// &
                    '"!  It is being overriden with default value.')
              CALL remove_ParamType(thisParam,prefix//p%name)
              CALL add_ParamType(thisParam,prefix(1:nprefix),p)
            ENDIF
          ENDIF
      ENDSELECT
   ENDSUBROUTINE validateOpt_ParamType
!
!-------------------------------------------------------------------------------
!> @brief Compares a parameter list to another parameter list and reports any
!> extra parameters that are in the first list and not the second list or the
!> third (optional) list.
!> @param thisParam the parameter in which to check for extra parameters
!> @param reqParams the set of required parameters that must appear in
!>        @c thisParam
!> @param optParams the set of optional parameters that must appear in
!>        @c thisParam
!> @param prefix a prefix path for the parameter's full path name
!>
    RECURSIVE SUBROUTINE checkExtras_Paramtype(thisParam,reqParams,optParams,prefix)
      CHARACTER(LEN=*),PARAMETER :: myName='checkExtras_Paramtype'
      CLASS(ParamType),INTENT(INOUT) :: thisParam
      CLASS(ParamType),INTENT(IN) :: reqParams
      CLASS(ParamType),INTENT(IN) :: optParams
      CHARACTER(LEN=*),INTENT(IN) :: prefix
      INTEGER(SIK) :: i
      CLASS(ParamType),POINTER :: tmpParam

      i=0
      SELECTTYPE(p=>thisParam)
        TYPE IS(ParamType)
          !Call check on the thisParam's value
          IF(ASSOCIATED(p%pdat)) &
            CALL checkExtras_Paramtype(p%pdat,reqParams,optParams,prefix)
        TYPE IS(ParamType_List)
          !Check that the list exists in reqParams
          CALL reqParams%getParam(prefix//p%name,tmpParam)
          IF(ASSOCIATED(tmpParam)) THEN
            SELECTTYPE(tmpParam); TYPE IS(ParamType_List)
              IF(ALLOCATED(tmpParam%pList)) THEN
                !The list in reqParams is allocated so check if the
                !subparameters in this list are extraneous
                IF(ALLOCATED(p%pList)) THEN
                  DO i=1,SIZE(p%pList)
                    CALL checkExtras_Paramtype(p%pList(i),reqParams, &
                      optParams,prefix//p%name//'->')
                  ENDDO
                ENDIF
              ENDIF
            ENDSELECT
          ELSE
            !Check the optional list
            CALL optParams%get(prefix//p%name,tmpParam)
            IF(ASSOCIATED(tmpParam)) THEN
              SELECTTYPE(tmpParam); TYPE IS(ParamType_List)
                IF(ALLOCATED(tmpParam%pList)) THEN
                  !The list in optParams is allocated so check if the
                  !subparameters in this list are extraneous
                  IF(ALLOCATED(p%pList)) THEN
                    DO i=1,SIZE(p%pList)
                      CALL checkExtras_Paramtype(p%pList(i),reqParams, &
                        optParams,prefix//p%name//'->')
                    ENDDO
                  ENDIF
                ENDIF
              ENDSELECT
            ELSE
              CALL eParams%raiseInformation(modName//'::'//myName// &
                ' - Possible extraneous parameter "'//prefix//p%name// &
                  '" is not present in the reference list!')
            ENDIF
          ENDIF
        CLASS DEFAULT
          !This is a meaningful parameter so search reqParams and optParams for
          !the parameter's name and warn if it is not present
          CALL reqParams%getParam(prefix//p%name,tmpParam)
          IF(.NOT.ASSOCIATED(tmpParam)) &
            CALL optParams%get(prefix//p%name,tmpParam)
          IF(.NOT.ASSOCIATED(tmpParam)) THEN
            CALL eParams%raiseInformation(modName//'::'//myName// &
              ' - Possible extraneous parameter "'//prefix//p%name// &
                '" is not present in the reference list!')
          ENDIF
      ENDSELECT
    ENDSUBROUTINE checkExtras_Paramtype
!
!-------------------------------------------------------------------------------
!> @brief
!> @param thisParam
!> @param reqParams
!> @param optParams
!> @param printExtras
!>
    SUBROUTINE validate_Paramtype(thisParam,reqParams,optParams,printExtras)
      CLASS(ParamType),INTENT(INOUT) :: thisParam
      CLASS(ParamType),INTENT(IN) :: reqParams
      CLASS(ParamType),INTENT(IN),OPTIONAL :: optParams
      LOGICAL(SBK),INTENT(IN),OPTIONAL :: printExtras
      LOGICAL(SBK) :: isValid
      TYPE(ParamType) :: nullParam

      !Assume the list is valid, check it only if the required parameter
      !list is not empty.
      isValid=.TRUE.
      IF(ASSOCIATED(reqParams%pdat)) &
        isValid=validateReq_ParamType(thisParam,reqParams,'')

      IF(isValid) THEN
        IF(PRESENT(optParams)) THEN
          CALL validateOpt_Paramtype(thisParam,optParams,'')
          !Logic to suppress excessive printing of parameter list information and warnings
          IF(PRESENT(printExtras)) THEN
            IF(printExtras) CALL checkExtras_Paramtype(thisParam,reqParams,optParams,'')
          ENDIF
        ELSE
          !Logic to suppress excessive printing of parameter list information and warnings
          IF(PRESENT(printExtras)) THEN
            IF(printExtras) CALL checkExtras_Paramtype(thisParam,reqParams,nullParam,'')
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE validate_Paramtype
!
!-------------------------------------------------------------------------------
!> @brief Verify should only be used in a unit test setting.  It is for checking
!>        the structure AND values in two parameter lists.  If they are a match,
!>        isMatch will be returned as true.  If not, false.  Assertion failures
!>        will be printed for the parameter list values that fail.
!> @param thisParam
!> @param reqParams
!> @param isMatch
!>
    SUBROUTINE verify_Paramtype(thisParam,reqParams,isMatch)
      CLASS(ParamType),INTENT(INOUT) :: thisParam
      CLASS(ParamType),INTENT(IN) :: reqParams
      LOGICAL(SBK),INTENT(OUT) :: isMatch
      LOGICAL(SBK) :: isValid

      !Assume the list is valid, check it only if the required parameter
      !list is not empty.
      isValid=.TRUE.
      isMatch=.FALSE.
      IF(ASSOCIATED(reqParams%pdat)) &
        isValid=validateReq_ParamType(thisParam,reqParams,'',isMatch)
    ENDSUBROUTINE verify_Paramtype
!
!-------------------------------------------------------------------------------
!> @brief This function assumes that thisParam and thatParam are of the same
!>        extended ParamType.  It also assumes that there is a "gettable" value
!>        that is of thisParam%name on the ParamType.  This function determines
!>        the extended type, then "gets" the appropriate parameter from both
!>        lists, then checks their equivalence.  If they are equal or
!>        approximately equal, the function results in true.  If not, false.
!>        The function also performs unit test harness assertions when checking
!>        the values.
!> @param thisParam  The parameter list being validated
!> @param thatParam  The parameter list being checked against
!> @param bool The logical result of the checked parameters.
!>
    FUNCTION match_ParamType(thisParam,thatParam,prefix) RESULT(bool)
      CLASS(ParamType),INTENT(INOUT) :: thisParam
      CLASS(ParamType),INTENT(IN),TARGET :: thatParam
      CHARACTER(LEN=*),INTENT(IN) :: prefix
      LOGICAL(SBK) :: bool
      CLASS(ParamType),POINTER :: paramPtr
      INTEGER(SIK) :: i,j
      LOGICAL(SBK) :: tmpsbk1,tmpsbk2
      LOGICAL(SBK),ALLOCATABLE :: tmpsbka11(:),tmpsbka12(:)
      REAL(SSK) :: tmpssk1,tmpssk2
      REAL(SSK),ALLOCATABLE :: tmpsska11(:),tmpsska21(:,:),tmpsska31(:,:,:)
      REAL(SSK),ALLOCATABLE :: tmpsska12(:),tmpsska22(:,:),tmpsska32(:,:,:)
      REAL(SDK) :: tmpsdk1,tmpsdk2
      REAL(SDK),ALLOCATABLE :: tmpsdka11(:),tmpsdka21(:,:),tmpsdka31(:,:,:)
      REAL(SDK),ALLOCATABLE :: tmpsdka12(:),tmpsdka22(:,:),tmpsdka32(:,:,:)
      INTEGER(SNK) :: tmpsnk1,tmpsnk2
      INTEGER(SNK),ALLOCATABLE :: tmpsnka11(:),tmpsnka21(:,:),tmpsnka31(:,:,:)
      INTEGER(SNK),ALLOCATABLE :: tmpsnka12(:),tmpsnka22(:,:),tmpsnka32(:,:,:)
      INTEGER(SLK) :: tmpslk1,tmpslk2
      INTEGER(SLK),ALLOCATABLE :: tmpslka11(:),tmpslka21(:,:),tmpslka31(:,:,:)
      INTEGER(SLK),ALLOCATABLE :: tmpslka12(:),tmpslka22(:,:),tmpslka32(:,:,:)
      TYPE(StringType) :: tmpstr1,tmpstr2
      TYPE(StringType),ALLOCATABLE :: tmpstra11(:),tmpstra21(:,:)
      TYPE(StringType),ALLOCATABLE :: tmpstra12(:),tmpstra22(:,:)

      !Point to the intent(in) param to use the get function
      paramPtr => NULL()
      bool=.FALSE.
      !Find the extended parameter type, then use the appropriate variable
      !and "get" the data to check.
      SELECTTYPE(paramPtr => thatParam)
        TYPE IS(ParamType_SSK)
          CALL thisParam%get(CHAR(thisParam%name),tmpssk1)
          CALL paramPtr%get(CHAR(paramPtr%name),tmpssk2)
          bool=(tmpssk1 .APPROXEQ. tmpssk2)
          ASSERT(bool, prefix//CHAR(thisParam%name))
          FINFO() 'test value=',tmpssk1
          FINFO() 'ref. value=',tmpssk2
        TYPE IS(ParamType_SDK)
          CALL thisParam%get(CHAR(thisParam%name),tmpsdk1)
          CALL paramPtr%get(CHAR(paramPtr%name),tmpsdk2)
          bool=(tmpsdk1 .APPROXEQ. tmpsdk2)
          IF(.NOT.bool) bool=SOFTEQ(tmpsdk1,tmpsdk2,EPSD*10._SRK)
          ASSERT(bool, prefix//CHAR(thisParam%name))
          FINFO() 'test value=',tmpsdk1
          FINFO() 'ref. value=',tmpsdk2
        TYPE IS(ParamType_SNK)
          CALL thisParam%get(CHAR(thisParam%name),tmpsnk1)
          CALL paramPtr%get(CHAR(paramPtr%name),tmpsnk2)
          bool=(tmpsnk1 == tmpsnk2)
          ASSERT(bool, prefix//CHAR(thisParam%name))
          FINFO() 'test value=',tmpsnk1
          FINFO() 'ref. value=',tmpsnk2
        TYPE IS(ParamType_SLK)
          CALL thisParam%get(CHAR(thisParam%name),tmpslk1)
          CALL paramPtr%get(CHAR(paramPtr%name),tmpslk2)
          bool=(tmpslk1 == tmpslk2)
          ASSERT(bool, prefix//CHAR(thisParam%name))
          FINFO() 'test value=',tmpslk1
          FINFO() 'ref. value=',tmpslk2
        TYPE IS(ParamType_SBK)
          CALL thisParam%get(CHAR(thisParam%name),tmpsbk1)
          CALL paramPtr%get(CHAR(paramPtr%name),tmpsbk2)
          bool=(tmpsbk1 .EQV. tmpsbk2)
          ASSERT(bool, prefix//CHAR(thisParam%name))
          FINFO() 'test value=',tmpsbk1
          FINFO() 'ref. value=',tmpsbk2
        TYPE IS(ParamType_STR)
          CALL thisParam%get(CHAR(thisParam%name),tmpstr1)
          CALL paramPtr%get(CHAR(paramPtr%name),tmpstr2)
          bool=(tmpstr1 == tmpstr2)
          ASSERT(bool, prefix//CHAR(thisParam%name))
          FINFO() 'test value=',CHAR(tmpstr1)
          FINFO() 'ref. value=',CHAR(tmpstr2)
        TYPE IS(ParamType_SSK_a1)
          CALL thisParam%get(CHAR(thisParam%name),tmpsska11)
          CALL paramPtr%get(CHAR(paramPtr%name),tmpsska12)
          bool=SIZE(tmpsska11,DIM=1) == SIZE(tmpsska12,DIM=1)
          ASSERT(bool, 'SIZE of '//prefix//CHAR(thisParam%name))
          FINFO() SIZE(tmpsska11,DIM=1), SIZE(tmpsska12,DIM=1)
          IF(bool) THEN
            bool=ALL(tmpsska11 .APPROXEQ. tmpsska12)
            ASSERT(bool, prefix//CHAR(thisParam%name))
            FINFO() 'test values=',tmpsska11
            FINFO() 'ref. values=',tmpsska12
          ENDIF
          DEALLOCATE(tmpsska11); DEALLOCATE(tmpsska12)
        TYPE IS(ParamType_SDK_a1)
          CALL thisParam%get(CHAR(thisParam%name),tmpsdka11)
          CALL paramPtr%get(CHAR(paramPtr%name),tmpsdka12)
          bool=SIZE(tmpsdka11,DIM=1) == SIZE(tmpsdka12,DIM=1)
          ASSERT(bool, 'SIZE of '//prefix//CHAR(thisParam%name))
          FINFO() SIZE(tmpsdka11,DIM=1), SIZE(tmpsdka12,DIM=1)
          IF(bool) THEN
            bool=ALL(tmpsdka11 .APPROXEQ. tmpsdka12)
            ASSERT(bool, prefix//CHAR(thisParam%name))
            FINFO() 'test values=',tmpsdka11
            FINFO() 'ref. values=',tmpsdka12
          ENDIF
          DEALLOCATE(tmpsdka11); DEALLOCATE(tmpsdka12)
        TYPE IS(ParamType_SNK_a1)
          CALL thisParam%get(CHAR(thisParam%name),tmpsnka11)
          CALL paramPtr%get(CHAR(paramPtr%name),tmpsnka12)
          bool=SIZE(tmpsnka11,DIM=1) == SIZE(tmpsnka12,DIM=1)
          ASSERT(bool, 'SIZE of '//prefix//CHAR(thisParam%name))
          FINFO() SIZE(tmpsnka11,DIM=1), SIZE(tmpsnka12,DIM=1)
          IF(bool) THEN
            bool=ALL(tmpsnka11 == tmpsnka12)
            ASSERT(bool, prefix//CHAR(thisParam%name))
            FINFO() 'test values=',tmpsnka11
            FINFO() 'ref. values=',tmpsnka12
          ENDIF
          DEALLOCATE(tmpsnka11); DEALLOCATE(tmpsnka12)
        TYPE IS(ParamType_SLK_a1)
          CALL thisParam%get(CHAR(thisParam%name),tmpslka11)
          CALL paramPtr%get(CHAR(paramPtr%name),tmpslka12)
          bool=SIZE(tmpslka11,DIM=1) == SIZE(tmpslka12,DIM=1)
          ASSERT(bool, 'SIZE of '//prefix//CHAR(thisParam%name))
          FINFO() SIZE(tmpslka11,DIM=1), SIZE(tmpslka12,DIM=1)
          IF(bool) THEN
            bool=ALL(tmpslka11 == tmpslka12)
            ASSERT(bool, prefix//CHAR(thisParam%name))
            FINFO() 'test values=',tmpslka11
            FINFO() 'ref. values=',tmpslka12
          ENDIF
          DEALLOCATE(tmpslka11); DEALLOCATE(tmpslka12)
        TYPE IS(ParamType_SBK_a1)
          CALL thisParam%get(CHAR(thisParam%name),tmpsbka11)
          CALL paramPtr%get(CHAR(paramPtr%name),tmpsbka12)
          bool=SIZE(tmpsbka11,DIM=1) == SIZE(tmpsbka12,DIM=1)
          ASSERT(bool, 'SIZE of '//prefix//CHAR(thisParam%name))
          FINFO() SIZE(tmpsbka11,DIM=1), SIZE(tmpsbka12,DIM=1)
          IF(bool) THEN
            bool=ALL(tmpsbka11 .EQV. tmpsbka12)
            ASSERT(bool, prefix//CHAR(thisParam%name))
            FINFO() 'test values=',tmpsbka11
            FINFO() 'ref. values=',tmpsbka12
          ENDIF
          DEALLOCATE(tmpsbka11); DEALLOCATE(tmpsbka12)
        TYPE IS(ParamType_STR_a1)
          CALL thisParam%get(CHAR(thisParam%name),tmpstra11)
          CALL paramPtr%get(CHAR(paramPtr%name),tmpstra12)
          bool=SIZE(tmpstra11,DIM=1) == SIZE(tmpstra12,DIM=1)
          ASSERT(bool, 'SIZE of '//prefix//CHAR(thisParam%name))
          FINFO() SIZE(tmpstra11,DIM=1), SIZE(tmpstra12,DIM=1)
          IF(bool) THEN
            DO i=1,SIZE(tmpstra11)
              bool=tmpstra11(i) == tmpstra12(i)
              ASSERT(bool, prefix//CHAR(thisParam%name))
              FINFO() 'test values=',CHAR(tmpstra11(i))
              FINFO() 'ref. values=',CHAR(tmpstra12(i))
              IF(.NOT. bool) EXIT
            ENDDO
            !clear?
           ENDIF
           DEALLOCATE(tmpstra11); DEALLOCATE(tmpstra12)
        TYPE IS(ParamType_SSK_a2)
          CALL thisParam%get(CHAR(thisParam%name),tmpsska21)
          CALL paramPtr%get(CHAR(paramPtr%name),tmpsska22)
          bool=SIZE(tmpsska21,DIM=1) == SIZE(tmpsska22,DIM=1)
          ASSERT(bool, 'SIZE DIM=1 of '//prefix//CHAR(thisParam%name))
          FINFO() SIZE(tmpsska21,DIM=1), SIZE(tmpsska22,DIM=1)
          IF(bool) THEN
            bool=SIZE(tmpsska21,DIM=2) == SIZE(tmpsska22,DIM=2)
            ASSERT(bool, 'SIZE DIM=2 of '//prefix//CHAR(thisParam%name))
            FINFO() SIZE(tmpsska21,DIM=2), SIZE(tmpsska22,DIM=2)
            IF(bool) THEN
              bool=ALL(tmpsska21 .APPROXEQ. tmpsska22)
              ASSERT(bool, prefix//CHAR(thisParam%name))
              FINFO() 'test values=',tmpsska21
              FINFO() 'ref. values=',tmpsska22
            ENDIF
          ENDIF
          DEALLOCATE(tmpsska21); DEALLOCATE(tmpsska22)
        TYPE IS(ParamType_SDK_a2)
          CALL thisParam%get(CHAR(thisParam%name),tmpsdka21)
          CALL paramPtr%get(CHAR(paramPtr%name),tmpsdka22)
          bool=SIZE(tmpsdka21,DIM=1) == SIZE(tmpsdka22,DIM=1)
          ASSERT(bool, 'SIZE DIM=1 of '//prefix//CHAR(thisParam%name))
          FINFO() SIZE(tmpsdka21,DIM=1), SIZE(tmpsdka22,DIM=1)
          IF(bool) THEN
            bool=SIZE(tmpsdka21,DIM=2) == SIZE(tmpsdka22,DIM=2)
            ASSERT(bool, 'SIZE DIM=2 of '//prefix//CHAR(thisParam%name))
            FINFO() SIZE(tmpsdka21,DIM=2), SIZE(tmpsdka22,DIM=2)
            IF(bool) THEN
              bool=ALL(tmpsdka21 .APPROXEQ. tmpsdka22)
              ASSERT(bool, prefix//CHAR(thisParam%name))
              FINFO() 'test values=',tmpsdka21
              FINFO() 'ref. values=',tmpsdka22
            ENDIF
          ENDIF
          DEALLOCATE(tmpsdka21); DEALLOCATE(tmpsdka22)
        TYPE IS(ParamType_SNK_a2)
          CALL thisParam%get(CHAR(thisParam%name),tmpsnka21)
          CALL paramPtr%get(CHAR(paramPtr%name),tmpsnka22)
          bool=SIZE(tmpsnka21,DIM=1) == SIZE(tmpsnka22,DIM=1)
          ASSERT(bool, 'SIZE DIM=1 of '//prefix//CHAR(thisParam%name))
          FINFO() SIZE(tmpsnka21,DIM=1), SIZE(tmpsnka22,DIM=1)
          IF(bool) THEN
            bool=SIZE(tmpsnka21,DIM=2) == SIZE(tmpsnka22,DIM=2)
            ASSERT(bool, 'SIZE DIM=2 of '//prefix//CHAR(thisParam%name))
            FINFO() SIZE(tmpsnka21,DIM=2), SIZE(tmpsnka22,DIM=2)
            IF(bool) THEN
              bool=ALL(tmpsnka21 == tmpsnka22)
              ASSERT(bool, prefix//CHAR(thisParam%name))
              FINFO() 'test values=',tmpsnka21
              FINFO() 'ref. values=',tmpsnka22
            ENDIF
          ENDIF
          DEALLOCATE(tmpsnka21); DEALLOCATE(tmpsnka22)
        TYPE IS(ParamType_SLK_a2)
          CALL thisParam%get(CHAR(thisParam%name),tmpslka21)
          CALL paramPtr%get(CHAR(paramPtr%name),tmpslka22)
          bool=SIZE(tmpslka21,DIM=1) == SIZE(tmpslka22,DIM=1)
          ASSERT(bool, 'SIZE DIM=1 of '//prefix//CHAR(thisParam%name))
          FINFO() SIZE(tmpslka21,DIM=1), SIZE(tmpslka22,DIM=1)
          IF(bool) THEN
            bool=SIZE(tmpslka21,DIM=2) == SIZE(tmpslka22,DIM=2)
            ASSERT(bool, 'SIZE DIM=2 of '//prefix//CHAR(thisParam%name))
            FINFO() SIZE(tmpslka21,DIM=2), SIZE(tmpslka22,DIM=2)
            IF(bool) THEN
              bool=ALL(tmpslka21 == tmpslka22)
              ASSERT(bool, prefix//CHAR(thisParam%name))
              FINFO() 'test values=',tmpslka21
              FINFO() 'ref. values=',tmpslka22
            ENDIF
          ENDIF
          DEALLOCATE(tmpslka21); DEALLOCATE(tmpslka22)
        TYPE IS(ParamType_STR_a2)
          CALL thisParam%get(CHAR(thisParam%name),tmpstra21)
          CALL paramPtr%get(CHAR(paramPtr%name),tmpstra22)
          bool=SIZE(tmpstra21,DIM=1) == SIZE(tmpstra22,DIM=1)
          ASSERT(bool, 'SIZE DIM=1 of '//prefix//CHAR(thisParam%name))
          FINFO() SIZE(tmpstra21,DIM=1), SIZE(tmpstra22,DIM=1)
          IF(bool) THEN
            bool=SIZE(tmpstra21,DIM=2) == SIZE(tmpstra22,DIM=2)
            ASSERT(bool, 'SIZE DIM=2 of '//prefix//CHAR(thisParam%name))
            FINFO() SIZE(tmpstra21,DIM=2), SIZE(tmpstra22,DIM=2)
            IF(bool) THEN
              outer : DO j=1,SIZE(tmpstra21,DIM=2)
                DO i=1,SIZE(tmpstra21,DIM=1)
                  bool=tmpstra21(i,j) == tmpstra22(i,j)
                  ASSERT(bool, prefix//CHAR(thisParam%name))
                  FINFO() 'test values=',CHAR(tmpstra21(i,j))
                  FINFO() 'ref. values=',CHAR(tmpstra22(i,j))
                  IF(.NOT.bool) EXIT outer
                ENDDO
              ENDDO outer
            ENDIF
          ENDIF
          !clear?
          DEALLOCATE(tmpstra21); DEALLOCATE(tmpstra22)
        TYPE IS(ParamType_SSK_a3)
          CALL thisParam%get(CHAR(thisParam%name),tmpsska31)
          CALL paramPtr%get(CHAR(paramPtr%name),tmpsska32)
          bool=SIZE(tmpsska31,DIM=1) == SIZE(tmpsska32,DIM=1)
          ASSERT(bool, 'SIZE DIM=1 of '//prefix//CHAR(thisParam%name))
          FINFO() SIZE(tmpsska31,DIM=1), SIZE(tmpsska32,DIM=1)
          IF(bool) THEN
            bool=SIZE(tmpsska31,DIM=2) == SIZE(tmpsska32,DIM=2)
            ASSERT(bool, 'SIZE DIM=2 of '//prefix//CHAR(thisParam%name))
            FINFO() SIZE(tmpsska31,DIM=2), SIZE(tmpsska32,DIM=2)
            IF(bool) THEN
              bool=SIZE(tmpsska31,DIM=3) == SIZE(tmpsska32,DIM=3)
              ASSERT(bool, 'SIZE DIM=3 of '//prefix//CHAR(thisParam%name))
              FINFO() SIZE(tmpsska31,DIM=3), SIZE(tmpsska32,DIM=3)
              IF(bool) THEN
                bool=ALL(tmpsska31 .APPROXEQ. tmpsska32)
                ASSERT(bool, prefix//CHAR(thisParam%name))
                FINFO() 'test values=',tmpsska31
                FINFO() 'ref. values=',tmpsska32
              ENDIF
            ENDIF
          ENDIF
          DEALLOCATE(tmpsska31); DEALLOCATE(tmpsska32)
        TYPE IS(ParamType_SDK_a3)
          CALL thisParam%get(CHAR(thisParam%name),tmpsdka31)
          CALL paramPtr%get(CHAR(paramPtr%name),tmpsdka32)
          bool=SIZE(tmpsdka31,DIM=1) == SIZE(tmpsdka32,DIM=1)
          ASSERT(bool, 'SIZE DIM=1 of '//prefix//CHAR(thisParam%name))
          FINFO() SIZE(tmpsdka31,DIM=1), SIZE(tmpsdka32,DIM=1)
          IF(bool) THEN
            bool=SIZE(tmpsdka31,DIM=2) == SIZE(tmpsdka32,DIM=2)
            ASSERT(bool, 'SIZE DIM=2 of '//prefix//CHAR(thisParam%name))
            FINFO() SIZE(tmpsdka31,DIM=2), SIZE(tmpsdka32,DIM=2)
            IF(bool) THEN
              bool=SIZE(tmpsdka31,DIM=3) == SIZE(tmpsdka32,DIM=3)
              ASSERT(bool, 'SIZE DIM=3 of '//prefix//CHAR(thisParam%name))
              FINFO() SIZE(tmpsdka31,DIM=3), SIZE(tmpsdka32,DIM=3)
              IF(bool) THEN
                bool=ALL(tmpsdka31 .APPROXEQ. tmpsdka32)
                ASSERT(bool, prefix//CHAR(thisParam%name))
                FINFO() 'test values=',tmpsdka31
                FINFO() 'ref. values=',tmpsdka32
              ENDIF
            ENDIF
          ENDIF
          DEALLOCATE(tmpsdka31); DEALLOCATE(tmpsdka32)
        TYPE IS(ParamType_SNK_a3)
          CALL thisParam%get(CHAR(thisParam%name),tmpsnka31)
          CALL paramPtr%get(CHAR(paramPtr%name),tmpsnka32)
          bool=SIZE(tmpsnka31,DIM=1) == SIZE(tmpsnka32,DIM=1)
          ASSERT(bool, 'SIZE DIM=1 of '//prefix//CHAR(thisParam%name))
          FINFO() SIZE(tmpsnka31,DIM=1), SIZE(tmpsnka32,DIM=1)
          IF(bool) THEN
            bool=SIZE(tmpsnka31,DIM=2) == SIZE(tmpsnka32,DIM=2)
            ASSERT(bool, 'SIZE DIM=2 of '//prefix//CHAR(thisParam%name))
            FINFO() SIZE(tmpsnka31,DIM=2), SIZE(tmpsnka32,DIM=2)
            IF(bool) THEN
              bool=SIZE(tmpsnka31,DIM=3) == SIZE(tmpsnka32,DIM=3)
              ASSERT(bool, 'SIZE DIM=3 of '//prefix//CHAR(thisParam%name))
              FINFO() SIZE(tmpsnka31,DIM=3), SIZE(tmpsnka32,DIM=3)
              IF(bool) THEN
                bool=ALL(tmpsnka31 == tmpsnka32)
                ASSERT(bool, prefix//CHAR(thisParam%name))
                FINFO() 'test values=',tmpsnka31
                FINFO() 'ref. values=',tmpsnka32
              ENDIF
            ENDIF
          ENDIF
          DEALLOCATE(tmpsnka31); DEALLOCATE(tmpsnka32)
        TYPE IS(ParamType_SLK_a3)
          CALL thisParam%get(CHAR(thisParam%name),tmpslka31)
          CALL paramPtr%get(CHAR(paramPtr%name),tmpslka32)
          bool=SIZE(tmpslka31,DIM=1) == SIZE(tmpslka32,DIM=1)
          ASSERT(bool, 'SIZE DIM=1 of '//prefix//CHAR(thisParam%name))
          FINFO() SIZE(tmpslka31,DIM=1), SIZE(tmpslka32,DIM=1)
          IF(bool) THEN
            bool=SIZE(tmpslka31,DIM=2) == SIZE(tmpslka32,DIM=2)
            ASSERT(bool, 'SIZE DIM=2 of '//prefix//CHAR(thisParam%name))
            FINFO() SIZE(tmpslka31,DIM=2), SIZE(tmpslka32,DIM=2)
            IF(bool) THEN
              bool=SIZE(tmpslka31,DIM=3) == SIZE(tmpslka32,DIM=3)
              ASSERT(bool, 'SIZE DIM=3 of '//prefix//CHAR(thisParam%name))
              FINFO() SIZE(tmpslka31,DIM=3), SIZE(tmpslka32,DIM=3)
              IF(bool) THEN
                bool=ALL(tmpslka31 == tmpslka32)
                ASSERT(bool, prefix//CHAR(thisParam%name))
                FINFO() 'test values=',tmpslka31
                FINFO() 'ref. values=',tmpslka32
              ENDIF
            ENDIF
          ENDIF
          DEALLOCATE(tmpslka31); DEALLOCATE(tmpslka32)
        TYPE IS(ParamType_List)
          bool=SAME_TYPE_AS(thisParam,paramPtr)
          ASSERT(bool,'ParamType_List for'//prefix//CHAR(thisParam%name))
          FINFO() 'test value is ParamType_List, while ref value is not.'
        CLASS DEFAULT
          CONTINUE
      ENDSELECT
    ENDFUNCTION match_ParamType
!
!-------------------------------------------------------------------------------
!> @brief Initializes a ParamType object as a parameter list
!> @param thisParam the parameter to initialize
!> @param name the name of the parameter
!> @param param an array or list of parameters
!> @param description an optional description for this parameter
!>
!> This routine is not recursive, so it is like setting a scalar parameter.
!> Therefore the name cannot contain the "->" symbol to indicate access to a
!> sub-list. @c thisParam must not already be inititalized.
!>
    SUBROUTINE init_ParamType_List(thisParam,name,param,description)
      CHARACTER(LEN=*),PARAMETER :: myName='init_ParamType_List'
      CLASS(ParamType),INTENT(INOUT) :: thisParam
      TYPE(ParamType),INTENT(IN) :: param(:)
      CHARACTER(LEN=*),INTENT(IN) :: name
      CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
      INTEGER(SIK) :: ipos,i

      IF(.NOT.ASSOCIATED(thisParam%pdat)) THEN
        !Check that '->' character is not in name
        ipos=INDEX(name,'->')
        IF(ipos == 0) THEN
          ALLOCATE(ParamType_List :: thisParam%pdat)
          thisParam%pdat%name=TRIM(name)
          thisParam%pdat%dataType='TYPE(ParamType_List)'
          IF(PRESENT(description)) thisParam%pdat%description=TRIM(description)
          SELECTTYPE(p=>thisParam%pdat); TYPE IS(ParamType_List)
            ALLOCATE(p%pList(SIZE(param)))
            DO i=1,SIZE(param)
              CALL assign_ParamType(p%pList(i),param(i))
            ENDDO
          ENDSELECT
        ELSE
          CALL eParams%raiseError(modName//'::'//myName// &
            ' - "->" symbol is not allowed in name!')
        ENDIF
      ELSE
        CALL eParams%raiseError(modName//'::'//myName// &
          ' - parameter '//thisParam%name//' is already initialized!'// &
            ' Use set method!')
      ENDIF
    ENDSUBROUTINE init_ParamType_List
!
!-------------------------------------------------------------------------------
!> @brief Edits a parameter list
!> @param thisParam the parameter list to edit
!> @param funit the unit number to edit the parameter to
!> @param indent optional indicates the number of blank spaces to precede the
!>        beginning of text to edit.
!>
!> This routine is recursive because it essentially calls the edit routine
!> on all parameters in it's list.
!>
    RECURSIVE SUBROUTINE edit_ParamType_List(thisParam,funit,indent)
      CLASS(ParamType_List),INTENT(IN) :: thisParam
      INTEGER(SIK),INTENT(IN) :: funit
      INTEGER(SIK),INTENT(IN),OPTIONAL :: indent
      CHARACTER(LEN=12) :: fmt
      INTEGER(SIK) :: i,j

      IF(LEN_TRIM(thisParam%name) > 0) THEN
        i=1
        IF(PRESENT(indent)) i=i+indent
        WRITE(fmt,'(i12)') i; fmt=ADJUSTL(fmt)
        IF(LEN_TRIM(thisParam%description) == 0) THEN
          WRITE(UNIT=funit,FMT='('//TRIM(fmt)//'x,a)') &
            thisParam%dataType//' :: '//thisParam%name//'='
        ELSE
          WRITE(UNIT=funit,FMT='('//TRIM(fmt)//'x,a)') thisParam%dataType// &
            ' :: '//thisParam%name//'= !'//thisParam%description
        ENDIF
      ENDIF
      IF(ALLOCATED(thisParam%pList)) THEN
        DO j=1,SIZE(thisParam%pList)
          IF(ASSOCIATED(thisParam%pList(j)%pdat)) &
            CALL thisParam%pList(j)%pdat%edit(funit,i+3)
        ENDDO
      ENDIF
    ENDSUBROUTINE edit_ParamType_List
!
!-------------------------------------------------------------------------------
!> @brief Clears a parameter list type
!> @param thisParam the parameter list object to clear
!>
!> This routine recursively clears all subparameters in this list.
!>
    SUBROUTINE clear_ParamType_List(thisParam)
      CLASS(ParamType_List),INTENT(INOUT) :: thisParam
      INTEGER(SIK) :: i
      thisParam%name=''
      thisParam%dataType=''
      thisParam%description=''
      IF(ALLOCATED(thisParam%pList)) THEN
        DO i=1,SIZE(thisParam%pList)
          CALL thisParam%pList(i)%clear()
        ENDDO
        DEALLOCATE(thisParam%pList)
      ENDIF
    ENDSUBROUTINE clear_ParamType_List
!
!-------------------------------------------------------------------------------
!> @brief Sets the value of an existing parameter list to a new value.
!> @param thisParam the parameter in which an existing parameter with name
!>        matching @c name will be to set the new value of @c paramlist
!> @param name the name of an existing parameter to set the value of
!> @param paramlist the new value to set for the parameter
!> @param description an optional new description for the parameter identified
!>        by @c name
!>
!> If a parameter with @c name is not found an error is produced. If the
!> parameter with @c name is not a parameter list then an error is produced.
!>
    SUBROUTINE set_ParamType_List(thisParam,name,paramlist,description)
      CHARACTER(LEN=*),PARAMETER :: myName='set_ParamType_List'
      CLASS(ParamType),INTENT(INOUT) :: thisParam
      TYPE(ParamType),INTENT(IN) :: paramlist(:)
      CHARACTER(LEN=*),INTENT(IN) :: name
      CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
      INTEGER(SIK) :: np,i
      CLASS(ParamType),POINTER :: tmpParam

      SELECTTYPE(thisParam)
        TYPE IS(ParamType_List)
          IF(thisParam%name == TRIM(name)) THEN
            IF(PRESENT(description)) thisParam%description=TRIM(description)

            IF(ALLOCATED(thisParam%pList)) THEN
              !Clear the existing list
              DO i=1,SIZE(thisParam%pList)
                CALL thisParam%pList(i)%clear()
              ENDDO
              DEALLOCATE(thisParam%pList)
            ENDIF

            !Assign the new list
            np=SIZE(paramlist)
            ALLOCATE(thisParam%pList(np))
            DO i=1,np
              CALL assign_ParamType(thisParam%pList(i),paramlist(i))
            ENDDO
          ELSE
            CALL eParams%raiseError(modName//'::'//myName// &
              ' - parameter name mismatch! Tried to set "'//TRIM(name)// &
                '" but name is "'//thisParam%name//'"!')
          ENDIF
        CLASS DEFAULT
          !Search for the parameter name
          CALL thisParam%getParam(name,tmpParam)
          IF(ASSOCIATED(tmpParam)) THEN
            !Parameter was found
            SELECTTYPE(p=>tmpParam)
              TYPE IS(ParamType_List)
                IF(PRESENT(description)) p%description=TRIM(description)
                IF(ALLOCATED(p%pList)) THEN
                  !Clear the existing list
                  DO i=1,SIZE(p%pList)
                    CALL p%pList(i)%clear()
                  ENDDO
                  DEALLOCATE(p%pList)
                ENDIF

                !Assign the new list
                np=SIZE(paramlist)
                ALLOCATE(p%pList(np))
                DO i=1,np
                  CALL assign_ParamType(p%pList(i),paramlist(i))
                ENDDO
              CLASS DEFAULT
                CALL eParams%raiseError(modName//'::'//myName// &
                  ' - parameter data type mismatch! Parameter '//TRIM(name)//' type is '// &
                    tmpParam%dataType//' and must be TYPE(ParamType_List)!')
            ENDSELECT
          ELSE
            CALL eParams%raiseError(modName//'::'//myName// &
              ' - unable to locate parameter "'//TRIM(name)//'" in "'// &
                thisParam%name//'"!')
          ENDIF
      ENDSELECT
    ENDSUBROUTINE set_ParamType_List
!
!-------------------------------------------------------------------------------
!> @brief Gets the array or list of parameters for a specified parameter name
!> @param thisParam the parameter in which an existing parameter with name
!>        matching @c name will have it's value returned
!> @param name the name of the parameter to return the value of
!> @param paramlist the current value of the parameter with @c name
!>
!> If a parameter with @c name is not found an error is produced. If the
!> parameter with @c name is not a parameter list then an error is produced.
!> If the length of @c paramlist is less then the size of the parameter list
!> of the parameter with name matching @c name, then not all values are returned
!> and warning is raised.
!>
    SUBROUTINE get_ParamType_List(thisParam,name,paramlist)
      CHARACTER(LEN=*),PARAMETER :: myName='get_ParamType_List'
      CLASS(ParamType),INTENT(IN) :: thisParam
      CHARACTER(LEN=*),INTENT(IN) :: name
      TYPE(ParamType),INTENT(INOUT) :: paramlist(:)
      INTEGER(SIK) :: i,np
      CLASS(ParamType),POINTER :: tmpParam

      SELECTTYPE(thisParam)
        TYPE IS(ParamType_List)
          IF(thisParam%name == TRIM(name)) THEN
            np=SIZE(thisParam%pList)
            IF(SIZE(paramlist) < np) THEN
              !List lengths are unequal so choose the lesser
              CALL eParams%raiseWarning(modName//'::'//myName// &
                ' - parameter list lengths are unequal! '// &
                  'All parameters may not be returned!')
              np=SIZE(paramlist)
            ENDIF
            DO i=1,np
              CALL assign_ParamType(paramlist(i),thisParam%pList(i))
            ENDDO
          ELSE
            CALL eParams%raiseError(modName//'::'//myName// &
              ' - parameter name mismatch "'//TRIM(name)//'" in "'// &
                thisParam%name//'"!')
          ENDIF
        CLASS DEFAULT
          !Search for the parameter name
          CALL thisParam%getParam(name,tmpParam)
          IF(ASSOCIATED(tmpParam)) THEN
            !Parameter was found
            SELECTTYPE(p=>tmpParam)
              TYPE IS(ParamType_List)
                np=SIZE(p%pList)
                IF(SIZE(paramlist) < np) THEN
                  !List lengths are unequal so choose the lesser
                  CALL eParams%raiseWarning(modName//'::'//myName// &
                    ' - parameter list lengths are unequal! '// &
                      'All parameters may not be returned!')
                  np=SIZE(paramlist)
                ENDIF
                DO i=1,np
                  CALL assign_ParamType(paramlist(i),p%pList(i))
                ENDDO
              CLASS DEFAULT
                CALL eParams%raiseError(modName//'::'//myName// &
                  ' - parameter data type mismatch! Parameter '//TRIM(name)//' type is '// &
                    tmpParam%dataType//' and must be TYPE(ParamType_List)!')
            ENDSELECT
          ELSE
            CALL eParams%raiseError(modName//'::'//myName// &
              ' - unable to locate parameter "'//TRIM(name)//'" in "'// &
                thisParam%name//'"!')
          ENDIF
      ENDSELECT
    ENDSUBROUTINE get_ParamType_List
!
!-------------------------------------------------------------------------------
!> @brief Adds a new parameter list to a set of parameters
!> @param thisParam the set of parameters to which a new parameter will be added
!> @param name the location and name of the new parameter
!> @param param the list of parameters that will be added
!> @param description an optional input for a description of the new parameter
!>
!> This routine creates a new parameter within @c thisParam with @c name.
!> @c name may contain a full or partial path to the new parameter. If @c name
!> can be matched to an existing parameter in @c thisParam an error is produced
!> If @c name contains a full path for which intermediate lists do not exist
!> then this lists are created in the process of adding the new parameter.
!> If @c thisParam is not initialized and @c name does not contain a "->"
!> symbol then this routine behaves equivalently to
!> @ref ParameterLists::init_ParamType_List "initParamList".
!>
    SUBROUTINE add_ParamType_List(thisParam,name,param,description)
      CHARACTER(LEN=*),PARAMETER :: myName='add_ParamType_List'
      CLASS(ParamType),INTENT(INOUT) :: thisParam
      CHARACTER(LEN=*),INTENT(IN) :: name
      TYPE(ParamType),INTENT(IN) :: param(:)
      CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
      CHARACTER(LEN=LEN(name)) :: prevname,thisname
      INTEGER(SIK) :: ipos
      TYPE(ParamType) :: newParam
      CLASS(ParamType),POINTER :: tmpParam

      !Search for the name to make sure it does not exist
      CALL get_ParamType(thisParam,name,tmpParam)

      IF(.NOT.ASSOCIATED(tmpParam)) THEN
        prevname=''
        thisname=ADJUSTL(name)
        ipos=INDEX(name,'->',.TRUE.)
        IF(ipos > 0) THEN
          prevname=ADJUSTL(name(1:ipos-1))
          thisname=ADJUSTL(name(ipos+2:LEN(name)))
        ENDIF

        !Initialize the new parameter
        IF(PRESENT(description)) THEN
          CALL init_ParamType_List(newParam,thisname,param,description)
        ELSE
          CALL init_ParamType_List(newParam,thisname,param)
        ENDIF

        !Add the new parameter to thisParam
        CALL add_ParamType(thisParam,prevname,newParam)
        CALL newParam%clear()
      ELSE
        CALL eParams%raiseError(modName//'::'//myName// &
          ' - parameter name "'//TRIM(name)// &
            '" already exists! Use set method or full parameter list path!')
      ENDIF
    ENDSUBROUTINE add_ParamType_List
!
!-------------------------------------------------------------------------------
!> @brief Initializes a ParamType object as a scalar single precision real
!> @param thisParam the parameter to initialize
!> @param name the name of the parameter
!> @param param a scalar single precision real
!> @param description an optional description for this parameter
!>
!> This routine is not recursive, so it is like setting a scalar parameter.
!> Therefore the name cannot contain the "->" symbol to indicate access to a
!> sub-list. @c thisParam must not already be inititalized.
!>
    SUBROUTINE init_ParamType_SSK(thisParam,name,param,description)
      CHARACTER(LEN=*),PARAMETER :: myName='init_ParamType_SSK'
      CLASS(ParamType),INTENT(INOUT) :: thisParam
      REAL(SSK),INTENT(IN) :: param
      CHARACTER(LEN=*),INTENT(IN) :: name
      CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
      INTEGER(SIK) :: ipos

      IF(.NOT.ASSOCIATED(thisParam%pdat)) THEN
        !Check that '->' character is not in name
        ipos=INDEX(name,'->')
        IF(ipos == 0) THEN
          ALLOCATE(ParamType_SSK :: thisParam%pdat)
          thisParam%pdat%name=TRIM(name)
          IF(PRESENT(description)) thisParam%pdat%description=TRIM(description)
          thisParam%pdat%dataType='REAL(SSK)'
          SELECTTYPE(p=>thisParam%pdat)
            TYPE IS(ParamType_SSK); p%val=param
          ENDSELECT
        ELSE
          CALL eParams%raiseError(modName//'::'//myName// &
            ' - "->" symbol is not allowed in name!')
        ENDIF
      ELSE
        CALL eParams%raiseError(modName//'::'//myName// &
          ' - parameter is already initialized! Use set method!')
      ENDIF
    ENDSUBROUTINE init_ParamType_SSK
!
!-------------------------------------------------------------------------------
!> @brief Edits a scalar single precision real valued parameter
!> @param thisParam the scalar single precision real valued parameter to edit
!> @param funit the unit number to edit the parameter to
!> @param indent optional indicates the number of blank spaces to precede the
!>        beginning of text to edit.
!>
!> The formatted write uses the "general" edit descriptor so that 7 digits (one
!> more than the significant number in a single precision real) are always
!> printed if the number is very large in absolute value engineering format
!> is used otherwise floating point form is used to write the value.
!>
    SUBROUTINE edit_ParamType_SSK(thisParam,funit,indent)
      CLASS(ParamType_SSK),INTENT(IN) :: thisParam
      INTEGER(SIK),INTENT(IN) :: funit
      INTEGER(SIK),INTENT(IN),OPTIONAL :: indent
      CHARACTER(LEN=12) :: fmt
      INTEGER(SIK) :: i

      i=1
      IF(PRESENT(indent)) i=i+indent
      WRITE(fmt,'(i12)') i; fmt=ADJUSTL(fmt)
      IF(LEN_TRIM(thisParam%description) == 0) THEN
        WRITE(UNIT=funit,FMT='('//TRIM(fmt)//'x,a,g13.7)') &
          thisParam%dataType//' :: '//thisParam%name//'=',thisParam%val
      ELSE
        WRITE(UNIT=funit,FMT='('//TRIM(fmt)//'x,a,g13.7,a)') &
          thisParam%dataType//' :: '//thisParam%name//'=',thisParam%val, &
            ' !'//thisParam%description
      ENDIF
    ENDSUBROUTINE edit_ParamType_SSK
!
!-------------------------------------------------------------------------------
!> @brief Clears a scalar single precision real valued parameter
!> @param thisParam the scalar single precision real valued parameter to clear
!>
    SUBROUTINE clear_ParamType_SSK(thisParam)
      CLASS(ParamType_SSK),INTENT(INOUT) :: thisParam
      thisParam%val=0.0_SSK
      thisParam%name=''
      thisParam%dataType=''
      thisParam%description=''
    ENDSUBROUTINE clear_ParamType_SSK
!
!-------------------------------------------------------------------------------
!> @brief Sets the value of an existing scalar single precision real valued
!> parameter to a new value.
!> @param thisParam the parameter in which an existing parameter with name
!>        matching @c name will be to set the new value of @c param
!> @param name the name of an existing parameter to set the value of
!> @param param the new value to set for the parameter
!> @param description an optional new description for the parameter identified
!>        by @c name
!>
!> If a parameter with @c name is not found an error is produced. If the
!> parameter with @c name is not a scalar single precision real valued parameter
!> then an error is produced.
!>
    SUBROUTINE set_ParamType_SSK(thisParam,name,param,description)
      CHARACTER(LEN=*),PARAMETER :: myName='set_ParamType_SSK'
      CLASS(ParamType),INTENT(INOUT) :: thisParam
      CHARACTER(LEN=*),INTENT(IN) :: name
      REAL(SSK),INTENT(IN) :: param
      CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
      CLASS(ParamType),POINTER :: tmpParam

      SELECTTYPE(thisParam)
        TYPE IS(ParamType_SSK)
          IF(thisParam%name == TRIM(name)) THEN
            thisParam%val=param
            IF(PRESENT(description)) thisParam%description=TRIM(description)
          ELSE
            CALL eParams%raiseError(modName//'::'//myName// &
              ' - parameter name mismatch! Tried to set "'//TRIM(name)// &
                '" but name is "'//thisParam%name//'"!')
          ENDIF
        CLASS DEFAULT
          !Search for the parameter name
          CALL thisParam%getParam(name,tmpParam)
          IF(ASSOCIATED(tmpParam)) THEN
            !Parameter was found
            SELECTTYPE(p=>tmpParam)
              TYPE IS(ParamType_SSK)
                p%val=param
                IF(PRESENT(description)) p%description=TRIM(description)
              CLASS DEFAULT
                CALL eParams%raiseError(modName//'::'//myName// &
                  ' - parameter data type mismatch! Parameter '//TRIM(name)//' type is '// &
                    tmpParam%dataType//' and must be REAL(SSK)!')
            ENDSELECT
          ELSE
            CALL eParams%raiseError(modName//'::'//myName// &
              ' - unable to locate parameter "'//TRIM(name)//'" in "'// &
                thisParam%name//'"!')
          ENDIF
      ENDSELECT
    ENDSUBROUTINE set_ParamType_SSK
!
!-------------------------------------------------------------------------------
!> @brief Gets the scalar single precision real value for a specified parameter
!> @param thisParam the parameter in which an existing parameter with name
!>        matching @c name will have it's value returned
!> @param name the name of the parameter to return the value of
!> @param val the current value of the parameter with @c name
!>
!> If a parameter with @c name is not found an error is produced. If the
!> parameter with @c name is not a scalar single precision real valued parameter
!> then an error is produced.
!>
    SUBROUTINE get_ParamType_SSK(thisParam,name,val)
      CHARACTER(LEN=*),PARAMETER :: myName='get_ParamType_SSK'
      CLASS(ParamType),INTENT(IN) :: thisParam
      CHARACTER(LEN=*),INTENT(IN) :: name
      REAL(SSK),INTENT(INOUT) :: val
      CLASS(ParamType),POINTER :: tmpParam

      SELECTTYPE(thisParam)
        TYPE IS(ParamType_SSK)
          IF(thisParam%name == TRIM(name)) THEN
            val=thisParam%val
          ELSE
            CALL eParams%raiseError(modName//'::'//myName// &
              ' - parameter name mismatch "'//TRIM(name)//'" in "'// &
                thisParam%name//'"!')
          ENDIF
        CLASS DEFAULT
          !Search for the parameter name
          CALL thisParam%getParam(name,tmpParam)
          IF(ASSOCIATED(tmpParam)) THEN
            !Parameter was found
            SELECTTYPE(p=>tmpParam)
              TYPE IS(ParamType_SSK)
                val=p%val
              CLASS DEFAULT
                CALL eParams%raiseError(modName//'::'//myName// &
                  ' - parameter data type mismatch! Parameter '//TRIM(name)//' type is '// &
                    tmpParam%dataType//' and must be REAL(SSK)!')
            ENDSELECT
          ELSE
            CALL eParams%raiseError(modName//'::'//myName// &
              ' - unable to locate parameter "'//TRIM(name)//'" in "'// &
                thisParam%name//'"!')
          ENDIF
      ENDSELECT
    ENDSUBROUTINE get_ParamType_SSK
!
!-------------------------------------------------------------------------------
!> @brief Adds a new scalar single precision real valued parameter to a set of
!> parameters
!> @param thisParam the set of parameters to which a new parameter will be added
!> @param name the location and name of the new parameter
!> @param param the single precision real value of the new parameter
!> @param description an optional input for a description of the new parameter
!>
!> This routine creates a new parameter within @c thisParam with @c name.
!> @c name may contain a full or partial path to the new parameter. If @c name
!> can be matched to an existing parameter in @c thisParam an error is produced
!> If @c name contains a full path for which intermediate lists do not exist
!> then this lists are created in the process of adding the new parameter.
!> If @c thisParam is not initialized and @c name does not contain a "->"
!> symbol then this routine behaves equivalently to
!> @ref ParameterLists::init_ParamType_SSK "initSSK".
!>
    SUBROUTINE add_ParamType_SSK(thisParam,name,param,description)
      CHARACTER(LEN=*),PARAMETER :: myName='add_ParamType_SSK'
      CLASS(ParamType),INTENT(INOUT) :: thisParam
      CHARACTER(LEN=*),INTENT(IN) :: name
      REAL(SSK),INTENT(IN) :: param
      CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
      CHARACTER(LEN=LEN(name)) :: prevname,thisname
      INTEGER(SIK) :: ipos
      TYPE(ParamType) :: newParam
      CLASS(ParamType),POINTER :: tmpParam

      !Search for the name to make sure it does not exist
      CALL get_ParamType(thisParam,name,tmpParam)

      IF(.NOT.ASSOCIATED(tmpParam)) THEN
        prevname=''
        thisname=ADJUSTL(name)
        ipos=INDEX(name,'->',.TRUE.)
        IF(ipos > 0) THEN
          prevname=ADJUSTL(name(1:ipos-1))
          thisname=ADJUSTL(name(ipos+2:LEN(name)))
        ENDIF

        !Initialize the new parameter
        IF(PRESENT(description)) THEN
          CALL init_ParamType_SSK(newParam,thisname,param,description)
        ELSE
          CALL init_ParamType_SSK(newParam,thisname,param)
        ENDIF

        !Add the new parameter to thisParam
        CALL add_ParamType(thisParam,prevname,newParam)
        CALL newParam%clear()
      ELSE
        CALL eParams%raiseError(modName//'::'//myName// &
          ' - parameter name "'//TRIM(name)// &
            '" already exists! Use set method or full parameter list path!')
      ENDIF
    ENDSUBROUTINE add_ParamType_SSK
!
!-------------------------------------------------------------------------------
!> @brief Initializes a ParamType object as a scalar double precision real
!> @param thisParam the parameter to initialize
!> @param name the name of the parameter
!> @param param a scalar double precision real
!> @param description an optional description for this parameter
!>
!> This routine is not recursive, so it is like setting a scalar parameter.
!> Therefore the name cannot contain the "->" symbol to indicate access to a
!> sub-list. @c thisParam must not already be inititalized.
!>
    SUBROUTINE init_ParamType_SDK(thisParam,name,param,description)
      CHARACTER(LEN=*),PARAMETER :: myName='init_ParamType_SDK'
      CLASS(ParamType),INTENT(INOUT) :: thisParam
      REAL(SDK),INTENT(IN) :: param
      CHARACTER(LEN=*),INTENT(IN) :: name
      CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
      INTEGER(SIK) :: ipos

      IF(.NOT.ASSOCIATED(thisParam%pdat)) THEN
        !Check that '->' character is not in name
        ipos=INDEX(name,'->')
        IF(ipos == 0) THEN
          ALLOCATE(ParamType_SDK :: thisParam%pdat)
          thisParam%pdat%name=TRIM(name)
          IF(PRESENT(description)) thisParam%pdat%description=TRIM(description)
          thisParam%pdat%dataType='REAL(SDK)'
          SELECTTYPE(p=>thisParam%pdat)
            TYPE IS(ParamType_SDK); p%val=param
          ENDSELECT
        ELSE
          CALL eParams%raiseError(modName//'::'//myName// &
            ' - "->" symbol is not allowed in name!')
        ENDIF
      ELSE
        CALL eParams%raiseError(modName//'::'//myName// &
          ' - parameter is already initialized! Use set method!')
      ENDIF
    ENDSUBROUTINE init_ParamType_SDK
!
!-------------------------------------------------------------------------------
!> @brief Edits a scalar double precision real valued parameter
!> @param thisParam the scalar double precision real valued parameter to edit
!> @param funit the unit number to edit the parameter to
!> @param indent optional indicates the number of blank spaces to precede the
!>        beginning of text to edit.
!>
!> The formatted write uses the "general" edit descriptor so that 7 digits (one
!> more than the significant number in a double precision real) are always
!> printed if the number is very large in absolute value engineering format
!> is used otherwise floating point form is used to write the value.
!>
    SUBROUTINE edit_ParamType_SDK(thisParam,funit,indent)
      CLASS(ParamType_SDK),INTENT(IN) :: thisParam
      INTEGER(SIK),INTENT(IN) :: funit
      INTEGER(SIK),INTENT(IN),OPTIONAL :: indent
      CHARACTER(LEN=12) :: fmt
      INTEGER(SIK) :: i

      i=1
      IF(PRESENT(indent)) i=i+indent
      WRITE(fmt,'(i12)') i; fmt=ADJUSTL(fmt)
      IF(LEN_TRIM(thisParam%description) == 0) THEN
        WRITE(UNIT=funit,FMT='('//TRIM(fmt)//'x,a,g23.16)') &
          thisParam%dataType//' :: '//thisParam%name//'=',thisParam%val
      ELSE
        WRITE(UNIT=funit,FMT='('//TRIM(fmt)//'x,a,g23.16,a)') &
          thisParam%dataType//' :: '//thisParam%name//'=',thisParam%val, &
            ' !'//thisParam%description
      ENDIF
    ENDSUBROUTINE edit_ParamType_SDK
!
!-------------------------------------------------------------------------------
!> @brief Clears a scalar double precision real valued parameter
!> @param thisParam the scalar double precision real valued parameter to clear
!>
    SUBROUTINE clear_ParamType_SDK(thisParam)
      CLASS(ParamType_SDK),INTENT(INOUT) :: thisParam
      thisParam%val=0.0_SDK
      thisParam%name=''
      thisParam%dataType=''
      thisParam%description=''
    ENDSUBROUTINE clear_ParamType_SDK
!
!-------------------------------------------------------------------------------
!> @brief Sets the value of an existing scalar double precision real valued
!> parameter to a new value.
!> @param thisParam the parameter in which an existing parameter with name
!>        matching @c name will be to set the new value of @c param
!> @param name the name of an existing parameter to set the value of
!> @param param the new value to set for the parameter
!> @param description an optional new description for the parameter identified
!>        by @c name
!>
!> If a parameter with @c name is not found an error is produced. If the
!> parameter with @c name is not a scalar double precision real valued parameter
!> then an error is produced.
!>
    SUBROUTINE set_ParamType_SDK(thisParam,name,param,description)
      CHARACTER(LEN=*),PARAMETER :: myName='set_ParamType_SDK'
      CLASS(ParamType),INTENT(INOUT) :: thisParam
      CHARACTER(LEN=*),INTENT(IN) :: name
      REAL(SDK),INTENT(IN) :: param
      CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
      CLASS(ParamType),POINTER :: tmpParam

      SELECTTYPE(thisParam)
        TYPE IS(ParamType_SDK)
          IF(thisParam%name == TRIM(name)) THEN
            thisParam%val=param
            IF(PRESENT(description)) thisParam%description=TRIM(description)
          ELSE
            CALL eParams%raiseError(modName//'::'//myName// &
              ' - parameter name mismatch! Tried to set "'//TRIM(name)// &
                '" but name is "'//thisParam%name//'"!')
          ENDIF
        CLASS DEFAULT
          !Search for the parameter name
          CALL thisParam%getParam(name,tmpParam)
          IF(ASSOCIATED(tmpParam)) THEN
            !Parameter was found
            SELECTTYPE(p=>tmpParam)
              TYPE IS(ParamType_SDK)
                p%val=param
                IF(PRESENT(description)) p%description=TRIM(description)
              CLASS DEFAULT
                CALL eParams%raiseError(modName//'::'//myName// &
                  ' - parameter data type mismatch! Parameter '//TRIM(name)//' type is '// &
                    tmpParam%dataType//' and must be REAL(SDK)!')
            ENDSELECT
          ELSE
            CALL eParams%raiseError(modName//'::'//myName// &
              ' - unable to locate parameter "'//TRIM(name)//'" in "'// &
                thisParam%name//'"!')
          ENDIF
      ENDSELECT
    ENDSUBROUTINE set_ParamType_SDK
!
!-------------------------------------------------------------------------------
!> @brief Gets the scalar double precision real value for a specified parameter
!> @param thisParam the parameter in which an existing parameter with name
!>        matching @c name will have it's value returned
!> @param name the name of the parameter to return the value of
!> @param val the current value of the parameter with @c name
!>
!> If a parameter with @c name is not found an error is produced. If the
!> parameter with @c name is not a scalar double precision real valued parameter
!> then an error is produced.
!>
    SUBROUTINE get_ParamType_SDK(thisParam,name,val)
      CHARACTER(LEN=*),PARAMETER :: myName='get_ParamType_SDK'
      CLASS(ParamType),INTENT(IN) :: thisParam
      CHARACTER(LEN=*),INTENT(IN) :: name
      REAL(SDK),INTENT(INOUT) :: val
      CLASS(ParamType),POINTER :: tmpParam

      SELECTTYPE(thisParam)
        TYPE IS(ParamType_SDK)
          IF(thisParam%name == TRIM(name)) THEN
            val=thisParam%val
          ELSE
            CALL eParams%raiseError(modName//'::'//myName// &
              ' - parameter name mismatch "'//TRIM(name)//'" in "'// &
                thisParam%name//'"!')
          ENDIF
        CLASS DEFAULT
          !Search for the parameter name
          CALL thisParam%getParam(name,tmpParam)
          IF(ASSOCIATED(tmpParam)) THEN
            !Parameter was found
            SELECTTYPE(p=>tmpParam)
              TYPE IS(ParamType_SDK)
                val=p%val
              CLASS DEFAULT
                CALL eParams%raiseError(modName//'::'//myName// &
                  ' - parameter data type mismatch! Parameter '//TRIM(name)//' type is '// &
                    tmpParam%dataType//' and must be REAL(SDK)!')
            ENDSELECT
          ELSE
            CALL eParams%raiseError(modName//'::'//myName// &
              ' - unable to locate parameter "'//TRIM(name)//'" in "'// &
                thisParam%name//'"!')
          ENDIF
      ENDSELECT
    ENDSUBROUTINE get_ParamType_SDK
!
!-------------------------------------------------------------------------------
!> @brief Adds a new scalar double precision real valued parameter to a set of
!> parameters
!> @param thisParam the set of parameters to which a new parameter will be added
!> @param name the location and name of the new parameter
!> @param param the double precision real value of the new parameter
!> @param description an optional input for a description of the new parameter
!>
!> This routine creates a new parameter within @c thisParam with @c name.
!> @c name may contain a full or partial path to the new parameter. If @c name
!> can be matched to an existing parameter in @c thisParam an error is produced
!> If @c name contains a full path for which intermediate lists do not exist
!> then this lists are created in the process of adding the new parameter.
!> If @c thisParam is not initialized and @c name does not contain a "->"
!> symbol then this routine behaves equivalently to
!> @ref ParameterLists::init_ParamType_SDK "initSDK".
!>
    SUBROUTINE add_ParamType_SDK(thisParam,name,param,description)
      CHARACTER(LEN=*),PARAMETER :: myName='add_ParamType_SDK'
      CLASS(ParamType),INTENT(INOUT) :: thisParam
      CHARACTER(LEN=*),INTENT(IN) :: name
      REAL(SDK),INTENT(IN) :: param
      CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
      CHARACTER(LEN=LEN(name)) :: prevname,thisname
      INTEGER(SIK) :: ipos
      TYPE(ParamType) :: newParam
      CLASS(ParamType),POINTER :: tmpParam

      !Search for the name to make sure it does not exist
      CALL get_ParamType(thisParam,name,tmpParam)

      IF(.NOT.ASSOCIATED(tmpParam)) THEN
        prevname=''
        thisname=ADJUSTL(name)
        ipos=INDEX(name,'->',.TRUE.)
        IF(ipos > 0) THEN
          prevname=ADJUSTL(name(1:ipos-1))
          thisname=ADJUSTL(name(ipos+2:LEN(name)))
        ENDIF

        !Initialize the new parameter
        IF(PRESENT(description)) THEN
          CALL init_ParamType_SDK(newParam,thisname,param,description)
        ELSE
          CALL init_ParamType_SDK(newParam,thisname,param)
        ENDIF

        !Add the new parameter to thisParam
        CALL add_ParamType(thisParam,prevname,newParam)
        CALL newParam%clear()
      ELSE
        CALL eParams%raiseError(modName//'::'//myName// &
          ' - parameter name "'//TRIM(name)// &
            '" already exists! Use set method or full parameter list path!')
      ENDIF
    ENDSUBROUTINE add_ParamType_SDK
!
!-------------------------------------------------------------------------------
!> @brief Initializes a ParamType object as a scalar 32-bit integer
!> @param thisParam the parameter to initialize
!> @param name the name of the parameter
!> @param param a scalar 32-bit integer
!> @param description an optional description for this parameter
!>
!> This routine is not recursive, so it is like setting a scalar parameter.
!> Therefore the name cannot contain the "->" symbol to indicate access to a
!> sub-list. @c thisParam must not already be inititalized.
!>
    SUBROUTINE init_ParamType_SNK(thisParam,name,param,description)
      CHARACTER(LEN=*),PARAMETER :: myName='init_ParamType_SNK'
      CLASS(ParamType),INTENT(INOUT) :: thisParam
      INTEGER(SNK),INTENT(IN) :: param
      CHARACTER(LEN=*),INTENT(IN) :: name
      CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
      INTEGER(SIK) :: ipos

      IF(.NOT.ASSOCIATED(thisParam%pdat)) THEN
        !Check that '->' character is not in name
        ipos=INDEX(name,'->')
        IF(ipos == 0) THEN
          ALLOCATE(ParamType_SNK :: thisParam%pdat)
          thisParam%pdat%name=TRIM(name)
          IF(PRESENT(description)) thisParam%pdat%description=TRIM(description)
          thisParam%pdat%dataType='INTEGER(SNK)'
          SELECTTYPE(p=>thisParam%pdat)
            TYPE IS(ParamType_SNK); p%val=param
          ENDSELECT
        ELSE
          CALL eParams%raiseError(modName//'::'//myName// &
            ' - "->" symbol is not allowed in name!')
        ENDIF
      ELSE
        CALL eParams%raiseError(modName//'::'//myName// &
          ' - parameter is already initialized! Use set method!')
      ENDIF
    ENDSUBROUTINE init_ParamType_SNK
!
!-------------------------------------------------------------------------------
!> @brief Edits a scalar 32-bit integer valued parameter
!> @param thisParam the scalar 32-bit integer valued parameter to edit
!> @param funit the unit number to edit the parameter to
!> @param indent optional indicates the number of blank spaces to precede the
!>        beginning of text to edit.
!>
!> The formatted write uses the "general" edit descriptor so that 7 digits (one
!> more than the significant number in a 32-bit integer) are always
!> printed if the number is very large in absolute value engineering format
!> is used otherwise floating point form is used to write the value.
!>
    SUBROUTINE edit_ParamType_SNK(thisParam,funit,indent)
      CLASS(ParamType_SNK),INTENT(IN) :: thisParam
      INTEGER(SIK),INTENT(IN) :: funit
      INTEGER(SIK),INTENT(IN),OPTIONAL :: indent
      CHARACTER(LEN=12) :: fmt
      INTEGER(SIK) :: i

      i=1
      IF(PRESENT(indent)) i=i+indent
      WRITE(fmt,'(i12)') i; fmt=ADJUSTL(fmt)
      IF(LEN_TRIM(thisParam%description) == 0) THEN
        WRITE(UNIT=funit,FMT='('//TRIM(fmt)//'x,a,g13.7)') &
          thisParam%dataType//' :: '//thisParam%name//'=',thisParam%val
      ELSE
        WRITE(UNIT=funit,FMT='('//TRIM(fmt)//'x,a,g13.7,a)') &
          thisParam%dataType//' :: '//thisParam%name//'=',thisParam%val, &
            ' !'//thisParam%description
      ENDIF
    ENDSUBROUTINE edit_ParamType_SNK
!
!-------------------------------------------------------------------------------
!> @brief Clears a scalar 32-bit integer valued parameter
!> @param thisParam the scalar 32-bit integer valued parameter to clear
!>
    SUBROUTINE clear_ParamType_SNK(thisParam)
      CLASS(ParamType_SNK),INTENT(INOUT) :: thisParam
      thisParam%val=0_SNK
      thisParam%name=''
      thisParam%dataType=''
      thisParam%description=''
    ENDSUBROUTINE clear_ParamType_SNK
!
!-------------------------------------------------------------------------------
!> @brief Sets the value of an existing scalar 32-bit integer valued
!> parameter to a new value.
!> @param thisParam the parameter in which an existing parameter with name
!>        matching @c name will be to set the new value of @c param
!> @param name the name of an existing parameter to set the value of
!> @param param the new value to set for the parameter
!> @param description an optional new description for the parameter identified
!>        by @c name
!>
!> If a parameter with @c name is not found an error is produced. If the
!> parameter with @c name is not a scalar 32-bit integer valued parameter
!> then an error is produced.
!>
    SUBROUTINE set_ParamType_SNK(thisParam,name,param,description)
      CHARACTER(LEN=*),PARAMETER :: myName='set_ParamType_SNK'
      CLASS(ParamType),INTENT(INOUT) :: thisParam
      CHARACTER(LEN=*),INTENT(IN) :: name
      INTEGER(SNK),INTENT(IN) :: param
      CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
      CLASS(ParamType),POINTER :: tmpParam

      SELECTTYPE(thisParam)
        TYPE IS(ParamType_SNK)
          IF(thisParam%name == TRIM(name)) THEN
            thisParam%val=param
            IF(PRESENT(description)) thisParam%description=TRIM(description)
          ELSE
            CALL eParams%raiseError(modName//'::'//myName// &
              ' - parameter name mismatch! Tried to set "'//TRIM(name)// &
                '" but name is "'//thisParam%name//'"!')
          ENDIF
        CLASS DEFAULT
          !Search for the parameter name
          CALL thisParam%getParam(name,tmpParam)
          IF(ASSOCIATED(tmpParam)) THEN
            !Parameter was found
            SELECTTYPE(p=>tmpParam)
              TYPE IS(ParamType_SNK)
                p%val=param
                IF(PRESENT(description)) p%description=TRIM(description)
              CLASS DEFAULT
                CALL eParams%raiseError(modName//'::'//myName// &
                  ' - parameter data type mismatch! Parameter '//TRIM(name)//' type is '// &
                    tmpParam%dataType//' and must be INTEGER(SNK)!')
            ENDSELECT
          ELSE
            CALL eParams%raiseError(modName//'::'//myName// &
              ' - unable to locate parameter "'//TRIM(name)//'" in "'// &
                thisParam%name//'"!')
          ENDIF
      ENDSELECT
    ENDSUBROUTINE set_ParamType_SNK
!
!-------------------------------------------------------------------------------
!> @brief Gets the scalar 32-bit integer value for a specified parameter
!> @param thisParam the parameter in which an existing parameter with name
!>        matching @c name will have it's value returned
!> @param name the name of the parameter to return the value of
!> @param val the current value of the parameter with @c name
!>
!> If a parameter with @c name is not found an error is produced. If the
!> parameter with @c name is not a scalar 32-bit integer valued parameter
!> then an error is produced.
!>
    SUBROUTINE get_ParamType_SNK(thisParam,name,val)
      CHARACTER(LEN=*),PARAMETER :: myName='get_ParamType_SNK'
      CLASS(ParamType),INTENT(IN) :: thisParam
      CHARACTER(LEN=*),INTENT(IN) :: name
      INTEGER(SNK),INTENT(INOUT) :: val
      CLASS(ParamType),POINTER :: tmpParam

      SELECTTYPE(thisParam)
        TYPE IS(ParamType_SNK)
          IF(thisParam%name == TRIM(name)) THEN
            val=thisParam%val
          ELSE
            CALL eParams%raiseError(modName//'::'//myName// &
              ' - parameter name mismatch "'//TRIM(name)//'" in "'// &
                thisParam%name//'"!')
          ENDIF
        CLASS DEFAULT
          !Search for the parameter name
          CALL thisParam%getParam(name,tmpParam)
          IF(ASSOCIATED(tmpParam)) THEN
            !Parameter was found
            SELECTTYPE(p=>tmpParam)
              TYPE IS(ParamType_SNK)
                val=p%val
              CLASS DEFAULT
                CALL eParams%raiseError(modName//'::'//myName// &
                  ' - parameter data type mismatch! Parameter '//TRIM(name)//' type is '// &
                    tmpParam%dataType//' and must be INTEGER(SNK)!')
            ENDSELECT
          ELSE
            CALL eParams%raiseError(modName//'::'//myName// &
              ' - unable to locate parameter "'//TRIM(name)//'" in "'// &
                thisParam%name//'"!')
          ENDIF
      ENDSELECT
    ENDSUBROUTINE get_ParamType_SNK
!
!-------------------------------------------------------------------------------
!> @brief Adds a new scalar 32-bit integer valued parameter to a set of
!> parameters
!> @param thisParam the set of parameters to which a new parameter will be added
!> @param name the location and name of the new parameter
!> @param param the 32-bit integer value of the new parameter
!> @param description an optional input for a description of the new parameter
!>
!> This routine creates a new parameter within @c thisParam with @c name.
!> @c name may contain a full or partial path to the new parameter. If @c name
!> can be matched to an existing parameter in @c thisParam an error is produced
!> If @c name contains a full path for which intermediate lists do not exist
!> then this lists are created in the process of adding the new parameter.
!> If @c thisParam is not initialized and @c name does not contain a "->"
!> symbol then this routine behaves equivalently to
!> @ref ParameterLists::init_ParamType_SNK "initSNK".
!>
    SUBROUTINE add_ParamType_SNK(thisParam,name,param,description)
      CHARACTER(LEN=*),PARAMETER :: myName='add_ParamType_SNK'
      CLASS(ParamType),INTENT(INOUT) :: thisParam
      CHARACTER(LEN=*),INTENT(IN) :: name
      INTEGER(SNK),INTENT(IN) :: param
      CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
      CHARACTER(LEN=LEN(name)) :: prevname,thisname
      INTEGER(SIK) :: ipos
      TYPE(ParamType) :: newParam
      CLASS(ParamType),POINTER :: tmpParam

      !Search for the name to make sure it does not exist
      CALL get_ParamType(thisParam,name,tmpParam)

      IF(.NOT.ASSOCIATED(tmpParam)) THEN
        prevname=''
        thisname=ADJUSTL(name)
        ipos=INDEX(name,'->',.TRUE.)
        IF(ipos > 0) THEN
          prevname=ADJUSTL(name(1:ipos-1))
          thisname=ADJUSTL(name(ipos+2:LEN(name)))
        ENDIF

        !Initialize the new parameter
        CALL init_ParamType_SNK(newParam,thisname,param,description)

        !Add the new parameter to thisParam
        CALL add_ParamType(thisParam,prevname,newParam)
        CALL newParam%clear()
      ELSE
        CALL eParams%raiseError(modName//'::'//myName// &
          ' - parameter name "'//TRIM(name)// &
            '" already exists! Use set method or full parameter list path!')
      ENDIF
    ENDSUBROUTINE add_ParamType_SNK
!
!-------------------------------------------------------------------------------
!> @brief Initializes a ParamType object as a scalar 64-bit integer
!> @param thisParam the parameter to initialize
!> @param name the name of the parameter
!> @param param a scalar 64-bit integer
!> @param description an optional description for this parameter
!>
!> This routine is not recursive, so it is like setting a scalar parameter.
!> Therefore the name cannot contain the "->" symbol to indicate access to a
!> sub-list. @c thisParam must not already be inititalized.
!>
    SUBROUTINE init_ParamType_SLK(thisParam,name,param,description)
      CHARACTER(LEN=*),PARAMETER :: myName='init_ParamType_SLK'
      CLASS(ParamType),INTENT(INOUT) :: thisParam
      INTEGER(SLK),INTENT(IN) :: param
      CHARACTER(LEN=*),INTENT(IN) :: name
      CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
      INTEGER(SIK) :: ipos

      IF(.NOT.ASSOCIATED(thisParam%pdat)) THEN
        !Check that '->' character is not in name
        ipos=INDEX(name,'->')
        IF(ipos == 0) THEN
          ALLOCATE(ParamType_SLK :: thisParam%pdat)
          thisParam%pdat%name=TRIM(name)
          IF(PRESENT(description)) thisParam%pdat%description=TRIM(description)
          thisParam%pdat%dataType='INTEGER(SLK)'
          SELECTTYPE(p=>thisParam%pdat)
            TYPE IS(ParamType_SLK); p%val=param
          ENDSELECT
        ELSE
          CALL eParams%raiseError(modName//'::'//myName// &
            ' - "->" symbol is not allowed in name!')
        ENDIF
      ELSE
        CALL eParams%raiseError(modName//'::'//myName// &
          ' - parameter is already initialized! Use set method!')
      ENDIF
    ENDSUBROUTINE init_ParamType_SLK
!
!-------------------------------------------------------------------------------
!> @brief Edits a scalar 64-bit integer valued parameter
!> @param thisParam the scalar 64-bit integer valued parameter to edit
!> @param funit the unit number to edit the parameter to
!> @param indent optional indicates the number of blank spaces to precede the
!>        beginning of text to edit.
!>
!> The formatted write uses the "general" edit descriptor so that 7 digits (one
!> more than the significant number in a 64-bit integer) are always
!> printed if the number is very large in absolute value engineering format
!> is used otherwise floating point form is used to write the value.
!>
    SUBROUTINE edit_ParamType_SLK(thisParam,funit,indent)
      CLASS(ParamType_SLK),INTENT(IN) :: thisParam
      INTEGER(SIK),INTENT(IN) :: funit
      INTEGER(SIK),INTENT(IN),OPTIONAL :: indent
      CHARACTER(LEN=12) :: fmt
      INTEGER(SIK) :: i

      i=1
      IF(PRESENT(indent)) i=i+indent
      WRITE(fmt,'(i12)') i; fmt=ADJUSTL(fmt)
      IF(LEN_TRIM(thisParam%description) == 0) THEN
        WRITE(UNIT=funit,FMT='('//TRIM(fmt)//'x,a,g13.7)') &
          thisParam%dataType//' :: '//thisParam%name//'=',thisParam%val
      ELSE
        WRITE(UNIT=funit,FMT='('//TRIM(fmt)//'x,a,g13.7,a)') &
          thisParam%dataType//' :: '//thisParam%name//'=',thisParam%val, &
            ' !'//thisParam%description
      ENDIF
    ENDSUBROUTINE edit_ParamType_SLK
!
!-------------------------------------------------------------------------------
!> @brief Clears a scalar 64-bit integer valued parameter
!> @param thisParam the scalar 64-bit integer valued parameter to clear
!>
    SUBROUTINE clear_ParamType_SLK(thisParam)
      CLASS(ParamType_SLK),INTENT(INOUT) :: thisParam
      thisParam%val=0_SLK
      thisParam%name=''
      thisParam%dataType=''
      thisParam%description=''
    ENDSUBROUTINE clear_ParamType_SLK
!
!-------------------------------------------------------------------------------
!> @brief Sets the value of an existing scalar 64-bit integer valued
!> parameter to a new value.
!> @param thisParam the parameter in which an existing parameter with name
!>        matching @c name will be to set the new value of @c param
!> @param name the name of an existing parameter to set the value of
!> @param param the new value to set for the parameter
!> @param description an optional new description for the parameter identified
!>        by @c name
!>
!> If a parameter with @c name is not found an error is produced. If the
!> parameter with @c name is not a scalar 64-bit integer valued parameter
!> then an error is produced.
!>
    SUBROUTINE set_ParamType_SLK(thisParam,name,param,description)
      CHARACTER(LEN=*),PARAMETER :: myName='set_ParamType_SLK'
      CLASS(ParamType),INTENT(INOUT) :: thisParam
      CHARACTER(LEN=*),INTENT(IN) :: name
      INTEGER(SLK),INTENT(IN) :: param
      CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
      CLASS(ParamType),POINTER :: tmpParam

      SELECTTYPE(thisParam)
        TYPE IS(ParamType_SLK)
          IF(thisParam%name == TRIM(name)) THEN
            thisParam%val=param
            IF(PRESENT(description)) thisParam%description=TRIM(description)
          ELSE
            CALL eParams%raiseError(modName//'::'//myName// &
              ' - parameter name mismatch! Tried to set "'//TRIM(name)// &
                '" but name is "'//thisParam%name//'"!')
          ENDIF
        CLASS DEFAULT
          !Search for the parameter name
          CALL thisParam%getParam(name,tmpParam)
          IF(ASSOCIATED(tmpParam)) THEN
            !Parameter was found
            SELECTTYPE(p=>tmpParam)
              TYPE IS(ParamType_SLK)
                p%val=param
                IF(PRESENT(description)) p%description=TRIM(description)
              CLASS DEFAULT
                CALL eParams%raiseError(modName//'::'//myName// &
                  ' - parameter data type mismatch! Parameter '//TRIM(name)//' type is '// &
                    tmpParam%dataType//' and must be INTEGER(SLK)!')
            ENDSELECT
          ELSE
            CALL eParams%raiseError(modName//'::'//myName// &
              ' - unable to locate parameter "'//TRIM(name)//'" in "'// &
                thisParam%name//'"!')
          ENDIF
      ENDSELECT
    ENDSUBROUTINE set_ParamType_SLK
!
!-------------------------------------------------------------------------------
!> @brief Gets the scalar 64-bit integer value for a specified parameter
!> @param thisParam the parameter in which an existing parameter with name
!>        matching @c name will have it's value returned
!> @param name the name of the parameter to return the value of
!> @param val the current value of the parameter with @c name
!>
!> If a parameter with @c name is not found an error is produced. If the
!> parameter with @c name is not a scalar 64-bit integer valued parameter
!> then an error is produced.
!>
    SUBROUTINE get_ParamType_SLK(thisParam,name,val)
      CHARACTER(LEN=*),PARAMETER :: myName='get_ParamType_SLK'
      CLASS(ParamType),INTENT(IN) :: thisParam
      CHARACTER(LEN=*),INTENT(IN) :: name
      INTEGER(SLK),INTENT(INOUT) :: val
      CLASS(ParamType),POINTER :: tmpParam

      SELECTTYPE(thisParam)
        TYPE IS(ParamType_SLK)
          IF(thisParam%name == TRIM(name)) THEN
            val=thisParam%val
          ELSE
            CALL eParams%raiseError(modName//'::'//myName// &
              ' - parameter name mismatch "'//TRIM(name)//'" in "'// &
                thisParam%name//'"!')
          ENDIF
        CLASS DEFAULT
          !Search for the parameter name
          CALL thisParam%getParam(name,tmpParam)
          IF(ASSOCIATED(tmpParam)) THEN
            !Parameter was found
            SELECTTYPE(p=>tmpParam)
              TYPE IS(ParamType_SLK)
                val=p%val
              CLASS DEFAULT
                CALL eParams%raiseError(modName//'::'//myName// &
                  ' - parameter data type mismatch! Parameter '//TRIM(name)//' type is '// &
                    tmpParam%dataType//' and must be INTEGER(SLK)!')
            ENDSELECT
          ELSE
            CALL eParams%raiseError(modName//'::'//myName// &
              ' - unable to locate parameter "'//TRIM(name)//'" in "'// &
                thisParam%name//'"!')
          ENDIF
      ENDSELECT
    ENDSUBROUTINE get_ParamType_SLK
!
!-------------------------------------------------------------------------------
!> @brief Adds a new scalar 64-bit integer valued parameter to a set of
!> parameters
!> @param thisParam the set of parameters to which a new parameter will be added
!> @param name the location and name of the new parameter
!> @param param the 64-bit integer value of the new parameter
!> @param description an optional input for a description of the new parameter
!>
!> This routine creates a new parameter within @c thisParam with @c name.
!> @c name may contain a full or partial path to the new parameter. If @c name
!> can be matched to an existing parameter in @c thisParam an error is produced
!> If @c name contains a full path for which intermediate lists do not exist
!> then this lists are created in the process of adding the new parameter.
!> If @c thisParam is not initialized and @c name does not contain a "->"
!> symbol then this routine behaves equivalently to
!> @ref ParameterLists::init_ParamType_SLK "initSLK".
!>
    SUBROUTINE add_ParamType_SLK(thisParam,name,param,description)
      CHARACTER(LEN=*),PARAMETER :: myName='add_ParamType_SLK'
      CLASS(ParamType),INTENT(INOUT) :: thisParam
      CHARACTER(LEN=*),INTENT(IN) :: name
      INTEGER(SLK),INTENT(IN) :: param
      CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
      CHARACTER(LEN=LEN(name)) :: prevname,thisname
      INTEGER(SIK) :: ipos
      TYPE(ParamType) :: newParam
      CLASS(ParamType),POINTER :: tmpParam

      !Search for the name to make sure it does not exist
      CALL get_ParamType(thisParam,name,tmpParam)

      IF(.NOT.ASSOCIATED(tmpParam)) THEN
        prevname=''
        thisname=ADJUSTL(name)
        ipos=INDEX(name,'->',.TRUE.)
        IF(ipos > 0) THEN
          prevname=ADJUSTL(name(1:ipos-1))
          thisname=ADJUSTL(name(ipos+2:LEN(name)))
        ENDIF

        !Initialize the new parameter
        CALL init_ParamType_SLK(newParam,thisname,param,description)

        !Add the new parameter to thisParam
        CALL add_ParamType(thisParam,prevname,newParam)
        CALL newParam%clear()
      ELSE
        CALL eParams%raiseError(modName//'::'//myName// &
          ' - parameter name "'//TRIM(name)// &
            '" already exists! Use set method or full parameter list path!')
      ENDIF
    ENDSUBROUTINE add_ParamType_SLK
!
!-------------------------------------------------------------------------------
!> @brief Initializes a ParamType object as a logical
!> @param thisParam the parameter to initialize
!> @param name the name of the parameter
!> @param param a scalar logical
!> @param description an optional description for this parameter
!>
!> This routine is not recursive, so it is like setting a scalar parameter.
!> Therefore the name cannot contain the "->" symbol to indicate access to a
!> sub-list. @c thisParam must not already be inititalized.
!>
    SUBROUTINE init_ParamType_SBK(thisParam,name,param,description)
      CHARACTER(LEN=*),PARAMETER :: myName='init_ParamType_SBK'
      CLASS(ParamType),INTENT(INOUT) :: thisParam
      LOGICAL(SBK),INTENT(IN) :: param
      CHARACTER(LEN=*),INTENT(IN) :: name
      CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
      INTEGER(SIK) :: ipos

      IF(.NOT.ASSOCIATED(thisParam%pdat)) THEN
        !Check that '->' character is not in name
        ipos=INDEX(name,'->')
        IF(ipos == 0) THEN
          ALLOCATE(ParamType_SBK :: thisParam%pdat)
          thisParam%pdat%name=TRIM(name)
          IF(PRESENT(description)) thisParam%pdat%description=TRIM(description)
          thisParam%pdat%dataType='LOGICAL(SBK)'
          SELECTTYPE(p=>thisParam%pdat)
            TYPE IS(ParamType_SBK); p%val=param
          ENDSELECT
        ELSE
          CALL eParams%raiseError(modName//'::'//myName// &
            ' - "->" symbol is not allowed in name!')
        ENDIF
      ELSE
        CALL eParams%raiseError(modName//'::'//myName// &
          ' - parameter is already initialized! Use set method!')
      ENDIF
    ENDSUBROUTINE init_ParamType_SBK
!
!-------------------------------------------------------------------------------
!> @brief Edits a scalar logical valued parameter
!> @param thisParam the scalar logical valued parameter to edit
!> @param funit the unit number to edit the parameter to
!> @param indent optional indicates the number of blank spaces to precede the
!>        beginning of text to edit.
!>
!> The formatted write uses the "general" edit descriptor so that 7 digits (one
!> more than the significant number in a logical) are always
!> printed if the number is very large in absolute value engineering format
!> is used otherwise floating point form is used to write the value.
!>
    SUBROUTINE edit_ParamType_SBK(thisParam,funit,indent)
      CLASS(ParamType_SBK),INTENT(IN) :: thisParam
      INTEGER(SIK),INTENT(IN) :: funit
      INTEGER(SIK),INTENT(IN),OPTIONAL :: indent
      CHARACTER(LEN=12) :: fmt
      INTEGER(SIK) :: i

      i=1
      IF(PRESENT(indent)) i=i+indent
      WRITE(fmt,'(i12)') i; fmt=ADJUSTL(fmt)
      IF(LEN_TRIM(thisParam%description) == 0) THEN
        WRITE(UNIT=funit,FMT='('//TRIM(fmt)//'x,a,l2)') &
          thisParam%dataType//' :: '//thisParam%name//'=',thisParam%val
      ELSE
        WRITE(UNIT=funit,FMT='('//TRIM(fmt)//'x,a,l2,a)') &
          thisParam%dataType//' :: '//thisParam%name//'=',thisParam%val, &
            ' !'//thisParam%description
      ENDIF
    ENDSUBROUTINE edit_ParamType_SBK
!
!-------------------------------------------------------------------------------
!> @brief Clears a scalar logical valued parameter
!> @param thisParam the scalar logical valued parameter to clear
!>
    SUBROUTINE clear_ParamType_SBK(thisParam)
      CLASS(ParamType_SBK),INTENT(INOUT) :: thisParam
      thisParam%val=.FALSE.
      thisParam%name=''
      thisParam%dataType=''
      thisParam%description=''
    ENDSUBROUTINE clear_ParamType_SBK
!
!-------------------------------------------------------------------------------
!> @brief Sets the value of an existing scalar logical valued
!> parameter to a new value.
!> @param thisParam the parameter in which an existing parameter with name
!>        matching @c name will be to set the new value of @c param
!> @param name the name of an existing parameter to set the value of
!> @param param the new value to set for the parameter
!> @param description an optional new description for the parameter identified
!>        by @c name
!>
!> If a parameter with @c name is not found an error is produced. If the
!> parameter with @c name is not a scalar logical valued parameter
!> then an error is produced.
!>
    SUBROUTINE set_ParamType_SBK(thisParam,name,param,description)
      CHARACTER(LEN=*),PARAMETER :: myName='set_ParamType_SBK'
      CLASS(ParamType),INTENT(INOUT) :: thisParam
      CHARACTER(LEN=*),INTENT(IN) :: name
      LOGICAL(SBK),INTENT(IN) :: param
      CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
      CLASS(ParamType),POINTER :: tmpParam

      SELECTTYPE(thisParam)
        TYPE IS(ParamType_SBK)
          IF(thisParam%name == TRIM(name)) THEN
            thisParam%val=param
            IF(PRESENT(description)) thisParam%description=TRIM(description)
          ELSE
            CALL eParams%raiseError(modName//'::'//myName// &
              ' - parameter name mismatch! Tried to set "'//TRIM(name)// &
                '" but name is "'//thisParam%name//'"!')
          ENDIF
        CLASS DEFAULT
          !Search for the parameter name
          CALL thisParam%getParam(name,tmpParam)
          IF(ASSOCIATED(tmpParam)) THEN
            !Parameter was found
            SELECTTYPE(p=>tmpParam)
              TYPE IS(ParamType_SBK)
                p%val=param
                IF(PRESENT(description)) p%description=TRIM(description)
              CLASS DEFAULT
                CALL eParams%raiseError(modName//'::'//myName// &
                  ' - parameter data type mismatch! Parameter '//TRIM(name)//' type is '// &
                    tmpParam%dataType//' and must be LOGICAL(SBK)!')
            ENDSELECT
          ELSE
            CALL eParams%raiseError(modName//'::'//myName// &
              ' - unable to locate parameter "'//TRIM(name)//'" in "'// &
                thisParam%name//'"!')
          ENDIF
      ENDSELECT
    ENDSUBROUTINE set_ParamType_SBK
!
!-------------------------------------------------------------------------------
!> @brief Gets the scalar logical value for a specified parameter
!> @param thisParam the parameter in which an existing parameter with name
!>        matching @c name will have it's value returned
!> @param name the name of the parameter to return the value of
!> @param val the current value of the parameter with @c name
!>
!> If a parameter with @c name is not found an error is produced. If the
!> parameter with @c name is not a scalar logical valued parameter
!> then an error is produced.
!>
    SUBROUTINE get_ParamType_SBK(thisParam,name,val)
      CHARACTER(LEN=*),PARAMETER :: myName='get_ParamType_SBK'
      CLASS(ParamType),INTENT(IN) :: thisParam
      CHARACTER(LEN=*),INTENT(IN) :: name
      LOGICAL(SBK),INTENT(INOUT) :: val
      CLASS(ParamType),POINTER :: tmpParam

      SELECTTYPE(thisParam)
        TYPE IS(ParamType_SBK)
          IF(thisParam%name == TRIM(name)) THEN
            val=thisParam%val
          ELSE
            CALL eParams%raiseError(modName//'::'//myName// &
              ' - parameter name mismatch "'//TRIM(name)//'" in "'// &
                thisParam%name//'"!')
          ENDIF
        CLASS DEFAULT
          !Search for the parameter name
          CALL thisParam%getParam(name,tmpParam)
          IF(ASSOCIATED(tmpParam)) THEN
            !Parameter was found
            SELECTTYPE(p=>tmpParam)
              TYPE IS(ParamType_SBK)
                val=p%val
              CLASS DEFAULT
                CALL eParams%raiseError(modName//'::'//myName// &
                  ' - parameter data type mismatch! Parameter '//TRIM(name)//' type is '// &
                    tmpParam%dataType//' and must be LOGICAL(SBK)!')
            ENDSELECT
          ELSE
            CALL eParams%raiseError(modName//'::'//myName// &
              ' - unable to locate parameter "'//TRIM(name)//'" in "'// &
                thisParam%name//'"!')
          ENDIF
      ENDSELECT
    ENDSUBROUTINE get_ParamType_SBK
!
!-------------------------------------------------------------------------------
!> @brief Adds a new scalar logical valued parameter to a set of
!> parameters
!> @param thisParam the set of parameters to which a new parameter will be added
!> @param name the location and name of the new parameter
!> @param param the logical value of the new parameter
!> @param description an optional input for a description of the new parameter
!>
!> This routine creates a new parameter within @c thisParam with @c name.
!> @c name may contain a full or partial path to the new parameter. If @c name
!> can be matched to an existing parameter in @c thisParam an error is produced
!> If @c name contains a full path for which intermediate lists do not exist
!> then this lists are created in the process of adding the new parameter.
!> If @c thisParam is not initialized and @c name does not contain a "->"
!> symbol then this routine behaves equivalently to
!> @ref ParameterLists::init_ParamType_SBK "initSBK".
!>
    SUBROUTINE add_ParamType_SBK(thisParam,name,param,description)
      CHARACTER(LEN=*),PARAMETER :: myName='add_ParamType_SBK'
      CLASS(ParamType),INTENT(INOUT) :: thisParam
      CHARACTER(LEN=*),INTENT(IN) :: name
      LOGICAL(SBK),INTENT(IN) :: param
      CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
      CHARACTER(LEN=LEN(name)) :: prevname,thisname
      INTEGER(SIK) :: ipos
      TYPE(ParamType) :: newParam
      CLASS(ParamType),POINTER :: tmpParam

      !Search for the name to make sure it does not exist
      CALL get_ParamType(thisParam,name,tmpParam)

      IF(.NOT.ASSOCIATED(tmpParam)) THEN
        prevname=''
        thisname=ADJUSTL(name)
        ipos=INDEX(name,'->',.TRUE.)
        IF(ipos > 0) THEN
          prevname=ADJUSTL(name(1:ipos-1))
          thisname=ADJUSTL(name(ipos+2:LEN(name)))
        ENDIF

        !Initialize the new parameter
        CALL init_ParamType_SBK(newParam,thisname,param,description)

        !Add the new parameter to thisParam
        CALL add_ParamType(thisParam,prevname,newParam)
        CALL newParam%clear()
      ELSE
        CALL eParams%raiseError(modName//'::'//myName// &
          ' - parameter name "'//TRIM(name)// &
            '" already exists! Use set method or full parameter list path!')
      ENDIF
    ENDSUBROUTINE add_ParamType_SBK
!
!-------------------------------------------------------------------------------
!> @brief Initializes a ParamType object as a string derived type
!> @param thisParam the parameter to initialize
!> @param name the name of the parameter
!> @param param a string derived type
!> @param description an optional description for this parameter
!>
!> This routine is not recursive, so it is like setting a scalar parameter.
!> Therefore the name cannot contain the "->" symbol to indicate access to a
!> sub-list. @c thisParam must not already be inititalized.
!>
    SUBROUTINE init_ParamType_STR(thisParam,name,param,description)
      CHARACTER(LEN=*),PARAMETER :: myName='init_ParamType_STR'
      CLASS(ParamType),INTENT(INOUT) :: thisParam
      TYPE(StringType),INTENT(IN) :: param
      CHARACTER(LEN=*),INTENT(IN) :: name
      CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
      INTEGER(SIK) :: ipos

      IF(.NOT.ASSOCIATED(thisParam%pdat)) THEN
        !Check that '->' character is not in name
        ipos=INDEX(name,'->')
        IF(ipos == 0) THEN
          ALLOCATE(ParamType_STR :: thisParam%pdat)
          thisParam%pdat%name=TRIM(name)
          IF(PRESENT(description)) thisParam%pdat%description=TRIM(description)
          thisParam%pdat%dataType='TYPE(StringType)'
          SELECTTYPE(p=>thisParam%pdat)
            TYPE IS(ParamType_STR); p%val=param
          ENDSELECT
        ELSE
          CALL eParams%raiseError(modName//'::'//myName// &
            ' - "->" symbol is not allowed in name!')
        ENDIF
      ELSE
        CALL eParams%raiseError(modName//'::'//myName// &
          ' - parameter is already initialized! Use set method!')
      ENDIF
    ENDSUBROUTINE init_ParamType_STR
!
!-------------------------------------------------------------------------------
!> @brief Edits a string derived type parameter
!> @param thisParam the string derived type parameter to edit
!> @param funit the unit number to edit the parameter to
!> @param indent optional indicates the number of blank spaces to precede the
!>        beginning of text to edit.
!>
    SUBROUTINE edit_ParamType_STR(thisParam,funit,indent)
      CLASS(ParamType_STR),INTENT(IN) :: thisParam
      INTEGER(SIK),INTENT(IN) :: funit
      INTEGER(SIK),INTENT(IN),OPTIONAL :: indent
      CHARACTER(LEN=12) :: fmt
      INTEGER(SIK) :: i

      i=1
      IF(PRESENT(indent)) i=i+indent
      WRITE(fmt,'(i12)') i; fmt=ADJUSTL(fmt)
      IF(LEN_TRIM(thisParam%description) == 0) THEN
        WRITE(UNIT=funit,FMT='('//TRIM(fmt)//'x,a)') &
          thisParam%dataType//' :: '//thisParam%name//'='//thisParam%val
      ELSE
        WRITE(UNIT=funit,FMT='('//TRIM(fmt)//'x,a)') &
          thisParam%dataType//' :: '//thisParam%name//'='//thisParam%val// &
            ' !'//thisParam%description
      ENDIF
    ENDSUBROUTINE edit_ParamType_STR
!
!-------------------------------------------------------------------------------
!> @brief Clears a string derived type parameter
!> @param thisParam the string derived type parameter to clear
!>
    SUBROUTINE clear_ParamType_STR(thisParam)
      CLASS(ParamType_STR),INTENT(INOUT) :: thisParam
      thisParam%val=''         !Not sure how to clear this since it doesn't have a clear routine!
      thisParam%name=''
      thisParam%dataType=''
      thisParam%description=''
    ENDSUBROUTINE clear_ParamType_STR
!
!-------------------------------------------------------------------------------
!> @brief Sets the value of an existing string derived type parameter to a new value.
!> @param thisParam the parameter in which an existing parameter with name
!>        matching @c name will be to set the new value of @c param
!> @param name the name of an existing parameter to set the value of
!> @param param the new value to set for the parameter
!> @param description an optional new description for the parameter identified
!>        by @c name
!>
!> If a parameter with @c name is not found an error is produced. If the
!> parameter with @c name is not a string derived type parameter
!> then an error is produced.
!>
    SUBROUTINE set_ParamType_STR(thisParam,name,param,description)
      CHARACTER(LEN=*),PARAMETER :: myName='set_ParamType_STR'
      CLASS(ParamType),INTENT(INOUT) :: thisParam
      CHARACTER(LEN=*),INTENT(IN) :: name
      TYPE(StringType),INTENT(IN) :: param
      CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
      CLASS(ParamType),POINTER :: tmpParam

      SELECTTYPE(thisParam)
        TYPE IS(ParamType_STR)
          IF(thisParam%name == TRIM(name)) THEN
            thisParam%val=param
            IF(PRESENT(description)) thisParam%description=TRIM(description)
          ELSE
            CALL eParams%raiseError(modName//'::'//myName// &
              ' - parameter name mismatch! Tried to set "'//TRIM(name)// &
                '" but name is "'//thisParam%name//'"!')
          ENDIF
        CLASS DEFAULT
          !Search for the parameter name
          CALL thisParam%getParam(name,tmpParam)
          IF(ASSOCIATED(tmpParam)) THEN
            !Parameter was found
            SELECTTYPE(p=>tmpParam)
              TYPE IS(ParamType_STR)
                p%val=param
                IF(PRESENT(description)) p%description=TRIM(description)
              CLASS DEFAULT
                CALL eParams%raiseError(modName//'::'//myName// &
                  ' - parameter data type mismatch! Parameter '//TRIM(name)//' type is '// &
                    tmpParam%dataType//' and must be TYPE(StringType)!')
            ENDSELECT
          ELSE
            CALL eParams%raiseError(modName//'::'//myName// &
              ' - unable to locate parameter "'//TRIM(name)//'" in "'// &
                thisParam%name//'"!')
          ENDIF
      ENDSELECT
    ENDSUBROUTINE set_ParamType_STR
!
!-------------------------------------------------------------------------------
!> @brief Gets the string derived type for a specified parameter
!> @param thisParam the parameter in which an existing parameter with name
!>        matching @c name will have it's value returned
!> @param name the name of the parameter to return the value of
!> @param val the current value of the parameter with @c name
!>
!> If a parameter with @c name is not found an error is produced. If the
!> parameter with @c name is not a string derived type parameter
!> then an error is produced.
!>
    SUBROUTINE get_ParamType_STR(thisParam,name,val)
      CHARACTER(LEN=*),PARAMETER :: myName='get_ParamType_STR'
      CLASS(ParamType),INTENT(IN) :: thisParam
      CHARACTER(LEN=*),INTENT(IN) :: name
      TYPE(StringType),INTENT(INOUT) :: val
      CLASS(ParamType),POINTER :: tmpParam

      SELECTTYPE(thisParam)
        TYPE IS(ParamType_STR)
          IF(thisParam%name == TRIM(name)) THEN
            val=thisParam%val
          ELSE
            CALL eParams%raiseError(modName//'::'//myName// &
              ' - parameter name mismatch "'//TRIM(name)//'" in "'// &
                thisParam%name//'"!')
          ENDIF
        CLASS DEFAULT
          !Search for the parameter name
          CALL thisParam%getParam(name,tmpParam)
          IF(ASSOCIATED(tmpParam)) THEN
            !Parameter was found
            SELECTTYPE(p=>tmpParam)
              TYPE IS(ParamType_STR)
                val=p%val
              CLASS DEFAULT
                CALL eParams%raiseError(modName//'::'//myName// &
                  ' - parameter data type mismatch! Parameter '//TRIM(name)//' type is '// &
                    tmpParam%dataType//' and must be TYPE(StringType)!')
            ENDSELECT
          ELSE
            CALL eParams%raiseError(modName//'::'//myName// &
              ' - unable to locate parameter "'//TRIM(name)//'" in "'// &
                thisParam%name//'"!')
          ENDIF
      ENDSELECT
    ENDSUBROUTINE get_ParamType_STR
!
!-------------------------------------------------------------------------------
!> @brief Adds a string derived type parameter to a set of parameters
!> @param thisParam the set of parameters to which a new parameter will be added
!> @param name the location and name of the new parameter
!> @param param the string derived type of the new parameter
!> @param description an optional input for a description of the new parameter
!>
!> This routine creates a new parameter within @c thisParam with @c name.
!> @c name may contain a full or partial path to the new parameter. If @c name
!> can be matched to an existing parameter in @c thisParam an error is produced
!> If @c name contains a full path for which intermediate lists do not exist
!> then this lists are created in the process of adding the new parameter.
!> If @c thisParam is not initialized and @c name does not contain a "->"
!> symbol then this routine behaves equivalently to
!> @ref ParameterLists::init_ParamType_STR "initSTR".
!>
    SUBROUTINE add_ParamType_STR(thisParam,name,param,description)
      CHARACTER(LEN=*),PARAMETER :: myName='add_ParamType_STR'
      CLASS(ParamType),INTENT(INOUT) :: thisParam
      CHARACTER(LEN=*),INTENT(IN) :: name
      TYPE(StringType),INTENT(IN) :: param
      CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
      CHARACTER(LEN=LEN(name)) :: prevname,thisname
      INTEGER(SIK) :: ipos
      TYPE(ParamType) :: newParam
      CLASS(ParamType),POINTER :: tmpParam

      !Search for the name to make sure it does not exist
      CALL get_ParamType(thisParam,name,tmpParam)

      IF(.NOT.ASSOCIATED(tmpParam)) THEN
        prevname=''
        thisname=ADJUSTL(name)
        ipos=INDEX(name,'->',.TRUE.)
        IF(ipos > 0) THEN
          prevname=ADJUSTL(name(1:ipos-1))
          thisname=ADJUSTL(name(ipos+2:LEN(name)))
        ENDIF

        !Initialize the new parameter
        CALL init_ParamType_STR(newParam,thisname,param,description)

        !Add the new parameter to thisParam
        CALL add_ParamType(thisParam,prevname,newParam)
        CALL newParam%clear()
      ELSE
        CALL eParams%raiseError(modName//'::'//myName// &
          ' - parameter name "'//TRIM(name)// &
            '" already exists! Use set method or full parameter list path!')
      ENDIF
    ENDSUBROUTINE add_ParamType_STR
!
!-------------------------------------------------------------------------------
!> @brief Wrapper for init_ParamType_STR to pass a character string instead of
!> a string type
!> @param thisParam the parameter to initialize
!> @param name the name of the parameter
!> @param param a character type
!> @param description an optional description for this parameter
!>
    SUBROUTINE init_ParamType_CHAR(thisParam,name,param,description)
      CLASS(ParamType),INTENT(INOUT) :: thisParam
      CHARACTER(LEN=*),INTENT(IN) :: param
      CHARACTER(LEN=*),INTENT(IN) :: name
      CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
      TYPE(StringType) :: s

      s=param
      CALL init_ParamType_STR(thisParam,name,s,description)
      s=''
    ENDSUBROUTINE init_ParamType_CHAR
!
!-------------------------------------------------------------------------------
!> @brief Wrapper for set_ParamType_STR to pass a character string instead of
!> a string type
!> @param thisParam the parameter in which an existing parameter with name
!>        matching @c name will be to set the new value of @c param
!> @param name the name of an existing parameter to set the value of
!> @param param the new value to set for the parameter
!> @param description an optional new description for the parameter identified
!>        by @c name
!>
    SUBROUTINE set_ParamType_CHAR(thisParam,name,param,description)
      CLASS(ParamType),INTENT(INOUT) :: thisParam
      CHARACTER(LEN=*),INTENT(IN) :: name
      CHARACTER(LEN=*),INTENT(IN) :: param
      CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
      TYPE(StringType) :: s

      s=param
      IF(PRESENT(description)) THEN
        CALL set_ParamType_STR(thisParam,name,s,description)
      ELSE
        CALL set_ParamType_STR(thisParam,name,s)
      ENDIF
      s=''
    ENDSUBROUTINE set_ParamType_CHAR
!
!-------------------------------------------------------------------------------
!> @brief Wrapper for set_ParamType_STR to pass a character string instead of
!> a string type
!> @param thisParam the set of parameters to which a new parameter will be added
!> @param name the location and name of the new parameter
!> @param param the new character string parameter
!> @param description an optional input for a description of the new parameter
!>
    SUBROUTINE add_ParamType_CHAR(thisParam,name,param,description)
      CLASS(ParamType),INTENT(INOUT) :: thisParam
      CHARACTER(LEN=*),INTENT(IN) :: name
      CHARACTER(LEN=*),INTENT(IN) :: param
      CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
      TYPE(StringType) :: s

      s=param
      IF(PRESENT(description)) THEN
        CALL add_ParamType_STR(thisParam,name,s,description)
      ELSE
        CALL add_ParamType_STR(thisParam,name,s)
      ENDIF
      s=''
    ENDSUBROUTINE add_ParamType_CHAR
!
!-------------------------------------------------------------------------------
!> @brief Wrapper for get_ParamType_STR to pass a character string instead of
!> a string type
!> @param thisParam the parameter in which an existing parameter with name
!>        matching @c name will have it's value returned
!> @param name the name of the parameter to return the value of
!> @param val the current value of the parameter with @c name
!>
    SUBROUTINE get_ParamType_CHAR(thisParam,name,val)
      CHARACTER(LEN=*),PARAMETER :: myName='get_ParamType_STR'
      CLASS(ParamType),INTENT(IN) :: thisParam
      CHARACTER(LEN=*),INTENT(IN) :: name
      CHARACTER(LEN=*),INTENT(INOUT) :: val
      TYPE(StringType) :: s

      CALL get_ParamType_STR(thisParam,name,s)
      val=s
      s=''
    ENDSUBROUTINE get_ParamType_CHAR
!1111111111111111111111111111111111111111111111111111111111111111111111111111111
!        One Dimensional Arrays
!1111111111111111111111111111111111111111111111111111111111111111111111111111111
!
!-------------------------------------------------------------------------------
!> @brief Initializes a ParamType object as a one dimensional array of single
!>        precision reals
!> @param thisParam the parameter to initialize
!> @param name the name of the parameter
!> @param param a one dimensional array of single precision reals
!> @param description an optional description for this parameter
!>
!> This routine is not recursive, so it is like setting a one dimensional array of parameter.
!> Therefore the name cannot contain the "->" symbol to indicate access to a
!> sub-list. @c thisParam must not already be inititalized.
!>
    SUBROUTINE init_ParamType_SSK_a1(thisParam,name,param,description)
      CHARACTER(LEN=*),PARAMETER :: myName='init_ParamType_SSK_a1'
      CLASS(ParamType),INTENT(INOUT) :: thisParam
      REAL(SSK),INTENT(IN) :: param(:)
      CHARACTER(LEN=*),INTENT(IN) :: name
      CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
      INTEGER(SIK) :: ipos

      IF(.NOT.ASSOCIATED(thisParam%pdat)) THEN
        !Check that '->' character is not in name
        ipos=INDEX(name,'->')
        IF(ipos == 0) THEN
          ALLOCATE(ParamType_SSK_a1 :: thisParam%pdat)
          thisParam%pdat%name=TRIM(name)
          IF(PRESENT(description)) thisParam%pdat%description=TRIM(description)
          thisParam%pdat%dataType='1-D ARRAY REAL(SSK)'
          SELECTTYPE(p=>thisParam%pdat)
            TYPE IS(ParamType_SSK_a1)
              ALLOCATE(p%val(SIZE(param)))
              p%val=param
          ENDSELECT
        ELSE
          CALL eParams%raiseError(modName//'::'//myName// &
            ' - "->" symbol is not allowed in name!')
        ENDIF
      ELSE
        CALL eParams%raiseError(modName//'::'//myName// &
          ' - parameter is already initialized! Use set method!')
      ENDIF
    ENDSUBROUTINE init_ParamType_SSK_a1
!
!-------------------------------------------------------------------------------
!> @brief Edits a one dimensional array of single precision real valued parameters
!> @param thisParam the one dimensional array of single precision real valued
!>        parameters to edit
!> @param funit the unit number to edit the parameter to
!> @param indent optional indicates the number of blank spaces to precede the
!>        beginning of text to edit.
!>
!> The formatted write uses the "general" edit descriptor so that 7 digits (one
!> more than the significant number in a single precision real) are always
!> printed if the number is very large in absolute value engineering format
!> is used otherwise floating point form is used to write the value.
!>
    SUBROUTINE edit_ParamType_SSK_a1(thisParam,funit,indent)
      CLASS(ParamType_SSK_a1),INTENT(IN) :: thisParam
      INTEGER(SIK),INTENT(IN) :: funit
      INTEGER(SIK),INTENT(IN),OPTIONAL :: indent
      CHARACTER(LEN=12) :: fmt,fmt2
      INTEGER(SIK) :: i,j,k

      i=1
      j=5
      IF(PRESENT(indent)) i=i+indent
      WRITE(fmt,'(i12)') i; fmt=ADJUSTL(fmt)
      WRITE(UNIT=funit,FMT='('//TRIM(fmt)//'x,a,g13.7)',ADVANCE='NO') &
        thisParam%dataType//' :: '//thisParam%name//'=',thisParam%val(1)
      j=j+LEN(thisParam%dataType)+LEN(thisParam%name)
      WRITE(fmt2,'(i12)') j; fmt2=ADJUSTL(fmt2)
      IF(SIZE(thisParam%val)>MAX_1D_LEN) THEN
        DO k=2,SIZE(thisParam%val)
          WRITE(UNIT=funit,FMT='(", ",g13.7)',ADVANCE='NO') thisParam%val(k)
        ENDDO

        IF(LEN_TRIM(thisParam%description) == 0) THEN
          WRITE(funit,*)
        ELSE
          WRITE(UNIT=funit,FMT='(a)') ' !'//thisParam%description
        ENDIF
      ELSE
        IF(LEN_TRIM(thisParam%description) == 0) THEN
          WRITE(funit,*)
        ELSE
          WRITE(UNIT=funit,FMT='(a)') ' !'//thisParam%description
        ENDIF
        DO k=2,SIZE(thisParam%val)
          WRITE(UNIT=funit,FMT='('//TRIM(fmt)//'x,'//TRIM(fmt2)//'x,g13.7)') &
            thisParam%val(k)
        ENDDO
      ENDIF
    ENDSUBROUTINE edit_ParamType_SSK_a1
!
!-------------------------------------------------------------------------------
!> @brief Clears a one dimensional array of single precision real valued parameter
!> @param thisParam the one dimensional array of single precision real valued parameter to clear
!>
    SUBROUTINE clear_ParamType_SSK_a1(thisParam)
      CLASS(ParamType_SSK_a1),INTENT(INOUT) :: thisParam
      DEALLOCATE(thisParam%val)
      thisParam%name=''
      thisParam%dataType=''
      thisParam%description=''
    ENDSUBROUTINE clear_ParamType_SSK_a1
!
!-------------------------------------------------------------------------------
!> @brief Sets the value of an existing one dimensional array of single precision real valued
!> parameter to a new value.
!> @param thisParam the parameter in which an existing parameter with name
!>        matching @c name will be to set the new value of @c param
!> @param name the name of an existing parameter to set the value of
!> @param param the new value to set for the parameter
!> @param description an optional new description for the parameter identified
!>        by @c name
!>
!> If a parameter with @c name is not found an error is produced. If the
!> parameter with @c name is not a one dimensional array of single precision real valued parameter
!> then an error is produced.
!>
    SUBROUTINE set_ParamType_SSK_a1(thisParam,name,param,description)
      CHARACTER(LEN=*),PARAMETER :: myName='set_ParamType_SSK_a1'
      CLASS(ParamType),INTENT(INOUT) :: thisParam
      CHARACTER(LEN=*),INTENT(IN) :: name
      REAL(SSK),INTENT(IN) :: param(:)
      CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
      CLASS(ParamType),POINTER :: tmpParam

      SELECTTYPE(thisParam)
        TYPE IS(ParamType_SSK_a1)
          IF(thisParam%name == TRIM(name)) THEN
            IF(SIZE(thisParam%val) /= SIZE(param)) THEN
              DEALLOCATE(thisParam%val)
              ALLOCATE(thisParam%val(SIZE(param)))
            ENDIF
            thisParam%val=param
            IF(PRESENT(description)) thisParam%description=TRIM(description)
          ELSE
            CALL eParams%raiseError(modName//'::'//myName// &
              ' - parameter name mismatch! Tried to set "'//TRIM(name)// &
                '" but name is "'//thisParam%name//'"!')
          ENDIF
        CLASS DEFAULT
          !Search for the parameter name
          CALL thisParam%getParam(name,tmpParam)
          IF(ASSOCIATED(tmpParam)) THEN
            !Parameter was found
            SELECTTYPE(p=>tmpParam)
              TYPE IS(ParamType_SSK_a1)
                IF(SIZE(p%val) /= SIZE(param)) THEN
                  DEALLOCATE(p%val)
                  ALLOCATE(p%val(SIZE(param)))
                ENDIF
                p%val=param
                IF(PRESENT(description)) p%description=TRIM(description)
              CLASS DEFAULT
                CALL eParams%raiseError(modName//'::'//myName// &
                  ' - parameter data type mismatch! Parameter '//TRIM(name)//' type is '// &
                    tmpParam%dataType//' and must be 1-D ARRAY REAL(SSK)!')
            ENDSELECT
          ELSE
            CALL eParams%raiseError(modName//'::'//myName// &
              ' - unable to locate parameter "'//TRIM(name)//'" in "'// &
                thisParam%name//'"!')
          ENDIF
      ENDSELECT
    ENDSUBROUTINE set_ParamType_SSK_a1
!
!-------------------------------------------------------------------------------
!> @brief Gets the one dimensional array of single precision real value for a specified parameter
!> @param thisParam the parameter in which an existing parameter with name
!>        matching @c name will have it's value returned
!> @param name the name of the parameter to return the value of
!> @param val the current value of the parameter with @c name
!>
!> If a parameter with @c name is not found an error is produced. If the
!> parameter with @c name is not a one dimensional array of single precision real valued parameter
!> then an error is produced.
!>
    SUBROUTINE get_ParamType_SSK_a1(thisParam,name,val)
      CHARACTER(LEN=*),PARAMETER :: myName='get_ParamType_SSK_a1'
      CLASS(ParamType),INTENT(IN) :: thisParam
      CHARACTER(LEN=*),INTENT(IN) :: name
      REAL(SSK),ALLOCATABLE,INTENT(INOUT) :: val(:)
      CLASS(ParamType),POINTER :: tmpParam

      SELECTTYPE(thisParam)
        TYPE IS(ParamType_SSK_a1)
          IF(thisParam%name == TRIM(name)) THEN
            IF(ALLOCATED(val)) THEN
              IF(SIZE(thisParam%val) /= SIZE(val)) THEN
                DEALLOCATE(val)
                ALLOCATE(val(SIZE(thisParam%val)))
              ENDIF
              val=thisParam%val
            ELSE
              ALLOCATE(val(SIZE(thisParam%val)))
              val=thisParam%val
            ENDIF
          ELSE
            CALL eParams%raiseError(modName//'::'//myName// &
              ' - parameter name mismatch "'//TRIM(name)//'" in "'// &
                thisParam%name//'"!')
          ENDIF
        CLASS DEFAULT
          !Search for the parameter name
          CALL thisParam%getParam(name,tmpParam)
          IF(ASSOCIATED(tmpParam)) THEN
            !Parameter was found
            SELECTTYPE(p=>tmpParam)
              TYPE IS(ParamType_SSK_a1)
                IF(ALLOCATED(val)) THEN
                  IF(SIZE(p%val) /= SIZE(val)) THEN
                    DEALLOCATE(val)
                    ALLOCATE(val(SIZE(p%val)))
                  ENDIF
                  val=p%val
                ELSE
                  ALLOCATE(val(SIZE(p%val)))
                  val=p%val
                ENDIF
              CLASS DEFAULT
                CALL eParams%raiseError(modName//'::'//myName// &
                  ' - parameter data type mismatch! Parameter '//TRIM(name)//' type is '// &
                    tmpParam%dataType//' and must be 1-D ARRAY REAL(SSK)!')
            ENDSELECT
          ELSE
            CALL eParams%raiseError(modName//'::'//myName// &
              ' - unable to locate parameter "'//TRIM(name)//'" in "'// &
                thisParam%name//'"!')
          ENDIF
      ENDSELECT
    ENDSUBROUTINE get_ParamType_SSK_a1
!
!-------------------------------------------------------------------------------
!> @brief Adds a new one dimensional array of single precision real valued parameter to a set of
!> parameters
!> @param thisParam the set of parameters to which a new parameter will be added
!> @param name the location and name of the new parameter
!> @param param the single precision real value of the new parameter
!> @param description an optional input for a description of the new parameter
!>
!> This routine creates a new parameter within @c thisParam with @c name.
!> @c name may contain a full or partial path to the new parameter. If @c name
!> can be matched to an existing parameter in @c thisParam an error is produced
!> If @c name contains a full path for which intermediate lists do not exist
!> then this lists are created in the process of adding the new parameter.
!> If @c thisParam is not initialized and @c name does not contain a "->"
!> symbol then this routine behaves equivalently to
!> @ref ParameterLists::init_ParamType_SSK "initSSK".
!>
    SUBROUTINE add_ParamType_SSK_a1(thisParam,name,param,description)
      CHARACTER(LEN=*),PARAMETER :: myName='add_ParamType_SSK_a1'
      CLASS(ParamType),INTENT(INOUT) :: thisParam
      CHARACTER(LEN=*),INTENT(IN) :: name
      REAL(SSK),INTENT(IN) :: param(:)
      CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
      CHARACTER(LEN=LEN(name)) :: prevname,thisname
      INTEGER(SIK) :: ipos
      TYPE(ParamType) :: newParam
      CLASS(ParamType),POINTER :: tmpParam

      !Search for the name to make sure it does not exist
      CALL get_ParamType(thisParam,name,tmpParam)

      IF(.NOT.ASSOCIATED(tmpParam)) THEN
        prevname=''
        thisname=ADJUSTL(name)
        ipos=INDEX(name,'->',.TRUE.)
        IF(ipos > 0) THEN
          prevname=ADJUSTL(name(1:ipos-1))
          thisname=ADJUSTL(name(ipos+2:LEN(name)))
        ENDIF

        !Initialize the new parameter
        CALL init_ParamType_SSK_a1(newParam,thisname,param,description)

        !Add the new parameter to thisParam
        CALL add_ParamType(thisParam,prevname,newParam)
        CALL newParam%clear()
      ELSE
        CALL eParams%raiseError(modName//'::'//myName// &
          ' - parameter name "'//TRIM(name)// &
            '" already exists! Use set method or full parameter list path!')
      ENDIF
    ENDSUBROUTINE add_ParamType_SSK_a1
!
!-------------------------------------------------------------------------------
!> @brief Initializes a ParamType object as a one dimensional array of double precision real
!> @param thisParam the parameter to initialize
!> @param name the name of the parameter
!> @param param a one dimensional array of double precision real
!> @param description an optional description for this parameter
!>
!> This routine is not recursive, so it is like setting a one dimensional array of parameter.
!> Therefore the name cannot contain the "->" symbol to indicate access to a
!> sub-list. @c thisParam must not already be inititalized.
!>
    SUBROUTINE init_ParamType_SDK_a1(thisParam,name,param,description)
      CHARACTER(LEN=*),PARAMETER :: myName='init_ParamType_SDK_a1'
      CLASS(ParamType),INTENT(INOUT) :: thisParam
      REAL(SDK),INTENT(IN) :: param(:)
      CHARACTER(LEN=*),INTENT(IN) :: name
      CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
      INTEGER(SIK) :: ipos

      IF(.NOT.ASSOCIATED(thisParam%pdat)) THEN
        !Check that '->' character is not in name
        ipos=INDEX(name,'->')
        IF(ipos == 0) THEN
          ALLOCATE(ParamType_SDK_a1 :: thisParam%pdat)
          thisParam%pdat%name=TRIM(name)
          IF(PRESENT(description)) thisParam%pdat%description=TRIM(description)
          thisParam%pdat%dataType='1-D ARRAY REAL(SDK)'
          SELECTTYPE(p=>thisParam%pdat)
            TYPE IS(ParamType_SDK_a1)
              ALLOCATE(p%val(SIZE(param)))
              p%val=param
          ENDSELECT
        ELSE
          CALL eParams%raiseError(modName//'::'//myName// &
            ' - "->" symbol is not allowed in name!')
        ENDIF
      ELSE
        CALL eParams%raiseError(modName//'::'//myName// &
          ' - parameter is already initialized! Use set method!')
      ENDIF
    ENDSUBROUTINE init_ParamType_SDK_a1
!
!-------------------------------------------------------------------------------
!> @brief Edits a one dimensional array of double precision real valued parameter
!> @param thisParam the one dimensional array of double precision real valued parameter to edit
!> @param funit the unit number to edit the parameter to
!> @param indent optional indicates the number of blank spaces to precede the
!>        beginning of text to edit.
!>
!> The formatted write uses the "general" edit descriptor so that 7 digits (one
!> more than the significant number in a double precision real) are always
!> printed if the number is very large in absolute value engineering format
!> is used otherwise floating point form is used to write the value.
!>
    SUBROUTINE edit_ParamType_SDK_a1(thisParam,funit,indent)
      CLASS(ParamType_SDK_a1),INTENT(IN) :: thisParam
      INTEGER(SIK),INTENT(IN) :: funit
      INTEGER(SIK),INTENT(IN),OPTIONAL :: indent
      CHARACTER(LEN=12) :: fmt,fmt2
      INTEGER(SIK) :: i,j,k

      i=1
      j=5
      IF(PRESENT(indent)) i=i+indent
      WRITE(fmt,'(i12)') i; fmt=ADJUSTL(fmt)
      WRITE(UNIT=funit,FMT='('//TRIM(fmt)//'x,a,g20.14)',ADVANCE='NO') &
        thisParam%dataType//' :: '//thisParam%name//'=',thisParam%val(1)
      j=j+LEN(thisParam%dataType)+LEN(thisParam%name)
      WRITE(fmt2,'(i12)') j; fmt2=ADJUSTL(fmt2)
      IF(SIZE(thisParam%val)>MAX_1D_LEN) THEN
        DO k=2,SIZE(thisParam%val)
          WRITE(UNIT=funit,FMT='(", ",g20.14)',ADVANCE='NO') thisParam%val(k)
        ENDDO

        IF(LEN_TRIM(thisParam%description) == 0) THEN
          WRITE(funit,*)
        ELSE
          WRITE(UNIT=funit,FMT='(a)') ' !'//thisParam%description
        ENDIF
      ELSE
        IF(LEN_TRIM(thisParam%description) == 0) THEN
          WRITE(funit,*)
        ELSE
          WRITE(UNIT=funit,FMT='(a)') ' !'//thisParam%description
        ENDIF
        DO k=2,SIZE(thisParam%val)
          WRITE(UNIT=funit,FMT='('//TRIM(fmt)//'x,'//TRIM(fmt2)//'x,g20.14)') &
            thisParam%val(k)
        ENDDO
      ENDIF

    ENDSUBROUTINE edit_ParamType_SDK_a1
!
!-------------------------------------------------------------------------------
!> @brief Clears a one dimensional array of double precision real valued parameter
!> @param thisParam the one dimensional array of double precision real valued parameter to clear
!>
    SUBROUTINE clear_ParamType_SDK_a1(thisParam)
      CLASS(ParamType_SDK_a1),INTENT(INOUT) :: thisParam
      DEALLOCATE(thisParam%val)
      thisParam%name=''
      thisParam%dataType=''
      thisParam%description=''
    ENDSUBROUTINE clear_ParamType_SDK_a1
!
!-------------------------------------------------------------------------------
!> @brief Sets the value of an existing one dimensional array of double precision real valued
!> parameter to a new value.
!> @param thisParam the parameter in which an existing parameter with name
!>        matching @c name will be to set the new value of @c param
!> @param name the name of an existing parameter to set the value of
!> @param param the new value to set for the parameter
!> @param description an optional new description for the parameter identified
!>        by @c name
!>
!> If a parameter with @c name is not found an error is produced. If the
!> parameter with @c name is not a one dimensional array of double precision real valued parameter
!> then an error is produced.
!>
    SUBROUTINE set_ParamType_SDK_a1(thisParam,name,param,description)
      CHARACTER(LEN=*),PARAMETER :: myName='set_ParamType_SDK_a1'
      CLASS(ParamType),INTENT(INOUT) :: thisParam
      CHARACTER(LEN=*),INTENT(IN) :: name
      REAL(SDK),INTENT(IN) :: param(:)
      CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
      CLASS(ParamType),POINTER :: tmpParam

      SELECTTYPE(thisParam)
        TYPE IS(ParamType_SDK_a1)
          IF(thisParam%name == TRIM(name)) THEN
            IF(SIZE(thisParam%val) /= SIZE(param)) THEN
              DEALLOCATE(thisParam%val)
              ALLOCATE(thisParam%val(SIZE(param)))
            ENDIF
            thisParam%val=param
            IF(PRESENT(description)) thisParam%description=TRIM(description)
          ELSE
            CALL eParams%raiseError(modName//'::'//myName// &
              ' - parameter name mismatch! Tried to set "'//TRIM(name)// &
                '" but name is "'//thisParam%name//'"!')
          ENDIF
        CLASS DEFAULT
          !Search for the parameter name
          CALL thisParam%getParam(name,tmpParam)
          IF(ASSOCIATED(tmpParam)) THEN
            !Parameter was found
            SELECTTYPE(p=>tmpParam)
              TYPE IS(ParamType_SDK_a1)
                IF(SIZE(p%val) /= SIZE(param)) THEN
                  DEALLOCATE(p%val)
                  ALLOCATE(p%val(SIZE(param)))
                ENDIF
                p%val=param
                IF(PRESENT(description)) p%description=TRIM(description)
              CLASS DEFAULT
                CALL eParams%raiseError(modName//'::'//myName// &
                  ' - parameter data type mismatch! Parameter '//TRIM(name)//' type is '// &
                    tmpParam%dataType//' and must be 1-D ARRAY REAL(SDK)!')
            ENDSELECT
          ELSE
            CALL eParams%raiseError(modName//'::'//myName// &
              ' - unable to locate parameter "'//TRIM(name)//'" in "'// &
                thisParam%name//'"!')
          ENDIF
      ENDSELECT
    ENDSUBROUTINE set_ParamType_SDK_a1
!
!-------------------------------------------------------------------------------
!> @brief Gets the one dimensional array of double precision real value for a specified parameter
!> @param thisParam the parameter in which an existing parameter with name
!>        matching @c name will have it's value returned
!> @param name the name of the parameter to return the value of
!> @param val the current value of the parameter with @c name
!>
!> If a parameter with @c name is not found an error is produced. If the
!> parameter with @c name is not a one dimensional array of double precision real valued parameter
!> then an error is produced.
!>
    SUBROUTINE get_ParamType_SDK_a1(thisParam,name,val)
      CHARACTER(LEN=*),PARAMETER :: myName='get_ParamType_SDK_a1'
      CLASS(ParamType),INTENT(IN) :: thisParam
      CHARACTER(LEN=*),INTENT(IN) :: name
      REAL(SDK),ALLOCATABLE,INTENT(INOUT) :: val(:)
      CLASS(ParamType),POINTER :: tmpParam

      SELECTTYPE(thisParam)
        TYPE IS(ParamType_SDK_a1)
          IF(thisParam%name == TRIM(name)) THEN
            IF(ALLOCATED(val)) THEN
              IF(SIZE(thisParam%val) /= SIZE(val)) THEN
                DEALLOCATE(val)
                ALLOCATE(val(SIZE(thisParam%val)))
              ENDIF
              val=thisParam%val
            ELSE
              ALLOCATE(val(SIZE(thisParam%val)))
              val=thisParam%val
            ENDIF
          ELSE
            CALL eParams%raiseError(modName//'::'//myName// &
              ' - parameter name mismatch "'//TRIM(name)//'" in "'// &
                thisParam%name//'"!')
          ENDIF
        CLASS DEFAULT
          !Search for the parameter name
          CALL thisParam%getParam(name,tmpParam)
          IF(ASSOCIATED(tmpParam)) THEN
            !Parameter was found
            SELECTTYPE(p=>tmpParam)
              TYPE IS(ParamType_SDK_a1)
                IF(ALLOCATED(val)) THEN
                  IF(SIZE(p%val) /= SIZE(val)) THEN
                    DEALLOCATE(val)
                    ALLOCATE(val(SIZE(p%val)))
                  ENDIF
                  val=p%val
                ELSE
                  ALLOCATE(val(SIZE(p%val)))
                  val=p%val
                ENDIF
              CLASS DEFAULT
                CALL eParams%raiseError(modName//'::'//myName// &
                  ' - parameter data type mismatch! Parameter '//TRIM(name)//' type is '// &
                    tmpParam%dataType//' and must be 1-D ARRAY REAL(SDK)!')
            ENDSELECT
          ELSE
            CALL eParams%raiseError(modName//'::'//myName// &
              ' - unable to locate parameter "'//TRIM(name)//'" in "'// &
                thisParam%name//'"!')
          ENDIF
      ENDSELECT
    ENDSUBROUTINE get_ParamType_SDK_a1
!
!-------------------------------------------------------------------------------
!> @brief Adds a new one dimensional array of double precision real valued parameter to a set of
!> parameters
!> @param thisParam the set of parameters to which a new parameter will be added
!> @param name the location and name of the new parameter
!> @param param the double precision real value of the new parameter
!> @param description an optional input for a description of the new parameter
!>
!> This routine creates a new parameter within @c thisParam with @c name.
!> @c name may contain a full or partial path to the new parameter. If @c name
!> can be matched to an existing parameter in @c thisParam an error is produced
!> If @c name contains a full path for which intermediate lists do not exist
!> then this lists are created in the process of adding the new parameter.
!> If @c thisParam is not initialized and @c name does not contain a "->"
!> symbol then this routine behaves equivalently to
!> @ref ParameterLists::init_ParamType_SDK "initSDK".
!>
    SUBROUTINE add_ParamType_SDK_a1(thisParam,name,param,description)
      CHARACTER(LEN=*),PARAMETER :: myName='add_ParamType_SDK_a1'
      CLASS(ParamType),INTENT(INOUT) :: thisParam
      CHARACTER(LEN=*),INTENT(IN) :: name
      REAL(SDK),INTENT(IN) :: param(:)
      CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
      CHARACTER(LEN=LEN(name)) :: prevname,thisname
      INTEGER(SIK) :: ipos
      TYPE(ParamType) :: newParam
      CLASS(ParamType),POINTER :: tmpParam

      !Search for the name to make sure it does not exist
      CALL get_ParamType(thisParam,name,tmpParam)

      IF(.NOT.ASSOCIATED(tmpParam)) THEN
        prevname=''
        thisname=ADJUSTL(name)
        ipos=INDEX(name,'->',.TRUE.)
        IF(ipos > 0) THEN
          prevname=ADJUSTL(name(1:ipos-1))
          thisname=ADJUSTL(name(ipos+2:LEN(name)))
        ENDIF

        !Initialize the new parameter
        CALL init_ParamType_SDK_a1(newParam,thisname,param,description)

        !Add the new parameter to thisParam
        CALL add_ParamType(thisParam,prevname,newParam)
        CALL newParam%clear()
      ELSE
        CALL eParams%raiseError(modName//'::'//myName// &
          ' - parameter name "'//TRIM(name)// &
            '" already exists! Use set method or full parameter list path!')
      ENDIF
    ENDSUBROUTINE add_ParamType_SDK_a1
!
!-------------------------------------------------------------------------------
!> @brief Initializes a ParamType object as a one dimensional array of 32-bit integer
!> @param thisParam the parameter to initialize
!> @param name the name of the parameter
!> @param param a one dimensional array of 32-bit integer
!> @param description an optional description for this parameter
!>
!> This routine is not recursive, so it is like setting a one dimensional array of parameter.
!> Therefore the name cannot contain the "->" symbol to indicate access to a
!> sub-list. @c thisParam must not already be inititalized.
!>
    SUBROUTINE init_ParamType_SNK_a1(thisParam,name,param,description)
      CHARACTER(LEN=*),PARAMETER :: myName='init_ParamType_SNK_a1'
      CLASS(ParamType),INTENT(INOUT) :: thisParam
      INTEGER(SNK),INTENT(IN) :: param(:)
      CHARACTER(LEN=*),INTENT(IN) :: name
      CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
      INTEGER(SIK) :: ipos

      IF(.NOT.ASSOCIATED(thisParam%pdat)) THEN
        !Check that '->' character is not in name
        ipos=INDEX(name,'->')
        IF(ipos == 0) THEN
          ALLOCATE(ParamType_SNK_a1 :: thisParam%pdat)
          thisParam%pdat%name=TRIM(name)
          IF(PRESENT(description)) thisParam%pdat%description=TRIM(description)
          thisParam%pdat%dataType='1-D ARRAY INTEGER(SNK)'
          SELECTTYPE(p=>thisParam%pdat)
            TYPE IS(ParamType_SNK_a1)
              ALLOCATE(p%val(SIZE(param)))
              p%val=param
          ENDSELECT
        ELSE
          CALL eParams%raiseError(modName//'::'//myName// &
            ' - "->" symbol is not allowed in name!')
        ENDIF
      ELSE
        CALL eParams%raiseError(modName//'::'//myName// &
          ' - parameter is already initialized! Use set method!')
      ENDIF
    ENDSUBROUTINE init_ParamType_SNK_a1
!
!-------------------------------------------------------------------------------
!> @brief Edits a one dimensional array of 32-bit integer valued parameter
!> @param thisParam the one dimensional array of 32-bit integer valued parameter to edit
!> @param funit the unit number to edit the parameter to
!> @param indent optional indicates the number of blank spaces to precede the
!>        beginning of text to edit.
!>
!> The formatted write uses the "general" edit descriptor so that 7 digits (one
!> more than the significant number in a 32-bit integer) are always
!> printed if the number is very large in absolute value engineering format
!> is used otherwise floating point form is used to write the value.
!>
    SUBROUTINE edit_ParamType_SNK_a1(thisParam,funit,indent)
      CLASS(ParamType_SNK_a1),INTENT(IN) :: thisParam
      INTEGER(SIK),INTENT(IN) :: funit
      INTEGER(SIK),INTENT(IN),OPTIONAL :: indent
      CHARACTER(LEN=12) :: fmt,fmt2
      INTEGER(SIK) :: i,j,k

      i=1
      j=5
      IF(PRESENT(indent)) i=i+indent
      WRITE(fmt,'(i12)') i; fmt=ADJUSTL(fmt)
      WRITE(UNIT=funit,FMT='('//TRIM(fmt)//'x,a,g13.7)',ADVANCE='NO') &
          thisParam%dataType//' :: '//thisParam%name//'=',thisParam%val(1)
      j=j+LEN(thisParam%dataType)+LEN(thisParam%name)
      WRITE(fmt2,'(i12)') j; fmt2=ADJUSTL(fmt2)
      IF(SIZE(thisParam%val)>MAX_1D_LEN) THEN
        DO k=2,SIZE(thisParam%val)
          WRITE(UNIT=funit,FMT='(", ",g13.7)',ADVANCE='NO') thisParam%val(k)
        ENDDO
        IF(LEN_TRIM(thisParam%description) == 0) THEN
          WRITE(funit,*)
        ELSE
          WRITE(UNIT=funit,FMT='(a)') ' !'//thisParam%description
        ENDIF
      ELSE
        IF(LEN_TRIM(thisParam%description) == 0) THEN
          WRITE(funit,*)
        ELSE
          WRITE(UNIT=funit,FMT='(a)') ' !'//thisParam%description
        ENDIF
        DO k=2,SIZE(thisParam%val)
          WRITE(UNIT=funit,FMT='('//TRIM(fmt)//'x,'//TRIM(fmt2)//'x,g13.7)') &
            thisParam%val(k)
        ENDDO
      ENDIF
    ENDSUBROUTINE edit_ParamType_SNK_a1
!
!-------------------------------------------------------------------------------
!> @brief Clears a one dimensional array of 32-bit integer valued parameter
!> @param thisParam the one dimensional array of 32-bit integer valued parameter to clear
!>
    SUBROUTINE clear_ParamType_SNK_a1(thisParam)
      CLASS(ParamType_SNK_a1),INTENT(INOUT) :: thisParam
      DEALLOCATE(thisParam%val)
      thisParam%name=''
      thisParam%dataType=''
      thisParam%description=''
    ENDSUBROUTINE clear_ParamType_SNK_a1
!
!-------------------------------------------------------------------------------
!> @brief Sets the value of an existing one dimensional array of 32-bit integer valued
!> parameter to a new value.
!> @param thisParam the parameter in which an existing parameter with name
!>        matching @c name will be to set the new value of @c param
!> @param name the name of an existing parameter to set the value of
!> @param param the new value to set for the parameter
!> @param description an optional new description for the parameter identified
!>        by @c name
!>
!> If a parameter with @c name is not found an error is produced. If the
!> parameter with @c name is not a one dimensional array of 32-bit integer valued parameter
!> then an error is produced.
!>
    SUBROUTINE set_ParamType_SNK_a1(thisParam,name,param,description)
      CHARACTER(LEN=*),PARAMETER :: myName='set_ParamType_SNK_a1'
      CLASS(ParamType),INTENT(INOUT) :: thisParam
      CHARACTER(LEN=*),INTENT(IN) :: name
      INTEGER(SNK),INTENT(IN) :: param(:)
      CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
      CLASS(ParamType),POINTER :: tmpParam

      SELECTTYPE(thisParam)
        TYPE IS(ParamType_SNK_a1)
          IF(thisParam%name == TRIM(name)) THEN
            IF(SIZE(thisParam%val) /= SIZE(param)) THEN
              DEALLOCATE(thisParam%val)
              ALLOCATE(thisParam%val(SIZE(param)))
            ENDIF
            thisParam%val=param
            IF(PRESENT(description)) thisParam%description=TRIM(description)
          ELSE
            CALL eParams%raiseError(modName//'::'//myName// &
              ' - parameter name mismatch! Tried to set "'//TRIM(name)// &
                '" but name is "'//thisParam%name//'"!')
          ENDIF
        CLASS DEFAULT
          !Search for the parameter name
          CALL thisParam%getParam(name,tmpParam)
          IF(ASSOCIATED(tmpParam)) THEN
            !Parameter was found
            SELECTTYPE(p=>tmpParam)
              TYPE IS(ParamType_SNK_a1)
                IF(SIZE(p%val) /= SIZE(param)) THEN
                  DEALLOCATE(p%val)
                  ALLOCATE(p%val(SIZE(param)))
                ENDIF
                p%val=param
                IF(PRESENT(description)) p%description=TRIM(description)
              CLASS DEFAULT
                CALL eParams%raiseError(modName//'::'//myName// &
                  ' - parameter data type mismatch! Parameter '//TRIM(name)//' type is '// &
                    tmpParam%dataType//' and must be 1-D ARRAY INTEGER(SNK)!')
            ENDSELECT
          ELSE
            CALL eParams%raiseError(modName//'::'//myName// &
              ' - unable to locate parameter "'//TRIM(name)//'" in "'// &
                thisParam%name//'"!')
          ENDIF
      ENDSELECT
    ENDSUBROUTINE set_ParamType_SNK_a1
!
!-------------------------------------------------------------------------------
!> @brief Gets the one dimensional array of 32-bit integer value for a specified parameter
!> @param thisParam the parameter in which an existing parameter with name
!>        matching @c name will have it's value returned
!> @param name the name of the parameter to return the value of
!> @param val the current value of the parameter with @c name
!>
!> If a parameter with @c name is not found an error is produced. If the
!> parameter with @c name is not a one dimensional array of 32-bit integer valued parameter
!> then an error is produced.
!>
    SUBROUTINE get_ParamType_SNK_a1(thisParam,name,val)
      CHARACTER(LEN=*),PARAMETER :: myName='get_ParamType_SNK_a1'
      CLASS(ParamType),INTENT(IN) :: thisParam
      CHARACTER(LEN=*),INTENT(IN) :: name
      INTEGER(SNK),ALLOCATABLE,INTENT(INOUT) :: val(:)
      CLASS(ParamType),POINTER :: tmpParam

      SELECTTYPE(thisParam)
        TYPE IS(ParamType_SNK_a1)
          IF(thisParam%name == TRIM(name)) THEN
            IF(ALLOCATED(val)) THEN
              IF(SIZE(thisParam%val) /= SIZE(val)) THEN
                DEALLOCATE(val)
                ALLOCATE(val(SIZE(thisParam%val)))
              ENDIF
              val=thisParam%val
            ELSE
              ALLOCATE(val(SIZE(thisParam%val)))
              val=thisParam%val
            ENDIF
          ELSE
            CALL eParams%raiseError(modName//'::'//myName// &
              ' - parameter name mismatch "'//TRIM(name)//'" in "'// &
                thisParam%name//'"!')
          ENDIF
        CLASS DEFAULT
          !Search for the parameter name
          CALL thisParam%getParam(name,tmpParam)
          IF(ASSOCIATED(tmpParam)) THEN
            !Parameter was found
            SELECTTYPE(p=>tmpParam)
              TYPE IS(ParamType_SNK_a1)
                IF(ALLOCATED(val)) THEN
                  IF(SIZE(p%val) /= SIZE(val)) THEN
                    DEALLOCATE(val)
                    ALLOCATE(val(SIZE(p%val)))
                  ENDIF
                  val=p%val
                ELSE
                  ALLOCATE(val(SIZE(p%val)))
                  val=p%val
                ENDIF
              CLASS DEFAULT
                CALL eParams%raiseError(modName//'::'//myName// &
                  ' - parameter data type mismatch! Parameter '//TRIM(name)//' type is '// &
                    tmpParam%dataType//' and must be 1-D ARRAY INTEGER(SNK)!')
            ENDSELECT
          ELSE
            CALL eParams%raiseError(modName//'::'//myName// &
              ' - unable to locate parameter "'//TRIM(name)//'" in "'// &
                thisParam%name//'"!')
          ENDIF
      ENDSELECT
    ENDSUBROUTINE get_ParamType_SNK_a1
!
!-------------------------------------------------------------------------------
!> @brief Adds a new one dimensional array of 32-bit integer valued parameter to a set of
!> parameters
!> @param thisParam the set of parameters to which a new parameter will be added
!> @param name the location and name of the new parameter
!> @param param the 32-bit integer value of the new parameter
!> @param description an optional input for a description of the new parameter
!>
!> This routine creates a new parameter within @c thisParam with @c name.
!> @c name may contain a full or partial path to the new parameter. If @c name
!> can be matched to an existing parameter in @c thisParam an error is produced
!> If @c name contains a full path for which intermediate lists do not exist
!> then this lists are created in the process of adding the new parameter.
!> If @c thisParam is not initialized and @c name does not contain a "->"
!> symbol then this routine behaves equivalently to
!> @ref ParameterLists::init_ParamType_SNK "initSNK".
!>
    SUBROUTINE add_ParamType_SNK_a1(thisParam,name,param,description)
      CHARACTER(LEN=*),PARAMETER :: myName='add_ParamType_SNK_a1'
      CLASS(ParamType),INTENT(INOUT) :: thisParam
      CHARACTER(LEN=*),INTENT(IN) :: name
      INTEGER(SNK),INTENT(IN) :: param(:)
      CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
      CHARACTER(LEN=LEN(name)) :: prevname,thisname
      INTEGER(SIK) :: ipos
      TYPE(ParamType) :: newParam
      CLASS(ParamType),POINTER :: tmpParam

      !Search for the name to make sure it does not exist
      CALL get_ParamType(thisParam,name,tmpParam)

      IF(.NOT.ASSOCIATED(tmpParam)) THEN
        prevname=''
        thisname=ADJUSTL(name)
        ipos=INDEX(name,'->',.TRUE.)
        IF(ipos > 0) THEN
          prevname=ADJUSTL(name(1:ipos-1))
          thisname=ADJUSTL(name(ipos+2:LEN(name)))
        ENDIF

        !Initialize the new parameter
        CALL init_ParamType_SNK_a1(newParam,thisname,param,description)

        !Add the new parameter to thisParam
        CALL add_ParamType(thisParam,prevname,newParam)
        CALL newParam%clear()
      ELSE
        CALL eParams%raiseError(modName//'::'//myName// &
          ' - parameter name "'//TRIM(name)// &
            '" already exists! Use set method or full parameter list path!')
      ENDIF
    ENDSUBROUTINE add_ParamType_SNK_a1
!
!-------------------------------------------------------------------------------
!> @brief Initializes a ParamType object as a one dimensional array of 64-bit integer
!> @param thisParam the parameter to initialize
!> @param name the name of the parameter
!> @param param a one dimensional array of 64-bit integer
!> @param description an optional description for this parameter
!>
!> This routine is not recursive, so it is like setting a one dimensional array of parameter.
!> Therefore the name cannot contain the "->" symbol to indicate access to a
!> sub-list. @c thisParam must not already be inititalized.
!>
    SUBROUTINE init_ParamType_SLK_a1(thisParam,name,param,description)
      CHARACTER(LEN=*),PARAMETER :: myName='init_ParamType_SLK_a1'
      CLASS(ParamType),INTENT(INOUT) :: thisParam
      INTEGER(SLK),INTENT(IN) :: param(:)
      CHARACTER(LEN=*),INTENT(IN) :: name
      CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
      INTEGER(SIK) :: ipos

      IF(.NOT.ASSOCIATED(thisParam%pdat)) THEN
        !Check that '->' character is not in name
        ipos=INDEX(name,'->')
        IF(ipos == 0) THEN
          ALLOCATE(ParamType_SLK_a1 :: thisParam%pdat)
          thisParam%pdat%name=TRIM(name)
          IF(PRESENT(description)) thisParam%pdat%description=TRIM(description)
          thisParam%pdat%dataType='1-D ARRAY INTEGER(SLK)'
          SELECTTYPE(p=>thisParam%pdat)
            TYPE IS(ParamType_SLK_a1)
              ALLOCATE(p%val(SIZE(param)))
              p%val=param
          ENDSELECT
        ELSE
          CALL eParams%raiseError(modName//'::'//myName// &
            ' - "->" symbol is not allowed in name!')
        ENDIF
      ELSE
        CALL eParams%raiseError(modName//'::'//myName// &
          ' - parameter is already initialized! Use set method!')
      ENDIF
    ENDSUBROUTINE init_ParamType_SLK_a1
!
!-------------------------------------------------------------------------------
!> @brief Edits a one dimensional array of 64-bit integer valued parameter
!> @param thisParam the one dimensional array of 64-bit integer valued parameter to edit
!> @param funit the unit number to edit the parameter to
!> @param indent optional indicates the number of blank spaces to precede the
!>        beginning of text to edit.
!>
!> The formatted write uses the "general" edit descriptor so that 7 digits (one
!> more than the significant number in a 64-bit integer) are always
!> printed if the number is very large in absolute value engineering format
!> is used otherwise floating point form is used to write the value.
!>
    SUBROUTINE edit_ParamType_SLK_a1(thisParam,funit,indent)
      CLASS(ParamType_SLK_a1),INTENT(IN) :: thisParam
      INTEGER(SIK),INTENT(IN) :: funit
      INTEGER(SIK),INTENT(IN),OPTIONAL :: indent
      CHARACTER(LEN=12) :: fmt,fmt2
      INTEGER(SIK) :: i,j,k

      i=1
      j=5
      IF(PRESENT(indent)) i=i+indent
      WRITE(fmt,'(i12)') i; fmt=ADJUSTL(fmt)
      WRITE(UNIT=funit,FMT='('//TRIM(fmt)//'x,a,g20.14)',ADVANCE='NO') &
        thisParam%dataType//' :: '//thisParam%name//'=',thisParam%val(1)
      j=j+LEN(thisParam%dataType)+LEN(thisParam%name)
      WRITE(fmt2,'(i12)') j; fmt2=ADJUSTL(fmt2)
      IF(SIZE(thisParam%val)>MAX_1D_LEN) THEN
        DO k=2,SIZE(thisParam%val)
          WRITE(UNIT=funit,FMT='(", ",g20.14)',ADVANCE='NO') thisParam%val(k)
        ENDDO

        IF(LEN_TRIM(thisParam%description) == 0) THEN
          WRITE(funit,*)
        ELSE
          WRITE(UNIT=funit,FMT='(a)') ' !'//thisParam%description
        ENDIF
      ELSE
        IF(LEN_TRIM(thisParam%description) == 0) THEN
          WRITE(funit,*)
        ELSE
          WRITE(UNIT=funit,FMT='(a)') ' !'//thisParam%description
        ENDIF
        DO k=2,SIZE(thisParam%val)
          WRITE(UNIT=funit,FMT='('//TRIM(fmt)//'x,'//TRIM(fmt2)//'x,g20.14)') &
            thisParam%val(k)
        ENDDO
      ENDIF
    ENDSUBROUTINE edit_ParamType_SLK_a1
!
!-------------------------------------------------------------------------------
!> @brief Clears a one dimensional array of 64-bit integer valued parameter
!> @param thisParam the one dimensional array of 64-bit integer valued parameter to clear
!>
    SUBROUTINE clear_ParamType_SLK_a1(thisParam)
      CLASS(ParamType_SLK_a1),INTENT(INOUT) :: thisParam
      DEALLOCATE(thisParam%val)
      thisParam%name=''
      thisParam%dataType=''
      thisParam%description=''
    ENDSUBROUTINE clear_ParamType_SLK_a1
!
!-------------------------------------------------------------------------------
!> @brief Sets the value of an existing one dimensional array of 64-bit integer valued
!> parameter to a new value.
!> @param thisParam the parameter in which an existing parameter with name
!>        matching @c name will be to set the new value of @c param
!> @param name the name of an existing parameter to set the value of
!> @param param the new value to set for the parameter
!> @param description an optional new description for the parameter identified
!>        by @c name
!>
!> If a parameter with @c name is not found an error is produced. If the
!> parameter with @c name is not a one dimensional array of 64-bit integer valued parameter
!> then an error is produced.
!>
    SUBROUTINE set_ParamType_SLK_a1(thisParam,name,param,description)
      CHARACTER(LEN=*),PARAMETER :: myName='set_ParamType_SLK_a1'
      CLASS(ParamType),INTENT(INOUT) :: thisParam
      CHARACTER(LEN=*),INTENT(IN) :: name
      INTEGER(SLK),INTENT(IN) :: param(:)
      CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
      CLASS(ParamType),POINTER :: tmpParam

      SELECTTYPE(thisParam)
        TYPE IS(ParamType_SLK_a1)
          IF(thisParam%name == TRIM(name)) THEN
            IF(SIZE(thisParam%val) /= SIZE(param)) THEN
              DEALLOCATE(thisParam%val)
              ALLOCATE(thisParam%val(SIZE(param)))
            ENDIF
            thisParam%val=param
            IF(PRESENT(description)) thisParam%description=TRIM(description)
          ELSE
            CALL eParams%raiseError(modName//'::'//myName// &
              ' - parameter name mismatch! Tried to set "'//TRIM(name)// &
                '" but name is "'//thisParam%name//'"!')
          ENDIF
        CLASS DEFAULT
          !Search for the parameter name
          CALL thisParam%getParam(name,tmpParam)
          IF(ASSOCIATED(tmpParam)) THEN
            !Parameter was found
            SELECTTYPE(p=>tmpParam)
              TYPE IS(ParamType_SLK_a1)
                IF(SIZE(p%val) /= SIZE(param)) THEN
                  DEALLOCATE(p%val)
                  ALLOCATE(p%val(SIZE(param)))
                ENDIF
                p%val=param
                IF(PRESENT(description)) p%description=TRIM(description)
              CLASS DEFAULT
                CALL eParams%raiseError(modName//'::'//myName// &
                  ' - parameter data type mismatch! Parameter '//TRIM(name)//' type is '// &
                    tmpParam%dataType//' and must be 1-D ARRAY INTEGER(SLK)!')
            ENDSELECT
          ELSE
            CALL eParams%raiseError(modName//'::'//myName// &
              ' - unable to locate parameter "'//TRIM(name)//'" in "'// &
                thisParam%name//'"!')
          ENDIF
      ENDSELECT
    ENDSUBROUTINE set_ParamType_SLK_a1
!
!-------------------------------------------------------------------------------
!> @brief Gets the one dimensional array of 64-bit integer value for a specified parameter
!> @param thisParam the parameter in which an existing parameter with name
!>        matching @c name will have it's value returned
!> @param name the name of the parameter to return the value of
!> @param val the current value of the parameter with @c name
!>
!> If a parameter with @c name is not found an error is produced. If the
!> parameter with @c name is not a one dimensional array of 64-bit integer valued parameter
!> then an error is produced.
!>
    SUBROUTINE get_ParamType_SLK_a1(thisParam,name,val)
      CHARACTER(LEN=*),PARAMETER :: myName='get_ParamType_SLK_a1'
      CLASS(ParamType),INTENT(IN) :: thisParam
      CHARACTER(LEN=*),INTENT(IN) :: name
      INTEGER(SLK),ALLOCATABLE,INTENT(INOUT) :: val(:)
      CLASS(ParamType),POINTER :: tmpParam

      SELECTTYPE(thisParam)
        TYPE IS(ParamType_SLK_a1)
          IF(thisParam%name == TRIM(name)) THEN
            IF(ALLOCATED(val)) THEN
              IF(SIZE(thisParam%val) /= SIZE(val)) THEN
                DEALLOCATE(val)
                ALLOCATE(val(SIZE(thisParam%val)))
              ENDIF
              val=thisParam%val
            ELSE
              ALLOCATE(val(SIZE(thisParam%val)))
              val=thisParam%val
            ENDIF
          ELSE
            CALL eParams%raiseError(modName//'::'//myName// &
              ' - parameter name mismatch "'//TRIM(name)//'" in "'// &
                thisParam%name//'"!')
          ENDIF
        CLASS DEFAULT
          !Search for the parameter name
          CALL thisParam%getParam(name,tmpParam)
          IF(ASSOCIATED(tmpParam)) THEN
            !Parameter was found
            SELECTTYPE(p=>tmpParam)
              TYPE IS(ParamType_SLK_a1)
                IF(ALLOCATED(val)) THEN
                  IF(SIZE(p%val) /= SIZE(val)) THEN
                    DEALLOCATE(val)
                    ALLOCATE(val(SIZE(p%val)))
                  ENDIF
                  val=p%val
                ELSE
                  ALLOCATE(val(SIZE(p%val)))
                  val=p%val
                ENDIF
              CLASS DEFAULT
                CALL eParams%raiseError(modName//'::'//myName// &
                  ' - parameter data type mismatch! Parameter '//TRIM(name)//' type is '// &
                    tmpParam%dataType//' and must be 1-D ARRAY INTEGER(SLK)!')
            ENDSELECT
          ELSE
            CALL eParams%raiseError(modName//'::'//myName// &
              ' - unable to locate parameter "'//TRIM(name)//'" in "'// &
                thisParam%name//'"!')
          ENDIF
      ENDSELECT
    ENDSUBROUTINE get_ParamType_SLK_a1
!
!-------------------------------------------------------------------------------
!> @brief Adds a new one dimensional array of 64-bit integer valued parameter to a set of
!> parameters
!> @param thisParam the set of parameters to which a new parameter will be added
!> @param name the location and name of the new parameter
!> @param param the 64-bit integer value of the new parameter
!> @param description an optional input for a description of the new parameter
!>
!> This routine creates a new parameter within @c thisParam with @c name.
!> @c name may contain a full or partial path to the new parameter. If @c name
!> can be matched to an existing parameter in @c thisParam an error is produced
!> If @c name contains a full path for which intermediate lists do not exist
!> then this lists are created in the process of adding the new parameter.
!> If @c thisParam is not initialized and @c name does not contain a "->"
!> symbol then this routine behaves equivalently to
!> @ref ParameterLists::init_ParamType_SLK "initSLK".
!>
    SUBROUTINE add_ParamType_SLK_a1(thisParam,name,param,description)
      CHARACTER(LEN=*),PARAMETER :: myName='add_ParamType_SLK_a1'
      CLASS(ParamType),INTENT(INOUT) :: thisParam
      CHARACTER(LEN=*),INTENT(IN) :: name
      INTEGER(SLK),INTENT(IN) :: param(:)
      CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
      CHARACTER(LEN=LEN(name)) :: prevname,thisname
      INTEGER(SIK) :: ipos
      TYPE(ParamType) :: newParam
      CLASS(ParamType),POINTER :: tmpParam

      !Search for the name to make sure it does not exist
      CALL get_ParamType(thisParam,name,tmpParam)

      IF(.NOT.ASSOCIATED(tmpParam)) THEN
        prevname=''
        thisname=ADJUSTL(name)
        ipos=INDEX(name,'->',.TRUE.)
        IF(ipos > 0) THEN
          prevname=ADJUSTL(name(1:ipos-1))
          thisname=ADJUSTL(name(ipos+2:LEN(name)))
        ENDIF

        !Initialize the new parameter
        CALL init_ParamType_SLK_a1(newParam,thisname,param,description)

        !Add the new parameter to thisParam
        CALL add_ParamType(thisParam,prevname,newParam)
        CALL newParam%clear()
      ELSE
        CALL eParams%raiseError(modName//'::'//myName// &
          ' - parameter name "'//TRIM(name)// &
            '" already exists! Use set method or full parameter list path!')
      ENDIF
    ENDSUBROUTINE add_ParamType_SLK_a1
!
!-------------------------------------------------------------------------------
!> @brief Initializes a ParamType object as a one dimensional array of logicals
!> @param thisParam the parameter to initialize
!> @param name the name of the parameter
!> @param param a one dimensional array of logicals
!> @param description an optional description for this parameter
!>
!> This routine is not recursive, so it is like setting a one dimensional array of parameter.
!> Therefore the name cannot contain the "->" symbol to indicate access to a
!> sub-list. @c thisParam must not already be inititalized.
!>
    SUBROUTINE init_ParamType_SBK_a1(thisParam,name,param,description)
      CHARACTER(LEN=*),PARAMETER :: myName='init_ParamType_SBK_a1'
      CLASS(ParamType),INTENT(INOUT) :: thisParam
      LOGICAL(SBK),INTENT(IN) :: param(:)
      CHARACTER(LEN=*),INTENT(IN) :: name
      CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
      INTEGER(SIK) :: ipos

      IF(.NOT.ASSOCIATED(thisParam%pdat)) THEN
        !Check that '->' character is not in name
        ipos=INDEX(name,'->')
        IF(ipos == 0) THEN
          ALLOCATE(ParamType_SBK_a1 :: thisParam%pdat)
          thisParam%pdat%name=TRIM(name)
          IF(PRESENT(description)) thisParam%pdat%description=TRIM(description)
          thisParam%pdat%dataType='1-D ARRAY LOGICAL(SBK)'
          SELECTTYPE(p=>thisParam%pdat)
            TYPE IS(ParamType_SBK_a1)
              ALLOCATE(p%val(SIZE(param)))
              p%val=param
          ENDSELECT
        ELSE
          CALL eParams%raiseError(modName//'::'//myName// &
            ' - "->" symbol is not allowed in name!')
        ENDIF
      ELSE
        CALL eParams%raiseError(modName//'::'//myName// &
          ' - parameter is already initialized! Use set method!')
      ENDIF
    ENDSUBROUTINE init_ParamType_SBK_a1
!
!-------------------------------------------------------------------------------
!> @brief Edits a one dimensional array of logical valued parameters
!> @param thisParam the one dimensional array of logical valued
!>        parameters to edit
!> @param funit the unit number to edit the parameter to
!> @param indent optional indicates the number of blank spaces to precede the
!>        beginning of text to edit.
!>
    SUBROUTINE edit_ParamType_SBK_a1(thisParam,funit,indent)
      CLASS(ParamType_SBK_a1),INTENT(IN) :: thisParam
      INTEGER(SIK),INTENT(IN) :: funit
      INTEGER(SIK),INTENT(IN),OPTIONAL :: indent
      CHARACTER(LEN=12) :: fmt,fmt2
      INTEGER(SIK) :: i,j,k

      i=1
      j=5
      IF(PRESENT(indent)) i=i+indent
      WRITE(fmt,'(i12)') i; fmt=ADJUSTL(fmt)
      WRITE(UNIT=funit,FMT='('//TRIM(fmt)//'x,a,l3)',ADVANCE='NO') &
        thisParam%dataType//' :: '//thisParam%name//'=',thisParam%val(1)
      j=j+LEN(thisParam%dataType)+LEN(thisParam%name)
      WRITE(fmt2,'(i12)') j; fmt2=ADJUSTL(fmt2)
      IF(SIZE(thisParam%val)>MAX_1D_LEN) THEN
        DO k=2,SIZE(thisParam%val)
          WRITE(UNIT=funit,FMT='(", ",l3)',ADVANCE='NO') &
            thisParam%val(k)
        ENDDO

        IF(LEN_TRIM(thisParam%description) == 0) THEN
          WRITE(funit,*)
        ELSE
          WRITE(UNIT=funit,FMT='('//TRIM(fmt)//'x,a,l3,a)') ' !'//thisParam%description
        ENDIF
      ELSE
        IF(LEN_TRIM(thisParam%description) == 0) THEN
          WRITE(funit,*)
        ELSE
          WRITE(UNIT=funit,FMT='('//TRIM(fmt)//'x,a,l3,a)') ' !'//thisParam%description
        ENDIF
        DO k=2,SIZE(thisParam%val)
          WRITE(UNIT=funit,FMT='('//TRIM(fmt)//'x,'//TRIM(fmt2)//'x,l3)') &
            thisParam%val(k)
        ENDDO
      ENDIF
    ENDSUBROUTINE edit_ParamType_SBK_a1
!
!-------------------------------------------------------------------------------
!> @brief Clears a one dimensional array of logical valued parameter
!> @param thisParam the one dimensional array of logical valued parameter to clear
!>
    SUBROUTINE clear_ParamType_SBK_a1(thisParam)
      CLASS(ParamType_SBK_a1),INTENT(INOUT) :: thisParam
      DEALLOCATE(thisParam%val)
      thisParam%name=''
      thisParam%dataType=''
      thisParam%description=''
    ENDSUBROUTINE clear_ParamType_SBK_a1
!
!-------------------------------------------------------------------------------
!> @brief Sets the value of an existing one dimensional array of logical valued
!> parameter to a new value.
!> @param thisParam the parameter in which an existing parameter with name
!>        matching @c name will be to set the new value of @c param
!> @param name the name of an existing parameter to set the value of
!> @param param the new value to set for the parameter
!> @param description an optional new description for the parameter identified
!>        by @c name
!>
!> If a parameter with @c name is not found an error is produced. If the
!> parameter with @c name is not a one dimensional array of logical valued parameter
!> then an error is produced.
!>
    SUBROUTINE set_ParamType_SBK_a1(thisParam,name,param,description)
      CHARACTER(LEN=*),PARAMETER :: myName='set_ParamType_SBK_a1'
      CLASS(ParamType),INTENT(INOUT) :: thisParam
      CHARACTER(LEN=*),INTENT(IN) :: name
      LOGICAL(SBK),INTENT(IN) :: param(:)
      CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
      CLASS(ParamType),POINTER :: tmpParam

      SELECTTYPE(thisParam)
        TYPE IS(ParamType_SBK_a1)
          IF(thisParam%name == TRIM(name)) THEN
            IF(SIZE(thisParam%val) /= SIZE(param)) THEN
              DEALLOCATE(thisParam%val)
              ALLOCATE(thisParam%val(SIZE(param)))
            ENDIF
            thisParam%val=param
            IF(PRESENT(description)) thisParam%description=TRIM(description)
          ELSE
            CALL eParams%raiseError(modName//'::'//myName// &
              ' - parameter name mismatch! Tried to set "'//TRIM(name)// &
                '" but name is "'//thisParam%name//'"!')
          ENDIF
        CLASS DEFAULT
          !Search for the parameter name
          CALL thisParam%getParam(name,tmpParam)
          IF(ASSOCIATED(tmpParam)) THEN
            !Parameter was found
            SELECTTYPE(p=>tmpParam)
              TYPE IS(ParamType_SBK_a1)
                IF(SIZE(p%val) /= SIZE(param)) THEN
                  DEALLOCATE(p%val)
                  ALLOCATE(p%val(SIZE(param)))
                ENDIF
                p%val=param
                IF(PRESENT(description)) p%description=TRIM(description)
              CLASS DEFAULT
                CALL eParams%raiseError(modName//'::'//myName// &
                  ' - parameter data type mismatch! Parameter '//TRIM(name)//' type is '// &
                    tmpParam%dataType//' and must be 1-D ARRAY LOGICAL(SBK)!')
            ENDSELECT
          ELSE
            CALL eParams%raiseError(modName//'::'//myName// &
              ' - unable to locate parameter "'//TRIM(name)//'" in "'// &
                thisParam%name//'"!')
          ENDIF
      ENDSELECT
    ENDSUBROUTINE set_ParamType_SBK_a1
!
!-------------------------------------------------------------------------------
!> @brief Gets the one dimensional array of logical value for a specified parameter
!> @param thisParam the parameter in which an existing parameter with name
!>        matching @c name will have it's value returned
!> @param name the name of the parameter to return the value of
!> @param val the current value of the parameter with @c name
!>
!> If a parameter with @c name is not found an error is produced. If the
!> parameter with @c name is not a one dimensional array of logical valued parameter
!> then an error is produced.
!>
    SUBROUTINE get_ParamType_SBK_a1(thisParam,name,val)
      CHARACTER(LEN=*),PARAMETER :: myName='get_ParamType_SBK_a1'
      CLASS(ParamType),INTENT(IN) :: thisParam
      CHARACTER(LEN=*),INTENT(IN) :: name
      LOGICAL(SBK),ALLOCATABLE,INTENT(INOUT) :: val(:)
      CLASS(ParamType),POINTER :: tmpParam

      SELECTTYPE(thisParam)
        TYPE IS(ParamType_SBK_a1)
          IF(thisParam%name == TRIM(name)) THEN
            IF(ALLOCATED(val)) THEN
              IF(SIZE(thisParam%val) /= SIZE(val)) THEN
                DEALLOCATE(val)
                ALLOCATE(val(SIZE(thisParam%val)))
              ENDIF
              val=thisParam%val
            ELSE
              ALLOCATE(val(SIZE(thisParam%val)))
              val=thisParam%val
            ENDIF
          ELSE
            CALL eParams%raiseError(modName//'::'//myName// &
              ' - parameter name mismatch "'//TRIM(name)//'" in "'// &
                thisParam%name//'"!')
          ENDIF
        CLASS DEFAULT
          !Search for the parameter name
          CALL thisParam%getParam(name,tmpParam)
          IF(ASSOCIATED(tmpParam)) THEN
            !Parameter was found
            SELECTTYPE(p=>tmpParam)
              TYPE IS(ParamType_SBK_a1)
                IF(ALLOCATED(val)) THEN
                  IF(SIZE(p%val) /= SIZE(val)) THEN
                    DEALLOCATE(val)
                    ALLOCATE(val(SIZE(p%val)))
                  ENDIF
                  val=p%val
                ELSE
                  ALLOCATE(val(SIZE(p%val)))
                  val=p%val
                ENDIF
              CLASS DEFAULT
                CALL eParams%raiseError(modName//'::'//myName// &
                  ' - parameter data type mismatch! Parameter '//TRIM(name)//' type is '// &
                    tmpParam%dataType//' and must be 1-D ARRAY LOGICAL(SBK)!')
            ENDSELECT
          ELSE
            CALL eParams%raiseError(modName//'::'//myName// &
              ' - unable to locate parameter "'//TRIM(name)//'" in "'// &
                thisParam%name//'"!')
          ENDIF
      ENDSELECT
    ENDSUBROUTINE get_ParamType_SBK_a1
!
!-------------------------------------------------------------------------------
!> @brief Adds a new one dimensional array of logical valued parameter to a set of
!> parameters
!> @param thisParam the set of parameters to which a new parameter will be added
!> @param name the location and name of the new parameter
!> @param param the logical value of the new parameter
!> @param description an optional input for a description of the new parameter
!>
!> This routine creates a new parameter within @c thisParam with @c name.
!> @c name may contain a full or partial path to the new parameter. If @c name
!> can be matched to an existing parameter in @c thisParam an error is produced
!> If @c name contains a full path for which intermediate lists do not exist
!> then this lists are created in the process of adding the new parameter.
!> If @c thisParam is not initialized and @c name does not contain a "->"
!> symbol then this routine behaves equivalently to
!> @ref ParameterLists::init_ParamType_SBK "initSBK".
!>
    SUBROUTINE add_ParamType_SBK_a1(thisParam,name,param,description)
      CHARACTER(LEN=*),PARAMETER :: myName='add_ParamType_SBK_a1'
      CLASS(ParamType),INTENT(INOUT) :: thisParam
      CHARACTER(LEN=*),INTENT(IN) :: name
      LOGICAL(SBK),INTENT(IN) :: param(:)
      CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
      CHARACTER(LEN=LEN(name)) :: prevname,thisname
      INTEGER(SIK) :: ipos
      TYPE(ParamType) :: newParam
      CLASS(ParamType),POINTER :: tmpParam

      !Search for the name to make sure it does not exist
      CALL get_ParamType(thisParam,name,tmpParam)

      IF(.NOT.ASSOCIATED(tmpParam)) THEN
        prevname=''
        thisname=ADJUSTL(name)
        ipos=INDEX(name,'->',.TRUE.)
        IF(ipos > 0) THEN
          prevname=ADJUSTL(name(1:ipos-1))
          thisname=ADJUSTL(name(ipos+2:LEN(name)))
        ENDIF

        !Initialize the new parameter
        CALL init_ParamType_SBK_a1(newParam,thisname,param,description)

        !Add the new parameter to thisParam
        CALL add_ParamType(thisParam,prevname,newParam)
        CALL newParam%clear()
      ELSE
        CALL eParams%raiseError(modName//'::'//myName// &
          ' - parameter name "'//TRIM(name)// &
            '" already exists! Use set method or full parameter list path!')
      ENDIF
    ENDSUBROUTINE add_ParamType_SBK_a1
!
!-------------------------------------------------------------------------------
!> @brief Initializes a ParamType object as a 1-D array string derived type
!> @param thisParam the parameter to initialize
!> @param name the name of the parameter
!> @param param a 1-D array string derived type
!> @param description an optional description for this parameter
!>
!> This routine is not recursive, so it is like setting a scalar parameter.
!> Therefore the name cannot contain the "->" symbol to indicate access to a
!> sub-list. @c thisParam must not already be inititalized.
!>
    SUBROUTINE init_ParamType_STR_a1(thisParam,name,param,description)
      CHARACTER(LEN=*),PARAMETER :: myName='init_ParamType_STR_a1'
      CLASS(ParamType),INTENT(INOUT) :: thisParam
      TYPE(StringType),INTENT(IN) :: param(:)
      CHARACTER(LEN=*),INTENT(IN) :: name
      CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
      INTEGER(SIK) :: ipos

      IF(.NOT.ASSOCIATED(thisParam%pdat)) THEN
        !Check that '->' character is not in name
        ipos=INDEX(name,'->')
        IF(ipos == 0) THEN
          ALLOCATE(ParamType_STR_a1 :: thisParam%pdat)
          thisParam%pdat%name=TRIM(name)
          IF(PRESENT(description)) thisParam%pdat%description=TRIM(description)
          thisParam%pdat%dataType='1-D ARRAY TYPE(StringType)'
          SELECTTYPE(p => thisParam%pdat); TYPE IS(ParamType_STR_a1)
              ALLOCATE(p%val(SIZE(param)))
              p%val=param
          ENDSELECT
        ELSE
          CALL eParams%raiseError(modName//'::'//myName// &
            ' - "->" symbol is not allowed in name!')
        ENDIF
      ELSE
        CALL eParams%raiseError(modName//'::'//myName// &
          ' - parameter is already initialized! Use set method!')
      ENDIF
    ENDSUBROUTINE init_ParamType_STR_a1
!
!-------------------------------------------------------------------------------
!> @brief Edits a string derived type parameter
!> @param thisParam the string derived type parameter to edit
!> @param funit the unit number to edit the parameter to
!> @param indent optional indicates the number of blank spaces to precede the
!>        beginning of text to edit.
!>
    SUBROUTINE edit_ParamType_STR_a1(thisParam,funit,indent)
      CLASS(ParamType_STR_a1),INTENT(IN) :: thisParam
      INTEGER(SIK),INTENT(IN) :: funit
      INTEGER(SIK),INTENT(IN),OPTIONAL :: indent
      CHARACTER(LEN=12) :: fmt,fmt2
      INTEGER(SIK) :: i,j,k

      i=1
      j=5
      IF(PRESENT(indent)) i=i+indent
      WRITE(fmt,'(i12)') i; fmt=ADJUSTL(fmt)
      WRITE(UNIT=funit,FMT='('//TRIM(fmt)//'x,a)',ADVANCE='NO') &
            thisParam%dataType//' :: '//thisParam%name//'='//CHAR(thisParam%val(1))
      j=j+LEN(thisParam%dataType)+LEN(thisParam%name)
      WRITE(fmt2,'(i12)') j; fmt2=ADJUSTL(fmt2)

      IF (SIZE(thisParam%val)>MAX_1D_LEN) THEN
        DO k=2,SIZE(thisParam%val)
          WRITE(UNIT=funit,FMT='(", ",a)',ADVANCE='NO') CHAR(thisParam%val(k))
        ENDDO

        IF(LEN_TRIM(thisParam%description) == 0) THEN
          WRITE(funit,*)
        ELSE
          WRITE(UNIT=funit,FMT='(a)') " !"//thisParam%description
        ENDIF
      ELSE
        IF(LEN_TRIM(thisParam%description) == 0) THEN
          WRITE(funit,*)
        ELSE
          WRITE(UNIT=funit,FMT='(a)') " !"//thisParam%description
        ENDIF
        DO k=2,SIZE(thisParam%val)
          WRITE(UNIT=funit,FMT='('//TRIM(fmt)//'x,'//TRIM(fmt2)//'x,a)') &
            CHAR(thisParam%val(k))
        ENDDO
      ENDIF

    ENDSUBROUTINE edit_ParamType_STR_a1
!
!-------------------------------------------------------------------------------
!> @brief Clears a string derived type parameter
!> @param thisParam the string derived type parameter to clear
!>
    SUBROUTINE clear_ParamType_STR_a1(thisParam)
      CLASS(ParamType_STR_a1),INTENT(INOUT) :: thisParam
      INTEGER(SIK) :: i
      DO i=SIZE(thisParam%val),1,-1
        thisParam%val(i)=''
      ENDDO
      DEALLOCATE(thisParam%val)
      thisParam%name=''
      thisParam%dataType=''
      thisParam%description=''
    ENDSUBROUTINE clear_ParamType_STR_a1
!
!-------------------------------------------------------------------------------
!> @brief Sets the value of an existing string derived type parameter to a new value.
!> @param thisParam the parameter in which an existing parameter with name
!>        matching @c name will be to set the new value of @c param
!> @param name the name of an existing parameter to set the value of
!> @param param the new value to set for the parameter
!> @param description an optional new description for the parameter identified
!>        by @c name
!>
!> If a parameter with @c name is not found an error is produced. If the
!> parameter with @c name is not a string derived type parameter
!> then an error is produced.
!>
    SUBROUTINE set_ParamType_STR_a1(thisParam,name,param,description)
      CHARACTER(LEN=*),PARAMETER :: myName='set_ParamType_STR_a1'
      CLASS(ParamType),INTENT(INOUT) :: thisParam
      CHARACTER(LEN=*),INTENT(IN) :: name
      TYPE(StringType),INTENT(IN) :: param(:)
      CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
      CLASS(ParamType),POINTER :: tmpParam

      SELECTTYPE(thisParam)
        TYPE IS(ParamType_STR_a1)
          IF(thisParam%name == TRIM(name)) THEN
            IF(SIZE(thisParam%val) /= SIZE(param)) THEN
              DEALLOCATE(thisParam%val)
              ALLOCATE(thisParam%val(SIZE(param)))
            ENDIF
            thisParam%val=param
            IF(PRESENT(description)) thisParam%description=TRIM(description)
          ELSE
            CALL eParams%raiseError(modName//'::'//myName// &
              ' - parameter name mismatch! Tried to set "'//TRIM(name)// &
                '" but name is "'//thisParam%name//'"!')
          ENDIF
        CLASS DEFAULT
          !Search for the parameter name
          CALL thisParam%getParam(name,tmpParam)
          IF(ASSOCIATED(tmpParam)) THEN
            !Parameter was found
            SELECTTYPE(p=>tmpParam)
              TYPE IS(ParamType_STR_a1)
                IF(SIZE(p%val) /= SIZE(param)) THEN
                  DEALLOCATE(p%val)
                  ALLOCATE(p%val(SIZE(param)))
                ENDIF
                p%val=param
                IF(PRESENT(description)) p%description=TRIM(description)
              CLASS DEFAULT
                CALL eParams%raiseError(modName//'::'//myName// &
                  ' - parameter data type mismatch! Parameter '//TRIM(name)//' type is '// &
                    tmpParam%dataType//' and must be 1-D ARRAY TYPE(StringType)!')
            ENDSELECT
          ELSE
            CALL eParams%raiseError(modName//'::'//myName// &
              ' - unable to locate parameter "'//TRIM(name)//'" in "'// &
                thisParam%name//'"!')
          ENDIF
      ENDSELECT
    ENDSUBROUTINE set_ParamType_STR_a1
!
!-------------------------------------------------------------------------------
!> @brief Gets the string derived type for a specified parameter
!> @param thisParam the parameter in which an existing parameter with name
!>        matching @c name will have it's value returned
!> @param name the name of the parameter to return the value of
!> @param val the current value of the parameter with @c name
!>
!> If a parameter with @c name is not found an error is produced. If the
!> parameter with @c name is not a string derived type parameter
!> then an error is produced.
!>
    SUBROUTINE get_ParamType_STR_a1(thisParam,name,val)
      CHARACTER(LEN=*),PARAMETER :: myName='get_ParamType_STR_a1'
      CLASS(ParamType),INTENT(IN) :: thisParam
      CHARACTER(LEN=*),INTENT(IN) :: name
      TYPE(StringType),ALLOCATABLE,INTENT(INOUT) :: val(:)
      CLASS(ParamType),POINTER :: tmpParam

      SELECTTYPE(thisParam)
        TYPE IS(ParamType_STR_a1)
          IF(thisParam%name == TRIM(name)) THEN
            IF(ALLOCATED(val)) THEN
              IF(SIZE(thisParam%val) /= SIZE(val)) THEN
                DEALLOCATE(val)
                ALLOCATE(val(SIZE(thisParam%val)))
              ENDIF
              val=thisParam%val
            ELSE
              ALLOCATE(val(SIZE(thisParam%val)))
              val=thisParam%val
            ENDIF
          ELSE
            CALL eParams%raiseError(modName//'::'//myName// &
              ' - parameter name mismatch "'//TRIM(name)//'" in "'// &
                thisParam%name//'"!')
          ENDIF
        CLASS DEFAULT
          !Search for the parameter name
          CALL thisParam%getParam(name,tmpParam)
          IF(ASSOCIATED(tmpParam)) THEN
            !Parameter was found
            SELECTTYPE(p=>tmpParam)
              TYPE IS(ParamType_STR_a1)
                IF(ALLOCATED(val)) THEN
                  IF(SIZE(p%val) /= SIZE(val)) THEN
                    DEALLOCATE(val)
                    ALLOCATE(val(SIZE(p%val)))
                  ENDIF
                  val=p%val
                ELSE
                  ALLOCATE(val(SIZE(p%val)))
                  val=p%val
                ENDIF
              CLASS DEFAULT
                CALL eParams%raiseError(modName//'::'//myName// &
                  ' - parameter data type mismatch! Parameter '//TRIM(name)//' type is '// &
                    tmpParam%dataType//' and must be 1-D ARRAY TYPE(StringType)!')
            ENDSELECT
          ELSE
            CALL eParams%raiseError(modName//'::'//myName// &
              ' - unable to locate parameter "'//TRIM(name)//'" in "'// &
                thisParam%name//'"!')
          ENDIF
      ENDSELECT
    ENDSUBROUTINE get_ParamType_STR_a1
!
!-------------------------------------------------------------------------------
!> @brief Adds a string derived type parameter to a set of parameters
!> @param thisParam the set of parameters to which a new parameter will be added
!> @param name the location and name of the new parameter
!> @param param the string derived type of the new parameter
!> @param description an optional input for a description of the new parameter
!>
!> This routine creates a new parameter within @c thisParam with @c name.
!> @c name may contain a full or partial path to the new parameter. If @c name
!> can be matched to an existing parameter in @c thisParam an error is produced
!> If @c name contains a full path for which intermediate lists do not exist
!> then this lists are created in the process of adding the new parameter.
!> If @c thisParam is not initialized and @c name does not contain a "->"
!> symbol then this routine behaves equivalently to
!> @ref ParameterLists::init_ParamType_STR_a1 "initSTR".
!>
    SUBROUTINE add_ParamType_STR_a1(thisParam,name,param,description)
      CHARACTER(LEN=*),PARAMETER :: myName='add_ParamType_STR_a1'
      CLASS(ParamType),INTENT(INOUT) :: thisParam
      CHARACTER(LEN=*),INTENT(IN) :: name
      TYPE(StringType),INTENT(IN) :: param(:)
      CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
      CHARACTER(LEN=LEN(name)) :: prevname,thisname
      INTEGER(SIK) :: ipos
      TYPE(ParamType) :: newParam
      CLASS(ParamType),POINTER :: tmpParam

      !Search for the name to make sure it does not exist
      CALL get_ParamType(thisParam,name,tmpParam)

      IF(.NOT.ASSOCIATED(tmpParam)) THEN
        prevname=''
        thisname=ADJUSTL(name)
        ipos=INDEX(name,'->',.TRUE.)
        IF(ipos > 0) THEN
          prevname=ADJUSTL(name(1:ipos-1))
          thisname=ADJUSTL(name(ipos+2:LEN(name)))
        ENDIF

        !Initialize the new parameter
        CALL init_ParamType_STR_a1(newParam,thisname,param,description)

        !Add the new parameter to thisParam
        CALL add_ParamType(thisParam,prevname,newParam)
        CALL newParam%clear()
      ELSE
        CALL eParams%raiseError(modName//'::'//myName// &
          ' - parameter name "'//TRIM(name)// &
            '" already exists! Use set method or full parameter list path!')
      ENDIF
    ENDSUBROUTINE add_ParamType_STR_a1
!
!2222222222222222222222222222222222222222222222222222222222222222222222222222222
!        Two Dimensional Arrays
!2222222222222222222222222222222222222222222222222222222222222222222222222222222
!
!-------------------------------------------------------------------------------
!> @brief Initializes a ParamType object as a two dimensional array of single
!>        precision reals
!> @param thisParam the parameter to initialize
!> @param name the name of the parameter
!> @param param a two dimensional array of single precision reals
!> @param description an optional description for this parameter
!>
!> This routine is not recursive, so it is like setting a two dimensional array of parameter.
!> Therefore the name cannot contain the "->" symbol to indicate access to a
!> sub-list. @c thisParam must not already be inititalized.
!>
    SUBROUTINE init_ParamType_SSK_a2(thisParam,name,param,description)
      CHARACTER(LEN=*),PARAMETER :: myName='init_ParamType_SSK_a2'
      CLASS(ParamType),INTENT(INOUT) :: thisParam
      REAL(SSK),INTENT(IN) :: param(:,:)
      CHARACTER(LEN=*),INTENT(IN) :: name
      CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
      INTEGER(SIK) :: ipos

      IF(.NOT.ASSOCIATED(thisParam%pdat)) THEN
        !Check that '->' character is not in name
        ipos=INDEX(name,'->')
        IF(ipos == 0) THEN
          ALLOCATE(ParamType_SSK_a2 :: thisParam%pdat)
          thisParam%pdat%name=TRIM(name)
          IF(PRESENT(description)) thisParam%pdat%description=TRIM(description)
          thisParam%pdat%dataType='2-D ARRAY REAL(SSK)'
          SELECTTYPE(p=>thisParam%pdat)
            TYPE IS(ParamType_SSK_a2)
              ALLOCATE(p%val(SIZE(param,1),SIZE(param,2)))
              p%val=param
          ENDSELECT
        ELSE
          CALL eParams%raiseError(modName//'::'//myName// &
            ' - "->" symbol is not allowed in name!')
        ENDIF
      ELSE
        CALL eParams%raiseError(modName//'::'//myName// &
          ' - parameter is already initialized! Use set method!')
      ENDIF
    ENDSUBROUTINE init_ParamType_SSK_a2
!
!-------------------------------------------------------------------------------
!> @brief Edits a two dimensional array of single precision real valued parameters
!> @param thisParam the two dimensional array of single precision real valued
!>        parameters to edit
!> @param funit the unit number to edit the parameter to
!> @param indent optional indicates the number of blank spaces to precede the
!>        beginning of text to edit.
!>
!> The formatted write uses the "general" edit descriptor so that 7 digits (two
!> more than the significant number in a single precision real) are always
!> printed if the number is very large in absolute value engineering format
!> is used otherwise floating point form is used to write the value.
!>
    SUBROUTINE edit_ParamType_SSK_a2(thisParam,funit,indent)
      CLASS(ParamType_SSK_a2),INTENT(IN) :: thisParam
      INTEGER(SIK),INTENT(IN) :: funit
      INTEGER(SIK),INTENT(IN),OPTIONAL :: indent
      CHARACTER(LEN=12) :: fmt,fmt2,fmt3
      INTEGER(SIK) :: i,j,k,l

      i=1
      j=6
      IF(PRESENT(indent)) i=i+indent
      WRITE(fmt,'(i12)') i; fmt=ADJUSTL(fmt)
      IF(LEN_TRIM(thisParam%description) == 0) THEN
        WRITE(UNIT=funit,FMT='('//TRIM(fmt)//'x,a)') &
          thisParam%dataType//' :: '//thisParam%name//'= ...'
      ELSE
        WRITE(UNIT=funit,FMT='('//TRIM(fmt)//'x,a,a)') &
          thisParam%dataType//' :: '//thisParam%name//'= ... !'//thisParam%description
      ENDIF
      j=j+LEN(thisParam%dataType)+LEN(thisParam%name)
      WRITE(fmt2,'(i12)') j; fmt2=ADJUSTL(fmt2)
      WRITE(fmt3,'(i12)') SIZE(thisParam%val,1); fmt3=ADJUSTL(fmt3)
      DO k=1,SIZE(thisParam%val,2)
        WRITE(UNIT=funit,FMT='('//TRIM(fmt)//'x,'//TRIM(fmt2)//'x,'// &
          TRIM(fmt3)//'(g13.7))') (thisParam%val(l,k),l=1,SIZE(thisParam%val,1))
      ENDDO
    ENDSUBROUTINE edit_ParamType_SSK_a2
!
!-------------------------------------------------------------------------------
!> @brief Clears a two dimensional array of single precision real valued parameter
!> @param thisParam the two dimensional array of single precision real valued parameter to clear
!>
    SUBROUTINE clear_ParamType_SSK_a2(thisParam)
      CLASS(ParamType_SSK_a2),INTENT(INOUT) :: thisParam
      DEALLOCATE(thisParam%val)
      thisParam%name=''
      thisParam%dataType=''
      thisParam%description=''
    ENDSUBROUTINE clear_ParamType_SSK_a2
!
!-------------------------------------------------------------------------------
!> @brief Sets the value of an existing two dimensional array of single precision real valued
!> parameter to a new value.
!> @param thisParam the parameter in which an existing parameter with name
!>        matching @c name will be to set the new value of @c param
!> @param name the name of an existing parameter to set the value of
!> @param param the new value to set for the parameter
!> @param description an optional new description for the parameter identified
!>        by @c name
!>
!> If a parameter with @c name is not found an error is produced. If the
!> parameter with @c name is not a two dimensional array of single precision real valued parameter
!> then an error is produced.
!>
    SUBROUTINE set_ParamType_SSK_a2(thisParam,name,param,description)
      CHARACTER(LEN=*),PARAMETER :: myName='set_ParamType_SSK_a2'
      CLASS(ParamType),INTENT(INOUT) :: thisParam
      CHARACTER(LEN=*),INTENT(IN) :: name
      REAL(SSK),INTENT(IN) :: param(:,:)
      CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
      CLASS(ParamType),POINTER :: tmpParam

      SELECTTYPE(thisParam)
        TYPE IS(ParamType_SSK_a2)
          IF(thisParam%name == TRIM(name)) THEN
            IF(SIZE(thisParam%val,1) /= SIZE(param,1) .OR. &
                 SIZE(thisParam%val,2) /= SIZE(param,2)) THEN
              DEALLOCATE(thisParam%val)
              ALLOCATE(thisParam%val(SIZE(param,1),SIZE(param,2)))
            ENDIF
            thisParam%val=param
            IF(PRESENT(description)) thisParam%description=TRIM(description)
          ELSE
            CALL eParams%raiseError(modName//'::'//myName// &
              ' - parameter name mismatch! Tried to set "'//TRIM(name)// &
                '" but name is "'//thisParam%name//'"!')
          ENDIF
        CLASS DEFAULT
          !Search for the parameter name
          CALL thisParam%getParam(name,tmpParam)
          IF(ASSOCIATED(tmpParam)) THEN
            !Parameter was found
            SELECTTYPE(p=>tmpParam)
              TYPE IS(ParamType_SSK_a2)
                IF(SIZE(p%val,1) /= SIZE(param,1) .OR. &
                     SIZE(p%val,2) /= SIZE(param,2)) THEN
                  DEALLOCATE(p%val)
                  ALLOCATE(p%val(SIZE(param,1),SIZE(param,2)))
                ENDIF
                p%val=param
                IF(PRESENT(description)) p%description=TRIM(description)
              CLASS DEFAULT
                CALL eParams%raiseError(modName//'::'//myName// &
                  ' - parameter data type mismatch! Parameter '//TRIM(name)//' type is '// &
                    tmpParam%dataType//' and must be 2-D ARRAY REAL(SSK)!')
            ENDSELECT
          ELSE
            CALL eParams%raiseError(modName//'::'//myName// &
              ' - unable to locate parameter "'//TRIM(name)//'" in "'// &
                thisParam%name//'"!')
          ENDIF
      ENDSELECT
    ENDSUBROUTINE set_ParamType_SSK_a2
!
!-------------------------------------------------------------------------------
!> @brief Gets the two dimensional array of single precision real value for a specified parameter
!> @param thisParam the parameter in which an existing parameter with name
!>        matching @c name will have it's value returned
!> @param name the name of the parameter to return the value of
!> @param val the current value of the parameter with @c name
!>
!> If a parameter with @c name is not found an error is produced. If the
!> parameter with @c name is not a two dimensional array of single precision real valued parameter
!> then an error is produced.
!>
    SUBROUTINE get_ParamType_SSK_a2(thisParam,name,val)
      CHARACTER(LEN=*),PARAMETER :: myName='get_ParamType_SSK_a2'
      CLASS(ParamType),INTENT(IN) :: thisParam
      CHARACTER(LEN=*),INTENT(IN) :: name
      REAL(SSK),ALLOCATABLE,INTENT(INOUT) :: val(:,:)
      CLASS(ParamType),POINTER :: tmpParam

      SELECTTYPE(thisParam)
        TYPE IS(ParamType_SSK_a2)
          IF(thisParam%name == TRIM(name)) THEN
            IF(ALLOCATED(val)) THEN
              IF(SIZE(thisParam%val,1) /= SIZE(val,1) .OR. &
                   SIZE(thisParam%val,2) /= SIZE(val,2)) THEN
                DEALLOCATE(val)
                ALLOCATE(val(SIZE(thisParam%val,1),SIZE(thisParam%val,2)))
              ENDIF
              val=thisParam%val
            ELSE
              ALLOCATE(val(SIZE(thisParam%val,1),SIZE(thisParam%val,2)))
              val=thisParam%val
            ENDIF
          ELSE
            CALL eParams%raiseError(modName//'::'//myName// &
              ' - parameter name mismatch "'//TRIM(name)//'" in "'// &
                thisParam%name//'"!')
          ENDIF
        CLASS DEFAULT
          !Search for the parameter name
          CALL thisParam%getParam(name,tmpParam)
          IF(ASSOCIATED(tmpParam)) THEN
            !Parameter was found
            SELECTTYPE(p=>tmpParam)
              TYPE IS(ParamType_SSK_a2)
                IF(ALLOCATED(val)) THEN
                  IF(SIZE(p%val,1) /= SIZE(val,1) .OR. &
                       SIZE(p%val,2) /= SIZE(val,2)) THEN
                    DEALLOCATE(val)
                    ALLOCATE(val(SIZE(p%val,1),SIZE(p%val,2)))
                  ENDIF
                  val=p%val
                ELSE
                  ALLOCATE(val(SIZE(p%val,1),SIZE(p%val,2)))
                  val=p%val
                ENDIF
              CLASS DEFAULT
                CALL eParams%raiseError(modName//'::'//myName// &
                  ' - parameter data type mismatch! Parameter '//TRIM(name)//' type is '// &
                    tmpParam%dataType//' and must be 2-D ARRAY REAL(SSK)!')
            ENDSELECT
          ELSE
            CALL eParams%raiseError(modName//'::'//myName// &
              ' - unable to locate parameter "'//TRIM(name)//'" in "'// &
                thisParam%name//'"!')
          ENDIF
      ENDSELECT
    ENDSUBROUTINE get_ParamType_SSK_a2
!
!-------------------------------------------------------------------------------
!> @brief Adds a new two dimensional array of single precision real valued parameter to a set of
!> parameters
!> @param thisParam the set of parameters to which a new parameter will be added
!> @param name the location and name of the new parameter
!> @param param the single precision real value of the new parameter
!> @param description an optional input for a description of the new parameter
!>
!> This routine creates a new parameter within @c thisParam with @c name.
!> @c name may contain a full or partial path to the new parameter. If @c name
!> can be matched to an existing parameter in @c thisParam an error is produced
!> If @c name contains a full path for which intermediate lists do not exist
!> then this lists are created in the process of adding the new parameter.
!> If @c thisParam is not initialized and @c name does not contain a "->"
!> symbol then this routine behaves equivalently to
!> @ref ParameterLists::init_ParamType_SSK "initSSK".
!>
    SUBROUTINE add_ParamType_SSK_a2(thisParam,name,param,description)
      CHARACTER(LEN=*),PARAMETER :: myName='add_ParamType_SSK_a2'
      CLASS(ParamType),INTENT(INOUT) :: thisParam
      CHARACTER(LEN=*),INTENT(IN) :: name
      REAL(SSK),INTENT(IN) :: param(:,:)
      CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
      CHARACTER(LEN=LEN(name)) :: prevname,thisname
      INTEGER(SIK) :: ipos
      TYPE(ParamType) :: newParam
      CLASS(ParamType),POINTER :: tmpParam

      !Search for the name to make sure it does not exist
      CALL get_ParamType(thisParam,name,tmpParam)

      IF(.NOT.ASSOCIATED(tmpParam)) THEN
        prevname=''
        thisname=ADJUSTL(name)
        ipos=INDEX(name,'->',.TRUE.)
        IF(ipos > 0) THEN
          prevname=ADJUSTL(name(1:ipos-1))
          thisname=ADJUSTL(name(ipos+2:LEN(name)))
        ENDIF

        !Initialize the new parameter
        CALL init_ParamType_SSK_a2(newParam,thisname,param,description)

        !Add the new parameter to thisParam
        CALL add_ParamType(thisParam,prevname,newParam)
        CALL newParam%clear()
      ELSE
        CALL eParams%raiseError(modName//'::'//myName// &
          ' - parameter name "'//TRIM(name)// &
            '" already exists! Use set method or full parameter list path!')
      ENDIF
    ENDSUBROUTINE add_ParamType_SSK_a2
!
!-------------------------------------------------------------------------------
!> @brief Initializes a ParamType object as a two dimensional array of double precision real
!> @param thisParam the parameter to initialize
!> @param name the name of the parameter
!> @param param a two dimensional array of double precision real
!> @param description an optional description for this parameter
!>
!> This routine is not recursive, so it is like setting a two dimensional array of parameter.
!> Therefore the name cannot contain the "->" symbol to indicate access to a
!> sub-list. @c thisParam must not already be inititalized.
!>
    SUBROUTINE init_ParamType_SDK_a2(thisParam,name,param,description)
      CHARACTER(LEN=*),PARAMETER :: myName='init_ParamType_SDK_a2'
      CLASS(ParamType),INTENT(INOUT) :: thisParam
      REAL(SDK),INTENT(IN) :: param(:,:)
      CHARACTER(LEN=*),INTENT(IN) :: name
      CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
      INTEGER(SIK) :: ipos

      IF(.NOT.ASSOCIATED(thisParam%pdat)) THEN
        !Check that '->' character is not in name
        ipos=INDEX(name,'->')
        IF(ipos == 0) THEN
          ALLOCATE(ParamType_SDK_a2 :: thisParam%pdat)
          thisParam%pdat%name=TRIM(name)
          IF(PRESENT(description)) thisParam%pdat%description=TRIM(description)
          thisParam%pdat%dataType='2-D ARRAY REAL(SDK)'
          SELECTTYPE(p=>thisParam%pdat)
            TYPE IS(ParamType_SDK_a2)
              ALLOCATE(p%val(SIZE(param,1),SIZE(param,2)))
              p%val=param
          ENDSELECT
        ELSE
          CALL eParams%raiseError(modName//'::'//myName// &
            ' - "->" symbol is not allowed in name!')
        ENDIF
      ELSE
        CALL eParams%raiseError(modName//'::'//myName// &
          ' - parameter is already initialized! Use set method!')
      ENDIF
    ENDSUBROUTINE init_ParamType_SDK_a2
!
!-------------------------------------------------------------------------------
!> @brief Edits a two dimensional array of double precision real valued parameter
!> @param thisParam the two dimensional array of double precision real valued parameter to edit
!> @param funit the unit number to edit the parameter to
!> @param indent optional indicates the number of blank spaces to precede the
!>        beginning of text to edit.
!>
!> The formatted write uses the "general" edit descriptor so that 7 digits (two
!> more than the significant number in a double precision real) are always
!> printed if the number is very large in absolute value engineering format
!> is used otherwise floating point form is used to write the value.
!>
    SUBROUTINE edit_ParamType_SDK_a2(thisParam,funit,indent)
      CLASS(ParamType_SDK_a2),INTENT(IN) :: thisParam
      INTEGER(SIK),INTENT(IN) :: funit
      INTEGER(SIK),INTENT(IN),OPTIONAL :: indent
      CHARACTER(LEN=12) :: fmt,fmt2,fmt3
      INTEGER(SIK) :: i,j,k,l

      i=1
      j=6
      IF(PRESENT(indent)) i=i+indent
      WRITE(fmt,'(i12)') i; fmt=ADJUSTL(fmt)
      IF(LEN_TRIM(thisParam%description) == 0) THEN
        WRITE(UNIT=funit,FMT='('//TRIM(fmt)//'x,a)') &
          thisParam%dataType//' :: '//thisParam%name//'= ...'
      ELSE
        WRITE(UNIT=funit,FMT='('//TRIM(fmt)//'x,a,a)') &
          thisParam%dataType//' :: '//thisParam%name//'= ... !'//thisParam%description
      ENDIF
      j=j+LEN(thisParam%dataType)+LEN(thisParam%name)
      WRITE(fmt2,'(i12)') j; fmt2=ADJUSTL(fmt2)
      WRITE(fmt3,'(i12)') SIZE(thisParam%val,1); fmt3=ADJUSTL(fmt3)
      DO k=1,SIZE(thisParam%val,2)
        WRITE(UNIT=funit,FMT='('//TRIM(fmt)//'x,'//TRIM(fmt2)//'x,'// &
          TRIM(fmt3)//'(g20.14))') (thisParam%val(l,k),l=1,SIZE(thisParam%val,1))
      ENDDO
    ENDSUBROUTINE edit_ParamType_SDK_a2
!
!-------------------------------------------------------------------------------
!> @brief Clears a two dimensional array of double precision real valued parameter
!> @param thisParam the two dimensional array of double precision real valued parameter to clear
!>
    SUBROUTINE clear_ParamType_SDK_a2(thisParam)
      CLASS(ParamType_SDK_a2),INTENT(INOUT) :: thisParam
      DEALLOCATE(thisParam%val)
      thisParam%name=''
      thisParam%dataType=''
      thisParam%description=''
    ENDSUBROUTINE clear_ParamType_SDK_a2
!
!-------------------------------------------------------------------------------
!> @brief Sets the value of an existing two dimensional array of double precision real valued
!> parameter to a new value.
!> @param thisParam the parameter in which an existing parameter with name
!>        matching @c name will be to set the new value of @c param
!> @param name the name of an existing parameter to set the value of
!> @param param the new value to set for the parameter
!> @param description an optional new description for the parameter identified
!>        by @c name
!>
!> If a parameter with @c name is not found an error is produced. If the
!> parameter with @c name is not a two dimensional array of double precision real valued parameter
!> then an error is produced.
!>
    SUBROUTINE set_ParamType_SDK_a2(thisParam,name,param,description)
      CHARACTER(LEN=*),PARAMETER :: myName='set_ParamType_SDK_a2'
      CLASS(ParamType),INTENT(INOUT) :: thisParam
      CHARACTER(LEN=*),INTENT(IN) :: name
      REAL(SDK),INTENT(IN) :: param(:,:)
      CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
      CLASS(ParamType),POINTER :: tmpParam

      SELECTTYPE(thisParam)
        TYPE IS(ParamType_SDK_a2)
          IF(thisParam%name == TRIM(name)) THEN
            IF(SIZE(thisParam%val,1) /= SIZE(param,1) .OR. &
                 SIZE(thisParam%val,2) /= SIZE(param,2)) THEN
              DEALLOCATE(thisParam%val)
              ALLOCATE(thisParam%val(SIZE(param,1),SIZE(param,2)))
            ENDIF
            thisParam%val=param
            IF(PRESENT(description)) thisParam%description=TRIM(description)
          ELSE
            CALL eParams%raiseError(modName//'::'//myName// &
              ' - parameter name mismatch! Tried to set "'//TRIM(name)// &
                '" but name is "'//thisParam%name//'"!')
          ENDIF
        CLASS DEFAULT
          !Search for the parameter name
          CALL thisParam%getParam(name,tmpParam)
          IF(ASSOCIATED(tmpParam)) THEN
            !Parameter was found
            SELECTTYPE(p=>tmpParam)
              TYPE IS(ParamType_SDK_a2)
                IF(SIZE(p%val,1) /= SIZE(param,1) .OR. &
                     SIZE(p%val,2) /= SIZE(param,2)) THEN
                  DEALLOCATE(p%val)
                  ALLOCATE(p%val(SIZE(param,1),SIZE(param,2)))
                ENDIF
                p%val=param
                IF(PRESENT(description)) p%description=TRIM(description)
              CLASS DEFAULT
                CALL eParams%raiseError(modName//'::'//myName// &
                  ' - parameter data type mismatch! Parameter '//TRIM(name)//' type is '// &
                    tmpParam%dataType//' and must be 2-D ARRAY REAL(SDK)!')
            ENDSELECT
          ELSE
            CALL eParams%raiseError(modName//'::'//myName// &
              ' - unable to locate parameter "'//TRIM(name)//'" in "'// &
                thisParam%name//'"!')
          ENDIF
      ENDSELECT
    ENDSUBROUTINE set_ParamType_SDK_a2
!
!-------------------------------------------------------------------------------
!> @brief Gets the two dimensional array of double precision real value for a specified parameter
!> @param thisParam the parameter in which an existing parameter with name
!>        matching @c name will have it's value returned
!> @param name the name of the parameter to return the value of
!> @param val the current value of the parameter with @c name
!>
!> If a parameter with @c name is not found an error is produced. If the
!> parameter with @c name is not a two dimensional array of double precision real valued parameter
!> then an error is produced.
!>
    SUBROUTINE get_ParamType_SDK_a2(thisParam,name,val)
      CHARACTER(LEN=*),PARAMETER :: myName='get_ParamType_SDK_a2'
      CLASS(ParamType),INTENT(IN) :: thisParam
      CHARACTER(LEN=*),INTENT(IN) :: name
      REAL(SDK),ALLOCATABLE,INTENT(INOUT) :: val(:,:)
      CLASS(ParamType),POINTER :: tmpParam

      SELECTTYPE(thisParam)
        TYPE IS(ParamType_SDK_a2)
          IF(thisParam%name == TRIM(name)) THEN
            IF(ALLOCATED(val)) THEN
              IF(SIZE(thisParam%val,1) /= SIZE(val,1) .OR. &
                   SIZE(thisParam%val,2) /= SIZE(val,2)) THEN
                DEALLOCATE(val)
                ALLOCATE(val(SIZE(thisParam%val,1),SIZE(thisParam%val,2)))
              ENDIF
              val=thisParam%val
            ELSE
              ALLOCATE(val(SIZE(thisParam%val,1),SIZE(thisParam%val,2)))
              val=thisParam%val
            ENDIF
          ELSE
            CALL eParams%raiseError(modName//'::'//myName// &
              ' - parameter name mismatch "'//TRIM(name)//'" in "'// &
                thisParam%name//'"!')
          ENDIF
        CLASS DEFAULT
          !Search for the parameter name
          CALL thisParam%getParam(name,tmpParam)
          IF(ASSOCIATED(tmpParam)) THEN
            !Parameter was found
            SELECTTYPE(p=>tmpParam)
              TYPE IS(ParamType_SDK_a2)
                IF(ALLOCATED(val)) THEN
                  IF(SIZE(p%val,1) /= SIZE(val,1) .OR. &
                       SIZE(p%val,2) /= SIZE(val,2)) THEN
                    DEALLOCATE(val)
                    ALLOCATE(val(SIZE(p%val,1),SIZE(p%val,2)))
                  ENDIF
                  val=p%val
                ELSE
                  ALLOCATE(val(SIZE(p%val,1),SIZE(p%val,2)))
                  val=p%val
                ENDIF
              CLASS DEFAULT
                CALL eParams%raiseError(modName//'::'//myName// &
                  ' - parameter data type mismatch! Parameter '//TRIM(name)//' type is '// &
                    tmpParam%dataType//' and must be 2-D ARRAY REAL(SDK)!')
            ENDSELECT
          ELSE
            CALL eParams%raiseError(modName//'::'//myName// &
              ' - unable to locate parameter "'//TRIM(name)//'" in "'// &
                thisParam%name//'"!')
          ENDIF
      ENDSELECT
    ENDSUBROUTINE get_ParamType_SDK_a2
!
!-------------------------------------------------------------------------------
!> @brief Adds a new two dimensional array of double precision real valued parameter to a set of
!> parameters
!> @param thisParam the set of parameters to which a new parameter will be added
!> @param name the location and name of the new parameter
!> @param param the double precision real value of the new parameter
!> @param description an optional input for a description of the new parameter
!>
!> This routine creates a new parameter within @c thisParam with @c name.
!> @c name may contain a full or partial path to the new parameter. If @c name
!> can be matched to an existing parameter in @c thisParam an error is produced
!> If @c name contains a full path for which intermediate lists do not exist
!> then this lists are created in the process of adding the new parameter.
!> If @c thisParam is not initialized and @c name does not contain a "->"
!> symbol then this routine behaves equivalently to
!> @ref ParameterLists::init_ParamType_SDK "initSDK".
!>
    SUBROUTINE add_ParamType_SDK_a2(thisParam,name,param,description)
      CHARACTER(LEN=*),PARAMETER :: myName='add_ParamType_SDK_a2'
      CLASS(ParamType),INTENT(INOUT) :: thisParam
      CHARACTER(LEN=*),INTENT(IN) :: name
      REAL(SDK),INTENT(IN) :: param(:,:)
      CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
      CHARACTER(LEN=LEN(name)) :: prevname,thisname
      INTEGER(SIK) :: ipos
      TYPE(ParamType) :: newParam
      CLASS(ParamType),POINTER :: tmpParam

      !Search for the name to make sure it does not exist
      CALL get_ParamType(thisParam,name,tmpParam)

      IF(.NOT.ASSOCIATED(tmpParam)) THEN
        prevname=''
        thisname=ADJUSTL(name)
        ipos=INDEX(name,'->',.TRUE.)
        IF(ipos > 0) THEN
          prevname=ADJUSTL(name(1:ipos-1))
          thisname=ADJUSTL(name(ipos+2:LEN(name)))
        ENDIF

        !Initialize the new parameter
        CALL init_ParamType_SDK_a2(newParam,thisname,param,description)

        !Add the new parameter to thisParam
        CALL add_ParamType(thisParam,prevname,newParam)
        CALL newParam%clear()
      ELSE
        CALL eParams%raiseError(modName//'::'//myName// &
          ' - parameter name "'//TRIM(name)// &
            '" already exists! Use set method or full parameter list path!')
      ENDIF
    ENDSUBROUTINE add_ParamType_SDK_a2
!
!-------------------------------------------------------------------------------
!> @brief Initializes a ParamType object as a two dimensional array of 32-bit integer
!> @param thisParam the parameter to initialize
!> @param name the name of the parameter
!> @param param a two dimensional array of 32-bit integer
!> @param description an optional description for this parameter
!>
!> This routine is not recursive, so it is like setting a two dimensional array of parameter.
!> Therefore the name cannot contain the "->" symbol to indicate access to a
!> sub-list. @c thisParam must not already be inititalized.
!>
    SUBROUTINE init_ParamType_SNK_a2(thisParam,name,param,description)
      CHARACTER(LEN=*),PARAMETER :: myName='init_ParamType_SNK_a2'
      CLASS(ParamType),INTENT(INOUT) :: thisParam
      INTEGER(SNK),INTENT(IN) :: param(:,:)
      CHARACTER(LEN=*),INTENT(IN) :: name
      CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
      INTEGER(SIK) :: ipos

      IF(.NOT.ASSOCIATED(thisParam%pdat)) THEN
        !Check that '->' character is not in name
        ipos=INDEX(name,'->')
        IF(ipos == 0) THEN
          ALLOCATE(ParamType_SNK_a2 :: thisParam%pdat)
          thisParam%pdat%name=TRIM(name)
          IF(PRESENT(description)) thisParam%pdat%description=TRIM(description)
          thisParam%pdat%dataType='2-D ARRAY INTEGER(SNK)'
          SELECTTYPE(p=>thisParam%pdat)
            TYPE IS(ParamType_SNK_a2)
              ALLOCATE(p%val(SIZE(param,1),SIZE(param,2)))
              p%val=param
          ENDSELECT
        ELSE
          CALL eParams%raiseError(modName//'::'//myName// &
            ' - "->" symbol is not allowed in name!')
        ENDIF
      ELSE
        CALL eParams%raiseError(modName//'::'//myName// &
          ' - parameter is already initialized! Use set method!')
      ENDIF
    ENDSUBROUTINE init_ParamType_SNK_a2
!
!-------------------------------------------------------------------------------
!> @brief Edits a two dimensional array of 32-bit integer valued parameter
!> @param thisParam the two dimensional array of 32-bit integer valued parameter to edit
!> @param funit the unit number to edit the parameter to
!> @param indent optional indicates the number of blank spaces to precede the
!>        beginning of text to edit.
!>
!> The formatted write uses the "general" edit descriptor so that 7 digits (two
!> more than the significant number in a 32-bit integer) are always
!> printed if the number is very large in absolute value engineering format
!> is used otherwise floating point form is used to write the value.
!>
    SUBROUTINE edit_ParamType_SNK_a2(thisParam,funit,indent)
      CLASS(ParamType_SNK_a2),INTENT(IN) :: thisParam
      INTEGER(SIK),INTENT(IN) :: funit
      INTEGER(SIK),INTENT(IN),OPTIONAL :: indent
      CHARACTER(LEN=12) :: fmt,fmt2,fmt3
      INTEGER(SIK) :: i,j,k,l

      i=1
      j=6
      IF(PRESENT(indent)) i=i+indent
      WRITE(fmt,'(i12)') i; fmt=ADJUSTL(fmt)
      IF(LEN_TRIM(thisParam%description) == 0) THEN
        WRITE(UNIT=funit,FMT='('//TRIM(fmt)//'x,a)') &
          thisParam%dataType//' :: '//thisParam%name//'= ...'
      ELSE
        WRITE(UNIT=funit,FMT='('//TRIM(fmt)//'x,a,a)') &
          thisParam%dataType//' :: '//thisParam%name//'= ... !'//thisParam%description
      ENDIF
      j=j+LEN(thisParam%dataType)+LEN(thisParam%name)
      WRITE(fmt2,'(i12)') j; fmt2=ADJUSTL(fmt2)
      WRITE(fmt3,'(i12)') SIZE(thisParam%val,1); fmt3=ADJUSTL(fmt3)
      DO k=1,SIZE(thisParam%val,2)
        WRITE(UNIT=funit,FMT='('//TRIM(fmt)//'x,'//TRIM(fmt2)//'x,'// &
          TRIM(fmt3)//'(g13.7))') (thisParam%val(l,k),l=1,SIZE(thisParam%val,1))
      ENDDO
    ENDSUBROUTINE edit_ParamType_SNK_a2
!
!-------------------------------------------------------------------------------
!> @brief Clears a two dimensional array of 32-bit integer valued parameter
!> @param thisParam the two dimensional array of 32-bit integer valued parameter to clear
!>
    SUBROUTINE clear_ParamType_SNK_a2(thisParam)
      CLASS(ParamType_SNK_a2),INTENT(INOUT) :: thisParam
      DEALLOCATE(thisParam%val)
      thisParam%name=''
      thisParam%dataType=''
      thisParam%description=''
    ENDSUBROUTINE clear_ParamType_SNK_a2
!
!-------------------------------------------------------------------------------
!> @brief Sets the value of an existing two dimensional array of 32-bit integer valued
!> parameter to a new value.
!> @param thisParam the parameter in which an existing parameter with name
!>        matching @c name will be to set the new value of @c param
!> @param name the name of an existing parameter to set the value of
!> @param param the new value to set for the parameter
!> @param description an optional new description for the parameter identified
!>        by @c name
!>
!> If a parameter with @c name is not found an error is produced. If the
!> parameter with @c name is not a two dimensional array of 32-bit integer valued parameter
!> then an error is produced.
!>
    SUBROUTINE set_ParamType_SNK_a2(thisParam,name,param,description)
      CHARACTER(LEN=*),PARAMETER :: myName='set_ParamType_SNK_a2'
      CLASS(ParamType),INTENT(INOUT) :: thisParam
      CHARACTER(LEN=*),INTENT(IN) :: name
      INTEGER(SNK),INTENT(IN) :: param(:,:)
      CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
      CLASS(ParamType),POINTER :: tmpParam

      SELECTTYPE(thisParam)
        TYPE IS(ParamType_SNK_a2)
          IF(thisParam%name == TRIM(name)) THEN
            IF(SIZE(thisParam%val,1) /= SIZE(param,1) .OR. &
                 SIZE(thisParam%val,2) /= SIZE(param,2)) THEN
              DEALLOCATE(thisParam%val)
              ALLOCATE(thisParam%val(SIZE(param,1),SIZE(param,2)))
            ENDIF
            thisParam%val=param
            IF(PRESENT(description)) thisParam%description=TRIM(description)
          ELSE
            CALL eParams%raiseError(modName//'::'//myName// &
              ' - parameter name mismatch! Tried to set "'//TRIM(name)// &
                '" but name is "'//thisParam%name//'"!')
          ENDIF
        CLASS DEFAULT
          !Search for the parameter name
          CALL thisParam%getParam(name,tmpParam)
          IF(ASSOCIATED(tmpParam)) THEN
            !Parameter was found
            SELECTTYPE(p=>tmpParam)
              TYPE IS(ParamType_SNK_a2)
                IF(SIZE(p%val,1) /= SIZE(param,1) .OR. &
                     SIZE(p%val,2) /= SIZE(param,2)) THEN
                  DEALLOCATE(p%val)
                  ALLOCATE(p%val(SIZE(param,1),SIZE(param,2)))
                ENDIF
                p%val=param
                IF(PRESENT(description)) p%description=TRIM(description)
              CLASS DEFAULT
                CALL eParams%raiseError(modName//'::'//myName// &
                  ' - parameter data type mismatch! Parameter '//TRIM(name)//' type is '// &
                    tmpParam%dataType//' and must be 2-D ARRAY INTEGER(SNK)!')
            ENDSELECT
          ELSE
            CALL eParams%raiseError(modName//'::'//myName// &
              ' - unable to locate parameter "'//TRIM(name)//'" in "'// &
                thisParam%name//'"!')
          ENDIF
      ENDSELECT
    ENDSUBROUTINE set_ParamType_SNK_a2
!
!-------------------------------------------------------------------------------
!> @brief Gets the two dimensional array of 32-bit integer value for a specified parameter
!> @param thisParam the parameter in which an existing parameter with name
!>        matching @c name will have it's value returned
!> @param name the name of the parameter to return the value of
!> @param val the current value of the parameter with @c name
!>
!> If a parameter with @c name is not found an error is produced. If the
!> parameter with @c name is not a two dimensional array of 32-bit integer valued parameter
!> then an error is produced.
!>
    SUBROUTINE get_ParamType_SNK_a2(thisParam,name,val)
      CHARACTER(LEN=*),PARAMETER :: myName='get_ParamType_SNK_a2'
      CLASS(ParamType),INTENT(IN) :: thisParam
      CHARACTER(LEN=*),INTENT(IN) :: name
      INTEGER(SNK),ALLOCATABLE,INTENT(INOUT) :: val(:,:)
      CLASS(ParamType),POINTER :: tmpParam

      SELECTTYPE(thisParam)
        TYPE IS(ParamType_SNK_a2)
          IF(thisParam%name == TRIM(name)) THEN
            IF(ALLOCATED(val)) THEN
              IF(SIZE(thisParam%val,1) /= SIZE(val,1) .OR. &
                   SIZE(thisParam%val,2) /= SIZE(val,2)) THEN
                DEALLOCATE(val)
                ALLOCATE(val(SIZE(thisParam%val,1),SIZE(thisParam%val,2)))
              ENDIF
              val=thisParam%val
            ELSE
              ALLOCATE(val(SIZE(thisParam%val,1),SIZE(thisParam%val,2)))
              val=thisParam%val
            ENDIF
          ELSE
            CALL eParams%raiseError(modName//'::'//myName// &
              ' - parameter name mismatch "'//TRIM(name)//'" in "'// &
                thisParam%name//'"!')
          ENDIF
        CLASS DEFAULT
          !Search for the parameter name
          CALL thisParam%getParam(name,tmpParam)
          IF(ASSOCIATED(tmpParam)) THEN
            !Parameter was found
            SELECTTYPE(p=>tmpParam)
              TYPE IS(ParamType_SNK_a2)
                IF(ALLOCATED(val)) THEN
                  IF(SIZE(p%val,1) /= SIZE(val,1) .OR. &
                       SIZE(p%val,2) /= SIZE(val,2)) THEN
                    DEALLOCATE(val)
                    ALLOCATE(val(SIZE(p%val,1),SIZE(p%val,2)))
                  ENDIF
                  val=p%val
                ELSE
                  ALLOCATE(val(SIZE(p%val,1),SIZE(p%val,2)))
                  val=p%val
                ENDIF
              CLASS DEFAULT
                CALL eParams%raiseError(modName//'::'//myName// &
                  ' - parameter data type mismatch! Parameter '//TRIM(name)//' type is '// &
                    tmpParam%dataType//' and must be 2-D ARRAY INTEGER(SNK)!')
            ENDSELECT
          ELSE
            CALL eParams%raiseError(modName//'::'//myName// &
              ' - unable to locate parameter "'//TRIM(name)//'" in "'// &
                thisParam%name//'"!')
          ENDIF
      ENDSELECT
    ENDSUBROUTINE get_ParamType_SNK_a2
!
!-------------------------------------------------------------------------------
!> @brief Adds a new two dimensional array of 32-bit integer valued parameter to a set of
!> parameters
!> @param thisParam the set of parameters to which a new parameter will be added
!> @param name the location and name of the new parameter
!> @param param the 32-bit integer value of the new parameter
!> @param description an optional input for a description of the new parameter
!>
!> This routine creates a new parameter within @c thisParam with @c name.
!> @c name may contain a full or partial path to the new parameter. If @c name
!> can be matched to an existing parameter in @c thisParam an error is produced
!> If @c name contains a full path for which intermediate lists do not exist
!> then this lists are created in the process of adding the new parameter.
!> If @c thisParam is not initialized and @c name does not contain a "->"
!> symbol then this routine behaves equivalently to
!> @ref ParameterLists::init_ParamType_SNK "initSNK".
!>
    SUBROUTINE add_ParamType_SNK_a2(thisParam,name,param,description)
      CHARACTER(LEN=*),PARAMETER :: myName='add_ParamType_SNK_a2'
      CLASS(ParamType),INTENT(INOUT) :: thisParam
      CHARACTER(LEN=*),INTENT(IN) :: name
      INTEGER(SNK),INTENT(IN) :: param(:,:)
      CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
      CHARACTER(LEN=LEN(name)) :: prevname,thisname
      INTEGER(SIK) :: ipos
      TYPE(ParamType) :: newParam
      CLASS(ParamType),POINTER :: tmpParam

      !Search for the name to make sure it does not exist
      CALL get_ParamType(thisParam,name,tmpParam)

      IF(.NOT.ASSOCIATED(tmpParam)) THEN
        prevname=''
        thisname=ADJUSTL(name)
        ipos=INDEX(name,'->',.TRUE.)
        IF(ipos > 0) THEN
          prevname=ADJUSTL(name(1:ipos-1))
          thisname=ADJUSTL(name(ipos+2:LEN(name)))
        ENDIF

        !Initialize the new parameter
        CALL init_ParamType_SNK_a2(newParam,thisname,param,description)

        !Add the new parameter to thisParam
        CALL add_ParamType(thisParam,prevname,newParam)
        CALL newParam%clear()
      ELSE
        CALL eParams%raiseError(modName//'::'//myName// &
          ' - parameter name "'//TRIM(name)// &
            '" already exists! Use set method or full parameter list path!')
      ENDIF
    ENDSUBROUTINE add_ParamType_SNK_a2
!
!-------------------------------------------------------------------------------
!> @brief Initializes a ParamType object as a two dimensional array of 64-bit integer
!> @param thisParam the parameter to initialize
!> @param name the name of the parameter
!> @param param a two dimensional array of 64-bit integer
!> @param description an optional description for this parameter
!>
!> This routine is not recursive, so it is like setting a two dimensional array of parameter.
!> Therefore the name cannot contain the "->" symbol to indicate access to a
!> sub-list. @c thisParam must not already be inititalized.
!>
    SUBROUTINE init_ParamType_SLK_a2(thisParam,name,param,description)
      CHARACTER(LEN=*),PARAMETER :: myName='init_ParamType_SLK_a2'
      CLASS(ParamType),INTENT(INOUT) :: thisParam
      INTEGER(SLK),INTENT(IN) :: param(:,:)
      CHARACTER(LEN=*),INTENT(IN) :: name
      CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
      INTEGER(SIK) :: ipos

      IF(.NOT.ASSOCIATED(thisParam%pdat)) THEN
        !Check that '->' character is not in name
        ipos=INDEX(name,'->')
        IF(ipos == 0) THEN
          ALLOCATE(ParamType_SLK_a2 :: thisParam%pdat)
          thisParam%pdat%name=TRIM(name)
          IF(PRESENT(description)) thisParam%pdat%description=TRIM(description)
          thisParam%pdat%dataType='2-D ARRAY INTEGER(SLK)'
          SELECTTYPE(p=>thisParam%pdat)
            TYPE IS(ParamType_SLK_a2)
              ALLOCATE(p%val(SIZE(param,1),SIZE(param,2)))
              p%val=param
          ENDSELECT
        ELSE
          CALL eParams%raiseError(modName//'::'//myName// &
            ' - "->" symbol is not allowed in name!')
        ENDIF
      ELSE
        CALL eParams%raiseError(modName//'::'//myName// &
          ' - parameter is already initialized! Use set method!')
      ENDIF
    ENDSUBROUTINE init_ParamType_SLK_a2
!
!-------------------------------------------------------------------------------
!> @brief Edits a two dimensional array of 64-bit integer valued parameter
!> @param thisParam the two dimensional array of 64-bit integer valued parameter to edit
!> @param funit the unit number to edit the parameter to
!> @param indent optional indicates the number of blank spaces to precede the
!>        beginning of text to edit.
!>
!> The formatted write uses the "general" edit descriptor so that 7 digits (two
!> more than the significant number in a 64-bit integer) are always
!> printed if the number is very large in absolute value engineering format
!> is used otherwise floating point form is used to write the value.
!>
    SUBROUTINE edit_ParamType_SLK_a2(thisParam,funit,indent)
      CLASS(ParamType_SLK_a2),INTENT(IN) :: thisParam
      INTEGER(SIK),INTENT(IN) :: funit
      INTEGER(SIK),INTENT(IN),OPTIONAL :: indent
      CHARACTER(LEN=12) :: fmt,fmt2,fmt3
      INTEGER(SIK) :: i,j,k,l

      i=1
      j=6
      IF(PRESENT(indent)) i=i+indent
      WRITE(fmt,'(i12)') i; fmt=ADJUSTL(fmt)
      IF(LEN_TRIM(thisParam%description) == 0) THEN
        WRITE(UNIT=funit,FMT='('//TRIM(fmt)//'x,a)') &
          thisParam%dataType//' :: '//thisParam%name//'= ...'
      ELSE
        WRITE(UNIT=funit,FMT='('//TRIM(fmt)//'x,a,a)') &
          thisParam%dataType//' :: '//thisParam%name//'= ... !'//thisParam%description
      ENDIF
      j=j+LEN(thisParam%dataType)+LEN(thisParam%name)
      WRITE(fmt2,'(i12)') j; fmt2=ADJUSTL(fmt2)
      WRITE(fmt3,'(i12)') SIZE(thisParam%val,1); fmt3=ADJUSTL(fmt3)
      DO k=1,SIZE(thisParam%val,2)
        WRITE(UNIT=funit,FMT='('//TRIM(fmt)//'x,'//TRIM(fmt2)//'x,'// &
          TRIM(fmt3)//'(g20.14))') (thisParam%val(l,k),l=1,SIZE(thisParam%val,1))
      ENDDO
    ENDSUBROUTINE edit_ParamType_SLK_a2
!
!-------------------------------------------------------------------------------
!> @brief Clears a two dimensional array of 64-bit integer valued parameter
!> @param thisParam the two dimensional array of 64-bit integer valued parameter to clear
!>
    SUBROUTINE clear_ParamType_SLK_a2(thisParam)
      CLASS(ParamType_SLK_a2),INTENT(INOUT) :: thisParam
      DEALLOCATE(thisParam%val)
      thisParam%name=''
      thisParam%dataType=''
      thisParam%description=''
    ENDSUBROUTINE clear_ParamType_SLK_a2
!
!-------------------------------------------------------------------------------
!> @brief Sets the value of an existing two dimensional array of 64-bit integer valued
!> parameter to a new value.
!> @param thisParam the parameter in which an existing parameter with name
!>        matching @c name will be to set the new value of @c param
!> @param name the name of an existing parameter to set the value of
!> @param param the new value to set for the parameter
!> @param description an optional new description for the parameter identified
!>        by @c name
!>
!> If a parameter with @c name is not found an error is produced. If the
!> parameter with @c name is not a two dimensional array of 64-bit integer valued parameter
!> then an error is produced.
!>
    SUBROUTINE set_ParamType_SLK_a2(thisParam,name,param,description)
      CHARACTER(LEN=*),PARAMETER :: myName='set_ParamType_SLK_a2'
      CLASS(ParamType),INTENT(INOUT) :: thisParam
      CHARACTER(LEN=*),INTENT(IN) :: name
      INTEGER(SLK),INTENT(IN) :: param(:,:)
      CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
      CLASS(ParamType),POINTER :: tmpParam

      SELECTTYPE(thisParam)
        TYPE IS(ParamType_SLK_a2)
          IF(thisParam%name == TRIM(name)) THEN
            IF(SIZE(thisParam%val,1) /= SIZE(param,1) .OR. &
                 SIZE(thisParam%val,2) /= SIZE(param,2)) THEN
              DEALLOCATE(thisParam%val)
              ALLOCATE(thisParam%val(SIZE(param,1),SIZE(param,2)))
            ENDIF
            thisParam%val=param
            IF(PRESENT(description)) thisParam%description=TRIM(description)
          ELSE
            CALL eParams%raiseError(modName//'::'//myName// &
              ' - parameter name mismatch! Tried to set "'//TRIM(name)// &
                '" but name is "'//thisParam%name//'"!')
          ENDIF
        CLASS DEFAULT
          !Search for the parameter name
          CALL thisParam%getParam(name,tmpParam)
          IF(ASSOCIATED(tmpParam)) THEN
            !Parameter was found
            SELECTTYPE(p=>tmpParam)
              TYPE IS(ParamType_SLK_a2)
                IF(SIZE(p%val,1) /= SIZE(param,1) .OR. &
                     SIZE(p%val,2) /= SIZE(param,2)) THEN
                  DEALLOCATE(p%val)
                  ALLOCATE(p%val(SIZE(param,1),SIZE(param,2)))
                ENDIF
                p%val=param
                IF(PRESENT(description)) p%description=TRIM(description)
              CLASS DEFAULT
                CALL eParams%raiseError(modName//'::'//myName// &
                  ' - parameter data type mismatch! Parameter '//TRIM(name)//' type is '// &
                    tmpParam%dataType//' and must be 2-D ARRAY INTEGER(SLK)!')
            ENDSELECT
          ELSE
            CALL eParams%raiseError(modName//'::'//myName// &
              ' - unable to locate parameter "'//TRIM(name)//'" in "'// &
                thisParam%name//'"!')
          ENDIF
      ENDSELECT
    ENDSUBROUTINE set_ParamType_SLK_a2
!
!-------------------------------------------------------------------------------
!> @brief Gets the two dimensional array of 64-bit integer value for a specified parameter
!> @param thisParam the parameter in which an existing parameter with name
!>        matching @c name will have it's value returned
!> @param name the name of the parameter to return the value of
!> @param val the current value of the parameter with @c name
!>
!> If a parameter with @c name is not found an error is produced. If the
!> parameter with @c name is not a two dimensional array of 64-bit integer valued parameter
!> then an error is produced.
!>
    SUBROUTINE get_ParamType_SLK_a2(thisParam,name,val)
      CHARACTER(LEN=*),PARAMETER :: myName='get_ParamType_SLK_a2'
      CLASS(ParamType),INTENT(IN) :: thisParam
      CHARACTER(LEN=*),INTENT(IN) :: name
      INTEGER(SLK),ALLOCATABLE,INTENT(INOUT) :: val(:,:)
      CLASS(ParamType),POINTER :: tmpParam

      SELECTTYPE(thisParam)
        TYPE IS(ParamType_SLK_a2)
          IF(thisParam%name == TRIM(name)) THEN
            IF(ALLOCATED(val)) THEN
              IF(SIZE(thisParam%val,1) /= SIZE(val,1) .OR. &
                   SIZE(thisParam%val,2) /= SIZE(val,2)) THEN
                DEALLOCATE(val)
                ALLOCATE(val(SIZE(thisParam%val,1),SIZE(thisParam%val,2)))
              ENDIF
              val=thisParam%val
            ELSE
              ALLOCATE(val(SIZE(thisParam%val,1),SIZE(thisParam%val,2)))
              val=thisParam%val
            ENDIF
          ELSE
            CALL eParams%raiseError(modName//'::'//myName// &
              ' - parameter name mismatch "'//TRIM(name)//'" in "'// &
                thisParam%name//'"!')
          ENDIF
        CLASS DEFAULT
          !Search for the parameter name
          CALL thisParam%getParam(name,tmpParam)
          IF(ASSOCIATED(tmpParam)) THEN
            !Parameter was found
            SELECTTYPE(p=>tmpParam)
              TYPE IS(ParamType_SLK_a2)
                IF(ALLOCATED(val)) THEN
                  IF(SIZE(p%val,1) /= SIZE(val,1) .OR. &
                       SIZE(p%val,2) /= SIZE(val,2)) THEN
                    DEALLOCATE(val)
                    ALLOCATE(val(SIZE(p%val,1),SIZE(p%val,2)))
                  ENDIF
                  val=p%val
                ELSE
                  ALLOCATE(val(SIZE(p%val,1),SIZE(p%val,2)))
                  val=p%val
                ENDIF
              CLASS DEFAULT
                CALL eParams%raiseError(modName//'::'//myName// &
                  ' - parameter data type mismatch! Parameter '//TRIM(name)//' type is '// &
                    tmpParam%dataType//' and must be 2-D ARRAY INTEGER(SLK)!')
            ENDSELECT
          ELSE
            CALL eParams%raiseError(modName//'::'//myName// &
              ' - unable to locate parameter "'//TRIM(name)//'" in "'// &
                thisParam%name//'"!')
          ENDIF
      ENDSELECT
    ENDSUBROUTINE get_ParamType_SLK_a2
!
!-------------------------------------------------------------------------------
!> @brief Adds a new two dimensional array of 64-bit integer valued parameter to a set of
!> parameters
!> @param thisParam the set of parameters to which a new parameter will be added
!> @param name the location and name of the new parameter
!> @param param the 64-bit integer value of the new parameter
!> @param description an optional input for a description of the new parameter
!>
!> This routine creates a new parameter within @c thisParam with @c name.
!> @c name may contain a full or partial path to the new parameter. If @c name
!> can be matched to an existing parameter in @c thisParam an error is produced
!> If @c name contains a full path for which intermediate lists do not exist
!> then this lists are created in the process of adding the new parameter.
!> If @c thisParam is not initialized and @c name does not contain a "->"
!> symbol then this routine behaves equivalently to
!> @ref ParameterLists::init_ParamType_SLK "initSLK".
!>
    SUBROUTINE add_ParamType_SLK_a2(thisParam,name,param,description)
      CHARACTER(LEN=*),PARAMETER :: myName='add_ParamType_SLK_a2'
      CLASS(ParamType),INTENT(INOUT) :: thisParam
      CHARACTER(LEN=*),INTENT(IN) :: name
      INTEGER(SLK),INTENT(IN) :: param(:,:)
      CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
      CHARACTER(LEN=LEN(name)) :: prevname,thisname
      INTEGER(SIK) :: ipos
      TYPE(ParamType) :: newParam
      CLASS(ParamType),POINTER :: tmpParam

      !Search for the name to make sure it does not exist
      CALL get_ParamType(thisParam,name,tmpParam)

      IF(.NOT.ASSOCIATED(tmpParam)) THEN
        prevname=''
        thisname=ADJUSTL(name)
        ipos=INDEX(name,'->',.TRUE.)
        IF(ipos > 0) THEN
          prevname=ADJUSTL(name(1:ipos-1))
          thisname=ADJUSTL(name(ipos+2:LEN(name)))
        ENDIF

        !Initialize the new parameter
        CALL init_ParamType_SLK_a2(newParam,thisname,param,description)

        !Add the new parameter to thisParam
        CALL add_ParamType(thisParam,prevname,newParam)
        CALL newParam%clear()
      ELSE
        CALL eParams%raiseError(modName//'::'//myName// &
          ' - parameter name "'//TRIM(name)// &
            '" already exists! Use set method or full parameter list path!')
      ENDIF
    ENDSUBROUTINE add_ParamType_SLK_a2
!
!-------------------------------------------------------------------------------
!> @brief Initializes a ParamType object as a 2-D array string derived type
!> @param thisParam the parameter to initialize
!> @param name the name of the parameter
!> @param param a 2-D array string derived type
!> @param description an optional description for this parameter
!>
!> This routine is not recursive, so it is like setting a scalar parameter.
!> Therefore the name cannot contain the "->" symbol to indicate access to a
!> sub-list. @c thisParam must not already be inititalized.
!>
    SUBROUTINE init_ParamType_STR_a2(thisParam,name,param,description)
      CHARACTER(LEN=*),PARAMETER :: myName='init_ParamType_STR_a2'
      CLASS(ParamType),INTENT(INOUT) :: thisParam
      TYPE(StringType),INTENT(IN) :: param(:,:)
      CHARACTER(LEN=*),INTENT(IN) :: name
      CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
      INTEGER(SIK) :: ipos

      IF(.NOT.ASSOCIATED(thisParam%pdat)) THEN
        !Check that '->' character is not in name
        ipos=INDEX(name,'->')
        IF(ipos == 0) THEN
          ALLOCATE(ParamType_STR_a2 :: thisParam%pdat)
          thisParam%pdat%name=TRIM(name)
          IF(PRESENT(description)) thisParam%pdat%description=TRIM(description)
          thisParam%pdat%dataType='2-D ARRAY TYPE(StringType)'
          SELECTTYPE(p => thisParam%pdat); TYPE IS(ParamType_STR_a2)
              ALLOCATE(p%val(SIZE(param,1),SIZE(param,2)))
              p%val=param
          ENDSELECT
        ELSE
          CALL eParams%raiseError(modName//'::'//myName// &
            ' - "->" symbol is not allowed in name!')
        ENDIF
      ELSE
        CALL eParams%raiseError(modName//'::'//myName// &
          ' - parameter is already initialized! Use set method!')
      ENDIF
    ENDSUBROUTINE init_ParamType_STR_a2
!
!-------------------------------------------------------------------------------
!> @brief Edits a string derived type parameter
!> @param thisParam the string derived type parameter to edit
!> @param funit the unit number to edit the parameter to
!> @param indent optional indicates the number of blank spaces to precede the
!>        beginning of text to edit.
!>
!>  The way this is set up could go horribly awry for printing things nicely.
!>  Check back later.
    SUBROUTINE edit_ParamType_STR_a2(thisParam,funit,indent)
      CLASS(ParamType_STR_a2),INTENT(IN) :: thisParam
      INTEGER(SIK),INTENT(IN) :: funit
      INTEGER(SIK),INTENT(IN),OPTIONAL :: indent
      CHARACTER(LEN=12) :: fmt,fmt2,fmt3
      INTEGER(SIK) :: i,j,k,l
      !Compiler problem for gnu-4.6.3.  It is fixed in gnu-4.7.0.
      !CHARACTER(LEN=MAXVAL(LEN(thisParam%val))) :: tmpstr(SIZE(thisParam%val))

      i=1
      j=6
      IF(PRESENT(indent)) i=i+indent
      WRITE(fmt,'(i12)') i; fmt=ADJUSTL(fmt)
      !tmpstr(1)=CHAR(thisParam%val(1))
      IF(LEN_TRIM(thisParam%description) == 0) THEN
        WRITE(UNIT=funit,FMT='('//TRIM(fmt)//'x,a)') &
          thisParam%dataType//' :: '//thisParam%name//'= ...'
      ELSE
        WRITE(UNIT=funit,FMT='('//TRIM(fmt)//'x,a,a)') &
          thisParam%dataType//' :: '//thisParam%name//'= ... !'//thisParam%description
      ENDIF
      j=j+LEN(thisParam%dataType)+LEN(thisParam%name)
      WRITE(fmt2,'(i12)') j; fmt2=ADJUSTL(fmt2)
      WRITE(fmt3,'(i12)') SIZE(thisParam%val,1); fmt3=ADJUSTL(fmt3)
      DO k=1,SIZE(thisParam%val,2)
        !DO l=1,SIZE(thisParam%val,1)
        !  WRITE(UNIT=funit,FMT='('//TRIM(fmt)//'x,'//TRIM(fmt2)//'x,a)') &
        !    TRIM(CHAR(thisParam%val(l,k)) ) )
        !ENDDO
        !Doesn't work sadly... syntax error in the format for some reason.
        WRITE(UNIT=funit,FMT='('//TRIM(fmt)//'x,'//TRIM(fmt2)//'x,'//TRIM(fmt3)//'a)') &
           (TRIM(CHAR(thisParam%val(l,k)))//' ',l=1,SIZE(thisParam%val,1) )
      ENDDO

    ENDSUBROUTINE edit_ParamType_STR_a2
!
!-------------------------------------------------------------------------------
!> @brief Clears a string derived type parameter
!> @param thisParam the string derived type parameter to clear
!>
    SUBROUTINE clear_ParamType_STR_a2(thisParam)
      CLASS(ParamType_STR_a2),INTENT(INOUT) :: thisParam
      INTEGER(SIK) :: i,j
      DO j=SIZE(thisParam%val,1),1,-1
        DO i=SIZE(thisParam%val,2),1,-1
          thisParam%val(j,i)=''
        ENDDO
      ENDDO
      DEALLOCATE(thisParam%val)
      thisParam%name=''
      thisParam%dataType=''
      thisParam%description=''
    ENDSUBROUTINE clear_ParamType_STR_a2
!
!-------------------------------------------------------------------------------
!> @brief Sets the value of an existing string derived type parameter to a new value.
!> @param thisParam the parameter in which an existing parameter with name
!>        matching @c name will be to set the new value of @c param
!> @param name the name of an existing parameter to set the value of
!> @param param the new value to set for the parameter
!> @param description an optional new description for the parameter identified
!>        by @c name
!>
!> If a parameter with @c name is not found an error is produced. If the
!> parameter with @c name is not a string derived type parameter
!> then an error is produced.
!>
    SUBROUTINE set_ParamType_STR_a2(thisParam,name,param,description)
      CHARACTER(LEN=*),PARAMETER :: myName='set_ParamType_STR_a2'
      CLASS(ParamType),INTENT(INOUT) :: thisParam
      CHARACTER(LEN=*),INTENT(IN) :: name
      TYPE(StringType),INTENT(IN) :: param(:,:)
      CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
      CLASS(ParamType),POINTER :: tmpParam

      SELECTTYPE(thisParam)
        TYPE IS(ParamType_STR_a2)
          IF(thisParam%name == TRIM(name)) THEN
            IF(SIZE(thisParam%val,1) /= SIZE(param,1) .OR. &
                 SIZE(thisParam%val,2) /= SIZE(param,2)) THEN
              DEALLOCATE(thisParam%val)
              ALLOCATE(thisParam%val(SIZE(param,1),SIZE(param,2)))
            ENDIF
            thisParam%val=param
            IF(PRESENT(description)) thisParam%description=TRIM(description)
          ELSE
            CALL eParams%raiseError(modName//'::'//myName// &
              ' - parameter name mismatch! Tried to set "'//TRIM(name)// &
                '" but name is "'//thisParam%name//'"!')
          ENDIF
        CLASS DEFAULT
          !Search for the parameter name
          CALL thisParam%getParam(name,tmpParam)
          IF(ASSOCIATED(tmpParam)) THEN
            !Parameter was found
            SELECTTYPE(p=>tmpParam)
              TYPE IS(ParamType_STR_a2)
                IF(SIZE(p%val,1) /= SIZE(param,1) .OR. &
                     SIZE(p%val,2) /= SIZE(param,2)) THEN
                  DEALLOCATE(p%val)
                  ALLOCATE(p%val(SIZE(param,1),SIZE(param,2)))
                ENDIF
                p%val=param
                IF(PRESENT(description)) p%description=TRIM(description)
              CLASS DEFAULT
                CALL eParams%raiseError(modName//'::'//myName// &
                  ' - parameter data type mismatch! Parameter '//TRIM(name)//' type is '// &
                    tmpParam%dataType//' and must be 2-D ARRAY TYPE(StringType)!')
            ENDSELECT
          ELSE
            CALL eParams%raiseError(modName//'::'//myName// &
              ' - unable to locate parameter "'//TRIM(name)//'" in "'// &
                thisParam%name//'"!')
          ENDIF
      ENDSELECT
    ENDSUBROUTINE set_ParamType_STR_a2
!
!-------------------------------------------------------------------------------
!> @brief Gets the string derived type for a specified parameter
!> @param thisParam the parameter in which an existing parameter with name
!>        matching @c name will have it's value returned
!> @param name the name of the parameter to return the value of
!> @param val the current value of the parameter with @c name
!>
!> If a parameter with @c name is not found an error is produced. If the
!> parameter with @c name is not a string derived type parameter
!> then an error is produced.
!>
    SUBROUTINE get_ParamType_STR_a2(thisParam,name,val)
      CHARACTER(LEN=*),PARAMETER :: myName='get_ParamType_STR_a2'
      CLASS(ParamType),INTENT(IN) :: thisParam
      CHARACTER(LEN=*),INTENT(IN) :: name
      TYPE(StringType),ALLOCATABLE,INTENT(INOUT) :: val(:,:)
      CLASS(ParamType),POINTER :: tmpParam

      SELECTTYPE(thisParam)
        TYPE IS(ParamType_STR_a2)
          IF(thisParam%name == TRIM(name)) THEN
            IF(ALLOCATED(val)) THEN
              IF(SIZE(thisParam%val,1) /= SIZE(val,1) .OR. &
                   SIZE(thisParam%val,2) /= SIZE(val,2)) THEN
                DEALLOCATE(val)
                ALLOCATE(val(SIZE(thisParam%val,1),SIZE(thisParam%val,2)))
              ENDIF
              val=thisParam%val
            ELSE
              ALLOCATE(val(SIZE(thisParam%val,1),SIZE(thisParam%val,2)))
              val=thisParam%val
            ENDIF
          ELSE
            CALL eParams%raiseError(modName//'::'//myName// &
              ' - parameter name mismatch "'//TRIM(name)//'" in "'// &
                thisParam%name//'"!')
          ENDIF
        CLASS DEFAULT
          !Search for the parameter name
          CALL thisParam%getParam(name,tmpParam)
          IF(ASSOCIATED(tmpParam)) THEN
            !Parameter was found
            SELECTTYPE(p=>tmpParam)
              TYPE IS(ParamType_STR_a2)
                IF(ALLOCATED(val)) THEN
                  IF(SIZE(p%val,1) /= SIZE(val,1) .OR. &
                       SIZE(p%val,2) /= SIZE(val,2)) THEN
                    DEALLOCATE(val)
                    ALLOCATE(val(SIZE(p%val,1),SIZE(p%val,2)))
                  ENDIF
                  val=p%val
                ELSE
                  ALLOCATE(val(SIZE(p%val,1),SIZE(p%val,2)))
                  val=p%val
                ENDIF
              CLASS DEFAULT
                CALL eParams%raiseError(modName//'::'//myName// &
                  ' - parameter data type mismatch! Parameter '//TRIM(name)//' type is '// &
                    tmpParam%dataType//' and must be 2-D ARRAY TYPE(StringType)!')
            ENDSELECT
          ELSE
            CALL eParams%raiseError(modName//'::'//myName// &
              ' - unable to locate parameter "'//TRIM(name)//'" in "'// &
                thisParam%name//'"!')
          ENDIF
      ENDSELECT
    ENDSUBROUTINE get_ParamType_STR_a2
!
!-------------------------------------------------------------------------------
!> @brief Adds a string derived type parameter to a set of parameters
!> @param thisParam the set of parameters to which a new parameter will be added
!> @param name the location and name of the new parameter
!> @param param the string derived type of the new parameter
!> @param description an optional input for a description of the new parameter
!>
!> This routine creates a new parameter within @c thisParam with @c name.
!> @c name may contain a full or partial path to the new parameter. If @c name
!> can be matched to an existing parameter in @c thisParam an error is produced
!> If @c name contains a full path for which intermediate lists do not exist
!> then this lists are created in the process of adding the new parameter.
!> If @c thisParam is not initialized and @c name does not contain a "->"
!> symbol then this routine behaves equivalently to
!> @ref ParameterLists::init_ParamType_STR_a2 "initSTR".
!>
    SUBROUTINE add_ParamType_STR_a2(thisParam,name,param,description)
      CHARACTER(LEN=*),PARAMETER :: myName='add_ParamType_STR_a2'
      CLASS(ParamType),INTENT(INOUT) :: thisParam
      CHARACTER(LEN=*),INTENT(IN) :: name
      TYPE(StringType),INTENT(IN) :: param(:,:)
      CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
      CHARACTER(LEN=LEN(name)) :: prevname,thisname
      INTEGER(SIK) :: ipos
      TYPE(ParamType) :: newParam
      CLASS(ParamType),POINTER :: tmpParam

      !Search for the name to make sure it does not exist
      CALL get_ParamType(thisParam,name,tmpParam)

      IF(.NOT.ASSOCIATED(tmpParam)) THEN
        prevname=''
        thisname=ADJUSTL(name)
        ipos=INDEX(name,'->',.TRUE.)
        IF(ipos > 0) THEN
          prevname=ADJUSTL(name(1:ipos-1))
          thisname=ADJUSTL(name(ipos+2:LEN(name)))
        ENDIF

        !Initialize the new parameter
        CALL init_ParamType_STR_a2(newParam,thisname,param,description)

        !Add the new parameter to thisParam
        CALL add_ParamType(thisParam,prevname,newParam)
        CALL newParam%clear()
      ELSE
        CALL eParams%raiseError(modName//'::'//myName// &
          ' - parameter name "'//TRIM(name)// &
            '" already exists! Use set method or full parameter list path!')
      ENDIF
    ENDSUBROUTINE add_ParamType_STR_a2
!
!3333333333333333333333333333333333333333333333333333333333333333333333333333333
!        Three Dimensional Arrays
!3333333333333333333333333333333333333333333333333333333333333333333333333333333
!
!-------------------------------------------------------------------------------
!> @brief Initializes a ParamType object as a three dimensional array of single
!>        precision reals
!> @param thisParam the parameter to initialize
!> @param name the name of the parameter
!> @param param a three dimensional array of single precision reals
!> @param description an optional description for this parameter
!>
!> This routine is not recursive, so it is like setting a three dimensional array of parameter.
!> Therefore the name cannot contain the "->" symbol to indicate access to a
!> sub-list. @c thisParam must not already be inititalized.
!>
    SUBROUTINE init_ParamType_SSK_a3(thisParam,name,param,description)
      CHARACTER(LEN=*),PARAMETER :: myName='init_ParamType_SSK_a3'
      CLASS(ParamType),INTENT(INOUT) :: thisParam
      REAL(SSK),INTENT(IN) :: param(:,:,:)
      CHARACTER(LEN=*),INTENT(IN) :: name
      CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
      INTEGER(SIK) :: ipos

      IF(.NOT.ASSOCIATED(thisParam%pdat)) THEN
        !Check that '->' character is not in name
        ipos=INDEX(name,'->')
        IF(ipos == 0) THEN
          ALLOCATE(ParamType_SSK_a3 :: thisParam%pdat)
          thisParam%pdat%name=TRIM(name)
          IF(PRESENT(description)) thisParam%pdat%description=TRIM(description)
          thisParam%pdat%dataType='3-D ARRAY REAL(SSK)'
          SELECTTYPE(p=>thisParam%pdat)
            TYPE IS(ParamType_SSK_a3)
              ALLOCATE(p%val(SIZE(param,1),SIZE(param,2),SIZE(param,3)))
              p%val=param
          ENDSELECT
        ELSE
          CALL eParams%raiseError(modName//'::'//myName// &
            ' - "->" symbol is not allowed in name!')
        ENDIF
      ELSE
        CALL eParams%raiseError(modName//'::'//myName// &
          ' - parameter is already initialized! Use set method!')
      ENDIF
    ENDSUBROUTINE init_ParamType_SSK_a3
!
!-------------------------------------------------------------------------------
!> @brief Edits a three dimensional array of single precision real valued parameters
!> @param thisParam the three dimensional array of single precision real valued
!>        parameters to edit
!> @param funit the unit number to edit the parameter to
!> @param indent optional indicates the number of blank spaces to precede the
!>        beginning of text to edit.
!>
!> The formatted write uses the "general" edit descriptor so that 7 digits (three
!> more than the significant number in a single precision real) are always
!> printed if the number is very large in absolute value engineering format
!> is used otherwise floating point form is used to write the value.
!>
    SUBROUTINE edit_ParamType_SSK_a3(thisParam,funit,indent)
      CLASS(ParamType_SSK_a3),INTENT(IN) :: thisParam
      INTEGER(SIK),INTENT(IN) :: funit
      INTEGER(SIK),INTENT(IN),OPTIONAL :: indent
      CHARACTER(LEN=12) :: fmt,fmt2,fmt3
      INTEGER(SIK) :: i,j,k,l,m

      i=1
      j=6
      IF(PRESENT(indent)) i=i+indent
      WRITE(fmt,'(i12)') i; fmt=ADJUSTL(fmt)
      IF(LEN_TRIM(thisParam%description) == 0) THEN
        WRITE(UNIT=funit,FMT='('//TRIM(fmt)//'x,a)') &
          thisParam%dataType//' :: '//thisParam%name//'= ...'
      ELSE
        WRITE(UNIT=funit,FMT='('//TRIM(fmt)//'x,a,a)') &
          thisParam%dataType//' :: '//thisParam%name//'= ... !'//thisParam%description
      ENDIF
      j=j+LEN(thisParam%dataType)+LEN(thisParam%name)
      WRITE(fmt2,'(i12)') j; fmt2=ADJUSTL(fmt2)
      WRITE(fmt3,'(i12)') SIZE(thisParam%val,1); fmt3=ADJUSTL(fmt3)
      DO k=1,SIZE(thisParam%val,3)
        DO l=1,SIZE(thisParam%val,2)
          WRITE(UNIT=funit,FMT='('//TRIM(fmt)//'x,'//TRIM(fmt2)//'x,'// &
            TRIM(fmt3)//'(g13.7))') (thisParam%val(m,l,k),m=1,SIZE(thisParam%val,1))
        ENDDO
      ENDDO
    ENDSUBROUTINE edit_ParamType_SSK_a3
!
!-------------------------------------------------------------------------------
!> @brief Clears a three dimensional array of single precision real valued parameter
!> @param thisParam the three dimensional array of single precision real valued parameter to clear
!>
    SUBROUTINE clear_ParamType_SSK_a3(thisParam)
      CLASS(ParamType_SSK_a3),INTENT(INOUT) :: thisParam
      DEALLOCATE(thisParam%val)
      thisParam%name=''
      thisParam%dataType=''
      thisParam%description=''
    ENDSUBROUTINE clear_ParamType_SSK_a3
!
!-------------------------------------------------------------------------------
!> @brief Sets the value of an existing three dimensional array of single precision real valued
!> parameter to a new value.
!> @param thisParam the parameter in which an existing parameter with name
!>        matching @c name will be to set the new value of @c param
!> @param name the name of an existing parameter to set the value of
!> @param param the new value to set for the parameter
!> @param description an optional new description for the parameter identified
!>        by @c name
!>
!> If a parameter with @c name is not found an error is produced. If the
!> parameter with @c name is not a three dimensional array of single precision real valued parameter
!> then an error is produced.
!>
    SUBROUTINE set_ParamType_SSK_a3(thisParam,name,param,description)
      CHARACTER(LEN=*),PARAMETER :: myName='set_ParamType_SSK_a3'
      CLASS(ParamType),INTENT(INOUT) :: thisParam
      CHARACTER(LEN=*),INTENT(IN) :: name
      REAL(SSK),INTENT(IN) :: param(:,:,:)
      CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
      CLASS(ParamType),POINTER :: tmpParam

      SELECTTYPE(thisParam)
        TYPE IS(ParamType_SSK_a3)
          IF(thisParam%name == TRIM(name)) THEN
            IF(SIZE(thisParam%val,1) /= SIZE(param,1) .OR. &
                 SIZE(thisParam%val,2) /= SIZE(param,2) .OR. &
                   SIZE(thisParam%val,3) /= SIZE(param,3)) THEN
              DEALLOCATE(thisParam%val)
              ALLOCATE(thisParam%val(SIZE(param,1),SIZE(param,2),SIZE(param,3)))
            ENDIF
            thisParam%val=param
            IF(PRESENT(description)) thisParam%description=TRIM(description)
          ELSE
            CALL eParams%raiseError(modName//'::'//myName// &
              ' - parameter name mismatch! Tried to set "'//TRIM(name)// &
                '" but name is "'//thisParam%name//'"!')
          ENDIF
        CLASS DEFAULT
          !Search for the parameter name
          CALL thisParam%getParam(name,tmpParam)
          IF(ASSOCIATED(tmpParam)) THEN
            !Parameter was found
            SELECTTYPE(p=>tmpParam)
              TYPE IS(ParamType_SSK_a3)
                IF(SIZE(p%val,1) /= SIZE(param,1) .OR. &
                     SIZE(p%val,2) /= SIZE(param,2) .OR. &
                       SIZE(p%val,3) /= SIZE(param,3)) THEN
                  DEALLOCATE(p%val)
                  ALLOCATE(p%val(SIZE(param,1),SIZE(param,2),SIZE(param,3)))
                ENDIF
                p%val=param
                IF(PRESENT(description)) p%description=TRIM(description)
              CLASS DEFAULT
                CALL eParams%raiseError(modName//'::'//myName// &
                  ' - parameter data type mismatch! Parameter '//TRIM(name)//' type is '// &
                    tmpParam%dataType//' and must be 3-D ARRAY REAL(SSK)!')
            ENDSELECT
          ELSE
            CALL eParams%raiseError(modName//'::'//myName// &
              ' - unable to locate parameter "'//TRIM(name)//'" in "'// &
                thisParam%name//'"!')
          ENDIF
      ENDSELECT
    ENDSUBROUTINE set_ParamType_SSK_a3
!
!-------------------------------------------------------------------------------
!> @brief Gets the three dimensional array of single precision real value for a specified parameter
!> @param thisParam the parameter in which an existing parameter with name
!>        matching @c name will have it's value returned
!> @param name the name of the parameter to return the value of
!> @param val the current value of the parameter with @c name
!>
!> If a parameter with @c name is not found an error is produced. If the
!> parameter with @c name is not a three dimensional array of single precision real valued parameter
!> then an error is produced.
!>
    SUBROUTINE get_ParamType_SSK_a3(thisParam,name,val)
      CHARACTER(LEN=*),PARAMETER :: myName='get_ParamType_SSK_a3'
      CLASS(ParamType),INTENT(IN) :: thisParam
      CHARACTER(LEN=*),INTENT(IN) :: name
      REAL(SSK),ALLOCATABLE,INTENT(INOUT) :: val(:,:,:)
      CLASS(ParamType),POINTER :: tmpParam

      SELECTTYPE(thisParam)
        TYPE IS(ParamType_SSK_a3)
          IF(thisParam%name == TRIM(name)) THEN
            IF(ALLOCATED(val)) THEN
              IF(SIZE(thisParam%val,1) /= SIZE(val,1) .OR. &
                   SIZE(thisParam%val,2) /= SIZE(val,2) .OR. &
                     SIZE(thisParam%val,3) /= SIZE(val,3)) THEN
                DEALLOCATE(val)
                ALLOCATE(val(SIZE(thisParam%val,1), &
                  SIZE(thisParam%val,2),SIZE(thisParam%val,3)))
              ENDIF
              val=thisParam%val
            ELSE
              ALLOCATE(val(SIZE(thisParam%val,1), &
                  SIZE(thisParam%val,2),SIZE(thisParam%val,3)))
              val=thisParam%val
            ENDIF
          ELSE
            CALL eParams%raiseError(modName//'::'//myName// &
              ' - parameter name mismatch "'//TRIM(name)//'" in "'// &
                thisParam%name//'"!')
          ENDIF
        CLASS DEFAULT
          !Search for the parameter name
          CALL thisParam%getParam(name,tmpParam)
          IF(ASSOCIATED(tmpParam)) THEN
            !Parameter was found
            SELECTTYPE(p=>tmpParam)
              TYPE IS(ParamType_SSK_a3)
                IF(ALLOCATED(val)) THEN
                  IF(SIZE(p%val,1) /= SIZE(val,1) .OR. &
                       SIZE(p%val,2) /= SIZE(val,2) .OR. &
                         SIZE(p%val,3) /= SIZE(val,3)) THEN
                    DEALLOCATE(val)
                    ALLOCATE(val(SIZE(p%val,1),SIZE(p%val,2),SIZE(p%val,3)))
                  ENDIF
                  val=p%val
                ELSE
                  ALLOCATE(val(SIZE(p%val,1),SIZE(p%val,2),SIZE(p%val,3)))
                  val=p%val
                ENDIF
              CLASS DEFAULT
                CALL eParams%raiseError(modName//'::'//myName// &
                  ' - parameter data type mismatch! Parameter '//TRIM(name)//' type is '// &
                    tmpParam%dataType//' and must be 3-D ARRAY REAL(SSK)!')
            ENDSELECT
          ELSE
            CALL eParams%raiseError(modName//'::'//myName// &
              ' - unable to locate parameter "'//TRIM(name)//'" in "'// &
                thisParam%name//'"!')
          ENDIF
      ENDSELECT
    ENDSUBROUTINE get_ParamType_SSK_a3
!
!-------------------------------------------------------------------------------
!> @brief Adds a new three dimensional array of single precision real valued parameter to a set of
!> parameters
!> @param thisParam the set of parameters to which a new parameter will be added
!> @param name the location and name of the new parameter
!> @param param the single precision real value of the new parameter
!> @param description an optional input for a description of the new parameter
!>
!> This routine creates a new parameter within @c thisParam with @c name.
!> @c name may contain a full or partial path to the new parameter. If @c name
!> can be matched to an existing parameter in @c thisParam an error is produced
!> If @c name contains a full path for which intermediate lists do not exist
!> then this lists are created in the process of adding the new parameter.
!> If @c thisParam is not initialized and @c name does not contain a "->"
!> symbol then this routine behaves equivalently to
!> @ref ParameterLists::init_ParamType_SSK "initSSK".
!>
    SUBROUTINE add_ParamType_SSK_a3(thisParam,name,param,description)
      CHARACTER(LEN=*),PARAMETER :: myName='add_ParamType_SSK_a3'
      CLASS(ParamType),INTENT(INOUT) :: thisParam
      CHARACTER(LEN=*),INTENT(IN) :: name
      REAL(SSK),INTENT(IN) :: param(:,:,:)
      CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
      CHARACTER(LEN=LEN(name)) :: prevname,thisname
      INTEGER(SIK) :: ipos
      TYPE(ParamType) :: newParam
      CLASS(ParamType),POINTER :: tmpParam

      !Search for the name to make sure it does not exist
      CALL get_ParamType(thisParam,name,tmpParam)

      IF(.NOT.ASSOCIATED(tmpParam)) THEN
        prevname=''
        thisname=ADJUSTL(name)
        ipos=INDEX(name,'->',.TRUE.)
        IF(ipos > 0) THEN
          prevname=ADJUSTL(name(1:ipos-1))
          thisname=ADJUSTL(name(ipos+2:LEN(name)))
        ENDIF

        !Initialize the new parameter
        CALL init_ParamType_SSK_a3(newParam,thisname,param,description)

        !Add the new parameter to thisParam
        CALL add_ParamType(thisParam,prevname,newParam)
        CALL newParam%clear()
      ELSE
        CALL eParams%raiseError(modName//'::'//myName// &
          ' - parameter name "'//TRIM(name)// &
            '" already exists! Use set method or full parameter list path!')
      ENDIF
    ENDSUBROUTINE add_ParamType_SSK_a3
!
!-------------------------------------------------------------------------------
!> @brief Initializes a ParamType object as a three dimensional array of double precision real
!> @param thisParam the parameter to initialize
!> @param name the name of the parameter
!> @param param a three dimensional array of double precision real
!> @param description an optional description for this parameter
!>
!> This routine is not recursive, so it is like setting a three dimensional array of parameter.
!> Therefore the name cannot contain the "->" symbol to indicate access to a
!> sub-list. @c thisParam must not already be inititalized.
!>
    SUBROUTINE init_ParamType_SDK_a3(thisParam,name,param,description)
      CHARACTER(LEN=*),PARAMETER :: myName='init_ParamType_SDK_a3'
      CLASS(ParamType),INTENT(INOUT) :: thisParam
      REAL(SDK),INTENT(IN) :: param(:,:,:)
      CHARACTER(LEN=*),INTENT(IN) :: name
      CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
      INTEGER(SIK) :: ipos

      IF(.NOT.ASSOCIATED(thisParam%pdat)) THEN
        !Check that '->' character is not in name
        ipos=INDEX(name,'->')
        IF(ipos == 0) THEN
          ALLOCATE(ParamType_SDK_a3 :: thisParam%pdat)
          thisParam%pdat%name=TRIM(name)
          IF(PRESENT(description)) thisParam%pdat%description=TRIM(description)
          thisParam%pdat%dataType='3-D ARRAY REAL(SDK)'
          SELECTTYPE(p=>thisParam%pdat)
            TYPE IS(ParamType_SDK_a3)
              ALLOCATE(p%val(SIZE(param,1),SIZE(param,2),SIZE(param,3)))
              p%val=param
          ENDSELECT
        ELSE
          CALL eParams%raiseError(modName//'::'//myName// &
            ' - "->" symbol is not allowed in name!')
        ENDIF
      ELSE
        CALL eParams%raiseError(modName//'::'//myName// &
          ' - parameter is already initialized! Use set method!')
      ENDIF
    ENDSUBROUTINE init_ParamType_SDK_a3
!
!-------------------------------------------------------------------------------
!> @brief Edits a three dimensional array of double precision real valued parameter
!> @param thisParam the three dimensional array of double precision real valued parameter to edit
!> @param funit the unit number to edit the parameter to
!> @param indent optional indicates the number of blank spaces to precede the
!>        beginning of text to edit.
!>
!> The formatted write uses the "general" edit descriptor so that 7 digits (three
!> more than the significant number in a double precision real) are always
!> printed if the number is very large in absolute value engineering format
!> is used otherwise floating point form is used to write the value.
!>
    SUBROUTINE edit_ParamType_SDK_a3(thisParam,funit,indent)
      CLASS(ParamType_SDK_a3),INTENT(IN) :: thisParam
      INTEGER(SIK),INTENT(IN) :: funit
      INTEGER(SIK),INTENT(IN),OPTIONAL :: indent
      CHARACTER(LEN=12) :: fmt,fmt2,fmt3
      INTEGER(SIK) :: i,j,k,l,m

      i=1
      j=6
      IF(PRESENT(indent)) i=i+indent
      WRITE(fmt,'(i12)') i; fmt=ADJUSTL(fmt)
      IF(LEN_TRIM(thisParam%description) == 0) THEN
        WRITE(UNIT=funit,FMT='('//TRIM(fmt)//'x,a)') &
          thisParam%dataType//' :: '//thisParam%name//'= ...'
      ELSE
        WRITE(UNIT=funit,FMT='('//TRIM(fmt)//'x,a,a)') &
          thisParam%dataType//' :: '//thisParam%name//'= ... !'//thisParam%description
      ENDIF
      j=j+LEN(thisParam%dataType)+LEN(thisParam%name)
      WRITE(fmt2,'(i12)') j; fmt2=ADJUSTL(fmt2)
      WRITE(fmt3,'(i12)') SIZE(thisParam%val,1); fmt3=ADJUSTL(fmt3)
      DO k=1,SIZE(thisParam%val,3)
        DO l=1,SIZE(thisParam%val,2)
          WRITE(UNIT=funit,FMT='('//TRIM(fmt)//'x,'//TRIM(fmt2)//'x,'// &
            TRIM(fmt3)//'(g20.14))') (thisParam%val(m,l,k),m=1,SIZE(thisParam%val,1))
        ENDDO
      ENDDO
    ENDSUBROUTINE edit_ParamType_SDK_a3
!
!-------------------------------------------------------------------------------
!> @brief Clears a three dimensional array of double precision real valued parameter
!> @param thisParam the three dimensional array of double precision real valued parameter to clear
!>
    SUBROUTINE clear_ParamType_SDK_a3(thisParam)
      CLASS(ParamType_SDK_a3),INTENT(INOUT) :: thisParam
      DEALLOCATE(thisParam%val)
      thisParam%name=''
      thisParam%dataType=''
      thisParam%description=''
    ENDSUBROUTINE clear_ParamType_SDK_a3
!
!-------------------------------------------------------------------------------
!> @brief Sets the value of an existing three dimensional array of double precision real valued
!> parameter to a new value.
!> @param thisParam the parameter in which an existing parameter with name
!>        matching @c name will be to set the new value of @c param
!> @param name the name of an existing parameter to set the value of
!> @param param the new value to set for the parameter
!> @param description an optional new description for the parameter identified
!>        by @c name
!>
!> If a parameter with @c name is not found an error is produced. If the
!> parameter with @c name is not a three dimensional array of double precision real valued parameter
!> then an error is produced.
!>
    SUBROUTINE set_ParamType_SDK_a3(thisParam,name,param,description)
      CHARACTER(LEN=*),PARAMETER :: myName='set_ParamType_SDK_a3'
      CLASS(ParamType),INTENT(INOUT) :: thisParam
      CHARACTER(LEN=*),INTENT(IN) :: name
      REAL(SDK),INTENT(IN) :: param(:,:,:)
      CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
      CLASS(ParamType),POINTER :: tmpParam

      SELECTTYPE(thisParam)
        TYPE IS(ParamType_SDK_a3)
          IF(thisParam%name == TRIM(name)) THEN
            IF(SIZE(thisParam%val,1) /= SIZE(param,1) .OR. &
                 SIZE(thisParam%val,2) /= SIZE(param,2) .OR. &
                   SIZE(thisParam%val,3) /= SIZE(param,3)) THEN
              DEALLOCATE(thisParam%val)
              ALLOCATE(thisParam%val(SIZE(param,1),SIZE(param,2),SIZE(param,3)))
            ENDIF
            thisParam%val=param
            IF(PRESENT(description)) thisParam%description=TRIM(description)
          ELSE
            CALL eParams%raiseError(modName//'::'//myName// &
              ' - parameter name mismatch! Tried to set "'//TRIM(name)// &
                '" but name is "'//thisParam%name//'"!')
          ENDIF
        CLASS DEFAULT
          !Search for the parameter name
          CALL thisParam%getParam(name,tmpParam)
          IF(ASSOCIATED(tmpParam)) THEN
            !Parameter was found
            SELECTTYPE(p=>tmpParam)
              TYPE IS(ParamType_SDK_a3)
                IF(SIZE(p%val,1) /= SIZE(param,1) .OR. &
                     SIZE(p%val,2) /= SIZE(param,2) .OR. &
                       SIZE(p%val,3) /= SIZE(param,3)) THEN
                  DEALLOCATE(p%val)
                  ALLOCATE(p%val(SIZE(param,1),SIZE(param,2),SIZE(param,3)))
                ENDIF
                p%val=param
                IF(PRESENT(description)) p%description=TRIM(description)
              CLASS DEFAULT
                CALL eParams%raiseError(modName//'::'//myName// &
                  ' - parameter data type mismatch! Parameter '//TRIM(name)//' type is '// &
                    tmpParam%dataType//' and must be 3-D ARRAY REAL(SDK)!')
            ENDSELECT
          ELSE
            CALL eParams%raiseError(modName//'::'//myName// &
              ' - unable to locate parameter "'//TRIM(name)//'" in "'// &
                thisParam%name//'"!')
          ENDIF
      ENDSELECT
    ENDSUBROUTINE set_ParamType_SDK_a3
!
!-------------------------------------------------------------------------------
!> @brief Gets the three dimensional array of double precision real value for a specified parameter
!> @param thisParam the parameter in which an existing parameter with name
!>        matching @c name will have it's value returned
!> @param name the name of the parameter to return the value of
!> @param val the current value of the parameter with @c name
!>
!> If a parameter with @c name is not found an error is produced. If the
!> parameter with @c name is not a three dimensional array of double precision real valued parameter
!> then an error is produced.
!>
    SUBROUTINE get_ParamType_SDK_a3(thisParam,name,val)
      CHARACTER(LEN=*),PARAMETER :: myName='get_ParamType_SDK_a3'
      CLASS(ParamType),INTENT(IN) :: thisParam
      CHARACTER(LEN=*),INTENT(IN) :: name
      REAL(SDK),ALLOCATABLE,INTENT(INOUT) :: val(:,:,:)
      CLASS(ParamType),POINTER :: tmpParam

      SELECTTYPE(thisParam)
        TYPE IS(ParamType_SDK_a3)
          IF(thisParam%name == TRIM(name)) THEN
            IF(ALLOCATED(val)) THEN
              IF(SIZE(thisParam%val,1) /= SIZE(val,1) .OR. &
                   SIZE(thisParam%val,2) /= SIZE(val,2) .OR. &
                     SIZE(thisParam%val,3) /= SIZE(val,3)) THEN
                DEALLOCATE(val)
                ALLOCATE(val(SIZE(thisParam%val,1), &
                  SIZE(thisParam%val,2),SIZE(thisParam%val,3)))
              ENDIF
              val=thisParam%val
            ELSE
              ALLOCATE(val(SIZE(thisParam%val,1), &
                  SIZE(thisParam%val,2),SIZE(thisParam%val,3)))
              val=thisParam%val
            ENDIF
          ELSE
            CALL eParams%raiseError(modName//'::'//myName// &
              ' - parameter name mismatch "'//TRIM(name)//'" in "'// &
                thisParam%name//'"!')
          ENDIF
        CLASS DEFAULT
          !Search for the parameter name
          CALL thisParam%getParam(name,tmpParam)
          IF(ASSOCIATED(tmpParam)) THEN
            !Parameter was found
            SELECTTYPE(p=>tmpParam)
              TYPE IS(ParamType_SDK_a3)
                IF(ALLOCATED(val)) THEN
                  IF(SIZE(p%val,1) /= SIZE(val,1) .OR. &
                       SIZE(p%val,2) /= SIZE(val,2) .OR. &
                         SIZE(p%val,3) /= SIZE(val,3)) THEN
                    DEALLOCATE(val)
                    ALLOCATE(val(SIZE(p%val,1),SIZE(p%val,2),SIZE(p%val,3)))
                  ENDIF
                  val=p%val
                ELSE
                  ALLOCATE(val(SIZE(p%val,1),SIZE(p%val,2),SIZE(p%val,3)))
                  val=p%val
                ENDIF
              CLASS DEFAULT
                CALL eParams%raiseError(modName//'::'//myName// &
                  ' - parameter data type mismatch! Parameter '//TRIM(name)//' type is '// &
                    tmpParam%dataType//' and must be 3-D ARRAY REAL(SDK)!')
            ENDSELECT
          ELSE
            CALL eParams%raiseError(modName//'::'//myName// &
              ' - unable to locate parameter "'//TRIM(name)//'" in "'// &
                thisParam%name//'"!')
          ENDIF
      ENDSELECT
    ENDSUBROUTINE get_ParamType_SDK_a3
!
!-------------------------------------------------------------------------------
!> @brief Adds a new three dimensional array of double precision real valued parameter to a set of
!> parameters
!> @param thisParam the set of parameters to which a new parameter will be added
!> @param name the location and name of the new parameter
!> @param param the double precision real value of the new parameter
!> @param description an optional input for a description of the new parameter
!>
!> This routine creates a new parameter within @c thisParam with @c name.
!> @c name may contain a full or partial path to the new parameter. If @c name
!> can be matched to an existing parameter in @c thisParam an error is produced
!> If @c name contains a full path for which intermediate lists do not exist
!> then this lists are created in the process of adding the new parameter.
!> If @c thisParam is not initialized and @c name does not contain a "->"
!> symbol then this routine behaves equivalently to
!> @ref ParameterLists::init_ParamType_SDK "initSDK".
!>
    SUBROUTINE add_ParamType_SDK_a3(thisParam,name,param,description)
      CHARACTER(LEN=*),PARAMETER :: myName='add_ParamType_SDK_a3'
      CLASS(ParamType),INTENT(INOUT) :: thisParam
      CHARACTER(LEN=*),INTENT(IN) :: name
      REAL(SDK),INTENT(IN) :: param(:,:,:)
      CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
      CHARACTER(LEN=LEN(name)) :: prevname,thisname
      INTEGER(SIK) :: ipos
      TYPE(ParamType) :: newParam
      CLASS(ParamType),POINTER :: tmpParam

      !Search for the name to make sure it does not exist
      CALL get_ParamType(thisParam,name,tmpParam)

      IF(.NOT.ASSOCIATED(tmpParam)) THEN
        prevname=''
        thisname=ADJUSTL(name)
        ipos=INDEX(name,'->',.TRUE.)
        IF(ipos > 0) THEN
          prevname=ADJUSTL(name(1:ipos-1))
          thisname=ADJUSTL(name(ipos+2:LEN(name)))
        ENDIF

        !Initialize the new parameter
        CALL init_ParamType_SDK_a3(newParam,thisname,param,description)

        !Add the new parameter to thisParam
        CALL add_ParamType(thisParam,prevname,newParam)
        CALL newParam%clear()
      ELSE
        CALL eParams%raiseError(modName//'::'//myName// &
          ' - parameter name "'//TRIM(name)// &
            '" already exists! Use set method or full parameter list path!')
      ENDIF
    ENDSUBROUTINE add_ParamType_SDK_a3
!
!-------------------------------------------------------------------------------
!> @brief Initializes a ParamType object as a three dimensional array of 32-bit integer
!> @param thisParam the parameter to initialize
!> @param name the name of the parameter
!> @param param a three dimensional array of 32-bit integer
!> @param description an optional description for this parameter
!>
!> This routine is not recursive, so it is like setting a three dimensional array of parameter.
!> Therefore the name cannot contain the "->" symbol to indicate access to a
!> sub-list. @c thisParam must not already be inititalized.
!>
    SUBROUTINE init_ParamType_SNK_a3(thisParam,name,param,description)
      CHARACTER(LEN=*),PARAMETER :: myName='init_ParamType_SNK_a3'
      CLASS(ParamType),INTENT(INOUT) :: thisParam
      INTEGER(SNK),INTENT(IN) :: param(:,:,:)
      CHARACTER(LEN=*),INTENT(IN) :: name
      CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
      INTEGER(SIK) :: ipos

      IF(.NOT.ASSOCIATED(thisParam%pdat)) THEN
        !Check that '->' character is not in name
        ipos=INDEX(name,'->')
        IF(ipos == 0) THEN
          ALLOCATE(ParamType_SNK_a3 :: thisParam%pdat)
          thisParam%pdat%name=TRIM(name)
          IF(PRESENT(description)) thisParam%pdat%description=TRIM(description)
          thisParam%pdat%dataType='3-D ARRAY INTEGER(SNK)'
          SELECTTYPE(p=>thisParam%pdat)
            TYPE IS(ParamType_SNK_a3)
              ALLOCATE(p%val(SIZE(param,1),SIZE(param,2),SIZE(param,3)))
              p%val=param
          ENDSELECT
        ELSE
          CALL eParams%raiseError(modName//'::'//myName// &
            ' - "->" symbol is not allowed in name!')
        ENDIF
      ELSE
        CALL eParams%raiseError(modName//'::'//myName// &
          ' - parameter is already initialized! Use set method!')
      ENDIF
    ENDSUBROUTINE init_ParamType_SNK_a3
!
!-------------------------------------------------------------------------------
!> @brief Edits a three dimensional array of 32-bit integer valued parameter
!> @param thisParam the three dimensional array of 32-bit integer valued parameter to edit
!> @param funit the unit number to edit the parameter to
!> @param indent optional indicates the number of blank spaces to precede the
!>        beginning of text to edit.
!>
!> The formatted write uses the "general" edit descriptor so that 7 digits (three
!> more than the significant number in a 32-bit integer) are always
!> printed if the number is very large in absolute value engineering format
!> is used otherwise floating point form is used to write the value.
!>
    SUBROUTINE edit_ParamType_SNK_a3(thisParam,funit,indent)
      CLASS(ParamType_SNK_a3),INTENT(IN) :: thisParam
      INTEGER(SIK),INTENT(IN) :: funit
      INTEGER(SIK),INTENT(IN),OPTIONAL :: indent
      CHARACTER(LEN=12) :: fmt,fmt2,fmt3
      INTEGER(SIK) :: i,j,k,l,m

      i=1
      j=6
      IF(PRESENT(indent)) i=i+indent
      WRITE(fmt,'(i12)') i; fmt=ADJUSTL(fmt)
      IF(LEN_TRIM(thisParam%description) == 0) THEN
        WRITE(UNIT=funit,FMT='('//TRIM(fmt)//'x,a)') &
          thisParam%dataType//' :: '//thisParam%name//'= ...'
      ELSE
        WRITE(UNIT=funit,FMT='('//TRIM(fmt)//'x,a,a)') &
          thisParam%dataType//' :: '//thisParam%name//'= ... !'//thisParam%description
      ENDIF
      j=j+LEN(thisParam%dataType)+LEN(thisParam%name)
      WRITE(fmt2,'(i12)') j; fmt2=ADJUSTL(fmt2)
      WRITE(fmt3,'(i12)') SIZE(thisParam%val,1); fmt3=ADJUSTL(fmt3)
      DO k=1,SIZE(thisParam%val,3)
        DO l=1,SIZE(thisParam%val,2)
          WRITE(UNIT=funit,FMT='('//TRIM(fmt)//'x,'//TRIM(fmt2)//'x,'// &
            TRIM(fmt3)//'(g13.7))') (thisParam%val(m,l,k),m=1,SIZE(thisParam%val,1))
        ENDDO
      ENDDO
    ENDSUBROUTINE edit_ParamType_SNK_a3
!
!-------------------------------------------------------------------------------
!> @brief Clears a three dimensional array of 32-bit integer valued parameter
!> @param thisParam the three dimensional array of 32-bit integer valued parameter to clear
!>
    SUBROUTINE clear_ParamType_SNK_a3(thisParam)
      CLASS(ParamType_SNK_a3),INTENT(INOUT) :: thisParam
      DEALLOCATE(thisParam%val)
      thisParam%name=''
      thisParam%dataType=''
      thisParam%description=''
    ENDSUBROUTINE clear_ParamType_SNK_a3
!
!-------------------------------------------------------------------------------
!> @brief Sets the value of an existing three dimensional array of 32-bit integer valued
!> parameter to a new value.
!> @param thisParam the parameter in which an existing parameter with name
!>        matching @c name will be to set the new value of @c param
!> @param name the name of an existing parameter to set the value of
!> @param param the new value to set for the parameter
!> @param description an optional new description for the parameter identified
!>        by @c name
!>
!> If a parameter with @c name is not found an error is produced. If the
!> parameter with @c name is not a three dimensional array of 32-bit integer valued parameter
!> then an error is produced.
!>
    SUBROUTINE set_ParamType_SNK_a3(thisParam,name,param,description)
      CHARACTER(LEN=*),PARAMETER :: myName='set_ParamType_SNK_a3'
      CLASS(ParamType),INTENT(INOUT) :: thisParam
      CHARACTER(LEN=*),INTENT(IN) :: name
      INTEGER(SNK),INTENT(IN) :: param(:,:,:)
      CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
      CLASS(ParamType),POINTER :: tmpParam

      SELECTTYPE(thisParam)
        TYPE IS(ParamType_SNK_a3)
          IF(thisParam%name == TRIM(name)) THEN
            IF(SIZE(thisParam%val,1) /= SIZE(param,1) .OR. &
                 SIZE(thisParam%val,2) /= SIZE(param,2) .OR. &
                   SIZE(thisParam%val,3) /= SIZE(param,3)) THEN
              DEALLOCATE(thisParam%val)
              ALLOCATE(thisParam%val(SIZE(param,1),SIZE(param,2),SIZE(param,3)))
            ENDIF
            thisParam%val=param
            IF(PRESENT(description)) thisParam%description=TRIM(description)
          ELSE
            CALL eParams%raiseError(modName//'::'//myName// &
              ' - parameter name mismatch! Tried to set "'//TRIM(name)// &
                '" but name is "'//thisParam%name//'"!')
          ENDIF
        CLASS DEFAULT
          !Search for the parameter name
          CALL thisParam%getParam(name,tmpParam)
          IF(ASSOCIATED(tmpParam)) THEN
            !Parameter was found
            SELECTTYPE(p=>tmpParam)
              TYPE IS(ParamType_SNK_a3)
                IF(SIZE(p%val,1) /= SIZE(param,1) .OR. &
                     SIZE(p%val,2) /= SIZE(param,2) .OR. &
                       SIZE(p%val,3) /= SIZE(param,3)) THEN
                  DEALLOCATE(p%val)
                  ALLOCATE(p%val(SIZE(param,1),SIZE(param,2),SIZE(param,3)))
                ENDIF
                p%val=param
                IF(PRESENT(description)) p%description=TRIM(description)
              CLASS DEFAULT
                CALL eParams%raiseError(modName//'::'//myName// &
                  ' - parameter data type mismatch! Parameter '//TRIM(name)//' type is '// &
                    tmpParam%dataType//' and must be 3-D ARRAY INTEGER(SNK)!')
            ENDSELECT
          ELSE
            CALL eParams%raiseError(modName//'::'//myName// &
              ' - unable to locate parameter "'//TRIM(name)//'" in "'// &
                thisParam%name//'"!')
          ENDIF
      ENDSELECT
    ENDSUBROUTINE set_ParamType_SNK_a3
!
!-------------------------------------------------------------------------------
!> @brief Gets the three dimensional array of 32-bit integer value for a specified parameter
!> @param thisParam the parameter in which an existing parameter with name
!>        matching @c name will have it's value returned
!> @param name the name of the parameter to return the value of
!> @param val the current value of the parameter with @c name
!>
!> If a parameter with @c name is not found an error is produced. If the
!> parameter with @c name is not a three dimensional array of 32-bit integer valued parameter
!> then an error is produced.
!>
    SUBROUTINE get_ParamType_SNK_a3(thisParam,name,val)
      CHARACTER(LEN=*),PARAMETER :: myName='get_ParamType_SNK_a3'
      CLASS(ParamType),INTENT(IN) :: thisParam
      CHARACTER(LEN=*),INTENT(IN) :: name
      INTEGER(SNK),ALLOCATABLE,INTENT(INOUT) :: val(:,:,:)
      CLASS(ParamType),POINTER :: tmpParam

      SELECTTYPE(thisParam)
        TYPE IS(ParamType_SNK_a3)
          IF(thisParam%name == TRIM(name)) THEN
            IF(ALLOCATED(val)) THEN
              IF(SIZE(thisParam%val,1) /= SIZE(val,1) .OR. &
                   SIZE(thisParam%val,2) /= SIZE(val,2) .OR. &
                     SIZE(thisParam%val,3) /= SIZE(val,3)) THEN
                DEALLOCATE(val)
                ALLOCATE(val(SIZE(thisParam%val,1), &
                  SIZE(thisParam%val,2),SIZE(thisParam%val,3)))
              ENDIF
              val=thisParam%val
            ELSE
              ALLOCATE(val(SIZE(thisParam%val,1), &
                  SIZE(thisParam%val,2),SIZE(thisParam%val,3)))
              val=thisParam%val
            ENDIF
          ELSE
            CALL eParams%raiseError(modName//'::'//myName// &
              ' - parameter name mismatch "'//TRIM(name)//'" in "'// &
                thisParam%name//'"!')
          ENDIF
        CLASS DEFAULT
          !Search for the parameter name
          CALL thisParam%getParam(name,tmpParam)
          IF(ASSOCIATED(tmpParam)) THEN
            !Parameter was found
            SELECTTYPE(p=>tmpParam)
              TYPE IS(ParamType_SNK_a3)
                IF(ALLOCATED(val)) THEN
                  IF(SIZE(p%val,1) /= SIZE(val,1) .OR. &
                       SIZE(p%val,2) /= SIZE(val,2) .OR. &
                         SIZE(p%val,3) /= SIZE(val,3)) THEN
                    DEALLOCATE(val)
                    ALLOCATE(val(SIZE(p%val,1),SIZE(p%val,2),SIZE(p%val,3)))
                  ENDIF
                  val=p%val
                ELSE
                  ALLOCATE(val(SIZE(p%val,1),SIZE(p%val,2),SIZE(p%val,3)))
                  val=p%val
                ENDIF
              CLASS DEFAULT
                CALL eParams%raiseError(modName//'::'//myName// &
                  ' - parameter data type mismatch! Parameter '//TRIM(name)//' type is '// &
                    tmpParam%dataType//' and must be 3-D ARRAY INTEGER(SNK)!')
            ENDSELECT
          ELSE
            CALL eParams%raiseError(modName//'::'//myName// &
              ' - unable to locate parameter "'//TRIM(name)//'" in "'// &
                thisParam%name//'"!')
          ENDIF
      ENDSELECT
    ENDSUBROUTINE get_ParamType_SNK_a3
!
!-------------------------------------------------------------------------------
!> @brief Adds a new three dimensional array of 32-bit integer valued parameter to a set of
!> parameters
!> @param thisParam the set of parameters to which a new parameter will be added
!> @param name the location and name of the new parameter
!> @param param the 32-bit integer value of the new parameter
!> @param description an optional input for a description of the new parameter
!>
!> This routine creates a new parameter within @c thisParam with @c name.
!> @c name may contain a full or partial path to the new parameter. If @c name
!> can be matched to an existing parameter in @c thisParam an error is produced
!> If @c name contains a full path for which intermediate lists do not exist
!> then this lists are created in the process of adding the new parameter.
!> If @c thisParam is not initialized and @c name does not contain a "->"
!> symbol then this routine behaves equivalently to
!> @ref ParameterLists::init_ParamType_SNK "initSNK".
!>
    SUBROUTINE add_ParamType_SNK_a3(thisParam,name,param,description)
      CHARACTER(LEN=*),PARAMETER :: myName='add_ParamType_SNK_a3'
      CLASS(ParamType),INTENT(INOUT) :: thisParam
      CHARACTER(LEN=*),INTENT(IN) :: name
      INTEGER(SNK),INTENT(IN) :: param(:,:,:)
      CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
      CHARACTER(LEN=LEN(name)) :: prevname,thisname
      INTEGER(SIK) :: ipos
      TYPE(ParamType) :: newParam
      CLASS(ParamType),POINTER :: tmpParam

      !Search for the name to make sure it does not exist
      CALL get_ParamType(thisParam,name,tmpParam)

      IF(.NOT.ASSOCIATED(tmpParam)) THEN
        prevname=''
        thisname=ADJUSTL(name)
        ipos=INDEX(name,'->',.TRUE.)
        IF(ipos > 0) THEN
          prevname=ADJUSTL(name(1:ipos-1))
          thisname=ADJUSTL(name(ipos+2:LEN(name)))
        ENDIF

        !Initialize the new parameter
        CALL init_ParamType_SNK_a3(newParam,thisname,param,description)

        !Add the new parameter to thisParam
        CALL add_ParamType(thisParam,prevname,newParam)
        CALL newParam%clear()
      ELSE
        CALL eParams%raiseError(modName//'::'//myName// &
          ' - parameter name "'//TRIM(name)// &
            '" already exists! Use set method or full parameter list path!')
      ENDIF
    ENDSUBROUTINE add_ParamType_SNK_a3
!
!-------------------------------------------------------------------------------
!> @brief Initializes a ParamType object as a three dimensional array of 64-bit integer
!> @param thisParam the parameter to initialize
!> @param name the name of the parameter
!> @param param a three dimensional array of 64-bit integer
!> @param description an optional description for this parameter
!>
!> This routine is not recursive, so it is like setting a three dimensional array of parameter.
!> Therefore the name cannot contain the "->" symbol to indicate access to a
!> sub-list. @c thisParam must not already be inititalized.
!>
    SUBROUTINE init_ParamType_SLK_a3(thisParam,name,param,description)
      CHARACTER(LEN=*),PARAMETER :: myName='init_ParamType_SLK_a3'
      CLASS(ParamType),INTENT(INOUT) :: thisParam
      INTEGER(SLK),INTENT(IN) :: param(:,:,:)
      CHARACTER(LEN=*),INTENT(IN) :: name
      CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
      INTEGER(SIK) :: ipos

      IF(.NOT.ASSOCIATED(thisParam%pdat)) THEN
        !Check that '->' character is not in name
        ipos=INDEX(name,'->')
        IF(ipos == 0) THEN
          ALLOCATE(ParamType_SLK_a3 :: thisParam%pdat)
          thisParam%pdat%name=TRIM(name)
          IF(PRESENT(description)) thisParam%pdat%description=TRIM(description)
          thisParam%pdat%dataType='3-D ARRAY INTEGER(SLK)'
          SELECTTYPE(p=>thisParam%pdat)
            TYPE IS(ParamType_SLK_a3)
              ALLOCATE(p%val(SIZE(param,1),SIZE(param,2),SIZE(param,3)))
              p%val=param
          ENDSELECT
        ELSE
          CALL eParams%raiseError(modName//'::'//myName// &
            ' - "->" symbol is not allowed in name!')
        ENDIF
      ELSE
        CALL eParams%raiseError(modName//'::'//myName// &
          ' - parameter is already initialized! Use set method!')
      ENDIF
    ENDSUBROUTINE init_ParamType_SLK_a3
!
!-------------------------------------------------------------------------------
!> @brief Edits a three dimensional array of 64-bit integer valued parameter
!> @param thisParam the three dimensional array of 64-bit integer valued parameter to edit
!> @param funit the unit number to edit the parameter to
!> @param indent optional indicates the number of blank spaces to precede the
!>        beginning of text to edit.
!>
!> The formatted write uses the "general" edit descriptor so that 7 digits (three
!> more than the significant number in a 64-bit integer) are always
!> printed if the number is very large in absolute value engineering format
!> is used otherwise floating point form is used to write the value.
!>
    SUBROUTINE edit_ParamType_SLK_a3(thisParam,funit,indent)
      CLASS(ParamType_SLK_a3),INTENT(IN) :: thisParam
      INTEGER(SIK),INTENT(IN) :: funit
      INTEGER(SIK),INTENT(IN),OPTIONAL :: indent
      CHARACTER(LEN=12) :: fmt,fmt2,fmt3
      INTEGER(SIK) :: i,j,k,l,m

      i=1
      j=6
      IF(PRESENT(indent)) i=i+indent
      WRITE(fmt,'(i12)') i; fmt=ADJUSTL(fmt)
      IF(LEN_TRIM(thisParam%description) == 0) THEN
        WRITE(UNIT=funit,FMT='('//TRIM(fmt)//'x,a)') &
          thisParam%dataType//' :: '//thisParam%name//'= ...'
      ELSE
        WRITE(UNIT=funit,FMT='('//TRIM(fmt)//'x,a,a)') &
          thisParam%dataType//' :: '//thisParam%name//'= ... !'//thisParam%description
      ENDIF
      j=j+LEN(thisParam%dataType)+LEN(thisParam%name)
      WRITE(fmt2,'(i12)') j; fmt2=ADJUSTL(fmt2)
      WRITE(fmt3,'(i12)') SIZE(thisParam%val,1); fmt3=ADJUSTL(fmt3)
      DO k=1,SIZE(thisParam%val,3)
        DO l=1,SIZE(thisParam%val,2)
          WRITE(UNIT=funit,FMT='('//TRIM(fmt)//'x,'//TRIM(fmt2)//'x,'// &
            TRIM(fmt3)//'(g20.14))') (thisParam%val(m,l,k),m=1,SIZE(thisParam%val,1))
        ENDDO
      ENDDO
    ENDSUBROUTINE edit_ParamType_SLK_a3
!
!-------------------------------------------------------------------------------
!> @brief Clears a three dimensional array of 64-bit integer valued parameter
!> @param thisParam the three dimensional array of 64-bit integer valued parameter to clear
!>
    SUBROUTINE clear_ParamType_SLK_a3(thisParam)
      CLASS(ParamType_SLK_a3),INTENT(INOUT) :: thisParam
      DEALLOCATE(thisParam%val)
      thisParam%name=''
      thisParam%dataType=''
      thisParam%description=''
    ENDSUBROUTINE clear_ParamType_SLK_a3
!
!-------------------------------------------------------------------------------
!> @brief Sets the value of an existing three dimensional array of 64-bit integer valued
!> parameter to a new value.
!> @param thisParam the parameter in which an existing parameter with name
!>        matching @c name will be to set the new value of @c param
!> @param name the name of an existing parameter to set the value of
!> @param param the new value to set for the parameter
!> @param description an optional new description for the parameter identified
!>        by @c name
!>
!> If a parameter with @c name is not found an error is produced. If the
!> parameter with @c name is not a three dimensional array of 64-bit integer valued parameter
!> then an error is produced.
!>
    SUBROUTINE set_ParamType_SLK_a3(thisParam,name,param,description)
      CHARACTER(LEN=*),PARAMETER :: myName='set_ParamType_SLK_a3'
      CLASS(ParamType),INTENT(INOUT) :: thisParam
      CHARACTER(LEN=*),INTENT(IN) :: name
      INTEGER(SLK),INTENT(IN) :: param(:,:,:)
      CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
      CLASS(ParamType),POINTER :: tmpParam

      SELECTTYPE(thisParam)
        TYPE IS(ParamType_SLK_a3)
          IF(thisParam%name == TRIM(name)) THEN
            IF(SIZE(thisParam%val,1) /= SIZE(param,1) .OR. &
                 SIZE(thisParam%val,2) /= SIZE(param,2) .OR. &
                   SIZE(thisParam%val,3) /= SIZE(param,3)) THEN
              DEALLOCATE(thisParam%val)
              ALLOCATE(thisParam%val(SIZE(param,1),SIZE(param,2),SIZE(param,3)))
            ENDIF
            thisParam%val=param
            IF(PRESENT(description)) thisParam%description=TRIM(description)
          ELSE
            CALL eParams%raiseError(modName//'::'//myName// &
              ' - parameter name mismatch! Tried to set "'//TRIM(name)// &
                '" but name is "'//thisParam%name//'"!')
          ENDIF
        CLASS DEFAULT
          !Search for the parameter name
          CALL thisParam%getParam(name,tmpParam)
          IF(ASSOCIATED(tmpParam)) THEN
            !Parameter was found
            SELECTTYPE(p=>tmpParam)
              TYPE IS(ParamType_SLK_a3)
                IF(SIZE(p%val,1) /= SIZE(param,1) .OR. &
                     SIZE(p%val,2) /= SIZE(param,2) .OR. &
                       SIZE(p%val,3) /= SIZE(param,3)) THEN
                  DEALLOCATE(p%val)
                  ALLOCATE(p%val(SIZE(param,1),SIZE(param,2),SIZE(param,3)))
                ENDIF
                p%val=param
                IF(PRESENT(description)) p%description=TRIM(description)
              CLASS DEFAULT
                CALL eParams%raiseError(modName//'::'//myName// &
                  ' - parameter data type mismatch! Parameter '//TRIM(name)//' type is '// &
                    tmpParam%dataType//' and must be 3-D ARRAY INTEGER(SLK)!')
            ENDSELECT
          ELSE
            CALL eParams%raiseError(modName//'::'//myName// &
              ' - unable to locate parameter "'//TRIM(name)//'" in "'// &
                thisParam%name//'"!')
          ENDIF
      ENDSELECT
    ENDSUBROUTINE set_ParamType_SLK_a3
!
!-------------------------------------------------------------------------------
!> @brief Gets the three dimensional array of 64-bit integer value for a specified parameter
!> @param thisParam the parameter in which an existing parameter with name
!>        matching @c name will have it's value returned
!> @param name the name of the parameter to return the value of
!> @param val the current value of the parameter with @c name
!>
!> If a parameter with @c name is not found an error is produced. If the
!> parameter with @c name is not a three dimensional array of 64-bit integer valued parameter
!> then an error is produced.
!>
    SUBROUTINE get_ParamType_SLK_a3(thisParam,name,val)
      CHARACTER(LEN=*),PARAMETER :: myName='get_ParamType_SLK_a3'
      CLASS(ParamType),INTENT(IN) :: thisParam
      CHARACTER(LEN=*),INTENT(IN) :: name
      INTEGER(SLK),ALLOCATABLE,INTENT(INOUT) :: val(:,:,:)
      CLASS(ParamType),POINTER :: tmpParam

      SELECTTYPE(thisParam)
        TYPE IS(ParamType_SLK_a3)
          IF(thisParam%name == TRIM(name)) THEN
            IF(ALLOCATED(val)) THEN
              IF(SIZE(thisParam%val,1) /= SIZE(val,1) .OR. &
                   SIZE(thisParam%val,2) /= SIZE(val,2) .OR. &
                     SIZE(thisParam%val,3) /= SIZE(val,3)) THEN
                DEALLOCATE(val)
                ALLOCATE(val(SIZE(thisParam%val,1), &
                  SIZE(thisParam%val,2),SIZE(thisParam%val,3)))
              ENDIF
              val=thisParam%val
            ELSE
              ALLOCATE(val(SIZE(thisParam%val,1), &
                  SIZE(thisParam%val,2),SIZE(thisParam%val,3)))
              val=thisParam%val
            ENDIF
          ELSE
            CALL eParams%raiseError(modName//'::'//myName// &
              ' - parameter name mismatch "'//TRIM(name)//'" in "'// &
                thisParam%name//'"!')
          ENDIF
        CLASS DEFAULT
          !Search for the parameter name
          CALL thisParam%getParam(name,tmpParam)
          IF(ASSOCIATED(tmpParam)) THEN
            !Parameter was found
            SELECTTYPE(p=>tmpParam)
              TYPE IS(ParamType_SLK_a3)
                IF(ALLOCATED(val)) THEN
                  IF(SIZE(p%val,1) /= SIZE(val,1) .OR. &
                       SIZE(p%val,2) /= SIZE(val,2) .OR. &
                         SIZE(p%val,3) /= SIZE(val,3)) THEN
                    DEALLOCATE(val)
                    ALLOCATE(val(SIZE(p%val,1),SIZE(p%val,2),SIZE(p%val,3)))
                  ENDIF
                  val=p%val
                ELSE
                  ALLOCATE(val(SIZE(p%val,1),SIZE(p%val,2),SIZE(p%val,3)))
                  val=p%val
                ENDIF
              CLASS DEFAULT
                CALL eParams%raiseError(modName//'::'//myName// &
                  ' - parameter data type mismatch! Parameter '//TRIM(name)//' type is '// &
                    tmpParam%dataType//' and must be 3-D ARRAY INTEGER(SLK)!')
            ENDSELECT
          ELSE
            CALL eParams%raiseError(modName//'::'//myName// &
              ' - unable to locate parameter "'//TRIM(name)//'" in "'// &
                thisParam%name//'"!')
          ENDIF
      ENDSELECT
    ENDSUBROUTINE get_ParamType_SLK_a3
!
!-------------------------------------------------------------------------------
!> @brief Adds a new three dimensional array of 64-bit integer valued parameter to a set of
!> parameters
!> @param thisParam the set of parameters to which a new parameter will be added
!> @param name the location and name of the new parameter
!> @param param the 64-bit integer value of the new parameter
!> @param description an optional input for a description of the new parameter
!>
!> This routine creates a new parameter within @c thisParam with @c name.
!> @c name may contain a full or partial path to the new parameter. If @c name
!> can be matched to an existing parameter in @c thisParam an error is produced
!> If @c name contains a full path for which intermediate lists do not exist
!> then this lists are created in the process of adding the new parameter.
!> If @c thisParam is not initialized and @c name does not contain a "->"
!> symbol then this routine behaves equivalently to
!> @ref ParameterLists::init_ParamType_SLK "initSLK".
!>
    SUBROUTINE add_ParamType_SLK_a3(thisParam,name,param,description)
      CHARACTER(LEN=*),PARAMETER :: myName='add_ParamType_SLK_a3'
      CLASS(ParamType),INTENT(INOUT) :: thisParam
      CHARACTER(LEN=*),INTENT(IN) :: name
      INTEGER(SLK),INTENT(IN) :: param(:,:,:)
      CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
      CHARACTER(LEN=LEN(name)) :: prevname,thisname
      INTEGER(SIK) :: ipos
      TYPE(ParamType) :: newParam
      CLASS(ParamType),POINTER :: tmpParam

      !Search for the name to make sure it does not exist
      CALL get_ParamType(thisParam,name,tmpParam)

      IF(.NOT.ASSOCIATED(tmpParam)) THEN
        prevname=''
        thisname=ADJUSTL(name)
        ipos=INDEX(name,'->',.TRUE.)
        IF(ipos > 0) THEN
          prevname=ADJUSTL(name(1:ipos-1))
          thisname=ADJUSTL(name(ipos+2:LEN(name)))
        ENDIF

        !Initialize the new parameter
        CALL init_ParamType_SLK_a3(newParam,thisname,param,description)

        !Add the new parameter to thisParam
        CALL add_ParamType(thisParam,prevname,newParam)
        CALL newParam%clear()
      ELSE
        CALL eParams%raiseError(modName//'::'//myName// &
          ' - parameter name "'//TRIM(name)// &
            '" already exists! Use set method or full parameter list path!')
      ENDIF
    ENDSUBROUTINE add_ParamType_SLK_a3
!
!-------------------------------------------------------------------------------
    RECURSIVE SUBROUTINE procXMLTree(thisParam,parent,currentPath)
      TYPE(ParamType),POINTER :: pList(:)
      CLASS(ParamType),POINTER :: Params
      CLASS(ParamType),INTENT(INOUT) :: thisParam
      TYPE(StringType),INTENT(IN) :: currentPath
      TYPE(XMLElementType),POINTER :: iXMLE,children(:),parent,nextXMLE
      TYPE(StringType) :: elname,tmpStr,typval,attrVal,nameVal,tmpPath
      INTEGER(SIK) :: ic

      LOGICAL(SBK) :: boolVal
      INTEGER(SIK) :: intVal,tmpInt
      REAL(SDK) :: doubleVal
      TYPE(StringType) :: strVal
      INTEGER(SIK),ALLOCATABLE :: intArry(:)
      REAL(SDK),ALLOCATABLE :: doubleArry(:)
      TYPE(StringType),ALLOCATABLE :: strArry(:)

      NULLIFY(pList)
      CALL parent%getChildren(children)
      !Check to see if it's an empty parameter list
      IF(.NOT.ASSOCIATED(children)) THEN
        ALLOCATE(pList(0))
        CALL thisParam%add(CHAR(currentPath),pList)
        RETURN
      ENDIF

      DO ic=1,SIZE(children)
        tmpPath=currentPath//' -> '
        iXMLE => children(ic)

        elname=iXMLE%name
        CALL toUPPER(elname)
        IF(elname == 'PARAMETER') THEN
          tmpStr='type'
          CALL iXMLE%getAttributeValue(tmpStr,typval)
          CALL toUPPER(typval)
          tmpStr='value'
          CALL iXMLE%getAttributeValue(tmpStr,attrVal)
          tmpStr='name'
          CALL iXMLE%getAttributeValue(tmpStr,nameVal)
          tmpPath=tmpPath//nameVal
          SELECTCASE(CHAR(typval))
            CASE('BOOL')
              boolVal=CHAR(attrVal)
              CALL thisParam%add(CHAR(tmpPath),boolVal)
            CASE('INT')
              intVal=CHAR(attrVal)
              CALL thisParam%add(CHAR(tmpPath),intVal)
            CASE('DOUBLE')
              doubleVal=CHAR(attrVal)
              CALL thisParam%add(CHAR(tmpPath),doubleVal)
            CASE('STRING')
              strVal=attrVal
              CALL thisParam%add(CHAR(tmpPath),strVal)
            CASE('ARRAY(INT)')
              CALL char_to_int_array(intArry,CHAR(attrVal))
              CALL thisParam%add(CHAR(tmpPath),intArry)
            CASE('ARRAY(DOUBLE)')
              CALL char_to_double_array(doubleArry,CHAR(attrVal))
              CALL thisParam%add(CHAR(tmpPath),doubleArry)
            CASE('ARRAY(STRING)')
              !strArry=CHAR(attrVal)
              !CALL char_to_string_array(strArry,CHAR(attrVal))
            CASE DEFAULT
              !Bad element type
          ENDSELECT
        ELSE IF(elname == 'PARAMETERLIST') THEN
          !Add to list (without arrow)
          tmpStr='name'
          CALL iXMLE%getAttributeValue(tmpStr,nameVal)
          tmpPath=tmpPath//nameVal
          CALL procXMLTree(thisParam,iXMLE,tmpPath)
        ENDIF
      ENDDO

    ENDSUBROUTINE procXMLTree
!
!-------------------------------------------------------------------------------
    SUBROUTINE initFromXML(thisParam,fname)
      CLASS(ParamType),INTENT(INOUT) :: thisParam
      CHARACTER(LEN=*),INTENT(IN) :: fname
      INTEGER(SIK) :: ic
      TYPE(StringType) :: elname,tmpStr,typval,attrVal,nameVal
      TYPE(XMLFileType) :: xmlFile
      TYPE(XMLElementType),POINTER :: iXMLE,children(:),parent,nextXMLE
      CLASS(ParamType),POINTER :: iParam
      TYPE(StringType) :: currentPath

      LOGICAL(SBK) :: boolVal
      INTEGER(SIK) :: intVal
      REAL(SDK) :: doubleVal
      TYPE(StringType) :: strVal
      INTEGER(SIK),ALLOCATABLE :: intArry(:)
      REAL(SDK),ALLOCATABLE :: doubleArry(:)
      TYPE(StringType),ALLOCATABLE :: strArry(:)
      TYPE(StringType) :: pathName,tmpPath

      SELECTTYPE(thisParam); TYPE IS(ParamType)
        IF(.NOT.ASSOCIATED(thisParam%pdat)) THEN
          !Initialize the XML file
          CALL xmlfile%importFromDisk(fname)
          iXMLE => xmlfile%root
          iParam => thisParam%pdat
          !tmpStr='name'
          !CALL iXMLE%getAttributeValue(tmpStr,nameVal)
          !currentPath=nameVal
          CALL iXMLE%getChildren(children)
          tmpStr='name'
          CALL iXMLE%getAttributeValue(tmpStr,nameVal)
          currentPath=nameVal
          CALL procXMLTree(thisParam,iXMLE,currentPath)
  CALL thisParam%edit(6)
          !CALL thisParam%add(iParam,elname,
!  !CALL procXMLTree(iXMLE, currentPath)
!          DO WHILE(ASSOCIATED(iXMLE))
!            !Process this iXMLE
!            elname=iXMLE%name
!            CALL toUPPER(elname)
!            IF(elname == 'PARAMETER') THEN
!              !Get the attribute type
!              tmpStr='type'
!              CALL iXMLE%getAttributeValue(tmpStr,typval)
!              CALL toUPPER(typval)
!              !Get the attribute value
!              tmpStr='value'
!              CALL iXMLE%getAttributeValue(tmpStr,attrVal)
!              SELECTCASE(CHAR(typval))
!                CASE('BOOL')
!                  boolVal=CHAR(attrVal)
!                  !thisParam%add(boolVal)
!                CASE('INT')
!                  intVal=CHAR(attrVal)
!                CASE('DOUBLE')
!                  doubleVal=CHAR(attrVal)
!                CASE('STRING')
!                  strVal=attrVal
!                CASE('ARRAY(INT)')
!                  CALL char_to_int_array(intArry,CHAR(attrVal))
!                CASE('ARRAY(DOUBLE)')
!                  CALL char_to_double_array(doubleArry,CHAR(attrVal))
!                CASE('ARRAY(STRING)')
!                  !strArry=CHAR(attrVal)
!                  !CALL char_to_string_array(strArry,CHAR(attrVal))
!              ENDSELECT
!            ELSEIF(elname == 'PARAMETERLIST') THEN
!              ALLOCATE(ParamType_List :: iParam)
!              tmpStr='name'
!              CALL iXMLE%getAttributeValue(tmpStr,elname)
!              !CALL thisParam%add(iParam,elname,
!              !iParam%init(elname,
!            ELSE
!              !Bad element name
!            ENDIF
!
!            !Get the next XML element
!            NULLIFY(nextXMLE)
!            IF(iXMLE%hasChildren()) THEN !It's a ParameterList
!              CALL iXMLE%getChildren(children)
!              nextXMLE => children(1)
!            ELSE
!              !Get the parent and go to the next child
!              findParent: DO WHILE(ASSOCIATED(iXMLE))
!                CALL iXMLE%getParent(parent)
!                CALL parent%getChildren(children)
!                DO ic=1,SIZE(children)-1
!                  IF(ASSOCIATED(iXMLE,children(ic))) THEN
!                    nextXMLE => children(ic+1)
!                    EXIT findParent
!                  ENDIF
!                ENDDO
!                IF(ASSOCIATED(iXMLE,children(ic))) iXMLE => parent
!              ENDDO findParent
!            ENDIF
!            iXMLE => nextXMLE
!          ENDDO
        ELSE
          !Must be uninitialized
        ENDIF
      CLASS DEFAULT
        !Wrong type
      ENDSELECT
    ENDSUBROUTINE initFromXML
!
!-------------------------------------------------------------------------------
!> @brief Defines the operation for performing an assignment of a character
!> string to an array of strings
!> @param sArr the array of strings
!> @param c the character value
!    SUBROUTINE char_to_string_array(sArr,c)
!      TYPE(StringType),ALLOCATABLE,INTENT(OUT) :: sArr(:)
!      CHARACTER(LEN=*),INTENT(IN) :: c
!      CHARACTER(LEN=50) :: tmpStr
!      TYPE(StringType) :: tmpElt
!      INTEGER(SIK) :: numElts
!      INTEGER(SIK) :: i,j,k
!
!      numElts=countArrayElts(c)
!      !Empty array case
!      IF(numElts == 0) THEN
!        RETURN
!      ENDIF
!
!      j=0
!      k=1 ! sArr index
!      ALLOCATE(dArr(numElts))
!      DO i=2,LEN(c)
!        IF(c(i:i) /= ',' .AND. c(i:i) /= '}') THEN
!          j=j+1
!          tmpStr(j:j)=c(i:i)
!        ELSE
!          tmpElt=tmpElt(1:j)
!          sArr(k:k)=tmpElt
!          j=0
!          k=k+1
!        ENDIF
!      ENDDO
!    ENDSUBROUTINE char_to_string_array
!
ENDMODULE ParameterLists

