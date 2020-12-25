!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
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
!>  - 4-D arrays of 32-bit integers
!>  - 4-D arrays of 64-bit integers
!>  - 4-D arrays of single precision reals
!>  - 4-D arrays of double precision reals
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
#ifdef FUTILITY_HAVE_Trilinos
USE ForTeuchos_ParameterList
#endif

IMPLICIT NONE
PRIVATE !Default private for module contents
!
! List of Public items
PUBLIC :: eParams
PUBLIC :: ParamType
PUBLIC :: ParamTypePtr
PUBLIC :: ASSIGNMENT(=)
PUBLIC :: OPERATOR(==)

PUBLIC :: char_to_int_array
PUBLIC :: char_to_double_array
PUBLIC :: char_to_string_array

!> The module name
CHARACTER(LEN=*),PARAMETER :: modName='PARAMETERLISTS'
INTEGER(SIK),PARAMETER :: MAX_1D_LEN=10

!> Verification enumerations
INTEGER(SIK),PARAMETER :: VALIDTYPE_VALIDATE=0
INTEGER(SIK),PARAMETER :: VALIDTYPE_VERIFYTEST=1
INTEGER(SIK),PARAMETER :: VALIDTYPE_VERIFYLIST=2

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
    !> @copybrief ParameterLists::init_ParamType_SSK_a4
    !> @copydoc ParameterLists::init_ParamType_SSK_a4
    PROCEDURE,PASS,PRIVATE :: initSSKa4 => init_ParamType_SSK_a4
    !> @copybrief ParameterLists::init_ParamType_SDK_a4
    !> @copydoc ParameterLists::init_ParamType_SDK_a4
    PROCEDURE,PASS,PRIVATE :: initSDKa4 => init_ParamType_SDK_a4
    !> @copybrief ParameterLists::init_ParamType_SNK_a4
    !> @copydoc ParameterLists::init_ParamType_SNK_a4
    PROCEDURE,PASS,PRIVATE :: initSNKa4 => init_ParamType_SNK_a4
    !> @copybrief ParameterLists::init_ParamType_SLK_a4
    !> @copydoc ParameterLists::init_ParamType_SLK_a4
    PROCEDURE,PASS,PRIVATE :: initSLKa4 => init_ParamType_SLK_a4
    !> Generic type bound interface for all @c init operations
    GENERIC :: init => initParamList,initSSK,initSDK,initSNK,initSLK, &
        initSBK,initSTR,initCHAR,initSSKa1,initSDKa1,initSNKa1, &
        initSLKa1,initSBKa1,initSTRa1,initSSKa2,initSDKa2,initSNKa2, &
        initSLKa2,initSTRa2,initSSKa3,initSDKa3,initSNKa3,initSLKa3, &
        initSSKa4,initSDKa4,initSNKa4,initSLKa4
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
    !> @copybrief ParameterLists::set_ParamType_SSK_a4
    !> @copydoc ParameterLists::set_ParamType_SSK_a4
    PROCEDURE,PASS,PRIVATE :: setSSKa4 => set_ParamType_SSK_a4
    !> @copybrief ParameterLists::set_ParamType_SDK_a4
    !> @copydoc ParameterLists::set_ParamType_SDK_a4
    PROCEDURE,PASS,PRIVATE :: setSDKa4 => set_ParamType_SDK_a4
    !> @copybrief ParameterLists::set_ParamType_SNK_a4
    !> @copydoc ParameterLists::set_ParamType_SNK_a4
    PROCEDURE,PASS,PRIVATE :: setSNKa4 => set_ParamType_SNK_a4
    !> @copybrief ParameterLists::set_ParamType_SLK_a4
    !> @copydoc ParameterLists::set_ParamType_SLK_a4
    PROCEDURE,PASS,PRIVATE :: setSLKa4 => set_ParamType_SLK_a4
    !> Generic type bound interface for all @c set operations
    GENERIC :: set => setParamList,setSSK,setSDK,setSNK,setSLK, &
        setSBK,setSTR,setCHAR,setSSKa1,setSDKa1,setSNKa1, &
        setSLKa1,setSBKa1,setSTRa1,setSSKa2,setSDKa2,setSNKa2, &
        setSLKa2,setSTRa2,setSSKa3,setSDKa3,setSNKa3,setSLKa3, &
        setSSKa4,setSDKa4,setSNKa4,setSLKa4
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
    !> @copybrief ParameterLists::get_ParamType_SSK_a4
    !> @copydoc ParameterLists::get_ParamType_SSK_a4
    PROCEDURE,PASS,PRIVATE :: getSSKa4 => get_ParamType_SSK_a4
    !> @copybrief ParameterLists::get_ParamType_SDK_a4
    !> @copydoc ParameterLists::get_ParamType_SDK_a4
    PROCEDURE,PASS,PRIVATE :: getSDKa4 => get_ParamType_SDK_a4
    !> @copybrief ParameterLists::get_ParamType_SNK_a4
    !> @copydoc ParameterLists::get_ParamType_SNK_a4
    PROCEDURE,PASS,PRIVATE :: getSNKa4 => get_ParamType_SNK_a4
    !> @copybrief ParameterLists::get_ParamType_SLK_a4
    !> @copydoc ParameterLists::get_ParamType_SLK_a4
    PROCEDURE,PASS,PRIVATE :: getSLKa4 => get_ParamType_SLK_a4
    !> Generic type bound interface for all @c get operations
    GENERIC :: get => getParam,getParamList,getSSK,getSDK,getSNK, &
        getSLK,getSBK,getSTR,getCHAR,getSSKa1,getSDKa1,getSNKa1, &
        getSBKa1,getSLKa1,getSTRa1,getSSKa2,getSDKa2,getSNKa2, &
        getSLKa2,getSTRa2,getSSKa3,getSDKa3,getSNKa3,getSLKa3, &
        getSSKa4,getSDKa4,getSNKa4,getSLKa4
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
    !> @copybrief ParameterLists::add_ParamType_SSK_a4
    !> @copydoc ParameterLists::add_ParamType_SSK_a4
    PROCEDURE,PASS,PRIVATE :: addSSKa4 => add_ParamType_SSK_a4
    !> @copybrief ParameterLists::add_ParamType_SDK_a4
    !> @copydoc ParameterLists::add_ParamType_SDK_a4
    PROCEDURE,PASS,PRIVATE :: addSDKa4 => add_ParamType_SDK_a4
    !> @copybrief ParameterLists::add_ParamType_SNK_a4
    !> @copydoc ParameterLists::add_ParamType_SNK_a4
    PROCEDURE,PASS,PRIVATE :: addSNKa4 => add_ParamType_SNK_a4
    !> @copybrief ParameterLists::add_ParamType_SLK_a4
    !> @copydoc ParameterLists::add_ParamType_SLK_a4
    PROCEDURE,PASS,PRIVATE :: addSLKa4 => add_ParamType_SLK_a4
    !> Generic type bound interface for all @c add operations
    GENERIC :: add => addParam,addList,addSSK,addSDK, &
        addSNK,addSLK,addSBK,addSTR,addCHAR,addSSKa1,addSDKa1, &
        addSNKa1,addSLKa1,addSBKa1,addSTRa1,addSSKa2,addSDKa2, &
        addSNKa2,addSLKa2,addSTRa2,addSSKa3,addSDKa3,addSNKa3,addSLKa3, &
        addSSKa4,addSDKa4,addSNKa4,addSLKa4
    !> @copybrief ParameterLists::remove_ParamType
    !> @copydoc ParameterLists::remove_ParamType
    PROCEDURE,PASS :: remove => remove_ParamType
    !> @copybrief ParameterLists::getString_ParamType_scalar
    !> @copydoc ParameterLists::getString_scalar_ParamType_scalar
    PROCEDURE,PASS,PRIVATE :: getString_scalar => getString_ParamType_scalar
    !> @copybrief ParameterLists::getString_ParamType_a1
    !> @copydoc ParameterLists::getString_ParamType_a1
    PROCEDURE,PASS,PRIVATE :: getString_a1 => getString_ParamType_a1
    !> @copybrief ParameterLists::getString_ParamType_a2
    !> @copydoc ParameterLists::getString_ParamType_a2
    PROCEDURE,PASS,PRIVATE :: getString_a2 => getString_ParamType_a2
    !> @copybrief ParameterLists::getString_ParamType_a3
    !> @copydoc ParameterLists::getString_ParamType_a3
    PROCEDURE,PASS,PRIVATE :: getString_a3 => getString_ParamType_a3
    !> Generic type bound interface for all @c getString operations
    GENERIC :: getString => getString_scalar,getString_a1,getString_a2,getString_a3
    !> @copybrief ParameterLists::has_ParamType
    !> @copydoc ParameterLists::has_ParamType
    PROCEDURE,PASS :: has => has_ParamType
    !> @copybrief ParameterLists::convertTo2DStringArray_ParamType
    !> @copydoc ParameterLists::convertTo2DStringArray_ParamType
    PROCEDURE,PASS :: convertTo2DStringArray => convertTo2DStringArray_ParamType
    !> @copybrief ParameterLists::getNextParam_ParamType
    !> @copydoc ParameterLists::getNextParam_ParamType
    PROCEDURE,PASS :: getNextParam => getNextParam_ParamType
    !> @copybrief ParameterLists::getSubParam_List
    !> @copydoc ParameterLists::getSubParam_List
    PROCEDURE,PASS :: getSubPL => getSubParam_List
    !> @copybrief ParameterLists::getSubParams
    !> @copydoc ParameterLists::getSubParams
    PROCEDURE,PASS :: getSubParams => getSubParams
    !> @copybrief ParameterLists::validate_ParamType
    !> @copydoc ParameterLists::validate_ParamType
    PROCEDURE,PASS :: validate => validate_ParamType
    !> @copybrief ParameterLists::verifyTest_ParamType
    !> @copydoc ParameterLists::verifyTest_ParamType
    PROCEDURE,PASS :: verify => verifyTest_ParamType
    !> @copybrief ParameterLists::verifyTest_ParamType
    !> @copydoc ParameterLists::verifyTest_ParamType
    PROCEDURE,PASS :: verifyList => verifyList_ParamType
    !> @copybrief ParameterLists::edit_ParamType
    !> @copydoc ParameterLists::edit_ParamType
    PROCEDURE,PASS :: edit => edit_ParamType
    !> @copybrief ParameterLists::editToXML_ParamType
    !> @copydoc ParameterLists::editToXML_ParamType
    PROCEDURE,PASS :: editToXML => editToXML_ParamType
    !> @copybrief ParameterLists::clear_ParamType
    !> @copydoc ParameterLists::clear_ParamType
    PROCEDURE,PASS :: clear => clear_ParamType
#ifdef FUTILITY_HAVE_Trilinos
    PROCEDURE,PASS :: toTeuchosPlist
#endif
PROCEDURE :: procXMLTree
PROCEDURE :: procFMUXMLTree
ENDTYPE ParamType

!> @brief Wrapper type for an array of ParamType pointers
TYPE :: ParamTypePtr
  !> Pointer to the ParamType
  CLASS(ParamType),POINTER :: p => NULL()
ENDTYPE ParamTypePtr

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
!----------------------------------------------------------------------
!     Four-Dimensional Arrays
!----------------------------------------------------------------------
!> @brief Extended type of a ParamType for defining a four dimensional
!> array parameter of single precision reals
TYPE,EXTENDS(ParamType) :: ParamType_SSK_a4
  !> The value of the parameter
  REAL(SSK),ALLOCATABLE :: val(:,:,:,:)
!
!List of type bound procedures
  CONTAINS
    !> @copybrief ParameterLists::edit_ParamType_SSK_a4
    !> @copydoc ParameterLists::edit_ParamType_SSK_a4
    PROCEDURE,PASS :: edit => edit_ParamType_SSK_a4
    !> @copybrief ParameterLists::clear_ParamType_SSK_a4
    !> @copydoc ParameterLists::clear_ParamType_SSK_a4
    PROCEDURE,PASS :: clear => clear_ParamType_SSK_a4
ENDTYPE ParamType_SSK_a4

!> @brief Extended type of a ParamType for defining a four dimensional
!> array parameter of double precision reals
TYPE,EXTENDS(ParamType) :: ParamType_SDK_a4
  !> The value of the parameter
  REAL(SDK),ALLOCATABLE :: val(:,:,:,:)
!
!List of type bound procedures
  CONTAINS
    !> @copybrief ParameterLists::edit_ParamType_SDK_a4
    !> @copydoc ParameterLists::edit_ParamType_SDK_a4
    PROCEDURE,PASS :: edit => edit_ParamType_SDK_a4
    !> @copybrief ParameterLists::clear_ParamType_SDK_a4
    !> @copydoc ParameterLists::clear_ParamType_SDK_a4
    PROCEDURE,PASS :: clear => clear_ParamType_SDK_a4
ENDTYPE ParamType_SDK_a4

!> @brief Extended type of a ParamType for defining a four dimensional
!> array parameter of 32-bit integers
TYPE,EXTENDS(ParamType) :: ParamType_SNK_a4
  !> The value of the parameter
  INTEGER(SNK),ALLOCATABLE :: val(:,:,:,:)
!
!List of type bound procedures
  CONTAINS
    !> @copybrief ParameterLists::edit_ParamType_SNK_a4
    !> @copydoc ParameterLists::edit_ParamType_SNK_a4
    PROCEDURE,PASS :: edit => edit_ParamType_SNK_a4
    !> @copybrief ParameterLists::clear_ParamType_SNK_a4
    !> @copydoc ParameterLists::clear_ParamType_SNK_a4
    PROCEDURE,PASS :: clear => clear_ParamType_SNK_a4
ENDTYPE ParamType_SNK_a4

!> @brief Extended type of a ParamType for defining a four dimensional
!> array parameter of 64-bit integers
TYPE,EXTENDS(ParamType) :: ParamType_SLK_a4
  !> The value of the parameter
  INTEGER(SLK),ALLOCATABLE :: val(:,:,:,:)
!
!List of type bound procedures
  CONTAINS
    !> @copybrief ParameterLists::edit_ParamType_SLK_a4
    !> @copydoc ParameterLists::edit_ParamType_SLK_a4
    PROCEDURE,PASS :: edit => edit_ParamType_SLK_a4
    !> @copybrief ParameterLists::clear_ParamType_SLK_a4
    !> @copydoc ParameterLists::clear_ParamType_SLK_a4
    PROCEDURE,PASS :: clear => clear_ParamType_SLK_a4
ENDTYPE ParamType_SLK_a4
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

INTEGER(SIK),PARAMETER :: PARAM_MAX_DAT_LEN=26
!
!===============================================================================
CONTAINS
#ifdef FUTILITY_HAVE_Trilinos
RECURSIVE SUBROUTINE toTeuchosPlist(this, that, n)
  CLASS(ParamType),INTENT(IN) :: this
  TYPE(ForTeuchos_ParameterList_ID),INTENT(IN) :: that
  INTEGER(SNK),INTENT(IN),OPTIONAL :: n
  !
  CLASS(ParamType), POINTER :: itr
  Type(ParamType) :: nextParam
  TYPE(ForTeuchos_ParameterList_ID) :: new
  INTEGER(C_INT) :: ierr
  INTEGER(SNK) :: level
  TYPE(StringType) :: path

  nullify(itr)

  level = 0
  IF(PRESENT(n)) THEN
    level = n
  ENDIF

  path = ''
  CALL this%getSubParams(path, itr)

  DO WHILE(ASSOCIATED(itr))
    SELECT TYPE(itr)
    TYPE IS(ParamType_List)
      ! This node is its own parameter list
      new = ForTeuchos_PL_sublist(that, CHAR(itr%name), 0, &
          "Imported from MPACT PList", ierr)
      nextParam = itr
      CALL toTeuchosPlist(nextParam, new, level+1)
    TYPE IS(ParamType_SBK)
      CALL ForTeuchos_PL_set_bool(that, CHAR(itr%name), itr%val,&
          CHAR(itr%description), ierr)
    TYPE IS(ParamType_SDK)
      CALL ForTeuchos_PL_set_double(that, CHAR(itr%name), itr%val,&
          CHAR(itr%description), ierr)
    TYPE IS(ParamType_SNK)
      CALL ForTeuchos_PL_set_int(that, CHAR(itr%name), itr%val,&
          CHAR(itr%description), ierr)
    TYPE IS(ParamType_STR)
      CALL ForTeuchos_PL_set_string(that, CHAR(itr%name), CHAR(itr%val),&
          CHAR(itr%description), ierr)
    CLASS DEFAULT
      CALL eParams%raiseError(&
          "Unsupported PARAMETER TYPE for Teuchos conversion.")
    ENDSELECT
    CALL this%getSubParams(path, itr)
  ENDDO
ENDSUBROUTINE
#endif
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
    TYPE IS(ParamType_SSK_a4)
      CALL thisParam%init(CHAR(p%name),p%val, &
          CHAR(p%description))
    TYPE IS(ParamType_SDK_a4)
      CALL thisParam%init(CHAR(p%name),p%val, &
          CHAR(p%description))
    TYPE IS(ParamType_SNK_a4)
      CALL thisParam%init(CHAR(p%name),p%val, &
          CHAR(p%description))
    TYPE IS(ParamType_SLK_a4)
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
      TYPE IS(ParamType_SSK_a4)
        SELECTTYPE(p2); TYPE IS(ParamType_SSK_a4)
          bool=(ALLOCATED(p1%val) .EQV. ALLOCATED(p2%val))
          IF(ALLOCATED(p1%val) .AND. ALLOCATED(p1%val)) THEN
            dims1(1:4)=SHAPE(p1%val)
            dims2(1:4)=SHAPE(p2%val)
            IF(ALL(dims1(1:4) == dims2(1:4))) bool=ALL(p1%val == p2%val)
          ENDIF
        ENDSELECT
      TYPE IS(ParamType_SDK_a4)
        SELECTTYPE(p2); TYPE IS(ParamType_SDK_a4)
          bool=(ALLOCATED(p1%val) .EQV. ALLOCATED(p2%val))
          IF(ALLOCATED(p1%val) .AND. ALLOCATED(p1%val)) THEN
            dims1(1:4)=SHAPE(p1%val)
            dims2(1:4)=SHAPE(p2%val)
            IF(ALL(dims1(1:4) == dims2(1:4))) bool=ALL(p1%val == p2%val)
          ENDIF
        ENDSELECT
      TYPE IS(ParamType_SNK_a4)
        SELECTTYPE(p2); TYPE IS(ParamType_SNK_a4)
          bool=(ALLOCATED(p1%val) .EQV. ALLOCATED(p2%val))
          IF(ALLOCATED(p1%val) .AND. ALLOCATED(p1%val)) THEN
            dims1(1:4)=SHAPE(p1%val)
            dims2(1:4)=SHAPE(p2%val)
            IF(ALL(dims1(1:4) == dims2(1:4))) bool=ALL(p1%val == p2%val)
          ENDIF
        ENDSELECT
      TYPE IS(ParamType_SLK_a4)
        SELECTTYPE(p2); TYPE IS(ParamType_SLK_a4)
          bool=(ALLOCATED(p1%val) .EQV. ALLOCATED(p2%val))
          IF(ALLOCATED(p1%val) .AND. ALLOCATED(p1%val)) THEN
            dims1(1:4)=SHAPE(p1%val)
            dims2(1:4)=SHAPE(p2%val)
            IF(ALL(dims1(1:4) == dims2(1:4))) bool=ALL(p1%val == p2%val)
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

  CHARACTER(LEN=LEN_TRIM(addr)) :: addrIn,newAddr
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
!> @brief Routine can be used as an "iterator" over sublists one level deep.
!>        It takes an absolute list address a sublist and returns the next
!>        sublist
!> @param thisParam the parent parameter list to obtain the next sublist from
!> @param addr the absolute address of the parent list
!> @param param a pointer to the next sublist, if null the first sublist will
!>        be returned
!>
!> To use this routine as an iterator, a loop of the following form should be
!> written in the client code:
!> @code
!> TYPE(StringType) :: addr
!> TYPE(ParameType) :: paramList
!> CLASS(ParamType),POINTER :: nextParam
!> TYPE(ParamType) :: iterPL
!> addr=''
!> nextParam => NULL()
!> CALL paramList%getSubPL(addr,nextParam)
!> DO WHILE(ASSOCIATED(nextParam))
!>   iterPL=nextParam
!>   !Do stuff with nextParam
!>   !...
!>   CALL paramList%getSubPL(addr,nextParam)
!> ENDDO
!> @endcode
!>
SUBROUTINE getSubParam_List(thisParam,addr,param)
  CLASS(ParamType),TARGET,INTENT(IN) :: thisParam
  TYPE(StringType),INTENT(IN) :: addr
  CLASS(ParamType),POINTER,INTENT(INOUT) :: param

  INTEGER(SIK) :: i,istt
  CLASS(ParamType),POINTER :: aParam,nextParam,tmpPtr

  nextParam => NULL()
  IF(LEN_TRIM(addr) > 0) THEN
    CALL get_ParamType(thisParam,CHAR(addr),aParam)
    IF(ASSOCIATED(aParam)) THEN
      SELECTTYPE(aParam); TYPE IS(ParamType_List)
        !Sublists only exist for lists
        IF(ALLOCATED(aParam%pList)) THEN
          istt=1
          DO i=1,SIZE(aParam%pList)
            tmpPtr => aParam%pList(i)%pdat
            IF(ASSOCIATED(tmpPtr,param)) THEN
              istt=i+1
              EXIT
            ENDIF
          ENDDO
          DO i=istt,SIZE(aParam%pList)
            SELECTTYPE(p => aParam%pList(i)%pdat); TYPE IS(ParamType_List)
              nextParam => aParam%pList(i)%pdat
              EXIT
            ENDSELECT
          ENDDO
        ENDIF
      ENDSELECT
    ENDIF
  ELSE
    SELECTTYPE(p => thisParam)
    TYPE IS(ParamType_List)
       tmpPtr => thisParam
      IF(.NOT.ASSOCIATED(tmpPtr,param)) nextParam => thisParam
    CLASS DEFAULT
      tmpPtr => thisParam%pdat
      IF(.NOT.ASSOCIATED(tmpPtr,param)) THEN
        SELECTTYPE(pdat => thisParam%pdat); TYPE IS(ParamType_List)
          nextParam => thisParam%pdat
        ENDSELECT
      ENDIF
    ENDSELECT
  ENDIF
  param => nextParam
ENDSUBROUTINE getSubParam_List
!
!-------------------------------------------------------------------------------
!> @brief Routine can be used as an "iterator" over parameters(not just lists)
!>        one level deep. It takes an absolute list address a parameter and
!>        returns the next parameter
!> @param thisParam the parent parameter list to obtain the next sublist from
!> @param addr the absolute address of the parent list
!> @param param a pointer to the next parameter(1-level deep), if null the first
!>        sub-parameter will be returned
!>
!> To use this routine as an iterator, a loop of the following form should be
!> written in the client code:
!> @code
!> TYPE(StringType) :: addr
!> TYPE(ParameType) :: paramList
!> CLASS(ParamType),POINTER :: nextParam
!> TYPE(ParamType) :: iterPL
!> addr=''
!> nextParam => NULL()
!> CALL paramList%getSubParams(addr,nextParam)
!> DO WHILE(ASSOCIATED(nextParam))
!>   iterPL=nextParam
!>   !Do stuff with nextParam
!>   !...
!>   CALL paramList%getSubParams(addr,nextParam)
!> ENDDO
!> @endcode
!>
SUBROUTINE getSubParams(thisParam,addr,param)
  CLASS(ParamType),TARGET,INTENT(IN) :: thisParam
  TYPE(StringType),INTENT(IN) :: addr
  CLASS(ParamType),POINTER,INTENT(INOUT) :: param

  INTEGER(SIK) :: i
  CLASS(ParamType),POINTER :: tmpParam,nextParam

  nextParam => NULL()
  tmpParam => NULL()
  IF(LEN_TRIM(addr) > 0) THEN
    !Address passed in is the parent list
    CALL get_ParamType(thisParam,TRIM(addr),tmpParam)
  ELSE
    !No Address so assume root
    IF(LEN_TRIM(thisParam%name) > 0) THEN
      tmpParam => thisParam
    ELSE
      tmpParam => thisParam%pdat
    ENDIF
  ENDIF

  IF(ASSOCIATED(tmpParam)) THEN
    !Check to make sure param is in thisParam
    !if param is null that's ok too because we're guaranteed to
    !be within thisParam
    SELECTTYPE(tp => tmpParam); TYPE IS(ParamType_List)
      IF(ALLOCATED(tp%pList)) THEN
        !Search the PL for an element
        IF(ASSOCIATED(param)) THEN
          DO i=1,SIZE(tp%pList)
            IF(ASSOCIATED(tp%pList(i)%pdat,param)) THEN
              IF(i+1 <= SIZE(tp%pList)) THEN
                nextParam => tp%pList(i+1)%pdat
              ELSE
                nextParam => NULL()
              ENDIF
              EXIT
            ENDIF
          ENDDO
        ELSE
          !Return first element
          IF(SIZE(tp%pList) > 0) THEN
            nextParam => tp%pList(1)%pdat
          ENDIF
        ENDIF
      ELSE
        !Return NULL if parent list is empty
        nextParam => NULL()
      ENDIF
    ENDSELECT
  ENDIF
  param => nextParam
ENDSUBROUTINE getSubParams
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
  CHARACTER(LEN=LEN(name)) :: thisname,nextname
  CHARACTER(LEN=:),ALLOCATABLE :: pname
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

  param => NULL()
  IF(LEN_TRIM(thisname) > 0) THEN
    SELECTTYPE(thisParam)
    TYPE IS(ParamType_List)
      CALL toUPPER(thisname)
      IF(LEN_TRIM(nextname) > 0) THEN
        !Set names to upper case for matching
        pname=CHAR(thisParam%name%upper())
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
        pname=CHAR(thisParam%name%upper())
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
        pname=CHAR(thisParam%pdat%name%upper())
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
        pname=CHAR(thisParam%name%upper())
        IF(TRIM(pname) == TRIM(thisname) .AND. LEN_TRIM(nextName) == 0) &
            param => thisParam
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
  TYPE(StringType) :: thisname,nextname,pname,listName
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
        CALL add_ParamType(thisParam%pdat,TRIM(nextname),newParam)
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

      pname=thisParam%name%upper()
      thisname = thisname%upper()
      IF(TRIM(pname) == TRIM(thisname)) THEN
        !only search if it's not the last name in the
        !full address. last name is guaranteed not to exist
        !and this prevents accidental partial matching in sublists.
        lsubListSearch=.FALSE.
        CALL add_ParamType(thisParam,TRIM(nextname),newParam)
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
                listName=TRIM(thisParam%pList(i)%pdat%name%upper())
            IF(TRIM(listName) == TRIM(thisName)) THEN
              tmpParam => thisParam%pList(i)%pdat
              EXIT
            ENDIF
          ENDDO
        ENDIF

        IF(ASSOCIATED(tmpParam)) THEN
          !Found parameter with matching name
          CALL add_ParamType(tmpParam,TRIM(nextname),newParam)
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
          thisname=newParam%pdat%name%upper()
        ELSE
          thisname=newParam%name%upper()
        ENDIF
        NULLIFY(tmpParam)
        DO i=1,np
          listName=''
          IF(ASSOCIATED(thisParam%pList(i)%pdat)) &
              listName=TRIM(thisParam%pList(i)%pdat%name%upper())
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
  CHARACTER(LEN=LEN(name)) :: thisname,nextname
  CHARACTER(LEN=:),ALLOCATABLE :: pname
  INTEGER(SIK) :: i,ipos,np,npnew
  TYPE(ParamType),ALLOCATABLE :: tmpList(:)

  ipos=INDEX(name,'->')
  thisname=name
  nextname=''
  IF(ipos > 0) THEN
    thisname=ADJUSTL(name(1:ipos-1))
    nextname=ADJUSTL(name(ipos+2:LEN(name)))
  ENDIF

  IF(LEN_TRIM(thisname) > 0) THEN
    SELECTTYPE(thisParam)
    TYPE IS(ParamType_List)
      IF(LEN_TRIM(nextname) > 0) THEN
        !Set names to upper case for matching
        pname=CHAR(thisParam%name%upper())
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
        pname=CHAR(thisParam%pdat%name%upper())
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
!> @brief This subroutine takes a parameter type and a path, and converts
!>        whatever intrinsic parameter type it finds into a scalar string. This
!>        will not work if the parameter type is a parameter list.
!> @param thisParam The parameter type to be searched
!> @param name The path name to the parameter to be converted to a string
!> @param string The output scalar string type
!> @param sskfmt The optional single floating point format character string
!> @param sdkfmt The optional double floating point format character string
!>
SUBROUTINE getString_ParamType_scalar(thisParam,name,string,sskfmt,sdkfmt)
  CHARACTER(LEN=*),PARAMETER :: myName='getString_ParamType_scalar'
  CLASS(ParamType),TARGET,INTENT(IN) :: thisParam
  CHARACTER(LEN=*),INTENT(IN) :: name
  TYPE(StringType),INTENT(OUT) :: string
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: sskfmt
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: sdkfmt
  INTEGER(SIK) :: i,j,k,m
  CLASS(ParamType),POINTER :: param
  CHARACTER(LEN=16) :: sskfmtDef,sdkfmtDef
  CHARACTER(LEN=128) :: tmpchar
  TYPE(StringType) :: delim

  IF(PRESENT(sskfmt)) THEN
    sskfmtDef=sskfmt
  ELSE
    sskfmtDef='(es14.6)'
  ENDIF
  IF(PRESENT(sdkfmt)) THEN
    sdkfmtDef=sdkfmt
  ELSE
    sdkfmtDef='(es23.15)'
  ENDIF
  delim='"'
  string=''
  CALL thisParam%get(name,param)
  IF(ASSOCIATED(param)) THEN
    SELECTTYPE(param)
    TYPE IS(ParamType_List)
      !Error, can't do anything with a Plist.
    TYPE IS(ParamType_SSK)
      WRITE(tmpchar,TRIM(sskfmtDef)) param%val
      string=TRIM(ADJUSTL(tmpchar))
    TYPE IS(ParamType_SDK)
      WRITE(tmpchar,TRIM(sdkfmtDef)) param%val
      string=TRIM(ADJUSTL(tmpchar))
    TYPE IS(ParamType_SNK)
      string=str(param%val)
    TYPE IS(ParamType_SLK)
      string=str(param%val)
    TYPE IS(ParamType_SBK)
      WRITE(tmpchar,'(L1)') param%val
      string=TRIM(ADJUSTL(tmpchar))
    TYPE IS(ParamType_STR)
      string=param%val
    TYPE IS(ParamType_SSK_a1)
      WRITE(tmpchar,TRIM(sskfmtDef)) param%val(1)
      string=delim//TRIM(ADJUSTL(tmpchar))//delim//' '
      DO i=2,SIZE(param%val)
        WRITE(tmpchar,TRIM(sskfmtDef)) param%val(i)
        string=string//delim//TRIM(ADJUSTL(tmpchar))//delim//' '
      ENDDO
    TYPE IS(ParamType_SDK_a1)
      WRITE(tmpchar,TRIM(sdkfmtDef)) param%val(1)
      string=delim//TRIM(ADJUSTL(tmpchar))//delim//' '
      DO i=2,SIZE(param%val)
        WRITE(tmpchar,TRIM(sdkfmtDef)) param%val(i)
        string=string//delim//TRIM(ADJUSTL(tmpchar))//delim//' '
      ENDDO
    TYPE IS(ParamType_SNK_a1)
      string=delim//str(param%val(1))//delim//' '
      DO i=2,SIZE(param%val)
        string=string//delim//str(param%val(i))//delim//' '
      ENDDO
    TYPE IS(ParamType_SLK_a1)
      string=delim//str(param%val(1))//delim//' '
      DO i=2,SIZE(param%val)
        string=string//delim//str(param%val(i))//delim//' '
      ENDDO
    TYPE IS(ParamType_SBK_a1)
      WRITE(tmpchar,'(L1)') param%val(1)
      string=delim//TRIM(ADJUSTL(tmpchar))//delim//' '
      DO i=2,SIZE(param%val)
        WRITE(tmpchar,'(L1)') param%val(i)
        string=string//delim//TRIM(ADJUSTL(tmpchar))//delim//' '
      ENDDO
    TYPE IS(ParamType_STR_a1)
      string=delim//param%val(1)//delim//' '
      DO i=2,SIZE(param%val)
        string=string//delim//param%val(i)//delim//' '
      ENDDO
    TYPE IS(ParamType_SSK_a2)
      string=''
      DO j=1,SIZE(param%val,DIM=2)
        DO i=1,SIZE(param%val,DIM=1)
          WRITE(tmpchar,TRIM(sskfmtDef)) param%val(i,j)
          string=string//delim//TRIM(ADJUSTL(tmpchar))//delim//' '
        ENDDO
      ENDDO
    TYPE IS(ParamType_SDK_a2)
      string=''
      DO j=1,SIZE(param%val,DIM=2)
        DO i=1,SIZE(param%val,DIM=1)
          WRITE(tmpchar,TRIM(sdkfmtDef)) param%val(i,j)
          string=string//delim//TRIM(ADJUSTL(tmpchar))//delim//' '
        ENDDO
      ENDDO
    TYPE IS(ParamType_SNK_a2)
      string=''
      DO j=1,SIZE(param%val,DIM=2)
        DO i=1,SIZE(param%val,DIM=1)
          string=string//delim//str(param%val(i,j))//delim//' '
        ENDDO
      ENDDO
    TYPE IS(ParamType_SLK_a2)
      string=''
      DO j=1,SIZE(param%val,DIM=2)
        DO i=1,SIZE(param%val,DIM=1)
          string=string//delim//str(param%val(i,j))//delim//' '
        ENDDO
      ENDDO
    TYPE IS(ParamType_STR_a2)
      string=''
      DO j=1,SIZE(param%val,DIM=2)
        DO i=1,SIZE(param%val,DIM=1)
          string=string//delim//param%val(i,j)//delim//' '
        ENDDO
      ENDDO
    TYPE IS(ParamType_SSK_a3)
      string=''
      DO k=1,SIZE(param%val,DIM=3)
        DO j=1,SIZE(param%val,DIM=2)
          DO i=1,SIZE(param%val,DIM=1)
            WRITE(tmpchar,TRIM(sskfmtDef)) param%val(i,j,k)
            string=string//delim//TRIM(ADJUSTL(tmpchar))//delim//' '
          ENDDO
        ENDDO
      ENDDO
    TYPE IS(ParamType_SDK_a3)
      string=''
      DO k=1,SIZE(param%val,DIM=3)
        DO j=1,SIZE(param%val,DIM=2)
          DO i=1,SIZE(param%val,DIM=1)
            WRITE(tmpchar,TRIM(sdkfmtDef)) param%val(i,j,k)
            string=string//delim//TRIM(ADJUSTL(tmpchar))//delim//' '
          ENDDO
        ENDDO
      ENDDO
    TYPE IS(ParamType_SNK_a3)
      string=''
      DO k=1,SIZE(param%val,DIM=3)
        DO j=1,SIZE(param%val,DIM=2)
          DO i=1,SIZE(param%val,DIM=1)
            string=string//delim//str(param%val(i,j,k))//delim//' '
          ENDDO
        ENDDO
      ENDDO
    TYPE IS(ParamType_SLK_a3)
      string=''
      DO k=1,SIZE(param%val,DIM=3)
        DO j=1,SIZE(param%val,DIM=2)
          DO i=1,SIZE(param%val,DIM=1)
            string=string//delim//str(param%val(i,j,k))//delim//' '
          ENDDO
        ENDDO
      ENDDO
    TYPE IS(ParamType_SSK_a4)
      string=''
      DO m=1,SIZE(param%val,DIM=4)
        DO k=1,SIZE(param%val,DIM=3)
          DO j=1,SIZE(param%val,DIM=2)
            DO i=1,SIZE(param%val,DIM=1)
              WRITE(tmpchar,TRIM(sskfmtDef)) param%val(i,j,k,m)
              string=string//delim//TRIM(ADJUSTL(tmpchar))//delim//' '
            ENDDO
          ENDDO
        ENDDO
      ENDDO
    TYPE IS(ParamType_SDK_a4)
      string=''
      DO m=1,SIZE(param%val,DIM=4)
        DO k=1,SIZE(param%val,DIM=3)
          DO j=1,SIZE(param%val,DIM=2)
            DO i=1,SIZE(param%val,DIM=1)
              WRITE(tmpchar,TRIM(sdkfmtDef)) param%val(i,j,k,m)
              string=string//delim//TRIM(ADJUSTL(tmpchar))//delim//' '
            ENDDO
          ENDDO
        ENDDO
      ENDDO
    TYPE IS(ParamType_SNK_a4)
      string=''
      DO m=1,SIZE(param%val,DIM=4)
        DO k=1,SIZE(param%val,DIM=3)
          DO j=1,SIZE(param%val,DIM=2)
            DO i=1,SIZE(param%val,DIM=1)
              string=string//delim//str(param%val(i,j,k,m))//delim//' '
            ENDDO
          ENDDO
        ENDDO
      ENDDO
    TYPE IS(ParamType_SLK_a4)
      string=''
      DO m=1,SIZE(param%val,DIM=4)
        DO k=1,SIZE(param%val,DIM=3)
          DO j=1,SIZE(param%val,DIM=2)
            DO i=1,SIZE(param%val,DIM=1)
              string=string//delim//str(param%val(i,j,k,m))//delim//' '
            ENDDO
          ENDDO
        ENDDO
      ENDDO
    CLASS DEFAULT
      CALL eParams%raiseError(modName//'::'//myName//' - The ParamType '// &
          'is unknown or undefined, so it cannot be converted to a String!')
    ENDSELECT
    string=TRIM(string)
  ENDIF
ENDSUBROUTINE getString_ParamType_scalar
!
!-------------------------------------------------------------------------------
!> @brief This subroutine takes a parameter type and a path, and converts
!>        the 1-D intrinsic parameter type it finds into a 1-D array of strings.
!>        This will not work if the parameter type is a parameter list.
!> @param thisParam The parameter type to be searched
!> @param name The path name to the parameter to be converted to a 1-D array of
!>        strings
!> @param string The output 1-D array of strings
!> @param sskfmt The optional single floating point format character string
!> @param sdkfmt The optional double floating point format character string
!>
SUBROUTINE getString_ParamType_a1(thisParam,name,string,sskfmt,sdkfmt)
  CHARACTER(LEN=*),PARAMETER :: myName='getString_ParamType_a1'
  CLASS(ParamType),TARGET,INTENT(IN) :: thisParam
  CHARACTER(LEN=*),INTENT(IN) :: name
  TYPE(StringType),ALLOCATABLE,INTENT(INOUT) :: string(:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: sskfmt
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: sdkfmt
  CLASS(ParamType),POINTER :: param
  CHARACTER(LEN=16) :: sskfmtDef,sdkfmtDef
  CHARACTER(LEN=128) :: tmpchar
  INTEGER(SIK) :: i

  IF(PRESENT(sskfmt)) THEN
    sskfmtDef=sskfmt
  ELSE
    sskfmtDef='(es14.6)'
  ENDIF
  IF(PRESENT(sdkfmt)) THEN
    sdkfmtDef=sdkfmt
  ELSE
    sdkfmtDef='(es23.15)'
  ENDIF
  CALL thisParam%get(name,param)
  IF(ALLOCATED(string)) DEALLOCATE(string)
  IF(ASSOCIATED(param)) THEN
    SELECTTYPE(param)
    TYPE IS(ParamType_SSK_a1)
      ALLOCATE(string(SIZE(param%val,DIM=1)))
      DO i=1,SIZE(param%val)
        WRITE(tmpchar,TRIM(sskfmtDef)) param%val(i)
        string(i)=TRIM(ADJUSTL(tmpchar))
      ENDDO
    TYPE IS(ParamType_SDK_a1)
      ALLOCATE(string(SIZE(param%val,DIM=1)))
      DO i=1,SIZE(param%val)
        WRITE(tmpchar,TRIM(sdkfmtDef)) param%val(i)
        string(i)=TRIM(ADJUSTL(tmpchar))
      ENDDO
    TYPE IS(ParamType_SNK_a1)
      ALLOCATE(string(SIZE(param%val,DIM=1)))
      DO i=1,SIZE(param%val)
        string(i)=str(param%val(i))
      ENDDO
    TYPE IS(ParamType_SLK_a1)
      ALLOCATE(string(SIZE(param%val,DIM=1)))
      DO i=1,SIZE(param%val)
        string(i)=str(param%val(i))
      ENDDO
    TYPE IS(ParamType_SBK_a1)
      ALLOCATE(string(SIZE(param%val,DIM=1)))
      DO i=1,SIZE(param%val)
        WRITE(tmpchar,'(L1)') param%val(i)
        string(i)=TRIM(ADJUSTL(tmpchar))
      ENDDO
    TYPE IS(ParamType_STR_a1)
      ALLOCATE(string(SIZE(param%val,DIM=1)))
      DO i=1,SIZE(param%val)
        string(i)=param%val(i)
      ENDDO
    CLASS DEFAULT
      CALL eParams%raiseError(modName//'::'//myName//' - The ParamType '// &
          'is unknown or undefined, so it cannot be converted to a 1-D '// &
          'String Array!')
    ENDSELECT
  ENDIF
ENDSUBROUTINE getString_ParamType_a1
!
!-------------------------------------------------------------------------------
!> @brief This subroutine takes a parameter type and a path, and converts
!>        the 2-D intrinsic parameter type it finds into a 2-D array of strings.
!>        This will not work if the parameter type is a parameter list.
!> @param thisParam The parameter type to be searched
!> @param name The path name to the parameter to be converted to a 2-D array of
!>        strings
!> @param string The output 2-D array of strings
!> @param sskfmt The optional single floating point format character string
!> @param sdkfmt The optional double floating point format character string
!>
SUBROUTINE getString_ParamType_a2(thisParam,name,string,sskfmt,sdkfmt)
  CHARACTER(LEN=*),PARAMETER :: myName='getString_ParamType_a2'
  CLASS(ParamType),TARGET,INTENT(IN) :: thisParam
  CHARACTER(LEN=*),INTENT(IN) :: name
  TYPE(StringType),ALLOCATABLE,INTENT(INOUT) :: string(:,:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: sskfmt
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: sdkfmt
  CLASS(ParamType),POINTER :: param
  CHARACTER(LEN=16) :: sskfmtDef,sdkfmtDef
  CHARACTER(LEN=128) :: tmpchar
  INTEGER(SIK) :: i,j

  IF(PRESENT(sskfmt)) THEN
    sskfmtDef=sskfmt
  ELSE
    sskfmtDef='(es14.6)'
  ENDIF
  IF(PRESENT(sdkfmt)) THEN
    sdkfmtDef=sdkfmt
  ELSE
    sdkfmtDef='(es23.15)'
  ENDIF
  CALL thisParam%get(name,param)
  IF(ALLOCATED(string)) DEALLOCATE(string)
  IF(ASSOCIATED(param)) THEN
    SELECTTYPE(param)
    TYPE IS(ParamType_SSK_a2)
      ALLOCATE(string(SIZE(param%val,DIM=1),SIZE(param%val,DIM=2)))
      DO j=1,SIZE(param%val,DIM=2)
        DO i=1,SIZE(param%val,DIM=1)
          WRITE(tmpchar,TRIM(sskfmtDef)) param%val(i,j)
          string(i,j)=TRIM(ADJUSTL(tmpchar))
        ENDDO
      ENDDO
    TYPE IS(ParamType_SDK_a2)
      ALLOCATE(string(SIZE(param%val,DIM=1),SIZE(param%val,DIM=2)))
      DO j=1,SIZE(param%val,DIM=2)
        DO i=1,SIZE(param%val,DIM=1)
          WRITE(tmpchar,TRIM(sdkfmtDef)) param%val(i,j)
          string(i,j)=TRIM(ADJUSTL(tmpchar))
        ENDDO
      ENDDO
    TYPE IS(ParamType_SNK_a2)
      ALLOCATE(string(SIZE(param%val,DIM=1),SIZE(param%val,DIM=2)))
      DO j=1,SIZE(param%val,DIM=2)
        DO i=1,SIZE(param%val,DIM=1)
          string(i,j)=str(param%val(i,j))
        ENDDO
      ENDDO
    TYPE IS(ParamType_SLK_a2)
      ALLOCATE(string(SIZE(param%val,DIM=1),SIZE(param%val,DIM=2)))
      DO j=1,SIZE(param%val,DIM=2)
        DO i=1,SIZE(param%val,DIM=1)
          string(i,j)=str(param%val(i,j))
        ENDDO
      ENDDO
    TYPE IS(ParamType_STR_a2)
      ALLOCATE(string(SIZE(param%val,DIM=1),SIZE(param%val,DIM=2)))
      DO j=1,SIZE(param%val,DIM=2)
        DO i=1,SIZE(param%val,DIM=1)
          string(i,j)=param%val(i,j)
        ENDDO
      ENDDO
    CLASS DEFAULT
      CALL eParams%raiseError(modName//'::'//myName//' - The ParamType '// &
          'is unknown or undefined, so it cannot be converted to a 2-D '// &
          'String Array!')
    ENDSELECT
  ENDIF
ENDSUBROUTINE getString_ParamType_a2
!
!-------------------------------------------------------------------------------
!> @brief This subroutine takes a parameter type and a path, and converts
!>        the 3-D intrinsic parameter type it finds into a 3-D array of strings.
!>        This will not work if the parameter type is a parameter list.
!> @param thisParam The parameter type to be searched
!> @param name The path name to the parameter to be converted to a 3-D array of
!>        strings
!> @param string The output 3-D array of strings
!> @param sskfmt The optional single floating point format character string
!> @param sdkfmt The optional double floating point format character string
!>
SUBROUTINE getString_ParamType_a3(thisParam,name,string,sskfmt,sdkfmt)
  CHARACTER(LEN=*),PARAMETER :: myName='getString_ParamType_a3'
  CLASS(ParamType),TARGET,INTENT(IN) :: thisParam
  CHARACTER(LEN=*),INTENT(IN) :: name
  TYPE(StringType),ALLOCATABLE,INTENT(INOUT) :: string(:,:,:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: sskfmt
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: sdkfmt
  CLASS(ParamType),POINTER :: param
  CHARACTER(LEN=16) :: sskfmtDef,sdkfmtDef
  CHARACTER(LEN=128) :: tmpchar
  INTEGER(SIK) :: i,j,k

  IF(PRESENT(sskfmt)) THEN
    sskfmtDef=sskfmt
  ELSE
    sskfmtDef='(es14.6)'
  ENDIF
  IF(PRESENT(sdkfmt)) THEN
    sdkfmtDef=sdkfmt
  ELSE
    sdkfmtDef='(es23.15)'
  ENDIF
  CALL thisParam%get(name,param)
  IF(ALLOCATED(string)) DEALLOCATE(string)
  IF(ASSOCIATED(param)) THEN
    SELECTTYPE(param)
    TYPE IS(ParamType_SSK_a3)
      ALLOCATE(string(SIZE(param%val,DIM=1),SIZE(param%val,DIM=2),SIZE(param%val,DIM=3)))
      DO k=1,SIZE(param%val,DIM=3)
        DO j=1,SIZE(param%val,DIM=2)
          DO i=1,SIZE(param%val,DIM=1)
            WRITE(tmpchar,TRIM(sskfmtDef)) param%val(i,j,k)
            string(i,j,k)=TRIM(ADJUSTL(tmpchar))
          ENDDO
        ENDDO
      ENDDO
    TYPE IS(ParamType_SDK_a3)
      ALLOCATE(string(SIZE(param%val,DIM=1),SIZE(param%val,DIM=2),SIZE(param%val,DIM=3)))
      DO k=1,SIZE(param%val,DIM=3)
        DO j=1,SIZE(param%val,DIM=2)
          DO i=1,SIZE(param%val,DIM=1)
            WRITE(tmpchar,TRIM(sdkfmtDef)) param%val(i,j,k)
            string(i,j,k)=TRIM(ADJUSTL(tmpchar))
          ENDDO
        ENDDO
      ENDDO
    TYPE IS(ParamType_SNK_a3)
      ALLOCATE(string(SIZE(param%val,DIM=1),SIZE(param%val,DIM=2),SIZE(param%val,DIM=3)))
      DO k=1,SIZE(param%val,DIM=3)
        DO j=1,SIZE(param%val,DIM=2)
          DO i=1,SIZE(param%val,DIM=1)
            string(i,j,k)=str(param%val(i,j,k))
          ENDDO
        ENDDO
      ENDDO
    TYPE IS(ParamType_SLK_a3)
      ALLOCATE(string(SIZE(param%val,DIM=1),SIZE(param%val,DIM=2),SIZE(param%val,DIM=3)))
      DO k=1,SIZE(param%val,DIM=3)
        DO j=1,SIZE(param%val,DIM=2)
          DO i=1,SIZE(param%val,DIM=1)
            string(i,j,k)=str(param%val(i,j,k))
          ENDDO
        ENDDO
      ENDDO
    CLASS DEFAULT
      CALL eParams%raiseError(modName//'::'//myName//' - The ParamType '// &
          'is unknown or undefined, so it cannot be converted to a 3-D '// &
          'String Array!')
    ENDSELECT
  ENDIF
ENDSUBROUTINE getString_ParamType_a3
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
!> @brief This subroutine will take a parameter list of parameter lists, where
!>        each parameter list is a column to be added in the table. The first
!>        parameter list must be the maximum number of rows and must be uniquely
!>        labeled. The ordering of the parameters on the first list is the
!>        order they will be written to the table. The following columns of
!>        parameter lists must have parameters with names that match those the
!>        first column. The value given to the parameter is arbitrary. This
!>        ensures that the parameter will be placed in the desired location.
!>
!> Example:
!>    thisParam:
!>    'TestPL->List1->1->"TitleRow"','"TitleRow"'
!>                   '1->"Scalar Row1"','"Scalar Row1"'
!>                   '1->"1-D Row2"','"1-D Row2"'
!>                   '1->"2-D Row3"','"2-D Row3"'
!>                   '1->"3-D Row4"','"3-D Row4"'
!>            'List1->2->"TitleRow"','SNK'
!>                   '2->"Scalar Row1"',1_SNK
!>                   '2->"1-D Row2"',(/2_SNK,3_SNK/)
!>                   '2->"2-D Row3"',RESHAPE((/4_SNK,5_SNK,6_SNK,7_SNK/),(/2,2/))
!>                   '2->"3-D Row4"',RESHAPE((/8_SNK,9_SNK,11_SNK,12_SNK,
!>                                             13_SNK,14_SNK,15_SNK,16_SNK/),(/2,2,2/))
!>            'List1->3->"TitleRow"','SLK'
!>                   '3->"Scalar Row1"',1_SLK
!>                   '3->"1-D Row2"',(/2_SLK,3_SLK/)
!>                   '3->"2-D Row3"',RESHAPE((/4_SLK,5_SLK,6_SLK,7_SLK/),(/2,2/))
!>                   '3->"3-D Row4"',RESHAPE((/8_SLK,9_SLK,11_SLK,12_SLK,
!>                                             13_SLK,14_SLK,15_SLK,16_SLK/),(/2,2,2/))
!>
!>    baseAddr='TestPL->List1'
!>    CALL thisParam%convertTo2DStringArray(baseAddr,table)
!>    table:
!>      x=    1           2                                      3
!>  y=1  "Title Row"      SNK                                    SLK
!>    2  "Scalar Row1"    1                                      1
!>    3  "1-D Row2"       "2" "3"                                "2" "3"
!>    4  "2-D Row3"       "4" "5" "6" "7"                        "4" "5" "6" "7"
!>    5  "3-D Row4"       "8" "9" "11" "12" "13" "14" "15" "16"  "8" "9" "11" "12" "13" "14" "15" "16"
!>
!> @param thisParam The parameter list of parameter lists from which to create a
!>        table.
!> @param baseAddr The path used to extract the parameter list of parameter
!>        lists.
!> @param tablevals The 2-D string array that is allocated and returned.
!>
SUBROUTINE convertTo2DStringArray_ParamType(thisParam,baseAddr,tablevals)
  CLASS(ParamType),INTENT(IN) :: thisParam
  TYPE(StringType),INTENT(IN) :: baseAddr
  TYPE(StringType),ALLOCATABLE,INTENT(OUT) :: tablevals(:,:)
  INTEGER(SIK) :: i,j,ncol,nrow
  TYPE(StringType) :: addr,plstr
  TYPE(StringType),ALLOCATABLE :: rownames(:)
  TYPE(ParamType) :: colListPL,rowListPL
  CLASS(ParamType),POINTER :: colListPLPtr,rowListPLPtr

  !Initialize data
  !Loop over all columns, get the number of columns for the table
  ncol=0
  nrow=0
  addr=baseAddr
  colListPLPtr => NULL()
  CALL thisParam%getSubPL(addr,colListPLPtr)
  DO WHILE(ASSOCIATED(colListPLPtr))
    ncol=ncol+1
    CALL thisParam%getSubPL(addr,colListPLPtr)
  ENDDO

  addr='1'
  CALL thisParam%get(baseAddr//'->1',colListPLPtr)
  IF(ASSOCIATED(colListPLPtr)) THEN
    colListPL=colListPLPtr
    CALL colListPL%getNextParam(addr,rowListPLPtr)
    !Loop over first column, get the number of rows for the table
    DO WHILE(ASSOCIATED(rowListPLPtr))
      nrow=nrow+1
      CALL colListPL%getNextParam(addr,rowListPLPtr)
    ENDDO
  ENDIF

  IF((nrow > 0) .AND. (ncol > 0)) THEN
    !Allocate data
    ALLOCATE(tablevals(ncol,nrow))
    ALLOCATE(rownames(nrow))
    tablevals='-'
    rownames=''

    !Get rownames so they can be searched and indexed.
    addr='1'
    CALL thisParam%get(baseAddr//'->1',colListPLPtr)
    colListPL=colListPLPtr
    DO j=1,nrow
      CALL colListPL%getNextParam(addr,rowListPLPtr)
      rowListPL=rowListPLPtr
      CALL rowListPL%get(CHAR(rowListPL%pdat%name),rownames(j))
      CALL rowListPL%getString(TRIM(rowListPL%pdat%name),plstr)
      IF(LEN_TRIM(plstr) > 0) tablevals(1,j)=plstr
    ENDDO

    !Loop over all of the columns
    DO i=2,ncol
      !Init variables
      addr=i
      !Get the specified sub PL list to iterate over.
      CALL thisParam%get(baseAddr//'->'//addr,colListPLPtr)
      colListPL=colListPLPtr
      !Get the first parameter in the sublist and loop.
      CALL colListPL%getNextParam(addr,rowListPLPtr)
      DO WHILE(ASSOCIATED(rowListPLPtr))
        rowListPL=rowListPLPtr
        !Since we assume the PL is not a full fixed column, find the j index
        j=strarrayeqind(rownames,rowListPL%pdat%name)
        !Add data to string table
        CALL rowListPL%getString(TRIM(rowListPL%pdat%name),plstr)
        IF(LEN_TRIM(plstr) > 0) tablevals(i,j)=plstr
        CALL colListPL%getNextParam(addr,rowListPLPtr)
        CALL rowListPL%clear()
      ENDDO
      CALL colListPL%clear()
    ENDDO

    !Deallocate and nullify variables
    DEALLOCATE(rownames)
    colListPLPtr => NULL()
    rowListPLPtr => NULL()
  ENDIF
ENDSUBROUTINE convertTo2DStringArray_ParamType
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
RECURSIVE SUBROUTINE edit_ParamType(thisParam,funit,indent,prefix,paddtw)
  CLASS(ParamType),INTENT(IN) :: thisParam
  INTEGER(SIK),INTENT(IN) :: funit
  INTEGER(SIK),INTENT(IN),OPTIONAL :: indent
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: prefix
  LOGICAL(SBK),INTENT(IN),OPTIONAL :: paddtw
  INTEGER(SIK) :: i

  i=3
  IF(PRESENT(indent)) i=i+indent
  IF(ASSOCIATED(thisParam%pdat)) &
      CALL thisParam%pdat%edit(funit,i,prefix,paddtw)
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
RECURSIVE SUBROUTINE validateReq_ParamType(thisParam,reqParams,prefix,validType, &
    isValid,isMatch,e)
  CHARACTER(LEN=*),PARAMETER :: myName='validateReq_ParamType'
  CLASS(ParamType),INTENT(INOUT) :: thisParam
  CLASS(ParamType),INTENT(IN) :: reqParams
  CHARACTER(LEN=*),INTENT(IN) :: prefix
  INTEGER(SIK),INTENT(IN) :: validType
  LOGICAL(SBK),INTENT(OUT) :: isValid
  LOGICAL(SBK),INTENT(OUT) :: isMatch
  CLASS(ExceptionHandlerType),INTENT(INOUT) :: e
  LOGICAL(SBK) :: tmpbool
  INTEGER(SIK) :: i,ntrue
  CLASS(ParamType),POINTER :: tmpParam

  isValid=.FALSE.
!
!Loop over all required params in reqParams and search thisParam for
!each parameter and check type
  SELECTTYPE(p=>reqParams)
  TYPE IS(ParamType)
    !Call validate on the required parameter's value
    IF((validType == VALIDTYPE_VERIFYTEST) .OR. &
        (validType == VALIDTYPE_VERIFYLIST)) THEN
      IF(ASSOCIATED(p%pdat)) &
          CALL validateReq_ParamType(thisParam,p%pdat,prefix,validType,isValid,isMatch,e)
    ELSE
      IF(ASSOCIATED(p%pdat)) &
          CALL validateReq_ParamType(thisParam,p%pdat,prefix,validType,isValid,tmpbool,e)
    ENDIF
  TYPE IS(ParamType_List)
    !Loop over all parameters in the list and check each
    IF(ALLOCATED(p%pList)) THEN
      ntrue=0
      DO i=1,SIZE(p%pList)
        IF((validType == VALIDTYPE_VERIFYTEST) .OR. &
            (validType == VALIDTYPE_VERIFYLIST)) THEN
          CALL validateReq_ParamType(thisParam,p%pList(i), &
              prefix//p%name//'->',validType,isValid,isMatch,e)
          IF(isValid) ntrue=ntrue+1
        ELSE
          CALL validateReq_ParamType(thisParam,p%pList(i), &
              prefix//p%name//'->',validType,isValid,tmpbool,e)
          IF(isValid) ntrue=ntrue+1
        ENDIF
      ENDDO
      isValid=(ntrue == SIZE(p%pList))
    ELSE
      !The required list is not allocated, which means we do not
      !check any of it's possible subparameters, but we must at least
      !check that the list exists
      CALL thisParam%getParam(prefix//p%name,tmpParam)
      IF(.NOT.ASSOCIATED(tmpParam)) THEN
        SELECTCASE(validType)
        CASE(VALIDTYPE_VERIFYLIST,VALIDTYPE_VERIFYTEST)
          isMatch=.FALSE.
          CALL e%raiseError(modName//'::'//myName// &
              ' - When verifying that parameters are equal, the parameter "'// &
              prefix//p%name//'" was not found on both lists!')
        CASE DEFAULT
          CALL e%raiseError(modName//'::'//myName// &
              ' - Failed to locate required parameter "'//prefix// &
              p%name//'"!')
        ENDSELECT
      ELSE
        IF(SAME_TYPE_AS(tmpParam,p)) THEN
          isValid=.TRUE.
          SELECTCASE(validType)
          CASE(VALIDTYPE_VERIFYTEST)
            isMatch=isMatch .AND. matchTest_ParamType(tmpParam,p,prefix)
          CASE(VALIDTYPE_VERIFYLIST)
            isMatch=isMatch .AND. matchList_ParamType(tmpParam,p,prefix,e)
          ENDSELECT
        ELSE
          CALL e%raiseError(modName//'::'//myName// &
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
      SELECTCASE(validType)
      CASE(VALIDTYPE_VERIFYLIST,VALIDTYPE_VERIFYTEST)
        isMatch=.FALSE.
        CALL e%raiseError(modName//'::'//myName// &
            ' - When verifying that parameters are equal, the parameter "'// &
            prefix//p%name//'" was not found on both lists!')
      CASE DEFAULT
        CALL e%raiseError(modName//'::'//myName// &
            ' - Failed to locate required parameter "'//prefix// &
            p%name//'"!')
      ENDSELECT
    ELSE
      IF(SAME_TYPE_AS(tmpParam,p)) THEN
        isValid=.TRUE.
        SELECTCASE(validType)
        CASE(VALIDTYPE_VERIFYTEST)
          isMatch=isMatch .AND. matchTest_ParamType(tmpParam,p,prefix)
        CASE(VALIDTYPE_VERIFYLIST)
          isMatch=isMatch .AND. matchList_ParamType(tmpParam,p,prefix,e)
        ENDSELECT
      ELSE
        CALL e%raiseError(modName//'::'//myName// &
            ' - Required parameter "'//prefix//p%name//'" has type "'// &
            tmpParam%dataType//'" and must be type "'//p%dataType//'"!')
      ENDIF
    ENDIF
  ENDSELECT
ENDSUBROUTINE validateReq_ParamType
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
  LOGICAL(SBK) :: isValid,tmpbool
  TYPE(ParamType) :: nullParam

  !Assume the list is valid, check it only if the required parameter
  !list is not empty.
  isValid=.TRUE.
  IF(ASSOCIATED(reqParams%pdat)) &
      CALL validateReq_ParamType(thisParam,reqParams,'',VALIDTYPE_VALIDATE, &
      isValid,tmpbool,eParams)
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
SUBROUTINE verifyTest_Paramtype(thisParam,reqParams,isMatch)
  CLASS(ParamType),INTENT(INOUT) :: thisParam
  CLASS(ParamType),INTENT(IN) :: reqParams
  LOGICAL(SBK),INTENT(OUT) :: isMatch
  LOGICAL(SBK) :: isValid

  !Assume the list is valid, check it only if the required parameter
  !list is not empty.
  isValid=.TRUE.
  isMatch=.TRUE.
  IF(ASSOCIATED(reqParams%pdat)) THEN
    CALL validateReq_ParamType(thisParam,reqParams,'',VALIDTYPE_VERIFYTEST, &
        isValid,isMatch,eParams)
  ELSE
    isMatch=.NOT.ASSOCIATED(thisParam%pdat)
  ENDIF
ENDSUBROUTINE verifyTest_Paramtype
!
!-------------------------------------------------------------------------------
!> @brief Verify should only be used in a unit test setting.  It is for checking
!>        the structure AND values in two parameter lists.  If they are a match,
!>        isMatch will be returned as true.  If not, false.  Assertion failures
!>        will be printed for the parameter list values that fail.
!> @param thisParam The parameter list on which to verify the values
!> @param reqParams The reference parameter list and values
!> @param e The exception handler to pass
!> @param isMatch The logical if all parameter names and values are the same
!>
SUBROUTINE verifyList_Paramtype(thisParam,reqParams,isMatch,e)
  CLASS(ParamType),INTENT(INOUT) :: thisParam
  CLASS(ParamType),INTENT(IN) :: reqParams
  LOGICAL(SBK),INTENT(OUT) :: isMatch
  CLASS(ExceptionHandlerType),INTENT(INOUT) :: e
  LOGICAL(SBK) :: isValid

  !Assume the list is valid, check it only if the required parameter
  !list is not empty.
  isValid=.TRUE.
  isMatch=.TRUE.
  IF(ASSOCIATED(reqParams%pdat)) THEN
    CALL validateReq_ParamType(thisParam,reqParams,'',VALIDTYPE_VERIFYLIST, &
        isValid,isMatch,e)
  ELSE
    isMatch=.NOT.ASSOCIATED(thisParam%pdat)
  ENDIF
ENDSUBROUTINE verifyList_Paramtype
!
!-------------------------------------------------------------------------------
!> @brief This function checks the values of thisParam and thatParam and returns
!>        if they are equal or approximately equal.
!> @param thisParam  The parameter list being validated
!> @param thatParam  The parameter list being checked against
!> @param bool The logical result of the checked parameters.
!>
!> The assumptions of this routine are that the parameters passed in are the
!> same extended ParamType.  It also assumes that there is a "gettable" value
!> that is of thisParam%name on the ParamType.  This function determines
!> the extended type, then "gets" the appropriate parameter from both lists,
!> then checks their equivalence.  If they are equal or approximately equal,
!> the function results in true.  If not, false.  The function also performs
!> unit test harness assertions when checking the values.
!>
FUNCTION matchTest_ParamType(thisParam,thatParam,prefix) RESULT(bool)
  CLASS(ParamType),INTENT(INOUT) :: thisParam
  CLASS(ParamType),INTENT(IN),TARGET :: thatParam
  CHARACTER(LEN=*),INTENT(IN) :: prefix
  LOGICAL(SBK) :: bool
  CLASS(ParamType),POINTER :: paramPtr
  INTEGER(SIK) :: i,j
  LOGICAL(SBK) :: tmpsbk1,tmpsbk2
  LOGICAL(SBK),ALLOCATABLE :: tmpsbka11(:),tmpsbka12(:)
  REAL(SSK) :: tmpssk1,tmpssk2
  REAL(SSK),ALLOCATABLE :: tmpsska11(:),tmpsska21(:,:),tmpsska31(:,:,:),tmpsska41(:,:,:,:)
  REAL(SSK),ALLOCATABLE :: tmpsska12(:),tmpsska22(:,:),tmpsska32(:,:,:),tmpsska42(:,:,:,:)
  REAL(SDK) :: tmpsdk1,tmpsdk2
  REAL(SDK),ALLOCATABLE :: tmpsdka11(:),tmpsdka21(:,:),tmpsdka31(:,:,:),tmpsdka41(:,:,:,:)
  REAL(SDK),ALLOCATABLE :: tmpsdka12(:),tmpsdka22(:,:),tmpsdka32(:,:,:),tmpsdka42(:,:,:,:)
  INTEGER(SNK) :: tmpsnk1,tmpsnk2
  INTEGER(SNK),ALLOCATABLE :: tmpsnka11(:),tmpsnka21(:,:),tmpsnka31(:,:,:),tmpsnka41(:,:,:,:)
  INTEGER(SNK),ALLOCATABLE :: tmpsnka12(:),tmpsnka22(:,:),tmpsnka32(:,:,:),tmpsnka42(:,:,:,:)
  INTEGER(SLK) :: tmpslk1,tmpslk2
  INTEGER(SLK),ALLOCATABLE :: tmpslka11(:),tmpslka21(:,:),tmpslka31(:,:,:),tmpslka41(:,:,:,:)
  INTEGER(SLK),ALLOCATABLE :: tmpslka12(:),tmpslka22(:,:),tmpslka32(:,:,:),tmpslka42(:,:,:,:)
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
      IF(.NOT.bool) bool=ALL(SOFTEQ(tmpsdka11,tmpsdka12,EPSD*1000._SRK))
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
        FINFO() i
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
  TYPE IS(ParamType_SSK_a4)
    CALL thisParam%get(CHAR(thisParam%name),tmpsska41)
    CALL paramPtr%get(CHAR(paramPtr%name),tmpsska42)
    bool=SIZE(tmpsska41,DIM=1) == SIZE(tmpsska42,DIM=1)
    ASSERT(bool, 'SIZE DIM=1 of '//prefix//CHAR(thisParam%name))
    FINFO() SIZE(tmpsska41,DIM=1), SIZE(tmpsska42,DIM=1)
    IF(bool) THEN
      bool=SIZE(tmpsska41,DIM=2) == SIZE(tmpsska42,DIM=2)
      ASSERT(bool, 'SIZE DIM=2 of '//prefix//CHAR(thisParam%name))
      FINFO() SIZE(tmpsska41,DIM=2), SIZE(tmpsska42,DIM=2)
      IF(bool) THEN
        bool=SIZE(tmpsska41,DIM=3) == SIZE(tmpsska42,DIM=3)
        ASSERT(bool, 'SIZE DIM=3 of '//prefix//CHAR(thisParam%name))
        FINFO() SIZE(tmpsska41,DIM=3), SIZE(tmpsska42,DIM=3)
        IF(bool) THEN
          bool=SIZE(tmpsska41,DIM=4) == SIZE(tmpsska42,DIM=4)
          ASSERT(bool, 'SIZE DIM=4 of '//PREFIX//char(thisParam%name))
          FINFO() SIZE(tmpsska41,DIM=4),SIZE(tmpsska42,DIM=4)
          IF(bool) THEN
             bool=ALL(tmpsska41 .APPROXEQ. tmpsska42)
             ASSERT(bool, prefix//CHAR(thisParam%name))
             FINFO() 'test values=',tmpsska41
             FINFO() 'ref. values=',tmpsska42
           ENDIF
        ENDIF
      ENDIF
    ENDIF
    DEALLOCATE(tmpsska41); DEALLOCATE(tmpsska42)
  TYPE IS(ParamType_SDK_a4)
    CALL thisParam%get(CHAR(thisParam%name),tmpsdka41)
    CALL paramPtr%get(CHAR(paramPtr%name),tmpsdka42)
    bool=SIZE(tmpsdka41,DIM=1) == SIZE(tmpsdka42,DIM=1)
    ASSERT(bool, 'SIZE DIM=1 of '//prefix//CHAR(thisParam%name))
    FINFO() SIZE(tmpsdka41,DIM=1), SIZE(tmpsdka42,DIM=1)
    IF(bool) THEN
      bool=SIZE(tmpsdka41,DIM=2) == SIZE(tmpsdka42,DIM=2)
      ASSERT(bool, 'SIZE DIM=2 of '//prefix//CHAR(thisParam%name))
      FINFO() SIZE(tmpsdka41,DIM=2), SIZE(tmpsdka42,DIM=2)
      IF(bool) THEN
        bool=SIZE(tmpsdka41,DIM=3) == SIZE(tmpsdka42,DIM=3)
        ASSERT(bool, 'SIZE DIM=3 of '//prefix//CHAR(thisParam%name))
        FINFO() SIZE(tmpsdka41,DIM=3), SIZE(tmpsdka42,DIM=3)
        IF(bool) THEN
          bool=SIZE(tmpsdka41,DIM=4) == SIZE(tmpsdka42,DIM=4)
          ASSERT(bool, 'SIZE DIM=4 of '//PREFIX//char(thisParam%name))
          FINFO() SIZE(tmpsdka41,DIM=4),SIZE(tmpsdka42,DIM=4)
          IF(bool) THEN
            bool=ALL(tmpsdka41 .APPROXEQ. tmpsdka42)
            ASSERT(bool, prefix//CHAR(thisParam%name))
            FINFO() 'test values=',tmpsdka41
            FINFO() 'ref. values=',tmpsdka42
          ENDIF
        ENDIF
      ENDIF
    ENDIF
    DEALLOCATE(tmpsdka41); DEALLOCATE(tmpsdka42)
  TYPE IS(ParamType_SNK_a4)
    CALL thisParam%get(CHAR(thisParam%name),tmpsnka41)
    CALL paramPtr%get(CHAR(paramPtr%name),tmpsnka42)
    bool=SIZE(tmpsnka41,DIM=1) == SIZE(tmpsnka42,DIM=1)
    ASSERT(bool, 'SIZE DIM=1 of '//prefix//CHAR(thisParam%name))
    FINFO() SIZE(tmpsnka41,DIM=1), SIZE(tmpsnka42,DIM=1)
    IF(bool) THEN
      bool=SIZE(tmpsnka41,DIM=2) == SIZE(tmpsnka42,DIM=2)
      ASSERT(bool, 'SIZE DIM=2 of '//prefix//CHAR(thisParam%name))
      FINFO() SIZE(tmpsnka41,DIM=2), SIZE(tmpsnka42,DIM=2)
      IF(bool) THEN
        bool=SIZE(tmpsnka41,DIM=3) == SIZE(tmpsnka42,DIM=3)
        ASSERT(bool, 'SIZE DIM=3 of '//prefix//CHAR(thisParam%name))
        FINFO() SIZE(tmpsnka41,DIM=3), SIZE(tmpsnka42,DIM=3)
        IF(bool) THEN
          bool=SIZE(tmpsnka41,DIM=4) == SIZE(tmpsnka42,DIM=4)
          ASSERT(bool, 'SIZE DIM=4 of '//PREFIX//char(thisParam%name))
          FINFO() SIZE(tmpsnka41,DIM=4),SIZE(tmpsnka42,DIM=4)
          IF(bool) THEN
            bool=ALL(tmpsnka41 == tmpsnka42)
            ASSERT(bool, prefix//CHAR(thisParam%name))
            FINFO() 'test values=',tmpsnka41
            FINFO() 'ref. values=',tmpsnka42
          ENDIF
        ENDIF
      ENDIF
    ENDIF
    DEALLOCATE(tmpsnka41); DEALLOCATE(tmpsnka42)
  TYPE IS(ParamType_SLK_a4)
    CALL thisParam%get(CHAR(thisParam%name),tmpslka41)
    CALL paramPtr%get(CHAR(paramPtr%name),tmpslka42)
    bool=SIZE(tmpslka41,DIM=1) == SIZE(tmpslka42,DIM=1)
    ASSERT(bool, 'SIZE DIM=1 of '//prefix//CHAR(thisParam%name))
    FINFO() SIZE(tmpslka41,DIM=1), SIZE(tmpslka42,DIM=1)
    IF(bool) THEN
      bool=SIZE(tmpslka41,DIM=2) == SIZE(tmpslka42,DIM=2)
      ASSERT(bool, 'SIZE DIM=2 of '//prefix//CHAR(thisParam%name))
      FINFO() SIZE(tmpslka41,DIM=2), SIZE(tmpslka42,DIM=2)
      IF(bool) THEN
        bool=SIZE(tmpslka41,DIM=3) == SIZE(tmpslka42,DIM=3)
        ASSERT(bool, 'SIZE DIM=3 of '//prefix//CHAR(thisParam%name))
        FINFO() SIZE(tmpslka41,DIM=3), SIZE(tmpslka42,DIM=3)
        IF(bool) THEN
          bool=SIZE(tmpslka41,DIM=4) == SIZE(tmpslka42,DIM=4)
          ASSERT(bool, 'SIZE DIM=4 of '//PREFIX//char(thisParam%name))
          FINFO() SIZE(tmpslka41,DIM=4),SIZE(tmpslka42,DIM=4)
          IF(bool) THEN
            bool=ALL(tmpslka41 == tmpslka42)
            ASSERT(bool, prefix//CHAR(thisParam%name))
            FINFO() 'test values=',tmpslka41
            FINFO() 'ref. values=',tmpslka42
          ENDIF
        ENDIF
      ENDIF
    ENDIF
    DEALLOCATE(tmpslka41); DEALLOCATE(tmpslka42)
  TYPE IS(ParamType_List)
    bool=SAME_TYPE_AS(thisParam,paramPtr)
    ASSERT(bool,'ParamType_List for'//prefix//CHAR(thisParam%name))
    FINFO() 'test value is ParamType_List, while ref value is not.'
  CLASS DEFAULT
    CONTINUE
  ENDSELECT
ENDFUNCTION matchTest_ParamType
!
!-------------------------------------------------------------------------------
!> @brief This function checks the values of thisParam and thatParam and returns
!>        if they are equal or approximately equal.
!> @param thisParam  The parameter list being validated
!> @param thatParam  The parameter list being checked against
!> @param bool The logical result of the checked parameters.
!>
!> The assumptions of this routine are that the parameters passed in are the
!> same extended ParamType.  It also assumes that there is a "gettable" value
!> that is of thisParam%name on the ParamType.  This function determines
!> the extended type, then "gets" the appropriate parameter from both lists,
!> then checks their equivalence.  If they are equal or approximately equal,
!> the function results in true.  If not, false.  An error is reported if the
!> comparison fails.
!>
FUNCTION matchList_ParamType(thisParam,thatParam,prefix,e) RESULT(bool)
  CHARACTER(LEN=*),PARAMETER :: myName='matchList_ParamType'
  CLASS(ParamType),INTENT(INOUT) :: thisParam
  CLASS(ParamType),INTENT(IN),TARGET :: thatParam
  CHARACTER(LEN=*),INTENT(IN) :: prefix
  CLASS(ExceptionHandlerType),INTENT(INOUT) :: e
  LOGICAL(SBK) :: bool
  TYPE(StringType) :: errmesstt,errmess,errmesstp
  CLASS(ParamType),POINTER :: paramPtr
  INTEGER(SIK) :: i,j
  LOGICAL(SBK) :: tmpsbk1,tmpsbk2
  LOGICAL(SBK),ALLOCATABLE :: tmpsbka11(:),tmpsbka12(:)
  REAL(SSK) :: tmpssk1,tmpssk2
  REAL(SSK),ALLOCATABLE :: tmpsska11(:),tmpsska21(:,:),tmpsska31(:,:,:),tmpsska41(:,:,:,:)
  REAL(SSK),ALLOCATABLE :: tmpsska12(:),tmpsska22(:,:),tmpsska32(:,:,:),tmpsska42(:,:,:,:)
  REAL(SDK) :: tmpsdk1,tmpsdk2
  REAL(SDK),ALLOCATABLE :: tmpsdka11(:),tmpsdka21(:,:),tmpsdka31(:,:,:),tmpsdka41(:,:,:,:)
  REAL(SDK),ALLOCATABLE :: tmpsdka12(:),tmpsdka22(:,:),tmpsdka32(:,:,:),tmpsdka42(:,:,:,:)
  INTEGER(SNK) :: tmpsnk1,tmpsnk2
  INTEGER(SNK),ALLOCATABLE :: tmpsnka11(:),tmpsnka21(:,:),tmpsnka31(:,:,:),tmpsnka41(:,:,:,:)
  INTEGER(SNK),ALLOCATABLE :: tmpsnka12(:),tmpsnka22(:,:),tmpsnka32(:,:,:),tmpsnka42(:,:,:,:)
  INTEGER(SLK) :: tmpslk1,tmpslk2
  INTEGER(SLK),ALLOCATABLE :: tmpslka11(:),tmpslka21(:,:),tmpslka31(:,:,:),tmpslka41(:,:,:,:)
  INTEGER(SLK),ALLOCATABLE :: tmpslka12(:),tmpslka22(:,:),tmpslka32(:,:,:),tmpslka42(:,:,:,:)
  TYPE(StringType) :: tmpstr1,tmpstr2
  TYPE(StringType),ALLOCATABLE :: tmpstra11(:),tmpstra21(:,:)
  TYPE(StringType),ALLOCATABLE :: tmpstra12(:),tmpstra22(:,:)

  !Point to the intent(in) param to use the get function
  paramPtr => NULL()
  bool=.FALSE.
  !Find the extended parameter type, then use the appropriate variable
  !and "get" the data to check.
  errmesstt=' - The values'
  errmess=' of the two parameter lists with parameter path "'//prefix//thisParam%name//'"'
  errmesstp=' are not equal!'
  SELECTTYPE(paramPtr => thatParam)
  TYPE IS(ParamType_SSK)
    CALL thisParam%get(CHAR(thisParam%name),tmpssk1)
    CALL paramPtr%get(CHAR(paramPtr%name),tmpssk2)
    bool=(tmpssk1 .APPROXEQ. tmpssk2)
  TYPE IS(ParamType_SDK)
    CALL thisParam%get(CHAR(thisParam%name),tmpsdk1)
    CALL paramPtr%get(CHAR(paramPtr%name),tmpsdk2)
    bool=(tmpsdk1 .APPROXEQ. tmpsdk2)
    IF(.NOT.bool) bool=SOFTEQ(tmpsdk1,tmpsdk2,EPSD*10._SRK)
  TYPE IS(ParamType_SNK)
    CALL thisParam%get(CHAR(thisParam%name),tmpsnk1)
    CALL paramPtr%get(CHAR(paramPtr%name),tmpsnk2)
    bool=(tmpsnk1 == tmpsnk2)
  TYPE IS(ParamType_SLK)
    CALL thisParam%get(CHAR(thisParam%name),tmpslk1)
    CALL paramPtr%get(CHAR(paramPtr%name),tmpslk2)
    bool=(tmpslk1 == tmpslk2)
  TYPE IS(ParamType_SBK)
    CALL thisParam%get(CHAR(thisParam%name),tmpsbk1)
    CALL paramPtr%get(CHAR(paramPtr%name),tmpsbk2)
    bool=(tmpsbk1 .EQV. tmpsbk2)
  TYPE IS(ParamType_STR)
    CALL thisParam%get(CHAR(thisParam%name),tmpstr1)
    CALL paramPtr%get(CHAR(paramPtr%name),tmpstr2)
    bool=(tmpstr1 == tmpstr2)
  TYPE IS(ParamType_SSK_a1)
    CALL thisParam%get(CHAR(thisParam%name),tmpsska11)
    CALL paramPtr%get(CHAR(paramPtr%name),tmpsska12)
    bool=SIZE(tmpsska11,DIM=1) == SIZE(tmpsska12,DIM=1)
    IF(bool) THEN
      bool=ALL(tmpsska11 .APPROXEQ. tmpsska12)
    ELSE
      errmesstt=' - Dimension 1'
    ENDIF
    DEALLOCATE(tmpsska11); DEALLOCATE(tmpsska12)
  TYPE IS(ParamType_SDK_a1)
    CALL thisParam%get(CHAR(thisParam%name),tmpsdka11)
    CALL paramPtr%get(CHAR(paramPtr%name),tmpsdka12)
    bool=SIZE(tmpsdka11,DIM=1) == SIZE(tmpsdka12,DIM=1)
    IF(bool) THEN
      bool=ALL(tmpsdka11 .APPROXEQ. tmpsdka12)
      IF(.NOT.bool) bool=ALL(SOFTEQ(tmpsdka11,tmpsdka12,EPSD*1000._SRK))
    ELSE
      errmesstt=' - Dimension 1'
    ENDIF
    DEALLOCATE(tmpsdka11); DEALLOCATE(tmpsdka12)
  TYPE IS(ParamType_SNK_a1)
    CALL thisParam%get(CHAR(thisParam%name),tmpsnka11)
    CALL paramPtr%get(CHAR(paramPtr%name),tmpsnka12)
    bool=SIZE(tmpsnka11,DIM=1) == SIZE(tmpsnka12,DIM=1)
    IF(bool) THEN
      bool=ALL(tmpsnka11 == tmpsnka12)
    ELSE
      errmesstt=' - Dimension 1'
    ENDIF
    DEALLOCATE(tmpsnka11); DEALLOCATE(tmpsnka12)
  TYPE IS(ParamType_SLK_a1)
    CALL thisParam%get(CHAR(thisParam%name),tmpslka11)
    CALL paramPtr%get(CHAR(paramPtr%name),tmpslka12)
    bool=SIZE(tmpslka11,DIM=1) == SIZE(tmpslka12,DIM=1)
    IF(bool) THEN
      bool=ALL(tmpslka11 == tmpslka12)
    ELSE
      errmesstt=' - Dimension 1'
    ENDIF
    DEALLOCATE(tmpslka11); DEALLOCATE(tmpslka12)
  TYPE IS(ParamType_SBK_a1)
    CALL thisParam%get(CHAR(thisParam%name),tmpsbka11)
    CALL paramPtr%get(CHAR(paramPtr%name),tmpsbka12)
    bool=SIZE(tmpsbka11,DIM=1) == SIZE(tmpsbka12,DIM=1)
    IF(bool) THEN
      bool=ALL(tmpsbka11 .EQV. tmpsbka12)
    ELSE
      errmesstt=' - Dimension 1'
    ENDIF
    DEALLOCATE(tmpsbka11); DEALLOCATE(tmpsbka12)
  TYPE IS(ParamType_STR_a1)
    CALL thisParam%get(CHAR(thisParam%name),tmpstra11)
    CALL paramPtr%get(CHAR(paramPtr%name),tmpstra12)
    bool=SIZE(tmpstra11,DIM=1) == SIZE(tmpstra12,DIM=1)
    IF(bool) THEN
      DO i=1,SIZE(tmpstra11)
        bool=tmpstra11(i) == tmpstra12(i)
        IF(.NOT. bool) EXIT
      ENDDO
      !clear?
    ELSE
      errmesstt=' - Dimension 1'
    ENDIF
    DEALLOCATE(tmpstra11); DEALLOCATE(tmpstra12)
  TYPE IS(ParamType_SSK_a2)
    CALL thisParam%get(CHAR(thisParam%name),tmpsska21)
    CALL paramPtr%get(CHAR(paramPtr%name),tmpsska22)
    bool=SIZE(tmpsska21,DIM=1) == SIZE(tmpsska22,DIM=1)
    IF(bool) THEN
      bool=SIZE(tmpsska21,DIM=2) == SIZE(tmpsska22,DIM=2)
      IF(bool) THEN
        bool=ALL(tmpsska21 .APPROXEQ. tmpsska22)
      ELSE
        errmesstt=' - Dimension 2'
      ENDIF
    ELSE
      errmesstt=' - Dimension 1'
    ENDIF
    DEALLOCATE(tmpsska21); DEALLOCATE(tmpsska22)
  TYPE IS(ParamType_SDK_a2)
    CALL thisParam%get(CHAR(thisParam%name),tmpsdka21)
    CALL paramPtr%get(CHAR(paramPtr%name),tmpsdka22)
    bool=SIZE(tmpsdka21,DIM=1) == SIZE(tmpsdka22,DIM=1)
    IF(bool) THEN
      bool=SIZE(tmpsdka21,DIM=2) == SIZE(tmpsdka22,DIM=2)
      IF(bool) THEN
        bool=ALL(tmpsdka21 .APPROXEQ. tmpsdka22)
      ELSE
        errmesstt=' - Dimension 2'
      ENDIF
    ELSE
      errmesstt=' - Dimension 1'
    ENDIF
    DEALLOCATE(tmpsdka21); DEALLOCATE(tmpsdka22)
  TYPE IS(ParamType_SNK_a2)
    CALL thisParam%get(CHAR(thisParam%name),tmpsnka21)
    CALL paramPtr%get(CHAR(paramPtr%name),tmpsnka22)
    bool=SIZE(tmpsnka21,DIM=1) == SIZE(tmpsnka22,DIM=1)
    IF(bool) THEN
      bool=SIZE(tmpsnka21,DIM=2) == SIZE(tmpsnka22,DIM=2)
      IF(bool) THEN
        bool=ALL(tmpsnka21 == tmpsnka22)
      ELSE
        errmesstt=' - Dimension 2'
      ENDIF
    ELSE
      errmesstt=' - Dimension 1'
    ENDIF
    DEALLOCATE(tmpsnka21); DEALLOCATE(tmpsnka22)
  TYPE IS(ParamType_SLK_a2)
    CALL thisParam%get(CHAR(thisParam%name),tmpslka21)
    CALL paramPtr%get(CHAR(paramPtr%name),tmpslka22)
    bool=SIZE(tmpslka21,DIM=1) == SIZE(tmpslka22,DIM=1)
    IF(bool) THEN
      bool=SIZE(tmpslka21,DIM=2) == SIZE(tmpslka22,DIM=2)
      IF(bool) THEN
        bool=ALL(tmpslka21 == tmpslka22)
      ELSE
        errmesstt=' - Dimension 2'
      ENDIF
    ELSE
      errmesstt=' - Dimension 1'
    ENDIF
    DEALLOCATE(tmpslka21); DEALLOCATE(tmpslka22)
  TYPE IS(ParamType_STR_a2)
    CALL thisParam%get(CHAR(thisParam%name),tmpstra21)
    CALL paramPtr%get(CHAR(paramPtr%name),tmpstra22)
    bool=SIZE(tmpstra21,DIM=1) == SIZE(tmpstra22,DIM=1)
    IF(bool) THEN
      bool=SIZE(tmpstra21,DIM=2) == SIZE(tmpstra22,DIM=2)
      IF(bool) THEN
        outer : DO j=1,SIZE(tmpstra21,DIM=2)
          DO i=1,SIZE(tmpstra21,DIM=1)
            bool=tmpstra21(i,j) == tmpstra22(i,j)
            IF(.NOT.bool) EXIT outer
          ENDDO
        ENDDO outer
      ELSE
        errmesstt=' - Dimension 2'
      ENDIF
    ELSE
      errmesstt=' - Dimension 1'
    ENDIF
    !clear?
    DEALLOCATE(tmpstra21); DEALLOCATE(tmpstra22)
  TYPE IS(ParamType_SSK_a3)
    CALL thisParam%get(CHAR(thisParam%name),tmpsska31)
    CALL paramPtr%get(CHAR(paramPtr%name),tmpsska32)
    bool=SIZE(tmpsska31,DIM=1) == SIZE(tmpsska32,DIM=1)
    IF(bool) THEN
      bool=SIZE(tmpsska31,DIM=2) == SIZE(tmpsska32,DIM=2)
      IF(bool) THEN
        bool=SIZE(tmpsska31,DIM=3) == SIZE(tmpsska32,DIM=3)
        IF(bool) THEN
          bool=ALL(tmpsska31 .APPROXEQ. tmpsska32)
        ELSE
          errmesstt=' - Dimension 3'
        ENDIF
      ELSE
        errmesstt=' - Dimension 2'
      ENDIF
    ELSE
      errmesstt=' - Dimension 1'
    ENDIF
    DEALLOCATE(tmpsska31); DEALLOCATE(tmpsska32)
  TYPE IS(ParamType_SDK_a3)
    CALL thisParam%get(CHAR(thisParam%name),tmpsdka31)
    CALL paramPtr%get(CHAR(paramPtr%name),tmpsdka32)
    bool=SIZE(tmpsdka31,DIM=1) == SIZE(tmpsdka32,DIM=1)
    IF(bool) THEN
      bool=SIZE(tmpsdka31,DIM=2) == SIZE(tmpsdka32,DIM=2)
      IF(bool) THEN
        bool=SIZE(tmpsdka31,DIM=3) == SIZE(tmpsdka32,DIM=3)
        IF(bool) THEN
          bool=ALL(tmpsdka31 .APPROXEQ. tmpsdka32)
        ELSE
          errmesstt=' - Dimension 3'
        ENDIF
      ELSE
        errmesstt=' - Dimension 2'
      ENDIF
    ELSE
      errmesstt=' - Dimension 1'
    ENDIF
    DEALLOCATE(tmpsdka31); DEALLOCATE(tmpsdka32)
  TYPE IS(ParamType_SNK_a3)
    CALL thisParam%get(CHAR(thisParam%name),tmpsnka31)
    CALL paramPtr%get(CHAR(paramPtr%name),tmpsnka32)
    bool=SIZE(tmpsnka31,DIM=1) == SIZE(tmpsnka32,DIM=1)
    IF(bool) THEN
      bool=SIZE(tmpsnka31,DIM=2) == SIZE(tmpsnka32,DIM=2)
      IF(bool) THEN
        bool=SIZE(tmpsnka31,DIM=3) == SIZE(tmpsnka32,DIM=3)
        IF(bool) THEN
          bool=ALL(tmpsnka31 == tmpsnka32)
        ELSE
          errmesstt=' - Dimension 3'
        ENDIF
      ELSE
        errmesstt=' - Dimension 2'
      ENDIF
    ELSE
      errmesstt=' - Dimension 1'
    ENDIF
    DEALLOCATE(tmpsnka31); DEALLOCATE(tmpsnka32)
  TYPE IS(ParamType_SLK_a3)
    CALL thisParam%get(CHAR(thisParam%name),tmpslka31)
    CALL paramPtr%get(CHAR(paramPtr%name),tmpslka32)
    bool=SIZE(tmpslka31,DIM=1) == SIZE(tmpslka32,DIM=1)
    IF(bool) THEN
      bool=SIZE(tmpslka31,DIM=2) == SIZE(tmpslka32,DIM=2)
      IF(bool) THEN
        bool=SIZE(tmpslka31,DIM=3) == SIZE(tmpslka32,DIM=3)
        IF(bool) THEN
          bool=ALL(tmpslka31 == tmpslka32)
        ELSE
          errmesstt=' - Dimension 3'
        ENDIF
      ELSE
        errmesstt=' - Dimension 2'
      ENDIF
    ELSE
      errmesstt=' - Dimension 1'
    ENDIF
    DEALLOCATE(tmpslka31); DEALLOCATE(tmpslka32)
  TYPE IS(ParamType_SSK_a4)
    CALL thisParam%get(CHAR(thisParam%name),tmpsska41)
    CALL paramPtr%get(CHAR(paramPtr%name),tmpsska42)
    bool=SIZE(tmpsska41,DIM=1) == SIZE(tmpsska42,DIM=1)
    IF(bool) THEN
      bool=SIZE(tmpsska41,DIM=2) == SIZE(tmpsska42,DIM=2)
      IF(bool) THEN
        bool=SIZE(tmpsska41,DIM=3) == SIZE(tmpsska42,DIM=3)
        IF(bool) THEN
          bool=SIZE(tmpsska41,DIM=4) == SIZE(tmpsska42,DIM=4)
          IF(bool) THEN
            bool=ALL(tmpsska41 == tmpsska42)
          ELSE
            errmesstt=' - Dimension 4'
          ENDIF
        ELSE
          errmesstt=' - Dimension 3'
        ENDIF
      ELSE
        errmesstt=' - Dimension 2'
      ENDIF
    ELSE
      errmesstt=' - Dimension 1'
    ENDIF
    DEALLOCATE(tmpsska41); DEALLOCATE(tmpsska42)
  TYPE IS(ParamType_SDK_a4)
    CALL thisParam%get(CHAR(thisParam%name),tmpsdka41)
    CALL paramPtr%get(CHAR(paramPtr%name),tmpsdka42)
    bool=SIZE(tmpsdka41,DIM=1) == SIZE(tmpsdka42,DIM=1)
    IF(bool) THEN
      bool=SIZE(tmpsdka41,DIM=2) == SIZE(tmpsdka42,DIM=2)
      IF(bool) THEN
        bool=SIZE(tmpsdka41,DIM=3) == SIZE(tmpsdka42,DIM=3)
        IF(bool) THEN
          bool=SIZE(tmpsdka41,DIM=4) == SIZE(tmpsdka42,DIM=4)
          IF(bool) THEN
            bool=ALL(tmpsdka41 == tmpsdka42)
          ELSE
            errmesstt=' - Dimension 4'
          ENDIF
        ELSE
          errmesstt=' - Dimension 3'
        ENDIF
      ELSE
        errmesstt=' - Dimension 2'
      ENDIF
    ELSE
      errmesstt=' - Dimension 1'
    ENDIF
    DEALLOCATE(tmpsdka41); DEALLOCATE(tmpsdka42)
  TYPE IS(ParamType_SNK_a4)
    CALL thisParam%get(CHAR(thisParam%name),tmpsnka41)
    CALL paramPtr%get(CHAR(paramPtr%name),tmpsnka42)
    bool=SIZE(tmpsnka41,DIM=1) == SIZE(tmpsnka42,DIM=1)
    IF(bool) THEN
      bool=SIZE(tmpsnka41,DIM=2) == SIZE(tmpsnka42,DIM=2)
      IF(bool) THEN
        bool=SIZE(tmpsnka41,DIM=3) == SIZE(tmpsnka42,DIM=3)
        IF(bool) THEN
          bool=SIZE(tmpsnka41,DIM=4) == SIZE(tmpsnka42,DIM=4)
          IF(bool) THEN
            bool=ALL(tmpsnka41 == tmpsnka42)
          ELSE
            errmesstt=' - Dimension 4'
          ENDIF
        ELSE
          errmesstt=' - Dimension 3'
        ENDIF
      ELSE
        errmesstt=' - Dimension 2'
      ENDIF
    ELSE
      errmesstt=' - Dimension 1'
    ENDIF
    DEALLOCATE(tmpsnka41); DEALLOCATE(tmpsnka42)
  TYPE IS(ParamType_SLK_a4)
    CALL thisParam%get(CHAR(thisParam%name),tmpslka41)
    CALL paramPtr%get(CHAR(paramPtr%name),tmpslka42)
    bool=SIZE(tmpslka41,DIM=1) == SIZE(tmpslka42,DIM=1)
    IF(bool) THEN
      bool=SIZE(tmpslka41,DIM=2) == SIZE(tmpslka42,DIM=2)
      IF(bool) THEN
        bool=SIZE(tmpslka41,DIM=3) == SIZE(tmpslka42,DIM=3)
        IF(bool) THEN
          bool=SIZE(tmpslka41,DIM=4) == SIZE(tmpslka42,DIM=4)
          IF(bool) THEN
            bool=ALL(tmpslka41 == tmpslka42)
          ELSE
            errmesstt=' - Dimension 4'
          ENDIF
        ELSE
          errmesstt=' - Dimension 3'
        ENDIF
      ELSE
        errmesstt=' - Dimension 2'
      ENDIF
    ELSE
      errmesstt=' - Dimension 1'
    ENDIF
    DEALLOCATE(tmpslka41); DEALLOCATE(tmpslka42)
  TYPE IS(ParamType_List)
    bool=SAME_TYPE_AS(thisParam,paramPtr)
    errmesstt=' - The parameters'
    errmesstp=' are not the same type!'
  CLASS DEFAULT
    CONTINUE
  ENDSELECT
  !Error message.
  IF(.NOT. bool) CALL e%raiseError(modName//'::'//myName// &
      errmesstt//errmess//errmesstp)
ENDFUNCTION matchList_ParamType
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
RECURSIVE SUBROUTINE init_ParamType_List(thisParam,name,param,description)
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
RECURSIVE SUBROUTINE edit_ParamType_List(thisParam,funit,indent,prefix,paddtw)
  CLASS(ParamType_List),INTENT(IN) :: thisParam
  INTEGER(SIK),INTENT(IN) :: funit
  INTEGER(SIK),INTENT(IN),OPTIONAL :: indent
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: prefix
  LOGICAL(SBK),INTENT(IN),OPTIONAL :: paddtw
  CHARACTER(LEN=12) :: fmt
  CHARACTER(LEN=:),ALLOCATABLE :: dtype
  INTEGER(SIK) :: i,j
  TYPE(StringType) :: sprefix,sdtype


  IF(LEN_TRIM(thisParam%name) > 0) THEN
    IF(PRESENT(prefix)) sprefix=prefix
    sdtype=thisParam%datatype
    IF(PRESENT(paddtw)) THEN
      IF(paddtw) THEN
        ALLOCATE(CHARACTER(PARAM_MAX_DAT_LEN) :: dtype)
        dtype=CHAR(thisParam%dataType)
        sdtype=dtype
      ENDIF
    ENDIF
    i=1
    IF(PRESENT(indent)) i=i+indent
    WRITE(fmt,'(i12)') i; fmt=ADJUSTL(fmt)
    IF(LEN_TRIM(thisParam%description) == 0) THEN
      WRITE(UNIT=funit,FMT='('//TRIM(fmt)//'x,a)') sprefix// &
          sdtype//' :: '//thisParam%name//'='
    ELSE
      WRITE(UNIT=funit,FMT='('//TRIM(fmt)//'x,a)') sprefix// &
          sdtype//' :: '//thisParam%name//'= !'// &
          thisParam%description
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
RECURSIVE SUBROUTINE clear_ParamType_List(thisParam)
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
SUBROUTINE edit_ParamType_SSK(thisParam,funit,indent,prefix,paddtw)
  CLASS(ParamType_SSK),INTENT(IN) :: thisParam
  INTEGER(SIK),INTENT(IN) :: funit
  INTEGER(SIK),INTENT(IN),OPTIONAL :: indent
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: prefix
  LOGICAL(SBK),INTENT(IN),OPTIONAL :: paddtw
  CHARACTER(LEN=12) :: fmt
  CHARACTER(LEN=:),ALLOCATABLE :: dtype
  INTEGER(SIK) :: i
  TYPE(StringType) :: sprefix,sdtype

  i=1
  IF(PRESENT(indent)) i=i+indent
  IF(PRESENT(prefix)) sprefix=prefix
  sdtype=thisParam%datatype
  IF(PRESENT(paddtw)) THEN
    IF(paddtw) THEN
      ALLOCATE(CHARACTER(PARAM_MAX_DAT_LEN) :: dtype)
      dtype=CHAR(thisParam%dataType)
      sdtype=dtype
    ENDIF
  ENDIF
  WRITE(fmt,'(i12)') i; fmt=ADJUSTL(fmt)
  IF(LEN_TRIM(thisParam%description) == 0) THEN
    WRITE(UNIT=funit,FMT='('//TRIM(fmt)//'x,a,g13.7)') sprefix// &
        sdtype//' :: '//thisParam%name//'=',thisParam%val
  ELSE
    WRITE(UNIT=funit,FMT='('//TRIM(fmt)//'x,a,g13.7,a)') sprefix// &
        sdtype//' :: '//thisParam%name//'=',thisParam%val, &
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
SUBROUTINE edit_ParamType_SDK(thisParam,funit,indent,prefix,paddtw)
  CLASS(ParamType_SDK),INTENT(IN) :: thisParam
  INTEGER(SIK),INTENT(IN) :: funit
  INTEGER(SIK),INTENT(IN),OPTIONAL :: indent
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: prefix
  LOGICAL(SBK),INTENT(IN),OPTIONAL :: paddtw
  CHARACTER(LEN=12) :: fmt
  CHARACTER(LEN=:),ALLOCATABLE :: dtype
  INTEGER(SIK) :: i
  TYPE(StringType) :: sprefix,sdtype

  i=1
  IF(PRESENT(indent)) i=i+indent
  IF(PRESENT(prefix)) sprefix=prefix
  sdtype=thisParam%datatype
  IF(PRESENT(paddtw)) THEN
    IF(paddtw) THEN
      ALLOCATE(CHARACTER(PARAM_MAX_DAT_LEN) :: dtype)
      dtype=CHAR(thisParam%dataType)
      sdtype=dtype
    ENDIF
  ENDIF
  WRITE(fmt,'(i12)') i; fmt=ADJUSTL(fmt)
  IF(LEN_TRIM(thisParam%description) == 0) THEN
    WRITE(UNIT=funit,FMT='('//TRIM(fmt)//'x,a,g23.16)') sprefix// &
        sdtype//' :: '//thisParam%name//'=',thisParam%val
  ELSE
    WRITE(UNIT=funit,FMT='('//TRIM(fmt)//'x,a,g23.16,a)') sprefix// &
        sdtype//' :: '//thisParam%name//'=',thisParam%val, &
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
SUBROUTINE edit_ParamType_SNK(thisParam,funit,indent,prefix,paddtw)
  CLASS(ParamType_SNK),INTENT(IN) :: thisParam
  INTEGER(SIK),INTENT(IN) :: funit
  INTEGER(SIK),INTENT(IN),OPTIONAL :: indent
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: prefix
  LOGICAL(SBK),INTENT(IN),OPTIONAL :: paddtw
  CHARACTER(LEN=12) :: fmt
  CHARACTER(LEN=:),ALLOCATABLE :: dtype
  INTEGER(SIK) :: i
  TYPE(StringType) :: sprefix,sdtype

  i=1
  IF(PRESENT(indent)) i=i+indent
  IF(PRESENT(prefix)) sprefix=prefix
  sdtype=thisParam%datatype
  IF(PRESENT(paddtw)) THEN
    IF(paddtw) THEN
      ALLOCATE(CHARACTER(PARAM_MAX_DAT_LEN) :: dtype)
      dtype=CHAR(thisParam%dataType)
      sdtype=dtype
    ENDIF
  ENDIF
  WRITE(fmt,'(i12)') i; fmt=ADJUSTL(fmt)
  IF(LEN_TRIM(thisParam%description) == 0) THEN
    WRITE(UNIT=funit,FMT='('//TRIM(fmt)//'x,a,g13.7)') sprefix// &
        sdtype//' :: '//thisParam%name//'=',thisParam%val
  ELSE
    WRITE(UNIT=funit,FMT='('//TRIM(fmt)//'x,a,g13.7,a)') sprefix// &
        sdtype//' :: '//thisParam%name//'=',thisParam%val, &
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
SUBROUTINE edit_ParamType_SLK(thisParam,funit,indent,prefix,paddtw)
  CLASS(ParamType_SLK),INTENT(IN) :: thisParam
  INTEGER(SIK),INTENT(IN) :: funit
  INTEGER(SIK),INTENT(IN),OPTIONAL :: indent
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: prefix
  LOGICAL(SBK),INTENT(IN),OPTIONAL :: paddtw
  CHARACTER(LEN=:),ALLOCATABLE :: dtype
  CHARACTER(LEN=12) :: fmt
  INTEGER(SIK) :: i
  TYPE(StringType) :: sprefix,sdtype

  i=1
  IF(PRESENT(indent)) i=i+indent
  IF(PRESENT(prefix)) sprefix=prefix
  sdtype=thisParam%datatype
  IF(PRESENT(paddtw)) THEN
    IF(paddtw) THEN
      ALLOCATE(CHARACTER(PARAM_MAX_DAT_LEN) :: dtype)
      dtype=CHAR(thisParam%dataType)
      sdtype=dtype
    ENDIF
  ENDIF
  WRITE(fmt,'(i12)') i; fmt=ADJUSTL(fmt)
  IF(LEN_TRIM(thisParam%description) == 0) THEN
    WRITE(UNIT=funit,FMT='('//TRIM(fmt)//'x,a,g13.7)') sprefix// &
        sdtype//' :: '//thisParam%name//'=',thisParam%val
  ELSE
    WRITE(UNIT=funit,FMT='('//TRIM(fmt)//'x,a,g13.7,a)') sprefix// &
        sdtype//' :: '//thisParam%name//'=',thisParam%val, &
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
SUBROUTINE edit_ParamType_SBK(thisParam,funit,indent,prefix,paddtw)
  CLASS(ParamType_SBK),INTENT(IN) :: thisParam
  INTEGER(SIK),INTENT(IN) :: funit
  INTEGER(SIK),INTENT(IN),OPTIONAL :: indent
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: prefix
  LOGICAL(SBK),INTENT(IN),OPTIONAL :: paddtw
  CHARACTER(LEN=:),ALLOCATABLE :: dtype
  CHARACTER(LEN=12) :: fmt
  INTEGER(SIK) :: i
  TYPE(StringType) :: sprefix,sdtype

  i=1
  IF(PRESENT(indent)) i=i+indent
  IF(PRESENT(prefix)) sprefix=prefix
  sdtype=thisParam%datatype
  IF(PRESENT(paddtw)) THEN
    IF(paddtw) THEN
      ALLOCATE(CHARACTER(PARAM_MAX_DAT_LEN) :: dtype)
      dtype=CHAR(thisParam%dataType)
      sdtype=dtype
    ENDIF
  ENDIF
  WRITE(fmt,'(i12)') i; fmt=ADJUSTL(fmt)
  IF(LEN_TRIM(thisParam%description) == 0) THEN
    WRITE(UNIT=funit,FMT='('//TRIM(fmt)//'x,a,l2)') sprefix// &
        sdtype//' :: '//thisParam%name//'=',thisParam%val
  ELSE
    WRITE(UNIT=funit,FMT='('//TRIM(fmt)//'x,a,l2,a)') sprefix// &
        sdtype//' :: '//thisParam%name//'=',thisParam%val, &
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
SUBROUTINE edit_ParamType_STR(thisParam,funit,indent,prefix,paddtw)
  CLASS(ParamType_STR),INTENT(IN) :: thisParam
  INTEGER(SIK),INTENT(IN) :: funit
  INTEGER(SIK),INTENT(IN),OPTIONAL :: indent
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: prefix
  LOGICAL(SBK),INTENT(IN),OPTIONAL :: paddtw
  CHARACTER(LEN=:),ALLOCATABLE :: dtype
  CHARACTER(LEN=12) :: fmt
  INTEGER(SIK) :: i
  TYPE(StringType) :: sprefix,sdtype,sval,sdesc

  i=1
  IF(PRESENT(indent)) i=i+indent
  sprefix=''
  IF(PRESENT(prefix)) sprefix=prefix
  sdtype=thisParam%datatype
  IF(PRESENT(paddtw)) THEN
    IF(paddtw) THEN
      ALLOCATE(CHARACTER(PARAM_MAX_DAT_LEN) :: dtype)
      dtype=CHAR(thisParam%dataType)
      sdtype=dtype
    ENDIF
  ENDIF
  WRITE(fmt,'(i12)') i; fmt=ADJUSTL(fmt)
  sval=''
  IF(LEN_TRIM(thisParam%val) > 0) sval=thisParam%val
  sdesc=''
  IF(LEN_TRIM(thisParam%description) > 0) sdesc=' !'//thisParam%description
  WRITE(UNIT=funit,FMT='('//TRIM(fmt)//'x,a)') sprefix// &
      sdtype//' :: '//thisParam%name//'='//sval//sdesc
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
  CLASS(ParamType),INTENT(IN) :: thisParam
  CHARACTER(LEN=*),INTENT(IN) :: name
  CHARACTER(LEN=:),ALLOCATABLE,INTENT(INOUT) :: val
  TYPE(StringType) :: s

  CALL get_ParamType_STR(thisParam,name,s)
  val=CHAR(s)
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
SUBROUTINE edit_ParamType_SSK_a1(thisParam,funit,indent,prefix,paddtw)
  CLASS(ParamType_SSK_a1),INTENT(IN) :: thisParam
  INTEGER(SIK),INTENT(IN) :: funit
  INTEGER(SIK),INTENT(IN),OPTIONAL :: indent
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: prefix
  LOGICAL(SBK),INTENT(IN),OPTIONAL :: paddtw
  CHARACTER(LEN=12) :: fmt,fmt2
  CHARACTER(LEN=:), ALLOCATABLE :: dtype
  INTEGER(SIK) :: i,j,k
  TYPE(StringType) :: sprefix,sdtype

  i=1
  j=5
  IF(PRESENT(indent)) i=i+indent
  IF(PRESENT(prefix)) sprefix=prefix
  sdtype=thisParam%datatype
  IF(PRESENT(paddtw)) THEN
    IF(paddtw) THEN
      ALLOCATE(CHARACTER(PARAM_MAX_DAT_LEN) :: dtype)
      dtype=CHAR(thisParam%dataType)
      sdtype=dtype
    ENDIF
  ENDIF
  j=j+LEN(sprefix)
  WRITE(fmt,'(i12)') i; fmt=ADJUSTL(fmt)
  WRITE(UNIT=funit,FMT='('//TRIM(fmt)//'x,a,g13.7)',ADVANCE='NO') sprefix// &
      sdtype//' :: '//thisParam%name//'=',thisParam%val(1)
  j=j+LEN(sdtype)+LEN(thisParam%name)
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
      TYPE IS(ParamType_SSK_a1)
        val=p%val
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
SUBROUTINE edit_ParamType_SDK_a1(thisParam,funit,indent,prefix,paddtw)
  CLASS(ParamType_SDK_a1),INTENT(IN) :: thisParam
  INTEGER(SIK),INTENT(IN) :: funit
  INTEGER(SIK),INTENT(IN),OPTIONAL :: indent
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: prefix
  LOGICAL(SBK),INTENT(IN),OPTIONAL :: paddtw
  CHARACTER(LEN=12) :: fmt,fmt2
  CHARACTER(LEN=:),ALLOCATABLE :: dtype
  INTEGER(SIK) :: i,j,k
  TYPE(StringType) :: sprefix,sdtype

  i=1
  j=5
  IF(PRESENT(indent)) i=i+indent
  IF(PRESENT(prefix)) sprefix=prefix
  sdtype=thisParam%datatype
  IF(PRESENT(paddtw)) THEN
    IF(paddtw) THEN
      ALLOCATE(CHARACTER(PARAM_MAX_DAT_LEN) :: dtype)
      dtype=CHAR(thisParam%dataType)
      sdtype=dtype
    ENDIF
  ENDIF
  j=j+LEN(sprefix)
  WRITE(fmt,'(i12)') i; fmt=ADJUSTL(fmt)
  WRITE(UNIT=funit,FMT='('//TRIM(fmt)//'x,a,g20.14)',ADVANCE='NO') sprefix// &
      sdtype//' :: '//thisParam%name//'=',thisParam%val(1)
  j=j+LEN(sdtype)+LEN(thisParam%name)
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
      TYPE IS(ParamType_SDK_a1)
        val=p%val
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
SUBROUTINE edit_ParamType_SNK_a1(thisParam,funit,indent,prefix,paddtw)
  CLASS(ParamType_SNK_a1),INTENT(IN) :: thisParam
  INTEGER(SIK),INTENT(IN) :: funit
  INTEGER(SIK),INTENT(IN),OPTIONAL :: indent
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: prefix
  LOGICAL(SBK),INTENT(IN),OPTIONAL :: paddtw
  CHARACTER(LEN=12) :: fmt,fmt2
  CHARACTER(LEN=:), ALLOCATABLE :: dtype
  INTEGER(SIK) :: i,j,k
  TYPE(StringType) :: sprefix,sdtype

  i=1
  j=5
  IF(PRESENT(indent)) i=i+indent
  IF(PRESENT(prefix)) sprefix=prefix
  sdtype=thisParam%datatype
  IF(PRESENT(paddtw)) THEN
    IF(paddtw) THEN
      ALLOCATE(CHARACTER(PARAM_MAX_DAT_LEN) :: dtype)
      dtype=CHAR(thisParam%dataType)
      sdtype=dtype
    ENDIF
  ENDIF
  j=j+LEN(sprefix)
  WRITE(fmt,'(i12)') i; fmt=ADJUSTL(fmt)
  WRITE(UNIT=funit,FMT='('//TRIM(fmt)//'x,a,g13.7)',ADVANCE='NO') sprefix// &
      sdtype//' :: '//thisParam%name//'=',thisParam%val(1)
  j=j+LEN(sdtype)+LEN(thisParam%name)
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
      TYPE IS(ParamType_SNK_a1)
        val=p%val
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
SUBROUTINE edit_ParamType_SLK_a1(thisParam,funit,indent,prefix,paddtw)
  CLASS(ParamType_SLK_a1),INTENT(IN) :: thisParam
  INTEGER(SIK),INTENT(IN) :: funit
  INTEGER(SIK),INTENT(IN),OPTIONAL :: indent
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: prefix
  LOGICAL(SBK),INTENT(IN),OPTIONAL :: paddtw
  CHARACTER(LEN=12) :: fmt,fmt2
  CHARACTER(LEN=:),ALLOCATABLE :: dtype
  INTEGER(SIK) :: i,j,k
  TYPE(StringType) :: sprefix,sdtype

  i=1
  j=5
  IF(PRESENT(indent)) i=i+indent
  IF(PRESENT(prefix)) sprefix=prefix
  sdtype=thisParam%datatype
  IF(PRESENT(paddtw)) THEN
    IF(paddtw) THEN
      ALLOCATE(CHARACTER(PARAM_MAX_DAT_LEN) :: dtype)
      dtype=CHAR(thisParam%dataType)
      sdtype=dtype
    ENDIF
  ENDIF
  j=j+LEN(sprefix)
  WRITE(fmt,'(i12)') i; fmt=ADJUSTL(fmt)
  WRITE(UNIT=funit,FMT='('//TRIM(fmt)//'x,a,g20.14)',ADVANCE='NO') sprefix// &
      sdtype//' :: '//thisParam%name//'=',thisParam%val(1)
  j=j+LEN(sdtype)+LEN(thisParam%name)
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
      TYPE IS(ParamType_SLK_a1)
        val=p%val
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
SUBROUTINE edit_ParamType_SBK_a1(thisParam,funit,indent,prefix,paddtw)
  CLASS(ParamType_SBK_a1),INTENT(IN) :: thisParam
  INTEGER(SIK),INTENT(IN) :: funit
  INTEGER(SIK),INTENT(IN),OPTIONAL :: indent
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: prefix
  LOGICAL(SBK),INTENT(IN),OPTIONAL :: paddtw
  CHARACTER(LEN=12) :: fmt,fmt2
  CHARACTER(LEN=:),ALLOCATABLE :: dtype
  INTEGER(SIK) :: i,j,k
  TYPE(StringType) :: sprefix,sdtype

  i=1
  j=5
  IF(PRESENT(indent)) i=i+indent
  IF(PRESENT(prefix)) sprefix=prefix
  sdtype=thisParam%datatype
  IF(PRESENT(paddtw)) THEN
    IF(paddtw) THEN
      ALLOCATE(CHARACTER(PARAM_MAX_DAT_LEN) :: dtype)
      dtype=CHAR(thisParam%dataType)
      sdtype=dtype
    ENDIF
  ENDIF
  j=j+LEN(sprefix)
  WRITE(fmt,'(i12)') i; fmt=ADJUSTL(fmt)
  WRITE(UNIT=funit,FMT='('//TRIM(fmt)//'x,a,l3)',ADVANCE='NO') sprefix// &
      sdtype//' :: '//thisParam%name//'=',thisParam%val(1)
  j=j+LEN(sdtype)+LEN(thisParam%name)
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
      TYPE IS(ParamType_SBK_a1)
        val=p%val
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
SUBROUTINE edit_ParamType_STR_a1(thisParam,funit,indent,prefix,paddtw)
  CLASS(ParamType_STR_a1),INTENT(IN) :: thisParam
  INTEGER(SIK),INTENT(IN) :: funit
  INTEGER(SIK),INTENT(IN),OPTIONAL :: indent
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: prefix
  LOGICAL(SBK),INTENT(IN),OPTIONAL :: paddtw
  CHARACTER(LEN=12) :: fmt,fmt2
  CHARACTER(LEN=:),ALLOCATABLE :: dtype
  INTEGER(SIK) :: i,j,k
  TYPE(StringType) :: sprefix,sdtype,sval,sdesc

  i=1
  j=5
  IF(PRESENT(indent)) i=i+indent
  sprefix=''
  IF(PRESENT(prefix)) sprefix=prefix
  sdtype=thisParam%datatype
  IF(PRESENT(paddtw)) THEN
    IF(paddtw) THEN
      ALLOCATE(CHARACTER(PARAM_MAX_DAT_LEN) :: dtype)
      dtype=CHAR(thisParam%dataType)
      sdtype=dtype
    ENDIF
  ENDIF
  j=j+LEN(sprefix)
  WRITE(fmt,'(i12)') i; fmt=ADJUSTL(fmt)

  sval=''
  IF(LEN_TRIM(thisParam%val(1)) > 0) sval=thisParam%val(1)
  sdesc=''
  IF(LEN_TRIM(thisParam%description) > 0) sdesc=' !'//thisParam%description
  WRITE(UNIT=funit,FMT='('//TRIM(fmt)//'x,a)',ADVANCE='NO') sprefix// &
      sdtype//' :: '//thisParam%name//'='//sval
  j=j+LEN(sdtype)+LEN(thisParam%name)
  WRITE(fmt2,'(i12)') j; fmt2=ADJUSTL(fmt2)

  IF (SIZE(thisParam%val)>MAX_1D_LEN) THEN
    DO k=2,SIZE(thisParam%val)
      sval=''
      IF(LEN_TRIM(thisParam%val(k)) > 0) sval=thisParam%val(k)
      WRITE(UNIT=funit,FMT='(", ",a)',ADVANCE='NO') CHAR(sval)
    ENDDO
    WRITE(UNIT=funit,FMT='(a)') CHAR(sdesc)
  ELSE
    WRITE(UNIT=funit,FMT='(a)') CHAR(sdesc)
    DO k=2,SIZE(thisParam%val)
      sval=''
      IF(LEN_TRIM(thisParam%val(k)) > 0) sval=thisParam%val(k)
      WRITE(UNIT=funit,FMT='('//TRIM(fmt)//'x,'//TRIM(fmt2)//'x,a)') &
          CHAR(sval)
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
  IF(ALLOCATED(thisParam%val)) THEN
    DEALLOCATE(thisParam%val)
  ENDIF
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
      TYPE IS(ParamType_STR_a1)
        val=p%val
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
SUBROUTINE edit_ParamType_SSK_a2(thisParam,funit,indent,prefix,paddtw)
  CLASS(ParamType_SSK_a2),INTENT(IN) :: thisParam
  INTEGER(SIK),INTENT(IN) :: funit
  INTEGER(SIK),INTENT(IN),OPTIONAL :: indent
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: prefix
  LOGICAL(SBK),INTENT(IN),OPTIONAL :: paddtw
  CHARACTER(LEN=12) :: fmt,fmt2,fmt3
  CHARACTER(LEN=:),ALLOCATABLE :: dtype
  INTEGER(SIK) :: i,j,k,l
  TYPE(StringType) :: sprefix,sdtype

  i=1
  j=6
  IF(PRESENT(indent)) i=i+indent
  IF(PRESENT(prefix)) sprefix=prefix
  sdtype=thisParam%datatype
  IF(PRESENT(paddtw)) THEN
    IF(paddtw) THEN
      ALLOCATE(CHARACTER(PARAM_MAX_DAT_LEN) :: dtype)
      dtype=CHAR(thisParam%dataType)
      sdtype=dtype
    ENDIF
  ENDIF
  j=j+LEN(sprefix)
  WRITE(fmt,'(i12)') i; fmt=ADJUSTL(fmt)
  IF(LEN_TRIM(thisParam%description) == 0) THEN
    WRITE(UNIT=funit,FMT='('//TRIM(fmt)//'x,a)') sprefix// &
        sdtype//' :: '//thisParam%name//'= ...'
  ELSE
    WRITE(UNIT=funit,FMT='('//TRIM(fmt)//'x,a,a)') sprefix// &
        sdtype//' :: '//thisParam%name//'= ... !'//thisParam%description
  ENDIF
  j=j+LEN(sdtype)+LEN(thisParam%name)
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
      TYPE IS(ParamType_SSK_a2)
        val=p%val
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
SUBROUTINE edit_ParamType_SDK_a2(thisParam,funit,indent,prefix,paddtw)
  CLASS(ParamType_SDK_a2),INTENT(IN) :: thisParam
  INTEGER(SIK),INTENT(IN) :: funit
  INTEGER(SIK),INTENT(IN),OPTIONAL :: indent
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: prefix
  LOGICAL(SBK),INTENT(IN),OPTIONAL :: paddtw
  CHARACTER(LEN=12) :: fmt,fmt2,fmt3
  CHARACTER(LEN=:),ALLOCATABLE :: dtype
  INTEGER(SIK) :: i,j,k,l
  TYPE(StringType) :: sprefix,sdtype

  i=1
  j=6
  IF(PRESENT(indent)) i=i+indent
  IF(PRESENT(prefix)) sprefix=prefix
  sdtype=thisParam%datatype
  IF(PRESENT(paddtw)) THEN
    IF(paddtw) THEN
      ALLOCATE(CHARACTER(PARAM_MAX_DAT_LEN) :: dtype)
      dtype=CHAR(thisParam%dataType)
      sdtype=dtype
    ENDIF
  ENDIF
  j=j+LEN(sprefix)
  WRITE(fmt,'(i12)') i; fmt=ADJUSTL(fmt)
  IF(LEN_TRIM(thisParam%description) == 0) THEN
    WRITE(UNIT=funit,FMT='('//TRIM(fmt)//'x,a)') sprefix// &
        sdtype//' :: '//thisParam%name//'= ...'
  ELSE
    WRITE(UNIT=funit,FMT='('//TRIM(fmt)//'x,a,a)') sprefix// &
        sdtype//' :: '//thisParam%name//'= ... !'//thisParam%description
  ENDIF
  j=j+LEN(sdtype)+LEN(thisParam%name)
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
      TYPE IS(ParamType_SDK_a2)
        val=p%val
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
SUBROUTINE edit_ParamType_SNK_a2(thisParam,funit,indent,prefix,paddtw)
  CLASS(ParamType_SNK_a2),INTENT(IN) :: thisParam
  INTEGER(SIK),INTENT(IN) :: funit
  INTEGER(SIK),INTENT(IN),OPTIONAL :: indent
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: prefix
  LOGICAL(SBK),INTENT(IN),OPTIONAL :: paddtw
  CHARACTER(LEN=12) :: fmt,fmt2,fmt3
  CHARACTER(LEN=:),ALLOCATABLE :: dtype
  INTEGER(SIK) :: i,j,k,l
  TYPE(StringType) :: sprefix,sdtype

  i=1
  j=6
  IF(PRESENT(indent)) i=i+indent
  IF(PRESENT(prefix)) sprefix=prefix
  sdtype=thisParam%datatype
  IF(PRESENT(paddtw)) THEN
    IF(paddtw) THEN
      ALLOCATE(CHARACTER(PARAM_MAX_DAT_LEN) :: dtype)
      dtype=CHAR(thisParam%dataType)
      sdtype=dtype
    ENDIF
  ENDIF
  j=j+LEN(sprefix)
  WRITE(fmt,'(i12)') i; fmt=ADJUSTL(fmt)
  IF(LEN_TRIM(thisParam%description) == 0) THEN
    WRITE(UNIT=funit,FMT='('//TRIM(fmt)//'x,a)') sprefix// &
        sdtype//' :: '//thisParam%name//'= ...'
  ELSE
    WRITE(UNIT=funit,FMT='('//TRIM(fmt)//'x,a,a)') sprefix// &
        sdtype//' :: '//thisParam%name//'= ... !'//thisParam%description
  ENDIF
  j=j+LEN(sdtype)+LEN(thisParam%name)
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
      TYPE IS(ParamType_SNK_a2)
        val=p%val
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
SUBROUTINE edit_ParamType_SLK_a2(thisParam,funit,indent,prefix,paddtw)
  CLASS(ParamType_SLK_a2),INTENT(IN) :: thisParam
  INTEGER(SIK),INTENT(IN) :: funit
  INTEGER(SIK),INTENT(IN),OPTIONAL :: indent
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: prefix
  LOGICAL(SBK),INTENT(IN),OPTIONAL :: paddtw
  CHARACTER(LEN=12) :: fmt,fmt2,fmt3
  CHARACTER(LEN=:),ALLOCATABLE :: dtype
  INTEGER(SIK) :: i,j,k,l
  TYPE(StringType) :: sprefix,sdtype

  i=1
  j=6
  IF(PRESENT(indent)) i=i+indent
  IF(PRESENT(prefix)) sprefix=prefix
  sdtype=thisParam%datatype
  IF(PRESENT(paddtw)) THEN
    IF(paddtw) THEN
      ALLOCATE(CHARACTER(PARAM_MAX_DAT_LEN) :: dtype)
      dtype=CHAR(thisParam%dataType)
      sdtype=dtype
    ENDIF
  ENDIF
  j=j+LEN(sprefix)
  WRITE(fmt,'(i12)') i; fmt=ADJUSTL(fmt)
  IF(LEN_TRIM(thisParam%description) == 0) THEN
    WRITE(UNIT=funit,FMT='('//TRIM(fmt)//'x,a)') sprefix// &
        sdtype//' :: '//thisParam%name//'= ...'
  ELSE
    WRITE(UNIT=funit,FMT='('//TRIM(fmt)//'x,a,a)') sprefix// &
        sdtype//' :: '//thisParam%name//'= ... !'//thisParam%description
  ENDIF
  j=j+LEN(sdtype)+LEN(thisParam%name)
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
      TYPE IS(ParamType_SLK_a2)
        val=p%val
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
SUBROUTINE edit_ParamType_STR_a2(thisParam,funit,indent,prefix,paddtw)
  CLASS(ParamType_STR_a2),INTENT(IN) :: thisParam
  INTEGER(SIK),INTENT(IN) :: funit
  INTEGER(SIK),INTENT(IN),OPTIONAL :: indent
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: prefix
  LOGICAL(SBK),INTENT(IN),OPTIONAL :: paddtw
  CHARACTER(LEN=12) :: fmt,fmt2,fmt3
  CHARACTER(LEN=:),ALLOCATABLE :: dtype
  INTEGER(SIK) :: i,j,k,l
  TYPE(StringType) :: sprefix,sdtype

  i=1
  j=6
  IF(PRESENT(indent)) i=i+indent
  IF(PRESENT(prefix)) sprefix=prefix
  sdtype=thisParam%datatype
  IF(PRESENT(paddtw)) THEN
    IF(paddtw) THEN
      ALLOCATE(CHARACTER(PARAM_MAX_DAT_LEN) :: dtype)
      dtype=CHAR(thisParam%dataType)
      sdtype=dtype
    ENDIF
  ENDIF
  j=j+LEN(sprefix)
  WRITE(fmt,'(i12)') i; fmt=ADJUSTL(fmt)
  !tmpstr(1)=CHAR(thisParam%val(1))
  IF(LEN_TRIM(thisParam%description) == 0) THEN
    WRITE(UNIT=funit,FMT='('//TRIM(fmt)//'x,a)') sprefix// &
        sdtype//' :: '//thisParam%name//'= ...'
  ELSE
    WRITE(UNIT=funit,FMT='('//TRIM(fmt)//'x,a,a)') sprefix// &
        sdtype//' :: '//thisParam%name//'= ... !'//thisParam%description
  ENDIF
  j=j+LEN(sdtype)+LEN(thisParam%name)
  WRITE(fmt2,'(i12)') j; fmt2=ADJUSTL(fmt2)
  WRITE(fmt3,'(i12)') SIZE(thisParam%val,1); fmt3=ADJUSTL(fmt3)
  DO k=1,SIZE(thisParam%val,2)
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
      TYPE IS(ParamType_STR_a2)
        val=p%val
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
SUBROUTINE edit_ParamType_SSK_a3(thisParam,funit,indent,prefix,paddtw)
  CLASS(ParamType_SSK_a3),INTENT(IN) :: thisParam
  INTEGER(SIK),INTENT(IN) :: funit
  INTEGER(SIK),INTENT(IN),OPTIONAL :: indent
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: prefix
  LOGICAL(SBK),INTENT(IN),OPTIONAL :: paddtw
  CHARACTER(LEN=12) :: fmt,fmt2,fmt3
  CHARACTER(LEN=:),ALLOCATABLE :: dtype
  INTEGER(SIK) :: i,j,k,l,m
  TYPE(StringType) :: sprefix,sdtype

  i=1
  j=6
  IF(PRESENT(indent)) i=i+indent
  IF(PRESENT(prefix)) sprefix=prefix
  sdtype=thisParam%datatype
  IF(PRESENT(paddtw)) THEN
    IF(paddtw) THEN
      ALLOCATE(CHARACTER(PARAM_MAX_DAT_LEN) :: dtype)
      dtype=CHAR(thisParam%dataType)
      sdtype=dtype
    ENDIF
  ENDIF
  WRITE(fmt,'(i12)') i; fmt=ADJUSTL(fmt)
  IF(LEN_TRIM(thisParam%description) == 0) THEN
    WRITE(UNIT=funit,FMT='('//TRIM(fmt)//'x,a)') sprefix// &
        sdtype//' :: '//thisParam%name//'= ...'
  ELSE
    WRITE(UNIT=funit,FMT='('//TRIM(fmt)//'x,a,a)') sprefix// &
        sdtype//' :: '//thisParam%name//'= ... !'//thisParam%description
  ENDIF
  j=j+LEN(sdtype)+LEN(thisParam%name)
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
      TYPE IS(ParamType_SSK_a3)
        val=p%val
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
SUBROUTINE edit_ParamType_SDK_a3(thisParam,funit,indent,prefix,paddtw)
  CLASS(ParamType_SDK_a3),INTENT(IN) :: thisParam
  INTEGER(SIK),INTENT(IN) :: funit
  INTEGER(SIK),INTENT(IN),OPTIONAL :: indent
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: prefix
  LOGICAL(SBK),INTENT(IN),OPTIONAL :: paddtw
  CHARACTER(LEN=12) :: fmt,fmt2,fmt3
  CHARACTER(LEN=:),ALLOCATABLE :: dtype
  INTEGER(SIK) :: i,j,k,l,m
  TYPE(StringType) :: sprefix,sdtype

  i=1
  j=6
  IF(PRESENT(indent)) i=i+indent
  IF(PRESENT(prefix)) sprefix=prefix
  sdtype=thisParam%datatype
  IF(PRESENT(paddtw)) THEN
    IF(paddtw) THEN
      ALLOCATE(CHARACTER(PARAM_MAX_DAT_LEN) :: dtype)
      dtype=CHAR(thisParam%dataType)
      sdtype=dtype
    ENDIF
  ENDIF
  WRITE(fmt,'(i12)') i; fmt=ADJUSTL(fmt)
  IF(LEN_TRIM(thisParam%description) == 0) THEN
    WRITE(UNIT=funit,FMT='('//TRIM(fmt)//'x,a)') sprefix// &
        sdtype//' :: '//thisParam%name//'= ...'
  ELSE
    WRITE(UNIT=funit,FMT='('//TRIM(fmt)//'x,a,a)') sprefix// &
        sdtype//' :: '//thisParam%name//'= ... !'//thisParam%description
  ENDIF
  j=j+LEN(sdtype)+LEN(thisParam%name)
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
      TYPE IS(ParamType_SDK_a3)
        val=p%val
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
SUBROUTINE edit_ParamType_SNK_a3(thisParam,funit,indent,prefix,paddtw)
  CLASS(ParamType_SNK_a3),INTENT(IN) :: thisParam
  INTEGER(SIK),INTENT(IN) :: funit
  INTEGER(SIK),INTENT(IN),OPTIONAL :: indent
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: prefix
  LOGICAL(SBK),INTENT(IN),OPTIONAL :: paddtw
  CHARACTER(LEN=12) :: fmt,fmt2,fmt3
  CHARACTER(LEN=:),ALLOCATABLE :: dtype
  INTEGER(SIK) :: i,j,k,l,m
  TYPE(StringType) :: sprefix,sdtype

  i=1
  j=6
  IF(PRESENT(indent)) i=i+indent
  IF(PRESENT(prefix)) sprefix=prefix
  sdtype=thisParam%datatype
  IF(PRESENT(paddtw)) THEN
    IF(paddtw) THEN
      ALLOCATE(CHARACTER(PARAM_MAX_DAT_LEN) :: dtype)
      dtype=CHAR(thisParam%dataType)
      sdtype=dtype
    ENDIF
  ENDIF
  WRITE(fmt,'(i12)') i; fmt=ADJUSTL(fmt)
  IF(LEN_TRIM(thisParam%description) == 0) THEN
    WRITE(UNIT=funit,FMT='('//TRIM(fmt)//'x,a)') sprefix// &
        sdtype//' :: '//thisParam%name//'= ...'
  ELSE
    WRITE(UNIT=funit,FMT='('//TRIM(fmt)//'x,a,a)') sprefix// &
        sdtype//' :: '//thisParam%name//'= ... !'//thisParam%description
  ENDIF
  j=j+LEN(sdtype)+LEN(thisParam%name)
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
      TYPE IS(ParamType_SNK_a3)
        val=p%val
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
SUBROUTINE edit_ParamType_SLK_a3(thisParam,funit,indent,prefix,paddtw)
  CLASS(ParamType_SLK_a3),INTENT(IN) :: thisParam
  INTEGER(SIK),INTENT(IN) :: funit
  INTEGER(SIK),INTENT(IN),OPTIONAL :: indent
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: prefix
  LOGICAL(SBK),INTENT(IN),OPTIONAL :: paddtw
  CHARACTER(LEN=12) :: fmt,fmt2,fmt3
  CHARACTER(LEN=:),ALLOCATABLE :: dtype
  INTEGER(SIK) :: i,j,k,l,m
  TYPE(StringType) :: sprefix,sdtype

  i=1
  j=6
  IF(PRESENT(indent)) i=i+indent
  IF(PRESENT(prefix)) sprefix=prefix
  sdtype=thisParam%datatype
  IF(PRESENT(paddtw)) THEN
    IF(paddtw) THEN
      ALLOCATE(CHARACTER(PARAM_MAX_DAT_LEN) :: dtype)
      dtype=CHAR(thisParam%dataType)
      sdtype=dtype
    ENDIF
  ENDIF
  WRITE(fmt,'(i12)') i; fmt=ADJUSTL(fmt)
  IF(LEN_TRIM(thisParam%description) == 0) THEN
    WRITE(UNIT=funit,FMT='('//TRIM(fmt)//'x,a)') sprefix// &
        sdtype//' :: '//thisParam%name//'= ...'
  ELSE
    WRITE(UNIT=funit,FMT='('//TRIM(fmt)//'x,a,a)') sprefix// &
        sdtype//' :: '//thisParam%name//'= ... !'//thisParam%description
  ENDIF
  j=j+LEN(sdtype)+LEN(thisParam%name)
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
      TYPE IS(ParamType_SLK_a3)
        val=p%val
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
!4444444444444444444444444444444444444444444444444444444444444444444444444444444
!        Four Dimensional Arrays
!4444444444444444444444444444444444444444444444444444444444444444444444444444444
!
!-------------------------------------------------------------------------------
!> @brief Initializes a ParamType object as a four dimensional array of single
!>        precision reals
!> @param thisParam the parameter to initialize
!> @param name the name of the parameter
!> @param param a four dimensional array of single precision reals
!> @param description an optional description for this parameter
!>
!> This routine is not recursive, so it is like setting a four dimensional array of parameter.
!> Therefore the name cannot contain the "->" symbol to indicate access to a
!> sub-list. @c thisParam must not already be inititalized.
!>
SUBROUTINE init_ParamType_SSK_a4(thisParam,name,param,description)
  CHARACTER(LEN=*),PARAMETER :: myName='init_ParamType_SSK_a4'
  CLASS(ParamType),INTENT(INOUT) :: thisParam
  REAL(SSK),INTENT(IN) :: param(:,:,:,:)
  CHARACTER(LEN=*),INTENT(IN) :: name
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
  INTEGER(SIK) :: ipos

  IF(.NOT.ASSOCIATED(thisParam%pdat)) THEN
    !Check that '->' character is not in name
    ipos=INDEX(name,'->')
    IF(ipos == 0) THEN
      ALLOCATE(ParamType_SSK_a4 :: thisParam%pdat)
      thisParam%pdat%name=TRIM(name)
      IF(PRESENT(description)) thisParam%pdat%description=TRIM(description)
      thisParam%pdat%dataType='4-D ARRAY REAL(SSK)'
      SELECTTYPE(p=>thisParam%pdat)
      TYPE IS(ParamType_SSK_a4)
        ALLOCATE(p%val(SIZE(param,1),SIZE(param,2),SIZE(param,3),SIZE(param,4)))
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
ENDSUBROUTINE init_ParamType_SSK_a4
!
!-------------------------------------------------------------------------------
!> @brief Edits a four dimensional array of single precision real valued parameters
!> @param thisParam the four dimensional array of single precision real valued
!>        parameters to edit
!> @param funit the unit number to edit the parameter to
!> @param indent optional indicates the number of blank spaces to precede the
!>        beginning of text to edit.
!>
!> The formatted write uses the "general" edit descriptor so that 7 digits (four
!> more than the significant number in a single precision real) are always
!> printed if the number is very large in absolute value engineering format
!> is used otherwise floating point form is used to write the value.
!>
SUBROUTINE edit_ParamType_SSK_a4(thisParam,funit,indent,prefix,paddtw)
  CLASS(ParamType_SSK_a4),INTENT(IN) :: thisParam
  INTEGER(SIK),INTENT(IN) :: funit
  INTEGER(SIK),INTENT(IN),OPTIONAL :: indent
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: prefix
  LOGICAL(SBK),INTENT(IN),OPTIONAL :: paddtw
  CHARACTER(LEN=12) :: fmt,fmt2,fmt3
  CHARACTER(LEN=:),ALLOCATABLE :: dtype
  INTEGER(SIK) :: i,j,k,l,m,n
  TYPE(StringType) :: sprefix,sdtype

  i=1
  j=6
  IF(PRESENT(indent)) i=i+indent
  IF(PRESENT(prefix)) sprefix=prefix
  sdtype=thisParam%datatype
  IF(PRESENT(paddtw)) THEN
    IF(paddtw) THEN
      ALLOCATE(CHARACTER(PARAM_MAX_DAT_LEN) :: dtype)
      dtype=CHAR(thisParam%dataType)
      sdtype=dtype
    ENDIF
  ENDIF
  WRITE(fmt,'(i12)') i; fmt=ADJUSTL(fmt)
  IF(LEN_TRIM(thisParam%description) == 0) THEN
    WRITE(UNIT=funit,FMT='('//TRIM(fmt)//'x,a)') sprefix// &
        sdtype//' :: '//thisParam%name//'= ...'
  ELSE
    WRITE(UNIT=funit,FMT='('//TRIM(fmt)//'x,a,a)') sprefix// &
        sdtype//' :: '//thisParam%name//'= ... !'//thisParam%description
  ENDIF
  j=j+LEN(sdtype)+LEN(thisParam%name)
  WRITE(fmt2,'(i12)') j; fmt2=ADJUSTL(fmt2)
  WRITE(fmt3,'(i12)') SIZE(thisParam%val,1); fmt3=ADJUSTL(fmt3)
  DO n=1,SIZE(thisParam%val,4)
    DO k=1,SIZE(thisParam%val,3)
      DO l=1,SIZE(thisParam%val,2)
        WRITE(UNIT=funit,FMT='('//TRIM(fmt)//'x,'//TRIM(fmt2)//'x,'// &
            TRIM(fmt3)//'(g13.7))') (thisParam%val(m,l,k,n),m=1,SIZE(thisParam%val,1))
      ENDDO
    ENDDO
  ENDDO
ENDSUBROUTINE edit_ParamType_SSK_a4
!
!-------------------------------------------------------------------------------
!> @brief Clears a four dimensional array of single precision real valued parameter
!> @param thisParam the four dimensional array of single precision real valued parameter to clear
!>
SUBROUTINE clear_ParamType_SSK_a4(thisParam)
  CLASS(ParamType_SSK_a4),INTENT(INOUT) :: thisParam
  DEALLOCATE(thisParam%val)
  thisParam%name=''
  thisParam%dataType=''
  thisParam%description=''
ENDSUBROUTINE clear_ParamType_SSK_a4
!
!-------------------------------------------------------------------------------
!> @brief Sets the value of an existing four dimensional array of single precision real valued
!> parameter to a new value.
!> @param thisParam the parameter in which an existing parameter with name
!>        matching @c name will be to set the new value of @c param
!> @param name the name of an existing parameter to set the value of
!> @param param the new value to set for the parameter
!> @param description an optional new description for the parameter identified
!>        by @c name
!>
!> If a parameter with @c name is not found an error is produced. If the
!> parameter with @c name is not a four dimensional array of single precision real valued parameter
!> then an error is produced.
!>
SUBROUTINE set_ParamType_SSK_a4(thisParam,name,param,description)
  CHARACTER(LEN=*),PARAMETER :: myName='set_ParamType_SSK_a4'
  CLASS(ParamType),INTENT(INOUT) :: thisParam
  CHARACTER(LEN=*),INTENT(IN) :: name
  REAL(SSK),INTENT(IN) :: param(:,:,:,:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
  CLASS(ParamType),POINTER :: tmpParam

  SELECTTYPE(thisParam)
  TYPE IS(ParamType_SSK_a4)
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
      TYPE IS(ParamType_SSK_a4)
        p%val=param
        IF(PRESENT(description)) p%description=TRIM(description)
      CLASS DEFAULT
        CALL eParams%raiseError(modName//'::'//myName// &
            ' - parameter data type mismatch! Parameter '//TRIM(name)//' type is '// &
            tmpParam%dataType//' and must be 4-D ARRAY REAL(SSK)!')
      ENDSELECT
    ELSE
      CALL eParams%raiseError(modName//'::'//myName// &
          ' - unable to locate parameter "'//TRIM(name)//'" in "'// &
          thisParam%name//'"!')
    ENDIF
  ENDSELECT
ENDSUBROUTINE set_ParamType_SSK_a4
!
!-------------------------------------------------------------------------------
!> @brief Gets the four dimensional array of single precision real value for a specified parameter
!> @param thisParam the parameter in which an existing parameter with name
!>        matching @c name will have it's value returned
!> @param name the name of the parameter to return the value of
!> @param val the current value of the parameter with @c name
!>
!> If a parameter with @c name is not found an error is produced. If the
!> parameter with @c name is not a four dimensional array of single precision real valued parameter
!> then an error is produced.
!>
SUBROUTINE get_ParamType_SSK_a4(thisParam,name,val)
  CHARACTER(LEN=*),PARAMETER :: myName='get_ParamType_SSK_a4'
  CLASS(ParamType),INTENT(IN) :: thisParam
  CHARACTER(LEN=*),INTENT(IN) :: name
  REAL(SSK),ALLOCATABLE,INTENT(INOUT) :: val(:,:,:,:)
  CLASS(ParamType),POINTER :: tmpParam

  SELECTTYPE(thisParam)
  TYPE IS(ParamType_SSK_a4)
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
      TYPE IS(ParamType_SSK_a4)
        val=p%val
      CLASS DEFAULT
        CALL eParams%raiseError(modName//'::'//myName// &
            ' - parameter data type mismatch! Parameter '//TRIM(name)//' type is '// &
            tmpParam%dataType//' and must be 4-D ARRAY REAL(SSK)!')
      ENDSELECT
    ELSE
      CALL eParams%raiseError(modName//'::'//myName// &
          ' - unable to locate parameter "'//TRIM(name)//'" in "'// &
          thisParam%name//'"!')
    ENDIF
  ENDSELECT
ENDSUBROUTINE get_ParamType_SSK_a4
!
!-------------------------------------------------------------------------------
!> @brief Adds a new four dimensional array of single precision real valued parameter to a set of
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
SUBROUTINE add_ParamType_SSK_a4(thisParam,name,param,description)
  CHARACTER(LEN=*),PARAMETER :: myName='add_ParamType_SSK_a4'
  CLASS(ParamType),INTENT(INOUT) :: thisParam
  CHARACTER(LEN=*),INTENT(IN) :: name
  REAL(SSK),INTENT(IN) :: param(:,:,:,:)
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
    CALL init_ParamType_SSK_a4(newParam,thisname,param,description)

    !Add the new parameter to thisParam
    CALL add_ParamType(thisParam,prevname,newParam)
    CALL newParam%clear()
  ELSE
    CALL eParams%raiseError(modName//'::'//myName// &
        ' - parameter name "'//TRIM(name)// &
        '" already exists! Use set method or full parameter list path!')
  ENDIF
ENDSUBROUTINE add_ParamType_SSK_a4
!
!-------------------------------------------------------------------------------
!> @brief Initializes a ParamType object as a four dimensional array of double precision real
!> @param thisParam the parameter to initialize
!> @param name the name of the parameter
!> @param param a four dimensional array of double precision real
!> @param description an optional description for this parameter
!>
!> This routine is not recursive, so it is like setting a four dimensional array of parameter.
!> Therefore the name cannot contain the "->" symbol to indicate access to a
!> sub-list. @c thisParam must not already be inititalized.
!>
SUBROUTINE init_ParamType_SDK_a4(thisParam,name,param,description)
  CHARACTER(LEN=*),PARAMETER :: myName='init_ParamType_SDK_a4'
  CLASS(ParamType),INTENT(INOUT) :: thisParam
  REAL(SDK),INTENT(IN) :: param(:,:,:,:)
  CHARACTER(LEN=*),INTENT(IN) :: name
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
  INTEGER(SIK) :: ipos

  IF(.NOT.ASSOCIATED(thisParam%pdat)) THEN
    !Check that '->' character is not in name
    ipos=INDEX(name,'->')
    IF(ipos == 0) THEN
      ALLOCATE(ParamType_SDK_a4 :: thisParam%pdat)
      thisParam%pdat%name=TRIM(name)
      IF(PRESENT(description)) thisParam%pdat%description=TRIM(description)
      thisParam%pdat%dataType='4-D ARRAY REAL(SDK)'
      SELECTTYPE(p=>thisParam%pdat)
      TYPE IS(ParamType_SDK_a4)
        ALLOCATE(p%val(SIZE(param,1),SIZE(param,2),SIZE(param,3),SIZE(param,4)))
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
ENDSUBROUTINE init_ParamType_SDK_a4
!
!-------------------------------------------------------------------------------
!> @brief Edits a four dimensional array of double precision real valued parameter
!> @param thisParam the four dimensional array of double precision real valued parameter to edit
!> @param funit the unit number to edit the parameter to
!> @param indent optional indicates the number of blank spaces to precede the
!>        beginning of text to edit.
!>
!> The formatted write uses the "general" edit descriptor so that 7 digits (four
!> more than the significant number in a double precision real) are always
!> printed if the number is very large in absolute value engineering format
!> is used otherwise floating point form is used to write the value.
!>
SUBROUTINE edit_ParamType_SDK_a4(thisParam,funit,indent,prefix,paddtw)
  CLASS(ParamType_SDK_a4),INTENT(IN) :: thisParam
  INTEGER(SIK),INTENT(IN) :: funit
  INTEGER(SIK),INTENT(IN),OPTIONAL :: indent
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: prefix
  LOGICAL(SBK),INTENT(IN),OPTIONAL :: paddtw
  CHARACTER(LEN=12) :: fmt,fmt2,fmt3
  CHARACTER(LEN=:),ALLOCATABLE :: dtype
  INTEGER(SIK) :: i,j,k,l,m,n
  TYPE(StringType) :: sprefix,sdtype

  i=1
  j=6
  IF(PRESENT(indent)) i=i+indent
  IF(PRESENT(prefix)) sprefix=prefix
  sdtype=thisParam%datatype
  IF(PRESENT(paddtw)) THEN
    IF(paddtw) THEN
      ALLOCATE(CHARACTER(PARAM_MAX_DAT_LEN) :: dtype)
      dtype=CHAR(thisParam%dataType)
      sdtype=dtype
    ENDIF
  ENDIF
  WRITE(fmt,'(i12)') i; fmt=ADJUSTL(fmt)
  IF(LEN_TRIM(thisParam%description) == 0) THEN
    WRITE(UNIT=funit,FMT='('//TRIM(fmt)//'x,a)') sprefix// &
        sdtype//' :: '//thisParam%name//'= ...'
  ELSE
    WRITE(UNIT=funit,FMT='('//TRIM(fmt)//'x,a,a)') sprefix// &
        sdtype//' :: '//thisParam%name//'= ... !'//thisParam%description
  ENDIF
  j=j+LEN(sdtype)+LEN(thisParam%name)
  WRITE(fmt2,'(i12)') j; fmt2=ADJUSTL(fmt2)
  WRITE(fmt3,'(i12)') SIZE(thisParam%val,1); fmt3=ADJUSTL(fmt3)
  DO n=1,SIZE(thisParam%val,4)
    DO k=1,SIZE(thisParam%val,3)
      DO l=1,SIZE(thisParam%val,2)
        WRITE(UNIT=funit,FMT='('//TRIM(fmt)//'x,'//TRIM(fmt2)//'x,'// &
            TRIM(fmt3)//'(g20.14))') (thisParam%val(m,l,k,n),m=1,SIZE(thisParam%val,1))
      ENDDO
    ENDDO
  ENDDO
ENDSUBROUTINE edit_ParamType_SDK_a4
!
!-------------------------------------------------------------------------------
!> @brief Clears a four dimensional array of double precision real valued parameter
!> @param thisParam the four dimensional array of double precision real valued parameter to clear
!>
SUBROUTINE clear_ParamType_SDK_a4(thisParam)
  CLASS(ParamType_SDK_a4),INTENT(INOUT) :: thisParam
  DEALLOCATE(thisParam%val)
  thisParam%name=''
  thisParam%dataType=''
  thisParam%description=''
ENDSUBROUTINE clear_ParamType_SDK_a4
!
!-------------------------------------------------------------------------------
!> @brief Sets the value of an existing four dimensional array of double precision real valued
!> parameter to a new value.
!> @param thisParam the parameter in which an existing parameter with name
!>        matching @c name will be to set the new value of @c param
!> @param name the name of an existing parameter to set the value of
!> @param param the new value to set for the parameter
!> @param description an optional new description for the parameter identified
!>        by @c name
!>
!> If a parameter with @c name is not found an error is produced. If the
!> parameter with @c name is not a four dimensional array of double precision real valued parameter
!> then an error is produced.
!>
SUBROUTINE set_ParamType_SDK_a4(thisParam,name,param,description)
  CHARACTER(LEN=*),PARAMETER :: myName='set_ParamType_SDK_a4'
  CLASS(ParamType),INTENT(INOUT) :: thisParam
  CHARACTER(LEN=*),INTENT(IN) :: name
  REAL(SDK),INTENT(IN) :: param(:,:,:,:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
  CLASS(ParamType),POINTER :: tmpParam

  SELECTTYPE(thisParam)
  TYPE IS(ParamType_SDK_a4)
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
      TYPE IS(ParamType_SDK_a4)
        p%val=param
        IF(PRESENT(description)) p%description=TRIM(description)
      CLASS DEFAULT
        CALL eParams%raiseError(modName//'::'//myName// &
            ' - parameter data type mismatch! Parameter '//TRIM(name)//' type is '// &
            tmpParam%dataType//' and must be 4-D ARRAY REAL(SDK)!')
      ENDSELECT
    ELSE
      CALL eParams%raiseError(modName//'::'//myName// &
          ' - unable to locate parameter "'//TRIM(name)//'" in "'// &
          thisParam%name//'"!')
    ENDIF
  ENDSELECT
ENDSUBROUTINE set_ParamType_SDK_a4
!
!-------------------------------------------------------------------------------
!> @brief Gets the four dimensional array of double precision real value for a specified parameter
!> @param thisParam the parameter in which an existing parameter with name
!>        matching @c name will have it's value returned
!> @param name the name of the parameter to return the value of
!> @param val the current value of the parameter with @c name
!>
!> If a parameter with @c name is not found an error is produced. If the
!> parameter with @c name is not a four dimensional array of double precision real valued parameter
!> then an error is produced.
!>
SUBROUTINE get_ParamType_SDK_a4(thisParam,name,val)
  CHARACTER(LEN=*),PARAMETER :: myName='get_ParamType_SDK_a4'
  CLASS(ParamType),INTENT(IN) :: thisParam
  CHARACTER(LEN=*),INTENT(IN) :: name
  REAL(SDK),ALLOCATABLE,INTENT(INOUT) :: val(:,:,:,:)
  CLASS(ParamType),POINTER :: tmpParam

  SELECTTYPE(thisParam)
  TYPE IS(ParamType_SDK_a4)
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
      TYPE IS(ParamType_SDK_a4)
        val=p%val
      CLASS DEFAULT
        CALL eParams%raiseError(modName//'::'//myName// &
            ' - parameter data type mismatch! Parameter '//TRIM(name)//' type is '// &
            tmpParam%dataType//' and must be 4-D ARRAY REAL(SDK)!')
      ENDSELECT
    ELSE
      CALL eParams%raiseError(modName//'::'//myName// &
          ' - unable to locate parameter "'//TRIM(name)//'" in "'// &
          thisParam%name//'"!')
    ENDIF
  ENDSELECT
ENDSUBROUTINE get_ParamType_SDK_a4
!
!-------------------------------------------------------------------------------
!> @brief Adds a new four dimensional array of double precision real valued parameter to a set of
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
SUBROUTINE add_ParamType_SDK_a4(thisParam,name,param,description)
  CHARACTER(LEN=*),PARAMETER :: myName='add_ParamType_SDK_a4'
  CLASS(ParamType),INTENT(INOUT) :: thisParam
  CHARACTER(LEN=*),INTENT(IN) :: name
  REAL(SDK),INTENT(IN) :: param(:,:,:,:)
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
    CALL init_ParamType_SDK_a4(newParam,thisname,param,description)

    !Add the new parameter to thisParam
    CALL add_ParamType(thisParam,prevname,newParam)
    CALL newParam%clear()
  ELSE
    CALL eParams%raiseError(modName//'::'//myName// &
        ' - parameter name "'//TRIM(name)// &
        '" already exists! Use set method or full parameter list path!')
  ENDIF
ENDSUBROUTINE add_ParamType_SDK_a4
!
!-------------------------------------------------------------------------------
!> @brief Initializes a ParamType object as a four dimensional array of 32-bit integer
!> @param thisParam the parameter to initialize
!> @param name the name of the parameter
!> @param param a four dimensional array of 32-bit integer
!> @param description an optional description for this parameter
!>
!> This routine is not recursive, so it is like setting a four dimensional array of parameter.
!> Therefore the name cannot contain the "->" symbol to indicate access to a
!> sub-list. @c thisParam must not already be inititalized.
!>
SUBROUTINE init_ParamType_SNK_a4(thisParam,name,param,description)
  CHARACTER(LEN=*),PARAMETER :: myName='init_ParamType_SNK_a4'
  CLASS(ParamType),INTENT(INOUT) :: thisParam
  INTEGER(SNK),INTENT(IN) :: param(:,:,:,:)
  CHARACTER(LEN=*),INTENT(IN) :: name
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
  INTEGER(SIK) :: ipos

  IF(.NOT.ASSOCIATED(thisParam%pdat)) THEN
    !Check that '->' character is not in name
    ipos=INDEX(name,'->')
    IF(ipos == 0) THEN
      ALLOCATE(ParamType_SNK_a4 :: thisParam%pdat)
      thisParam%pdat%name=TRIM(name)
      IF(PRESENT(description)) thisParam%pdat%description=TRIM(description)
      thisParam%pdat%dataType='4-D ARRAY INTEGER(SNK)'
      SELECTTYPE(p=>thisParam%pdat)
      TYPE IS(ParamType_SNK_a4)
        ALLOCATE(p%val(SIZE(param,1),SIZE(param,2),SIZE(param,3),SIZE(param,4)))
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
ENDSUBROUTINE init_ParamType_SNK_a4
!
!-------------------------------------------------------------------------------
!> @brief Edits a four dimensional array of 32-bit integer valued parameter
!> @param thisParam the four dimensional array of 32-bit integer valued parameter to edit
!> @param funit the unit number to edit the parameter to
!> @param indent optional indicates the number of blank spaces to precede the
!>        beginning of text to edit.
!>
!> The formatted write uses the "general" edit descriptor so that 7 digits (four
!> more than the significant number in a 32-bit integer) are always
!> printed if the number is very large in absolute value engineering format
!> is used otherwise floating point form is used to write the value.
!>
SUBROUTINE edit_ParamType_SNK_a4(thisParam,funit,indent,prefix,paddtw)
  CLASS(ParamType_SNK_a4),INTENT(IN) :: thisParam
  INTEGER(SIK),INTENT(IN) :: funit
  INTEGER(SIK),INTENT(IN),OPTIONAL :: indent
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: prefix
  LOGICAL(SBK),INTENT(IN),OPTIONAL :: paddtw
  CHARACTER(LEN=12) :: fmt,fmt2,fmt3
  CHARACTER(LEN=:),ALLOCATABLE :: dtype
  INTEGER(SIK) :: i,j,k,l,m,n
  TYPE(StringType) :: sprefix,sdtype

  i=1
  j=6
  IF(PRESENT(indent)) i=i+indent
  IF(PRESENT(prefix)) sprefix=prefix
  sdtype=thisParam%datatype
  IF(PRESENT(paddtw)) THEN
    IF(paddtw) THEN
      ALLOCATE(CHARACTER(PARAM_MAX_DAT_LEN) :: dtype)
      dtype=CHAR(thisParam%dataType)
      sdtype=dtype
    ENDIF
  ENDIF
  WRITE(fmt,'(i12)') i; fmt=ADJUSTL(fmt)
  IF(LEN_TRIM(thisParam%description) == 0) THEN
    WRITE(UNIT=funit,FMT='('//TRIM(fmt)//'x,a)') sprefix// &
        sdtype//' :: '//thisParam%name//'= ...'
  ELSE
    WRITE(UNIT=funit,FMT='('//TRIM(fmt)//'x,a,a)') sprefix// &
        sdtype//' :: '//thisParam%name//'= ... !'//thisParam%description
  ENDIF
  j=j+LEN(sdtype)+LEN(thisParam%name)
  WRITE(fmt2,'(i12)') j; fmt2=ADJUSTL(fmt2)
  WRITE(fmt3,'(i12)') SIZE(thisParam%val,1); fmt3=ADJUSTL(fmt3)
  DO n=1,SIZE(thisParam%val,4)
    DO k=1,SIZE(thisParam%val,3)
      DO l=1,SIZE(thisParam%val,2)
        WRITE(UNIT=funit,FMT='('//TRIM(fmt)//'x,'//TRIM(fmt2)//'x,'// &
            TRIM(fmt3)//'(g13.7))') (thisParam%val(m,l,k,n),m=1,SIZE(thisParam%val,1))
      ENDDO
    ENDDO
  ENDDO
ENDSUBROUTINE edit_ParamType_SNK_a4
!
!-------------------------------------------------------------------------------
!> @brief Clears a four dimensional array of 32-bit integer valued parameter
!> @param thisParam the four dimensional array of 32-bit integer valued parameter to clear
!>
SUBROUTINE clear_ParamType_SNK_a4(thisParam)
  CLASS(ParamType_SNK_a4),INTENT(INOUT) :: thisParam
  DEALLOCATE(thisParam%val)
  thisParam%name=''
  thisParam%dataType=''
  thisParam%description=''
ENDSUBROUTINE clear_ParamType_SNK_a4
!
!-------------------------------------------------------------------------------
!> @brief Sets the value of an existing four dimensional array of 32-bit integer valued
!> parameter to a new value.
!> @param thisParam the parameter in which an existing parameter with name
!>        matching @c name will be to set the new value of @c param
!> @param name the name of an existing parameter to set the value of
!> @param param the new value to set for the parameter
!> @param description an optional new description for the parameter identified
!>        by @c name
!>
!> If a parameter with @c name is not found an error is produced. If the
!> parameter with @c name is not a four dimensional array of 32-bit integer valued parameter
!> then an error is produced.
!>
SUBROUTINE set_ParamType_SNK_a4(thisParam,name,param,description)
  CHARACTER(LEN=*),PARAMETER :: myName='set_ParamType_SNK_a4'
  CLASS(ParamType),INTENT(INOUT) :: thisParam
  CHARACTER(LEN=*),INTENT(IN) :: name
  INTEGER(SNK),INTENT(IN) :: param(:,:,:,:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
  CLASS(ParamType),POINTER :: tmpParam

  SELECTTYPE(thisParam)
  TYPE IS(ParamType_SNK_a4)
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
      TYPE IS(ParamType_SNK_a4)
        p%val=param
        IF(PRESENT(description)) p%description=TRIM(description)
      CLASS DEFAULT
        CALL eParams%raiseError(modName//'::'//myName// &
            ' - parameter data type mismatch! Parameter '//TRIM(name)//' type is '// &
            tmpParam%dataType//' and must be 4-D ARRAY INTEGER(SNK)!')
      ENDSELECT
    ELSE
      CALL eParams%raiseError(modName//'::'//myName// &
          ' - unable to locate parameter "'//TRIM(name)//'" in "'// &
          thisParam%name//'"!')
    ENDIF
  ENDSELECT
ENDSUBROUTINE set_ParamType_SNK_a4
!
!-------------------------------------------------------------------------------
!> @brief Gets the four dimensional array of 32-bit integer value for a specified parameter
!> @param thisParam the parameter in which an existing parameter with name
!>        matching @c name will have it's value returned
!> @param name the name of the parameter to return the value of
!> @param val the current value of the parameter with @c name
!>
!> If a parameter with @c name is not found an error is produced. If the
!> parameter with @c name is not a four dimensional array of 32-bit integer valued parameter
!> then an error is produced.
!>
SUBROUTINE get_ParamType_SNK_a4(thisParam,name,val)
  CHARACTER(LEN=*),PARAMETER :: myName='get_ParamType_SNK_a4'
  CLASS(ParamType),INTENT(IN) :: thisParam
  CHARACTER(LEN=*),INTENT(IN) :: name
  INTEGER(SNK),ALLOCATABLE,INTENT(INOUT) :: val(:,:,:,:)
  CLASS(ParamType),POINTER :: tmpParam

  SELECTTYPE(thisParam)
  TYPE IS(ParamType_SNK_a4)
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
      TYPE IS(ParamType_SNK_a4)
        val=p%val
      CLASS DEFAULT
        CALL eParams%raiseError(modName//'::'//myName// &
            ' - parameter data type mismatch! Parameter '//TRIM(name)//' type is '// &
            tmpParam%dataType//' and must be 4-D ARRAY INTEGER(SNK)!')
      ENDSELECT
    ELSE
      CALL eParams%raiseError(modName//'::'//myName// &
          ' - unable to locate parameter "'//TRIM(name)//'" in "'// &
          thisParam%name//'"!')
    ENDIF
  ENDSELECT
ENDSUBROUTINE get_ParamType_SNK_a4
!
!-------------------------------------------------------------------------------
!> @brief Adds a new four dimensional array of 32-bit integer valued parameter to a set of
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
SUBROUTINE add_ParamType_SNK_a4(thisParam,name,param,description)
  CHARACTER(LEN=*),PARAMETER :: myName='add_ParamType_SNK_a4'
  CLASS(ParamType),INTENT(INOUT) :: thisParam
  CHARACTER(LEN=*),INTENT(IN) :: name
  INTEGER(SNK),INTENT(IN) :: param(:,:,:,:)
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
    CALL init_ParamType_SNK_a4(newParam,thisname,param,description)

    !Add the new parameter to thisParam
    CALL add_ParamType(thisParam,prevname,newParam)
    CALL newParam%clear()
  ELSE
    CALL eParams%raiseError(modName//'::'//myName// &
        ' - parameter name "'//TRIM(name)// &
        '" already exists! Use set method or full parameter list path!')
  ENDIF
ENDSUBROUTINE add_ParamType_SNK_a4
!
!-------------------------------------------------------------------------------
!> @brief Initializes a ParamType object as a four dimensional array of 64-bit integer
!> @param thisParam the parameter to initialize
!> @param name the name of the parameter
!> @param param a four dimensional array of 64-bit integer
!> @param description an optional description for this parameter
!>
!> This routine is not recursive, so it is like setting a four dimensional array of parameter.
!> Therefore the name cannot contain the "->" symbol to indicate access to a
!> sub-list. @c thisParam must not already be inititalized.
!>
SUBROUTINE init_ParamType_SLK_a4(thisParam,name,param,description)
  CHARACTER(LEN=*),PARAMETER :: myName='init_ParamType_SLK_a4'
  CLASS(ParamType),INTENT(INOUT) :: thisParam
  INTEGER(SLK),INTENT(IN) :: param(:,:,:,:)
  CHARACTER(LEN=*),INTENT(IN) :: name
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
  INTEGER(SIK) :: ipos

  IF(.NOT.ASSOCIATED(thisParam%pdat)) THEN
    !Check that '->' character is not in name
    ipos=INDEX(name,'->')
    IF(ipos == 0) THEN
      ALLOCATE(ParamType_SLK_a4 :: thisParam%pdat)
      thisParam%pdat%name=TRIM(name)
      IF(PRESENT(description)) thisParam%pdat%description=TRIM(description)
      thisParam%pdat%dataType='4-D ARRAY INTEGER(SLK)'
      SELECTTYPE(p=>thisParam%pdat)
      TYPE IS(ParamType_SLK_a4)
        ALLOCATE(p%val(SIZE(param,1),SIZE(param,2),SIZE(param,3),SIZE(param,4)))
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
ENDSUBROUTINE init_ParamType_SLK_a4
!
!-------------------------------------------------------------------------------
!> @brief Edits a four dimensional array of 64-bit integer valued parameter
!> @param thisParam the four dimensional array of 64-bit integer valued parameter to edit
!> @param funit the unit number to edit the parameter to
!> @param indent optional indicates the number of blank spaces to precede the
!>        beginning of text to edit.
!>
!> The formatted write uses the "general" edit descriptor so that 7 digits (four
!> more than the significant number in a 64-bit integer) are always
!> printed if the number is very large in absolute value engineering format
!> is used otherwise floating point form is used to write the value.
!>
SUBROUTINE edit_ParamType_SLK_a4(thisParam,funit,indent,prefix,paddtw)
  CLASS(ParamType_SLK_a4),INTENT(IN) :: thisParam
  INTEGER(SIK),INTENT(IN) :: funit
  INTEGER(SIK),INTENT(IN),OPTIONAL :: indent
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: prefix
  LOGICAL(SBK),INTENT(IN),OPTIONAL :: paddtw
  CHARACTER(LEN=12) :: fmt,fmt2,fmt3
  CHARACTER(LEN=:),ALLOCATABLE :: dtype
  INTEGER(SIK) :: i,j,k,l,m,n
  TYPE(StringType) :: sprefix,sdtype

  i=1
  j=6
  IF(PRESENT(indent)) i=i+indent
  IF(PRESENT(prefix)) sprefix=prefix
  sdtype=thisParam%datatype
  IF(PRESENT(paddtw)) THEN
    IF(paddtw) THEN
      ALLOCATE(CHARACTER(PARAM_MAX_DAT_LEN) :: dtype)
      dtype=CHAR(thisParam%dataType)
      sdtype=dtype
    ENDIF
  ENDIF
  WRITE(fmt,'(i12)') i; fmt=ADJUSTL(fmt)
  IF(LEN_TRIM(thisParam%description) == 0) THEN
    WRITE(UNIT=funit,FMT='('//TRIM(fmt)//'x,a)') sprefix// &
        sdtype//' :: '//thisParam%name//'= ...'
  ELSE
    WRITE(UNIT=funit,FMT='('//TRIM(fmt)//'x,a,a)') sprefix// &
        sdtype//' :: '//thisParam%name//'= ... !'//thisParam%description
  ENDIF
  j=j+LEN(sdtype)+LEN(thisParam%name)
  WRITE(fmt2,'(i12)') j; fmt2=ADJUSTL(fmt2)
  WRITE(fmt3,'(i12)') SIZE(thisParam%val,1); fmt3=ADJUSTL(fmt3)
  DO n=1,SIZE(thisParam%val,4)
    DO k=1,SIZE(thisParam%val,3)
      DO l=1,SIZE(thisParam%val,2)
        WRITE(UNIT=funit,FMT='('//TRIM(fmt)//'x,'//TRIM(fmt2)//'x,'// &
            TRIM(fmt3)//'(g20.14))') (thisParam%val(m,l,k,n),m=1,SIZE(thisParam%val,1))
      ENDDO
    ENDDO
  ENDDO
ENDSUBROUTINE edit_ParamType_SLK_a4
!
!-------------------------------------------------------------------------------
!> @brief Clears a four dimensional array of 64-bit integer valued parameter
!> @param thisParam the four dimensional array of 64-bit integer valued parameter to clear
!>
SUBROUTINE clear_ParamType_SLK_a4(thisParam)
  CLASS(ParamType_SLK_a4),INTENT(INOUT) :: thisParam
  DEALLOCATE(thisParam%val)
  thisParam%name=''
  thisParam%dataType=''
  thisParam%description=''
ENDSUBROUTINE clear_ParamType_SLK_a4
!
!-------------------------------------------------------------------------------
!> @brief Sets the value of an existing four dimensional array of 64-bit integer valued
!> parameter to a new value.
!> @param thisParam the parameter in which an existing parameter with name
!>        matching @c name will be to set the new value of @c param
!> @param name the name of an existing parameter to set the value of
!> @param param the new value to set for the parameter
!> @param description an optional new description for the parameter identified
!>        by @c name
!>
!> If a parameter with @c name is not found an error is produced. If the
!> parameter with @c name is not a four dimensional array of 64-bit integer valued parameter
!> then an error is produced.
!>
SUBROUTINE set_ParamType_SLK_a4(thisParam,name,param,description)
  CHARACTER(LEN=*),PARAMETER :: myName='set_ParamType_SLK_a4'
  CLASS(ParamType),INTENT(INOUT) :: thisParam
  CHARACTER(LEN=*),INTENT(IN) :: name
  INTEGER(SLK),INTENT(IN) :: param(:,:,:,:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
  CLASS(ParamType),POINTER :: tmpParam

  SELECTTYPE(thisParam)
  TYPE IS(ParamType_SLK_a4)
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
      TYPE IS(ParamType_SLK_a4)
        p%val=param
        IF(PRESENT(description)) p%description=TRIM(description)
      CLASS DEFAULT
        CALL eParams%raiseError(modName//'::'//myName// &
            ' - parameter data type mismatch! Parameter '//TRIM(name)//' type is '// &
            tmpParam%dataType//' and must be 4-D ARRAY INTEGER(SLK)!')
      ENDSELECT
    ELSE
      CALL eParams%raiseError(modName//'::'//myName// &
          ' - unable to locate parameter "'//TRIM(name)//'" in "'// &
          thisParam%name//'"!')
    ENDIF
  ENDSELECT
ENDSUBROUTINE set_ParamType_SLK_a4
!
!-------------------------------------------------------------------------------
!> @brief Gets the four dimensional array of 64-bit integer value for a specified parameter
!> @param thisParam the parameter in which an existing parameter with name
!>        matching @c name will have it's value returned
!> @param name the name of the parameter to return the value of
!> @param val the current value of the parameter with @c name
!>
!> If a parameter with @c name is not found an error is produced. If the
!> parameter with @c name is not a four dimensional array of 64-bit integer valued parameter
!> then an error is produced.
!>
SUBROUTINE get_ParamType_SLK_a4(thisParam,name,val)
  CHARACTER(LEN=*),PARAMETER :: myName='get_ParamType_SLK_a4'
  CLASS(ParamType),INTENT(IN) :: thisParam
  CHARACTER(LEN=*),INTENT(IN) :: name
  INTEGER(SLK),ALLOCATABLE,INTENT(INOUT) :: val(:,:,:,:)
  CLASS(ParamType),POINTER :: tmpParam

  SELECTTYPE(thisParam)
  TYPE IS(ParamType_SLK_a4)
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
      TYPE IS(ParamType_SLK_a4)
        val=p%val
      CLASS DEFAULT
        CALL eParams%raiseError(modName//'::'//myName// &
            ' - parameter data type mismatch! Parameter '//TRIM(name)//' type is '// &
            tmpParam%dataType//' and must be 4-D ARRAY INTEGER(SLK)!')
      ENDSELECT
    ELSE
      CALL eParams%raiseError(modName//'::'//myName// &
          ' - unable to locate parameter "'//TRIM(name)//'" in "'// &
          thisParam%name//'"!')
    ENDIF
  ENDSELECT
ENDSUBROUTINE get_ParamType_SLK_a4
!
!-------------------------------------------------------------------------------
!> @brief Adds a new four dimensional array of 64-bit integer valued parameter to a set of
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
SUBROUTINE add_ParamType_SLK_a4(thisParam,name,param,description)
  CHARACTER(LEN=*),PARAMETER :: myName='add_ParamType_SLK_a4'
  CLASS(ParamType),INTENT(INOUT) :: thisParam
  CHARACTER(LEN=*),INTENT(IN) :: name
  INTEGER(SLK),INTENT(IN) :: param(:,:,:,:)
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
    CALL init_ParamType_SLK_a4(newParam,thisname,param,description)

    !Add the new parameter to thisParam
    CALL add_ParamType(thisParam,prevname,newParam)
    CALL newParam%clear()
  ELSE
    CALL eParams%raiseError(modName//'::'//myName// &
        ' - parameter name "'//TRIM(name)// &
        '" already exists! Use set method or full parameter list path!')
  ENDIF
ENDSUBROUTINE add_ParamType_SLK_a4
!
!-------------------------------------------------------------------------------
RECURSIVE SUBROUTINE procXMLTree(thisParam,parent,currentPath)
  CLASS(ParamType),INTENT(INOUT) :: thisParam
  TYPE(StringType),INTENT(IN) :: currentPath
  TYPE(XMLElementType),POINTER :: iXMLE,children(:),parent
  TYPE(ParamType),POINTER :: pList(:)
  TYPE(StringType) :: elname,tmpStr,typval,attrVal,nameVal,tmpPath
  INTEGER(SIK) :: ic

  LOGICAL(SBK) :: boolVal
  INTEGER(SIK) :: intVal
  REAL(SSK) :: singleVal
  REAL(SDK) :: doubleVal
  INTEGER(SIK),ALLOCATABLE :: intArry(:)
  REAL(SDK),ALLOCATABLE :: doubleArry(:)
  TYPE(StringType),ALLOCATABLE :: strArry(:)

  NULLIFY(pList)
  CALL parent%getChildren(children)
  !Check to see if it's an empty parameter list
  IF(.NOT.ASSOCIATED(children)) THEN
    ALLOCATE(pList(0))
    CALL thisParam%add(CHAR(currentPath),pList)
    DEALLOCATE(pList)
    RETURN
  ENDIF

  DO ic=1,SIZE(children)
    tmpPath=currentPath//' -> '
    iXMLE => children(ic)

    elname=iXMLE%name%upper()
    IF(elname == 'PARAMETER') THEN
      tmpStr='type'
      CALL iXMLE%getAttributeValue(tmpStr,typval)
      typval = typval%upper()
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
      CASE('FLOAT')
        singleVal=CHAR(attrVal)
        CALL thisParam%add(CHAR(tmpPath),singleVal,'XML_IN_VAL='//attrval)
      CASE('DOUBLE')
        doubleVal=CHAR(attrVal)
        CALL thisParam%add(CHAR(tmpPath),doubleVal,'XML_IN_VAL='//attrval)
      CASE('STRING')
        CALL thisParam%add(CHAR(tmpPath),attrVal)
      CASE('ARRAY(INT)')
        CALL char_to_int_array(intArry,CHAR(attrVal))
        CALL thisParam%add(CHAR(tmpPath),intArry)
      CASE('ARRAY(DOUBLE)')
        CALL char_to_double_array(doubleArry,CHAR(attrVal))
        CALL thisParam%add(CHAR(tmpPath),doubleArry,'XML_IN_VAL='//attrval)
      CASE('ARRAY(STRING)')
        CALL char_to_string_array(strArry,CHAR(attrVal))
        CALL thisParam%add(CHAR(tmpPath),strArry)
        strArry=''
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
RECURSIVE SUBROUTINE procFMUXMLTree(thisParam,parent,currentPath)
  CLASS(ParamType),INTENT(INOUT) :: thisParam
  TYPE(StringType),INTENT(IN) :: currentPath
  TYPE(XMLElementType),POINTER :: iXMLE,children(:),dChildren(:),parent
  TYPE(ParamType),POINTER :: pList(:)
  TYPE(StringType) :: elname,tmpPath,tmpNewPath
  INTEGER(SIK) :: ic,ia,ib

  TYPE(StringType),ALLOCATABLE :: tmpKeys(:)
  TYPE(StringType),ALLOCATABLE :: tmpValues(:)
  TYPE(StringType) :: tmpKey, tmpVal, tmpPathToTmpVar

  NULLIFY(pList)
  CALL parent%getChildren(children)
  !Check to see if it's an empty parameter list
  IF(.NOT.ASSOCIATED(children)) THEN
    ALLOCATE(pList(0))
    CALL thisParam%add(CHAR(currentPath),pList)
    DEALLOCATE(pList)
    RETURN
  ENDIF

  DO ic=1,SIZE(children)
    tmpPath=currentPath//' -> '
    iXMLE => children(ic)
    elname=iXMLE%name%upper()
    tmpNewPath = tmpPath // elname
    IF(elname == 'SCALARVARIABLE') THEN
      CALL iXMLE%getAttributes(tmpKeys, tmpValues)
      DO ia=1,SIZE(tmpKeys)
        tmpKey = tmpKeys(ia)
        tmpVal = tmpValues(ia)
        IF(tmpKey=='name') THEN
          DO ib=1,SIZE(tmpKeys)
            IF(tmpKeys(ib)=='valueReference') THEN
              tmpPathToTmpVar = 'FMU'//currentPath//' -> '//tmpVal//' -> valueReference'
              IF(thisParam%has(CHAR(tmpPathToTmpVar))) THEN
                CALL eParams%raiseWarning(modName//" - Duplicate FMU Variable: "//CHAR(tmpPathToTmpVar))
                CALL thisParam%set(CHAR(tmpPathToTmpVar),tmpValues(ib))
              ELSE
                CALL thisParam%add(CHAR(tmpPathToTmpVar),tmpValues(ib))
              ENDIF
            ELSE IF(tmpKeys(ib)=='causality') THEN
              tmpPathToTmpVar = 'FMU'//currentPath//' -> '//tmpVal//' -> causality'
              CALL thisParam%add(CHAR(tmpPathToTmpVar),tmpValues(ib))
            ENDIF
          ENDDO
        ENDIF
      ENDDO
      DEALLOCATE(tmpKeys)
      DEALLOCATE(tmpValues)
    ELSE IF(elname == 'DEFAULTEXPERIMENT') THEN
      CALL iXMLE%getAttributes(tmpKeys, tmpValues)
      DO ia=1,SIZE(tmpKeys)
        tmpPathToTmpVar = elname//currentPath//' -> '//tmpKeys(ia)
        CALL thisParam%add(CHAR(tmpPathToTmpVar),tmpValues(ia))
      ENDDO
    ELSE IF(elname == 'COSIMULATION') THEN
      CALL iXMLE%getAttributes(tmpKeys, tmpValues)
      DO ia=1,SIZE(tmpKeys)
        tmpPathToTmpVar = elname//currentPath//' -> '//tmpKeys(ia)
        CALL thisParam%add(CHAR(tmpPathToTmpVar),tmpValues(ia))
      ENDDO
    ELSE IF(elname == 'MODELVARIABLES') THEN
      CALL procFMUXMLTree(thisParam,iXMLE,tmpNewPath)
    ELSE IF(elname == 'MODELSTRUCTURE') THEN
      CALL iXMLE%getChildren(dChildren)
      ! Check for empty parameterlist
      IF(ASSOCIATED(dChildren)) THEN
        CALL procFMUXMLTree(thisParam,iXMLE,tmpNewPath)
      ENDIF
    ELSE IF(elname == 'DERIVATIVES') THEN
      ! Count number of children
      CALL iXMLE%getChildren(dChildren)
      IF(ASSOCIATED(dChildren)) THEN
        tmpPathToTmpVar = 'FMU'//currentPath//' -> '//' -> nDerivatives'
        CALL thisParam%add(CHAR(tmpPathToTmpVar),SIZE(dChildren))
      ENDIF
    ENDIF
  ENDDO

  IF(parent%name%upper() == 'FMIMODELDESCRIPTION') THEN
    tmpKey='guid'
    CALL parent%getAttributeValue(tmpKey,tmpVal)
    CALL thisParam%add(CHAR(tmpKey),tmpVal)
  ENDIF

ENDSUBROUTINE procFMUXMLTree
!
!-------------------------------------------------------------------------------
!> @brief Initilize a parameter list from an XML file
!> @param thisParam the parameter list to be populated from the XML file
!> @param fname the name of the input XML file
!> @param fmuXML_opt a flag to denote that the XML file is a Functional
!>        Mockup Unit (FMU) model description
!>
SUBROUTINE initFromXML(thisParam, fname, fmuXML_opt)
  CLASS(ParamType),INTENT(INOUT) :: thisParam
  CHARACTER(LEN=*),INTENT(IN) :: fname
  LOGICAL(SBK),INTENT(IN),OPTIONAL :: fmuXML_opt
  LOGICAL(SBK) :: fmuXML
  TYPE(StringType) :: tmpStr,nameVal
  TYPE(XMLFileType) :: xmlFile
  TYPE(XMLElementType),POINTER :: iXMLE
  TYPE(StringType) :: currentPath

  IF(.NOT. PRESENT(fmuXML_opt)) THEN
    fmuXML=.FALSE.
  ELSE
    fmuXML=fmuXML_opt
  ENDIF

  SELECTTYPE(thisParam); TYPE IS(ParamType)
    IF(.NOT.ASSOCIATED(thisParam%pdat)) THEN
      !Initialize the XML file
      CALL xmlfile%importFromDisk(fname)
      iXMLE => xmlfile%root
      tmpStr='name'
      CALL iXMLE%getAttributeValue(tmpStr,nameVal)
      currentPath=nameVal
      IF(fmuXML) THEN
        CALL procFMUXMLTree(thisParam,iXMLE,currentPath)
      ELSE
        CALL procXMLTree(thisParam,iXMLE,currentPath)
      ENDIF
      CALL xmlfile%clear()
    ENDIF
  CLASS DEFAULT
    !Wrong type
  ENDSELECT
ENDSUBROUTINE initFromXML
!
!-------------------------------------------------------------------------------
!> @brief Recrusive routine to create an XML tree from a parameter list
!> @param param the parameter to be converted
!> @param currPath the current Path in the parameter list
!> @param currElem the XML element to store the data of param
!>
RECURSIVE SUBROUTINE paramToXML(param,currPath,currElem)
  CHARACTER(LEN=*),PARAMETER :: myName='paramToXML'
  CLASS(ParamType),INTENT(IN) :: param
  ! TYPE(XMLElementType),POINTER,INTENT(IN) :: parent
  TYPE(StringType),INTENT(IN) :: currPath
  TYPE(XMLElementType),POINTER,INTENT(INOUT) :: currElem


  LOGICAL(SBK) :: bool0
  INTEGER(SIK) :: i,idx,nChildren
  REAL(SRK) :: doubleVal,oDoubleVal,singleVal,oSingleVal
  REAL(SRK),ALLOCATABLE :: doubleArry(:),oDoubleArry(:)
  TYPE(StringType),ALLOCATABLE :: str1(:)

  TYPE(StringType) :: addr,val,name,typename,tmpPath,oVal
  CLASS(ParamType),POINTER :: nextParam
  TYPE(XMLElementType),POINTER :: tmpChild,myChildren(:)

  name=TRIM(param%name)
  IF(.NOT. ASSOCIATED(currElem)) ALLOCATE(currElem)
  addr=TRIM('name')
  CALL currElem%setAttribute(addr,name)

  addr=currPath
  nChildren=0
  nextParam => NULL()
  CALL param%getSubParams(addr,nextParam)
  DO WHILE(ASSOCIATED(nextParam))
    nChildren=nChildren+1
    CALL param%getSubParams(addr,nextParam)
  ENDDO

  IF(nChildren > 0) THEN
    ALLOCATE(myChildren(nChildren))
    addr=currPath
    nextParam => NULL()
    CALL param%getSubParams(addr,nextParam)
    NULLIFY(tmpChild)
    DO i=1,nChildren
      tmpPath=TRIM(nextParam%name)
      tmpChild => myChildren(i)
      CALL paramToXML(nextParam,tmpPath,tmpChild)
      NULLIFY(tmpChild)
      CALL param%getSubParams(addr,nextParam)
    ENDDO
    name=TRIM('ParameterList')
    CALL currElem%setName(name)
    CALL currElem%setChildren(myChildren)
  ELSE
    !Get name and value from parameter list
    SELECTCASE(TRIM(param%dataType))
    CASE('LOGICAL(SBK)')
      typename='bool'
      CALL param%get(TRIM(param%name),bool0)
      IF(bool0) THEN
        val='true'
      ELSE
        val='false'
      ENDIF
    CASE('INTEGER(SNK)')
      typename='int'
      CALL param%getString(TRIM(param%name),val)
    CASE('REAL(SSK)')
      typename='float'
      idx=INDEX(param%description,'XML_IN_VAL=')
      IF(idx > 0) THEN
        idx=idx+11
        oVal = param%description%substr(idx,LEN_TRIM(param%description))
        oSingleVal=CHAR(oVal)
        CALL param%get(TRIM(param%name),singleVal)

        IF(singleVal == oSingleVal) THEN
          val=oVal !use original input string
        ELSE
          !output variable with same number of sig figs
          CALL param%getString(TRIM(param%name),val)
        ENDIF
      ELSE
        CALL param%getString(TRIM(param%name),val)
      ENDIF
    CASE('REAL(SDK)')
      typename='double'
      idx=INDEX(param%description,'XML_IN_VAL=')
      IF(idx > 0) THEN
        idx=idx+11
        oVal = param%description%substr(idx,LEN_TRIM(param%description))
        oDoubleVal=CHAR(oVal)
        CALL param%get(TRIM(param%name),doubleVal)

        IF(doubleVal == oDoubleVal) THEN
          val=oVal !use original input string
        ELSE
          !output variable with same number of sig figs
          CALL param%getString(TRIM(param%name),val)
        ENDIF
      ELSE
        CALL param%getString(TRIM(param%name),val)
      ENDIF
    CASE('TYPE(StringType)')
      typename='string'
      CALL param%getString(TRIM(param%name),val)
    CASE('1-D ARRAY INTEGER(SNK)')
      typename='Array(int)'
      CALL param%getString(TRIM(param%name),str1)
      CALL string_array_to_string(str1,val)
    CASE('1-D ARRAY REAL(SDK)')
      typename='Array(double)'
      idx=INDEX(param%description,'XML_IN_VAL=')
      IF(idx > 0) THEN
        idx=idx+11
        oVal = param%description%substr(idx,LEN_TRIM(param%description))
        CALL char_to_double_array(oDoubleArry,CHAR(oVal))
        CALL param%get(TRIM(param%name),doubleArry)

        IF(ALL(doubleArry == oDoubleArry)) THEN
          val=oVal !use original input string
        ELSE
          !output variable with same number of sig figs
          CALL param%getString(TRIM(param%name),str1)
          CALL string_array_to_string(str1,val)
        ENDIF
      ELSE
        CALL param%getString(TRIM(param%name),str1)
        CALL string_array_to_string(str1,val)
      ENDIF
    CASE('1-D ARRAY TYPE(StringType)')
      typename='Array(string)'
      CALL param%getString(TRIM(param%name),str1)
      CALL string_array_to_string(str1,val)
    CASE DEFAULT
      CALL eParams%raiseError('Invalid paramType in '//modName//'::'//myName// &
          ' - dataType '//TRIM(param%dataType)//' is not valid for XML output!')
    ENDSELECT
    name=TRIM('Parameter')
    CALL currElem%setName(name)
    addr=TRIM('type')
    CALL currElem%setAttribute(addr,typename)
    addr=TRIM('value')
    CALL currElem%setAttribute(addr,val)
  ENDIF
ENDSUBROUTINE paramToXML
!
!-------------------------------------------------------------------------------
!> @brief Creates an XML file from the input parameter list
!> @param thisParam the parameter list to be written into the XML file
!> @param fname the name of the output file
!>
SUBROUTINE editToXML_ParamType(thisParam,fname)
  CLASS(ParamType),INTENT(IN) :: thisParam
  CHARACTER(LEN=*),INTENT(IN) :: fname
  TYPE(XMLFileType) :: xmlFile
  TYPE(StringType) :: addr

  SELECTTYPE(thisParam); TYPE IS(ParamType)
    !Initialize the XML file
    addr=''
    xmlFile%style_sheet='PL9.xsl'
    CALL paramToXML(thisParam%pdat,addr,xmlFile%root)
    CALL xmlFile%exportToDisk(fname)
    CALL xmlFile%clear()
  CLASS DEFAULT
    !Wrong type
  ENDSELECT
ENDSUBROUTINE editToXML_ParamType
!
!-------------------------------------------------------------------------------
FUNCTION countArrayElts(charArr) RESULT(numElts)
  INTEGER(SIK) :: numElts
  CHARACTER(LEN=*),INTENT(IN) :: charArr
  INTEGER(SIK) :: i

  numElts=0
  !If length is 2, array is empty
  IF(LEN(charArr) > 2) THEN
    DO i=2,LEN(charArr)-1
      IF(charArr(i:i) == ',') THEN
        numElts=numElts+1
      ENDIF
    ENDDO
    numElts=numElts+1
  ENDIF
ENDFUNCTION countArrayElts
!
!-------------------------------------------------------------------------------
!> @brief Defines the operation for performing an assignment of a character
!> string to an array of integers
!> @param iArr the array of integers
!> @param c the character value
SUBROUTINE char_to_int_array(iArr,c)
  INTEGER(SIK),ALLOCATABLE,INTENT(OUT) :: iArr(:)
  CHARACTER(LEN=*),INTENT(IN) :: c
  CHARACTER(LEN=50) :: tmpStr
  INTEGER(SIK) :: tmpInt
  INTEGER(SIK) :: numElts
  INTEGER(SIK) :: i,j,k

  numElts=countArrayElts(c)
  !Empty array case
  IF(numElts == 0) THEN
    RETURN
  ENDIF

  j=0
  k=1 ! iArr index
  ALLOCATE(iArr(numElts))
  DO i=2,LEN(c)
    IF(c(i:i) /= ',' .AND. c(i:i) /= '}') THEN
      j=j+1
      tmpStr(j:j)=c(i:i)
    ELSE
      tmpStr=tmpStr(1:j)
      READ(tmpStr, '(I12)') tmpInt
      iArr(k:k)=tmpInt
      j=0
      k=k+1
    ENDIF
  ENDDO
ENDSUBROUTINE
!
!-------------------------------------------------------------------------------
!> @brief Defines the operation for performing an assignment of a character
!> string to an array of doubles
!> @param dArr the array of doubles
!> @param c the character value
SUBROUTINE char_to_double_array(dArr,c)
  REAL(SDK),ALLOCATABLE,INTENT(OUT) :: dArr(:)
  CHARACTER(LEN=*),INTENT(IN) :: c
  CHARACTER(LEN=50) :: tmpStr
  REAL(SDK) :: tmpDouble
  INTEGER(SIK) :: numElts
  INTEGER(SIK) :: i,j,k

  numElts=countArrayElts(c)
  !Empty array case
  IF(numElts == 0) THEN
    RETURN
  ENDIF

  j=0
  k=1 ! dArr index
  ALLOCATE(dArr(numElts))
  DO i=2,LEN(c)
    IF(c(i:i) /= ',' .AND. c(i:i) /= '}') THEN
      j=j+1
      tmpStr(j:j)=c(i:i)
    ELSE
      tmpStr=tmpStr(1:j)
      READ(tmpStr, '(F35.0)') tmpDouble
      dArr(k:k)=tmpDouble
      j=0
      k=k+1
    ENDIF
  ENDDO
ENDSUBROUTINE char_to_double_array
!
!-------------------------------------------------------------------------------
!> @brief Defines the operation for performing an assignment of a character
!> string to an array of strings
!> @param sArr the array of strings
!> @param c the character value
SUBROUTINE char_to_string_array(sArr,c)
  TYPE(StringType),ALLOCATABLE,INTENT(OUT) :: sArr(:)
  CHARACTER(LEN=*),INTENT(IN) :: c
  CHARACTER(LEN=LEN(c)) :: tmpStr
  TYPE(StringType) :: tmpElt
  INTEGER(SIK) :: numElts
  INTEGER(SIK) :: i,j,k

  numElts=countArrayElts(c)
  !Empty array case
  IF(numElts == 0) THEN
    RETURN
  ENDIF

  j=0
  k=1 ! sArr index
  ALLOCATE(sArr(numElts))
  DO i=2,LEN(c)
    IF(c(i:i) /= ',' .AND. c(i:i) /= '}') THEN
      j=j+1
      tmpStr(j:j)=c(i:i)
    ELSE
      tmpElt=tmpStr(1:j)
      sArr(k:k)=tmpElt
      j=0
      k=k+1
    ENDIF
  ENDDO
ENDSUBROUTINE char_to_string_array
!
!-------------------------------------------------------------------------------
!> @brief Defines the operation for performing an assignment of an array of
!> strings to a string single string(for XML output)
!> @param sArr the array of strings
!> @param str the string value
SUBROUTINE string_array_to_string(sArr,str)
  TYPE(StringType),INTENT(IN) :: sArr(:)
  TYPE(StringType),INTENT(OUT) :: str
  INTEGER(SIK) :: i,numElts

  numElts=SIZE(sArr)
  str=''
  IF(numElts == 0) RETURN

  str='{'
  DO i=1,numElts
    str=TRIM(str)//TRIM(sArr(i))
    IF(i < numElts) str=TRIM(str)//','
  ENDDO
  str=TRIM(str)//'}'
ENDSUBROUTINE string_array_to_string
!
ENDMODULE ParameterLists
