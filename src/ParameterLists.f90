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
!>  - 2-D arrays of logicals
!>  - 2-D arrays of 32-bit integers
!>  - 2-D arrays of 64-bit integers
!>  - 2-D arrays of single precision reals
!>  - 2-D arrays of double precision reals
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
!> @author Brendan Kochunas
!>   @date 07/26/2012
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE ParameterLists
  USE IntrType
  USE Strings
  USE ExceptionHandler
  USE IO_Strings
  
  IMPLICIT NONE
  PRIVATE !Default private for module contents
!
! List of Public items
  PUBLIC :: eParams
  PUBLIC :: ParamType
  PUBLIC :: ASSIGNMENT(=)
  
  !> The module name
  CHARACTER(LEN=*),PARAMETER :: modName='PARAMETERLISTS'
  
  !> Exception handler for the module
  TYPE(ExceptionHandlerType),POINTER :: eParams => NULL()
  
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
      !> Generic type bound interface for all @c init operations
      GENERIC :: init => initParamList,initSSK
      !> @copybrief ParameterLists::set_ParamType_List
      !> @copydoc ParameterLists::set_ParamType_List
      PROCEDURE,PASS,PRIVATE :: setParamList => set_ParamType_List
      !> @copybrief ParameterLists::set_ParamType_SSK
      !> @copydoc ParameterLists::set_ParamType_SSK
      PROCEDURE,PASS,PRIVATE :: setSSK => set_ParamType_SSK
      !> Generic type bound interface for all @c set operations
      GENERIC :: set => setParamList,setSSK
      !> @copybrief ParameterLists::get_ParamType
      !> @copydoc ParameterLists::get_ParamType
      PROCEDURE,PASS,PRIVATE :: getParam => get_ParamType
      !> @copybrief ParameterLists::get_ParamType_List
      !> @copydoc ParameterLists::get_ParamType_List
      PROCEDURE,PASS,PRIVATE :: getParamList => get_ParamType_List
      !> @copybrief ParameterLists::get_ParamType_SSK
      !> @copydoc ParameterLists::get_ParamType_SSK
      PROCEDURE,PASS,PRIVATE :: getSSK => get_ParamType_SSK
      !> Generic type bound interface for all @c get operations
      GENERIC :: get => getParam,getParamList,getSSK
      !> @copybrief ParameterLists::add_ParamType
      !> @copydoc ParameterLists::add_ParamType
      PROCEDURE,PASS,PRIVATE :: addParam => add_ParamType
      !> @copybrief ParameterLists::add_ParamType_List
      !> @copydoc ParameterLists::add_ParamType_List
      PROCEDURE,PASS,PRIVATE :: addParamList => add_ParamType_List
      !> @copybrief ParameterLists::add_ParamType_SSK
      !> @copydoc ParameterLists::add_ParamType_SSK
      PROCEDURE,PASS,PRIVATE :: addParamSSK => add_ParamType_SSK
      !> Generic type bound interface for all @c add operations
      GENERIC :: add => addParam,addParamList,addParamSSK
      !> @copybrief ParameterLists::remove_ParamType
      !> @copydoc ParameterLists::remove_ParamType
      PROCEDURE,PASS :: remove => remove_ParamType
      !> @copybrief ParameterLists::validate_ParamType
      !> @copydoc ParameterLists::validate_ParamType
      PROCEDURE,PASS :: validate => validate_ParamType
      !> @copybrief ParameterLists::edit_ParamType
      !> @copydoc ParameterLists::edit_ParamType
      PROCEDURE,PASS :: edit => edit_ParamType
      !> @copybrief ParameterLists::clear_ParamType
      !> @copydoc ParameterLists::clear_ParamType
      PROCEDURE,PASS :: clear => clear_ParamType
  ENDTYPE ParamType
  
  !> @brief Extended type of a ParamType for defining a list of parameters
  TYPE,EXTENDS(ParamType) :: ParamType_List
    !> The list of parameters
    TYPE(ParamType),ALLOCATABLE :: plList(:)
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
  
  !> Generic interface adds a procedure to overload the intrinsic assignment
  !> operator with the given procedure
  INTERFACE ASSIGNMENT(=)
    !> @copybrief ParameterLists::assign_ParamType
    !> @copydoc ParameterLists::assign_ParamType
    MODULE PROCEDURE assign_ParamType
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
      LOGICAL(SBK) :: localalloc
      
      localalloc=.FALSE.
      IF(.NOT.ASSOCIATED(eParams)) THEN
        localalloc=.TRUE.
        ALLOCATE(eParams)
      ENDIF
      
      SELECTTYPE(thisParam)
        TYPE IS(ParamType)
          IF(ASSOCIATED(thisParam%pdat)) CALL thisParam%clear()
          SELECTTYPE(p=>param)
            TYPE IS(ParamType)
              !Assign the parameter value using a recursive call
              IF(ASSOCIATED(p%pdat)) CALL assign_ParamType(thisParam,p%pdat)
            TYPE IS(ParamType_SSK)
              CALL thisParam%init(p%name%sPrint(),p%val, &
                p%description%sPrint())
            TYPE IS(ParamType_List)
              IF(ALLOCATED(p%plList)) THEN
                CALL thisParam%init(p%name%sPrint(),p%plList, &
                  p%description%sPrint())
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
      IF(localalloc) DEALLOCATE(eParams)
    ENDSUBROUTINE assign_ParamType
!
!-------------------------------------------------------------------------------
!> @brief Returns a pointer to a parameter whose name matches the given input
!> name.
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
      CLASS(ParamType),INTENT(IN) :: thisParam
      CHARACTER(LEN=*),INTENT(IN) :: name
      CLASS(ParamType),POINTER,INTENT(INOUT) :: param
      CHARACTER(LEN=LEN(name)) :: thisname,nextname,pname
      LOGICAL(SBK) :: localalloc
      INTEGER(SIK) :: ipos,i
      CLASS(ParamType),POINTER :: tmpParam
      
      localalloc=.FALSE.
      IF(.NOT.ASSOCIATED(eParams)) THEN
        localalloc=.TRUE.
        ALLOCATE(eParams)
      ENDIF
      
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
            IF(LEN_TRIM(nextname) > 0) THEN
              !Set names to upper case for matching
              IF(LEN(pname) >= LEN_TRIM(thisParam%name)) pname=thisParam%name
              CALL toUPPER(pname)
              CALL toUPPER(thisname)
            
              !Search the list for nextname (thisname must match parameter name)
              IF(TRIM(pname) == TRIM(thisname) .AND. &
                ALLOCATED(thisParam%plList)) THEN
                DO i=1,SIZE(thisParam%plList)
                  CALL thisParam%plList(i)%getParam(TRIM(nextname),param)
                  IF(ASSOCIATED(param)) EXIT !Found it, stop searching
                ENDDO
              ENDIF
            ELSE
              !Search for thisname within the list
              IF(ALLOCATED(thisParam%plList)) THEN
                DO i=1,SIZE(thisParam%plList)
                  CALL thisParam%plList(i)%getParam(TRIM(thisname),param)
                  IF(ASSOCIATED(param)) EXIT !Found it, stop searching
                ENDDO
              ENDIF
            ENDIF
          CLASS DEFAULT
            IF(ASSOCIATED(thisParam%pdat)) THEN
              !Set names to upper case for matching
              IF(LEN(pname) >= LEN_TRIM(thisParam%pdat%name)) &
                pname=thisParam%pdat%name
              CALL toUPPER(pname)
              CALL toUPPER(thisname)
              IF(TRIM(pname) == TRIM(thisname)) THEN
                !Found the match
                tmpParam => thisParam%pdat
                IF(LEN_TRIM(nextname) > 0) THEN
                  CALL tmpParam%getParam(name,param)
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
                  CALL tmpParam%getParam(nextname,param)
                ENDIF
              ENDIF
            ENDIF
        ENDSELECT
      ELSE
        CALL eParams%raiseError(modName//'::'//myName// &
          ' - cannot search for a blank name!')
      ENDIF
      IF(localalloc) DEALLOCATE(eParams)
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
      CHARACTER(LEN=LEN(name)) :: thisname,nextname,pname
      LOGICAL(SBK) :: localalloc
      INTEGER(SIK) :: ipos,i,np
      TYPE(ParamType),ALLOCATABLE :: tmpList(:)
      CLASS(ParamType),POINTER :: tmpParam
      
      localalloc=.FALSE.
      IF(.NOT.ASSOCIATED(eParams)) THEN
        localalloc=.TRUE.
        ALLOCATE(eParams)
      ENDIF
      
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
              !The name refers to the list in thisParam
              CALL add_ParamType(thisParam,TRIM(nextname),newParam)
            ELSE
              !Search for thisname within this list
              NULLIFY(tmpParam)
              CALL thisParam%getParam(TRIM(thisname),tmpParam)
              IF(ASSOCIATED(tmpParam)) THEN
                !Found parameter with matching name
                CALL add_ParamType(tmpParam,TRIM(nextname),newParam)
              ELSE
                !Create a new entry in the list for the new parameter
                IF(ALLOCATED(thisParam%plList)) THEN
                  np=SIZE(thisParam%plList)
              
                  !Copy the parameter list to a temporary
                  ALLOCATE(tmpList(np))
                  DO i=1,np
                    CALL assign_ParamType(tmpList(i),thisParam%plList(i))
                    CALL thisParam%plList(i)%clear()
                  ENDDO
              
                  !Reallocate the parameter list and copy everything back
                  DEALLOCATE(thisParam%plList)
                  ALLOCATE(thisParam%plList(np+1))
                  DO i=1,np
                    CALL assign_ParamType(thisParam%plList(i),tmpList(i))
                    CALL tmpList(i)%clear()
                  ENDDO
                  DEALLOCATE(tmpList)
                  i=np+1
                ELSE
                  !Allocate the list to 1 element
                  ALLOCATE(thisParam%plList(1))
                  i=1
                ENDIF
            
                !Make recursive call to add the parameter in the new empty parameter
                CALL add_ParamType(thisParam%plList(i),name,newParam)
              ENDIF
            ENDIF
          ELSE
            !Create a new entry in the list for the new parameter
            IF(ALLOCATED(thisParam%plList)) THEN
              np=SIZE(thisParam%plList)
              
              !Copy the parameter list to a temporary
              ALLOCATE(tmpList(np))
              DO i=1,np
                CALL assign_ParamType(tmpList(i),thisParam%plList(i))
                CALL thisParam%plList(i)%clear()
              ENDDO
              
              !Reallocate the parameter list and copy everything back
              DEALLOCATE(thisParam%plList)
              ALLOCATE(thisParam%plList(np+1))
              DO i=1,np
                CALL assign_ParamType(thisParam%plList(i),tmpList(i))
                CALL tmpList(i)%clear()
              ENDDO
              DEALLOCATE(tmpList)
              i=np+1
            ELSE
              !Allocate the list to 1 element
              ALLOCATE(thisParam%plList(1))
              i=1
            ENDIF
            
            !Make recursive call to add the parameter in the new empty parameter
            CALL add_ParamType(thisParam%plList(i),name,newParam)
          ENDIF
        CLASS DEFAULT
          CALL eParams%raiseError(modName//'::'//myName// &
            ' - cannot add parameter to type "'//thisParam%datatype//'"!')
      ENDSELECT
      IF(localalloc) DEALLOCATE(eParams)
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
      LOGICAL(SBK) :: localalloc
      INTEGER(SIK) :: i,ipos,np,npnew
      TYPE(ParamType),ALLOCATABLE :: tmpList(:)
      
      localalloc=.FALSE.
      IF(.NOT.ASSOCIATED(eParams)) THEN
        localalloc=.TRUE.
        ALLOCATE(eParams)
      ENDIF
      
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
                IF(ALLOCATED(thisParam%plList)) THEN
                  DO i=1,SIZE(thisParam%plList)
                    !Try to remove the next name
                    IF(ASSOCIATED(thisParam%plList(i)%pdat)) THEN
                      CALL remove_ParamType(thisParam%plList(i),TRIM(nextname))
                      IF(.NOT.ASSOCIATED(thisParam%plList(i)%pdat)) EXIT !success
                    ENDIF
                  ENDDO
                ENDIF
              ELSE
                !Try another level down, this is not so efficient because
                !there is no way to tell in which element the name might've
                !been matched.
                IF(ALLOCATED(thisParam%plList)) THEN
                  DO i=1,SIZE(thisParam%plList)
                    IF(ASSOCIATED(thisParam%plList(i)%pdat)) THEN
                      SELECTTYPE(p=>thisParam%plList(i)%pdat)
                         TYPE IS(ParamType_List); CALL remove_ParamType(p,name)
                      ENDSELECT
                    ENDIF
                  ENDDO
                ENDIF
              ENDIF
            ELSE
              !Search for thisname within the list
              IF(ALLOCATED(thisParam%plList)) THEN
                DO i=1,SIZE(thisParam%plList)
                  !Try to remove the next name
                  IF(ASSOCIATED(thisParam%plList(i)%pdat)) THEN
                    CALL remove_ParamType(thisParam%plList(i),TRIM(thisname))
                    IF(.NOT.ASSOCIATED(thisParam%plList(i)%pdat)) EXIT !success
                  ENDIF
                ENDDO
              ENDIF
            ENDIF
            
            !Garbage collection, shrink the current list to remove
            !empty values
            IF(ALLOCATED(thisParam%plList)) THEN
              
              !Create temporary
              np=SIZE(thisParam%plList)
              ALLOCATE(tmpList(np))
              
              !Copy to temporary
              npnew=0
              DO i=1,np
                IF(ASSOCIATED(thisParam%plList(i)%pdat)) THEN
                  npnew=npnew+1
                  CALL assign_ParamType(tmpList(npnew),thisParam%plList(i))
                  CALL thisParam%plList(i)%clear()
                ENDIF
              ENDDO
              
              !Reallocate list
              DEALLOCATE(thisParam%plList)
              IF(npnew > 0) THEN
                ALLOCATE(thisParam%plList(npnew))
              
                !Copy non-empty values back to list
                DO i=1,npnew
                  CALL assign_ParamType(thisParam%plList(i),tmpList(i))
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
      IF(localalloc) DEALLOCATE(eParams)
    ENDSUBROUTINE remove_ParamType
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
!> @brief Searches a parameter for a set of required parameters and determines
!> if all the required parameters are present and of the correct type.
!> @param thisParam the parameter to validate against reqParams
!> @param reqParams the set of required parameters that must appear in 
!>        @c thisParam
!> @param prefix a prefix path for the parameter's full path name
!> @returns isValid logical indicating that all the required parameters exist
!>          in @c thisParam and are of the correct type. 
!>
    RECURSIVE FUNCTION validateReq_ParamType(thisParam,reqParams,prefix) &
      RESULT(isValid)
      CHARACTER(LEN=*),PARAMETER :: myName='validateReq_ParamType'
      CLASS(ParamType),INTENT(INOUT) :: thisParam
      CLASS(ParamType),INTENT(IN) :: reqParams
      CHARACTER(LEN=*),INTENT(IN) :: prefix
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
          IF(ASSOCIATED(p%pdat)) &
            isValid=validateReq_ParamType(thisParam,p%pdat,prefix)
        TYPE IS(ParamType_List)
          !Loop over all parameters in the list and check each
          IF(ALLOCATED(p%plList)) THEN
            ntrue=0
            DO i=1,SIZE(p%plList)
              IF(validateReq_ParamType(thisParam,p%plList(i), &
                prefix//p%name//'->')) ntrue=ntrue+1
            ENDDO
            IF(ntrue == SIZE(p%plList)) isValid=.TRUE.
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
!> @brief Searches a parameter for a set of optional parameters when an optional
!> parameter is present is also checks the type. If an optional parameter
!> is not present or has the wrong type it is reset with the default value.
!> @param thisParam
!> @param optParams
!> @param prefix
!>
   RECURSIVE SUBROUTINE validateOpt_Paramtype(thisParam,optParams,prefix)
      CHARACTER(LEN=*),PARAMETER :: myName='validateOpt_Paramtype'
      CLASS(ParamType),INTENT(INOUT) :: thisParam
      CLASS(ParamType),INTENT(IN) :: optParams
      CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: prefix
      INTEGER(SIK) :: i,nprefix
      CLASS(ParamType),POINTER :: tmpParam
      
      nprefix=LEN(prefix)
      IF('->' == prefix(LEN(prefix)-1:LEN(prefix))) &
        nprefix=LEN(prefix)-2
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
          IF(ALLOCATED(p%plList)) THEN
            DO i=1,SIZE(p%plList)
              CALL validateOpt_Paramtype(thisParam,p%plList(i), &
                prefix//p%name//'->')
            ENDDO
          ELSE
            !The optional list is not allocated, which means we do not
            !have any default values for it's possible subparameters, but we
            !must at least check that the list exists
            CALL thisParam%getParam(prefix//p%name,tmpParam)
            IF(.NOT.ASSOCIATED(tmpParam)) THEN
              CALL eParams%raiseWarning(modName//'::'//myName// &
                ' - Failed to locate optional parameter "'//prefix// &
                  p%name//'"!')
              CALL eParams%raiseInformation(modName//'::'//myName// &
                ' - Optional parameter "'//prefix//p%name// &
                  '" is being added with no default value!')
              
              CALL add_ParamType(thisParam,prefix(1:nprefix),p)
            ELSE
              IF(.NOT.SAME_TYPE_AS(tmpParam,p)) THEN
                CALL eParams%raiseWarning(modName//'::'//myName// &
                  ' - Optional parameter "'//prefix//p%name//'" has type "'// &
                    tmpParam%dataType//'" and should be type "'//p%dataType// &
                      '"!')
                CALL eParams%raiseInformation(modName//'::'//myName// &
                  ' - Optional parameter "'//prefix//p%name// &
                    '" has no default value so it will remain unset!')
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
            CALL eParams%raiseWarning(modName//'::'//myName// &
              ' - Failed to locate optional parameter "'//prefix//p%name//'"!')
            CALL eParams%raiseInformation(modName//'::'//myName// &
              ' - Optional parameter "'//prefix//p%name// &
                '" is being added with default value.')
            CALL add_ParamType(thisParam,prefix(1:nprefix),p)
          ELSE
            IF(.NOT.SAME_TYPE_AS(tmpParam,p)) THEN
              CALL eParams%raiseWarning(modName//'::'//myName// &
                ' - Optional parameter "'//prefix//p%name//'" has type "'// &
                  tmpParam%dataType//'" and should be type "'//p%dataType//'"!')
              CALL eParams%raiseInformation(modName//'::'//myName// &
                ' - Optional parameter "'//prefix//p%name// &
                  '" is being overriden with default value.')
              CALL remove_ParamType(thisParam,prefix//p%name)
              CALL add_ParamType(thisParam,prefix(1:nprefix),p)
            ENDIF
          ENDIF
      ENDSELECT
   ENDSUBROUTINE validateOpt_Paramtype
!
!-------------------------------------------------------------------------------
!> @brief Compares a list to another list and reports any extra parameters
!> that are in the first list and not the second list.
!> @param thisParam
!> @param optParams
!> @param prefix
!>
    RECURSIVE SUBROUTINE checkExtras_Paramtype(thisParam,reqParams,optParams,prefix)
      CHARACTER(LEN=*),PARAMETER :: myName='checkExtras_Paramtype'
      CLASS(ParamType),INTENT(INOUT) :: thisParam
      CLASS(ParamType),INTENT(IN) :: reqParams
      CLASS(ParamType),INTENT(IN) :: optParams
      CHARACTER(LEN=*),INTENT(IN) :: prefix
      INTEGER(SIK) :: i
      CLASS(ParamType),POINTER :: tmpParam
      
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
              IF(ALLOCATED(tmpParam%plList)) THEN
                !The list in reqParams is allocated so check if the
                !subparameters in this list are extraneous
                IF(ALLOCATED(p%plList)) THEN
                  DO i=1,SIZE(p%plList)
                    CALL checkExtras_Paramtype(p%plList(i),reqParams, &
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
                IF(ALLOCATED(tmpParam%plList)) THEN
                  !The list in optParams is allocated so check if the
                  !subparameters in this list are extraneous
                  IF(ALLOCATED(p%plList)) THEN
                    DO i=1,SIZE(p%plList)
                      CALL checkExtras_Paramtype(p%plList(i),reqParams, &
                        optParams,prefix//p%name//'->')
                    ENDDO
                  ENDIF
                ENDIF
              ENDSELECT
            ELSE
              CALL eParams%raiseWarning(modName//'::'//myName// &
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
          IF(.NOT.ASSOCIATED(tmpParam)) &
            CALL eParams%raiseWarning(modName//'::'//myName// &
              ' - Possible extraneous parameter "'//prefix//p%name// &
                '" is not present in the reference list!')
      ENDSELECT
    ENDSUBROUTINE checkExtras_Paramtype
!
!-------------------------------------------------------------------------------
!> @brief 
!> @param thisParam
!> @param reqParams
!> @param optParams
!>
    SUBROUTINE validate_Paramtype(thisParam,reqParams,optParams)
      CLASS(ParamType),INTENT(INOUT) :: thisParam
      CLASS(ParamType),INTENT(IN) :: reqParams
      CLASS(ParamType),INTENT(IN),OPTIONAL :: optParams
      LOGICAL(SBK) :: isValid,localalloc
      TYPE(ParamType) :: nullParam
      
      localalloc=.FALSE.
      IF(.NOT.ASSOCIATED(eParams)) THEN
        localalloc=.TRUE.
        ALLOCATE(eParams)
      ENDIF
      
      isValid=validateReq_ParamType(thisParam,reqParams,'')
      IF(isValid) THEN
        IF(PRESENT(optParams)) THEN
          CALL validateOpt_Paramtype(thisParam,optParams,'')
          CALL checkExtras_Paramtype(thisParam,reqParams,optParams,'')
        ELSE
          CALL checkExtras_Paramtype(thisParam,reqParams,nullParam,'')
        ENDIF
      ENDIF
      IF(localalloc) DEALLOCATE(eParams)
    ENDSUBROUTINE validate_Paramtype
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
      LOGICAL(SBK) :: localalloc
      INTEGER(SIK) :: ipos,i
      
      localalloc=.FALSE.
      IF(.NOT.ASSOCIATED(eParams)) THEN
        localalloc=.TRUE.
        ALLOCATE(eParams)
      ENDIF
      
      IF(.NOT.ASSOCIATED(thisParam%pdat)) THEN
        !Check that '->' character is not in name
        ipos=INDEX(name,'->')
        IF(ipos == 0) THEN
          ALLOCATE(ParamType_List :: thisParam%pdat)
          thisParam%pdat%name=TRIM(name)
          thisParam%pdat%dataType='TYPE(ParamType_List)'
          IF(PRESENT(description)) thisParam%pdat%description=TRIM(description)
          SELECTTYPE(p=>thisParam%pdat); TYPE IS(ParamType_List)
            ALLOCATE(p%plList(SIZE(param)))
            DO i=1,SIZE(param)
              CALL assign_ParamType(p%plList(i),param(i))
            ENDDO
          ENDSELECT
        ELSE
          CALL eParams%raiseError(modName//'::'//myName// &
            ' - "->" character is not allowed in name!')
        ENDIF
      ELSE
        CALL eParams%raiseError(modName//'::'//myName// &
          ' - parameter '//thisParam%name//' is already initialized!'// &
            ' Use set method instead!')
      ENDIF
      IF(localalloc) DEALLOCATE(eParams)
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
      IF(ALLOCATED(thisParam%plList)) THEN  
        DO j=1,SIZE(thisParam%plList)
          IF(ASSOCIATED(thisParam%plList(j)%pdat)) &
            CALL thisParam%plList(j)%pdat%edit(funit,i+3)
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
      IF(ALLOCATED(thisParam%plList)) THEN
        DO i=1,SIZE(thisParam%plList)
          CALL thisParam%plList(i)%clear()
        ENDDO
        DEALLOCATE(thisParam%plList)
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
      LOGICAL(SBK) :: localalloc
      INTEGER(SIK) :: np,i
      CLASS(ParamType),POINTER :: tmpParam
      
      localalloc=.FALSE.
      IF(.NOT.ASSOCIATED(eParams)) THEN
        localalloc=.TRUE.
        ALLOCATE(eParams)
      ENDIF
      
      SELECTTYPE(thisParam)
        TYPE IS(ParamType_List)
          IF(thisParam%name == TRIM(name)) THEN
            IF(PRESENT(description)) thisParam%description=TRIM(description)
            
            IF(ALLOCATED(thisParam%plList)) THEN
              !Clear the existing list
              DO i=1,SIZE(thisParam%plList)
                CALL thisParam%plList(i)%clear()
              ENDDO
              DEALLOCATE(thisParam%plList)
            ENDIF
            
            !Assign the new list
            np=SIZE(paramlist)
            ALLOCATE(thisParam%plList(np))
            DO i=1,np
              CALL assign_ParamType(thisParam%plList(i),paramlist(i))
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
                IF(ALLOCATED(p%plList)) THEN
                  !Clear the existing list
                  DO i=1,SIZE(p%plList)
                    CALL p%plList(i)%clear()
                  ENDDO
                  DEALLOCATE(p%plList)
                ENDIF
            
                !Assign the new list
                np=SIZE(paramlist)
                ALLOCATE(p%plList(np))
                DO i=1,np
                  CALL assign_ParamType(p%plList(i),paramlist(i))
                ENDDO
              CLASS DEFAULT
                CALL eParams%raiseError(modName//'::'//myName// &
                  ' - parameter data type mismatch! Parameter type is '// &
                    tmpParam%dataType//' and must be TYPE(ParamType_List)!')
            ENDSELECT
          ELSE
            CALL eParams%raiseError(modName//'::'//myName// &
              ' - unable to locate parameter "'//TRIM(name)//'" in "'// &
                thisParam%name//'"!')
          ENDIF
      ENDSELECT
      IF(localalloc) DEALLOCATE(eParams)
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
      LOGICAL(SBK) :: localalloc
      INTEGER(SIK) :: i,np
      CLASS(ParamType),POINTER :: tmpParam
      
      localalloc=.FALSE.
      IF(.NOT.ASSOCIATED(eParams)) THEN
        localalloc=.TRUE.
        ALLOCATE(eParams)
      ENDIF
      
      SELECTTYPE(thisParam)
        TYPE IS(ParamType_List)
          IF(thisParam%name == TRIM(name)) THEN
            np=SIZE(thisParam%plList)
            IF(SIZE(paramlist) < np) THEN
              !List lengths are unequal so choose the lesser
              CALL eParams%raiseWarning(modName//'::'//myName// &
                ' - parameter list lengths are unequal! '// &
                  'All parameters may not be returned!')
              np=SIZE(paramlist)
            ENDIF
            DO i=1,np
              CALL assign_ParamType(paramlist(i),thisParam%plList(i))
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
                np=SIZE(p%plList)
                IF(SIZE(paramlist) < np) THEN
                  !List lengths are unequal so choose the lesser
                  CALL eParams%raiseWarning(modName//'::'//myName// &
                    ' - parameter list lengths are unequal! '// &
                      'All parameters may not be returned!')
                  np=SIZE(paramlist)
                ENDIF
                DO i=1,np
                  CALL assign_ParamType(paramlist(i),p%plList(i))
                ENDDO
              CLASS DEFAULT
                CALL eParams%raiseError(modName//'::'//myName// &
                  ' - parameter data type mismatch! Parameter type is '// &
                    tmpParam%dataType//' and must be TYPE(ParamType_List)!')
            ENDSELECT
          ELSE
            CALL eParams%raiseError(modName//'::'//myName// &
              ' - unable to locate parameter "'//TRIM(name)//'" in "'// &
                thisParam%name//'"!')
          ENDIF
      ENDSELECT
      IF(localalloc) DEALLOCATE(eParams)
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
      LOGICAL(SBK) :: localalloc
      INTEGER(SIK) :: ipos
      TYPE(ParamType) :: newParam
      CLASS(ParamType),POINTER :: tmpParam
      
      localalloc=.FALSE.
      IF(.NOT.ASSOCIATED(eParams)) THEN
        localalloc=.TRUE.
        ALLOCATE(eParams)
      ENDIF
      
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
      IF(localalloc) DEALLOCATE(eParams)
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
      REAL(SSK) :: param
      CHARACTER(LEN=*),INTENT(IN) :: name
      CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
      LOGICAL(SBK) :: localalloc
      INTEGER(SIK) :: ipos
      
      localalloc=.FALSE.
      IF(.NOT.ASSOCIATED(eParams)) THEN
        localalloc=.TRUE.
        ALLOCATE(eParams)
      ENDIF
      
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
      IF(localalloc) DEALLOCATE(eParams)
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
      LOGICAL(SBK) :: localalloc
      CLASS(ParamType),POINTER :: tmpParam
      
      localalloc=.FALSE.
      IF(.NOT.ASSOCIATED(eParams)) THEN
        localalloc=.TRUE.
        ALLOCATE(eParams)
      ENDIF
      
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
                  ' - parameter data type mismatch! Parameter type is '// &
                    tmpParam%dataType//' and must be REAL(SSK)!')
            ENDSELECT
          ELSE
            CALL eParams%raiseError(modName//'::'//myName// &
              ' - unable to locate parameter "'//TRIM(name)//'" in "'// &
                thisParam%name//'"!')
          ENDIF
      ENDSELECT
      IF(localalloc) DEALLOCATE(eParams)
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
      LOGICAL(SBK) :: localalloc
      CLASS(ParamType),POINTER :: tmpParam
      
      localalloc=.FALSE.
      IF(.NOT.ASSOCIATED(eParams)) THEN
        localalloc=.TRUE.
        ALLOCATE(eParams)
      ENDIF
      
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
                  ' - parameter data type mismatch! Parameter type is '// &
                    tmpParam%dataType//' and must be REAL(SSK)!')
            ENDSELECT
          ELSE
            CALL eParams%raiseError(modName//'::'//myName// &
              ' - unable to locate parameter "'//TRIM(name)//'" in "'// &
                thisParam%name//'"!')
          ENDIF
      ENDSELECT
      IF(localalloc) DEALLOCATE(eParams)
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
      LOGICAL(SBK) :: localalloc
      INTEGER(SIK) :: ipos
      TYPE(ParamType) :: newParam
      CLASS(ParamType),POINTER :: tmpParam
      
      localalloc=.FALSE.
      IF(.NOT.ASSOCIATED(eParams)) THEN
        localalloc=.TRUE.
        ALLOCATE(eParams)
      ENDIF
      
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
      IF(localalloc) DEALLOCATE(eParams)
    ENDSUBROUTINE add_ParamType_SSK
!
ENDMODULE ParameterLists
