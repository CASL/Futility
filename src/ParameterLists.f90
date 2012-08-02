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
!> @brief 
!> 
!>
!> @par Module Dependencies
!>  - @ref IntrType "IntrType": @copybrief IntrType
!>  - @ref IntrType "Strings": @copybrief Strings
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
!>
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE ParameterLists
  USE ISO_FORTRAN_ENV
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
  PUBLIC :: ParamListType
  PUBLIC :: ASSIGNMENT(=)
  PUBLIC :: assign_ParamType
  
  CHARACTER(LEN=*),PARAMETER :: modName='PARAMETERLISTS'
  
  TYPE(ExceptionHandlerType),POINTER :: eParams => NULL()
  
  !> Derived type for an arbitrary length string
  TYPE :: ParamType
    TYPE(StringType) :: name
    TYPE(StringType) :: dataType
    TYPE(StringType) :: description
    CLASS(ParamType),POINTER :: pdat => NULL()
!
!List of type bound procedures
    CONTAINS
      PROCEDURE,PASS,PRIVATE :: initParamList => init_ParamType_List
      PROCEDURE,PASS,PRIVATE :: initSSK => init_ParamType_SSK
      GENERIC :: init => initParamList,initSSK
      PROCEDURE,PASS,PRIVATE :: setParamList => set_ParamType_List
      PROCEDURE,PASS,PRIVATE :: setSSK => set_ParamType_SSK
      GENERIC :: set => setParamList,setSSK
      PROCEDURE,PASS,PRIVATE :: getParam => get_ParamType
      PROCEDURE,PASS,PRIVATE :: getParamList => get_ParamType_List
      PROCEDURE,PASS,PRIVATE :: getSSK => get_ParamType_SSK
      GENERIC :: get => getParam,getParamList,getSSK
      PROCEDURE,PASS,PRIVATE :: addParam => add_ParamType
      PROCEDURE,PASS,PRIVATE :: addParamList => add_ParamType_List
      PROCEDURE,PASS,PRIVATE :: addParamSSK => add_ParamType_SSK
      GENERIC :: add => addParam,addParamList,addParamSSK
      PROCEDURE,PASS :: edit => edit_ParamType
      PROCEDURE,PASS :: clear => clear_ParamType
  ENDTYPE ParamType
  
  TYPE,EXTENDS(ParamType) :: ParamListType
    TYPE(ParamType),ALLOCATABLE :: plList(:)
    CONTAINS
      PROCEDURE,PASS :: edit => edit_ParamType_List
      PROCEDURE,PASS :: clear => clear_ParamType_List
  ENDTYPE ParamListType
  
  TYPE,EXTENDS(ParamType) :: ParamType_SSK
    REAL(SSK) :: val=0.0_SSK
    CONTAINS
      PROCEDURE,PASS :: edit => edit_ParamType_SSK
      PROCEDURE,PASS :: clear => clear_ParamType_SSK
  ENDTYPE ParamType_SSK
  
  INTERFACE ASSIGNMENT(=)
    MODULE PROCEDURE assign_ParamType
  ENDINTERFACE
!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
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
            TYPE IS(ParamListType)
              CALL thisParam%init(p%name%sPrint(),p%plList, &
                p%description%sPrint())
          ENDSELECT
        CLASS DEFAULT
          CALL eParams%raiseError(modName//'::'//myName// &
            ' - cannot assign parameter data to a type extension of ParamType!')
      ENDSELECT
      IF(localalloc) DEALLOCATE(eParams)
    ENDSUBROUTINE assign_ParamType
!
!-------------------------------------------------------------------------------
    RECURSIVE SUBROUTINE get_ParamType(thisParam,name,param)
      CLASS(ParamType),INTENT(IN) :: thisParam
      CHARACTER(LEN=*),INTENT(IN) :: name
      CLASS(ParamType),POINTER,INTENT(INOUT) :: param
      CHARACTER(LEN=LEN(name)) :: thisname,nextname,pname
      INTEGER(SIK) :: ipos,i
      CLASS(ParamType),POINTER :: tmpParam
      
      ipos=INDEX(name,'->')
      thisname=name
      nextname=''
      IF(ipos > 0) THEN
        thisname=ADJUSTL(name(1:ipos-1))
        nextname=ADJUSTL(name(ipos+2:LEN(name)))
      ENDIF
      pname=''
      
      param => NULL()
      SELECTTYPE(thisParam)
        TYPE IS(ParamListType)
          IF(LEN_TRIM(nextname) > 0) THEN
            !Set names to upper case for matching
            IF(LEN(pname) >= LEN_TRIM(thisParam%name)) pname=thisParam%name
            CALL toUPPER(pname)
            CALL toUPPER(thisname)
            
            !Search the list for nextname (thisname must match parameter name)
            IF(TRIM(pname) == TRIM(thisname)) THEN
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
    ENDSUBROUTINE get_ParamType
!
!-------------------------------------------------------------------------------
    RECURSIVE SUBROUTINE add_ParamType(thisParam,name,newParam)
      CHARACTER(LEN=*),PARAMETER :: myName='add_ParamType'
      CLASS(ParamType),INTENT(INOUT) :: thisParam
      CHARACTER(LEN=*),INTENT(IN) :: name
      CLASS(ParamType),INTENT(IN) :: newParam
      CHARACTER(LEN=LEN(name)) :: nextname,pname
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
              ALLOCATE(ParamListType :: thisParam%pdat)
              thisParam%pdat%datatype='TYPE(ParamListType)'
              
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
        TYPE IS(ParamListType)
          !Search for the parameter with name
          NULLIFY(tmpParam)
          IF(LEN_TRIM(name) > 0) THEN
            !Check if the name matches this list
            pname=thisParam%name
            nextname=name
            CALL toUPPER(nextname)
            CALL toUPPER(pname)
            IF(TRIM(pname) == TRIM(nextname)) THEN
              !The name refers to the list in thisParam
              CALL add_ParamType(thisParam,'',newParam)
            ELSE
              !Search within this list
              CALL thisParam%getParam(name,tmpParam)
              IF(ASSOCIATED(tmpParam)) THEN
                !Found parameter with matching name
                CALL add_ParamType(tmpParam,'',newParam)
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
          ALLOCATE(ParamListType :: thisParam%pdat)
          thisParam%pdat%name=TRIM(name)
          thisParam%pdat%dataType='TYPE(ParamListType)'
          IF(PRESENT(description)) thisParam%pdat%description=TRIM(description)
          SELECTTYPE(p=>thisParam%pdat); TYPE IS(ParamListType)
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
    RECURSIVE SUBROUTINE edit_ParamType_List(thisParam,funit,indent)
      CLASS(ParamListType),INTENT(IN) :: thisParam
      INTEGER(SIK),INTENT(IN) :: funit
      INTEGER(SIK),INTENT(IN),OPTIONAL :: indent
      CHARACTER(LEN=12) :: fmt
      INTEGER(SIK) :: i,j
      
      IF(ALLOCATED(thisParam%plList)) THEN
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
        
        DO j=1,SIZE(thisParam%plList)
          IF(ASSOCIATED(thisParam%plList(j)%pdat)) &
            CALL thisParam%plList(j)%pdat%edit(funit,i+3)
        ENDDO
      ENDIF
    ENDSUBROUTINE edit_ParamType_List
!
!-------------------------------------------------------------------------------
    SUBROUTINE clear_ParamType_List(thisParam)
      CLASS(ParamListType),INTENT(INOUT) :: thisParam
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
!------------ -------------------------------------------------------------------
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
        TYPE IS(ParamListType)
          IF(thisParam%name == TRIM(name)) THEN
            IF(PRESENT(description)) thisParam%description=TRIM(description)
            np=SIZE(thisParam%plList)
            IF(SIZE(paramlist) /= np) THEN
              !List lengths are unequal so choose the lesser
              CALL eParams%raiseWarning(modName//'::'//myName// &
                ' - parameter list lengths are unequal! '// &
                  'All parameters may not be set!')
              np=MIN(SIZE(paramlist),SIZE(thisParam%plList))
            ENDIF
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
              TYPE IS(ParamListType)
                IF(PRESENT(description)) p%description=TRIM(description)
                np=SIZE(p%plList)
                IF(SIZE(paramlist) /= np) THEN
                  !List lengths are unequal so choose the lesser
                  CALL eParams%raiseWarning(modName//'::'//myName// &
                    ' - parameter list lengths are unequal! '// &
                      'All parameters may not be set!')
                  np=MIN(SIZE(paramlist),SIZE(p%plList))
                ENDIF
                DO i=1,np
                  CALL assign_ParamType(p%plList(i),paramlist(i))
                ENDDO
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
    ENDSUBROUTINE set_ParamType_List
!
!-------------------------------------------------------------------------------
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
        TYPE IS(ParamListType)
          IF(thisParam%name == TRIM(name)) THEN
            np=SIZE(thisParam%plList)
            IF(SIZE(paramlist) /= np) THEN
              !List lengths are unequal so choose the lesser
              CALL eParams%raiseWarning(modName//'::'//myName// &
                ' - parameter list lengths are unequal! '// &
                  'All parameters may not be returned!')
              np=MIN(SIZE(paramlist),SIZE(thisParam%plList))
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
              TYPE IS(ParamListType)
                np=SIZE(p%plList)
                IF(SIZE(paramlist) /= np) THEN
                  !List lengths are unequal so choose the lesser
                  CALL eParams%raiseWarning(modName//'::'//myName// &
                    ' - parameter list lengths are unequal! '// &
                      'All parameters may not be returned!')
                  np=MIN(SIZE(paramlist),SIZE(p%plList))
                ENDIF
                DO i=1,np
                  CALL assign_ParamType(paramlist(i),p%plList(i))
                ENDDO
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
    ENDSUBROUTINE get_ParamType_List
!
!-------------------------------------------------------------------------------
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
    SUBROUTINE clear_ParamType_SSK(thisParam)
      CLASS(ParamType_SSK),INTENT(INOUT) :: thisParam
      thisParam%val=0.0_SSK
      thisParam%name=''
      thisParam%dataType=''
      thisParam%description=''
    ENDSUBROUTINE clear_ParamType_SSK
!
!-------------------------------------------------------------------------------
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
