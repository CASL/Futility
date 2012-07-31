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
  
  IMPLICIT NONE
  PRIVATE !Default private for module contents
!
! List of Public items
  PUBLIC :: eParams
  PUBLIC :: ParamType
  PUBLIC :: ParamListType
  PUBLIC :: ASSIGNMENT(=)
  
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
      PROCEDURE,PASS,PRIVATE :: addParamList => add_ParamType_List
      PROCEDURE,PASS,PRIVATE :: addParamSSK => add_ParamType_SSK
      GENERIC :: add => addParamList,addParamSSK
      PROCEDURE,PASS,PRIVATE :: setParamList => set_ParamType_List
      PROCEDURE,PASS,PRIVATE :: setSSK => set_ParamType_SSK
      GENERIC :: set => setParamList,setSSK
      PROCEDURE,PASS,PRIVATE :: getParam => get_ParamType
      PROCEDURE,PASS,PRIVATE :: getParamList => get_ParamType_List
      PROCEDURE,PASS,PRIVATE :: getSSK => get_ParamType_SSK
      GENERIC :: get => getParam,getParamList,getSSK
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
      CHARACTER(LEN=LEN(name)) :: thisname,nextname
      INTEGER(SIK) :: ipos,i
      
      ipos=INDEX(name,'->')
      thisname=name
      nextname=''
      IF(ipos > 0) THEN
        thisname=ADJUSTL(name(1:ipos-1))
        nextname=ADJUSTL(name(ipos+2:LEN(name)))
      ENDIF
      
      param => NULL()
      SELECTTYPE(thisParam)
        TYPE IS(ParamListType)
          IF(LEN_TRIM(nextname) > 0) THEN
            !Search the list for nextname (thisname must match parameter name)
            IF(thisParam%name == TRIM(thisname)) THEN
              DO i=1,SIZE(thisParam%plList)
                CALL thisParam%plList(i)%getParam(TRIM(nextname),param)
                IF(ASSOCIATED(param)) EXIT !Found it, stop searching
              ENDDO
            ENDIF
          ELSE
            !Search for thisname within the list
            DO i=1,SIZE(thisParam%plList)
              CALL thisParam%plList(i)%getParam(TRIM(thisname),param)
              IF(ASSOCIATED(param)) EXIT !Found it, stop searching
            ENDDO
          ENDIF
        CLASS DEFAULT
          IF(ASSOCIATED(thisParam%pdat)) THEN
            IF(thisParam%pdat%name == TRIM(name)) THEN
              !Found the match
              param => thisParam%pdat
            ELSE
              !Search 1-level down
              CALL thisParam%pdat%getParam(name,param)
            ENDIF
          ENDIF
      ENDSELECT
    ENDSUBROUTINE get_ParamType
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
        WRITE(UNIT=funit,FMT='('//TRIM(fmt)//'x,a)') &
        thisParam%dataType//' :: '//thisParam%name//'='
        
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
    RECURSIVE SUBROUTINE add_ParamType_List(thisParam,name,param,description)
      CHARACTER(LEN=*),PARAMETER :: myName='add_ParamType_SSK'
      CLASS(ParamType),INTENT(INOUT) :: thisParam
      CHARACTER(LEN=*),INTENT(IN) :: name
      TYPE(ParamType),INTENT(IN) :: param(:)
      CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
      CHARACTER(LEN=LEN(name)) :: thisname,nextname
      LOGICAL(SBK) :: lfound,localalloc
      INTEGER(SIK) :: ipos,i,nold
      TYPE(ParamType),ALLOCATABLE :: tmpList(:)
      
      localalloc=.FALSE.
      IF(.NOT.ASSOCIATED(eParams)) THEN
        localalloc=.TRUE.
        ALLOCATE(eParams)
      ENDIF
      
!      ipos=INDEX(name,'->')
!      thisname=name
!      nextname=''
!      IF(ipos > 0) THEN
!        thisname=name(1:ipos-1)
!        nextname=name(ipos+2:LEN(name))
!      ENDIF
!      
!      SELECTTYPE(thisParam)
!        TYPE IS(ParamListType) !thisParam is a parameter list
!          
!          IF(LEN_TRIM(nextname) > 0) THEN
!            !Search the list to match the name
!            lfound=.FALSE.
!            DO i=1,SIZE(thisParam%plList)
!              IF(thisParam%plList(i)%name == TRIM(thisname)) THEN
!                !Found the match, proceed down one level
!                lfound=.TRUE.
!                IF(PRESENT(description)) THEN
!                  CALL add_ParamType_List(thisParam%plList(i),nextname, &
!                    param,description)
!                ELSE
!                  CALL add_ParamType_List(thisParam%plList(i),nextname,param)
!                ENDIF
!                EXIT
!              ENDIF
!            ENDDO
!            
!            IF(.NOT.lfound) THEN
!              !The containing list does not exist, so add it and proceed
!              ALLOCATE(tmpList(1))
!              CALL add_ParamType_List(thisParam,thisname,tmpList)
!              IF(PRESENT(description)) THEN
!                CALL add_ParamType_List(thisParam,nextname,param,description)
!              ELSE
!                CALL add_ParamType_List(thisParam,nextname,param)
!              ENDIF
!            ENDIF
!          ELSE
!!
!!Add the parameter at this level
!            !Reallocate list
!            nold=SIZE(thisParam%plList)
!            CALL MOVE_ALLOC(thisParam%plList,tmpList)
!            ALLOCATE(thisParam%plList(nold+1))
!            
!            !Assign old data
!            DO i=1,nold
!              thisParam%plList(i)=tmpList(i)
!            ENDDO
!            
!            !Assign new parameter
!            IF(PRESENT(description)) THEN
!              CALL init_ParamType_List(thisParam%plList(nold+1), &
!                thisname,param,description)
!            ELSE
!              CALL init_ParamType_List(thisParam%plList(nold+1), &
!                thisname,param)
!            ENDIF
!          ENDIF
!        TYPE IS(ParamType)
!!
!!All data is in pdat at this level, call this routine again for pdat
!          IF(ASSOCIATED(thisParam%pdat)) THEN
!            IF(PRESENT(description)) THEN
!              CALL add_ParamType_List(thisParam%pdat,name,param,description)
!            ELSE
!              CALL add_ParamType_List(thisParam%pdat,name,param)
!            ENDIF
!          ENDIF
!        CLASS DEFAULT
!          CALL eParams%raiseError(modName//'::'//myName// &
!            ' - parameter type data mismatch! Parameter type is '// &
!              thisParam%pdat%dataType//' and needs to be TYPE(ParamListType)!')
!      ENDSELECT
!      IF(localalloc) DEALLOCATE(eParams)
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
      WRITE(UNIT=funit,FMT='('//TRIM(fmt)//'x,a,f9.6)') &
        thisParam%dataType//' :: '//thisParam%name//'=',thisParam%val
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
    RECURSIVE SUBROUTINE add_ParamType_SSK(thisParam,name,param,description)
      CHARACTER(LEN=*),PARAMETER :: myName='add_ParamType_SSK'
      CLASS(ParamType),INTENT(INOUT) :: thisParam
      CHARACTER(LEN=*),INTENT(IN) :: name
      REAL(SSK),INTENT(IN) :: param
      CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
      CHARACTER(LEN=LEN(name)) :: thisname,nextname
      LOGICAL(SBK) :: localalloc,lfound
      INTEGER(SIK) :: ipos,i,nold
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
        thisname=name(1:ipos-1)
        nextname=name(ipos+2:LEN(name))
      ENDIF
      
      SELECTTYPE(thisParam)
        TYPE IS(ParamListType) !thisParam is a parameter list
          
          IF(LEN_TRIM(nextname) > 0) THEN
            !Search the list to match the name
            lfound=.FALSE.
            DO i=1,SIZE(thisParam%plList)
              IF(thisParam%plList(i)%name == TRIM(thisname)) THEN
                !Found the match, proceed down one level
                lfound=.TRUE.
                IF(PRESENT(description)) THEN
                  CALL add_ParamType_SSK(thisParam%plList(i),nextname, &
                    param,description)
                ELSE
                  CALL add_ParamType_SSK(thisParam%plList(i),nextname,param)
                ENDIF
                EXIT
              ENDIF
            ENDDO
            
            IF(.NOT.lfound) THEN
              !The containing list does not exist, so add it and proceed
              ALLOCATE(tmpList(1))
              CALL add_ParamType_List(thisParam,thisname,tmpList)
              IF(PRESENT(description)) THEN
                CALL add_ParamType_SSK(thisParam,nextname,param,description)
              ELSE
                CALL add_ParamType_SSK(thisParam,nextname,param)
              ENDIF
            ENDIF
          ELSE
!
!Add the parameter at this level
            !Reallocate list
            nold=SIZE(thisParam%plList)
            CALL MOVE_ALLOC(thisParam%plList,tmpList)
            ALLOCATE(thisParam%plList(nold+1))
            
            !Assign old data
            DO i=1,nold
              thisParam%plList(i)=tmpList(i)
            ENDDO
            
            !Check that there is not a duplicate name
            
            
            !Assign new parameter
            IF(PRESENT(description)) THEN
              CALL init_ParamType_SSK(thisParam%plList(nold+1), &
                thisname,param,description)
            ELSE
              CALL init_ParamType_SSK(thisParam%plList(nold+1), &
                thisname,param)
            ENDIF
          ENDIF
        TYPE IS(ParamType)
!
!All data is in pdat at this level, call this routine again for pdat
          IF(ASSOCIATED(thisParam%pdat)) THEN
            IF(PRESENT(description)) THEN
              CALL add_ParamType_SSK(thisParam%pdat,name,param,description)
            ELSE
              CALL add_ParamType_SSK(thisParam%pdat,name,param)
            ENDIF
          ELSE
            CALL eParams%raiseError(modName//'::'//myName// &
              ' - parameter is not initialized! Use init method!')
          ENDIF
        CLASS DEFAULT
          CALL eParams%raiseError(modName//'::'//myName// &
            ' - parameter data type mismatch! Parameter type is '// &
              thisParam%pdat%dataType//' and needs to be TYPE(ParamListType)!')
      ENDSELECT
      IF(localalloc) DEALLOCATE(eParams)
    ENDSUBROUTINE add_ParamType_SSK
!
ENDMODULE ParameterLists
