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
      PROCEDURE,PASS,PRIVATE :: initParam => init_ParamType
      PROCEDURE,PASS,PRIVATE :: initParamList => init_ParamListType
      PROCEDURE,PASS,PRIVATE :: initSSK => init_ParamType_SSK
      GENERIC :: init => initParam,initParamList,initSSK
      PROCEDURE,PASS,PRIVATE :: setParam => set_ParamType
      PROCEDURE,PASS,PRIVATE :: setParamList => set_ParamListType
      PROCEDURE,PASS,PRIVATE :: setSSK => set_ParamType_SSK
      GENERIC :: set => setParam,setParamList,setSSK
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
      PROCEDURE,PASS,PRIVATE :: getParam => get_ParamListType
      PROCEDURE,PASS :: edit => edit_ParamListType
      PROCEDURE,PASS :: clear => clear_ParamListType
  ENDTYPE ParamListType
  
  TYPE,EXTENDS(ParamType) :: ParamType_SSK
    REAL(SSK) :: val=0.0_SSK
    CONTAINS
      PROCEDURE,PASS :: edit => edit_ParamType_SSK
      PROCEDURE,PASS :: clear => clear_ParamType_SSK
  ENDTYPE ParamType_SSK
!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
    SUBROUTINE init_ParamType(thisParam,param,name,description)
      CHARACTER(LEN=*),PARAMETER :: myName='init_ParamType'
      CLASS(ParamType),INTENT(INOUT) :: thisParam
      CLASS(ParamType),TARGET,INTENT(IN) :: param
      CHARACTER(LEN=*),INTENT(IN) :: name
      CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
      CHARACTER(LEN=LEN(name)) :: thisname,nextname
      LOGICAL(SBK) :: localalloc
      INTEGER(SIK) :: ipos
      
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
      
      IF(.NOT.ASSOCIATED(thisParam%pdat)) THEN
        thisParam%name=TRIM(thisname)
        IF(LEN_TRIM(nextname) > 0) THEN
          !The parameter is declared more than one level down so create
          !the intermediate level
          ALLOCATE(ParamListType :: thisParam%pdat)
          thisParam%dataType='TYPE(ParamListType)'
          SELECTTYPE(p=>thisParam%pdat); TYPE IS(ParamListType)
            IF(PRESENT(description)) THEN
              !CALL p%add(param,TRIM(nextname),TRIM(description))
            ELSE
              !CALL p%add(param,TRIM(nextname))
            ENDIF
          ENDSELECT
        ELSE
          !Assign the parameter at this level
          IF(PRESENT(description)) thisParam%description=TRIM(description)
          thisParam%pdat => param
          thisParam%dataType='CLASS(ParamType)'
        ENDIF
      ELSE
        IF(LEN_TRIM(nextname) > 0) THEN
          IF(thisParam%name == thisname) THEN
            SELECTTYPE(p=>thisParam%pdat)
              TYPE IS(ParamListType)
                IF(PRESENT(description)) THEN
                  !CALL p%add(param,TRIM(nextname),TRIM(description))
                ELSE
                  !CALL p%add(param,TRIM(nextname))
                ENDIF
              CLASS DEFAULT
                CALL eParams%raiseError(modName//'::'//myName// &
                ' - parameter type data mismatch! Parameter type is '// &
                  thisParam%dataType//' and must be TYPE(ParamListType)!')
            ENDSELECT
          ELSE
            CALL eParams%raiseError(modName//'::'//myName// &
              ' - parameter name mistmatch! Use add to create new parameter "'// &
                TRIM(thisName)//'"')
          ENDIF
        ELSE
          CALL eParams%raiseError(modName//'::'//myName// &
            ' - parameter '//thisParam%name//' is already initialized!'// &
              ' Use set method instead!')
        ENDIF
      ENDIF
      IF(localalloc) DEALLOCATE(eParams)
    ENDSUBROUTINE init_ParamType
!
!-------------------------------------------------------------------------------
    SUBROUTINE set_ParamType(thisParam,param,name,description)
      CHARACTER(LEN=*),PARAMETER :: myName='init_ParamType'
      CLASS(ParamType),INTENT(INOUT) :: thisParam
      CLASS(ParamType),TARGET,INTENT(IN) :: param
      CHARACTER(LEN=*),INTENT(IN) :: name
      CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
      CHARACTER(LEN=LEN(name)) :: thisname,nextname
      LOGICAL(SBK) :: localalloc
      INTEGER(SIK) :: ipos
      
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
      
      IF(ASSOCIATED(thisParam%pdat)) THEN
        IF(thisParam%name == thisname) THEN
          IF(LEN_TRIM(nextname) > 0) THEN
            SELECTTYPE(p=>thisParam%pdat)
              TYPE IS(ParamListType)
                IF(PRESENT(description)) THEN
                  !CALL p%setListParam(param,TRIM(nextname),TRIM(description))
                ELSE
                  !CALL p%add(param,TRIM(nextname))
                ENDIF
              CLASS DEFAULT
                CALL eParams%raiseError(modName//'::'//myName// &
                ' - parameter type data mismatch! Parameter type is '// &
                  thisParam%dataType//' and must be TYPE(ParamListType)!')
            ENDSELECT
          ELSE
            IF(PRESENT(description)) thisParam%description=TRIM(description)
            CALL thisParam%pdat%clear()
            thisParam%pdat => param
          ENDIF
        ELSE
          CALL eParams%raiseError(modName//'::'//myName// &
            ' - parameter name mistmatch! Searched for "'// &
              TRIM(thisName)//'" found "'//thisParam%name//'"')
        ENDIF
      ELSE
        CALL eParams%raiseError(modName//'::'//myName// &
          ' - parameter is not initialized! Use init method!')
      ENDIF
      IF(localalloc) DEALLOCATE(eParams)
    ENDSUBROUTINE set_ParamType
!
!-------------------------------------------------------------------------------
    RECURSIVE SUBROUTINE get_ParamType(thisParam,name,param)
      CLASS(ParamType),INTENT(IN) :: thisParam
      CHARACTER(LEN=*),INTENT(IN) :: name
      CLASS(ParamType),POINTER,INTENT(OUT) :: param
      CHARACTER(LEN=LEN(name)) :: thisname,nextname
      INTEGER(SIK) :: ipos
      
      ipos=INDEX(name,'->')
      thisname=name
      nextname=''
      IF(ipos > 0) THEN
        thisname=name(1:ipos-1)
        nextname=name(ipos+2:LEN(name))
      ENDIF
      
      param => NULL()
      IF(ASSOCIATED(thisParam%pdat)) THEN
        IF(LEN_TRIM(nextName) > 0) THEN
          IF(thisParam%name == TRIM(thisname)) THEN
            SELECTTYPE(p=>thisParam%pdat); TYPE IS(ParamListType)
              CALL p%getParam(TRIM(nextname),param)
            ENDSELECT
          ENDIF
        ELSE
          IF(thisParam%name == TRIM(thisname)) THEN
            param => thisParam%pdat
          ELSE
            SELECTTYPE(p=>thisParam%pdat); TYPE IS(ParamListType)
              CALL p%getParam(TRIM(thisname),param)
            ENDSELECT
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE get_ParamType
!
!-------------------------------------------------------------------------------
    RECURSIVE SUBROUTINE edit_ParamType(thisParam,funit,indent)
      CLASS(ParamType),INTENT(IN) :: thisParam
      INTEGER(SIK),INTENT(IN) :: funit
      INTEGER(SIK),INTENT(IN),OPTIONAL :: indent
      CHARACTER(LEN=12) :: fmt
      INTEGER(SIK) :: i
      
      i=2
      IF(PRESENT(indent)) i=i+indent
      
      IF(ASSOCIATED(thisParam%pdat)) THEN
        WRITE(fmt,'(i12)') i; fmt=ADJUSTL(fmt)
        WRITE(UNIT=funit,FMT='('//TRIM(fmt)//'x,a)') &
          thisParam%dataType//' :: '//thisParam%name//' -> '
        CALL thisParam%pdat%edit(funit,i)
      ENDIF
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
    SUBROUTINE init_ParamListType(thisParam,param,name,description)
      CHARACTER(LEN=*),PARAMETER :: myName='init_ParamListType'
      CLASS(ParamType),INTENT(INOUT) :: thisParam
      TYPE(ParamType),INTENT(IN) :: param(:)
      CHARACTER(LEN=*),INTENT(IN) :: name
      CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
      CHARACTER(LEN=LEN(name)) :: thisname,nextname
      LOGICAL(SBK) :: localalloc
      INTEGER(SIK) :: ipos
      
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
      
      IF(.NOT.ASSOCIATED(thisParam%pdat)) THEN
        thisParam%name=TRIM(thisname)
        IF(LEN_TRIM(nextname) > 0) THEN
          !The parameter is declared more than one level down so create
          !the intermediate level
          ALLOCATE(ParamListType :: thisParam%pdat)
          thisParam%dataType='TYPE(ParamListType)'
          SELECTTYPE(p=>thisParam%pdat); TYPE IS(ParamListType)
            IF(PRESENT(description)) THEN
              !CALL p%add(param,nextname,description)
            ELSE
              !CALL p%add(param,nextname)
            ENDIF
          ENDSELECT
        ELSE
          !Assign the parameter at this level
          IF(PRESENT(description)) thisParam%description=description
          thisParam%dataType='TYPE(ParamListType)'
          ALLOCATE(ParamListType :: thisParam%pdat)
          SELECTTYPE(p=>thisParam%pdat); TYPE IS(ParamListType)
            p%plList=param
          ENDSELECT
        ENDIF
      ELSE
        IF(LEN_TRIM(nextname) > 0) THEN
          IF(thisParam%name == thisname) THEN
            SELECTTYPE(p=>thisParam%pdat)
              TYPE IS(ParamListType)
                IF(PRESENT(description)) THEN
                  !CALL p%add(param,nextname,description)
                ELSE
                  !CALL p%add(param,nextname)
                ENDIF
              CLASS DEFAULT
                CALL eParams%raiseError(modName//'::'//myName// &
                ' - parameter type data mismatch! Parameter type is '// &
                  thisParam%dataType//' and must be TYPE(ParamListType)!')
            ENDSELECT
          ELSE
            CALL eParams%raiseError(modName//'::'//myName// &
              ' - parameter name mistmatch! Use add to create new parameter "'// &
                TRIM(thisName)//'"')
          ENDIF
        ELSE
          CALL eParams%raiseError(modName//'::'//myName// &
            ' - parameter '//thisParam%name//' is already initialized!'// &
              ' Use set method instead!')
        ENDIF
      ENDIF
      IF(localalloc) DEALLOCATE(eParams)
    ENDSUBROUTINE init_ParamListType
!
!-------------------------------------------------------------------------------
    SUBROUTINE set_ParamListType(thisParam,param,name,description)
      CHARACTER(LEN=*),PARAMETER :: myName='set_ParamListType'
      CLASS(ParamType),INTENT(INOUT) :: thisParam
      TYPE(ParamType),INTENT(IN) :: param(:)
      CHARACTER(LEN=*),INTENT(IN) :: name
      CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
      CHARACTER(LEN=LEN(name)) :: thisname,nextname
      LOGICAL(SBK) :: localalloc
      INTEGER(SIK) :: ipos
      
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
      
      IF(ASSOCIATED(thisParam%pdat)) THEN
        IF(thisParam%name == TRIM(thisname)) THEN
          IF(LEN_TRIM(nextname) > 0) THEN
            SELECTTYPE(p=>thisParam%pdat)
              TYPE IS(ParamListType)
                IF(PRESENT(description)) THEN
                  CALL p%setParamList(param,TRIM(nextname),TRIM(description))
                ELSE
                  CALL p%setParamList(param,TRIM(nextname))
                ENDIF
              CLASS DEFAULT
                CALL eParams%raiseError(modName//'::'//myName// &
                ' - parameter type data mismatch! Parameter type is '// &
                  thisParam%dataType//' and must be TYPE(ParamListType)!')
            ENDSELECT
          ELSE
            SELECTTYPE(p=>thisParam%pdat)
              TYPE IS(ParamListType)
                IF(PRESENT(description)) thisParam%description=description
                p%plList=param
              CLASS DEFAULT
                CALL eParams%raiseError(modName//'::'//myName// &
                  ' - parameter type data mismatch! Parameter type is '// &
                    thisParam%dataType//' and data is must be '// &
                      'TYPE(ParamType),DIMENSION(:)!')
            ENDSELECT
          ENDIF
        ELSE
          CALL eParams%raiseError(modName//'::'//myName// &
            ' - parameter name mistmatch! Searched for "'// &
              TRIM(thisName)//'" found "'//thisParam%name//'"')
        ENDIF
      ELSE
        CALL eParams%raiseError(modName//'::'//myName// &
          ' - parameter is not initialized! Use init method!')
      ENDIF
      IF(localalloc) DEALLOCATE(eParams)
    ENDSUBROUTINE set_ParamListType
!
!-------------------------------------------------------------------------------
    SUBROUTINE get_ParamType_List(thisParam,name,paramlist)
      CHARACTER(LEN=*),PARAMETER :: myName='get_ParamType_List'
      CLASS(ParamType),INTENT(IN) :: thisParam
      CHARACTER(LEN=*),INTENT(IN) :: name
      TYPE(ParamType),INTENT(OUT) :: paramlist(:)
      CLASS(ParamType),POINTER :: tmpParam
      
      IF(thisParam%name == TRIM(name)) THEN
        tmpParam => thisParam%pdat
      ELSE
        CALL thisParam%pdat%getParam(TRIM(name),tmpParam)
      ENDIF
      
      IF(ASSOCIATED(tmpParam)) THEN
        SELECTTYPE(p=>tmpParam%pdat)
          TYPE IS(ParamListType)
            paramlist=p%plList
          CLASS DEFAULT
            CALL eParams%raiseError(modName//'::'//myName// &
              ' - parameter type data mismatch! Parameter type is '// &
                tmpParam%dataType//' and data is TYPE(ParamType),DIMENSION(:)!')
        ENDSELECT
      ELSE
        CALL eParams%raiseError(modName//'::'//myName// &
          ' - Unable to locate parameter "'//TRIM(name)//'" in "'// &
            thisParam%name//'"!')
      ENDIF
    ENDSUBROUTINE get_ParamType_List
!
!-------------------------------------------------------------------------------
    RECURSIVE SUBROUTINE get_ParamListType(thisParam,name,param)
      CLASS(ParamListType),INTENT(IN) :: thisParam
      CHARACTER(LEN=*),INTENT(IN) :: name
      CLASS(ParamType),POINTER,INTENT(OUT) :: param
      CHARACTER(LEN=LEN(name)) :: thisname,nextname
      INTEGER(SIK) :: ipos,i
      
      ipos=INDEX(name,'->')
      thisname=name
      nextname=''
      IF(ipos > 0) THEN
        thisname=name(1:ipos-1)
        nextname=name(ipos+2:LEN(name))
      ENDIF
      
      param => NULL()
      IF(ALLOCATED(thisParam%plList)) THEN
        IF(LEN_TRIM(nextName) > 0) THEN
          DO i=1,SIZE(thisParam%plList)
            IF(thisParam%plList(i)%name == TRIM(thisname)) THEN
              SELECTTYPE(p=>thisParam%plList(i)%pdat); TYPE IS(ParamListType)
                CALL p%getParam(TRIM(nextname),param)
              ENDSELECT
              EXIT
            ENDIF
          ENDDO
        ELSE
          DO i=1,SIZE(thisParam%plList)
            IF(thisParam%plList(i)%name == TRIM(thisname)) &
              param => thisParam%plList(i)%pdat
          ENDDO
          IF(.NOT.ASSOCIATED(param)) THEN
            DO i=1,SIZE(thisParam%plList)
              SELECTTYPE(p=>thisParam%plList(i)%pdat); TYPE IS(ParamListType)
                CALL p%getParam(TRIM(nextname),param)
              ENDSELECT
              IF(ASSOCIATED(param)) EXIT
            ENDDO
          ELSE
            SELECTTYPE(p=>thisParam%pdat); TYPE IS(ParamListType)
              CALL p%getParam(TRIM(thisname),param)
            ENDSELECT
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE get_ParamListType
!
!-------------------------------------------------------------------------------
    RECURSIVE SUBROUTINE edit_ParamListType(thisParam,funit,indent)
      CLASS(ParamListType),INTENT(IN) :: thisParam
      INTEGER(SIK),INTENT(IN) :: funit
      INTEGER(SIK),INTENT(IN),OPTIONAL :: indent
      INTEGER(SIK) :: i,j
      
      i=2
      IF(PRESENT(indent)) i=i+indent
      DO j=1,SIZE(thisParam%plList)
        IF(ASSOCIATED(thisParam%plList(j)%pdat)) &
          CALL thisParam%plList(j)%pdat%edit(funit,i)
      ENDDO
    ENDSUBROUTINE edit_ParamListType
!
!-------------------------------------------------------------------------------
    SUBROUTINE clear_ParamListType(thisParam)
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
    ENDSUBROUTINE clear_ParamListType
!
!-------------------------------------------------------------------------------
    SUBROUTINE init_ParamType_SSK(thisParam,param,name,description)
      CHARACTER(LEN=*),PARAMETER :: myName='init_ParamType_SSK'
      CLASS(ParamType),INTENT(INOUT) :: thisParam
      REAL(SSK) :: param
      CHARACTER(LEN=*),INTENT(IN) :: name
      CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
      
      IF(.NOT.ASSOCIATED(thisParam%pdat)) THEN
        ALLOCATE(ParamType_SSK :: thisParam%pdat)
        thisParam%pdat%name=name
        IF(PRESENT(description)) thisParam%pdat%description=TRIM(description)
        thisParam%pdat%dataType='REAL(SRK)'
        SELECTTYPE(p=>thisParam%pdat)
          TYPE IS(ParamType_SSK); p%val=param
        ENDSELECT
      ELSE
        CALL eParams%raiseError(modName//'::'//myName// &
          ' - parameter is already initialized! Use set method!')
      ENDIF
    ENDSUBROUTINE init_ParamType_SSK
!
!-------------------------------------------------------------------------------
    SUBROUTINE set_ParamType_SSK(thisParam,param,name,description)
      CHARACTER(LEN=*),PARAMETER :: myName='set_ParamType_SSK'
      CLASS(ParamType),INTENT(INOUT) :: thisParam
      REAL(SSK) :: param
      CHARACTER(LEN=*),INTENT(IN) :: name
      CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
      
      
      IF(ASSOCIATED(thisParam%pdat)) THEN
        IF(thisParam%pdat%name == TRIM(name)) THEN
          SELECTTYPE(p=>thisParam%pdat)
            TYPE IS(ParamType_SSK)
              IF(PRESENT(description)) p%description=TRIM(description)
              p%val=param
            CLASS DEFAULT
              CALL eParams%raiseError(modName//'::'//myName// &
                ' - parameter type data mismatch! Parameter type is '// &
                  thisParam%pdat%dataType//' and data is REAL(SSK)!')
          ENDSELECT
        ELSE
          
        ENDIF
      ELSE
        
      ENDIF
    ENDSUBROUTINE set_ParamType_SSK
!
!-------------------------------------------------------------------------------
    RECURSIVE SUBROUTINE clear_ParamType_SSK(thisParam)
      CLASS(ParamType_SSK),INTENT(INOUT) :: thisParam
      thisParam%val=0.0_SSK
      thisParam%name=''
      thisParam%dataType=''
      thisParam%description=''
      IF(ASSOCIATED(thisParam%pdat)) THEN
        CALL thisParam%pdat%clear()
        DEALLOCATE(thisParam%pdat)
      ENDIF
    ENDSUBROUTINE clear_ParamType_SSK
!
!-------------------------------------------------------------------------------
    SUBROUTINE get_ParamType_SSK(thisParam,name,val)
      CHARACTER(LEN=*),PARAMETER :: myName='get_ParamType_SSK'
      CLASS(ParamType),INTENT(IN) :: thisParam
      CHARACTER(LEN=*),INTENT(IN) :: name
      REAL(SSK),INTENT(OUT) :: val
      CLASS(ParamType),POINTER :: tmpParam
      
      val=0.0_SSK
      IF(thisParam%name == name) THEN
        tmpParam => thisParam%pdat
      ELSE
        CALL thisParam%pdat%getParam(name,tmpParam)
      ENDIF
      IF(ASSOCIATED(tmpParam)) THEN
        SELECTTYPE(p=>tmpParam%pdat)
          TYPE IS(ParamType_SSK); val=p%val
          CLASS DEFAULT
            CALL eParams%raiseError(modName//'::'//myName// &
              ' - parameter type data mismatch! Parameter type is '// &
                tmpParam%dataType//' and data is REAL(SSK)!')
        ENDSELECT
      ELSE
        CALL eParams%raiseError(modName//'::'//myName// &
          ' - Unable to locate parameter "'//TRIM(name)//'" in "'// &
            thisParam%name//'"!')
      ENDIF
    ENDSUBROUTINE get_ParamType_SSK
!
!-------------------------------------------------------------------------------
    SUBROUTINE edit_ParamType_SSK(thisParam,funit,indent)
      CLASS(ParamType_SSK),INTENT(IN) :: thisParam
      INTEGER(SIK),INTENT(IN) :: funit
      INTEGER(SIK),INTENT(IN),OPTIONAL :: indent
      CHARACTER(LEN=12) :: fmt
      INTEGER(SIK) :: i
      
      i=2
      IF(PRESENT(indent)) i=i+indent
      WRITE(fmt,'(i12)') i; fmt=ADJUSTL(fmt)
      
      WRITE(UNIT=funit,FMT='('//TRIM(fmt)//'x,a,f9.6)') &
        thisParam%dataType//' :: '//thisParam%name//'=',thisParam%val
    ENDSUBROUTINE edit_ParamType_SSK
!
ENDMODULE ParameterLists
