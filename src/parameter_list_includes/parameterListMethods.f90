!
!-------------------------------------------------------------------------------
!> @brief Initializes a rank-0 parameter container of type LOGICAL(SBK)
!> @param this the node to initialize data on
!> @param name the name of the parameter to initialize
!> @param val the values to use for initializing the node
!> @param description the description of the value; optional
!>
SUBROUTINE initSBK0(this,name,val,description)
  CLASS(ParamNode),TARGET,INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  LOGICAL(SBK),INTENT(IN) :: val
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description

  IF(.NOT.ALLOCATED(this%val)) THEN
    CALL this%initBase(name,description)
    this%dataType = PL_DATA_TYPE_SBK0
    ALLOCATE(Param_SBK0 :: this%val)
  ENDIF
  SELECTTYPE(ptr => this%val); TYPE IS(Param_SBK0)
    ptr%p = val
  ENDSELECT

ENDSUBROUTINE initSBK0
!
!-------------------------------------------------------------------------------
!> @brief Initializes a rank-1 parameter container of type LOGICAL(SBK)
!> @param this the node to initialize data on
!> @param name the name of the parameter to initialize
!> @param val the values to use for initializing the node
!> @param description the description of the value; optional
!>
SUBROUTINE initSBK1(this,name,val,description)
  CLASS(ParamNode),TARGET,INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  LOGICAL(SBK),INTENT(IN) :: val(:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description

  IF(.NOT.ALLOCATED(this%val)) THEN
    CALL this%initBase(name,description)
    this%dataType = PL_DATA_TYPE_SBK1
    ALLOCATE(Param_SBK1 :: this%val)
  ENDIF
  SELECTTYPE(ptr => this%val); TYPE IS(Param_SBK1)
    ptr%p = val
  ENDSELECT

ENDSUBROUTINE initSBK1
!
!-------------------------------------------------------------------------------
!> @brief Initializes a rank-2 parameter container of type LOGICAL(SBK)
!> @param this the node to initialize data on
!> @param name the name of the parameter to initialize
!> @param val the values to use for initializing the node
!> @param description the description of the value; optional
!>
SUBROUTINE initSBK2(this,name,val,description)
  CLASS(ParamNode),TARGET,INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  LOGICAL(SBK),INTENT(IN) :: val(:,:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description

  IF(.NOT.ALLOCATED(this%val)) THEN
    CALL this%initBase(name,description)
    this%dataType = PL_DATA_TYPE_SBK2
    ALLOCATE(Param_SBK2 :: this%val)
  ENDIF
  SELECTTYPE(ptr => this%val); TYPE IS(Param_SBK2)
    ptr%p = val
  ENDSELECT

ENDSUBROUTINE initSBK2
!
!-------------------------------------------------------------------------------
!> @brief Initializes a rank-3 parameter container of type LOGICAL(SBK)
!> @param this the node to initialize data on
!> @param name the name of the parameter to initialize
!> @param val the values to use for initializing the node
!> @param description the description of the value; optional
!>
SUBROUTINE initSBK3(this,name,val,description)
  CLASS(ParamNode),TARGET,INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  LOGICAL(SBK),INTENT(IN) :: val(:,:,:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description

  IF(.NOT.ALLOCATED(this%val)) THEN
    CALL this%initBase(name,description)
    this%dataType = PL_DATA_TYPE_SBK3
    ALLOCATE(Param_SBK3 :: this%val)
  ENDIF
  SELECTTYPE(ptr => this%val); TYPE IS(Param_SBK3)
    ptr%p = val
  ENDSELECT

ENDSUBROUTINE initSBK3
!
!-------------------------------------------------------------------------------
!> @brief Initializes a rank-4 parameter container of type LOGICAL(SBK)
!> @param this the node to initialize data on
!> @param name the name of the parameter to initialize
!> @param val the values to use for initializing the node
!> @param description the description of the value; optional
!>
SUBROUTINE initSBK4(this,name,val,description)
  CLASS(ParamNode),TARGET,INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  LOGICAL(SBK),INTENT(IN) :: val(:,:,:,:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description

  IF(.NOT.ALLOCATED(this%val)) THEN
    CALL this%initBase(name,description)
    this%dataType = PL_DATA_TYPE_SBK4
    ALLOCATE(Param_SBK4 :: this%val)
  ENDIF
  SELECTTYPE(ptr => this%val); TYPE IS(Param_SBK4)
    ptr%p = val
  ENDSELECT

ENDSUBROUTINE initSBK4
!
!-------------------------------------------------------------------------------
!> @brief Initializes a rank-5 parameter container of type LOGICAL(SBK)
!> @param this the node to initialize data on
!> @param name the name of the parameter to initialize
!> @param val the values to use for initializing the node
!> @param description the description of the value; optional
!>
SUBROUTINE initSBK5(this,name,val,description)
  CLASS(ParamNode),TARGET,INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  LOGICAL(SBK),INTENT(IN) :: val(:,:,:,:,:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description

  IF(.NOT.ALLOCATED(this%val)) THEN
    CALL this%initBase(name,description)
    this%dataType = PL_DATA_TYPE_SBK5
    ALLOCATE(Param_SBK5 :: this%val)
  ENDIF
  SELECTTYPE(ptr => this%val); TYPE IS(Param_SBK5)
    ptr%p = val
  ENDSELECT

ENDSUBROUTINE initSBK5
!
!-------------------------------------------------------------------------------
!> @brief Initializes a rank-6 parameter container of type LOGICAL(SBK)
!> @param this the node to initialize data on
!> @param name the name of the parameter to initialize
!> @param val the values to use for initializing the node
!> @param description the description of the value; optional
!>
SUBROUTINE initSBK6(this,name,val,description)
  CLASS(ParamNode),TARGET,INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  LOGICAL(SBK),INTENT(IN) :: val(:,:,:,:,:,:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description

  IF(.NOT.ALLOCATED(this%val)) THEN
    CALL this%initBase(name,description)
    this%dataType = PL_DATA_TYPE_SBK6
    ALLOCATE(Param_SBK6 :: this%val)
  ENDIF
  SELECTTYPE(ptr => this%val); TYPE IS(Param_SBK6)
    ptr%p = val
  ENDSELECT

ENDSUBROUTINE initSBK6
!
!-------------------------------------------------------------------------------
!> @brief Initializes a rank-7 parameter container of type LOGICAL(SBK)
!> @param this the node to initialize data on
!> @param name the name of the parameter to initialize
!> @param val the values to use for initializing the node
!> @param description the description of the value; optional
!>
SUBROUTINE initSBK7(this,name,val,description)
  CLASS(ParamNode),TARGET,INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  LOGICAL(SBK),INTENT(IN) :: val(:,:,:,:,:,:,:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description

  IF(.NOT.ALLOCATED(this%val)) THEN
    CALL this%initBase(name,description)
    this%dataType = PL_DATA_TYPE_SBK7
    ALLOCATE(Param_SBK7 :: this%val)
  ENDIF
  SELECTTYPE(ptr => this%val); TYPE IS(Param_SBK7)
    ptr%p = val
  ENDSELECT

ENDSUBROUTINE initSBK7
!
!-------------------------------------------------------------------------------
!> @brief Initializes a rank-0 parameter container of type INTEGER(SNK)
!> @param this the node to initialize data on
!> @param name the name of the parameter to initialize
!> @param val the values to use for initializing the node
!> @param description the description of the value; optional
!>
SUBROUTINE initSNK0(this,name,val,description)
  CLASS(ParamNode),TARGET,INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  INTEGER(SNK),INTENT(IN) :: val
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description

  IF(.NOT.ALLOCATED(this%val)) THEN
    CALL this%initBase(name,description)
    this%dataType = PL_DATA_TYPE_SNK0
    ALLOCATE(Param_SNK0 :: this%val)
  ENDIF
  SELECTTYPE(ptr => this%val); TYPE IS(Param_SNK0)
    ptr%p = val
  ENDSELECT

ENDSUBROUTINE initSNK0
!
!-------------------------------------------------------------------------------
!> @brief Initializes a rank-1 parameter container of type INTEGER(SNK)
!> @param this the node to initialize data on
!> @param name the name of the parameter to initialize
!> @param val the values to use for initializing the node
!> @param description the description of the value; optional
!>
SUBROUTINE initSNK1(this,name,val,description)
  CLASS(ParamNode),TARGET,INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  INTEGER(SNK),INTENT(IN) :: val(:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description

  IF(.NOT.ALLOCATED(this%val)) THEN
    CALL this%initBase(name,description)
    this%dataType = PL_DATA_TYPE_SNK1
    ALLOCATE(Param_SNK1 :: this%val)
  ENDIF
  SELECTTYPE(ptr => this%val); TYPE IS(Param_SNK1)
    ptr%p = val
  ENDSELECT

ENDSUBROUTINE initSNK1
!
!-------------------------------------------------------------------------------
!> @brief Initializes a rank-2 parameter container of type INTEGER(SNK)
!> @param this the node to initialize data on
!> @param name the name of the parameter to initialize
!> @param val the values to use for initializing the node
!> @param description the description of the value; optional
!>
SUBROUTINE initSNK2(this,name,val,description)
  CLASS(ParamNode),TARGET,INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  INTEGER(SNK),INTENT(IN) :: val(:,:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description

  IF(.NOT.ALLOCATED(this%val)) THEN
    CALL this%initBase(name,description)
    this%dataType = PL_DATA_TYPE_SNK2
    ALLOCATE(Param_SNK2 :: this%val)
  ENDIF
  SELECTTYPE(ptr => this%val); TYPE IS(Param_SNK2)
    ptr%p = val
  ENDSELECT

ENDSUBROUTINE initSNK2
!
!-------------------------------------------------------------------------------
!> @brief Initializes a rank-3 parameter container of type INTEGER(SNK)
!> @param this the node to initialize data on
!> @param name the name of the parameter to initialize
!> @param val the values to use for initializing the node
!> @param description the description of the value; optional
!>
SUBROUTINE initSNK3(this,name,val,description)
  CLASS(ParamNode),TARGET,INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  INTEGER(SNK),INTENT(IN) :: val(:,:,:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description

  IF(.NOT.ALLOCATED(this%val)) THEN
    CALL this%initBase(name,description)
    this%dataType = PL_DATA_TYPE_SNK3
    ALLOCATE(Param_SNK3 :: this%val)
  ENDIF
  SELECTTYPE(ptr => this%val); TYPE IS(Param_SNK3)
    ptr%p = val
  ENDSELECT

ENDSUBROUTINE initSNK3
!
!-------------------------------------------------------------------------------
!> @brief Initializes a rank-4 parameter container of type INTEGER(SNK)
!> @param this the node to initialize data on
!> @param name the name of the parameter to initialize
!> @param val the values to use for initializing the node
!> @param description the description of the value; optional
!>
SUBROUTINE initSNK4(this,name,val,description)
  CLASS(ParamNode),TARGET,INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  INTEGER(SNK),INTENT(IN) :: val(:,:,:,:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description

  IF(.NOT.ALLOCATED(this%val)) THEN
    CALL this%initBase(name,description)
    this%dataType = PL_DATA_TYPE_SNK4
    ALLOCATE(Param_SNK4 :: this%val)
  ENDIF
  SELECTTYPE(ptr => this%val); TYPE IS(Param_SNK4)
    ptr%p = val
  ENDSELECT

ENDSUBROUTINE initSNK4
!
!-------------------------------------------------------------------------------
!> @brief Initializes a rank-5 parameter container of type INTEGER(SNK)
!> @param this the node to initialize data on
!> @param name the name of the parameter to initialize
!> @param val the values to use for initializing the node
!> @param description the description of the value; optional
!>
SUBROUTINE initSNK5(this,name,val,description)
  CLASS(ParamNode),TARGET,INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  INTEGER(SNK),INTENT(IN) :: val(:,:,:,:,:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description

  IF(.NOT.ALLOCATED(this%val)) THEN
    CALL this%initBase(name,description)
    this%dataType = PL_DATA_TYPE_SNK5
    ALLOCATE(Param_SNK5 :: this%val)
  ENDIF
  SELECTTYPE(ptr => this%val); TYPE IS(Param_SNK5)
    ptr%p = val
  ENDSELECT

ENDSUBROUTINE initSNK5
!
!-------------------------------------------------------------------------------
!> @brief Initializes a rank-6 parameter container of type INTEGER(SNK)
!> @param this the node to initialize data on
!> @param name the name of the parameter to initialize
!> @param val the values to use for initializing the node
!> @param description the description of the value; optional
!>
SUBROUTINE initSNK6(this,name,val,description)
  CLASS(ParamNode),TARGET,INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  INTEGER(SNK),INTENT(IN) :: val(:,:,:,:,:,:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description

  IF(.NOT.ALLOCATED(this%val)) THEN
    CALL this%initBase(name,description)
    this%dataType = PL_DATA_TYPE_SNK6
    ALLOCATE(Param_SNK6 :: this%val)
  ENDIF
  SELECTTYPE(ptr => this%val); TYPE IS(Param_SNK6)
    ptr%p = val
  ENDSELECT

ENDSUBROUTINE initSNK6
!
!-------------------------------------------------------------------------------
!> @brief Initializes a rank-7 parameter container of type INTEGER(SNK)
!> @param this the node to initialize data on
!> @param name the name of the parameter to initialize
!> @param val the values to use for initializing the node
!> @param description the description of the value; optional
!>
SUBROUTINE initSNK7(this,name,val,description)
  CLASS(ParamNode),TARGET,INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  INTEGER(SNK),INTENT(IN) :: val(:,:,:,:,:,:,:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description

  IF(.NOT.ALLOCATED(this%val)) THEN
    CALL this%initBase(name,description)
    this%dataType = PL_DATA_TYPE_SNK7
    ALLOCATE(Param_SNK7 :: this%val)
  ENDIF
  SELECTTYPE(ptr => this%val); TYPE IS(Param_SNK7)
    ptr%p = val
  ENDSELECT

ENDSUBROUTINE initSNK7
!
!-------------------------------------------------------------------------------
!> @brief Initializes a rank-0 parameter container of type INTEGER(SLK)
!> @param this the node to initialize data on
!> @param name the name of the parameter to initialize
!> @param val the values to use for initializing the node
!> @param description the description of the value; optional
!>
SUBROUTINE initSLK0(this,name,val,description)
  CLASS(ParamNode),TARGET,INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  INTEGER(SLK),INTENT(IN) :: val
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description

  IF(.NOT.ALLOCATED(this%val)) THEN
    CALL this%initBase(name,description)
    this%dataType = PL_DATA_TYPE_SLK0
    ALLOCATE(Param_SLK0 :: this%val)
  ENDIF
  SELECTTYPE(ptr => this%val); TYPE IS(Param_SLK0)
    ptr%p = val
  ENDSELECT

ENDSUBROUTINE initSLK0
!
!-------------------------------------------------------------------------------
!> @brief Initializes a rank-1 parameter container of type INTEGER(SLK)
!> @param this the node to initialize data on
!> @param name the name of the parameter to initialize
!> @param val the values to use for initializing the node
!> @param description the description of the value; optional
!>
SUBROUTINE initSLK1(this,name,val,description)
  CLASS(ParamNode),TARGET,INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  INTEGER(SLK),INTENT(IN) :: val(:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description

  IF(.NOT.ALLOCATED(this%val)) THEN
    CALL this%initBase(name,description)
    this%dataType = PL_DATA_TYPE_SLK1
    ALLOCATE(Param_SLK1 :: this%val)
  ENDIF
  SELECTTYPE(ptr => this%val); TYPE IS(Param_SLK1)
    ptr%p = val
  ENDSELECT

ENDSUBROUTINE initSLK1
!
!-------------------------------------------------------------------------------
!> @brief Initializes a rank-2 parameter container of type INTEGER(SLK)
!> @param this the node to initialize data on
!> @param name the name of the parameter to initialize
!> @param val the values to use for initializing the node
!> @param description the description of the value; optional
!>
SUBROUTINE initSLK2(this,name,val,description)
  CLASS(ParamNode),TARGET,INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  INTEGER(SLK),INTENT(IN) :: val(:,:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description

  IF(.NOT.ALLOCATED(this%val)) THEN
    CALL this%initBase(name,description)
    this%dataType = PL_DATA_TYPE_SLK2
    ALLOCATE(Param_SLK2 :: this%val)
  ENDIF
  SELECTTYPE(ptr => this%val); TYPE IS(Param_SLK2)
    ptr%p = val
  ENDSELECT

ENDSUBROUTINE initSLK2
!
!-------------------------------------------------------------------------------
!> @brief Initializes a rank-3 parameter container of type INTEGER(SLK)
!> @param this the node to initialize data on
!> @param name the name of the parameter to initialize
!> @param val the values to use for initializing the node
!> @param description the description of the value; optional
!>
SUBROUTINE initSLK3(this,name,val,description)
  CLASS(ParamNode),TARGET,INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  INTEGER(SLK),INTENT(IN) :: val(:,:,:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description

  IF(.NOT.ALLOCATED(this%val)) THEN
    CALL this%initBase(name,description)
    this%dataType = PL_DATA_TYPE_SLK3
    ALLOCATE(Param_SLK3 :: this%val)
  ENDIF
  SELECTTYPE(ptr => this%val); TYPE IS(Param_SLK3)
    ptr%p = val
  ENDSELECT

ENDSUBROUTINE initSLK3
!
!-------------------------------------------------------------------------------
!> @brief Initializes a rank-4 parameter container of type INTEGER(SLK)
!> @param this the node to initialize data on
!> @param name the name of the parameter to initialize
!> @param val the values to use for initializing the node
!> @param description the description of the value; optional
!>
SUBROUTINE initSLK4(this,name,val,description)
  CLASS(ParamNode),TARGET,INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  INTEGER(SLK),INTENT(IN) :: val(:,:,:,:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description

  IF(.NOT.ALLOCATED(this%val)) THEN
    CALL this%initBase(name,description)
    this%dataType = PL_DATA_TYPE_SLK4
    ALLOCATE(Param_SLK4 :: this%val)
  ENDIF
  SELECTTYPE(ptr => this%val); TYPE IS(Param_SLK4)
    ptr%p = val
  ENDSELECT

ENDSUBROUTINE initSLK4
!
!-------------------------------------------------------------------------------
!> @brief Initializes a rank-5 parameter container of type INTEGER(SLK)
!> @param this the node to initialize data on
!> @param name the name of the parameter to initialize
!> @param val the values to use for initializing the node
!> @param description the description of the value; optional
!>
SUBROUTINE initSLK5(this,name,val,description)
  CLASS(ParamNode),TARGET,INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  INTEGER(SLK),INTENT(IN) :: val(:,:,:,:,:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description

  IF(.NOT.ALLOCATED(this%val)) THEN
    CALL this%initBase(name,description)
    this%dataType = PL_DATA_TYPE_SLK5
    ALLOCATE(Param_SLK5 :: this%val)
  ENDIF
  SELECTTYPE(ptr => this%val); TYPE IS(Param_SLK5)
    ptr%p = val
  ENDSELECT

ENDSUBROUTINE initSLK5
!
!-------------------------------------------------------------------------------
!> @brief Initializes a rank-6 parameter container of type INTEGER(SLK)
!> @param this the node to initialize data on
!> @param name the name of the parameter to initialize
!> @param val the values to use for initializing the node
!> @param description the description of the value; optional
!>
SUBROUTINE initSLK6(this,name,val,description)
  CLASS(ParamNode),TARGET,INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  INTEGER(SLK),INTENT(IN) :: val(:,:,:,:,:,:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description

  IF(.NOT.ALLOCATED(this%val)) THEN
    CALL this%initBase(name,description)
    this%dataType = PL_DATA_TYPE_SLK6
    ALLOCATE(Param_SLK6 :: this%val)
  ENDIF
  SELECTTYPE(ptr => this%val); TYPE IS(Param_SLK6)
    ptr%p = val
  ENDSELECT

ENDSUBROUTINE initSLK6
!
!-------------------------------------------------------------------------------
!> @brief Initializes a rank-7 parameter container of type INTEGER(SLK)
!> @param this the node to initialize data on
!> @param name the name of the parameter to initialize
!> @param val the values to use for initializing the node
!> @param description the description of the value; optional
!>
SUBROUTINE initSLK7(this,name,val,description)
  CLASS(ParamNode),TARGET,INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  INTEGER(SLK),INTENT(IN) :: val(:,:,:,:,:,:,:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description

  IF(.NOT.ALLOCATED(this%val)) THEN
    CALL this%initBase(name,description)
    this%dataType = PL_DATA_TYPE_SLK7
    ALLOCATE(Param_SLK7 :: this%val)
  ENDIF
  SELECTTYPE(ptr => this%val); TYPE IS(Param_SLK7)
    ptr%p = val
  ENDSELECT

ENDSUBROUTINE initSLK7
!
!-------------------------------------------------------------------------------
!> @brief Initializes a rank-0 parameter container of type REAL(SSK)
!> @param this the node to initialize data on
!> @param name the name of the parameter to initialize
!> @param val the values to use for initializing the node
!> @param description the description of the value; optional
!>
SUBROUTINE initSSK0(this,name,val,description)
  CLASS(ParamNode),TARGET,INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  REAL(SSK),INTENT(IN) :: val
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description

  IF(.NOT.ALLOCATED(this%val)) THEN
    CALL this%initBase(name,description)
    this%dataType = PL_DATA_TYPE_SSK0
    ALLOCATE(Param_SSK0 :: this%val)
  ENDIF
  SELECTTYPE(ptr => this%val); TYPE IS(Param_SSK0)
    ptr%p = val
  ENDSELECT

ENDSUBROUTINE initSSK0
!
!-------------------------------------------------------------------------------
!> @brief Initializes a rank-1 parameter container of type REAL(SSK)
!> @param this the node to initialize data on
!> @param name the name of the parameter to initialize
!> @param val the values to use for initializing the node
!> @param description the description of the value; optional
!>
SUBROUTINE initSSK1(this,name,val,description)
  CLASS(ParamNode),TARGET,INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  REAL(SSK),INTENT(IN) :: val(:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description

  IF(.NOT.ALLOCATED(this%val)) THEN
    CALL this%initBase(name,description)
    this%dataType = PL_DATA_TYPE_SSK1
    ALLOCATE(Param_SSK1 :: this%val)
  ENDIF
  SELECTTYPE(ptr => this%val); TYPE IS(Param_SSK1)
    ptr%p = val
  ENDSELECT

ENDSUBROUTINE initSSK1
!
!-------------------------------------------------------------------------------
!> @brief Initializes a rank-2 parameter container of type REAL(SSK)
!> @param this the node to initialize data on
!> @param name the name of the parameter to initialize
!> @param val the values to use for initializing the node
!> @param description the description of the value; optional
!>
SUBROUTINE initSSK2(this,name,val,description)
  CLASS(ParamNode),TARGET,INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  REAL(SSK),INTENT(IN) :: val(:,:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description

  IF(.NOT.ALLOCATED(this%val)) THEN
    CALL this%initBase(name,description)
    this%dataType = PL_DATA_TYPE_SSK2
    ALLOCATE(Param_SSK2 :: this%val)
  ENDIF
  SELECTTYPE(ptr => this%val); TYPE IS(Param_SSK2)
    ptr%p = val
  ENDSELECT

ENDSUBROUTINE initSSK2
!
!-------------------------------------------------------------------------------
!> @brief Initializes a rank-3 parameter container of type REAL(SSK)
!> @param this the node to initialize data on
!> @param name the name of the parameter to initialize
!> @param val the values to use for initializing the node
!> @param description the description of the value; optional
!>
SUBROUTINE initSSK3(this,name,val,description)
  CLASS(ParamNode),TARGET,INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  REAL(SSK),INTENT(IN) :: val(:,:,:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description

  IF(.NOT.ALLOCATED(this%val)) THEN
    CALL this%initBase(name,description)
    this%dataType = PL_DATA_TYPE_SSK3
    ALLOCATE(Param_SSK3 :: this%val)
  ENDIF
  SELECTTYPE(ptr => this%val); TYPE IS(Param_SSK3)
    ptr%p = val
  ENDSELECT

ENDSUBROUTINE initSSK3
!
!-------------------------------------------------------------------------------
!> @brief Initializes a rank-4 parameter container of type REAL(SSK)
!> @param this the node to initialize data on
!> @param name the name of the parameter to initialize
!> @param val the values to use for initializing the node
!> @param description the description of the value; optional
!>
SUBROUTINE initSSK4(this,name,val,description)
  CLASS(ParamNode),TARGET,INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  REAL(SSK),INTENT(IN) :: val(:,:,:,:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description

  IF(.NOT.ALLOCATED(this%val)) THEN
    CALL this%initBase(name,description)
    this%dataType = PL_DATA_TYPE_SSK4
    ALLOCATE(Param_SSK4 :: this%val)
  ENDIF
  SELECTTYPE(ptr => this%val); TYPE IS(Param_SSK4)
    ptr%p = val
  ENDSELECT

ENDSUBROUTINE initSSK4
!
!-------------------------------------------------------------------------------
!> @brief Initializes a rank-5 parameter container of type REAL(SSK)
!> @param this the node to initialize data on
!> @param name the name of the parameter to initialize
!> @param val the values to use for initializing the node
!> @param description the description of the value; optional
!>
SUBROUTINE initSSK5(this,name,val,description)
  CLASS(ParamNode),TARGET,INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  REAL(SSK),INTENT(IN) :: val(:,:,:,:,:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description

  IF(.NOT.ALLOCATED(this%val)) THEN
    CALL this%initBase(name,description)
    this%dataType = PL_DATA_TYPE_SSK5
    ALLOCATE(Param_SSK5 :: this%val)
  ENDIF
  SELECTTYPE(ptr => this%val); TYPE IS(Param_SSK5)
    ptr%p = val
  ENDSELECT

ENDSUBROUTINE initSSK5
!
!-------------------------------------------------------------------------------
!> @brief Initializes a rank-6 parameter container of type REAL(SSK)
!> @param this the node to initialize data on
!> @param name the name of the parameter to initialize
!> @param val the values to use for initializing the node
!> @param description the description of the value; optional
!>
SUBROUTINE initSSK6(this,name,val,description)
  CLASS(ParamNode),TARGET,INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  REAL(SSK),INTENT(IN) :: val(:,:,:,:,:,:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description

  IF(.NOT.ALLOCATED(this%val)) THEN
    CALL this%initBase(name,description)
    this%dataType = PL_DATA_TYPE_SSK6
    ALLOCATE(Param_SSK6 :: this%val)
  ENDIF
  SELECTTYPE(ptr => this%val); TYPE IS(Param_SSK6)
    ptr%p = val
  ENDSELECT

ENDSUBROUTINE initSSK6
!
!-------------------------------------------------------------------------------
!> @brief Initializes a rank-7 parameter container of type REAL(SSK)
!> @param this the node to initialize data on
!> @param name the name of the parameter to initialize
!> @param val the values to use for initializing the node
!> @param description the description of the value; optional
!>
SUBROUTINE initSSK7(this,name,val,description)
  CLASS(ParamNode),TARGET,INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  REAL(SSK),INTENT(IN) :: val(:,:,:,:,:,:,:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description

  IF(.NOT.ALLOCATED(this%val)) THEN
    CALL this%initBase(name,description)
    this%dataType = PL_DATA_TYPE_SSK7
    ALLOCATE(Param_SSK7 :: this%val)
  ENDIF
  SELECTTYPE(ptr => this%val); TYPE IS(Param_SSK7)
    ptr%p = val
  ENDSELECT

ENDSUBROUTINE initSSK7
!
!-------------------------------------------------------------------------------
!> @brief Initializes a rank-0 parameter container of type REAL(SDK)
!> @param this the node to initialize data on
!> @param name the name of the parameter to initialize
!> @param val the values to use for initializing the node
!> @param description the description of the value; optional
!>
SUBROUTINE initSDK0(this,name,val,description)
  CLASS(ParamNode),TARGET,INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  REAL(SDK),INTENT(IN) :: val
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description

  IF(.NOT.ALLOCATED(this%val)) THEN
    CALL this%initBase(name,description)
    this%dataType = PL_DATA_TYPE_SDK0
    ALLOCATE(Param_SDK0 :: this%val)
  ENDIF
  SELECTTYPE(ptr => this%val); TYPE IS(Param_SDK0)
    ptr%p = val
  ENDSELECT

ENDSUBROUTINE initSDK0
!
!-------------------------------------------------------------------------------
!> @brief Initializes a rank-1 parameter container of type REAL(SDK)
!> @param this the node to initialize data on
!> @param name the name of the parameter to initialize
!> @param val the values to use for initializing the node
!> @param description the description of the value; optional
!>
SUBROUTINE initSDK1(this,name,val,description)
  CLASS(ParamNode),TARGET,INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  REAL(SDK),INTENT(IN) :: val(:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description

  IF(.NOT.ALLOCATED(this%val)) THEN
    CALL this%initBase(name,description)
    this%dataType = PL_DATA_TYPE_SDK1
    ALLOCATE(Param_SDK1 :: this%val)
  ENDIF
  SELECTTYPE(ptr => this%val); TYPE IS(Param_SDK1)
    ptr%p = val
  ENDSELECT

ENDSUBROUTINE initSDK1
!
!-------------------------------------------------------------------------------
!> @brief Initializes a rank-2 parameter container of type REAL(SDK)
!> @param this the node to initialize data on
!> @param name the name of the parameter to initialize
!> @param val the values to use for initializing the node
!> @param description the description of the value; optional
!>
SUBROUTINE initSDK2(this,name,val,description)
  CLASS(ParamNode),TARGET,INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  REAL(SDK),INTENT(IN) :: val(:,:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description

  IF(.NOT.ALLOCATED(this%val)) THEN
    CALL this%initBase(name,description)
    this%dataType = PL_DATA_TYPE_SDK2
    ALLOCATE(Param_SDK2 :: this%val)
  ENDIF
  SELECTTYPE(ptr => this%val); TYPE IS(Param_SDK2)
    ptr%p = val
  ENDSELECT

ENDSUBROUTINE initSDK2
!
!-------------------------------------------------------------------------------
!> @brief Initializes a rank-3 parameter container of type REAL(SDK)
!> @param this the node to initialize data on
!> @param name the name of the parameter to initialize
!> @param val the values to use for initializing the node
!> @param description the description of the value; optional
!>
SUBROUTINE initSDK3(this,name,val,description)
  CLASS(ParamNode),TARGET,INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  REAL(SDK),INTENT(IN) :: val(:,:,:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description

  IF(.NOT.ALLOCATED(this%val)) THEN
    CALL this%initBase(name,description)
    this%dataType = PL_DATA_TYPE_SDK3
    ALLOCATE(Param_SDK3 :: this%val)
  ENDIF
  SELECTTYPE(ptr => this%val); TYPE IS(Param_SDK3)
    ptr%p = val
  ENDSELECT

ENDSUBROUTINE initSDK3
!
!-------------------------------------------------------------------------------
!> @brief Initializes a rank-4 parameter container of type REAL(SDK)
!> @param this the node to initialize data on
!> @param name the name of the parameter to initialize
!> @param val the values to use for initializing the node
!> @param description the description of the value; optional
!>
SUBROUTINE initSDK4(this,name,val,description)
  CLASS(ParamNode),TARGET,INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  REAL(SDK),INTENT(IN) :: val(:,:,:,:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description

  IF(.NOT.ALLOCATED(this%val)) THEN
    CALL this%initBase(name,description)
    this%dataType = PL_DATA_TYPE_SDK4
    ALLOCATE(Param_SDK4 :: this%val)
  ENDIF
  SELECTTYPE(ptr => this%val); TYPE IS(Param_SDK4)
    ptr%p = val
  ENDSELECT

ENDSUBROUTINE initSDK4
!
!-------------------------------------------------------------------------------
!> @brief Initializes a rank-5 parameter container of type REAL(SDK)
!> @param this the node to initialize data on
!> @param name the name of the parameter to initialize
!> @param val the values to use for initializing the node
!> @param description the description of the value; optional
!>
SUBROUTINE initSDK5(this,name,val,description)
  CLASS(ParamNode),TARGET,INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  REAL(SDK),INTENT(IN) :: val(:,:,:,:,:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description

  IF(.NOT.ALLOCATED(this%val)) THEN
    CALL this%initBase(name,description)
    this%dataType = PL_DATA_TYPE_SDK5
    ALLOCATE(Param_SDK5 :: this%val)
  ENDIF
  SELECTTYPE(ptr => this%val); TYPE IS(Param_SDK5)
    ptr%p = val
  ENDSELECT

ENDSUBROUTINE initSDK5
!
!-------------------------------------------------------------------------------
!> @brief Initializes a rank-6 parameter container of type REAL(SDK)
!> @param this the node to initialize data on
!> @param name the name of the parameter to initialize
!> @param val the values to use for initializing the node
!> @param description the description of the value; optional
!>
SUBROUTINE initSDK6(this,name,val,description)
  CLASS(ParamNode),TARGET,INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  REAL(SDK),INTENT(IN) :: val(:,:,:,:,:,:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description

  IF(.NOT.ALLOCATED(this%val)) THEN
    CALL this%initBase(name,description)
    this%dataType = PL_DATA_TYPE_SDK6
    ALLOCATE(Param_SDK6 :: this%val)
  ENDIF
  SELECTTYPE(ptr => this%val); TYPE IS(Param_SDK6)
    ptr%p = val
  ENDSELECT

ENDSUBROUTINE initSDK6
!
!-------------------------------------------------------------------------------
!> @brief Initializes a rank-7 parameter container of type REAL(SDK)
!> @param this the node to initialize data on
!> @param name the name of the parameter to initialize
!> @param val the values to use for initializing the node
!> @param description the description of the value; optional
!>
SUBROUTINE initSDK7(this,name,val,description)
  CLASS(ParamNode),TARGET,INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  REAL(SDK),INTENT(IN) :: val(:,:,:,:,:,:,:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description

  IF(.NOT.ALLOCATED(this%val)) THEN
    CALL this%initBase(name,description)
    this%dataType = PL_DATA_TYPE_SDK7
    ALLOCATE(Param_SDK7 :: this%val)
  ENDIF
  SELECTTYPE(ptr => this%val); TYPE IS(Param_SDK7)
    ptr%p = val
  ENDSELECT

ENDSUBROUTINE initSDK7
!
!-------------------------------------------------------------------------------
!> @brief Initializes a rank-0 parameter container of type TYPE(StringType)
!> @param this the node to initialize data on
!> @param name the name of the parameter to initialize
!> @param val the values to use for initializing the node
!> @param description the description of the value; optional
!>
SUBROUTINE initSTR0(this,name,val,description)
  CLASS(ParamNode),TARGET,INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  TYPE(StringType),INTENT(IN) :: val
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description

  IF(.NOT.ALLOCATED(this%val)) THEN
    CALL this%initBase(name,description)
    this%dataType = PL_DATA_TYPE_STR0
    ALLOCATE(Param_STR0 :: this%val)
  ENDIF
  SELECTTYPE(ptr => this%val); TYPE IS(Param_STR0)
    ptr%p = val
  ENDSELECT

ENDSUBROUTINE initSTR0
!
!-------------------------------------------------------------------------------
!> @brief Initializes a rank-1 parameter container of type TYPE(StringType)
!> @param this the node to initialize data on
!> @param name the name of the parameter to initialize
!> @param val the values to use for initializing the node
!> @param description the description of the value; optional
!>
SUBROUTINE initSTR1(this,name,val,description)
  CLASS(ParamNode),TARGET,INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  TYPE(StringType),INTENT(IN) :: val(:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description

  IF(.NOT.ALLOCATED(this%val)) THEN
    CALL this%initBase(name,description)
    this%dataType = PL_DATA_TYPE_STR1
    ALLOCATE(Param_STR1 :: this%val)
  ENDIF
  SELECTTYPE(ptr => this%val); TYPE IS(Param_STR1)
    ptr%p = val
  ENDSELECT

ENDSUBROUTINE initSTR1
!
!-------------------------------------------------------------------------------
!> @brief Initializes a rank-2 parameter container of type TYPE(StringType)
!> @param this the node to initialize data on
!> @param name the name of the parameter to initialize
!> @param val the values to use for initializing the node
!> @param description the description of the value; optional
!>
SUBROUTINE initSTR2(this,name,val,description)
  CLASS(ParamNode),TARGET,INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  TYPE(StringType),INTENT(IN) :: val(:,:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description

  IF(.NOT.ALLOCATED(this%val)) THEN
    CALL this%initBase(name,description)
    this%dataType = PL_DATA_TYPE_STR2
    ALLOCATE(Param_STR2 :: this%val)
  ENDIF
  SELECTTYPE(ptr => this%val); TYPE IS(Param_STR2)
    ptr%p = val
  ENDSELECT

ENDSUBROUTINE initSTR2
!
!-------------------------------------------------------------------------------
!> @brief Initializes a rank-3 parameter container of type TYPE(StringType)
!> @param this the node to initialize data on
!> @param name the name of the parameter to initialize
!> @param val the values to use for initializing the node
!> @param description the description of the value; optional
!>
SUBROUTINE initSTR3(this,name,val,description)
  CLASS(ParamNode),TARGET,INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  TYPE(StringType),INTENT(IN) :: val(:,:,:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description

  IF(.NOT.ALLOCATED(this%val)) THEN
    CALL this%initBase(name,description)
    this%dataType = PL_DATA_TYPE_STR3
    ALLOCATE(Param_STR3 :: this%val)
  ENDIF
  SELECTTYPE(ptr => this%val); TYPE IS(Param_STR3)
    ptr%p = val
  ENDSELECT

ENDSUBROUTINE initSTR3
!
!-------------------------------------------------------------------------------
!> @brief Initializes a rank-4 parameter container of type TYPE(StringType)
!> @param this the node to initialize data on
!> @param name the name of the parameter to initialize
!> @param val the values to use for initializing the node
!> @param description the description of the value; optional
!>
SUBROUTINE initSTR4(this,name,val,description)
  CLASS(ParamNode),TARGET,INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  TYPE(StringType),INTENT(IN) :: val(:,:,:,:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description

  IF(.NOT.ALLOCATED(this%val)) THEN
    CALL this%initBase(name,description)
    this%dataType = PL_DATA_TYPE_STR4
    ALLOCATE(Param_STR4 :: this%val)
  ENDIF
  SELECTTYPE(ptr => this%val); TYPE IS(Param_STR4)
    ptr%p = val
  ENDSELECT

ENDSUBROUTINE initSTR4
!
!-------------------------------------------------------------------------------
!> @brief Initializes a rank-5 parameter container of type TYPE(StringType)
!> @param this the node to initialize data on
!> @param name the name of the parameter to initialize
!> @param val the values to use for initializing the node
!> @param description the description of the value; optional
!>
SUBROUTINE initSTR5(this,name,val,description)
  CLASS(ParamNode),TARGET,INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  TYPE(StringType),INTENT(IN) :: val(:,:,:,:,:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description

  IF(.NOT.ALLOCATED(this%val)) THEN
    CALL this%initBase(name,description)
    this%dataType = PL_DATA_TYPE_STR5
    ALLOCATE(Param_STR5 :: this%val)
  ENDIF
  SELECTTYPE(ptr => this%val); TYPE IS(Param_STR5)
    ptr%p = val
  ENDSELECT

ENDSUBROUTINE initSTR5
!
!-------------------------------------------------------------------------------
!> @brief Initializes a rank-6 parameter container of type TYPE(StringType)
!> @param this the node to initialize data on
!> @param name the name of the parameter to initialize
!> @param val the values to use for initializing the node
!> @param description the description of the value; optional
!>
SUBROUTINE initSTR6(this,name,val,description)
  CLASS(ParamNode),TARGET,INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  TYPE(StringType),INTENT(IN) :: val(:,:,:,:,:,:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description

  IF(.NOT.ALLOCATED(this%val)) THEN
    CALL this%initBase(name,description)
    this%dataType = PL_DATA_TYPE_STR6
    ALLOCATE(Param_STR6 :: this%val)
  ENDIF
  SELECTTYPE(ptr => this%val); TYPE IS(Param_STR6)
    ptr%p = val
  ENDSELECT

ENDSUBROUTINE initSTR6
!
!-------------------------------------------------------------------------------
!> @brief Initializes a rank-7 parameter container of type TYPE(StringType)
!> @param this the node to initialize data on
!> @param name the name of the parameter to initialize
!> @param val the values to use for initializing the node
!> @param description the description of the value; optional
!>
SUBROUTINE initSTR7(this,name,val,description)
  CLASS(ParamNode),TARGET,INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  TYPE(StringType),INTENT(IN) :: val(:,:,:,:,:,:,:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description

  IF(.NOT.ALLOCATED(this%val)) THEN
    CALL this%initBase(name,description)
    this%dataType = PL_DATA_TYPE_STR7
    ALLOCATE(Param_STR7 :: this%val)
  ENDIF
  SELECTTYPE(ptr => this%val); TYPE IS(Param_STR7)
    ptr%p = val
  ENDSELECT

ENDSUBROUTINE initSTR7
!
!-------------------------------------------------------------------------------
!> @brief Gets a rank-0 parameter of type LOGICAL(SBK)
!> @param this the node to get data from
!> @param name the name of the parameter to get
!> @param val the values retrieved from the node
!> @param default the value to assign to @c val if the parameter is not found; optional
!>
!> If the parameter is not found and @c default is not provided, an error is thrown.
!>
SUBROUTINE getSBK0(this,name,val,default)
  CLASS(ParamType),INTENT(IN) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  LOGICAL(SBK),INTENT(OUT) :: val
  LOGICAL(SBK),INTENT(IN),OPTIONAL :: default
  !
  CLASS(ParamNode),POINTER :: node

  CALL this%getNode_Name(name,node)
  IF(errorChecks_get(node,PRESENT(default),name,PL_DATA_TYPE_SBK0) == 0) THEN
    IF(ASSOCIATED(node)) THEN
      SELECTTYPE(ptr => node%val); TYPE IS(Param_SBK0)
        val=ptr%p
      ENDSELECT
    ELSE
      val=default
    ENDIF
  ENDIF

ENDSUBROUTINE getSBK0
!
!-------------------------------------------------------------------------------
!> @brief Gets a rank-1 parameter of type LOGICAL(SBK)
!> @param this the node to get data from
!> @param name the name of the parameter to get
!> @param val the values retrieved from the node
!> @param default the value to assign to @c val if the parameter is not found; optional
!>
!> If the parameter is not found and @c default is not provided, an error is thrown.
!>
SUBROUTINE getSBK1(this,name,val,default)
  CLASS(ParamType),INTENT(IN) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  LOGICAL(SBK),ALLOCATABLE,INTENT(OUT) :: val(:)
  LOGICAL(SBK),INTENT(IN),OPTIONAL :: default(:)
  !
  CLASS(ParamNode),POINTER :: node

  CALL this%getNode_Name(name,node)
  IF(errorChecks_get(node,PRESENT(default),name,PL_DATA_TYPE_SBK1) == 0) THEN
    IF(ASSOCIATED(node)) THEN
      SELECTTYPE(ptr => node%val); TYPE IS(Param_SBK1)
        val=ptr%p
      ENDSELECT
    ELSE
      val=default
    ENDIF
  ENDIF

ENDSUBROUTINE getSBK1
!
!-------------------------------------------------------------------------------
!> @brief Gets a rank-2 parameter of type LOGICAL(SBK)
!> @param this the node to get data from
!> @param name the name of the parameter to get
!> @param val the values retrieved from the node
!> @param default the value to assign to @c val if the parameter is not found; optional
!>
!> If the parameter is not found and @c default is not provided, an error is thrown.
!>
SUBROUTINE getSBK2(this,name,val,default)
  CLASS(ParamType),INTENT(IN) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  LOGICAL(SBK),ALLOCATABLE,INTENT(OUT) :: val(:,:)
  LOGICAL(SBK),INTENT(IN),OPTIONAL :: default(:,:)
  !
  CLASS(ParamNode),POINTER :: node

  CALL this%getNode_Name(name,node)
  IF(errorChecks_get(node,PRESENT(default),name,PL_DATA_TYPE_SBK2) == 0) THEN
    IF(ASSOCIATED(node)) THEN
      SELECTTYPE(ptr => node%val); TYPE IS(Param_SBK2)
        val=ptr%p
      ENDSELECT
    ELSE
      val=default
    ENDIF
  ENDIF

ENDSUBROUTINE getSBK2
!
!-------------------------------------------------------------------------------
!> @brief Gets a rank-3 parameter of type LOGICAL(SBK)
!> @param this the node to get data from
!> @param name the name of the parameter to get
!> @param val the values retrieved from the node
!> @param default the value to assign to @c val if the parameter is not found; optional
!>
!> If the parameter is not found and @c default is not provided, an error is thrown.
!>
SUBROUTINE getSBK3(this,name,val,default)
  CLASS(ParamType),INTENT(IN) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  LOGICAL(SBK),ALLOCATABLE,INTENT(OUT) :: val(:,:,:)
  LOGICAL(SBK),INTENT(IN),OPTIONAL :: default(:,:,:)
  !
  CLASS(ParamNode),POINTER :: node

  CALL this%getNode_Name(name,node)
  IF(errorChecks_get(node,PRESENT(default),name,PL_DATA_TYPE_SBK3) == 0) THEN
    IF(ASSOCIATED(node)) THEN
      SELECTTYPE(ptr => node%val); TYPE IS(Param_SBK3)
        val=ptr%p
      ENDSELECT
    ELSE
      val=default
    ENDIF
  ENDIF

ENDSUBROUTINE getSBK3
!
!-------------------------------------------------------------------------------
!> @brief Gets a rank-4 parameter of type LOGICAL(SBK)
!> @param this the node to get data from
!> @param name the name of the parameter to get
!> @param val the values retrieved from the node
!> @param default the value to assign to @c val if the parameter is not found; optional
!>
!> If the parameter is not found and @c default is not provided, an error is thrown.
!>
SUBROUTINE getSBK4(this,name,val,default)
  CLASS(ParamType),INTENT(IN) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  LOGICAL(SBK),ALLOCATABLE,INTENT(OUT) :: val(:,:,:,:)
  LOGICAL(SBK),INTENT(IN),OPTIONAL :: default(:,:,:,:)
  !
  CLASS(ParamNode),POINTER :: node

  CALL this%getNode_Name(name,node)
  IF(errorChecks_get(node,PRESENT(default),name,PL_DATA_TYPE_SBK4) == 0) THEN
    IF(ASSOCIATED(node)) THEN
      SELECTTYPE(ptr => node%val); TYPE IS(Param_SBK4)
        val=ptr%p
      ENDSELECT
    ELSE
      val=default
    ENDIF
  ENDIF

ENDSUBROUTINE getSBK4
!
!-------------------------------------------------------------------------------
!> @brief Gets a rank-5 parameter of type LOGICAL(SBK)
!> @param this the node to get data from
!> @param name the name of the parameter to get
!> @param val the values retrieved from the node
!> @param default the value to assign to @c val if the parameter is not found; optional
!>
!> If the parameter is not found and @c default is not provided, an error is thrown.
!>
SUBROUTINE getSBK5(this,name,val,default)
  CLASS(ParamType),INTENT(IN) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  LOGICAL(SBK),ALLOCATABLE,INTENT(OUT) :: val(:,:,:,:,:)
  LOGICAL(SBK),INTENT(IN),OPTIONAL :: default(:,:,:,:,:)
  !
  CLASS(ParamNode),POINTER :: node

  CALL this%getNode_Name(name,node)
  IF(errorChecks_get(node,PRESENT(default),name,PL_DATA_TYPE_SBK5) == 0) THEN
    IF(ASSOCIATED(node)) THEN
      SELECTTYPE(ptr => node%val); TYPE IS(Param_SBK5)
        val=ptr%p
      ENDSELECT
    ELSE
      val=default
    ENDIF
  ENDIF

ENDSUBROUTINE getSBK5
!
!-------------------------------------------------------------------------------
!> @brief Gets a rank-6 parameter of type LOGICAL(SBK)
!> @param this the node to get data from
!> @param name the name of the parameter to get
!> @param val the values retrieved from the node
!> @param default the value to assign to @c val if the parameter is not found; optional
!>
!> If the parameter is not found and @c default is not provided, an error is thrown.
!>
SUBROUTINE getSBK6(this,name,val,default)
  CLASS(ParamType),INTENT(IN) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  LOGICAL(SBK),ALLOCATABLE,INTENT(OUT) :: val(:,:,:,:,:,:)
  LOGICAL(SBK),INTENT(IN),OPTIONAL :: default(:,:,:,:,:,:)
  !
  CLASS(ParamNode),POINTER :: node

  CALL this%getNode_Name(name,node)
  IF(errorChecks_get(node,PRESENT(default),name,PL_DATA_TYPE_SBK6) == 0) THEN
    IF(ASSOCIATED(node)) THEN
      SELECTTYPE(ptr => node%val); TYPE IS(Param_SBK6)
        val=ptr%p
      ENDSELECT
    ELSE
      val=default
    ENDIF
  ENDIF

ENDSUBROUTINE getSBK6
!
!-------------------------------------------------------------------------------
!> @brief Gets a rank-7 parameter of type LOGICAL(SBK)
!> @param this the node to get data from
!> @param name the name of the parameter to get
!> @param val the values retrieved from the node
!> @param default the value to assign to @c val if the parameter is not found; optional
!>
!> If the parameter is not found and @c default is not provided, an error is thrown.
!>
SUBROUTINE getSBK7(this,name,val,default)
  CLASS(ParamType),INTENT(IN) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  LOGICAL(SBK),ALLOCATABLE,INTENT(OUT) :: val(:,:,:,:,:,:,:)
  LOGICAL(SBK),INTENT(IN),OPTIONAL :: default(:,:,:,:,:,:,:)
  !
  CLASS(ParamNode),POINTER :: node

  CALL this%getNode_Name(name,node)
  IF(errorChecks_get(node,PRESENT(default),name,PL_DATA_TYPE_SBK7) == 0) THEN
    IF(ASSOCIATED(node)) THEN
      SELECTTYPE(ptr => node%val); TYPE IS(Param_SBK7)
        val=ptr%p
      ENDSELECT
    ELSE
      val=default
    ENDIF
  ENDIF

ENDSUBROUTINE getSBK7
!
!-------------------------------------------------------------------------------
!> @brief Gets a rank-0 parameter of type INTEGER(SNK)
!> @param this the node to get data from
!> @param name the name of the parameter to get
!> @param val the values retrieved from the node
!> @param default the value to assign to @c val if the parameter is not found; optional
!>
!> If the parameter is not found and @c default is not provided, an error is thrown.
!>
SUBROUTINE getSNK0(this,name,val,default)
  CLASS(ParamType),INTENT(IN) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  INTEGER(SNK),INTENT(OUT) :: val
  INTEGER(SNK),INTENT(IN),OPTIONAL :: default
  !
  CLASS(ParamNode),POINTER :: node

  CALL this%getNode_Name(name,node)
  IF(errorChecks_get(node,PRESENT(default),name,PL_DATA_TYPE_SNK0) == 0) THEN
    IF(ASSOCIATED(node)) THEN
      SELECTTYPE(ptr => node%val); TYPE IS(Param_SNK0)
        val=ptr%p
      ENDSELECT
    ELSE
      val=default
    ENDIF
  ENDIF

ENDSUBROUTINE getSNK0
!
!-------------------------------------------------------------------------------
!> @brief Gets a rank-1 parameter of type INTEGER(SNK)
!> @param this the node to get data from
!> @param name the name of the parameter to get
!> @param val the values retrieved from the node
!> @param default the value to assign to @c val if the parameter is not found; optional
!>
!> If the parameter is not found and @c default is not provided, an error is thrown.
!>
SUBROUTINE getSNK1(this,name,val,default)
  CLASS(ParamType),INTENT(IN) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  INTEGER(SNK),ALLOCATABLE,INTENT(OUT) :: val(:)
  INTEGER(SNK),INTENT(IN),OPTIONAL :: default(:)
  !
  CLASS(ParamNode),POINTER :: node

  CALL this%getNode_Name(name,node)
  IF(errorChecks_get(node,PRESENT(default),name,PL_DATA_TYPE_SNK1) == 0) THEN
    IF(ASSOCIATED(node)) THEN
      SELECTTYPE(ptr => node%val); TYPE IS(Param_SNK1)
        val=ptr%p
      ENDSELECT
    ELSE
      val=default
    ENDIF
  ENDIF

ENDSUBROUTINE getSNK1
!
!-------------------------------------------------------------------------------
!> @brief Gets a rank-2 parameter of type INTEGER(SNK)
!> @param this the node to get data from
!> @param name the name of the parameter to get
!> @param val the values retrieved from the node
!> @param default the value to assign to @c val if the parameter is not found; optional
!>
!> If the parameter is not found and @c default is not provided, an error is thrown.
!>
SUBROUTINE getSNK2(this,name,val,default)
  CLASS(ParamType),INTENT(IN) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  INTEGER(SNK),ALLOCATABLE,INTENT(OUT) :: val(:,:)
  INTEGER(SNK),INTENT(IN),OPTIONAL :: default(:,:)
  !
  CLASS(ParamNode),POINTER :: node

  CALL this%getNode_Name(name,node)
  IF(errorChecks_get(node,PRESENT(default),name,PL_DATA_TYPE_SNK2) == 0) THEN
    IF(ASSOCIATED(node)) THEN
      SELECTTYPE(ptr => node%val); TYPE IS(Param_SNK2)
        val=ptr%p
      ENDSELECT
    ELSE
      val=default
    ENDIF
  ENDIF

ENDSUBROUTINE getSNK2
!
!-------------------------------------------------------------------------------
!> @brief Gets a rank-3 parameter of type INTEGER(SNK)
!> @param this the node to get data from
!> @param name the name of the parameter to get
!> @param val the values retrieved from the node
!> @param default the value to assign to @c val if the parameter is not found; optional
!>
!> If the parameter is not found and @c default is not provided, an error is thrown.
!>
SUBROUTINE getSNK3(this,name,val,default)
  CLASS(ParamType),INTENT(IN) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  INTEGER(SNK),ALLOCATABLE,INTENT(OUT) :: val(:,:,:)
  INTEGER(SNK),INTENT(IN),OPTIONAL :: default(:,:,:)
  !
  CLASS(ParamNode),POINTER :: node

  CALL this%getNode_Name(name,node)
  IF(errorChecks_get(node,PRESENT(default),name,PL_DATA_TYPE_SNK3) == 0) THEN
    IF(ASSOCIATED(node)) THEN
      SELECTTYPE(ptr => node%val); TYPE IS(Param_SNK3)
        val=ptr%p
      ENDSELECT
    ELSE
      val=default
    ENDIF
  ENDIF

ENDSUBROUTINE getSNK3
!
!-------------------------------------------------------------------------------
!> @brief Gets a rank-4 parameter of type INTEGER(SNK)
!> @param this the node to get data from
!> @param name the name of the parameter to get
!> @param val the values retrieved from the node
!> @param default the value to assign to @c val if the parameter is not found; optional
!>
!> If the parameter is not found and @c default is not provided, an error is thrown.
!>
SUBROUTINE getSNK4(this,name,val,default)
  CLASS(ParamType),INTENT(IN) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  INTEGER(SNK),ALLOCATABLE,INTENT(OUT) :: val(:,:,:,:)
  INTEGER(SNK),INTENT(IN),OPTIONAL :: default(:,:,:,:)
  !
  CLASS(ParamNode),POINTER :: node

  CALL this%getNode_Name(name,node)
  IF(errorChecks_get(node,PRESENT(default),name,PL_DATA_TYPE_SNK4) == 0) THEN
    IF(ASSOCIATED(node)) THEN
      SELECTTYPE(ptr => node%val); TYPE IS(Param_SNK4)
        val=ptr%p
      ENDSELECT
    ELSE
      val=default
    ENDIF
  ENDIF

ENDSUBROUTINE getSNK4
!
!-------------------------------------------------------------------------------
!> @brief Gets a rank-5 parameter of type INTEGER(SNK)
!> @param this the node to get data from
!> @param name the name of the parameter to get
!> @param val the values retrieved from the node
!> @param default the value to assign to @c val if the parameter is not found; optional
!>
!> If the parameter is not found and @c default is not provided, an error is thrown.
!>
SUBROUTINE getSNK5(this,name,val,default)
  CLASS(ParamType),INTENT(IN) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  INTEGER(SNK),ALLOCATABLE,INTENT(OUT) :: val(:,:,:,:,:)
  INTEGER(SNK),INTENT(IN),OPTIONAL :: default(:,:,:,:,:)
  !
  CLASS(ParamNode),POINTER :: node

  CALL this%getNode_Name(name,node)
  IF(errorChecks_get(node,PRESENT(default),name,PL_DATA_TYPE_SNK5) == 0) THEN
    IF(ASSOCIATED(node)) THEN
      SELECTTYPE(ptr => node%val); TYPE IS(Param_SNK5)
        val=ptr%p
      ENDSELECT
    ELSE
      val=default
    ENDIF
  ENDIF

ENDSUBROUTINE getSNK5
!
!-------------------------------------------------------------------------------
!> @brief Gets a rank-6 parameter of type INTEGER(SNK)
!> @param this the node to get data from
!> @param name the name of the parameter to get
!> @param val the values retrieved from the node
!> @param default the value to assign to @c val if the parameter is not found; optional
!>
!> If the parameter is not found and @c default is not provided, an error is thrown.
!>
SUBROUTINE getSNK6(this,name,val,default)
  CLASS(ParamType),INTENT(IN) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  INTEGER(SNK),ALLOCATABLE,INTENT(OUT) :: val(:,:,:,:,:,:)
  INTEGER(SNK),INTENT(IN),OPTIONAL :: default(:,:,:,:,:,:)
  !
  CLASS(ParamNode),POINTER :: node

  CALL this%getNode_Name(name,node)
  IF(errorChecks_get(node,PRESENT(default),name,PL_DATA_TYPE_SNK6) == 0) THEN
    IF(ASSOCIATED(node)) THEN
      SELECTTYPE(ptr => node%val); TYPE IS(Param_SNK6)
        val=ptr%p
      ENDSELECT
    ELSE
      val=default
    ENDIF
  ENDIF

ENDSUBROUTINE getSNK6
!
!-------------------------------------------------------------------------------
!> @brief Gets a rank-7 parameter of type INTEGER(SNK)
!> @param this the node to get data from
!> @param name the name of the parameter to get
!> @param val the values retrieved from the node
!> @param default the value to assign to @c val if the parameter is not found; optional
!>
!> If the parameter is not found and @c default is not provided, an error is thrown.
!>
SUBROUTINE getSNK7(this,name,val,default)
  CLASS(ParamType),INTENT(IN) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  INTEGER(SNK),ALLOCATABLE,INTENT(OUT) :: val(:,:,:,:,:,:,:)
  INTEGER(SNK),INTENT(IN),OPTIONAL :: default(:,:,:,:,:,:,:)
  !
  CLASS(ParamNode),POINTER :: node

  CALL this%getNode_Name(name,node)
  IF(errorChecks_get(node,PRESENT(default),name,PL_DATA_TYPE_SNK7) == 0) THEN
    IF(ASSOCIATED(node)) THEN
      SELECTTYPE(ptr => node%val); TYPE IS(Param_SNK7)
        val=ptr%p
      ENDSELECT
    ELSE
      val=default
    ENDIF
  ENDIF

ENDSUBROUTINE getSNK7
!
!-------------------------------------------------------------------------------
!> @brief Gets a rank-0 parameter of type INTEGER(SLK)
!> @param this the node to get data from
!> @param name the name of the parameter to get
!> @param val the values retrieved from the node
!> @param default the value to assign to @c val if the parameter is not found; optional
!>
!> If the parameter is not found and @c default is not provided, an error is thrown.
!>
SUBROUTINE getSLK0(this,name,val,default)
  CLASS(ParamType),INTENT(IN) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  INTEGER(SLK),INTENT(OUT) :: val
  INTEGER(SLK),INTENT(IN),OPTIONAL :: default
  !
  CLASS(ParamNode),POINTER :: node

  CALL this%getNode_Name(name,node)
  IF(errorChecks_get(node,PRESENT(default),name,PL_DATA_TYPE_SLK0) == 0) THEN
    IF(ASSOCIATED(node)) THEN
      SELECTTYPE(ptr => node%val); TYPE IS(Param_SLK0)
        val=ptr%p
      ENDSELECT
    ELSE
      val=default
    ENDIF
  ENDIF

ENDSUBROUTINE getSLK0
!
!-------------------------------------------------------------------------------
!> @brief Gets a rank-1 parameter of type INTEGER(SLK)
!> @param this the node to get data from
!> @param name the name of the parameter to get
!> @param val the values retrieved from the node
!> @param default the value to assign to @c val if the parameter is not found; optional
!>
!> If the parameter is not found and @c default is not provided, an error is thrown.
!>
SUBROUTINE getSLK1(this,name,val,default)
  CLASS(ParamType),INTENT(IN) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  INTEGER(SLK),ALLOCATABLE,INTENT(OUT) :: val(:)
  INTEGER(SLK),INTENT(IN),OPTIONAL :: default(:)
  !
  CLASS(ParamNode),POINTER :: node

  CALL this%getNode_Name(name,node)
  IF(errorChecks_get(node,PRESENT(default),name,PL_DATA_TYPE_SLK1) == 0) THEN
    IF(ASSOCIATED(node)) THEN
      SELECTTYPE(ptr => node%val); TYPE IS(Param_SLK1)
        val=ptr%p
      ENDSELECT
    ELSE
      val=default
    ENDIF
  ENDIF

ENDSUBROUTINE getSLK1
!
!-------------------------------------------------------------------------------
!> @brief Gets a rank-2 parameter of type INTEGER(SLK)
!> @param this the node to get data from
!> @param name the name of the parameter to get
!> @param val the values retrieved from the node
!> @param default the value to assign to @c val if the parameter is not found; optional
!>
!> If the parameter is not found and @c default is not provided, an error is thrown.
!>
SUBROUTINE getSLK2(this,name,val,default)
  CLASS(ParamType),INTENT(IN) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  INTEGER(SLK),ALLOCATABLE,INTENT(OUT) :: val(:,:)
  INTEGER(SLK),INTENT(IN),OPTIONAL :: default(:,:)
  !
  CLASS(ParamNode),POINTER :: node

  CALL this%getNode_Name(name,node)
  IF(errorChecks_get(node,PRESENT(default),name,PL_DATA_TYPE_SLK2) == 0) THEN
    IF(ASSOCIATED(node)) THEN
      SELECTTYPE(ptr => node%val); TYPE IS(Param_SLK2)
        val=ptr%p
      ENDSELECT
    ELSE
      val=default
    ENDIF
  ENDIF

ENDSUBROUTINE getSLK2
!
!-------------------------------------------------------------------------------
!> @brief Gets a rank-3 parameter of type INTEGER(SLK)
!> @param this the node to get data from
!> @param name the name of the parameter to get
!> @param val the values retrieved from the node
!> @param default the value to assign to @c val if the parameter is not found; optional
!>
!> If the parameter is not found and @c default is not provided, an error is thrown.
!>
SUBROUTINE getSLK3(this,name,val,default)
  CLASS(ParamType),INTENT(IN) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  INTEGER(SLK),ALLOCATABLE,INTENT(OUT) :: val(:,:,:)
  INTEGER(SLK),INTENT(IN),OPTIONAL :: default(:,:,:)
  !
  CLASS(ParamNode),POINTER :: node

  CALL this%getNode_Name(name,node)
  IF(errorChecks_get(node,PRESENT(default),name,PL_DATA_TYPE_SLK3) == 0) THEN
    IF(ASSOCIATED(node)) THEN
      SELECTTYPE(ptr => node%val); TYPE IS(Param_SLK3)
        val=ptr%p
      ENDSELECT
    ELSE
      val=default
    ENDIF
  ENDIF

ENDSUBROUTINE getSLK3
!
!-------------------------------------------------------------------------------
!> @brief Gets a rank-4 parameter of type INTEGER(SLK)
!> @param this the node to get data from
!> @param name the name of the parameter to get
!> @param val the values retrieved from the node
!> @param default the value to assign to @c val if the parameter is not found; optional
!>
!> If the parameter is not found and @c default is not provided, an error is thrown.
!>
SUBROUTINE getSLK4(this,name,val,default)
  CLASS(ParamType),INTENT(IN) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  INTEGER(SLK),ALLOCATABLE,INTENT(OUT) :: val(:,:,:,:)
  INTEGER(SLK),INTENT(IN),OPTIONAL :: default(:,:,:,:)
  !
  CLASS(ParamNode),POINTER :: node

  CALL this%getNode_Name(name,node)
  IF(errorChecks_get(node,PRESENT(default),name,PL_DATA_TYPE_SLK4) == 0) THEN
    IF(ASSOCIATED(node)) THEN
      SELECTTYPE(ptr => node%val); TYPE IS(Param_SLK4)
        val=ptr%p
      ENDSELECT
    ELSE
      val=default
    ENDIF
  ENDIF

ENDSUBROUTINE getSLK4
!
!-------------------------------------------------------------------------------
!> @brief Gets a rank-5 parameter of type INTEGER(SLK)
!> @param this the node to get data from
!> @param name the name of the parameter to get
!> @param val the values retrieved from the node
!> @param default the value to assign to @c val if the parameter is not found; optional
!>
!> If the parameter is not found and @c default is not provided, an error is thrown.
!>
SUBROUTINE getSLK5(this,name,val,default)
  CLASS(ParamType),INTENT(IN) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  INTEGER(SLK),ALLOCATABLE,INTENT(OUT) :: val(:,:,:,:,:)
  INTEGER(SLK),INTENT(IN),OPTIONAL :: default(:,:,:,:,:)
  !
  CLASS(ParamNode),POINTER :: node

  CALL this%getNode_Name(name,node)
  IF(errorChecks_get(node,PRESENT(default),name,PL_DATA_TYPE_SLK5) == 0) THEN
    IF(ASSOCIATED(node)) THEN
      SELECTTYPE(ptr => node%val); TYPE IS(Param_SLK5)
        val=ptr%p
      ENDSELECT
    ELSE
      val=default
    ENDIF
  ENDIF

ENDSUBROUTINE getSLK5
!
!-------------------------------------------------------------------------------
!> @brief Gets a rank-6 parameter of type INTEGER(SLK)
!> @param this the node to get data from
!> @param name the name of the parameter to get
!> @param val the values retrieved from the node
!> @param default the value to assign to @c val if the parameter is not found; optional
!>
!> If the parameter is not found and @c default is not provided, an error is thrown.
!>
SUBROUTINE getSLK6(this,name,val,default)
  CLASS(ParamType),INTENT(IN) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  INTEGER(SLK),ALLOCATABLE,INTENT(OUT) :: val(:,:,:,:,:,:)
  INTEGER(SLK),INTENT(IN),OPTIONAL :: default(:,:,:,:,:,:)
  !
  CLASS(ParamNode),POINTER :: node

  CALL this%getNode_Name(name,node)
  IF(errorChecks_get(node,PRESENT(default),name,PL_DATA_TYPE_SLK6) == 0) THEN
    IF(ASSOCIATED(node)) THEN
      SELECTTYPE(ptr => node%val); TYPE IS(Param_SLK6)
        val=ptr%p
      ENDSELECT
    ELSE
      val=default
    ENDIF
  ENDIF

ENDSUBROUTINE getSLK6
!
!-------------------------------------------------------------------------------
!> @brief Gets a rank-7 parameter of type INTEGER(SLK)
!> @param this the node to get data from
!> @param name the name of the parameter to get
!> @param val the values retrieved from the node
!> @param default the value to assign to @c val if the parameter is not found; optional
!>
!> If the parameter is not found and @c default is not provided, an error is thrown.
!>
SUBROUTINE getSLK7(this,name,val,default)
  CLASS(ParamType),INTENT(IN) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  INTEGER(SLK),ALLOCATABLE,INTENT(OUT) :: val(:,:,:,:,:,:,:)
  INTEGER(SLK),INTENT(IN),OPTIONAL :: default(:,:,:,:,:,:,:)
  !
  CLASS(ParamNode),POINTER :: node

  CALL this%getNode_Name(name,node)
  IF(errorChecks_get(node,PRESENT(default),name,PL_DATA_TYPE_SLK7) == 0) THEN
    IF(ASSOCIATED(node)) THEN
      SELECTTYPE(ptr => node%val); TYPE IS(Param_SLK7)
        val=ptr%p
      ENDSELECT
    ELSE
      val=default
    ENDIF
  ENDIF

ENDSUBROUTINE getSLK7
!
!-------------------------------------------------------------------------------
!> @brief Gets a rank-0 parameter of type REAL(SSK)
!> @param this the node to get data from
!> @param name the name of the parameter to get
!> @param val the values retrieved from the node
!> @param default the value to assign to @c val if the parameter is not found; optional
!>
!> If the parameter is not found and @c default is not provided, an error is thrown.
!>
SUBROUTINE getSSK0(this,name,val,default)
  CLASS(ParamType),INTENT(IN) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  REAL(SSK),INTENT(OUT) :: val
  REAL(SSK),INTENT(IN),OPTIONAL :: default
  !
  CLASS(ParamNode),POINTER :: node

  CALL this%getNode_Name(name,node)
  IF(errorChecks_get(node,PRESENT(default),name,PL_DATA_TYPE_SSK0) == 0) THEN
    IF(ASSOCIATED(node)) THEN
      SELECTTYPE(ptr => node%val); TYPE IS(Param_SSK0)
        val=ptr%p
      ENDSELECT
    ELSE
      val=default
    ENDIF
  ENDIF

ENDSUBROUTINE getSSK0
!
!-------------------------------------------------------------------------------
!> @brief Gets a rank-1 parameter of type REAL(SSK)
!> @param this the node to get data from
!> @param name the name of the parameter to get
!> @param val the values retrieved from the node
!> @param default the value to assign to @c val if the parameter is not found; optional
!>
!> If the parameter is not found and @c default is not provided, an error is thrown.
!>
SUBROUTINE getSSK1(this,name,val,default)
  CLASS(ParamType),INTENT(IN) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  REAL(SSK),ALLOCATABLE,INTENT(OUT) :: val(:)
  REAL(SSK),INTENT(IN),OPTIONAL :: default(:)
  !
  CLASS(ParamNode),POINTER :: node

  CALL this%getNode_Name(name,node)
  IF(errorChecks_get(node,PRESENT(default),name,PL_DATA_TYPE_SSK1) == 0) THEN
    IF(ASSOCIATED(node)) THEN
      SELECTTYPE(ptr => node%val); TYPE IS(Param_SSK1)
        val=ptr%p
      ENDSELECT
    ELSE
      val=default
    ENDIF
  ENDIF

ENDSUBROUTINE getSSK1
!
!-------------------------------------------------------------------------------
!> @brief Gets a rank-2 parameter of type REAL(SSK)
!> @param this the node to get data from
!> @param name the name of the parameter to get
!> @param val the values retrieved from the node
!> @param default the value to assign to @c val if the parameter is not found; optional
!>
!> If the parameter is not found and @c default is not provided, an error is thrown.
!>
SUBROUTINE getSSK2(this,name,val,default)
  CLASS(ParamType),INTENT(IN) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  REAL(SSK),ALLOCATABLE,INTENT(OUT) :: val(:,:)
  REAL(SSK),INTENT(IN),OPTIONAL :: default(:,:)
  !
  CLASS(ParamNode),POINTER :: node

  CALL this%getNode_Name(name,node)
  IF(errorChecks_get(node,PRESENT(default),name,PL_DATA_TYPE_SSK2) == 0) THEN
    IF(ASSOCIATED(node)) THEN
      SELECTTYPE(ptr => node%val); TYPE IS(Param_SSK2)
        val=ptr%p
      ENDSELECT
    ELSE
      val=default
    ENDIF
  ENDIF

ENDSUBROUTINE getSSK2
!
!-------------------------------------------------------------------------------
!> @brief Gets a rank-3 parameter of type REAL(SSK)
!> @param this the node to get data from
!> @param name the name of the parameter to get
!> @param val the values retrieved from the node
!> @param default the value to assign to @c val if the parameter is not found; optional
!>
!> If the parameter is not found and @c default is not provided, an error is thrown.
!>
SUBROUTINE getSSK3(this,name,val,default)
  CLASS(ParamType),INTENT(IN) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  REAL(SSK),ALLOCATABLE,INTENT(OUT) :: val(:,:,:)
  REAL(SSK),INTENT(IN),OPTIONAL :: default(:,:,:)
  !
  CLASS(ParamNode),POINTER :: node

  CALL this%getNode_Name(name,node)
  IF(errorChecks_get(node,PRESENT(default),name,PL_DATA_TYPE_SSK3) == 0) THEN
    IF(ASSOCIATED(node)) THEN
      SELECTTYPE(ptr => node%val); TYPE IS(Param_SSK3)
        val=ptr%p
      ENDSELECT
    ELSE
      val=default
    ENDIF
  ENDIF

ENDSUBROUTINE getSSK3
!
!-------------------------------------------------------------------------------
!> @brief Gets a rank-4 parameter of type REAL(SSK)
!> @param this the node to get data from
!> @param name the name of the parameter to get
!> @param val the values retrieved from the node
!> @param default the value to assign to @c val if the parameter is not found; optional
!>
!> If the parameter is not found and @c default is not provided, an error is thrown.
!>
SUBROUTINE getSSK4(this,name,val,default)
  CLASS(ParamType),INTENT(IN) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  REAL(SSK),ALLOCATABLE,INTENT(OUT) :: val(:,:,:,:)
  REAL(SSK),INTENT(IN),OPTIONAL :: default(:,:,:,:)
  !
  CLASS(ParamNode),POINTER :: node

  CALL this%getNode_Name(name,node)
  IF(errorChecks_get(node,PRESENT(default),name,PL_DATA_TYPE_SSK4) == 0) THEN
    IF(ASSOCIATED(node)) THEN
      SELECTTYPE(ptr => node%val); TYPE IS(Param_SSK4)
        val=ptr%p
      ENDSELECT
    ELSE
      val=default
    ENDIF
  ENDIF

ENDSUBROUTINE getSSK4
!
!-------------------------------------------------------------------------------
!> @brief Gets a rank-5 parameter of type REAL(SSK)
!> @param this the node to get data from
!> @param name the name of the parameter to get
!> @param val the values retrieved from the node
!> @param default the value to assign to @c val if the parameter is not found; optional
!>
!> If the parameter is not found and @c default is not provided, an error is thrown.
!>
SUBROUTINE getSSK5(this,name,val,default)
  CLASS(ParamType),INTENT(IN) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  REAL(SSK),ALLOCATABLE,INTENT(OUT) :: val(:,:,:,:,:)
  REAL(SSK),INTENT(IN),OPTIONAL :: default(:,:,:,:,:)
  !
  CLASS(ParamNode),POINTER :: node

  CALL this%getNode_Name(name,node)
  IF(errorChecks_get(node,PRESENT(default),name,PL_DATA_TYPE_SSK5) == 0) THEN
    IF(ASSOCIATED(node)) THEN
      SELECTTYPE(ptr => node%val); TYPE IS(Param_SSK5)
        val=ptr%p
      ENDSELECT
    ELSE
      val=default
    ENDIF
  ENDIF

ENDSUBROUTINE getSSK5
!
!-------------------------------------------------------------------------------
!> @brief Gets a rank-6 parameter of type REAL(SSK)
!> @param this the node to get data from
!> @param name the name of the parameter to get
!> @param val the values retrieved from the node
!> @param default the value to assign to @c val if the parameter is not found; optional
!>
!> If the parameter is not found and @c default is not provided, an error is thrown.
!>
SUBROUTINE getSSK6(this,name,val,default)
  CLASS(ParamType),INTENT(IN) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  REAL(SSK),ALLOCATABLE,INTENT(OUT) :: val(:,:,:,:,:,:)
  REAL(SSK),INTENT(IN),OPTIONAL :: default(:,:,:,:,:,:)
  !
  CLASS(ParamNode),POINTER :: node

  CALL this%getNode_Name(name,node)
  IF(errorChecks_get(node,PRESENT(default),name,PL_DATA_TYPE_SSK6) == 0) THEN
    IF(ASSOCIATED(node)) THEN
      SELECTTYPE(ptr => node%val); TYPE IS(Param_SSK6)
        val=ptr%p
      ENDSELECT
    ELSE
      val=default
    ENDIF
  ENDIF

ENDSUBROUTINE getSSK6
!
!-------------------------------------------------------------------------------
!> @brief Gets a rank-7 parameter of type REAL(SSK)
!> @param this the node to get data from
!> @param name the name of the parameter to get
!> @param val the values retrieved from the node
!> @param default the value to assign to @c val if the parameter is not found; optional
!>
!> If the parameter is not found and @c default is not provided, an error is thrown.
!>
SUBROUTINE getSSK7(this,name,val,default)
  CLASS(ParamType),INTENT(IN) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  REAL(SSK),ALLOCATABLE,INTENT(OUT) :: val(:,:,:,:,:,:,:)
  REAL(SSK),INTENT(IN),OPTIONAL :: default(:,:,:,:,:,:,:)
  !
  CLASS(ParamNode),POINTER :: node

  CALL this%getNode_Name(name,node)
  IF(errorChecks_get(node,PRESENT(default),name,PL_DATA_TYPE_SSK7) == 0) THEN
    IF(ASSOCIATED(node)) THEN
      SELECTTYPE(ptr => node%val); TYPE IS(Param_SSK7)
        val=ptr%p
      ENDSELECT
    ELSE
      val=default
    ENDIF
  ENDIF

ENDSUBROUTINE getSSK7
!
!-------------------------------------------------------------------------------
!> @brief Gets a rank-0 parameter of type REAL(SDK)
!> @param this the node to get data from
!> @param name the name of the parameter to get
!> @param val the values retrieved from the node
!> @param default the value to assign to @c val if the parameter is not found; optional
!>
!> If the parameter is not found and @c default is not provided, an error is thrown.
!>
SUBROUTINE getSDK0(this,name,val,default)
  CLASS(ParamType),INTENT(IN) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  REAL(SDK),INTENT(OUT) :: val
  REAL(SDK),INTENT(IN),OPTIONAL :: default
  !
  CLASS(ParamNode),POINTER :: node

  CALL this%getNode_Name(name,node)
  IF(errorChecks_get(node,PRESENT(default),name,PL_DATA_TYPE_SDK0) == 0) THEN
    IF(ASSOCIATED(node)) THEN
      SELECTTYPE(ptr => node%val); TYPE IS(Param_SDK0)
        val=ptr%p
      ENDSELECT
    ELSE
      val=default
    ENDIF
  ENDIF

ENDSUBROUTINE getSDK0
!
!-------------------------------------------------------------------------------
!> @brief Gets a rank-1 parameter of type REAL(SDK)
!> @param this the node to get data from
!> @param name the name of the parameter to get
!> @param val the values retrieved from the node
!> @param default the value to assign to @c val if the parameter is not found; optional
!>
!> If the parameter is not found and @c default is not provided, an error is thrown.
!>
SUBROUTINE getSDK1(this,name,val,default)
  CLASS(ParamType),INTENT(IN) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  REAL(SDK),ALLOCATABLE,INTENT(OUT) :: val(:)
  REAL(SDK),INTENT(IN),OPTIONAL :: default(:)
  !
  CLASS(ParamNode),POINTER :: node

  CALL this%getNode_Name(name,node)
  IF(errorChecks_get(node,PRESENT(default),name,PL_DATA_TYPE_SDK1) == 0) THEN
    IF(ASSOCIATED(node)) THEN
      SELECTTYPE(ptr => node%val); TYPE IS(Param_SDK1)
        val=ptr%p
      ENDSELECT
    ELSE
      val=default
    ENDIF
  ENDIF

ENDSUBROUTINE getSDK1
!
!-------------------------------------------------------------------------------
!> @brief Gets a rank-2 parameter of type REAL(SDK)
!> @param this the node to get data from
!> @param name the name of the parameter to get
!> @param val the values retrieved from the node
!> @param default the value to assign to @c val if the parameter is not found; optional
!>
!> If the parameter is not found and @c default is not provided, an error is thrown.
!>
SUBROUTINE getSDK2(this,name,val,default)
  CLASS(ParamType),INTENT(IN) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  REAL(SDK),ALLOCATABLE,INTENT(OUT) :: val(:,:)
  REAL(SDK),INTENT(IN),OPTIONAL :: default(:,:)
  !
  CLASS(ParamNode),POINTER :: node

  CALL this%getNode_Name(name,node)
  IF(errorChecks_get(node,PRESENT(default),name,PL_DATA_TYPE_SDK2) == 0) THEN
    IF(ASSOCIATED(node)) THEN
      SELECTTYPE(ptr => node%val); TYPE IS(Param_SDK2)
        val=ptr%p
      ENDSELECT
    ELSE
      val=default
    ENDIF
  ENDIF

ENDSUBROUTINE getSDK2
!
!-------------------------------------------------------------------------------
!> @brief Gets a rank-3 parameter of type REAL(SDK)
!> @param this the node to get data from
!> @param name the name of the parameter to get
!> @param val the values retrieved from the node
!> @param default the value to assign to @c val if the parameter is not found; optional
!>
!> If the parameter is not found and @c default is not provided, an error is thrown.
!>
SUBROUTINE getSDK3(this,name,val,default)
  CLASS(ParamType),INTENT(IN) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  REAL(SDK),ALLOCATABLE,INTENT(OUT) :: val(:,:,:)
  REAL(SDK),INTENT(IN),OPTIONAL :: default(:,:,:)
  !
  CLASS(ParamNode),POINTER :: node

  CALL this%getNode_Name(name,node)
  IF(errorChecks_get(node,PRESENT(default),name,PL_DATA_TYPE_SDK3) == 0) THEN
    IF(ASSOCIATED(node)) THEN
      SELECTTYPE(ptr => node%val); TYPE IS(Param_SDK3)
        val=ptr%p
      ENDSELECT
    ELSE
      val=default
    ENDIF
  ENDIF

ENDSUBROUTINE getSDK3
!
!-------------------------------------------------------------------------------
!> @brief Gets a rank-4 parameter of type REAL(SDK)
!> @param this the node to get data from
!> @param name the name of the parameter to get
!> @param val the values retrieved from the node
!> @param default the value to assign to @c val if the parameter is not found; optional
!>
!> If the parameter is not found and @c default is not provided, an error is thrown.
!>
SUBROUTINE getSDK4(this,name,val,default)
  CLASS(ParamType),INTENT(IN) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  REAL(SDK),ALLOCATABLE,INTENT(OUT) :: val(:,:,:,:)
  REAL(SDK),INTENT(IN),OPTIONAL :: default(:,:,:,:)
  !
  CLASS(ParamNode),POINTER :: node

  CALL this%getNode_Name(name,node)
  IF(errorChecks_get(node,PRESENT(default),name,PL_DATA_TYPE_SDK4) == 0) THEN
    IF(ASSOCIATED(node)) THEN
      SELECTTYPE(ptr => node%val); TYPE IS(Param_SDK4)
        val=ptr%p
      ENDSELECT
    ELSE
      val=default
    ENDIF
  ENDIF

ENDSUBROUTINE getSDK4
!
!-------------------------------------------------------------------------------
!> @brief Gets a rank-5 parameter of type REAL(SDK)
!> @param this the node to get data from
!> @param name the name of the parameter to get
!> @param val the values retrieved from the node
!> @param default the value to assign to @c val if the parameter is not found; optional
!>
!> If the parameter is not found and @c default is not provided, an error is thrown.
!>
SUBROUTINE getSDK5(this,name,val,default)
  CLASS(ParamType),INTENT(IN) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  REAL(SDK),ALLOCATABLE,INTENT(OUT) :: val(:,:,:,:,:)
  REAL(SDK),INTENT(IN),OPTIONAL :: default(:,:,:,:,:)
  !
  CLASS(ParamNode),POINTER :: node

  CALL this%getNode_Name(name,node)
  IF(errorChecks_get(node,PRESENT(default),name,PL_DATA_TYPE_SDK5) == 0) THEN
    IF(ASSOCIATED(node)) THEN
      SELECTTYPE(ptr => node%val); TYPE IS(Param_SDK5)
        val=ptr%p
      ENDSELECT
    ELSE
      val=default
    ENDIF
  ENDIF

ENDSUBROUTINE getSDK5
!
!-------------------------------------------------------------------------------
!> @brief Gets a rank-6 parameter of type REAL(SDK)
!> @param this the node to get data from
!> @param name the name of the parameter to get
!> @param val the values retrieved from the node
!> @param default the value to assign to @c val if the parameter is not found; optional
!>
!> If the parameter is not found and @c default is not provided, an error is thrown.
!>
SUBROUTINE getSDK6(this,name,val,default)
  CLASS(ParamType),INTENT(IN) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  REAL(SDK),ALLOCATABLE,INTENT(OUT) :: val(:,:,:,:,:,:)
  REAL(SDK),INTENT(IN),OPTIONAL :: default(:,:,:,:,:,:)
  !
  CLASS(ParamNode),POINTER :: node

  CALL this%getNode_Name(name,node)
  IF(errorChecks_get(node,PRESENT(default),name,PL_DATA_TYPE_SDK6) == 0) THEN
    IF(ASSOCIATED(node)) THEN
      SELECTTYPE(ptr => node%val); TYPE IS(Param_SDK6)
        val=ptr%p
      ENDSELECT
    ELSE
      val=default
    ENDIF
  ENDIF

ENDSUBROUTINE getSDK6
!
!-------------------------------------------------------------------------------
!> @brief Gets a rank-7 parameter of type REAL(SDK)
!> @param this the node to get data from
!> @param name the name of the parameter to get
!> @param val the values retrieved from the node
!> @param default the value to assign to @c val if the parameter is not found; optional
!>
!> If the parameter is not found and @c default is not provided, an error is thrown.
!>
SUBROUTINE getSDK7(this,name,val,default)
  CLASS(ParamType),INTENT(IN) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  REAL(SDK),ALLOCATABLE,INTENT(OUT) :: val(:,:,:,:,:,:,:)
  REAL(SDK),INTENT(IN),OPTIONAL :: default(:,:,:,:,:,:,:)
  !
  CLASS(ParamNode),POINTER :: node

  CALL this%getNode_Name(name,node)
  IF(errorChecks_get(node,PRESENT(default),name,PL_DATA_TYPE_SDK7) == 0) THEN
    IF(ASSOCIATED(node)) THEN
      SELECTTYPE(ptr => node%val); TYPE IS(Param_SDK7)
        val=ptr%p
      ENDSELECT
    ELSE
      val=default
    ENDIF
  ENDIF

ENDSUBROUTINE getSDK7
!
!-------------------------------------------------------------------------------
!> @brief Gets a rank-0 parameter of type TYPE(StringType)
!> @param this the node to get data from
!> @param name the name of the parameter to get
!> @param val the values retrieved from the node
!> @param default the value to assign to @c val if the parameter is not found; optional
!>
!> If the parameter is not found and @c default is not provided, an error is thrown.
!>
SUBROUTINE getSTR0(this,name,val,default)
  CLASS(ParamType),INTENT(IN) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  TYPE(StringType),INTENT(OUT) :: val
  TYPE(StringType),INTENT(IN),OPTIONAL :: default
  !
  CLASS(ParamNode),POINTER :: node

  CALL this%getNode_Name(name,node)
  IF(errorChecks_get(node,PRESENT(default),name,PL_DATA_TYPE_STR0) == 0) THEN
    IF(ASSOCIATED(node)) THEN
      SELECTTYPE(ptr => node%val); TYPE IS(Param_STR0)
        val=ptr%p
      ENDSELECT
    ELSE
      val=default
    ENDIF
  ENDIF

ENDSUBROUTINE getSTR0
!
!-------------------------------------------------------------------------------
!> @brief Gets a rank-1 parameter of type TYPE(StringType)
!> @param this the node to get data from
!> @param name the name of the parameter to get
!> @param val the values retrieved from the node
!> @param default the value to assign to @c val if the parameter is not found; optional
!>
!> If the parameter is not found and @c default is not provided, an error is thrown.
!>
SUBROUTINE getSTR1(this,name,val,default)
  CLASS(ParamType),INTENT(IN) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  TYPE(StringType),ALLOCATABLE,INTENT(OUT) :: val(:)
  TYPE(StringType),INTENT(IN),OPTIONAL :: default(:)
  !
  CLASS(ParamNode),POINTER :: node

  CALL this%getNode_Name(name,node)
  IF(errorChecks_get(node,PRESENT(default),name,PL_DATA_TYPE_STR1) == 0) THEN
    IF(ASSOCIATED(node)) THEN
      SELECTTYPE(ptr => node%val); TYPE IS(Param_STR1)
        val=ptr%p
      ENDSELECT
    ELSE
      val=default
    ENDIF
  ENDIF

ENDSUBROUTINE getSTR1
!
!-------------------------------------------------------------------------------
!> @brief Gets a rank-2 parameter of type TYPE(StringType)
!> @param this the node to get data from
!> @param name the name of the parameter to get
!> @param val the values retrieved from the node
!> @param default the value to assign to @c val if the parameter is not found; optional
!>
!> If the parameter is not found and @c default is not provided, an error is thrown.
!>
SUBROUTINE getSTR2(this,name,val,default)
  CLASS(ParamType),INTENT(IN) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  TYPE(StringType),ALLOCATABLE,INTENT(OUT) :: val(:,:)
  TYPE(StringType),INTENT(IN),OPTIONAL :: default(:,:)
  !
  CLASS(ParamNode),POINTER :: node

  CALL this%getNode_Name(name,node)
  IF(errorChecks_get(node,PRESENT(default),name,PL_DATA_TYPE_STR2) == 0) THEN
    IF(ASSOCIATED(node)) THEN
      SELECTTYPE(ptr => node%val); TYPE IS(Param_STR2)
        val=ptr%p
      ENDSELECT
    ELSE
      val=default
    ENDIF
  ENDIF

ENDSUBROUTINE getSTR2
!
!-------------------------------------------------------------------------------
!> @brief Gets a rank-3 parameter of type TYPE(StringType)
!> @param this the node to get data from
!> @param name the name of the parameter to get
!> @param val the values retrieved from the node
!> @param default the value to assign to @c val if the parameter is not found; optional
!>
!> If the parameter is not found and @c default is not provided, an error is thrown.
!>
SUBROUTINE getSTR3(this,name,val,default)
  CLASS(ParamType),INTENT(IN) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  TYPE(StringType),ALLOCATABLE,INTENT(OUT) :: val(:,:,:)
  TYPE(StringType),INTENT(IN),OPTIONAL :: default(:,:,:)
  !
  CLASS(ParamNode),POINTER :: node

  CALL this%getNode_Name(name,node)
  IF(errorChecks_get(node,PRESENT(default),name,PL_DATA_TYPE_STR3) == 0) THEN
    IF(ASSOCIATED(node)) THEN
      SELECTTYPE(ptr => node%val); TYPE IS(Param_STR3)
        val=ptr%p
      ENDSELECT
    ELSE
      val=default
    ENDIF
  ENDIF

ENDSUBROUTINE getSTR3
!
!-------------------------------------------------------------------------------
!> @brief Gets a rank-4 parameter of type TYPE(StringType)
!> @param this the node to get data from
!> @param name the name of the parameter to get
!> @param val the values retrieved from the node
!> @param default the value to assign to @c val if the parameter is not found; optional
!>
!> If the parameter is not found and @c default is not provided, an error is thrown.
!>
SUBROUTINE getSTR4(this,name,val,default)
  CLASS(ParamType),INTENT(IN) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  TYPE(StringType),ALLOCATABLE,INTENT(OUT) :: val(:,:,:,:)
  TYPE(StringType),INTENT(IN),OPTIONAL :: default(:,:,:,:)
  !
  CLASS(ParamNode),POINTER :: node

  CALL this%getNode_Name(name,node)
  IF(errorChecks_get(node,PRESENT(default),name,PL_DATA_TYPE_STR4) == 0) THEN
    IF(ASSOCIATED(node)) THEN
      SELECTTYPE(ptr => node%val); TYPE IS(Param_STR4)
        val=ptr%p
      ENDSELECT
    ELSE
      val=default
    ENDIF
  ENDIF

ENDSUBROUTINE getSTR4
!
!-------------------------------------------------------------------------------
!> @brief Gets a rank-5 parameter of type TYPE(StringType)
!> @param this the node to get data from
!> @param name the name of the parameter to get
!> @param val the values retrieved from the node
!> @param default the value to assign to @c val if the parameter is not found; optional
!>
!> If the parameter is not found and @c default is not provided, an error is thrown.
!>
SUBROUTINE getSTR5(this,name,val,default)
  CLASS(ParamType),INTENT(IN) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  TYPE(StringType),ALLOCATABLE,INTENT(OUT) :: val(:,:,:,:,:)
  TYPE(StringType),INTENT(IN),OPTIONAL :: default(:,:,:,:,:)
  !
  CLASS(ParamNode),POINTER :: node

  CALL this%getNode_Name(name,node)
  IF(errorChecks_get(node,PRESENT(default),name,PL_DATA_TYPE_STR5) == 0) THEN
    IF(ASSOCIATED(node)) THEN
      SELECTTYPE(ptr => node%val); TYPE IS(Param_STR5)
        val=ptr%p
      ENDSELECT
    ELSE
      val=default
    ENDIF
  ENDIF

ENDSUBROUTINE getSTR5
!
!-------------------------------------------------------------------------------
!> @brief Gets a rank-6 parameter of type TYPE(StringType)
!> @param this the node to get data from
!> @param name the name of the parameter to get
!> @param val the values retrieved from the node
!> @param default the value to assign to @c val if the parameter is not found; optional
!>
!> If the parameter is not found and @c default is not provided, an error is thrown.
!>
SUBROUTINE getSTR6(this,name,val,default)
  CLASS(ParamType),INTENT(IN) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  TYPE(StringType),ALLOCATABLE,INTENT(OUT) :: val(:,:,:,:,:,:)
  TYPE(StringType),INTENT(IN),OPTIONAL :: default(:,:,:,:,:,:)
  !
  CLASS(ParamNode),POINTER :: node

  CALL this%getNode_Name(name,node)
  IF(errorChecks_get(node,PRESENT(default),name,PL_DATA_TYPE_STR6) == 0) THEN
    IF(ASSOCIATED(node)) THEN
      SELECTTYPE(ptr => node%val); TYPE IS(Param_STR6)
        val=ptr%p
      ENDSELECT
    ELSE
      val=default
    ENDIF
  ENDIF

ENDSUBROUTINE getSTR6
!
!-------------------------------------------------------------------------------
!> @brief Gets a rank-7 parameter of type TYPE(StringType)
!> @param this the node to get data from
!> @param name the name of the parameter to get
!> @param val the values retrieved from the node
!> @param default the value to assign to @c val if the parameter is not found; optional
!>
!> If the parameter is not found and @c default is not provided, an error is thrown.
!>
SUBROUTINE getSTR7(this,name,val,default)
  CLASS(ParamType),INTENT(IN) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  TYPE(StringType),ALLOCATABLE,INTENT(OUT) :: val(:,:,:,:,:,:,:)
  TYPE(StringType),INTENT(IN),OPTIONAL :: default(:,:,:,:,:,:,:)
  !
  CLASS(ParamNode),POINTER :: node

  CALL this%getNode_Name(name,node)
  IF(errorChecks_get(node,PRESENT(default),name,PL_DATA_TYPE_STR7) == 0) THEN
    IF(ASSOCIATED(node)) THEN
      SELECTTYPE(ptr => node%val); TYPE IS(Param_STR7)
        val=ptr%p
      ENDSELECT
    ELSE
      val=default
    ENDIF
  ENDIF

ENDSUBROUTINE getSTR7
!
!-------------------------------------------------------------------------------
!> @brief Sets a rank-0 parameter of type LOGICAL(SBK)
!> @param this the node to set data on
!> @param name the name of the parameter to set
!> @param val the values to set to the parameter
!> @param addmissing logical to indicate if the parameter should be created if not found; optional
!>
!> If the parameter is not found and @c addmissing is not set to true, an error is thrown.
!>
SUBROUTINE setSBK0(this,name,val,description,addmissing)
  CLASS(ParamType),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  LOGICAL(SBK),INTENT(IN) :: val
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
  LOGICAL(SBK),INTENT(IN),OPTIONAL :: addmissing
  !
  LOGICAL(SBK) :: laddmissing
  CLASS(ParamNode),POINTER :: node

  laddmissing=.FALSE.
  IF(PRESENT(addmissing)) laddmissing=addmissing
  IF(laddmissing) THEN
    CALL this%getOrCreateNode_Name(name,node,.TRUE.)
  ELSE
    CALL this%getNode_Name(name,node)
  ENDIF
  IF(errorChecks_set(node,laddmissing,name,PL_DATA_TYPE_SBK0) == 0) THEN
    CALL node%initSBK0(CHAR(node%name),val,description)
  ENDIF

ENDSUBROUTINE setSBK0
!
!-------------------------------------------------------------------------------
!> @brief Sets a rank-1 parameter of type LOGICAL(SBK)
!> @param this the node to set data on
!> @param name the name of the parameter to set
!> @param val the values to set to the parameter
!> @param addmissing logical to indicate if the parameter should be created if not found; optional
!>
!> If the parameter is not found and @c addmissing is not set to true, an error is thrown.
!>
SUBROUTINE setSBK1(this,name,val,description,addmissing)
  CLASS(ParamType),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  LOGICAL(SBK),INTENT(IN) :: val(:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
  LOGICAL(SBK),INTENT(IN),OPTIONAL :: addmissing
  !
  LOGICAL(SBK) :: laddmissing
  CLASS(ParamNode),POINTER :: node

  laddmissing=.FALSE.
  IF(PRESENT(addmissing)) laddmissing=addmissing
  IF(laddmissing) THEN
    CALL this%getOrCreateNode_Name(name,node,.TRUE.)
  ELSE
    CALL this%getNode_Name(name,node)
  ENDIF
  IF(errorChecks_set(node,laddmissing,name,PL_DATA_TYPE_SBK1) == 0) THEN
    CALL node%initSBK1(CHAR(node%name),val,description)
  ENDIF

ENDSUBROUTINE setSBK1
!
!-------------------------------------------------------------------------------
!> @brief Sets a rank-2 parameter of type LOGICAL(SBK)
!> @param this the node to set data on
!> @param name the name of the parameter to set
!> @param val the values to set to the parameter
!> @param addmissing logical to indicate if the parameter should be created if not found; optional
!>
!> If the parameter is not found and @c addmissing is not set to true, an error is thrown.
!>
SUBROUTINE setSBK2(this,name,val,description,addmissing)
  CLASS(ParamType),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  LOGICAL(SBK),INTENT(IN) :: val(:,:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
  LOGICAL(SBK),INTENT(IN),OPTIONAL :: addmissing
  !
  LOGICAL(SBK) :: laddmissing
  CLASS(ParamNode),POINTER :: node

  laddmissing=.FALSE.
  IF(PRESENT(addmissing)) laddmissing=addmissing
  IF(laddmissing) THEN
    CALL this%getOrCreateNode_Name(name,node,.TRUE.)
  ELSE
    CALL this%getNode_Name(name,node)
  ENDIF
  IF(errorChecks_set(node,laddmissing,name,PL_DATA_TYPE_SBK2) == 0) THEN
    CALL node%initSBK2(CHAR(node%name),val,description)
  ENDIF

ENDSUBROUTINE setSBK2
!
!-------------------------------------------------------------------------------
!> @brief Sets a rank-3 parameter of type LOGICAL(SBK)
!> @param this the node to set data on
!> @param name the name of the parameter to set
!> @param val the values to set to the parameter
!> @param addmissing logical to indicate if the parameter should be created if not found; optional
!>
!> If the parameter is not found and @c addmissing is not set to true, an error is thrown.
!>
SUBROUTINE setSBK3(this,name,val,description,addmissing)
  CLASS(ParamType),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  LOGICAL(SBK),INTENT(IN) :: val(:,:,:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
  LOGICAL(SBK),INTENT(IN),OPTIONAL :: addmissing
  !
  LOGICAL(SBK) :: laddmissing
  CLASS(ParamNode),POINTER :: node

  laddmissing=.FALSE.
  IF(PRESENT(addmissing)) laddmissing=addmissing
  IF(laddmissing) THEN
    CALL this%getOrCreateNode_Name(name,node,.TRUE.)
  ELSE
    CALL this%getNode_Name(name,node)
  ENDIF
  IF(errorChecks_set(node,laddmissing,name,PL_DATA_TYPE_SBK3) == 0) THEN
    CALL node%initSBK3(CHAR(node%name),val,description)
  ENDIF

ENDSUBROUTINE setSBK3
!
!-------------------------------------------------------------------------------
!> @brief Sets a rank-4 parameter of type LOGICAL(SBK)
!> @param this the node to set data on
!> @param name the name of the parameter to set
!> @param val the values to set to the parameter
!> @param addmissing logical to indicate if the parameter should be created if not found; optional
!>
!> If the parameter is not found and @c addmissing is not set to true, an error is thrown.
!>
SUBROUTINE setSBK4(this,name,val,description,addmissing)
  CLASS(ParamType),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  LOGICAL(SBK),INTENT(IN) :: val(:,:,:,:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
  LOGICAL(SBK),INTENT(IN),OPTIONAL :: addmissing
  !
  LOGICAL(SBK) :: laddmissing
  CLASS(ParamNode),POINTER :: node

  laddmissing=.FALSE.
  IF(PRESENT(addmissing)) laddmissing=addmissing
  IF(laddmissing) THEN
    CALL this%getOrCreateNode_Name(name,node,.TRUE.)
  ELSE
    CALL this%getNode_Name(name,node)
  ENDIF
  IF(errorChecks_set(node,laddmissing,name,PL_DATA_TYPE_SBK4) == 0) THEN
    CALL node%initSBK4(CHAR(node%name),val,description)
  ENDIF

ENDSUBROUTINE setSBK4
!
!-------------------------------------------------------------------------------
!> @brief Sets a rank-5 parameter of type LOGICAL(SBK)
!> @param this the node to set data on
!> @param name the name of the parameter to set
!> @param val the values to set to the parameter
!> @param addmissing logical to indicate if the parameter should be created if not found; optional
!>
!> If the parameter is not found and @c addmissing is not set to true, an error is thrown.
!>
SUBROUTINE setSBK5(this,name,val,description,addmissing)
  CLASS(ParamType),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  LOGICAL(SBK),INTENT(IN) :: val(:,:,:,:,:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
  LOGICAL(SBK),INTENT(IN),OPTIONAL :: addmissing
  !
  LOGICAL(SBK) :: laddmissing
  CLASS(ParamNode),POINTER :: node

  laddmissing=.FALSE.
  IF(PRESENT(addmissing)) laddmissing=addmissing
  IF(laddmissing) THEN
    CALL this%getOrCreateNode_Name(name,node,.TRUE.)
  ELSE
    CALL this%getNode_Name(name,node)
  ENDIF
  IF(errorChecks_set(node,laddmissing,name,PL_DATA_TYPE_SBK5) == 0) THEN
    CALL node%initSBK5(CHAR(node%name),val,description)
  ENDIF

ENDSUBROUTINE setSBK5
!
!-------------------------------------------------------------------------------
!> @brief Sets a rank-6 parameter of type LOGICAL(SBK)
!> @param this the node to set data on
!> @param name the name of the parameter to set
!> @param val the values to set to the parameter
!> @param addmissing logical to indicate if the parameter should be created if not found; optional
!>
!> If the parameter is not found and @c addmissing is not set to true, an error is thrown.
!>
SUBROUTINE setSBK6(this,name,val,description,addmissing)
  CLASS(ParamType),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  LOGICAL(SBK),INTENT(IN) :: val(:,:,:,:,:,:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
  LOGICAL(SBK),INTENT(IN),OPTIONAL :: addmissing
  !
  LOGICAL(SBK) :: laddmissing
  CLASS(ParamNode),POINTER :: node

  laddmissing=.FALSE.
  IF(PRESENT(addmissing)) laddmissing=addmissing
  IF(laddmissing) THEN
    CALL this%getOrCreateNode_Name(name,node,.TRUE.)
  ELSE
    CALL this%getNode_Name(name,node)
  ENDIF
  IF(errorChecks_set(node,laddmissing,name,PL_DATA_TYPE_SBK6) == 0) THEN
    CALL node%initSBK6(CHAR(node%name),val,description)
  ENDIF

ENDSUBROUTINE setSBK6
!
!-------------------------------------------------------------------------------
!> @brief Sets a rank-7 parameter of type LOGICAL(SBK)
!> @param this the node to set data on
!> @param name the name of the parameter to set
!> @param val the values to set to the parameter
!> @param addmissing logical to indicate if the parameter should be created if not found; optional
!>
!> If the parameter is not found and @c addmissing is not set to true, an error is thrown.
!>
SUBROUTINE setSBK7(this,name,val,description,addmissing)
  CLASS(ParamType),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  LOGICAL(SBK),INTENT(IN) :: val(:,:,:,:,:,:,:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
  LOGICAL(SBK),INTENT(IN),OPTIONAL :: addmissing
  !
  LOGICAL(SBK) :: laddmissing
  CLASS(ParamNode),POINTER :: node

  laddmissing=.FALSE.
  IF(PRESENT(addmissing)) laddmissing=addmissing
  IF(laddmissing) THEN
    CALL this%getOrCreateNode_Name(name,node,.TRUE.)
  ELSE
    CALL this%getNode_Name(name,node)
  ENDIF
  IF(errorChecks_set(node,laddmissing,name,PL_DATA_TYPE_SBK7) == 0) THEN
    CALL node%initSBK7(CHAR(node%name),val,description)
  ENDIF

ENDSUBROUTINE setSBK7
!
!-------------------------------------------------------------------------------
!> @brief Sets a rank-0 parameter of type INTEGER(SNK)
!> @param this the node to set data on
!> @param name the name of the parameter to set
!> @param val the values to set to the parameter
!> @param addmissing logical to indicate if the parameter should be created if not found; optional
!>
!> If the parameter is not found and @c addmissing is not set to true, an error is thrown.
!>
SUBROUTINE setSNK0(this,name,val,description,addmissing)
  CLASS(ParamType),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  INTEGER(SNK),INTENT(IN) :: val
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
  LOGICAL(SBK),INTENT(IN),OPTIONAL :: addmissing
  !
  LOGICAL(SBK) :: laddmissing
  CLASS(ParamNode),POINTER :: node

  laddmissing=.FALSE.
  IF(PRESENT(addmissing)) laddmissing=addmissing
  IF(laddmissing) THEN
    CALL this%getOrCreateNode_Name(name,node,.TRUE.)
  ELSE
    CALL this%getNode_Name(name,node)
  ENDIF
  IF(errorChecks_set(node,laddmissing,name,PL_DATA_TYPE_SNK0) == 0) THEN
    CALL node%initSNK0(CHAR(node%name),val,description)
  ENDIF

ENDSUBROUTINE setSNK0
!
!-------------------------------------------------------------------------------
!> @brief Sets a rank-1 parameter of type INTEGER(SNK)
!> @param this the node to set data on
!> @param name the name of the parameter to set
!> @param val the values to set to the parameter
!> @param addmissing logical to indicate if the parameter should be created if not found; optional
!>
!> If the parameter is not found and @c addmissing is not set to true, an error is thrown.
!>
SUBROUTINE setSNK1(this,name,val,description,addmissing)
  CLASS(ParamType),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  INTEGER(SNK),INTENT(IN) :: val(:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
  LOGICAL(SBK),INTENT(IN),OPTIONAL :: addmissing
  !
  LOGICAL(SBK) :: laddmissing
  CLASS(ParamNode),POINTER :: node

  laddmissing=.FALSE.
  IF(PRESENT(addmissing)) laddmissing=addmissing
  IF(laddmissing) THEN
    CALL this%getOrCreateNode_Name(name,node,.TRUE.)
  ELSE
    CALL this%getNode_Name(name,node)
  ENDIF
  IF(errorChecks_set(node,laddmissing,name,PL_DATA_TYPE_SNK1) == 0) THEN
    CALL node%initSNK1(CHAR(node%name),val,description)
  ENDIF

ENDSUBROUTINE setSNK1
!
!-------------------------------------------------------------------------------
!> @brief Sets a rank-2 parameter of type INTEGER(SNK)
!> @param this the node to set data on
!> @param name the name of the parameter to set
!> @param val the values to set to the parameter
!> @param addmissing logical to indicate if the parameter should be created if not found; optional
!>
!> If the parameter is not found and @c addmissing is not set to true, an error is thrown.
!>
SUBROUTINE setSNK2(this,name,val,description,addmissing)
  CLASS(ParamType),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  INTEGER(SNK),INTENT(IN) :: val(:,:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
  LOGICAL(SBK),INTENT(IN),OPTIONAL :: addmissing
  !
  LOGICAL(SBK) :: laddmissing
  CLASS(ParamNode),POINTER :: node

  laddmissing=.FALSE.
  IF(PRESENT(addmissing)) laddmissing=addmissing
  IF(laddmissing) THEN
    CALL this%getOrCreateNode_Name(name,node,.TRUE.)
  ELSE
    CALL this%getNode_Name(name,node)
  ENDIF
  IF(errorChecks_set(node,laddmissing,name,PL_DATA_TYPE_SNK2) == 0) THEN
    CALL node%initSNK2(CHAR(node%name),val,description)
  ENDIF

ENDSUBROUTINE setSNK2
!
!-------------------------------------------------------------------------------
!> @brief Sets a rank-3 parameter of type INTEGER(SNK)
!> @param this the node to set data on
!> @param name the name of the parameter to set
!> @param val the values to set to the parameter
!> @param addmissing logical to indicate if the parameter should be created if not found; optional
!>
!> If the parameter is not found and @c addmissing is not set to true, an error is thrown.
!>
SUBROUTINE setSNK3(this,name,val,description,addmissing)
  CLASS(ParamType),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  INTEGER(SNK),INTENT(IN) :: val(:,:,:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
  LOGICAL(SBK),INTENT(IN),OPTIONAL :: addmissing
  !
  LOGICAL(SBK) :: laddmissing
  CLASS(ParamNode),POINTER :: node

  laddmissing=.FALSE.
  IF(PRESENT(addmissing)) laddmissing=addmissing
  IF(laddmissing) THEN
    CALL this%getOrCreateNode_Name(name,node,.TRUE.)
  ELSE
    CALL this%getNode_Name(name,node)
  ENDIF
  IF(errorChecks_set(node,laddmissing,name,PL_DATA_TYPE_SNK3) == 0) THEN
    CALL node%initSNK3(CHAR(node%name),val,description)
  ENDIF

ENDSUBROUTINE setSNK3
!
!-------------------------------------------------------------------------------
!> @brief Sets a rank-4 parameter of type INTEGER(SNK)
!> @param this the node to set data on
!> @param name the name of the parameter to set
!> @param val the values to set to the parameter
!> @param addmissing logical to indicate if the parameter should be created if not found; optional
!>
!> If the parameter is not found and @c addmissing is not set to true, an error is thrown.
!>
SUBROUTINE setSNK4(this,name,val,description,addmissing)
  CLASS(ParamType),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  INTEGER(SNK),INTENT(IN) :: val(:,:,:,:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
  LOGICAL(SBK),INTENT(IN),OPTIONAL :: addmissing
  !
  LOGICAL(SBK) :: laddmissing
  CLASS(ParamNode),POINTER :: node

  laddmissing=.FALSE.
  IF(PRESENT(addmissing)) laddmissing=addmissing
  IF(laddmissing) THEN
    CALL this%getOrCreateNode_Name(name,node,.TRUE.)
  ELSE
    CALL this%getNode_Name(name,node)
  ENDIF
  IF(errorChecks_set(node,laddmissing,name,PL_DATA_TYPE_SNK4) == 0) THEN
    CALL node%initSNK4(CHAR(node%name),val,description)
  ENDIF

ENDSUBROUTINE setSNK4
!
!-------------------------------------------------------------------------------
!> @brief Sets a rank-5 parameter of type INTEGER(SNK)
!> @param this the node to set data on
!> @param name the name of the parameter to set
!> @param val the values to set to the parameter
!> @param addmissing logical to indicate if the parameter should be created if not found; optional
!>
!> If the parameter is not found and @c addmissing is not set to true, an error is thrown.
!>
SUBROUTINE setSNK5(this,name,val,description,addmissing)
  CLASS(ParamType),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  INTEGER(SNK),INTENT(IN) :: val(:,:,:,:,:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
  LOGICAL(SBK),INTENT(IN),OPTIONAL :: addmissing
  !
  LOGICAL(SBK) :: laddmissing
  CLASS(ParamNode),POINTER :: node

  laddmissing=.FALSE.
  IF(PRESENT(addmissing)) laddmissing=addmissing
  IF(laddmissing) THEN
    CALL this%getOrCreateNode_Name(name,node,.TRUE.)
  ELSE
    CALL this%getNode_Name(name,node)
  ENDIF
  IF(errorChecks_set(node,laddmissing,name,PL_DATA_TYPE_SNK5) == 0) THEN
    CALL node%initSNK5(CHAR(node%name),val,description)
  ENDIF

ENDSUBROUTINE setSNK5
!
!-------------------------------------------------------------------------------
!> @brief Sets a rank-6 parameter of type INTEGER(SNK)
!> @param this the node to set data on
!> @param name the name of the parameter to set
!> @param val the values to set to the parameter
!> @param addmissing logical to indicate if the parameter should be created if not found; optional
!>
!> If the parameter is not found and @c addmissing is not set to true, an error is thrown.
!>
SUBROUTINE setSNK6(this,name,val,description,addmissing)
  CLASS(ParamType),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  INTEGER(SNK),INTENT(IN) :: val(:,:,:,:,:,:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
  LOGICAL(SBK),INTENT(IN),OPTIONAL :: addmissing
  !
  LOGICAL(SBK) :: laddmissing
  CLASS(ParamNode),POINTER :: node

  laddmissing=.FALSE.
  IF(PRESENT(addmissing)) laddmissing=addmissing
  IF(laddmissing) THEN
    CALL this%getOrCreateNode_Name(name,node,.TRUE.)
  ELSE
    CALL this%getNode_Name(name,node)
  ENDIF
  IF(errorChecks_set(node,laddmissing,name,PL_DATA_TYPE_SNK6) == 0) THEN
    CALL node%initSNK6(CHAR(node%name),val,description)
  ENDIF

ENDSUBROUTINE setSNK6
!
!-------------------------------------------------------------------------------
!> @brief Sets a rank-7 parameter of type INTEGER(SNK)
!> @param this the node to set data on
!> @param name the name of the parameter to set
!> @param val the values to set to the parameter
!> @param addmissing logical to indicate if the parameter should be created if not found; optional
!>
!> If the parameter is not found and @c addmissing is not set to true, an error is thrown.
!>
SUBROUTINE setSNK7(this,name,val,description,addmissing)
  CLASS(ParamType),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  INTEGER(SNK),INTENT(IN) :: val(:,:,:,:,:,:,:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
  LOGICAL(SBK),INTENT(IN),OPTIONAL :: addmissing
  !
  LOGICAL(SBK) :: laddmissing
  CLASS(ParamNode),POINTER :: node

  laddmissing=.FALSE.
  IF(PRESENT(addmissing)) laddmissing=addmissing
  IF(laddmissing) THEN
    CALL this%getOrCreateNode_Name(name,node,.TRUE.)
  ELSE
    CALL this%getNode_Name(name,node)
  ENDIF
  IF(errorChecks_set(node,laddmissing,name,PL_DATA_TYPE_SNK7) == 0) THEN
    CALL node%initSNK7(CHAR(node%name),val,description)
  ENDIF

ENDSUBROUTINE setSNK7
!
!-------------------------------------------------------------------------------
!> @brief Sets a rank-0 parameter of type INTEGER(SLK)
!> @param this the node to set data on
!> @param name the name of the parameter to set
!> @param val the values to set to the parameter
!> @param addmissing logical to indicate if the parameter should be created if not found; optional
!>
!> If the parameter is not found and @c addmissing is not set to true, an error is thrown.
!>
SUBROUTINE setSLK0(this,name,val,description,addmissing)
  CLASS(ParamType),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  INTEGER(SLK),INTENT(IN) :: val
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
  LOGICAL(SBK),INTENT(IN),OPTIONAL :: addmissing
  !
  LOGICAL(SBK) :: laddmissing
  CLASS(ParamNode),POINTER :: node

  laddmissing=.FALSE.
  IF(PRESENT(addmissing)) laddmissing=addmissing
  IF(laddmissing) THEN
    CALL this%getOrCreateNode_Name(name,node,.TRUE.)
  ELSE
    CALL this%getNode_Name(name,node)
  ENDIF
  IF(errorChecks_set(node,laddmissing,name,PL_DATA_TYPE_SLK0) == 0) THEN
    CALL node%initSLK0(CHAR(node%name),val,description)
  ENDIF

ENDSUBROUTINE setSLK0
!
!-------------------------------------------------------------------------------
!> @brief Sets a rank-1 parameter of type INTEGER(SLK)
!> @param this the node to set data on
!> @param name the name of the parameter to set
!> @param val the values to set to the parameter
!> @param addmissing logical to indicate if the parameter should be created if not found; optional
!>
!> If the parameter is not found and @c addmissing is not set to true, an error is thrown.
!>
SUBROUTINE setSLK1(this,name,val,description,addmissing)
  CLASS(ParamType),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  INTEGER(SLK),INTENT(IN) :: val(:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
  LOGICAL(SBK),INTENT(IN),OPTIONAL :: addmissing
  !
  LOGICAL(SBK) :: laddmissing
  CLASS(ParamNode),POINTER :: node

  laddmissing=.FALSE.
  IF(PRESENT(addmissing)) laddmissing=addmissing
  IF(laddmissing) THEN
    CALL this%getOrCreateNode_Name(name,node,.TRUE.)
  ELSE
    CALL this%getNode_Name(name,node)
  ENDIF
  IF(errorChecks_set(node,laddmissing,name,PL_DATA_TYPE_SLK1) == 0) THEN
    CALL node%initSLK1(CHAR(node%name),val,description)
  ENDIF

ENDSUBROUTINE setSLK1
!
!-------------------------------------------------------------------------------
!> @brief Sets a rank-2 parameter of type INTEGER(SLK)
!> @param this the node to set data on
!> @param name the name of the parameter to set
!> @param val the values to set to the parameter
!> @param addmissing logical to indicate if the parameter should be created if not found; optional
!>
!> If the parameter is not found and @c addmissing is not set to true, an error is thrown.
!>
SUBROUTINE setSLK2(this,name,val,description,addmissing)
  CLASS(ParamType),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  INTEGER(SLK),INTENT(IN) :: val(:,:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
  LOGICAL(SBK),INTENT(IN),OPTIONAL :: addmissing
  !
  LOGICAL(SBK) :: laddmissing
  CLASS(ParamNode),POINTER :: node

  laddmissing=.FALSE.
  IF(PRESENT(addmissing)) laddmissing=addmissing
  IF(laddmissing) THEN
    CALL this%getOrCreateNode_Name(name,node,.TRUE.)
  ELSE
    CALL this%getNode_Name(name,node)
  ENDIF
  IF(errorChecks_set(node,laddmissing,name,PL_DATA_TYPE_SLK2) == 0) THEN
    CALL node%initSLK2(CHAR(node%name),val,description)
  ENDIF

ENDSUBROUTINE setSLK2
!
!-------------------------------------------------------------------------------
!> @brief Sets a rank-3 parameter of type INTEGER(SLK)
!> @param this the node to set data on
!> @param name the name of the parameter to set
!> @param val the values to set to the parameter
!> @param addmissing logical to indicate if the parameter should be created if not found; optional
!>
!> If the parameter is not found and @c addmissing is not set to true, an error is thrown.
!>
SUBROUTINE setSLK3(this,name,val,description,addmissing)
  CLASS(ParamType),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  INTEGER(SLK),INTENT(IN) :: val(:,:,:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
  LOGICAL(SBK),INTENT(IN),OPTIONAL :: addmissing
  !
  LOGICAL(SBK) :: laddmissing
  CLASS(ParamNode),POINTER :: node

  laddmissing=.FALSE.
  IF(PRESENT(addmissing)) laddmissing=addmissing
  IF(laddmissing) THEN
    CALL this%getOrCreateNode_Name(name,node,.TRUE.)
  ELSE
    CALL this%getNode_Name(name,node)
  ENDIF
  IF(errorChecks_set(node,laddmissing,name,PL_DATA_TYPE_SLK3) == 0) THEN
    CALL node%initSLK3(CHAR(node%name),val,description)
  ENDIF

ENDSUBROUTINE setSLK3
!
!-------------------------------------------------------------------------------
!> @brief Sets a rank-4 parameter of type INTEGER(SLK)
!> @param this the node to set data on
!> @param name the name of the parameter to set
!> @param val the values to set to the parameter
!> @param addmissing logical to indicate if the parameter should be created if not found; optional
!>
!> If the parameter is not found and @c addmissing is not set to true, an error is thrown.
!>
SUBROUTINE setSLK4(this,name,val,description,addmissing)
  CLASS(ParamType),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  INTEGER(SLK),INTENT(IN) :: val(:,:,:,:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
  LOGICAL(SBK),INTENT(IN),OPTIONAL :: addmissing
  !
  LOGICAL(SBK) :: laddmissing
  CLASS(ParamNode),POINTER :: node

  laddmissing=.FALSE.
  IF(PRESENT(addmissing)) laddmissing=addmissing
  IF(laddmissing) THEN
    CALL this%getOrCreateNode_Name(name,node,.TRUE.)
  ELSE
    CALL this%getNode_Name(name,node)
  ENDIF
  IF(errorChecks_set(node,laddmissing,name,PL_DATA_TYPE_SLK4) == 0) THEN
    CALL node%initSLK4(CHAR(node%name),val,description)
  ENDIF

ENDSUBROUTINE setSLK4
!
!-------------------------------------------------------------------------------
!> @brief Sets a rank-5 parameter of type INTEGER(SLK)
!> @param this the node to set data on
!> @param name the name of the parameter to set
!> @param val the values to set to the parameter
!> @param addmissing logical to indicate if the parameter should be created if not found; optional
!>
!> If the parameter is not found and @c addmissing is not set to true, an error is thrown.
!>
SUBROUTINE setSLK5(this,name,val,description,addmissing)
  CLASS(ParamType),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  INTEGER(SLK),INTENT(IN) :: val(:,:,:,:,:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
  LOGICAL(SBK),INTENT(IN),OPTIONAL :: addmissing
  !
  LOGICAL(SBK) :: laddmissing
  CLASS(ParamNode),POINTER :: node

  laddmissing=.FALSE.
  IF(PRESENT(addmissing)) laddmissing=addmissing
  IF(laddmissing) THEN
    CALL this%getOrCreateNode_Name(name,node,.TRUE.)
  ELSE
    CALL this%getNode_Name(name,node)
  ENDIF
  IF(errorChecks_set(node,laddmissing,name,PL_DATA_TYPE_SLK5) == 0) THEN
    CALL node%initSLK5(CHAR(node%name),val,description)
  ENDIF

ENDSUBROUTINE setSLK5
!
!-------------------------------------------------------------------------------
!> @brief Sets a rank-6 parameter of type INTEGER(SLK)
!> @param this the node to set data on
!> @param name the name of the parameter to set
!> @param val the values to set to the parameter
!> @param addmissing logical to indicate if the parameter should be created if not found; optional
!>
!> If the parameter is not found and @c addmissing is not set to true, an error is thrown.
!>
SUBROUTINE setSLK6(this,name,val,description,addmissing)
  CLASS(ParamType),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  INTEGER(SLK),INTENT(IN) :: val(:,:,:,:,:,:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
  LOGICAL(SBK),INTENT(IN),OPTIONAL :: addmissing
  !
  LOGICAL(SBK) :: laddmissing
  CLASS(ParamNode),POINTER :: node

  laddmissing=.FALSE.
  IF(PRESENT(addmissing)) laddmissing=addmissing
  IF(laddmissing) THEN
    CALL this%getOrCreateNode_Name(name,node,.TRUE.)
  ELSE
    CALL this%getNode_Name(name,node)
  ENDIF
  IF(errorChecks_set(node,laddmissing,name,PL_DATA_TYPE_SLK6) == 0) THEN
    CALL node%initSLK6(CHAR(node%name),val,description)
  ENDIF

ENDSUBROUTINE setSLK6
!
!-------------------------------------------------------------------------------
!> @brief Sets a rank-7 parameter of type INTEGER(SLK)
!> @param this the node to set data on
!> @param name the name of the parameter to set
!> @param val the values to set to the parameter
!> @param addmissing logical to indicate if the parameter should be created if not found; optional
!>
!> If the parameter is not found and @c addmissing is not set to true, an error is thrown.
!>
SUBROUTINE setSLK7(this,name,val,description,addmissing)
  CLASS(ParamType),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  INTEGER(SLK),INTENT(IN) :: val(:,:,:,:,:,:,:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
  LOGICAL(SBK),INTENT(IN),OPTIONAL :: addmissing
  !
  LOGICAL(SBK) :: laddmissing
  CLASS(ParamNode),POINTER :: node

  laddmissing=.FALSE.
  IF(PRESENT(addmissing)) laddmissing=addmissing
  IF(laddmissing) THEN
    CALL this%getOrCreateNode_Name(name,node,.TRUE.)
  ELSE
    CALL this%getNode_Name(name,node)
  ENDIF
  IF(errorChecks_set(node,laddmissing,name,PL_DATA_TYPE_SLK7) == 0) THEN
    CALL node%initSLK7(CHAR(node%name),val,description)
  ENDIF

ENDSUBROUTINE setSLK7
!
!-------------------------------------------------------------------------------
!> @brief Sets a rank-0 parameter of type REAL(SSK)
!> @param this the node to set data on
!> @param name the name of the parameter to set
!> @param val the values to set to the parameter
!> @param addmissing logical to indicate if the parameter should be created if not found; optional
!>
!> If the parameter is not found and @c addmissing is not set to true, an error is thrown.
!>
SUBROUTINE setSSK0(this,name,val,description,addmissing)
  CLASS(ParamType),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  REAL(SSK),INTENT(IN) :: val
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
  LOGICAL(SBK),INTENT(IN),OPTIONAL :: addmissing
  !
  LOGICAL(SBK) :: laddmissing
  CLASS(ParamNode),POINTER :: node

  laddmissing=.FALSE.
  IF(PRESENT(addmissing)) laddmissing=addmissing
  IF(laddmissing) THEN
    CALL this%getOrCreateNode_Name(name,node,.TRUE.)
  ELSE
    CALL this%getNode_Name(name,node)
  ENDIF
  IF(errorChecks_set(node,laddmissing,name,PL_DATA_TYPE_SSK0) == 0) THEN
    CALL node%initSSK0(CHAR(node%name),val,description)
  ENDIF

ENDSUBROUTINE setSSK0
!
!-------------------------------------------------------------------------------
!> @brief Sets a rank-1 parameter of type REAL(SSK)
!> @param this the node to set data on
!> @param name the name of the parameter to set
!> @param val the values to set to the parameter
!> @param addmissing logical to indicate if the parameter should be created if not found; optional
!>
!> If the parameter is not found and @c addmissing is not set to true, an error is thrown.
!>
SUBROUTINE setSSK1(this,name,val,description,addmissing)
  CLASS(ParamType),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  REAL(SSK),INTENT(IN) :: val(:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
  LOGICAL(SBK),INTENT(IN),OPTIONAL :: addmissing
  !
  LOGICAL(SBK) :: laddmissing
  CLASS(ParamNode),POINTER :: node

  laddmissing=.FALSE.
  IF(PRESENT(addmissing)) laddmissing=addmissing
  IF(laddmissing) THEN
    CALL this%getOrCreateNode_Name(name,node,.TRUE.)
  ELSE
    CALL this%getNode_Name(name,node)
  ENDIF
  IF(errorChecks_set(node,laddmissing,name,PL_DATA_TYPE_SSK1) == 0) THEN
    CALL node%initSSK1(CHAR(node%name),val,description)
  ENDIF

ENDSUBROUTINE setSSK1
!
!-------------------------------------------------------------------------------
!> @brief Sets a rank-2 parameter of type REAL(SSK)
!> @param this the node to set data on
!> @param name the name of the parameter to set
!> @param val the values to set to the parameter
!> @param addmissing logical to indicate if the parameter should be created if not found; optional
!>
!> If the parameter is not found and @c addmissing is not set to true, an error is thrown.
!>
SUBROUTINE setSSK2(this,name,val,description,addmissing)
  CLASS(ParamType),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  REAL(SSK),INTENT(IN) :: val(:,:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
  LOGICAL(SBK),INTENT(IN),OPTIONAL :: addmissing
  !
  LOGICAL(SBK) :: laddmissing
  CLASS(ParamNode),POINTER :: node

  laddmissing=.FALSE.
  IF(PRESENT(addmissing)) laddmissing=addmissing
  IF(laddmissing) THEN
    CALL this%getOrCreateNode_Name(name,node,.TRUE.)
  ELSE
    CALL this%getNode_Name(name,node)
  ENDIF
  IF(errorChecks_set(node,laddmissing,name,PL_DATA_TYPE_SSK2) == 0) THEN
    CALL node%initSSK2(CHAR(node%name),val,description)
  ENDIF

ENDSUBROUTINE setSSK2
!
!-------------------------------------------------------------------------------
!> @brief Sets a rank-3 parameter of type REAL(SSK)
!> @param this the node to set data on
!> @param name the name of the parameter to set
!> @param val the values to set to the parameter
!> @param addmissing logical to indicate if the parameter should be created if not found; optional
!>
!> If the parameter is not found and @c addmissing is not set to true, an error is thrown.
!>
SUBROUTINE setSSK3(this,name,val,description,addmissing)
  CLASS(ParamType),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  REAL(SSK),INTENT(IN) :: val(:,:,:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
  LOGICAL(SBK),INTENT(IN),OPTIONAL :: addmissing
  !
  LOGICAL(SBK) :: laddmissing
  CLASS(ParamNode),POINTER :: node

  laddmissing=.FALSE.
  IF(PRESENT(addmissing)) laddmissing=addmissing
  IF(laddmissing) THEN
    CALL this%getOrCreateNode_Name(name,node,.TRUE.)
  ELSE
    CALL this%getNode_Name(name,node)
  ENDIF
  IF(errorChecks_set(node,laddmissing,name,PL_DATA_TYPE_SSK3) == 0) THEN
    CALL node%initSSK3(CHAR(node%name),val,description)
  ENDIF

ENDSUBROUTINE setSSK3
!
!-------------------------------------------------------------------------------
!> @brief Sets a rank-4 parameter of type REAL(SSK)
!> @param this the node to set data on
!> @param name the name of the parameter to set
!> @param val the values to set to the parameter
!> @param addmissing logical to indicate if the parameter should be created if not found; optional
!>
!> If the parameter is not found and @c addmissing is not set to true, an error is thrown.
!>
SUBROUTINE setSSK4(this,name,val,description,addmissing)
  CLASS(ParamType),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  REAL(SSK),INTENT(IN) :: val(:,:,:,:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
  LOGICAL(SBK),INTENT(IN),OPTIONAL :: addmissing
  !
  LOGICAL(SBK) :: laddmissing
  CLASS(ParamNode),POINTER :: node

  laddmissing=.FALSE.
  IF(PRESENT(addmissing)) laddmissing=addmissing
  IF(laddmissing) THEN
    CALL this%getOrCreateNode_Name(name,node,.TRUE.)
  ELSE
    CALL this%getNode_Name(name,node)
  ENDIF
  IF(errorChecks_set(node,laddmissing,name,PL_DATA_TYPE_SSK4) == 0) THEN
    CALL node%initSSK4(CHAR(node%name),val,description)
  ENDIF

ENDSUBROUTINE setSSK4
!
!-------------------------------------------------------------------------------
!> @brief Sets a rank-5 parameter of type REAL(SSK)
!> @param this the node to set data on
!> @param name the name of the parameter to set
!> @param val the values to set to the parameter
!> @param addmissing logical to indicate if the parameter should be created if not found; optional
!>
!> If the parameter is not found and @c addmissing is not set to true, an error is thrown.
!>
SUBROUTINE setSSK5(this,name,val,description,addmissing)
  CLASS(ParamType),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  REAL(SSK),INTENT(IN) :: val(:,:,:,:,:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
  LOGICAL(SBK),INTENT(IN),OPTIONAL :: addmissing
  !
  LOGICAL(SBK) :: laddmissing
  CLASS(ParamNode),POINTER :: node

  laddmissing=.FALSE.
  IF(PRESENT(addmissing)) laddmissing=addmissing
  IF(laddmissing) THEN
    CALL this%getOrCreateNode_Name(name,node,.TRUE.)
  ELSE
    CALL this%getNode_Name(name,node)
  ENDIF
  IF(errorChecks_set(node,laddmissing,name,PL_DATA_TYPE_SSK5) == 0) THEN
    CALL node%initSSK5(CHAR(node%name),val,description)
  ENDIF

ENDSUBROUTINE setSSK5
!
!-------------------------------------------------------------------------------
!> @brief Sets a rank-6 parameter of type REAL(SSK)
!> @param this the node to set data on
!> @param name the name of the parameter to set
!> @param val the values to set to the parameter
!> @param addmissing logical to indicate if the parameter should be created if not found; optional
!>
!> If the parameter is not found and @c addmissing is not set to true, an error is thrown.
!>
SUBROUTINE setSSK6(this,name,val,description,addmissing)
  CLASS(ParamType),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  REAL(SSK),INTENT(IN) :: val(:,:,:,:,:,:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
  LOGICAL(SBK),INTENT(IN),OPTIONAL :: addmissing
  !
  LOGICAL(SBK) :: laddmissing
  CLASS(ParamNode),POINTER :: node

  laddmissing=.FALSE.
  IF(PRESENT(addmissing)) laddmissing=addmissing
  IF(laddmissing) THEN
    CALL this%getOrCreateNode_Name(name,node,.TRUE.)
  ELSE
    CALL this%getNode_Name(name,node)
  ENDIF
  IF(errorChecks_set(node,laddmissing,name,PL_DATA_TYPE_SSK6) == 0) THEN
    CALL node%initSSK6(CHAR(node%name),val,description)
  ENDIF

ENDSUBROUTINE setSSK6
!
!-------------------------------------------------------------------------------
!> @brief Sets a rank-7 parameter of type REAL(SSK)
!> @param this the node to set data on
!> @param name the name of the parameter to set
!> @param val the values to set to the parameter
!> @param addmissing logical to indicate if the parameter should be created if not found; optional
!>
!> If the parameter is not found and @c addmissing is not set to true, an error is thrown.
!>
SUBROUTINE setSSK7(this,name,val,description,addmissing)
  CLASS(ParamType),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  REAL(SSK),INTENT(IN) :: val(:,:,:,:,:,:,:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
  LOGICAL(SBK),INTENT(IN),OPTIONAL :: addmissing
  !
  LOGICAL(SBK) :: laddmissing
  CLASS(ParamNode),POINTER :: node

  laddmissing=.FALSE.
  IF(PRESENT(addmissing)) laddmissing=addmissing
  IF(laddmissing) THEN
    CALL this%getOrCreateNode_Name(name,node,.TRUE.)
  ELSE
    CALL this%getNode_Name(name,node)
  ENDIF
  IF(errorChecks_set(node,laddmissing,name,PL_DATA_TYPE_SSK7) == 0) THEN
    CALL node%initSSK7(CHAR(node%name),val,description)
  ENDIF

ENDSUBROUTINE setSSK7
!
!-------------------------------------------------------------------------------
!> @brief Sets a rank-0 parameter of type REAL(SDK)
!> @param this the node to set data on
!> @param name the name of the parameter to set
!> @param val the values to set to the parameter
!> @param addmissing logical to indicate if the parameter should be created if not found; optional
!>
!> If the parameter is not found and @c addmissing is not set to true, an error is thrown.
!>
SUBROUTINE setSDK0(this,name,val,description,addmissing)
  CLASS(ParamType),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  REAL(SDK),INTENT(IN) :: val
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
  LOGICAL(SBK),INTENT(IN),OPTIONAL :: addmissing
  !
  LOGICAL(SBK) :: laddmissing
  CLASS(ParamNode),POINTER :: node

  laddmissing=.FALSE.
  IF(PRESENT(addmissing)) laddmissing=addmissing
  IF(laddmissing) THEN
    CALL this%getOrCreateNode_Name(name,node,.TRUE.)
  ELSE
    CALL this%getNode_Name(name,node)
  ENDIF
  IF(errorChecks_set(node,laddmissing,name,PL_DATA_TYPE_SDK0) == 0) THEN
    CALL node%initSDK0(CHAR(node%name),val,description)
  ENDIF

ENDSUBROUTINE setSDK0
!
!-------------------------------------------------------------------------------
!> @brief Sets a rank-1 parameter of type REAL(SDK)
!> @param this the node to set data on
!> @param name the name of the parameter to set
!> @param val the values to set to the parameter
!> @param addmissing logical to indicate if the parameter should be created if not found; optional
!>
!> If the parameter is not found and @c addmissing is not set to true, an error is thrown.
!>
SUBROUTINE setSDK1(this,name,val,description,addmissing)
  CLASS(ParamType),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  REAL(SDK),INTENT(IN) :: val(:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
  LOGICAL(SBK),INTENT(IN),OPTIONAL :: addmissing
  !
  LOGICAL(SBK) :: laddmissing
  CLASS(ParamNode),POINTER :: node

  laddmissing=.FALSE.
  IF(PRESENT(addmissing)) laddmissing=addmissing
  IF(laddmissing) THEN
    CALL this%getOrCreateNode_Name(name,node,.TRUE.)
  ELSE
    CALL this%getNode_Name(name,node)
  ENDIF
  IF(errorChecks_set(node,laddmissing,name,PL_DATA_TYPE_SDK1) == 0) THEN
    CALL node%initSDK1(CHAR(node%name),val,description)
  ENDIF

ENDSUBROUTINE setSDK1
!
!-------------------------------------------------------------------------------
!> @brief Sets a rank-2 parameter of type REAL(SDK)
!> @param this the node to set data on
!> @param name the name of the parameter to set
!> @param val the values to set to the parameter
!> @param addmissing logical to indicate if the parameter should be created if not found; optional
!>
!> If the parameter is not found and @c addmissing is not set to true, an error is thrown.
!>
SUBROUTINE setSDK2(this,name,val,description,addmissing)
  CLASS(ParamType),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  REAL(SDK),INTENT(IN) :: val(:,:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
  LOGICAL(SBK),INTENT(IN),OPTIONAL :: addmissing
  !
  LOGICAL(SBK) :: laddmissing
  CLASS(ParamNode),POINTER :: node

  laddmissing=.FALSE.
  IF(PRESENT(addmissing)) laddmissing=addmissing
  IF(laddmissing) THEN
    CALL this%getOrCreateNode_Name(name,node,.TRUE.)
  ELSE
    CALL this%getNode_Name(name,node)
  ENDIF
  IF(errorChecks_set(node,laddmissing,name,PL_DATA_TYPE_SDK2) == 0) THEN
    CALL node%initSDK2(CHAR(node%name),val,description)
  ENDIF

ENDSUBROUTINE setSDK2
!
!-------------------------------------------------------------------------------
!> @brief Sets a rank-3 parameter of type REAL(SDK)
!> @param this the node to set data on
!> @param name the name of the parameter to set
!> @param val the values to set to the parameter
!> @param addmissing logical to indicate if the parameter should be created if not found; optional
!>
!> If the parameter is not found and @c addmissing is not set to true, an error is thrown.
!>
SUBROUTINE setSDK3(this,name,val,description,addmissing)
  CLASS(ParamType),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  REAL(SDK),INTENT(IN) :: val(:,:,:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
  LOGICAL(SBK),INTENT(IN),OPTIONAL :: addmissing
  !
  LOGICAL(SBK) :: laddmissing
  CLASS(ParamNode),POINTER :: node

  laddmissing=.FALSE.
  IF(PRESENT(addmissing)) laddmissing=addmissing
  IF(laddmissing) THEN
    CALL this%getOrCreateNode_Name(name,node,.TRUE.)
  ELSE
    CALL this%getNode_Name(name,node)
  ENDIF
  IF(errorChecks_set(node,laddmissing,name,PL_DATA_TYPE_SDK3) == 0) THEN
    CALL node%initSDK3(CHAR(node%name),val,description)
  ENDIF

ENDSUBROUTINE setSDK3
!
!-------------------------------------------------------------------------------
!> @brief Sets a rank-4 parameter of type REAL(SDK)
!> @param this the node to set data on
!> @param name the name of the parameter to set
!> @param val the values to set to the parameter
!> @param addmissing logical to indicate if the parameter should be created if not found; optional
!>
!> If the parameter is not found and @c addmissing is not set to true, an error is thrown.
!>
SUBROUTINE setSDK4(this,name,val,description,addmissing)
  CLASS(ParamType),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  REAL(SDK),INTENT(IN) :: val(:,:,:,:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
  LOGICAL(SBK),INTENT(IN),OPTIONAL :: addmissing
  !
  LOGICAL(SBK) :: laddmissing
  CLASS(ParamNode),POINTER :: node

  laddmissing=.FALSE.
  IF(PRESENT(addmissing)) laddmissing=addmissing
  IF(laddmissing) THEN
    CALL this%getOrCreateNode_Name(name,node,.TRUE.)
  ELSE
    CALL this%getNode_Name(name,node)
  ENDIF
  IF(errorChecks_set(node,laddmissing,name,PL_DATA_TYPE_SDK4) == 0) THEN
    CALL node%initSDK4(CHAR(node%name),val,description)
  ENDIF

ENDSUBROUTINE setSDK4
!
!-------------------------------------------------------------------------------
!> @brief Sets a rank-5 parameter of type REAL(SDK)
!> @param this the node to set data on
!> @param name the name of the parameter to set
!> @param val the values to set to the parameter
!> @param addmissing logical to indicate if the parameter should be created if not found; optional
!>
!> If the parameter is not found and @c addmissing is not set to true, an error is thrown.
!>
SUBROUTINE setSDK5(this,name,val,description,addmissing)
  CLASS(ParamType),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  REAL(SDK),INTENT(IN) :: val(:,:,:,:,:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
  LOGICAL(SBK),INTENT(IN),OPTIONAL :: addmissing
  !
  LOGICAL(SBK) :: laddmissing
  CLASS(ParamNode),POINTER :: node

  laddmissing=.FALSE.
  IF(PRESENT(addmissing)) laddmissing=addmissing
  IF(laddmissing) THEN
    CALL this%getOrCreateNode_Name(name,node,.TRUE.)
  ELSE
    CALL this%getNode_Name(name,node)
  ENDIF
  IF(errorChecks_set(node,laddmissing,name,PL_DATA_TYPE_SDK5) == 0) THEN
    CALL node%initSDK5(CHAR(node%name),val,description)
  ENDIF

ENDSUBROUTINE setSDK5
!
!-------------------------------------------------------------------------------
!> @brief Sets a rank-6 parameter of type REAL(SDK)
!> @param this the node to set data on
!> @param name the name of the parameter to set
!> @param val the values to set to the parameter
!> @param addmissing logical to indicate if the parameter should be created if not found; optional
!>
!> If the parameter is not found and @c addmissing is not set to true, an error is thrown.
!>
SUBROUTINE setSDK6(this,name,val,description,addmissing)
  CLASS(ParamType),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  REAL(SDK),INTENT(IN) :: val(:,:,:,:,:,:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
  LOGICAL(SBK),INTENT(IN),OPTIONAL :: addmissing
  !
  LOGICAL(SBK) :: laddmissing
  CLASS(ParamNode),POINTER :: node

  laddmissing=.FALSE.
  IF(PRESENT(addmissing)) laddmissing=addmissing
  IF(laddmissing) THEN
    CALL this%getOrCreateNode_Name(name,node,.TRUE.)
  ELSE
    CALL this%getNode_Name(name,node)
  ENDIF
  IF(errorChecks_set(node,laddmissing,name,PL_DATA_TYPE_SDK6) == 0) THEN
    CALL node%initSDK6(CHAR(node%name),val,description)
  ENDIF

ENDSUBROUTINE setSDK6
!
!-------------------------------------------------------------------------------
!> @brief Sets a rank-7 parameter of type REAL(SDK)
!> @param this the node to set data on
!> @param name the name of the parameter to set
!> @param val the values to set to the parameter
!> @param addmissing logical to indicate if the parameter should be created if not found; optional
!>
!> If the parameter is not found and @c addmissing is not set to true, an error is thrown.
!>
SUBROUTINE setSDK7(this,name,val,description,addmissing)
  CLASS(ParamType),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  REAL(SDK),INTENT(IN) :: val(:,:,:,:,:,:,:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
  LOGICAL(SBK),INTENT(IN),OPTIONAL :: addmissing
  !
  LOGICAL(SBK) :: laddmissing
  CLASS(ParamNode),POINTER :: node

  laddmissing=.FALSE.
  IF(PRESENT(addmissing)) laddmissing=addmissing
  IF(laddmissing) THEN
    CALL this%getOrCreateNode_Name(name,node,.TRUE.)
  ELSE
    CALL this%getNode_Name(name,node)
  ENDIF
  IF(errorChecks_set(node,laddmissing,name,PL_DATA_TYPE_SDK7) == 0) THEN
    CALL node%initSDK7(CHAR(node%name),val,description)
  ENDIF

ENDSUBROUTINE setSDK7
!
!-------------------------------------------------------------------------------
!> @brief Sets a rank-0 parameter of type TYPE(StringType)
!> @param this the node to set data on
!> @param name the name of the parameter to set
!> @param val the values to set to the parameter
!> @param addmissing logical to indicate if the parameter should be created if not found; optional
!>
!> If the parameter is not found and @c addmissing is not set to true, an error is thrown.
!>
SUBROUTINE setSTR0(this,name,val,description,addmissing)
  CLASS(ParamType),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  TYPE(StringType),INTENT(IN) :: val
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
  LOGICAL(SBK),INTENT(IN),OPTIONAL :: addmissing
  !
  LOGICAL(SBK) :: laddmissing
  CLASS(ParamNode),POINTER :: node

  laddmissing=.FALSE.
  IF(PRESENT(addmissing)) laddmissing=addmissing
  IF(laddmissing) THEN
    CALL this%getOrCreateNode_Name(name,node,.TRUE.)
  ELSE
    CALL this%getNode_Name(name,node)
  ENDIF
  IF(errorChecks_set(node,laddmissing,name,PL_DATA_TYPE_STR0) == 0) THEN
    CALL node%initSTR0(CHAR(node%name),val,description)
  ENDIF

ENDSUBROUTINE setSTR0
!
!-------------------------------------------------------------------------------
!> @brief Sets a rank-1 parameter of type TYPE(StringType)
!> @param this the node to set data on
!> @param name the name of the parameter to set
!> @param val the values to set to the parameter
!> @param addmissing logical to indicate if the parameter should be created if not found; optional
!>
!> If the parameter is not found and @c addmissing is not set to true, an error is thrown.
!>
SUBROUTINE setSTR1(this,name,val,description,addmissing)
  CLASS(ParamType),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  TYPE(StringType),INTENT(IN) :: val(:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
  LOGICAL(SBK),INTENT(IN),OPTIONAL :: addmissing
  !
  LOGICAL(SBK) :: laddmissing
  CLASS(ParamNode),POINTER :: node

  laddmissing=.FALSE.
  IF(PRESENT(addmissing)) laddmissing=addmissing
  IF(laddmissing) THEN
    CALL this%getOrCreateNode_Name(name,node,.TRUE.)
  ELSE
    CALL this%getNode_Name(name,node)
  ENDIF
  IF(errorChecks_set(node,laddmissing,name,PL_DATA_TYPE_STR1) == 0) THEN
    CALL node%initSTR1(CHAR(node%name),val,description)
  ENDIF

ENDSUBROUTINE setSTR1
!
!-------------------------------------------------------------------------------
!> @brief Sets a rank-2 parameter of type TYPE(StringType)
!> @param this the node to set data on
!> @param name the name of the parameter to set
!> @param val the values to set to the parameter
!> @param addmissing logical to indicate if the parameter should be created if not found; optional
!>
!> If the parameter is not found and @c addmissing is not set to true, an error is thrown.
!>
SUBROUTINE setSTR2(this,name,val,description,addmissing)
  CLASS(ParamType),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  TYPE(StringType),INTENT(IN) :: val(:,:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
  LOGICAL(SBK),INTENT(IN),OPTIONAL :: addmissing
  !
  LOGICAL(SBK) :: laddmissing
  CLASS(ParamNode),POINTER :: node

  laddmissing=.FALSE.
  IF(PRESENT(addmissing)) laddmissing=addmissing
  IF(laddmissing) THEN
    CALL this%getOrCreateNode_Name(name,node,.TRUE.)
  ELSE
    CALL this%getNode_Name(name,node)
  ENDIF
  IF(errorChecks_set(node,laddmissing,name,PL_DATA_TYPE_STR2) == 0) THEN
    CALL node%initSTR2(CHAR(node%name),val,description)
  ENDIF

ENDSUBROUTINE setSTR2
!
!-------------------------------------------------------------------------------
!> @brief Sets a rank-3 parameter of type TYPE(StringType)
!> @param this the node to set data on
!> @param name the name of the parameter to set
!> @param val the values to set to the parameter
!> @param addmissing logical to indicate if the parameter should be created if not found; optional
!>
!> If the parameter is not found and @c addmissing is not set to true, an error is thrown.
!>
SUBROUTINE setSTR3(this,name,val,description,addmissing)
  CLASS(ParamType),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  TYPE(StringType),INTENT(IN) :: val(:,:,:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
  LOGICAL(SBK),INTENT(IN),OPTIONAL :: addmissing
  !
  LOGICAL(SBK) :: laddmissing
  CLASS(ParamNode),POINTER :: node

  laddmissing=.FALSE.
  IF(PRESENT(addmissing)) laddmissing=addmissing
  IF(laddmissing) THEN
    CALL this%getOrCreateNode_Name(name,node,.TRUE.)
  ELSE
    CALL this%getNode_Name(name,node)
  ENDIF
  IF(errorChecks_set(node,laddmissing,name,PL_DATA_TYPE_STR3) == 0) THEN
    CALL node%initSTR3(CHAR(node%name),val,description)
  ENDIF

ENDSUBROUTINE setSTR3
!
!-------------------------------------------------------------------------------
!> @brief Sets a rank-4 parameter of type TYPE(StringType)
!> @param this the node to set data on
!> @param name the name of the parameter to set
!> @param val the values to set to the parameter
!> @param addmissing logical to indicate if the parameter should be created if not found; optional
!>
!> If the parameter is not found and @c addmissing is not set to true, an error is thrown.
!>
SUBROUTINE setSTR4(this,name,val,description,addmissing)
  CLASS(ParamType),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  TYPE(StringType),INTENT(IN) :: val(:,:,:,:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
  LOGICAL(SBK),INTENT(IN),OPTIONAL :: addmissing
  !
  LOGICAL(SBK) :: laddmissing
  CLASS(ParamNode),POINTER :: node

  laddmissing=.FALSE.
  IF(PRESENT(addmissing)) laddmissing=addmissing
  IF(laddmissing) THEN
    CALL this%getOrCreateNode_Name(name,node,.TRUE.)
  ELSE
    CALL this%getNode_Name(name,node)
  ENDIF
  IF(errorChecks_set(node,laddmissing,name,PL_DATA_TYPE_STR4) == 0) THEN
    CALL node%initSTR4(CHAR(node%name),val,description)
  ENDIF

ENDSUBROUTINE setSTR4
!
!-------------------------------------------------------------------------------
!> @brief Sets a rank-5 parameter of type TYPE(StringType)
!> @param this the node to set data on
!> @param name the name of the parameter to set
!> @param val the values to set to the parameter
!> @param addmissing logical to indicate if the parameter should be created if not found; optional
!>
!> If the parameter is not found and @c addmissing is not set to true, an error is thrown.
!>
SUBROUTINE setSTR5(this,name,val,description,addmissing)
  CLASS(ParamType),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  TYPE(StringType),INTENT(IN) :: val(:,:,:,:,:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
  LOGICAL(SBK),INTENT(IN),OPTIONAL :: addmissing
  !
  LOGICAL(SBK) :: laddmissing
  CLASS(ParamNode),POINTER :: node

  laddmissing=.FALSE.
  IF(PRESENT(addmissing)) laddmissing=addmissing
  IF(laddmissing) THEN
    CALL this%getOrCreateNode_Name(name,node,.TRUE.)
  ELSE
    CALL this%getNode_Name(name,node)
  ENDIF
  IF(errorChecks_set(node,laddmissing,name,PL_DATA_TYPE_STR5) == 0) THEN
    CALL node%initSTR5(CHAR(node%name),val,description)
  ENDIF

ENDSUBROUTINE setSTR5
!
!-------------------------------------------------------------------------------
!> @brief Sets a rank-6 parameter of type TYPE(StringType)
!> @param this the node to set data on
!> @param name the name of the parameter to set
!> @param val the values to set to the parameter
!> @param addmissing logical to indicate if the parameter should be created if not found; optional
!>
!> If the parameter is not found and @c addmissing is not set to true, an error is thrown.
!>
SUBROUTINE setSTR6(this,name,val,description,addmissing)
  CLASS(ParamType),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  TYPE(StringType),INTENT(IN) :: val(:,:,:,:,:,:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
  LOGICAL(SBK),INTENT(IN),OPTIONAL :: addmissing
  !
  LOGICAL(SBK) :: laddmissing
  CLASS(ParamNode),POINTER :: node

  laddmissing=.FALSE.
  IF(PRESENT(addmissing)) laddmissing=addmissing
  IF(laddmissing) THEN
    CALL this%getOrCreateNode_Name(name,node,.TRUE.)
  ELSE
    CALL this%getNode_Name(name,node)
  ENDIF
  IF(errorChecks_set(node,laddmissing,name,PL_DATA_TYPE_STR6) == 0) THEN
    CALL node%initSTR6(CHAR(node%name),val,description)
  ENDIF

ENDSUBROUTINE setSTR6
!
!-------------------------------------------------------------------------------
!> @brief Sets a rank-7 parameter of type TYPE(StringType)
!> @param this the node to set data on
!> @param name the name of the parameter to set
!> @param val the values to set to the parameter
!> @param addmissing logical to indicate if the parameter should be created if not found; optional
!>
!> If the parameter is not found and @c addmissing is not set to true, an error is thrown.
!>
SUBROUTINE setSTR7(this,name,val,description,addmissing)
  CLASS(ParamType),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  TYPE(StringType),INTENT(IN) :: val(:,:,:,:,:,:,:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
  LOGICAL(SBK),INTENT(IN),OPTIONAL :: addmissing
  !
  LOGICAL(SBK) :: laddmissing
  CLASS(ParamNode),POINTER :: node

  laddmissing=.FALSE.
  IF(PRESENT(addmissing)) laddmissing=addmissing
  IF(laddmissing) THEN
    CALL this%getOrCreateNode_Name(name,node,.TRUE.)
  ELSE
    CALL this%getNode_Name(name,node)
  ENDIF
  IF(errorChecks_set(node,laddmissing,name,PL_DATA_TYPE_STR7) == 0) THEN
    CALL node%initSTR7(CHAR(node%name),val,description)
  ENDIF

ENDSUBROUTINE setSTR7
!
!-------------------------------------------------------------------------------
!> @brief Adds a rank-0 parameter of type LOGICAL(SBK)
!> @param this the node to add data to
!> @param name the name of the parameter to set
!> @param val the values to add to the parameter
!> @param description the description of the value; optional
!>
SUBROUTINE addSBK0(this,name,val,description)
  CLASS(ParamType),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  LOGICAL(SBK),INTENT(IN) :: val
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
  !
  CLASS(ParamNode),POINTER :: node

  CALL this%getOrCreateNode_Name(name,node,.TRUE.)
  IF(errorChecks_add(node,name,PL_DATA_TYPE_SBK0) == 0) THEN
    CALL node%initSBK0(CHAR(node%name),val,description)
  ENDIF

ENDSUBROUTINE addSBK0
!
!-------------------------------------------------------------------------------
!> @brief Adds a rank-1 parameter of type LOGICAL(SBK)
!> @param this the node to add data to
!> @param name the name of the parameter to set
!> @param val the values to add to the parameter
!> @param description the description of the value; optional
!>
SUBROUTINE addSBK1(this,name,val,description)
  CLASS(ParamType),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  LOGICAL(SBK),INTENT(IN) :: val(:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
  !
  CLASS(ParamNode),POINTER :: node

  CALL this%getOrCreateNode_Name(name,node,.TRUE.)
  IF(errorChecks_add(node,name,PL_DATA_TYPE_SBK1) == 0) THEN
    CALL node%initSBK1(CHAR(node%name),val,description)
  ENDIF

ENDSUBROUTINE addSBK1
!
!-------------------------------------------------------------------------------
!> @brief Adds a rank-2 parameter of type LOGICAL(SBK)
!> @param this the node to add data to
!> @param name the name of the parameter to set
!> @param val the values to add to the parameter
!> @param description the description of the value; optional
!>
SUBROUTINE addSBK2(this,name,val,description)
  CLASS(ParamType),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  LOGICAL(SBK),INTENT(IN) :: val(:,:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
  !
  CLASS(ParamNode),POINTER :: node

  CALL this%getOrCreateNode_Name(name,node,.TRUE.)
  IF(errorChecks_add(node,name,PL_DATA_TYPE_SBK2) == 0) THEN
    CALL node%initSBK2(CHAR(node%name),val,description)
  ENDIF

ENDSUBROUTINE addSBK2
!
!-------------------------------------------------------------------------------
!> @brief Adds a rank-3 parameter of type LOGICAL(SBK)
!> @param this the node to add data to
!> @param name the name of the parameter to set
!> @param val the values to add to the parameter
!> @param description the description of the value; optional
!>
SUBROUTINE addSBK3(this,name,val,description)
  CLASS(ParamType),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  LOGICAL(SBK),INTENT(IN) :: val(:,:,:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
  !
  CLASS(ParamNode),POINTER :: node

  CALL this%getOrCreateNode_Name(name,node,.TRUE.)
  IF(errorChecks_add(node,name,PL_DATA_TYPE_SBK3) == 0) THEN
    CALL node%initSBK3(CHAR(node%name),val,description)
  ENDIF

ENDSUBROUTINE addSBK3
!
!-------------------------------------------------------------------------------
!> @brief Adds a rank-4 parameter of type LOGICAL(SBK)
!> @param this the node to add data to
!> @param name the name of the parameter to set
!> @param val the values to add to the parameter
!> @param description the description of the value; optional
!>
SUBROUTINE addSBK4(this,name,val,description)
  CLASS(ParamType),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  LOGICAL(SBK),INTENT(IN) :: val(:,:,:,:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
  !
  CLASS(ParamNode),POINTER :: node

  CALL this%getOrCreateNode_Name(name,node,.TRUE.)
  IF(errorChecks_add(node,name,PL_DATA_TYPE_SBK4) == 0) THEN
    CALL node%initSBK4(CHAR(node%name),val,description)
  ENDIF

ENDSUBROUTINE addSBK4
!
!-------------------------------------------------------------------------------
!> @brief Adds a rank-5 parameter of type LOGICAL(SBK)
!> @param this the node to add data to
!> @param name the name of the parameter to set
!> @param val the values to add to the parameter
!> @param description the description of the value; optional
!>
SUBROUTINE addSBK5(this,name,val,description)
  CLASS(ParamType),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  LOGICAL(SBK),INTENT(IN) :: val(:,:,:,:,:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
  !
  CLASS(ParamNode),POINTER :: node

  CALL this%getOrCreateNode_Name(name,node,.TRUE.)
  IF(errorChecks_add(node,name,PL_DATA_TYPE_SBK5) == 0) THEN
    CALL node%initSBK5(CHAR(node%name),val,description)
  ENDIF

ENDSUBROUTINE addSBK5
!
!-------------------------------------------------------------------------------
!> @brief Adds a rank-6 parameter of type LOGICAL(SBK)
!> @param this the node to add data to
!> @param name the name of the parameter to set
!> @param val the values to add to the parameter
!> @param description the description of the value; optional
!>
SUBROUTINE addSBK6(this,name,val,description)
  CLASS(ParamType),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  LOGICAL(SBK),INTENT(IN) :: val(:,:,:,:,:,:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
  !
  CLASS(ParamNode),POINTER :: node

  CALL this%getOrCreateNode_Name(name,node,.TRUE.)
  IF(errorChecks_add(node,name,PL_DATA_TYPE_SBK6) == 0) THEN
    CALL node%initSBK6(CHAR(node%name),val,description)
  ENDIF

ENDSUBROUTINE addSBK6
!
!-------------------------------------------------------------------------------
!> @brief Adds a rank-7 parameter of type LOGICAL(SBK)
!> @param this the node to add data to
!> @param name the name of the parameter to set
!> @param val the values to add to the parameter
!> @param description the description of the value; optional
!>
SUBROUTINE addSBK7(this,name,val,description)
  CLASS(ParamType),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  LOGICAL(SBK),INTENT(IN) :: val(:,:,:,:,:,:,:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
  !
  CLASS(ParamNode),POINTER :: node

  CALL this%getOrCreateNode_Name(name,node,.TRUE.)
  IF(errorChecks_add(node,name,PL_DATA_TYPE_SBK7) == 0) THEN
    CALL node%initSBK7(CHAR(node%name),val,description)
  ENDIF

ENDSUBROUTINE addSBK7
!
!-------------------------------------------------------------------------------
!> @brief Adds a rank-0 parameter of type INTEGER(SNK)
!> @param this the node to add data to
!> @param name the name of the parameter to set
!> @param val the values to add to the parameter
!> @param description the description of the value; optional
!>
SUBROUTINE addSNK0(this,name,val,description)
  CLASS(ParamType),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  INTEGER(SNK),INTENT(IN) :: val
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
  !
  CLASS(ParamNode),POINTER :: node

  CALL this%getOrCreateNode_Name(name,node,.TRUE.)
  IF(errorChecks_add(node,name,PL_DATA_TYPE_SNK0) == 0) THEN
    CALL node%initSNK0(CHAR(node%name),val,description)
  ENDIF

ENDSUBROUTINE addSNK0
!
!-------------------------------------------------------------------------------
!> @brief Adds a rank-1 parameter of type INTEGER(SNK)
!> @param this the node to add data to
!> @param name the name of the parameter to set
!> @param val the values to add to the parameter
!> @param description the description of the value; optional
!>
SUBROUTINE addSNK1(this,name,val,description)
  CLASS(ParamType),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  INTEGER(SNK),INTENT(IN) :: val(:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
  !
  CLASS(ParamNode),POINTER :: node

  CALL this%getOrCreateNode_Name(name,node,.TRUE.)
  IF(errorChecks_add(node,name,PL_DATA_TYPE_SNK1) == 0) THEN
    CALL node%initSNK1(CHAR(node%name),val,description)
  ENDIF

ENDSUBROUTINE addSNK1
!
!-------------------------------------------------------------------------------
!> @brief Adds a rank-2 parameter of type INTEGER(SNK)
!> @param this the node to add data to
!> @param name the name of the parameter to set
!> @param val the values to add to the parameter
!> @param description the description of the value; optional
!>
SUBROUTINE addSNK2(this,name,val,description)
  CLASS(ParamType),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  INTEGER(SNK),INTENT(IN) :: val(:,:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
  !
  CLASS(ParamNode),POINTER :: node

  CALL this%getOrCreateNode_Name(name,node,.TRUE.)
  IF(errorChecks_add(node,name,PL_DATA_TYPE_SNK2) == 0) THEN
    CALL node%initSNK2(CHAR(node%name),val,description)
  ENDIF

ENDSUBROUTINE addSNK2
!
!-------------------------------------------------------------------------------
!> @brief Adds a rank-3 parameter of type INTEGER(SNK)
!> @param this the node to add data to
!> @param name the name of the parameter to set
!> @param val the values to add to the parameter
!> @param description the description of the value; optional
!>
SUBROUTINE addSNK3(this,name,val,description)
  CLASS(ParamType),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  INTEGER(SNK),INTENT(IN) :: val(:,:,:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
  !
  CLASS(ParamNode),POINTER :: node

  CALL this%getOrCreateNode_Name(name,node,.TRUE.)
  IF(errorChecks_add(node,name,PL_DATA_TYPE_SNK3) == 0) THEN
    CALL node%initSNK3(CHAR(node%name),val,description)
  ENDIF

ENDSUBROUTINE addSNK3
!
!-------------------------------------------------------------------------------
!> @brief Adds a rank-4 parameter of type INTEGER(SNK)
!> @param this the node to add data to
!> @param name the name of the parameter to set
!> @param val the values to add to the parameter
!> @param description the description of the value; optional
!>
SUBROUTINE addSNK4(this,name,val,description)
  CLASS(ParamType),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  INTEGER(SNK),INTENT(IN) :: val(:,:,:,:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
  !
  CLASS(ParamNode),POINTER :: node

  CALL this%getOrCreateNode_Name(name,node,.TRUE.)
  IF(errorChecks_add(node,name,PL_DATA_TYPE_SNK4) == 0) THEN
    CALL node%initSNK4(CHAR(node%name),val,description)
  ENDIF

ENDSUBROUTINE addSNK4
!
!-------------------------------------------------------------------------------
!> @brief Adds a rank-5 parameter of type INTEGER(SNK)
!> @param this the node to add data to
!> @param name the name of the parameter to set
!> @param val the values to add to the parameter
!> @param description the description of the value; optional
!>
SUBROUTINE addSNK5(this,name,val,description)
  CLASS(ParamType),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  INTEGER(SNK),INTENT(IN) :: val(:,:,:,:,:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
  !
  CLASS(ParamNode),POINTER :: node

  CALL this%getOrCreateNode_Name(name,node,.TRUE.)
  IF(errorChecks_add(node,name,PL_DATA_TYPE_SNK5) == 0) THEN
    CALL node%initSNK5(CHAR(node%name),val,description)
  ENDIF

ENDSUBROUTINE addSNK5
!
!-------------------------------------------------------------------------------
!> @brief Adds a rank-6 parameter of type INTEGER(SNK)
!> @param this the node to add data to
!> @param name the name of the parameter to set
!> @param val the values to add to the parameter
!> @param description the description of the value; optional
!>
SUBROUTINE addSNK6(this,name,val,description)
  CLASS(ParamType),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  INTEGER(SNK),INTENT(IN) :: val(:,:,:,:,:,:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
  !
  CLASS(ParamNode),POINTER :: node

  CALL this%getOrCreateNode_Name(name,node,.TRUE.)
  IF(errorChecks_add(node,name,PL_DATA_TYPE_SNK6) == 0) THEN
    CALL node%initSNK6(CHAR(node%name),val,description)
  ENDIF

ENDSUBROUTINE addSNK6
!
!-------------------------------------------------------------------------------
!> @brief Adds a rank-7 parameter of type INTEGER(SNK)
!> @param this the node to add data to
!> @param name the name of the parameter to set
!> @param val the values to add to the parameter
!> @param description the description of the value; optional
!>
SUBROUTINE addSNK7(this,name,val,description)
  CLASS(ParamType),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  INTEGER(SNK),INTENT(IN) :: val(:,:,:,:,:,:,:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
  !
  CLASS(ParamNode),POINTER :: node

  CALL this%getOrCreateNode_Name(name,node,.TRUE.)
  IF(errorChecks_add(node,name,PL_DATA_TYPE_SNK7) == 0) THEN
    CALL node%initSNK7(CHAR(node%name),val,description)
  ENDIF

ENDSUBROUTINE addSNK7
!
!-------------------------------------------------------------------------------
!> @brief Adds a rank-0 parameter of type INTEGER(SLK)
!> @param this the node to add data to
!> @param name the name of the parameter to set
!> @param val the values to add to the parameter
!> @param description the description of the value; optional
!>
SUBROUTINE addSLK0(this,name,val,description)
  CLASS(ParamType),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  INTEGER(SLK),INTENT(IN) :: val
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
  !
  CLASS(ParamNode),POINTER :: node

  CALL this%getOrCreateNode_Name(name,node,.TRUE.)
  IF(errorChecks_add(node,name,PL_DATA_TYPE_SLK0) == 0) THEN
    CALL node%initSLK0(CHAR(node%name),val,description)
  ENDIF

ENDSUBROUTINE addSLK0
!
!-------------------------------------------------------------------------------
!> @brief Adds a rank-1 parameter of type INTEGER(SLK)
!> @param this the node to add data to
!> @param name the name of the parameter to set
!> @param val the values to add to the parameter
!> @param description the description of the value; optional
!>
SUBROUTINE addSLK1(this,name,val,description)
  CLASS(ParamType),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  INTEGER(SLK),INTENT(IN) :: val(:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
  !
  CLASS(ParamNode),POINTER :: node

  CALL this%getOrCreateNode_Name(name,node,.TRUE.)
  IF(errorChecks_add(node,name,PL_DATA_TYPE_SLK1) == 0) THEN
    CALL node%initSLK1(CHAR(node%name),val,description)
  ENDIF

ENDSUBROUTINE addSLK1
!
!-------------------------------------------------------------------------------
!> @brief Adds a rank-2 parameter of type INTEGER(SLK)
!> @param this the node to add data to
!> @param name the name of the parameter to set
!> @param val the values to add to the parameter
!> @param description the description of the value; optional
!>
SUBROUTINE addSLK2(this,name,val,description)
  CLASS(ParamType),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  INTEGER(SLK),INTENT(IN) :: val(:,:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
  !
  CLASS(ParamNode),POINTER :: node

  CALL this%getOrCreateNode_Name(name,node,.TRUE.)
  IF(errorChecks_add(node,name,PL_DATA_TYPE_SLK2) == 0) THEN
    CALL node%initSLK2(CHAR(node%name),val,description)
  ENDIF

ENDSUBROUTINE addSLK2
!
!-------------------------------------------------------------------------------
!> @brief Adds a rank-3 parameter of type INTEGER(SLK)
!> @param this the node to add data to
!> @param name the name of the parameter to set
!> @param val the values to add to the parameter
!> @param description the description of the value; optional
!>
SUBROUTINE addSLK3(this,name,val,description)
  CLASS(ParamType),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  INTEGER(SLK),INTENT(IN) :: val(:,:,:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
  !
  CLASS(ParamNode),POINTER :: node

  CALL this%getOrCreateNode_Name(name,node,.TRUE.)
  IF(errorChecks_add(node,name,PL_DATA_TYPE_SLK3) == 0) THEN
    CALL node%initSLK3(CHAR(node%name),val,description)
  ENDIF

ENDSUBROUTINE addSLK3
!
!-------------------------------------------------------------------------------
!> @brief Adds a rank-4 parameter of type INTEGER(SLK)
!> @param this the node to add data to
!> @param name the name of the parameter to set
!> @param val the values to add to the parameter
!> @param description the description of the value; optional
!>
SUBROUTINE addSLK4(this,name,val,description)
  CLASS(ParamType),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  INTEGER(SLK),INTENT(IN) :: val(:,:,:,:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
  !
  CLASS(ParamNode),POINTER :: node

  CALL this%getOrCreateNode_Name(name,node,.TRUE.)
  IF(errorChecks_add(node,name,PL_DATA_TYPE_SLK4) == 0) THEN
    CALL node%initSLK4(CHAR(node%name),val,description)
  ENDIF

ENDSUBROUTINE addSLK4
!
!-------------------------------------------------------------------------------
!> @brief Adds a rank-5 parameter of type INTEGER(SLK)
!> @param this the node to add data to
!> @param name the name of the parameter to set
!> @param val the values to add to the parameter
!> @param description the description of the value; optional
!>
SUBROUTINE addSLK5(this,name,val,description)
  CLASS(ParamType),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  INTEGER(SLK),INTENT(IN) :: val(:,:,:,:,:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
  !
  CLASS(ParamNode),POINTER :: node

  CALL this%getOrCreateNode_Name(name,node,.TRUE.)
  IF(errorChecks_add(node,name,PL_DATA_TYPE_SLK5) == 0) THEN
    CALL node%initSLK5(CHAR(node%name),val,description)
  ENDIF

ENDSUBROUTINE addSLK5
!
!-------------------------------------------------------------------------------
!> @brief Adds a rank-6 parameter of type INTEGER(SLK)
!> @param this the node to add data to
!> @param name the name of the parameter to set
!> @param val the values to add to the parameter
!> @param description the description of the value; optional
!>
SUBROUTINE addSLK6(this,name,val,description)
  CLASS(ParamType),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  INTEGER(SLK),INTENT(IN) :: val(:,:,:,:,:,:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
  !
  CLASS(ParamNode),POINTER :: node

  CALL this%getOrCreateNode_Name(name,node,.TRUE.)
  IF(errorChecks_add(node,name,PL_DATA_TYPE_SLK6) == 0) THEN
    CALL node%initSLK6(CHAR(node%name),val,description)
  ENDIF

ENDSUBROUTINE addSLK6
!
!-------------------------------------------------------------------------------
!> @brief Adds a rank-7 parameter of type INTEGER(SLK)
!> @param this the node to add data to
!> @param name the name of the parameter to set
!> @param val the values to add to the parameter
!> @param description the description of the value; optional
!>
SUBROUTINE addSLK7(this,name,val,description)
  CLASS(ParamType),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  INTEGER(SLK),INTENT(IN) :: val(:,:,:,:,:,:,:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
  !
  CLASS(ParamNode),POINTER :: node

  CALL this%getOrCreateNode_Name(name,node,.TRUE.)
  IF(errorChecks_add(node,name,PL_DATA_TYPE_SLK7) == 0) THEN
    CALL node%initSLK7(CHAR(node%name),val,description)
  ENDIF

ENDSUBROUTINE addSLK7
!
!-------------------------------------------------------------------------------
!> @brief Adds a rank-0 parameter of type REAL(SSK)
!> @param this the node to add data to
!> @param name the name of the parameter to set
!> @param val the values to add to the parameter
!> @param description the description of the value; optional
!>
SUBROUTINE addSSK0(this,name,val,description)
  CLASS(ParamType),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  REAL(SSK),INTENT(IN) :: val
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
  !
  CLASS(ParamNode),POINTER :: node

  CALL this%getOrCreateNode_Name(name,node,.TRUE.)
  IF(errorChecks_add(node,name,PL_DATA_TYPE_SSK0) == 0) THEN
    CALL node%initSSK0(CHAR(node%name),val,description)
  ENDIF

ENDSUBROUTINE addSSK0
!
!-------------------------------------------------------------------------------
!> @brief Adds a rank-1 parameter of type REAL(SSK)
!> @param this the node to add data to
!> @param name the name of the parameter to set
!> @param val the values to add to the parameter
!> @param description the description of the value; optional
!>
SUBROUTINE addSSK1(this,name,val,description)
  CLASS(ParamType),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  REAL(SSK),INTENT(IN) :: val(:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
  !
  CLASS(ParamNode),POINTER :: node

  CALL this%getOrCreateNode_Name(name,node,.TRUE.)
  IF(errorChecks_add(node,name,PL_DATA_TYPE_SSK1) == 0) THEN
    CALL node%initSSK1(CHAR(node%name),val,description)
  ENDIF

ENDSUBROUTINE addSSK1
!
!-------------------------------------------------------------------------------
!> @brief Adds a rank-2 parameter of type REAL(SSK)
!> @param this the node to add data to
!> @param name the name of the parameter to set
!> @param val the values to add to the parameter
!> @param description the description of the value; optional
!>
SUBROUTINE addSSK2(this,name,val,description)
  CLASS(ParamType),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  REAL(SSK),INTENT(IN) :: val(:,:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
  !
  CLASS(ParamNode),POINTER :: node

  CALL this%getOrCreateNode_Name(name,node,.TRUE.)
  IF(errorChecks_add(node,name,PL_DATA_TYPE_SSK2) == 0) THEN
    CALL node%initSSK2(CHAR(node%name),val,description)
  ENDIF

ENDSUBROUTINE addSSK2
!
!-------------------------------------------------------------------------------
!> @brief Adds a rank-3 parameter of type REAL(SSK)
!> @param this the node to add data to
!> @param name the name of the parameter to set
!> @param val the values to add to the parameter
!> @param description the description of the value; optional
!>
SUBROUTINE addSSK3(this,name,val,description)
  CLASS(ParamType),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  REAL(SSK),INTENT(IN) :: val(:,:,:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
  !
  CLASS(ParamNode),POINTER :: node

  CALL this%getOrCreateNode_Name(name,node,.TRUE.)
  IF(errorChecks_add(node,name,PL_DATA_TYPE_SSK3) == 0) THEN
    CALL node%initSSK3(CHAR(node%name),val,description)
  ENDIF

ENDSUBROUTINE addSSK3
!
!-------------------------------------------------------------------------------
!> @brief Adds a rank-4 parameter of type REAL(SSK)
!> @param this the node to add data to
!> @param name the name of the parameter to set
!> @param val the values to add to the parameter
!> @param description the description of the value; optional
!>
SUBROUTINE addSSK4(this,name,val,description)
  CLASS(ParamType),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  REAL(SSK),INTENT(IN) :: val(:,:,:,:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
  !
  CLASS(ParamNode),POINTER :: node

  CALL this%getOrCreateNode_Name(name,node,.TRUE.)
  IF(errorChecks_add(node,name,PL_DATA_TYPE_SSK4) == 0) THEN
    CALL node%initSSK4(CHAR(node%name),val,description)
  ENDIF

ENDSUBROUTINE addSSK4
!
!-------------------------------------------------------------------------------
!> @brief Adds a rank-5 parameter of type REAL(SSK)
!> @param this the node to add data to
!> @param name the name of the parameter to set
!> @param val the values to add to the parameter
!> @param description the description of the value; optional
!>
SUBROUTINE addSSK5(this,name,val,description)
  CLASS(ParamType),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  REAL(SSK),INTENT(IN) :: val(:,:,:,:,:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
  !
  CLASS(ParamNode),POINTER :: node

  CALL this%getOrCreateNode_Name(name,node,.TRUE.)
  IF(errorChecks_add(node,name,PL_DATA_TYPE_SSK5) == 0) THEN
    CALL node%initSSK5(CHAR(node%name),val,description)
  ENDIF

ENDSUBROUTINE addSSK5
!
!-------------------------------------------------------------------------------
!> @brief Adds a rank-6 parameter of type REAL(SSK)
!> @param this the node to add data to
!> @param name the name of the parameter to set
!> @param val the values to add to the parameter
!> @param description the description of the value; optional
!>
SUBROUTINE addSSK6(this,name,val,description)
  CLASS(ParamType),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  REAL(SSK),INTENT(IN) :: val(:,:,:,:,:,:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
  !
  CLASS(ParamNode),POINTER :: node

  CALL this%getOrCreateNode_Name(name,node,.TRUE.)
  IF(errorChecks_add(node,name,PL_DATA_TYPE_SSK6) == 0) THEN
    CALL node%initSSK6(CHAR(node%name),val,description)
  ENDIF

ENDSUBROUTINE addSSK6
!
!-------------------------------------------------------------------------------
!> @brief Adds a rank-7 parameter of type REAL(SSK)
!> @param this the node to add data to
!> @param name the name of the parameter to set
!> @param val the values to add to the parameter
!> @param description the description of the value; optional
!>
SUBROUTINE addSSK7(this,name,val,description)
  CLASS(ParamType),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  REAL(SSK),INTENT(IN) :: val(:,:,:,:,:,:,:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
  !
  CLASS(ParamNode),POINTER :: node

  CALL this%getOrCreateNode_Name(name,node,.TRUE.)
  IF(errorChecks_add(node,name,PL_DATA_TYPE_SSK7) == 0) THEN
    CALL node%initSSK7(CHAR(node%name),val,description)
  ENDIF

ENDSUBROUTINE addSSK7
!
!-------------------------------------------------------------------------------
!> @brief Adds a rank-0 parameter of type REAL(SDK)
!> @param this the node to add data to
!> @param name the name of the parameter to set
!> @param val the values to add to the parameter
!> @param description the description of the value; optional
!>
SUBROUTINE addSDK0(this,name,val,description)
  CLASS(ParamType),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  REAL(SDK),INTENT(IN) :: val
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
  !
  CLASS(ParamNode),POINTER :: node

  CALL this%getOrCreateNode_Name(name,node,.TRUE.)
  IF(errorChecks_add(node,name,PL_DATA_TYPE_SDK0) == 0) THEN
    CALL node%initSDK0(CHAR(node%name),val,description)
  ENDIF

ENDSUBROUTINE addSDK0
!
!-------------------------------------------------------------------------------
!> @brief Adds a rank-1 parameter of type REAL(SDK)
!> @param this the node to add data to
!> @param name the name of the parameter to set
!> @param val the values to add to the parameter
!> @param description the description of the value; optional
!>
SUBROUTINE addSDK1(this,name,val,description)
  CLASS(ParamType),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  REAL(SDK),INTENT(IN) :: val(:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
  !
  CLASS(ParamNode),POINTER :: node

  CALL this%getOrCreateNode_Name(name,node,.TRUE.)
  IF(errorChecks_add(node,name,PL_DATA_TYPE_SDK1) == 0) THEN
    CALL node%initSDK1(CHAR(node%name),val,description)
  ENDIF

ENDSUBROUTINE addSDK1
!
!-------------------------------------------------------------------------------
!> @brief Adds a rank-2 parameter of type REAL(SDK)
!> @param this the node to add data to
!> @param name the name of the parameter to set
!> @param val the values to add to the parameter
!> @param description the description of the value; optional
!>
SUBROUTINE addSDK2(this,name,val,description)
  CLASS(ParamType),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  REAL(SDK),INTENT(IN) :: val(:,:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
  !
  CLASS(ParamNode),POINTER :: node

  CALL this%getOrCreateNode_Name(name,node,.TRUE.)
  IF(errorChecks_add(node,name,PL_DATA_TYPE_SDK2) == 0) THEN
    CALL node%initSDK2(CHAR(node%name),val,description)
  ENDIF

ENDSUBROUTINE addSDK2
!
!-------------------------------------------------------------------------------
!> @brief Adds a rank-3 parameter of type REAL(SDK)
!> @param this the node to add data to
!> @param name the name of the parameter to set
!> @param val the values to add to the parameter
!> @param description the description of the value; optional
!>
SUBROUTINE addSDK3(this,name,val,description)
  CLASS(ParamType),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  REAL(SDK),INTENT(IN) :: val(:,:,:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
  !
  CLASS(ParamNode),POINTER :: node

  CALL this%getOrCreateNode_Name(name,node,.TRUE.)
  IF(errorChecks_add(node,name,PL_DATA_TYPE_SDK3) == 0) THEN
    CALL node%initSDK3(CHAR(node%name),val,description)
  ENDIF

ENDSUBROUTINE addSDK3
!
!-------------------------------------------------------------------------------
!> @brief Adds a rank-4 parameter of type REAL(SDK)
!> @param this the node to add data to
!> @param name the name of the parameter to set
!> @param val the values to add to the parameter
!> @param description the description of the value; optional
!>
SUBROUTINE addSDK4(this,name,val,description)
  CLASS(ParamType),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  REAL(SDK),INTENT(IN) :: val(:,:,:,:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
  !
  CLASS(ParamNode),POINTER :: node

  CALL this%getOrCreateNode_Name(name,node,.TRUE.)
  IF(errorChecks_add(node,name,PL_DATA_TYPE_SDK4) == 0) THEN
    CALL node%initSDK4(CHAR(node%name),val,description)
  ENDIF

ENDSUBROUTINE addSDK4
!
!-------------------------------------------------------------------------------
!> @brief Adds a rank-5 parameter of type REAL(SDK)
!> @param this the node to add data to
!> @param name the name of the parameter to set
!> @param val the values to add to the parameter
!> @param description the description of the value; optional
!>
SUBROUTINE addSDK5(this,name,val,description)
  CLASS(ParamType),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  REAL(SDK),INTENT(IN) :: val(:,:,:,:,:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
  !
  CLASS(ParamNode),POINTER :: node

  CALL this%getOrCreateNode_Name(name,node,.TRUE.)
  IF(errorChecks_add(node,name,PL_DATA_TYPE_SDK5) == 0) THEN
    CALL node%initSDK5(CHAR(node%name),val,description)
  ENDIF

ENDSUBROUTINE addSDK5
!
!-------------------------------------------------------------------------------
!> @brief Adds a rank-6 parameter of type REAL(SDK)
!> @param this the node to add data to
!> @param name the name of the parameter to set
!> @param val the values to add to the parameter
!> @param description the description of the value; optional
!>
SUBROUTINE addSDK6(this,name,val,description)
  CLASS(ParamType),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  REAL(SDK),INTENT(IN) :: val(:,:,:,:,:,:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
  !
  CLASS(ParamNode),POINTER :: node

  CALL this%getOrCreateNode_Name(name,node,.TRUE.)
  IF(errorChecks_add(node,name,PL_DATA_TYPE_SDK6) == 0) THEN
    CALL node%initSDK6(CHAR(node%name),val,description)
  ENDIF

ENDSUBROUTINE addSDK6
!
!-------------------------------------------------------------------------------
!> @brief Adds a rank-7 parameter of type REAL(SDK)
!> @param this the node to add data to
!> @param name the name of the parameter to set
!> @param val the values to add to the parameter
!> @param description the description of the value; optional
!>
SUBROUTINE addSDK7(this,name,val,description)
  CLASS(ParamType),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  REAL(SDK),INTENT(IN) :: val(:,:,:,:,:,:,:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
  !
  CLASS(ParamNode),POINTER :: node

  CALL this%getOrCreateNode_Name(name,node,.TRUE.)
  IF(errorChecks_add(node,name,PL_DATA_TYPE_SDK7) == 0) THEN
    CALL node%initSDK7(CHAR(node%name),val,description)
  ENDIF

ENDSUBROUTINE addSDK7
!
!-------------------------------------------------------------------------------
!> @brief Adds a rank-0 parameter of type TYPE(StringType)
!> @param this the node to add data to
!> @param name the name of the parameter to set
!> @param val the values to add to the parameter
!> @param description the description of the value; optional
!>
SUBROUTINE addSTR0(this,name,val,description)
  CLASS(ParamType),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  TYPE(StringType),INTENT(IN) :: val
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
  !
  CLASS(ParamNode),POINTER :: node

  CALL this%getOrCreateNode_Name(name,node,.TRUE.)
  IF(errorChecks_add(node,name,PL_DATA_TYPE_STR0) == 0) THEN
    CALL node%initSTR0(CHAR(node%name),val,description)
  ENDIF

ENDSUBROUTINE addSTR0
!
!-------------------------------------------------------------------------------
!> @brief Adds a rank-1 parameter of type TYPE(StringType)
!> @param this the node to add data to
!> @param name the name of the parameter to set
!> @param val the values to add to the parameter
!> @param description the description of the value; optional
!>
SUBROUTINE addSTR1(this,name,val,description)
  CLASS(ParamType),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  TYPE(StringType),INTENT(IN) :: val(:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
  !
  CLASS(ParamNode),POINTER :: node

  CALL this%getOrCreateNode_Name(name,node,.TRUE.)
  IF(errorChecks_add(node,name,PL_DATA_TYPE_STR1) == 0) THEN
    CALL node%initSTR1(CHAR(node%name),val,description)
  ENDIF

ENDSUBROUTINE addSTR1
!
!-------------------------------------------------------------------------------
!> @brief Adds a rank-2 parameter of type TYPE(StringType)
!> @param this the node to add data to
!> @param name the name of the parameter to set
!> @param val the values to add to the parameter
!> @param description the description of the value; optional
!>
SUBROUTINE addSTR2(this,name,val,description)
  CLASS(ParamType),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  TYPE(StringType),INTENT(IN) :: val(:,:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
  !
  CLASS(ParamNode),POINTER :: node

  CALL this%getOrCreateNode_Name(name,node,.TRUE.)
  IF(errorChecks_add(node,name,PL_DATA_TYPE_STR2) == 0) THEN
    CALL node%initSTR2(CHAR(node%name),val,description)
  ENDIF

ENDSUBROUTINE addSTR2
!
!-------------------------------------------------------------------------------
!> @brief Adds a rank-3 parameter of type TYPE(StringType)
!> @param this the node to add data to
!> @param name the name of the parameter to set
!> @param val the values to add to the parameter
!> @param description the description of the value; optional
!>
SUBROUTINE addSTR3(this,name,val,description)
  CLASS(ParamType),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  TYPE(StringType),INTENT(IN) :: val(:,:,:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
  !
  CLASS(ParamNode),POINTER :: node

  CALL this%getOrCreateNode_Name(name,node,.TRUE.)
  IF(errorChecks_add(node,name,PL_DATA_TYPE_STR3) == 0) THEN
    CALL node%initSTR3(CHAR(node%name),val,description)
  ENDIF

ENDSUBROUTINE addSTR3
!
!-------------------------------------------------------------------------------
!> @brief Adds a rank-4 parameter of type TYPE(StringType)
!> @param this the node to add data to
!> @param name the name of the parameter to set
!> @param val the values to add to the parameter
!> @param description the description of the value; optional
!>
SUBROUTINE addSTR4(this,name,val,description)
  CLASS(ParamType),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  TYPE(StringType),INTENT(IN) :: val(:,:,:,:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
  !
  CLASS(ParamNode),POINTER :: node

  CALL this%getOrCreateNode_Name(name,node,.TRUE.)
  IF(errorChecks_add(node,name,PL_DATA_TYPE_STR4) == 0) THEN
    CALL node%initSTR4(CHAR(node%name),val,description)
  ENDIF

ENDSUBROUTINE addSTR4
!
!-------------------------------------------------------------------------------
!> @brief Adds a rank-5 parameter of type TYPE(StringType)
!> @param this the node to add data to
!> @param name the name of the parameter to set
!> @param val the values to add to the parameter
!> @param description the description of the value; optional
!>
SUBROUTINE addSTR5(this,name,val,description)
  CLASS(ParamType),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  TYPE(StringType),INTENT(IN) :: val(:,:,:,:,:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
  !
  CLASS(ParamNode),POINTER :: node

  CALL this%getOrCreateNode_Name(name,node,.TRUE.)
  IF(errorChecks_add(node,name,PL_DATA_TYPE_STR5) == 0) THEN
    CALL node%initSTR5(CHAR(node%name),val,description)
  ENDIF

ENDSUBROUTINE addSTR5
!
!-------------------------------------------------------------------------------
!> @brief Adds a rank-6 parameter of type TYPE(StringType)
!> @param this the node to add data to
!> @param name the name of the parameter to set
!> @param val the values to add to the parameter
!> @param description the description of the value; optional
!>
SUBROUTINE addSTR6(this,name,val,description)
  CLASS(ParamType),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  TYPE(StringType),INTENT(IN) :: val(:,:,:,:,:,:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
  !
  CLASS(ParamNode),POINTER :: node

  CALL this%getOrCreateNode_Name(name,node,.TRUE.)
  IF(errorChecks_add(node,name,PL_DATA_TYPE_STR6) == 0) THEN
    CALL node%initSTR6(CHAR(node%name),val,description)
  ENDIF

ENDSUBROUTINE addSTR6
!
!-------------------------------------------------------------------------------
!> @brief Adds a rank-7 parameter of type TYPE(StringType)
!> @param this the node to add data to
!> @param name the name of the parameter to set
!> @param val the values to add to the parameter
!> @param description the description of the value; optional
!>
SUBROUTINE addSTR7(this,name,val,description)
  CLASS(ParamType),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  TYPE(StringType),INTENT(IN) :: val(:,:,:,:,:,:,:)
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: description
  !
  CLASS(ParamNode),POINTER :: node

  CALL this%getOrCreateNode_Name(name,node,.TRUE.)
  IF(errorChecks_add(node,name,PL_DATA_TYPE_STR7) == 0) THEN
    CALL node%initSTR7(CHAR(node%name),val,description)
  ENDIF

ENDSUBROUTINE addSTR7
!
!-------------------------------------------------------------------------------
!> @brief edits a rank-0 parameter of type LOGICAL(SBK)
!> @param this the parameter to edit
!> @param funit the file unit to write to
!> @param indent the additional indentation with which to prefix the parameter
!> @param prefix the additional prefix to prepend to the parameter
!> @param shift the additional spacing to add before the data
!> @param description the description of the parameter
!> @param paddtw logical to indicate if the parameter type should be padded to force alignment;
!>               optional, defaults to false
!>
SUBROUTINE editToText_SBK0(this,funit,indent,prefix,shift,description,paddtw)
  CLASS(Param_SBK0),INTENT(IN) :: this
  INTEGER(SIK),INTENT(IN) :: funit
  INTEGER(SIK),INTENT(IN) :: indent
  TYPE(StringType),INTENT(IN) :: prefix
  INTEGER(SIK),INTENT(IN) :: shift
  CHARACTER(LEN=*),INTENT(IN) :: description
  LOGICAL(SBK),INTENT(IN) :: paddtw
  !
  WRITE(UNIT=funit,FMT='(l,a)') this%p,' ! '//description

ENDSUBROUTINE editToText_SBK0
!
!-------------------------------------------------------------------------------
!> @brief edits a rank-1 parameter of type LOGICAL(SBK)
!> @param this the parameter to edit
!> @param funit the file unit to write to
!> @param indent the additional indentation with which to prefix the parameter
!> @param prefix the additional prefix to prepend to the parameter
!> @param shift the additional spacing to add before the data
!> @param description the description of the parameter
!> @param paddtw logical to indicate if the parameter type should be padded to force alignment;
!>               optional, defaults to false
!>
SUBROUTINE editToText_SBK1(this,funit,indent,prefix,shift,description,paddtw)
  CLASS(Param_SBK1),INTENT(IN) :: this
  INTEGER(SIK),INTENT(IN) :: funit
  INTEGER(SIK),INTENT(IN) :: indent
  TYPE(StringType),INTENT(IN) :: prefix
  INTEGER(SIK),INTENT(IN) :: shift
  CHARACTER(LEN=*),INTENT(IN) :: description
  LOGICAL(SBK),INTENT(IN) :: paddtw
  !
  TYPE(StringType) :: editString
  CALL this%getString('',editString)
  WRITE(UNIT=funit,FMT='(",",a)') editString//' ! '//description

ENDSUBROUTINE editToText_SBK1
!
!-------------------------------------------------------------------------------
!> @brief edits a rank-2 parameter of type LOGICAL(SBK)
!> @param this the parameter to edit
!> @param funit the file unit to write to
!> @param indent the additional indentation with which to prefix the parameter
!> @param prefix the additional prefix to prepend to the parameter
!> @param shift the additional spacing to add before the data
!> @param description the description of the parameter
!> @param paddtw logical to indicate if the parameter type should be padded to force alignment;
!>               optional, defaults to false
!>
SUBROUTINE editToText_SBK2(this,funit,indent,prefix,shift,description,paddtw)
  CLASS(Param_SBK2),INTENT(IN) :: this
  INTEGER(SIK),INTENT(IN) :: funit
  INTEGER(SIK),INTENT(IN) :: indent
  TYPE(StringType),INTENT(IN) :: prefix
  INTEGER(SIK),INTENT(IN) :: shift
  CHARACTER(LEN=*),INTENT(IN) :: description
  LOGICAL(SBK),INTENT(IN) :: paddtw
  !
  TYPE(StringType) :: editString
  CALL this%getString('',editString)
  WRITE(UNIT=funit,FMT='(",",a)') editString//' ! '//description

ENDSUBROUTINE editToText_SBK2
!
!-------------------------------------------------------------------------------
!> @brief edits a rank-3 parameter of type LOGICAL(SBK)
!> @param this the parameter to edit
!> @param funit the file unit to write to
!> @param indent the additional indentation with which to prefix the parameter
!> @param prefix the additional prefix to prepend to the parameter
!> @param shift the additional spacing to add before the data
!> @param description the description of the parameter
!> @param paddtw logical to indicate if the parameter type should be padded to force alignment;
!>               optional, defaults to false
!>
SUBROUTINE editToText_SBK3(this,funit,indent,prefix,shift,description,paddtw)
  CLASS(Param_SBK3),INTENT(IN) :: this
  INTEGER(SIK),INTENT(IN) :: funit
  INTEGER(SIK),INTENT(IN) :: indent
  TYPE(StringType),INTENT(IN) :: prefix
  INTEGER(SIK),INTENT(IN) :: shift
  CHARACTER(LEN=*),INTENT(IN) :: description
  LOGICAL(SBK),INTENT(IN) :: paddtw
  !
  TYPE(StringType) :: editString
  CALL this%getString('',editString)
  WRITE(UNIT=funit,FMT='(",",a)') editString//' ! '//description

ENDSUBROUTINE editToText_SBK3
!
!-------------------------------------------------------------------------------
!> @brief edits a rank-4 parameter of type LOGICAL(SBK)
!> @param this the parameter to edit
!> @param funit the file unit to write to
!> @param indent the additional indentation with which to prefix the parameter
!> @param prefix the additional prefix to prepend to the parameter
!> @param shift the additional spacing to add before the data
!> @param description the description of the parameter
!> @param paddtw logical to indicate if the parameter type should be padded to force alignment;
!>               optional, defaults to false
!>
SUBROUTINE editToText_SBK4(this,funit,indent,prefix,shift,description,paddtw)
  CLASS(Param_SBK4),INTENT(IN) :: this
  INTEGER(SIK),INTENT(IN) :: funit
  INTEGER(SIK),INTENT(IN) :: indent
  TYPE(StringType),INTENT(IN) :: prefix
  INTEGER(SIK),INTENT(IN) :: shift
  CHARACTER(LEN=*),INTENT(IN) :: description
  LOGICAL(SBK),INTENT(IN) :: paddtw
  !
  TYPE(StringType) :: editString
  CALL this%getString('',editString)
  WRITE(UNIT=funit,FMT='(",",a)') editString//' ! '//description

ENDSUBROUTINE editToText_SBK4
!
!-------------------------------------------------------------------------------
!> @brief edits a rank-5 parameter of type LOGICAL(SBK)
!> @param this the parameter to edit
!> @param funit the file unit to write to
!> @param indent the additional indentation with which to prefix the parameter
!> @param prefix the additional prefix to prepend to the parameter
!> @param shift the additional spacing to add before the data
!> @param description the description of the parameter
!> @param paddtw logical to indicate if the parameter type should be padded to force alignment;
!>               optional, defaults to false
!>
SUBROUTINE editToText_SBK5(this,funit,indent,prefix,shift,description,paddtw)
  CLASS(Param_SBK5),INTENT(IN) :: this
  INTEGER(SIK),INTENT(IN) :: funit
  INTEGER(SIK),INTENT(IN) :: indent
  TYPE(StringType),INTENT(IN) :: prefix
  INTEGER(SIK),INTENT(IN) :: shift
  CHARACTER(LEN=*),INTENT(IN) :: description
  LOGICAL(SBK),INTENT(IN) :: paddtw
  !
  TYPE(StringType) :: editString
  CALL this%getString('',editString)
  WRITE(UNIT=funit,FMT='(",",a)') editString//' ! '//description

ENDSUBROUTINE editToText_SBK5
!
!-------------------------------------------------------------------------------
!> @brief edits a rank-6 parameter of type LOGICAL(SBK)
!> @param this the parameter to edit
!> @param funit the file unit to write to
!> @param indent the additional indentation with which to prefix the parameter
!> @param prefix the additional prefix to prepend to the parameter
!> @param shift the additional spacing to add before the data
!> @param description the description of the parameter
!> @param paddtw logical to indicate if the parameter type should be padded to force alignment;
!>               optional, defaults to false
!>
SUBROUTINE editToText_SBK6(this,funit,indent,prefix,shift,description,paddtw)
  CLASS(Param_SBK6),INTENT(IN) :: this
  INTEGER(SIK),INTENT(IN) :: funit
  INTEGER(SIK),INTENT(IN) :: indent
  TYPE(StringType),INTENT(IN) :: prefix
  INTEGER(SIK),INTENT(IN) :: shift
  CHARACTER(LEN=*),INTENT(IN) :: description
  LOGICAL(SBK),INTENT(IN) :: paddtw
  !
  TYPE(StringType) :: editString
  CALL this%getString('',editString)
  WRITE(UNIT=funit,FMT='(",",a)') editString//' ! '//description

ENDSUBROUTINE editToText_SBK6
!
!-------------------------------------------------------------------------------
!> @brief edits a rank-7 parameter of type LOGICAL(SBK)
!> @param this the parameter to edit
!> @param funit the file unit to write to
!> @param indent the additional indentation with which to prefix the parameter
!> @param prefix the additional prefix to prepend to the parameter
!> @param shift the additional spacing to add before the data
!> @param description the description of the parameter
!> @param paddtw logical to indicate if the parameter type should be padded to force alignment;
!>               optional, defaults to false
!>
SUBROUTINE editToText_SBK7(this,funit,indent,prefix,shift,description,paddtw)
  CLASS(Param_SBK7),INTENT(IN) :: this
  INTEGER(SIK),INTENT(IN) :: funit
  INTEGER(SIK),INTENT(IN) :: indent
  TYPE(StringType),INTENT(IN) :: prefix
  INTEGER(SIK),INTENT(IN) :: shift
  CHARACTER(LEN=*),INTENT(IN) :: description
  LOGICAL(SBK),INTENT(IN) :: paddtw
  !
  TYPE(StringType) :: editString
  CALL this%getString('',editString)
  WRITE(UNIT=funit,FMT='(",",a)') editString//' ! '//description

ENDSUBROUTINE editToText_SBK7
!
!-------------------------------------------------------------------------------
!> @brief edits a rank-0 parameter of type INTEGER(SNK)
!> @param this the parameter to edit
!> @param funit the file unit to write to
!> @param indent the additional indentation with which to prefix the parameter
!> @param prefix the additional prefix to prepend to the parameter
!> @param shift the additional spacing to add before the data
!> @param description the description of the parameter
!> @param paddtw logical to indicate if the parameter type should be padded to force alignment;
!>               optional, defaults to false
!>
SUBROUTINE editToText_SNK0(this,funit,indent,prefix,shift,description,paddtw)
  CLASS(Param_SNK0),INTENT(IN) :: this
  INTEGER(SIK),INTENT(IN) :: funit
  INTEGER(SIK),INTENT(IN) :: indent
  TYPE(StringType),INTENT(IN) :: prefix
  INTEGER(SIK),INTENT(IN) :: shift
  CHARACTER(LEN=*),INTENT(IN) :: description
  LOGICAL(SBK),INTENT(IN) :: paddtw
  !
  WRITE(UNIT=funit,FMT='(i12,a)') this%p,' ! '//description

ENDSUBROUTINE editToText_SNK0
!
!-------------------------------------------------------------------------------
!> @brief edits a rank-1 parameter of type INTEGER(SNK)
!> @param this the parameter to edit
!> @param funit the file unit to write to
!> @param indent the additional indentation with which to prefix the parameter
!> @param prefix the additional prefix to prepend to the parameter
!> @param shift the additional spacing to add before the data
!> @param description the description of the parameter
!> @param paddtw logical to indicate if the parameter type should be padded to force alignment;
!>               optional, defaults to false
!>
SUBROUTINE editToText_SNK1(this,funit,indent,prefix,shift,description,paddtw)
  CLASS(Param_SNK1),INTENT(IN) :: this
  INTEGER(SIK),INTENT(IN) :: funit
  INTEGER(SIK),INTENT(IN) :: indent
  TYPE(StringType),INTENT(IN) :: prefix
  INTEGER(SIK),INTENT(IN) :: shift
  CHARACTER(LEN=*),INTENT(IN) :: description
  LOGICAL(SBK),INTENT(IN) :: paddtw
  !
  TYPE(StringType) :: editString
  CALL this%getString('',editString)
  WRITE(UNIT=funit,FMT='(",",a)') editString//' ! '//description

ENDSUBROUTINE editToText_SNK1
!
!-------------------------------------------------------------------------------
!> @brief edits a rank-2 parameter of type INTEGER(SNK)
!> @param this the parameter to edit
!> @param funit the file unit to write to
!> @param indent the additional indentation with which to prefix the parameter
!> @param prefix the additional prefix to prepend to the parameter
!> @param shift the additional spacing to add before the data
!> @param description the description of the parameter
!> @param paddtw logical to indicate if the parameter type should be padded to force alignment;
!>               optional, defaults to false
!>
SUBROUTINE editToText_SNK2(this,funit,indent,prefix,shift,description,paddtw)
  CLASS(Param_SNK2),INTENT(IN) :: this
  INTEGER(SIK),INTENT(IN) :: funit
  INTEGER(SIK),INTENT(IN) :: indent
  TYPE(StringType),INTENT(IN) :: prefix
  INTEGER(SIK),INTENT(IN) :: shift
  CHARACTER(LEN=*),INTENT(IN) :: description
  LOGICAL(SBK),INTENT(IN) :: paddtw
  !
  TYPE(StringType) :: editString
  CALL this%getString('',editString)
  WRITE(UNIT=funit,FMT='(",",a)') editString//' ! '//description

ENDSUBROUTINE editToText_SNK2
!
!-------------------------------------------------------------------------------
!> @brief edits a rank-3 parameter of type INTEGER(SNK)
!> @param this the parameter to edit
!> @param funit the file unit to write to
!> @param indent the additional indentation with which to prefix the parameter
!> @param prefix the additional prefix to prepend to the parameter
!> @param shift the additional spacing to add before the data
!> @param description the description of the parameter
!> @param paddtw logical to indicate if the parameter type should be padded to force alignment;
!>               optional, defaults to false
!>
SUBROUTINE editToText_SNK3(this,funit,indent,prefix,shift,description,paddtw)
  CLASS(Param_SNK3),INTENT(IN) :: this
  INTEGER(SIK),INTENT(IN) :: funit
  INTEGER(SIK),INTENT(IN) :: indent
  TYPE(StringType),INTENT(IN) :: prefix
  INTEGER(SIK),INTENT(IN) :: shift
  CHARACTER(LEN=*),INTENT(IN) :: description
  LOGICAL(SBK),INTENT(IN) :: paddtw
  !
  TYPE(StringType) :: editString
  CALL this%getString('',editString)
  WRITE(UNIT=funit,FMT='(",",a)') editString//' ! '//description

ENDSUBROUTINE editToText_SNK3
!
!-------------------------------------------------------------------------------
!> @brief edits a rank-4 parameter of type INTEGER(SNK)
!> @param this the parameter to edit
!> @param funit the file unit to write to
!> @param indent the additional indentation with which to prefix the parameter
!> @param prefix the additional prefix to prepend to the parameter
!> @param shift the additional spacing to add before the data
!> @param description the description of the parameter
!> @param paddtw logical to indicate if the parameter type should be padded to force alignment;
!>               optional, defaults to false
!>
SUBROUTINE editToText_SNK4(this,funit,indent,prefix,shift,description,paddtw)
  CLASS(Param_SNK4),INTENT(IN) :: this
  INTEGER(SIK),INTENT(IN) :: funit
  INTEGER(SIK),INTENT(IN) :: indent
  TYPE(StringType),INTENT(IN) :: prefix
  INTEGER(SIK),INTENT(IN) :: shift
  CHARACTER(LEN=*),INTENT(IN) :: description
  LOGICAL(SBK),INTENT(IN) :: paddtw
  !
  TYPE(StringType) :: editString
  CALL this%getString('',editString)
  WRITE(UNIT=funit,FMT='(",",a)') editString//' ! '//description

ENDSUBROUTINE editToText_SNK4
!
!-------------------------------------------------------------------------------
!> @brief edits a rank-5 parameter of type INTEGER(SNK)
!> @param this the parameter to edit
!> @param funit the file unit to write to
!> @param indent the additional indentation with which to prefix the parameter
!> @param prefix the additional prefix to prepend to the parameter
!> @param shift the additional spacing to add before the data
!> @param description the description of the parameter
!> @param paddtw logical to indicate if the parameter type should be padded to force alignment;
!>               optional, defaults to false
!>
SUBROUTINE editToText_SNK5(this,funit,indent,prefix,shift,description,paddtw)
  CLASS(Param_SNK5),INTENT(IN) :: this
  INTEGER(SIK),INTENT(IN) :: funit
  INTEGER(SIK),INTENT(IN) :: indent
  TYPE(StringType),INTENT(IN) :: prefix
  INTEGER(SIK),INTENT(IN) :: shift
  CHARACTER(LEN=*),INTENT(IN) :: description
  LOGICAL(SBK),INTENT(IN) :: paddtw
  !
  TYPE(StringType) :: editString
  CALL this%getString('',editString)
  WRITE(UNIT=funit,FMT='(",",a)') editString//' ! '//description

ENDSUBROUTINE editToText_SNK5
!
!-------------------------------------------------------------------------------
!> @brief edits a rank-6 parameter of type INTEGER(SNK)
!> @param this the parameter to edit
!> @param funit the file unit to write to
!> @param indent the additional indentation with which to prefix the parameter
!> @param prefix the additional prefix to prepend to the parameter
!> @param shift the additional spacing to add before the data
!> @param description the description of the parameter
!> @param paddtw logical to indicate if the parameter type should be padded to force alignment;
!>               optional, defaults to false
!>
SUBROUTINE editToText_SNK6(this,funit,indent,prefix,shift,description,paddtw)
  CLASS(Param_SNK6),INTENT(IN) :: this
  INTEGER(SIK),INTENT(IN) :: funit
  INTEGER(SIK),INTENT(IN) :: indent
  TYPE(StringType),INTENT(IN) :: prefix
  INTEGER(SIK),INTENT(IN) :: shift
  CHARACTER(LEN=*),INTENT(IN) :: description
  LOGICAL(SBK),INTENT(IN) :: paddtw
  !
  TYPE(StringType) :: editString
  CALL this%getString('',editString)
  WRITE(UNIT=funit,FMT='(",",a)') editString//' ! '//description

ENDSUBROUTINE editToText_SNK6
!
!-------------------------------------------------------------------------------
!> @brief edits a rank-7 parameter of type INTEGER(SNK)
!> @param this the parameter to edit
!> @param funit the file unit to write to
!> @param indent the additional indentation with which to prefix the parameter
!> @param prefix the additional prefix to prepend to the parameter
!> @param shift the additional spacing to add before the data
!> @param description the description of the parameter
!> @param paddtw logical to indicate if the parameter type should be padded to force alignment;
!>               optional, defaults to false
!>
SUBROUTINE editToText_SNK7(this,funit,indent,prefix,shift,description,paddtw)
  CLASS(Param_SNK7),INTENT(IN) :: this
  INTEGER(SIK),INTENT(IN) :: funit
  INTEGER(SIK),INTENT(IN) :: indent
  TYPE(StringType),INTENT(IN) :: prefix
  INTEGER(SIK),INTENT(IN) :: shift
  CHARACTER(LEN=*),INTENT(IN) :: description
  LOGICAL(SBK),INTENT(IN) :: paddtw
  !
  TYPE(StringType) :: editString
  CALL this%getString('',editString)
  WRITE(UNIT=funit,FMT='(",",a)') editString//' ! '//description

ENDSUBROUTINE editToText_SNK7
!
!-------------------------------------------------------------------------------
!> @brief edits a rank-0 parameter of type INTEGER(SLK)
!> @param this the parameter to edit
!> @param funit the file unit to write to
!> @param indent the additional indentation with which to prefix the parameter
!> @param prefix the additional prefix to prepend to the parameter
!> @param shift the additional spacing to add before the data
!> @param description the description of the parameter
!> @param paddtw logical to indicate if the parameter type should be padded to force alignment;
!>               optional, defaults to false
!>
SUBROUTINE editToText_SLK0(this,funit,indent,prefix,shift,description,paddtw)
  CLASS(Param_SLK0),INTENT(IN) :: this
  INTEGER(SIK),INTENT(IN) :: funit
  INTEGER(SIK),INTENT(IN) :: indent
  TYPE(StringType),INTENT(IN) :: prefix
  INTEGER(SIK),INTENT(IN) :: shift
  CHARACTER(LEN=*),INTENT(IN) :: description
  LOGICAL(SBK),INTENT(IN) :: paddtw
  !
  WRITE(UNIT=funit,FMT='(i12,a)') this%p,' ! '//description

ENDSUBROUTINE editToText_SLK0
!
!-------------------------------------------------------------------------------
!> @brief edits a rank-1 parameter of type INTEGER(SLK)
!> @param this the parameter to edit
!> @param funit the file unit to write to
!> @param indent the additional indentation with which to prefix the parameter
!> @param prefix the additional prefix to prepend to the parameter
!> @param shift the additional spacing to add before the data
!> @param description the description of the parameter
!> @param paddtw logical to indicate if the parameter type should be padded to force alignment;
!>               optional, defaults to false
!>
SUBROUTINE editToText_SLK1(this,funit,indent,prefix,shift,description,paddtw)
  CLASS(Param_SLK1),INTENT(IN) :: this
  INTEGER(SIK),INTENT(IN) :: funit
  INTEGER(SIK),INTENT(IN) :: indent
  TYPE(StringType),INTENT(IN) :: prefix
  INTEGER(SIK),INTENT(IN) :: shift
  CHARACTER(LEN=*),INTENT(IN) :: description
  LOGICAL(SBK),INTENT(IN) :: paddtw
  !
  TYPE(StringType) :: editString
  CALL this%getString('',editString)
  WRITE(UNIT=funit,FMT='(",",a)') editString//' ! '//description

ENDSUBROUTINE editToText_SLK1
!
!-------------------------------------------------------------------------------
!> @brief edits a rank-2 parameter of type INTEGER(SLK)
!> @param this the parameter to edit
!> @param funit the file unit to write to
!> @param indent the additional indentation with which to prefix the parameter
!> @param prefix the additional prefix to prepend to the parameter
!> @param shift the additional spacing to add before the data
!> @param description the description of the parameter
!> @param paddtw logical to indicate if the parameter type should be padded to force alignment;
!>               optional, defaults to false
!>
SUBROUTINE editToText_SLK2(this,funit,indent,prefix,shift,description,paddtw)
  CLASS(Param_SLK2),INTENT(IN) :: this
  INTEGER(SIK),INTENT(IN) :: funit
  INTEGER(SIK),INTENT(IN) :: indent
  TYPE(StringType),INTENT(IN) :: prefix
  INTEGER(SIK),INTENT(IN) :: shift
  CHARACTER(LEN=*),INTENT(IN) :: description
  LOGICAL(SBK),INTENT(IN) :: paddtw
  !
  TYPE(StringType) :: editString
  CALL this%getString('',editString)
  WRITE(UNIT=funit,FMT='(",",a)') editString//' ! '//description

ENDSUBROUTINE editToText_SLK2
!
!-------------------------------------------------------------------------------
!> @brief edits a rank-3 parameter of type INTEGER(SLK)
!> @param this the parameter to edit
!> @param funit the file unit to write to
!> @param indent the additional indentation with which to prefix the parameter
!> @param prefix the additional prefix to prepend to the parameter
!> @param shift the additional spacing to add before the data
!> @param description the description of the parameter
!> @param paddtw logical to indicate if the parameter type should be padded to force alignment;
!>               optional, defaults to false
!>
SUBROUTINE editToText_SLK3(this,funit,indent,prefix,shift,description,paddtw)
  CLASS(Param_SLK3),INTENT(IN) :: this
  INTEGER(SIK),INTENT(IN) :: funit
  INTEGER(SIK),INTENT(IN) :: indent
  TYPE(StringType),INTENT(IN) :: prefix
  INTEGER(SIK),INTENT(IN) :: shift
  CHARACTER(LEN=*),INTENT(IN) :: description
  LOGICAL(SBK),INTENT(IN) :: paddtw
  !
  TYPE(StringType) :: editString
  CALL this%getString('',editString)
  WRITE(UNIT=funit,FMT='(",",a)') editString//' ! '//description

ENDSUBROUTINE editToText_SLK3
!
!-------------------------------------------------------------------------------
!> @brief edits a rank-4 parameter of type INTEGER(SLK)
!> @param this the parameter to edit
!> @param funit the file unit to write to
!> @param indent the additional indentation with which to prefix the parameter
!> @param prefix the additional prefix to prepend to the parameter
!> @param shift the additional spacing to add before the data
!> @param description the description of the parameter
!> @param paddtw logical to indicate if the parameter type should be padded to force alignment;
!>               optional, defaults to false
!>
SUBROUTINE editToText_SLK4(this,funit,indent,prefix,shift,description,paddtw)
  CLASS(Param_SLK4),INTENT(IN) :: this
  INTEGER(SIK),INTENT(IN) :: funit
  INTEGER(SIK),INTENT(IN) :: indent
  TYPE(StringType),INTENT(IN) :: prefix
  INTEGER(SIK),INTENT(IN) :: shift
  CHARACTER(LEN=*),INTENT(IN) :: description
  LOGICAL(SBK),INTENT(IN) :: paddtw
  !
  TYPE(StringType) :: editString
  CALL this%getString('',editString)
  WRITE(UNIT=funit,FMT='(",",a)') editString//' ! '//description

ENDSUBROUTINE editToText_SLK4
!
!-------------------------------------------------------------------------------
!> @brief edits a rank-5 parameter of type INTEGER(SLK)
!> @param this the parameter to edit
!> @param funit the file unit to write to
!> @param indent the additional indentation with which to prefix the parameter
!> @param prefix the additional prefix to prepend to the parameter
!> @param shift the additional spacing to add before the data
!> @param description the description of the parameter
!> @param paddtw logical to indicate if the parameter type should be padded to force alignment;
!>               optional, defaults to false
!>
SUBROUTINE editToText_SLK5(this,funit,indent,prefix,shift,description,paddtw)
  CLASS(Param_SLK5),INTENT(IN) :: this
  INTEGER(SIK),INTENT(IN) :: funit
  INTEGER(SIK),INTENT(IN) :: indent
  TYPE(StringType),INTENT(IN) :: prefix
  INTEGER(SIK),INTENT(IN) :: shift
  CHARACTER(LEN=*),INTENT(IN) :: description
  LOGICAL(SBK),INTENT(IN) :: paddtw
  !
  TYPE(StringType) :: editString
  CALL this%getString('',editString)
  WRITE(UNIT=funit,FMT='(",",a)') editString//' ! '//description

ENDSUBROUTINE editToText_SLK5
!
!-------------------------------------------------------------------------------
!> @brief edits a rank-6 parameter of type INTEGER(SLK)
!> @param this the parameter to edit
!> @param funit the file unit to write to
!> @param indent the additional indentation with which to prefix the parameter
!> @param prefix the additional prefix to prepend to the parameter
!> @param shift the additional spacing to add before the data
!> @param description the description of the parameter
!> @param paddtw logical to indicate if the parameter type should be padded to force alignment;
!>               optional, defaults to false
!>
SUBROUTINE editToText_SLK6(this,funit,indent,prefix,shift,description,paddtw)
  CLASS(Param_SLK6),INTENT(IN) :: this
  INTEGER(SIK),INTENT(IN) :: funit
  INTEGER(SIK),INTENT(IN) :: indent
  TYPE(StringType),INTENT(IN) :: prefix
  INTEGER(SIK),INTENT(IN) :: shift
  CHARACTER(LEN=*),INTENT(IN) :: description
  LOGICAL(SBK),INTENT(IN) :: paddtw
  !
  TYPE(StringType) :: editString
  CALL this%getString('',editString)
  WRITE(UNIT=funit,FMT='(",",a)') editString//' ! '//description

ENDSUBROUTINE editToText_SLK6
!
!-------------------------------------------------------------------------------
!> @brief edits a rank-7 parameter of type INTEGER(SLK)
!> @param this the parameter to edit
!> @param funit the file unit to write to
!> @param indent the additional indentation with which to prefix the parameter
!> @param prefix the additional prefix to prepend to the parameter
!> @param shift the additional spacing to add before the data
!> @param description the description of the parameter
!> @param paddtw logical to indicate if the parameter type should be padded to force alignment;
!>               optional, defaults to false
!>
SUBROUTINE editToText_SLK7(this,funit,indent,prefix,shift,description,paddtw)
  CLASS(Param_SLK7),INTENT(IN) :: this
  INTEGER(SIK),INTENT(IN) :: funit
  INTEGER(SIK),INTENT(IN) :: indent
  TYPE(StringType),INTENT(IN) :: prefix
  INTEGER(SIK),INTENT(IN) :: shift
  CHARACTER(LEN=*),INTENT(IN) :: description
  LOGICAL(SBK),INTENT(IN) :: paddtw
  !
  TYPE(StringType) :: editString
  CALL this%getString('',editString)
  WRITE(UNIT=funit,FMT='(",",a)') editString//' ! '//description

ENDSUBROUTINE editToText_SLK7
!
!-------------------------------------------------------------------------------
!> @brief edits a rank-0 parameter of type REAL(SSK)
!> @param this the parameter to edit
!> @param funit the file unit to write to
!> @param indent the additional indentation with which to prefix the parameter
!> @param prefix the additional prefix to prepend to the parameter
!> @param shift the additional spacing to add before the data
!> @param description the description of the parameter
!> @param paddtw logical to indicate if the parameter type should be padded to force alignment;
!>               optional, defaults to false
!>
SUBROUTINE editToText_SSK0(this,funit,indent,prefix,shift,description,paddtw)
  CLASS(Param_SSK0),INTENT(IN) :: this
  INTEGER(SIK),INTENT(IN) :: funit
  INTEGER(SIK),INTENT(IN) :: indent
  TYPE(StringType),INTENT(IN) :: prefix
  INTEGER(SIK),INTENT(IN) :: shift
  CHARACTER(LEN=*),INTENT(IN) :: description
  LOGICAL(SBK),INTENT(IN) :: paddtw
  !
  WRITE(UNIT=funit,FMT='(g20.14,a)') this%p,' ! '//description

ENDSUBROUTINE editToText_SSK0
!
!-------------------------------------------------------------------------------
!> @brief edits a rank-1 parameter of type REAL(SSK)
!> @param this the parameter to edit
!> @param funit the file unit to write to
!> @param indent the additional indentation with which to prefix the parameter
!> @param prefix the additional prefix to prepend to the parameter
!> @param shift the additional spacing to add before the data
!> @param description the description of the parameter
!> @param paddtw logical to indicate if the parameter type should be padded to force alignment;
!>               optional, defaults to false
!>
SUBROUTINE editToText_SSK1(this,funit,indent,prefix,shift,description,paddtw)
  CLASS(Param_SSK1),INTENT(IN) :: this
  INTEGER(SIK),INTENT(IN) :: funit
  INTEGER(SIK),INTENT(IN) :: indent
  TYPE(StringType),INTENT(IN) :: prefix
  INTEGER(SIK),INTENT(IN) :: shift
  CHARACTER(LEN=*),INTENT(IN) :: description
  LOGICAL(SBK),INTENT(IN) :: paddtw
  !
  TYPE(StringType) :: editString
  CALL this%getString('',editString)
  WRITE(UNIT=funit,FMT='(",",a)') editString//' ! '//description

ENDSUBROUTINE editToText_SSK1
!
!-------------------------------------------------------------------------------
!> @brief edits a rank-2 parameter of type REAL(SSK)
!> @param this the parameter to edit
!> @param funit the file unit to write to
!> @param indent the additional indentation with which to prefix the parameter
!> @param prefix the additional prefix to prepend to the parameter
!> @param shift the additional spacing to add before the data
!> @param description the description of the parameter
!> @param paddtw logical to indicate if the parameter type should be padded to force alignment;
!>               optional, defaults to false
!>
SUBROUTINE editToText_SSK2(this,funit,indent,prefix,shift,description,paddtw)
  CLASS(Param_SSK2),INTENT(IN) :: this
  INTEGER(SIK),INTENT(IN) :: funit
  INTEGER(SIK),INTENT(IN) :: indent
  TYPE(StringType),INTENT(IN) :: prefix
  INTEGER(SIK),INTENT(IN) :: shift
  CHARACTER(LEN=*),INTENT(IN) :: description
  LOGICAL(SBK),INTENT(IN) :: paddtw
  !
  TYPE(StringType) :: editString
  CALL this%getString('',editString)
  WRITE(UNIT=funit,FMT='(",",a)') editString//' ! '//description

ENDSUBROUTINE editToText_SSK2
!
!-------------------------------------------------------------------------------
!> @brief edits a rank-3 parameter of type REAL(SSK)
!> @param this the parameter to edit
!> @param funit the file unit to write to
!> @param indent the additional indentation with which to prefix the parameter
!> @param prefix the additional prefix to prepend to the parameter
!> @param shift the additional spacing to add before the data
!> @param description the description of the parameter
!> @param paddtw logical to indicate if the parameter type should be padded to force alignment;
!>               optional, defaults to false
!>
SUBROUTINE editToText_SSK3(this,funit,indent,prefix,shift,description,paddtw)
  CLASS(Param_SSK3),INTENT(IN) :: this
  INTEGER(SIK),INTENT(IN) :: funit
  INTEGER(SIK),INTENT(IN) :: indent
  TYPE(StringType),INTENT(IN) :: prefix
  INTEGER(SIK),INTENT(IN) :: shift
  CHARACTER(LEN=*),INTENT(IN) :: description
  LOGICAL(SBK),INTENT(IN) :: paddtw
  !
  TYPE(StringType) :: editString
  CALL this%getString('',editString)
  WRITE(UNIT=funit,FMT='(",",a)') editString//' ! '//description

ENDSUBROUTINE editToText_SSK3
!
!-------------------------------------------------------------------------------
!> @brief edits a rank-4 parameter of type REAL(SSK)
!> @param this the parameter to edit
!> @param funit the file unit to write to
!> @param indent the additional indentation with which to prefix the parameter
!> @param prefix the additional prefix to prepend to the parameter
!> @param shift the additional spacing to add before the data
!> @param description the description of the parameter
!> @param paddtw logical to indicate if the parameter type should be padded to force alignment;
!>               optional, defaults to false
!>
SUBROUTINE editToText_SSK4(this,funit,indent,prefix,shift,description,paddtw)
  CLASS(Param_SSK4),INTENT(IN) :: this
  INTEGER(SIK),INTENT(IN) :: funit
  INTEGER(SIK),INTENT(IN) :: indent
  TYPE(StringType),INTENT(IN) :: prefix
  INTEGER(SIK),INTENT(IN) :: shift
  CHARACTER(LEN=*),INTENT(IN) :: description
  LOGICAL(SBK),INTENT(IN) :: paddtw
  !
  TYPE(StringType) :: editString
  CALL this%getString('',editString)
  WRITE(UNIT=funit,FMT='(",",a)') editString//' ! '//description

ENDSUBROUTINE editToText_SSK4
!
!-------------------------------------------------------------------------------
!> @brief edits a rank-5 parameter of type REAL(SSK)
!> @param this the parameter to edit
!> @param funit the file unit to write to
!> @param indent the additional indentation with which to prefix the parameter
!> @param prefix the additional prefix to prepend to the parameter
!> @param shift the additional spacing to add before the data
!> @param description the description of the parameter
!> @param paddtw logical to indicate if the parameter type should be padded to force alignment;
!>               optional, defaults to false
!>
SUBROUTINE editToText_SSK5(this,funit,indent,prefix,shift,description,paddtw)
  CLASS(Param_SSK5),INTENT(IN) :: this
  INTEGER(SIK),INTENT(IN) :: funit
  INTEGER(SIK),INTENT(IN) :: indent
  TYPE(StringType),INTENT(IN) :: prefix
  INTEGER(SIK),INTENT(IN) :: shift
  CHARACTER(LEN=*),INTENT(IN) :: description
  LOGICAL(SBK),INTENT(IN) :: paddtw
  !
  TYPE(StringType) :: editString
  CALL this%getString('',editString)
  WRITE(UNIT=funit,FMT='(",",a)') editString//' ! '//description

ENDSUBROUTINE editToText_SSK5
!
!-------------------------------------------------------------------------------
!> @brief edits a rank-6 parameter of type REAL(SSK)
!> @param this the parameter to edit
!> @param funit the file unit to write to
!> @param indent the additional indentation with which to prefix the parameter
!> @param prefix the additional prefix to prepend to the parameter
!> @param shift the additional spacing to add before the data
!> @param description the description of the parameter
!> @param paddtw logical to indicate if the parameter type should be padded to force alignment;
!>               optional, defaults to false
!>
SUBROUTINE editToText_SSK6(this,funit,indent,prefix,shift,description,paddtw)
  CLASS(Param_SSK6),INTENT(IN) :: this
  INTEGER(SIK),INTENT(IN) :: funit
  INTEGER(SIK),INTENT(IN) :: indent
  TYPE(StringType),INTENT(IN) :: prefix
  INTEGER(SIK),INTENT(IN) :: shift
  CHARACTER(LEN=*),INTENT(IN) :: description
  LOGICAL(SBK),INTENT(IN) :: paddtw
  !
  TYPE(StringType) :: editString
  CALL this%getString('',editString)
  WRITE(UNIT=funit,FMT='(",",a)') editString//' ! '//description

ENDSUBROUTINE editToText_SSK6
!
!-------------------------------------------------------------------------------
!> @brief edits a rank-7 parameter of type REAL(SSK)
!> @param this the parameter to edit
!> @param funit the file unit to write to
!> @param indent the additional indentation with which to prefix the parameter
!> @param prefix the additional prefix to prepend to the parameter
!> @param shift the additional spacing to add before the data
!> @param description the description of the parameter
!> @param paddtw logical to indicate if the parameter type should be padded to force alignment;
!>               optional, defaults to false
!>
SUBROUTINE editToText_SSK7(this,funit,indent,prefix,shift,description,paddtw)
  CLASS(Param_SSK7),INTENT(IN) :: this
  INTEGER(SIK),INTENT(IN) :: funit
  INTEGER(SIK),INTENT(IN) :: indent
  TYPE(StringType),INTENT(IN) :: prefix
  INTEGER(SIK),INTENT(IN) :: shift
  CHARACTER(LEN=*),INTENT(IN) :: description
  LOGICAL(SBK),INTENT(IN) :: paddtw
  !
  TYPE(StringType) :: editString
  CALL this%getString('',editString)
  WRITE(UNIT=funit,FMT='(",",a)') editString//' ! '//description

ENDSUBROUTINE editToText_SSK7
!
!-------------------------------------------------------------------------------
!> @brief edits a rank-0 parameter of type REAL(SDK)
!> @param this the parameter to edit
!> @param funit the file unit to write to
!> @param indent the additional indentation with which to prefix the parameter
!> @param prefix the additional prefix to prepend to the parameter
!> @param shift the additional spacing to add before the data
!> @param description the description of the parameter
!> @param paddtw logical to indicate if the parameter type should be padded to force alignment;
!>               optional, defaults to false
!>
SUBROUTINE editToText_SDK0(this,funit,indent,prefix,shift,description,paddtw)
  CLASS(Param_SDK0),INTENT(IN) :: this
  INTEGER(SIK),INTENT(IN) :: funit
  INTEGER(SIK),INTENT(IN) :: indent
  TYPE(StringType),INTENT(IN) :: prefix
  INTEGER(SIK),INTENT(IN) :: shift
  CHARACTER(LEN=*),INTENT(IN) :: description
  LOGICAL(SBK),INTENT(IN) :: paddtw
  !
  WRITE(UNIT=funit,FMT='(g20.14,a)') this%p,' ! '//description

ENDSUBROUTINE editToText_SDK0
!
!-------------------------------------------------------------------------------
!> @brief edits a rank-1 parameter of type REAL(SDK)
!> @param this the parameter to edit
!> @param funit the file unit to write to
!> @param indent the additional indentation with which to prefix the parameter
!> @param prefix the additional prefix to prepend to the parameter
!> @param shift the additional spacing to add before the data
!> @param description the description of the parameter
!> @param paddtw logical to indicate if the parameter type should be padded to force alignment;
!>               optional, defaults to false
!>
SUBROUTINE editToText_SDK1(this,funit,indent,prefix,shift,description,paddtw)
  CLASS(Param_SDK1),INTENT(IN) :: this
  INTEGER(SIK),INTENT(IN) :: funit
  INTEGER(SIK),INTENT(IN) :: indent
  TYPE(StringType),INTENT(IN) :: prefix
  INTEGER(SIK),INTENT(IN) :: shift
  CHARACTER(LEN=*),INTENT(IN) :: description
  LOGICAL(SBK),INTENT(IN) :: paddtw
  !
  TYPE(StringType) :: editString
  CALL this%getString('',editString)
  WRITE(UNIT=funit,FMT='(",",a)') editString//' ! '//description

ENDSUBROUTINE editToText_SDK1
!
!-------------------------------------------------------------------------------
!> @brief edits a rank-2 parameter of type REAL(SDK)
!> @param this the parameter to edit
!> @param funit the file unit to write to
!> @param indent the additional indentation with which to prefix the parameter
!> @param prefix the additional prefix to prepend to the parameter
!> @param shift the additional spacing to add before the data
!> @param description the description of the parameter
!> @param paddtw logical to indicate if the parameter type should be padded to force alignment;
!>               optional, defaults to false
!>
SUBROUTINE editToText_SDK2(this,funit,indent,prefix,shift,description,paddtw)
  CLASS(Param_SDK2),INTENT(IN) :: this
  INTEGER(SIK),INTENT(IN) :: funit
  INTEGER(SIK),INTENT(IN) :: indent
  TYPE(StringType),INTENT(IN) :: prefix
  INTEGER(SIK),INTENT(IN) :: shift
  CHARACTER(LEN=*),INTENT(IN) :: description
  LOGICAL(SBK),INTENT(IN) :: paddtw
  !
  TYPE(StringType) :: editString
  CALL this%getString('',editString)
  WRITE(UNIT=funit,FMT='(",",a)') editString//' ! '//description

ENDSUBROUTINE editToText_SDK2
!
!-------------------------------------------------------------------------------
!> @brief edits a rank-3 parameter of type REAL(SDK)
!> @param this the parameter to edit
!> @param funit the file unit to write to
!> @param indent the additional indentation with which to prefix the parameter
!> @param prefix the additional prefix to prepend to the parameter
!> @param shift the additional spacing to add before the data
!> @param description the description of the parameter
!> @param paddtw logical to indicate if the parameter type should be padded to force alignment;
!>               optional, defaults to false
!>
SUBROUTINE editToText_SDK3(this,funit,indent,prefix,shift,description,paddtw)
  CLASS(Param_SDK3),INTENT(IN) :: this
  INTEGER(SIK),INTENT(IN) :: funit
  INTEGER(SIK),INTENT(IN) :: indent
  TYPE(StringType),INTENT(IN) :: prefix
  INTEGER(SIK),INTENT(IN) :: shift
  CHARACTER(LEN=*),INTENT(IN) :: description
  LOGICAL(SBK),INTENT(IN) :: paddtw
  !
  TYPE(StringType) :: editString
  CALL this%getString('',editString)
  WRITE(UNIT=funit,FMT='(",",a)') editString//' ! '//description

ENDSUBROUTINE editToText_SDK3
!
!-------------------------------------------------------------------------------
!> @brief edits a rank-4 parameter of type REAL(SDK)
!> @param this the parameter to edit
!> @param funit the file unit to write to
!> @param indent the additional indentation with which to prefix the parameter
!> @param prefix the additional prefix to prepend to the parameter
!> @param shift the additional spacing to add before the data
!> @param description the description of the parameter
!> @param paddtw logical to indicate if the parameter type should be padded to force alignment;
!>               optional, defaults to false
!>
SUBROUTINE editToText_SDK4(this,funit,indent,prefix,shift,description,paddtw)
  CLASS(Param_SDK4),INTENT(IN) :: this
  INTEGER(SIK),INTENT(IN) :: funit
  INTEGER(SIK),INTENT(IN) :: indent
  TYPE(StringType),INTENT(IN) :: prefix
  INTEGER(SIK),INTENT(IN) :: shift
  CHARACTER(LEN=*),INTENT(IN) :: description
  LOGICAL(SBK),INTENT(IN) :: paddtw
  !
  TYPE(StringType) :: editString
  CALL this%getString('',editString)
  WRITE(UNIT=funit,FMT='(",",a)') editString//' ! '//description

ENDSUBROUTINE editToText_SDK4
!
!-------------------------------------------------------------------------------
!> @brief edits a rank-5 parameter of type REAL(SDK)
!> @param this the parameter to edit
!> @param funit the file unit to write to
!> @param indent the additional indentation with which to prefix the parameter
!> @param prefix the additional prefix to prepend to the parameter
!> @param shift the additional spacing to add before the data
!> @param description the description of the parameter
!> @param paddtw logical to indicate if the parameter type should be padded to force alignment;
!>               optional, defaults to false
!>
SUBROUTINE editToText_SDK5(this,funit,indent,prefix,shift,description,paddtw)
  CLASS(Param_SDK5),INTENT(IN) :: this
  INTEGER(SIK),INTENT(IN) :: funit
  INTEGER(SIK),INTENT(IN) :: indent
  TYPE(StringType),INTENT(IN) :: prefix
  INTEGER(SIK),INTENT(IN) :: shift
  CHARACTER(LEN=*),INTENT(IN) :: description
  LOGICAL(SBK),INTENT(IN) :: paddtw
  !
  TYPE(StringType) :: editString
  CALL this%getString('',editString)
  WRITE(UNIT=funit,FMT='(",",a)') editString//' ! '//description

ENDSUBROUTINE editToText_SDK5
!
!-------------------------------------------------------------------------------
!> @brief edits a rank-6 parameter of type REAL(SDK)
!> @param this the parameter to edit
!> @param funit the file unit to write to
!> @param indent the additional indentation with which to prefix the parameter
!> @param prefix the additional prefix to prepend to the parameter
!> @param shift the additional spacing to add before the data
!> @param description the description of the parameter
!> @param paddtw logical to indicate if the parameter type should be padded to force alignment;
!>               optional, defaults to false
!>
SUBROUTINE editToText_SDK6(this,funit,indent,prefix,shift,description,paddtw)
  CLASS(Param_SDK6),INTENT(IN) :: this
  INTEGER(SIK),INTENT(IN) :: funit
  INTEGER(SIK),INTENT(IN) :: indent
  TYPE(StringType),INTENT(IN) :: prefix
  INTEGER(SIK),INTENT(IN) :: shift
  CHARACTER(LEN=*),INTENT(IN) :: description
  LOGICAL(SBK),INTENT(IN) :: paddtw
  !
  TYPE(StringType) :: editString
  CALL this%getString('',editString)
  WRITE(UNIT=funit,FMT='(",",a)') editString//' ! '//description

ENDSUBROUTINE editToText_SDK6
!
!-------------------------------------------------------------------------------
!> @brief edits a rank-7 parameter of type REAL(SDK)
!> @param this the parameter to edit
!> @param funit the file unit to write to
!> @param indent the additional indentation with which to prefix the parameter
!> @param prefix the additional prefix to prepend to the parameter
!> @param shift the additional spacing to add before the data
!> @param description the description of the parameter
!> @param paddtw logical to indicate if the parameter type should be padded to force alignment;
!>               optional, defaults to false
!>
SUBROUTINE editToText_SDK7(this,funit,indent,prefix,shift,description,paddtw)
  CLASS(Param_SDK7),INTENT(IN) :: this
  INTEGER(SIK),INTENT(IN) :: funit
  INTEGER(SIK),INTENT(IN) :: indent
  TYPE(StringType),INTENT(IN) :: prefix
  INTEGER(SIK),INTENT(IN) :: shift
  CHARACTER(LEN=*),INTENT(IN) :: description
  LOGICAL(SBK),INTENT(IN) :: paddtw
  !
  TYPE(StringType) :: editString
  CALL this%getString('',editString)
  WRITE(UNIT=funit,FMT='(",",a)') editString//' ! '//description

ENDSUBROUTINE editToText_SDK7
!
!-------------------------------------------------------------------------------
!> @brief edits a rank-0 parameter of type TYPE(StringType)
!> @param this the parameter to edit
!> @param funit the file unit to write to
!> @param indent the additional indentation with which to prefix the parameter
!> @param prefix the additional prefix to prepend to the parameter
!> @param shift the additional spacing to add before the data
!> @param description the description of the parameter
!> @param paddtw logical to indicate if the parameter type should be padded to force alignment;
!>               optional, defaults to false
!>
SUBROUTINE editToText_STR0(this,funit,indent,prefix,shift,description,paddtw)
  CLASS(Param_STR0),INTENT(IN) :: this
  INTEGER(SIK),INTENT(IN) :: funit
  INTEGER(SIK),INTENT(IN) :: indent
  TYPE(StringType),INTENT(IN) :: prefix
  INTEGER(SIK),INTENT(IN) :: shift
  CHARACTER(LEN=*),INTENT(IN) :: description
  LOGICAL(SBK),INTENT(IN) :: paddtw
  !
  WRITE(UNIT=funit,FMT='(a,a)') CHAR(this%p),' ! '//description

ENDSUBROUTINE editToText_STR0
!
!-------------------------------------------------------------------------------
!> @brief edits a rank-1 parameter of type TYPE(StringType)
!> @param this the parameter to edit
!> @param funit the file unit to write to
!> @param indent the additional indentation with which to prefix the parameter
!> @param prefix the additional prefix to prepend to the parameter
!> @param shift the additional spacing to add before the data
!> @param description the description of the parameter
!> @param paddtw logical to indicate if the parameter type should be padded to force alignment;
!>               optional, defaults to false
!>
SUBROUTINE editToText_STR1(this,funit,indent,prefix,shift,description,paddtw)
  CLASS(Param_STR1),INTENT(IN) :: this
  INTEGER(SIK),INTENT(IN) :: funit
  INTEGER(SIK),INTENT(IN) :: indent
  TYPE(StringType),INTENT(IN) :: prefix
  INTEGER(SIK),INTENT(IN) :: shift
  CHARACTER(LEN=*),INTENT(IN) :: description
  LOGICAL(SBK),INTENT(IN) :: paddtw
  !
  TYPE(StringType) :: editString
  CALL this%getString('',editString)
  WRITE(UNIT=funit,FMT='(",",a)') editString//' ! '//description

ENDSUBROUTINE editToText_STR1
!
!-------------------------------------------------------------------------------
!> @brief edits a rank-2 parameter of type TYPE(StringType)
!> @param this the parameter to edit
!> @param funit the file unit to write to
!> @param indent the additional indentation with which to prefix the parameter
!> @param prefix the additional prefix to prepend to the parameter
!> @param shift the additional spacing to add before the data
!> @param description the description of the parameter
!> @param paddtw logical to indicate if the parameter type should be padded to force alignment;
!>               optional, defaults to false
!>
SUBROUTINE editToText_STR2(this,funit,indent,prefix,shift,description,paddtw)
  CLASS(Param_STR2),INTENT(IN) :: this
  INTEGER(SIK),INTENT(IN) :: funit
  INTEGER(SIK),INTENT(IN) :: indent
  TYPE(StringType),INTENT(IN) :: prefix
  INTEGER(SIK),INTENT(IN) :: shift
  CHARACTER(LEN=*),INTENT(IN) :: description
  LOGICAL(SBK),INTENT(IN) :: paddtw
  !
  TYPE(StringType) :: editString
  CALL this%getString('',editString)
  WRITE(UNIT=funit,FMT='(",",a)') editString//' ! '//description

ENDSUBROUTINE editToText_STR2
!
!-------------------------------------------------------------------------------
!> @brief edits a rank-3 parameter of type TYPE(StringType)
!> @param this the parameter to edit
!> @param funit the file unit to write to
!> @param indent the additional indentation with which to prefix the parameter
!> @param prefix the additional prefix to prepend to the parameter
!> @param shift the additional spacing to add before the data
!> @param description the description of the parameter
!> @param paddtw logical to indicate if the parameter type should be padded to force alignment;
!>               optional, defaults to false
!>
SUBROUTINE editToText_STR3(this,funit,indent,prefix,shift,description,paddtw)
  CLASS(Param_STR3),INTENT(IN) :: this
  INTEGER(SIK),INTENT(IN) :: funit
  INTEGER(SIK),INTENT(IN) :: indent
  TYPE(StringType),INTENT(IN) :: prefix
  INTEGER(SIK),INTENT(IN) :: shift
  CHARACTER(LEN=*),INTENT(IN) :: description
  LOGICAL(SBK),INTENT(IN) :: paddtw
  !
  TYPE(StringType) :: editString
  CALL this%getString('',editString)
  WRITE(UNIT=funit,FMT='(",",a)') editString//' ! '//description

ENDSUBROUTINE editToText_STR3
!
!-------------------------------------------------------------------------------
!> @brief edits a rank-4 parameter of type TYPE(StringType)
!> @param this the parameter to edit
!> @param funit the file unit to write to
!> @param indent the additional indentation with which to prefix the parameter
!> @param prefix the additional prefix to prepend to the parameter
!> @param shift the additional spacing to add before the data
!> @param description the description of the parameter
!> @param paddtw logical to indicate if the parameter type should be padded to force alignment;
!>               optional, defaults to false
!>
SUBROUTINE editToText_STR4(this,funit,indent,prefix,shift,description,paddtw)
  CLASS(Param_STR4),INTENT(IN) :: this
  INTEGER(SIK),INTENT(IN) :: funit
  INTEGER(SIK),INTENT(IN) :: indent
  TYPE(StringType),INTENT(IN) :: prefix
  INTEGER(SIK),INTENT(IN) :: shift
  CHARACTER(LEN=*),INTENT(IN) :: description
  LOGICAL(SBK),INTENT(IN) :: paddtw
  !
  TYPE(StringType) :: editString
  CALL this%getString('',editString)
  WRITE(UNIT=funit,FMT='(",",a)') editString//' ! '//description

ENDSUBROUTINE editToText_STR4
!
!-------------------------------------------------------------------------------
!> @brief edits a rank-5 parameter of type TYPE(StringType)
!> @param this the parameter to edit
!> @param funit the file unit to write to
!> @param indent the additional indentation with which to prefix the parameter
!> @param prefix the additional prefix to prepend to the parameter
!> @param shift the additional spacing to add before the data
!> @param description the description of the parameter
!> @param paddtw logical to indicate if the parameter type should be padded to force alignment;
!>               optional, defaults to false
!>
SUBROUTINE editToText_STR5(this,funit,indent,prefix,shift,description,paddtw)
  CLASS(Param_STR5),INTENT(IN) :: this
  INTEGER(SIK),INTENT(IN) :: funit
  INTEGER(SIK),INTENT(IN) :: indent
  TYPE(StringType),INTENT(IN) :: prefix
  INTEGER(SIK),INTENT(IN) :: shift
  CHARACTER(LEN=*),INTENT(IN) :: description
  LOGICAL(SBK),INTENT(IN) :: paddtw
  !
  TYPE(StringType) :: editString
  CALL this%getString('',editString)
  WRITE(UNIT=funit,FMT='(",",a)') editString//' ! '//description

ENDSUBROUTINE editToText_STR5
!
!-------------------------------------------------------------------------------
!> @brief edits a rank-6 parameter of type TYPE(StringType)
!> @param this the parameter to edit
!> @param funit the file unit to write to
!> @param indent the additional indentation with which to prefix the parameter
!> @param prefix the additional prefix to prepend to the parameter
!> @param shift the additional spacing to add before the data
!> @param description the description of the parameter
!> @param paddtw logical to indicate if the parameter type should be padded to force alignment;
!>               optional, defaults to false
!>
SUBROUTINE editToText_STR6(this,funit,indent,prefix,shift,description,paddtw)
  CLASS(Param_STR6),INTENT(IN) :: this
  INTEGER(SIK),INTENT(IN) :: funit
  INTEGER(SIK),INTENT(IN) :: indent
  TYPE(StringType),INTENT(IN) :: prefix
  INTEGER(SIK),INTENT(IN) :: shift
  CHARACTER(LEN=*),INTENT(IN) :: description
  LOGICAL(SBK),INTENT(IN) :: paddtw
  !
  TYPE(StringType) :: editString
  CALL this%getString('',editString)
  WRITE(UNIT=funit,FMT='(",",a)') editString//' ! '//description

ENDSUBROUTINE editToText_STR6
!
!-------------------------------------------------------------------------------
!> @brief edits a rank-7 parameter of type TYPE(StringType)
!> @param this the parameter to edit
!> @param funit the file unit to write to
!> @param indent the additional indentation with which to prefix the parameter
!> @param prefix the additional prefix to prepend to the parameter
!> @param shift the additional spacing to add before the data
!> @param description the description of the parameter
!> @param paddtw logical to indicate if the parameter type should be padded to force alignment;
!>               optional, defaults to false
!>
SUBROUTINE editToText_STR7(this,funit,indent,prefix,shift,description,paddtw)
  CLASS(Param_STR7),INTENT(IN) :: this
  INTEGER(SIK),INTENT(IN) :: funit
  INTEGER(SIK),INTENT(IN) :: indent
  TYPE(StringType),INTENT(IN) :: prefix
  INTEGER(SIK),INTENT(IN) :: shift
  CHARACTER(LEN=*),INTENT(IN) :: description
  LOGICAL(SBK),INTENT(IN) :: paddtw
  !
  TYPE(StringType) :: editString
  CALL this%getString('',editString)
  WRITE(UNIT=funit,FMT='(",",a)') editString//' ! '//description

ENDSUBROUTINE editToText_STR7
!
!-------------------------------------------------------------------------------
!> @brief Writes a rank-0parameter of type LOGICAL(SBK) to an HDF5 file
!> @param this the parameter to write
!> @param file the HDF5 file type
!> @param path the location to write the parameter to
!>
SUBROUTINE editToH5_SBK0(this,file,path)
  CLASS(Param_SBK0),INTENT(IN) :: this
  CLASS(HDF5FileType),INTENT(INOUT) :: file
  CHARACTER(LEN=*),INTENT(IN) :: path
  !

  REQUIRE(file%isInit)
  REQUIRE(file%isOpen())

  CALL file%fwrite(path,this%p)

ENDSUBROUTINE editToH5_SBK0
!
!-------------------------------------------------------------------------------
!> @brief Writes a rank-1parameter of type LOGICAL(SBK) to an HDF5 file
!> @param this the parameter to write
!> @param file the HDF5 file type
!> @param path the location to write the parameter to
!>
SUBROUTINE editToH5_SBK1(this,file,path)
  CLASS(Param_SBK1),INTENT(IN) :: this
  CLASS(HDF5FileType),INTENT(INOUT) :: file
  CHARACTER(LEN=*),INTENT(IN) :: path
  !

  REQUIRE(file%isInit)
  REQUIRE(file%isOpen())

  CALL file%fwrite(path,this%p)

ENDSUBROUTINE editToH5_SBK1
!
!-------------------------------------------------------------------------------
!> @brief Writes a rank-2parameter of type LOGICAL(SBK) to an HDF5 file
!> @param this the parameter to write
!> @param file the HDF5 file type
!> @param path the location to write the parameter to
!>
SUBROUTINE editToH5_SBK2(this,file,path)
  CLASS(Param_SBK2),INTENT(IN) :: this
  CLASS(HDF5FileType),INTENT(INOUT) :: file
  CHARACTER(LEN=*),INTENT(IN) :: path
  !

  REQUIRE(file%isInit)
  REQUIRE(file%isOpen())

  CALL file%fwrite(path,this%p)

ENDSUBROUTINE editToH5_SBK2
!
!-------------------------------------------------------------------------------
!> @brief Writes a rank-3parameter of type LOGICAL(SBK) to an HDF5 file
!> @param this the parameter to write
!> @param file the HDF5 file type
!> @param path the location to write the parameter to
!>
SUBROUTINE editToH5_SBK3(this,file,path)
  CLASS(Param_SBK3),INTENT(IN) :: this
  CLASS(HDF5FileType),INTENT(INOUT) :: file
  CHARACTER(LEN=*),INTENT(IN) :: path
  !

  REQUIRE(file%isInit)
  REQUIRE(file%isOpen())

  CALL file%fwrite(path,this%p)

ENDSUBROUTINE editToH5_SBK3
!
!-------------------------------------------------------------------------------
!> @brief Writes a rank-0parameter of type INTEGER(SNK) to an HDF5 file
!> @param this the parameter to write
!> @param file the HDF5 file type
!> @param path the location to write the parameter to
!>
SUBROUTINE editToH5_SNK0(this,file,path)
  CLASS(Param_SNK0),INTENT(IN) :: this
  CLASS(HDF5FileType),INTENT(INOUT) :: file
  CHARACTER(LEN=*),INTENT(IN) :: path
  !

  REQUIRE(file%isInit)
  REQUIRE(file%isOpen())

  CALL file%fwrite(path,this%p)

ENDSUBROUTINE editToH5_SNK0
!
!-------------------------------------------------------------------------------
!> @brief Writes a rank-1parameter of type INTEGER(SNK) to an HDF5 file
!> @param this the parameter to write
!> @param file the HDF5 file type
!> @param path the location to write the parameter to
!>
SUBROUTINE editToH5_SNK1(this,file,path)
  CLASS(Param_SNK1),INTENT(IN) :: this
  CLASS(HDF5FileType),INTENT(INOUT) :: file
  CHARACTER(LEN=*),INTENT(IN) :: path
  !

  REQUIRE(file%isInit)
  REQUIRE(file%isOpen())

  CALL file%fwrite(path,this%p)

ENDSUBROUTINE editToH5_SNK1
!
!-------------------------------------------------------------------------------
!> @brief Writes a rank-2parameter of type INTEGER(SNK) to an HDF5 file
!> @param this the parameter to write
!> @param file the HDF5 file type
!> @param path the location to write the parameter to
!>
SUBROUTINE editToH5_SNK2(this,file,path)
  CLASS(Param_SNK2),INTENT(IN) :: this
  CLASS(HDF5FileType),INTENT(INOUT) :: file
  CHARACTER(LEN=*),INTENT(IN) :: path
  !

  REQUIRE(file%isInit)
  REQUIRE(file%isOpen())

  CALL file%fwrite(path,this%p)

ENDSUBROUTINE editToH5_SNK2
!
!-------------------------------------------------------------------------------
!> @brief Writes a rank-3parameter of type INTEGER(SNK) to an HDF5 file
!> @param this the parameter to write
!> @param file the HDF5 file type
!> @param path the location to write the parameter to
!>
SUBROUTINE editToH5_SNK3(this,file,path)
  CLASS(Param_SNK3),INTENT(IN) :: this
  CLASS(HDF5FileType),INTENT(INOUT) :: file
  CHARACTER(LEN=*),INTENT(IN) :: path
  !

  REQUIRE(file%isInit)
  REQUIRE(file%isOpen())

  CALL file%fwrite(path,this%p)

ENDSUBROUTINE editToH5_SNK3
!
!-------------------------------------------------------------------------------
!> @brief Writes a rank-4parameter of type INTEGER(SNK) to an HDF5 file
!> @param this the parameter to write
!> @param file the HDF5 file type
!> @param path the location to write the parameter to
!>
SUBROUTINE editToH5_SNK4(this,file,path)
  CLASS(Param_SNK4),INTENT(IN) :: this
  CLASS(HDF5FileType),INTENT(INOUT) :: file
  CHARACTER(LEN=*),INTENT(IN) :: path
  !

  REQUIRE(file%isInit)
  REQUIRE(file%isOpen())

  CALL file%fwrite(path,this%p)

ENDSUBROUTINE editToH5_SNK4
!
!-------------------------------------------------------------------------------
!> @brief Writes a rank-5parameter of type INTEGER(SNK) to an HDF5 file
!> @param this the parameter to write
!> @param file the HDF5 file type
!> @param path the location to write the parameter to
!>
SUBROUTINE editToH5_SNK5(this,file,path)
  CLASS(Param_SNK5),INTENT(IN) :: this
  CLASS(HDF5FileType),INTENT(INOUT) :: file
  CHARACTER(LEN=*),INTENT(IN) :: path
  !

  REQUIRE(file%isInit)
  REQUIRE(file%isOpen())

  CALL file%fwrite(path,this%p)

ENDSUBROUTINE editToH5_SNK5
!
!-------------------------------------------------------------------------------
!> @brief Writes a rank-6parameter of type INTEGER(SNK) to an HDF5 file
!> @param this the parameter to write
!> @param file the HDF5 file type
!> @param path the location to write the parameter to
!>
SUBROUTINE editToH5_SNK6(this,file,path)
  CLASS(Param_SNK6),INTENT(IN) :: this
  CLASS(HDF5FileType),INTENT(INOUT) :: file
  CHARACTER(LEN=*),INTENT(IN) :: path
  !

  REQUIRE(file%isInit)
  REQUIRE(file%isOpen())

  CALL file%fwrite(path,this%p)

ENDSUBROUTINE editToH5_SNK6
!
!-------------------------------------------------------------------------------
!> @brief Writes a rank-7parameter of type INTEGER(SNK) to an HDF5 file
!> @param this the parameter to write
!> @param file the HDF5 file type
!> @param path the location to write the parameter to
!>
SUBROUTINE editToH5_SNK7(this,file,path)
  CLASS(Param_SNK7),INTENT(IN) :: this
  CLASS(HDF5FileType),INTENT(INOUT) :: file
  CHARACTER(LEN=*),INTENT(IN) :: path
  !

  REQUIRE(file%isInit)
  REQUIRE(file%isOpen())

  CALL file%fwrite(path,this%p)

ENDSUBROUTINE editToH5_SNK7
!
!-------------------------------------------------------------------------------
!> @brief Writes a rank-0parameter of type INTEGER(SLK) to an HDF5 file
!> @param this the parameter to write
!> @param file the HDF5 file type
!> @param path the location to write the parameter to
!>
SUBROUTINE editToH5_SLK0(this,file,path)
  CLASS(Param_SLK0),INTENT(IN) :: this
  CLASS(HDF5FileType),INTENT(INOUT) :: file
  CHARACTER(LEN=*),INTENT(IN) :: path
  !

  REQUIRE(file%isInit)
  REQUIRE(file%isOpen())

  CALL file%fwrite(path,this%p)

ENDSUBROUTINE editToH5_SLK0
!
!-------------------------------------------------------------------------------
!> @brief Writes a rank-1parameter of type INTEGER(SLK) to an HDF5 file
!> @param this the parameter to write
!> @param file the HDF5 file type
!> @param path the location to write the parameter to
!>
SUBROUTINE editToH5_SLK1(this,file,path)
  CLASS(Param_SLK1),INTENT(IN) :: this
  CLASS(HDF5FileType),INTENT(INOUT) :: file
  CHARACTER(LEN=*),INTENT(IN) :: path
  !

  REQUIRE(file%isInit)
  REQUIRE(file%isOpen())

  CALL file%fwrite(path,this%p)

ENDSUBROUTINE editToH5_SLK1
!
!-------------------------------------------------------------------------------
!> @brief Writes a rank-2parameter of type INTEGER(SLK) to an HDF5 file
!> @param this the parameter to write
!> @param file the HDF5 file type
!> @param path the location to write the parameter to
!>
SUBROUTINE editToH5_SLK2(this,file,path)
  CLASS(Param_SLK2),INTENT(IN) :: this
  CLASS(HDF5FileType),INTENT(INOUT) :: file
  CHARACTER(LEN=*),INTENT(IN) :: path
  !

  REQUIRE(file%isInit)
  REQUIRE(file%isOpen())

  CALL file%fwrite(path,this%p)

ENDSUBROUTINE editToH5_SLK2
!
!-------------------------------------------------------------------------------
!> @brief Writes a rank-3parameter of type INTEGER(SLK) to an HDF5 file
!> @param this the parameter to write
!> @param file the HDF5 file type
!> @param path the location to write the parameter to
!>
SUBROUTINE editToH5_SLK3(this,file,path)
  CLASS(Param_SLK3),INTENT(IN) :: this
  CLASS(HDF5FileType),INTENT(INOUT) :: file
  CHARACTER(LEN=*),INTENT(IN) :: path
  !

  REQUIRE(file%isInit)
  REQUIRE(file%isOpen())

  CALL file%fwrite(path,this%p)

ENDSUBROUTINE editToH5_SLK3
!
!-------------------------------------------------------------------------------
!> @brief Writes a rank-4parameter of type INTEGER(SLK) to an HDF5 file
!> @param this the parameter to write
!> @param file the HDF5 file type
!> @param path the location to write the parameter to
!>
SUBROUTINE editToH5_SLK4(this,file,path)
  CLASS(Param_SLK4),INTENT(IN) :: this
  CLASS(HDF5FileType),INTENT(INOUT) :: file
  CHARACTER(LEN=*),INTENT(IN) :: path
  !

  REQUIRE(file%isInit)
  REQUIRE(file%isOpen())

  CALL file%fwrite(path,this%p)

ENDSUBROUTINE editToH5_SLK4
!
!-------------------------------------------------------------------------------
!> @brief Writes a rank-5parameter of type INTEGER(SLK) to an HDF5 file
!> @param this the parameter to write
!> @param file the HDF5 file type
!> @param path the location to write the parameter to
!>
SUBROUTINE editToH5_SLK5(this,file,path)
  CLASS(Param_SLK5),INTENT(IN) :: this
  CLASS(HDF5FileType),INTENT(INOUT) :: file
  CHARACTER(LEN=*),INTENT(IN) :: path
  !

  REQUIRE(file%isInit)
  REQUIRE(file%isOpen())

  CALL file%fwrite(path,this%p)

ENDSUBROUTINE editToH5_SLK5
!
!-------------------------------------------------------------------------------
!> @brief Writes a rank-6parameter of type INTEGER(SLK) to an HDF5 file
!> @param this the parameter to write
!> @param file the HDF5 file type
!> @param path the location to write the parameter to
!>
SUBROUTINE editToH5_SLK6(this,file,path)
  CLASS(Param_SLK6),INTENT(IN) :: this
  CLASS(HDF5FileType),INTENT(INOUT) :: file
  CHARACTER(LEN=*),INTENT(IN) :: path
  !

  REQUIRE(file%isInit)
  REQUIRE(file%isOpen())

  CALL file%fwrite(path,this%p)

ENDSUBROUTINE editToH5_SLK6
!
!-------------------------------------------------------------------------------
!> @brief Writes a rank-7parameter of type INTEGER(SLK) to an HDF5 file
!> @param this the parameter to write
!> @param file the HDF5 file type
!> @param path the location to write the parameter to
!>
SUBROUTINE editToH5_SLK7(this,file,path)
  CLASS(Param_SLK7),INTENT(IN) :: this
  CLASS(HDF5FileType),INTENT(INOUT) :: file
  CHARACTER(LEN=*),INTENT(IN) :: path
  !

  REQUIRE(file%isInit)
  REQUIRE(file%isOpen())

  CALL file%fwrite(path,this%p)

ENDSUBROUTINE editToH5_SLK7
!
!-------------------------------------------------------------------------------
!> @brief Writes a rank-0parameter of type REAL(SSK) to an HDF5 file
!> @param this the parameter to write
!> @param file the HDF5 file type
!> @param path the location to write the parameter to
!>
SUBROUTINE editToH5_SSK0(this,file,path)
  CLASS(Param_SSK0),INTENT(IN) :: this
  CLASS(HDF5FileType),INTENT(INOUT) :: file
  CHARACTER(LEN=*),INTENT(IN) :: path
  !

  REQUIRE(file%isInit)
  REQUIRE(file%isOpen())

  CALL file%fwrite(path,this%p)

ENDSUBROUTINE editToH5_SSK0
!
!-------------------------------------------------------------------------------
!> @brief Writes a rank-1parameter of type REAL(SSK) to an HDF5 file
!> @param this the parameter to write
!> @param file the HDF5 file type
!> @param path the location to write the parameter to
!>
SUBROUTINE editToH5_SSK1(this,file,path)
  CLASS(Param_SSK1),INTENT(IN) :: this
  CLASS(HDF5FileType),INTENT(INOUT) :: file
  CHARACTER(LEN=*),INTENT(IN) :: path
  !

  REQUIRE(file%isInit)
  REQUIRE(file%isOpen())

  CALL file%fwrite(path,this%p)

ENDSUBROUTINE editToH5_SSK1
!
!-------------------------------------------------------------------------------
!> @brief Writes a rank-2parameter of type REAL(SSK) to an HDF5 file
!> @param this the parameter to write
!> @param file the HDF5 file type
!> @param path the location to write the parameter to
!>
SUBROUTINE editToH5_SSK2(this,file,path)
  CLASS(Param_SSK2),INTENT(IN) :: this
  CLASS(HDF5FileType),INTENT(INOUT) :: file
  CHARACTER(LEN=*),INTENT(IN) :: path
  !

  REQUIRE(file%isInit)
  REQUIRE(file%isOpen())

  CALL file%fwrite(path,this%p)

ENDSUBROUTINE editToH5_SSK2
!
!-------------------------------------------------------------------------------
!> @brief Writes a rank-3parameter of type REAL(SSK) to an HDF5 file
!> @param this the parameter to write
!> @param file the HDF5 file type
!> @param path the location to write the parameter to
!>
SUBROUTINE editToH5_SSK3(this,file,path)
  CLASS(Param_SSK3),INTENT(IN) :: this
  CLASS(HDF5FileType),INTENT(INOUT) :: file
  CHARACTER(LEN=*),INTENT(IN) :: path
  !

  REQUIRE(file%isInit)
  REQUIRE(file%isOpen())

  CALL file%fwrite(path,this%p)

ENDSUBROUTINE editToH5_SSK3
!
!-------------------------------------------------------------------------------
!> @brief Writes a rank-4parameter of type REAL(SSK) to an HDF5 file
!> @param this the parameter to write
!> @param file the HDF5 file type
!> @param path the location to write the parameter to
!>
SUBROUTINE editToH5_SSK4(this,file,path)
  CLASS(Param_SSK4),INTENT(IN) :: this
  CLASS(HDF5FileType),INTENT(INOUT) :: file
  CHARACTER(LEN=*),INTENT(IN) :: path
  !

  REQUIRE(file%isInit)
  REQUIRE(file%isOpen())

  CALL file%fwrite(path,this%p)

ENDSUBROUTINE editToH5_SSK4
!
!-------------------------------------------------------------------------------
!> @brief Writes a rank-5parameter of type REAL(SSK) to an HDF5 file
!> @param this the parameter to write
!> @param file the HDF5 file type
!> @param path the location to write the parameter to
!>
SUBROUTINE editToH5_SSK5(this,file,path)
  CLASS(Param_SSK5),INTENT(IN) :: this
  CLASS(HDF5FileType),INTENT(INOUT) :: file
  CHARACTER(LEN=*),INTENT(IN) :: path
  !

  REQUIRE(file%isInit)
  REQUIRE(file%isOpen())

  CALL file%fwrite(path,this%p)

ENDSUBROUTINE editToH5_SSK5
!
!-------------------------------------------------------------------------------
!> @brief Writes a rank-6parameter of type REAL(SSK) to an HDF5 file
!> @param this the parameter to write
!> @param file the HDF5 file type
!> @param path the location to write the parameter to
!>
SUBROUTINE editToH5_SSK6(this,file,path)
  CLASS(Param_SSK6),INTENT(IN) :: this
  CLASS(HDF5FileType),INTENT(INOUT) :: file
  CHARACTER(LEN=*),INTENT(IN) :: path
  !

  REQUIRE(file%isInit)
  REQUIRE(file%isOpen())

  CALL file%fwrite(path,this%p)

ENDSUBROUTINE editToH5_SSK6
!
!-------------------------------------------------------------------------------
!> @brief Writes a rank-7parameter of type REAL(SSK) to an HDF5 file
!> @param this the parameter to write
!> @param file the HDF5 file type
!> @param path the location to write the parameter to
!>
SUBROUTINE editToH5_SSK7(this,file,path)
  CLASS(Param_SSK7),INTENT(IN) :: this
  CLASS(HDF5FileType),INTENT(INOUT) :: file
  CHARACTER(LEN=*),INTENT(IN) :: path
  !

  REQUIRE(file%isInit)
  REQUIRE(file%isOpen())

  CALL file%fwrite(path,this%p)

ENDSUBROUTINE editToH5_SSK7
!
!-------------------------------------------------------------------------------
!> @brief Writes a rank-0parameter of type REAL(SDK) to an HDF5 file
!> @param this the parameter to write
!> @param file the HDF5 file type
!> @param path the location to write the parameter to
!>
SUBROUTINE editToH5_SDK0(this,file,path)
  CLASS(Param_SDK0),INTENT(IN) :: this
  CLASS(HDF5FileType),INTENT(INOUT) :: file
  CHARACTER(LEN=*),INTENT(IN) :: path
  !

  REQUIRE(file%isInit)
  REQUIRE(file%isOpen())

  CALL file%fwrite(path,this%p)

ENDSUBROUTINE editToH5_SDK0
!
!-------------------------------------------------------------------------------
!> @brief Writes a rank-1parameter of type REAL(SDK) to an HDF5 file
!> @param this the parameter to write
!> @param file the HDF5 file type
!> @param path the location to write the parameter to
!>
SUBROUTINE editToH5_SDK1(this,file,path)
  CLASS(Param_SDK1),INTENT(IN) :: this
  CLASS(HDF5FileType),INTENT(INOUT) :: file
  CHARACTER(LEN=*),INTENT(IN) :: path
  !

  REQUIRE(file%isInit)
  REQUIRE(file%isOpen())

  CALL file%fwrite(path,this%p)

ENDSUBROUTINE editToH5_SDK1
!
!-------------------------------------------------------------------------------
!> @brief Writes a rank-2parameter of type REAL(SDK) to an HDF5 file
!> @param this the parameter to write
!> @param file the HDF5 file type
!> @param path the location to write the parameter to
!>
SUBROUTINE editToH5_SDK2(this,file,path)
  CLASS(Param_SDK2),INTENT(IN) :: this
  CLASS(HDF5FileType),INTENT(INOUT) :: file
  CHARACTER(LEN=*),INTENT(IN) :: path
  !

  REQUIRE(file%isInit)
  REQUIRE(file%isOpen())

  CALL file%fwrite(path,this%p)

ENDSUBROUTINE editToH5_SDK2
!
!-------------------------------------------------------------------------------
!> @brief Writes a rank-3parameter of type REAL(SDK) to an HDF5 file
!> @param this the parameter to write
!> @param file the HDF5 file type
!> @param path the location to write the parameter to
!>
SUBROUTINE editToH5_SDK3(this,file,path)
  CLASS(Param_SDK3),INTENT(IN) :: this
  CLASS(HDF5FileType),INTENT(INOUT) :: file
  CHARACTER(LEN=*),INTENT(IN) :: path
  !

  REQUIRE(file%isInit)
  REQUIRE(file%isOpen())

  CALL file%fwrite(path,this%p)

ENDSUBROUTINE editToH5_SDK3
!
!-------------------------------------------------------------------------------
!> @brief Writes a rank-4parameter of type REAL(SDK) to an HDF5 file
!> @param this the parameter to write
!> @param file the HDF5 file type
!> @param path the location to write the parameter to
!>
SUBROUTINE editToH5_SDK4(this,file,path)
  CLASS(Param_SDK4),INTENT(IN) :: this
  CLASS(HDF5FileType),INTENT(INOUT) :: file
  CHARACTER(LEN=*),INTENT(IN) :: path
  !

  REQUIRE(file%isInit)
  REQUIRE(file%isOpen())

  CALL file%fwrite(path,this%p)

ENDSUBROUTINE editToH5_SDK4
!
!-------------------------------------------------------------------------------
!> @brief Writes a rank-5parameter of type REAL(SDK) to an HDF5 file
!> @param this the parameter to write
!> @param file the HDF5 file type
!> @param path the location to write the parameter to
!>
SUBROUTINE editToH5_SDK5(this,file,path)
  CLASS(Param_SDK5),INTENT(IN) :: this
  CLASS(HDF5FileType),INTENT(INOUT) :: file
  CHARACTER(LEN=*),INTENT(IN) :: path
  !

  REQUIRE(file%isInit)
  REQUIRE(file%isOpen())

  CALL file%fwrite(path,this%p)

ENDSUBROUTINE editToH5_SDK5
!
!-------------------------------------------------------------------------------
!> @brief Writes a rank-6parameter of type REAL(SDK) to an HDF5 file
!> @param this the parameter to write
!> @param file the HDF5 file type
!> @param path the location to write the parameter to
!>
SUBROUTINE editToH5_SDK6(this,file,path)
  CLASS(Param_SDK6),INTENT(IN) :: this
  CLASS(HDF5FileType),INTENT(INOUT) :: file
  CHARACTER(LEN=*),INTENT(IN) :: path
  !

  REQUIRE(file%isInit)
  REQUIRE(file%isOpen())

  CALL file%fwrite(path,this%p)

ENDSUBROUTINE editToH5_SDK6
!
!-------------------------------------------------------------------------------
!> @brief Writes a rank-7parameter of type REAL(SDK) to an HDF5 file
!> @param this the parameter to write
!> @param file the HDF5 file type
!> @param path the location to write the parameter to
!>
SUBROUTINE editToH5_SDK7(this,file,path)
  CLASS(Param_SDK7),INTENT(IN) :: this
  CLASS(HDF5FileType),INTENT(INOUT) :: file
  CHARACTER(LEN=*),INTENT(IN) :: path
  !

  REQUIRE(file%isInit)
  REQUIRE(file%isOpen())

  CALL file%fwrite(path,this%p)

ENDSUBROUTINE editToH5_SDK7
!
!-------------------------------------------------------------------------------
!> @brief Writes a rank-0parameter of type TYPE(StringType) to an HDF5 file
!> @param this the parameter to write
!> @param file the HDF5 file type
!> @param path the location to write the parameter to
!>
SUBROUTINE editToH5_STR0(this,file,path)
  CLASS(Param_STR0),INTENT(IN) :: this
  CLASS(HDF5FileType),INTENT(INOUT) :: file
  CHARACTER(LEN=*),INTENT(IN) :: path
  !

  REQUIRE(file%isInit)
  REQUIRE(file%isOpen())

  CALL file%fwrite(path,this%p)

ENDSUBROUTINE editToH5_STR0
!
!-------------------------------------------------------------------------------
!> @brief Writes a rank-1parameter of type TYPE(StringType) to an HDF5 file
!> @param this the parameter to write
!> @param file the HDF5 file type
!> @param path the location to write the parameter to
!>
SUBROUTINE editToH5_STR1(this,file,path)
  CLASS(Param_STR1),INTENT(IN) :: this
  CLASS(HDF5FileType),INTENT(INOUT) :: file
  CHARACTER(LEN=*),INTENT(IN) :: path
  !

  REQUIRE(file%isInit)
  REQUIRE(file%isOpen())

  CALL file%fwrite(path,this%p)

ENDSUBROUTINE editToH5_STR1
!
!-------------------------------------------------------------------------------
!> @brief Writes a rank-2parameter of type TYPE(StringType) to an HDF5 file
!> @param this the parameter to write
!> @param file the HDF5 file type
!> @param path the location to write the parameter to
!>
SUBROUTINE editToH5_STR2(this,file,path)
  CLASS(Param_STR2),INTENT(IN) :: this
  CLASS(HDF5FileType),INTENT(INOUT) :: file
  CHARACTER(LEN=*),INTENT(IN) :: path
  !

  REQUIRE(file%isInit)
  REQUIRE(file%isOpen())

  CALL file%fwrite(path,this%p)

ENDSUBROUTINE editToH5_STR2
!
!-------------------------------------------------------------------------------
!> @brief Writes a rank-3parameter of type TYPE(StringType) to an HDF5 file
!> @param this the parameter to write
!> @param file the HDF5 file type
!> @param path the location to write the parameter to
!>
SUBROUTINE editToH5_STR3(this,file,path)
  CLASS(Param_STR3),INTENT(IN) :: this
  CLASS(HDF5FileType),INTENT(INOUT) :: file
  CHARACTER(LEN=*),INTENT(IN) :: path
  !

  REQUIRE(file%isInit)
  REQUIRE(file%isOpen())

  CALL file%fwrite(path,this%p)

ENDSUBROUTINE editToH5_STR3
!
!-------------------------------------------------------------------------------
!> @brief Performs a soft equals operation on two Param_Base objects
!> @param lhs the first parameter to compare
!> @param lhs the second parameter to compare
!> @param tol the tolerance of the comparison
!> @returns equals the result of the comparison
!>
IMPURE ELEMENTAL FUNCTION SOFTEQ_Param(lhs,rhs,tol) RESULT(equals)
  CLASS(Param_Base),INTENT(IN) :: lhs
  CLASS(Param_Base),INTENT(IN) :: rhs
  REAL(SDK),INTENT(IN) :: tol
  LOGICAL(SBK) :: equals

  equals = SAME_TYPE_AS(lhs,rhs)
  IF(.NOT.equals) RETURN

  SELECTTYPE(lhs)
  TYPE IS(Param_SSK0)
    SELECTTYPE(rhs); TYPE IS(Param_SSK0)
      equals = SOFTEQ(lhs%p,rhs%p,REAL(tol,SSK))
    ENDSELECT
  TYPE IS(Param_SSK1)
    SELECTTYPE(rhs); TYPE IS(Param_SSK1)
      equals = ALL(SOFTEQ(lhs%p,rhs%p,REAL(tol,SSK)))
    ENDSELECT
  TYPE IS(Param_SSK2)
    SELECTTYPE(rhs); TYPE IS(Param_SSK2)
      equals = ALL(SOFTEQ(lhs%p,rhs%p,REAL(tol,SSK)))
    ENDSELECT
  TYPE IS(Param_SSK3)
    SELECTTYPE(rhs); TYPE IS(Param_SSK3)
      equals = ALL(SOFTEQ(lhs%p,rhs%p,REAL(tol,SSK)))
    ENDSELECT
  TYPE IS(Param_SSK4)
    SELECTTYPE(rhs); TYPE IS(Param_SSK4)
      equals = ALL(SOFTEQ(lhs%p,rhs%p,REAL(tol,SSK)))
    ENDSELECT
  TYPE IS(Param_SSK5)
    SELECTTYPE(rhs); TYPE IS(Param_SSK5)
      equals = ALL(SOFTEQ(lhs%p,rhs%p,REAL(tol,SSK)))
    ENDSELECT
  TYPE IS(Param_SSK6)
    SELECTTYPE(rhs); TYPE IS(Param_SSK6)
      equals = ALL(SOFTEQ(lhs%p,rhs%p,REAL(tol,SSK)))
    ENDSELECT
  TYPE IS(Param_SSK7)
    SELECTTYPE(rhs); TYPE IS(Param_SSK7)
      equals = ALL(SOFTEQ(lhs%p,rhs%p,REAL(tol,SSK)))
    ENDSELECT
  TYPE IS(Param_SDK0)
    SELECTTYPE(rhs); TYPE IS(Param_SDK0)
      equals = SOFTEQ(lhs%p,rhs%p,REAL(tol,SDK))
    ENDSELECT
  TYPE IS(Param_SDK1)
    SELECTTYPE(rhs); TYPE IS(Param_SDK1)
      equals = ALL(SOFTEQ(lhs%p,rhs%p,REAL(tol,SDK)))
    ENDSELECT
  TYPE IS(Param_SDK2)
    SELECTTYPE(rhs); TYPE IS(Param_SDK2)
      equals = ALL(SOFTEQ(lhs%p,rhs%p,REAL(tol,SDK)))
    ENDSELECT
  TYPE IS(Param_SDK3)
    SELECTTYPE(rhs); TYPE IS(Param_SDK3)
      equals = ALL(SOFTEQ(lhs%p,rhs%p,REAL(tol,SDK)))
    ENDSELECT
  TYPE IS(Param_SDK4)
    SELECTTYPE(rhs); TYPE IS(Param_SDK4)
      equals = ALL(SOFTEQ(lhs%p,rhs%p,REAL(tol,SDK)))
    ENDSELECT
  TYPE IS(Param_SDK5)
    SELECTTYPE(rhs); TYPE IS(Param_SDK5)
      equals = ALL(SOFTEQ(lhs%p,rhs%p,REAL(tol,SDK)))
    ENDSELECT
  TYPE IS(Param_SDK6)
    SELECTTYPE(rhs); TYPE IS(Param_SDK6)
      equals = ALL(SOFTEQ(lhs%p,rhs%p,REAL(tol,SDK)))
    ENDSELECT
  TYPE IS(Param_SDK7)
    SELECTTYPE(rhs); TYPE IS(Param_SDK7)
      equals = ALL(SOFTEQ(lhs%p,rhs%p,REAL(tol,SDK)))
    ENDSELECT
  CLASS DEFAULT
    equals = (lhs == rhs)
  ENDSELECT

ENDFUNCTION SOFTEQ_Param
