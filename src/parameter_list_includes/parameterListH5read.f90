!
!-------------------------------------------------------------------------------
!> @brief Reads a @c ParamNode from an HDF5 file
!> @param this the parameter to write
!> @param file the HDF5 file type
!> @param path the location to write the parameter to
!>
SUBROUTINE readNodeFromH5(this,file,path,dataType,rank)
  CLASS(ParamNode),INTENT(INOUT) :: this
  CLASS(HDF5FileType),INTENT(INOUT) :: file
  CHARACTER(LEN=*),INTENT(IN) :: path
  TYPE(StringType),INTENT(IN) :: dataType
  INTEGER(SIK),INTENT(IN) :: rank
  !
  TYPE(StringType) :: description
  LOGICAL(SBK) :: readSBK0
  LOGICAL(SBK),ALLOCATABLE :: readSBK1(:)
  LOGICAL(SBK),ALLOCATABLE :: readSBK2(:,:)
  LOGICAL(SBK),ALLOCATABLE :: readSBK3(:,:,:)
  INTEGER(SNK) :: readSNK0
  INTEGER(SNK),ALLOCATABLE :: readSNK1(:)
  INTEGER(SNK),ALLOCATABLE :: readSNK2(:,:)
  INTEGER(SNK),ALLOCATABLE :: readSNK3(:,:,:)
  INTEGER(SNK),ALLOCATABLE :: readSNK4(:,:,:,:)
  INTEGER(SNK),ALLOCATABLE :: readSNK5(:,:,:,:,:)
  INTEGER(SNK),ALLOCATABLE :: readSNK6(:,:,:,:,:,:)
  INTEGER(SNK),ALLOCATABLE :: readSNK7(:,:,:,:,:,:,:)
  INTEGER(SLK) :: readSLK0
  INTEGER(SLK),ALLOCATABLE :: readSLK1(:)
  INTEGER(SLK),ALLOCATABLE :: readSLK2(:,:)
  INTEGER(SLK),ALLOCATABLE :: readSLK3(:,:,:)
  INTEGER(SLK),ALLOCATABLE :: readSLK4(:,:,:,:)
  INTEGER(SLK),ALLOCATABLE :: readSLK5(:,:,:,:,:)
  INTEGER(SLK),ALLOCATABLE :: readSLK6(:,:,:,:,:,:)
  INTEGER(SLK),ALLOCATABLE :: readSLK7(:,:,:,:,:,:,:)
  REAL(SSK) :: readSSK0
  REAL(SSK),ALLOCATABLE :: readSSK1(:)
  REAL(SSK),ALLOCATABLE :: readSSK2(:,:)
  REAL(SSK),ALLOCATABLE :: readSSK3(:,:,:)
  REAL(SSK),ALLOCATABLE :: readSSK4(:,:,:,:)
  REAL(SSK),ALLOCATABLE :: readSSK5(:,:,:,:,:)
  REAL(SSK),ALLOCATABLE :: readSSK6(:,:,:,:,:,:)
  REAL(SSK),ALLOCATABLE :: readSSK7(:,:,:,:,:,:,:)
  REAL(SDK) :: readSDK0
  REAL(SDK),ALLOCATABLE :: readSDK1(:)
  REAL(SDK),ALLOCATABLE :: readSDK2(:,:)
  REAL(SDK),ALLOCATABLE :: readSDK3(:,:,:)
  REAL(SDK),ALLOCATABLE :: readSDK4(:,:,:,:)
  REAL(SDK),ALLOCATABLE :: readSDK5(:,:,:,:,:)
  REAL(SDK),ALLOCATABLE :: readSDK6(:,:,:,:,:,:)
  REAL(SDK),ALLOCATABLE :: readSDK7(:,:,:,:,:,:,:)
  TYPE(StringType) :: readSTR0
  TYPE(StringType),ALLOCATABLE :: readSTR1(:)
  TYPE(StringType),ALLOCATABLE :: readSTR2(:,:)
  TYPE(StringType),ALLOCATABLE :: readSTR3(:,:,:)

  REQUIRE(file%isInit)
  REQUIRE(file%isOpen())
  REQUIRE(.NOT.ALLOCATED(this%val))

  !Get the description if there is one
  IF(file%has_attribute(path,'description')) THEN
    CALL file%read_attribute(path,'description',description)
  ELSE
    description = ''
  ENDIF
  SELECTCASE(CHAR(dataType))
  CASE('Tree')
    CALL this%initTree(CHAR(this%name),DESCRIPTION=CHAR(description))
  CASE('SNK')
    SELECTCASE(rank)
    CASE(0)
      CALL file%fread(path,readSNK0)
      CALL this%initSNK0(CHAR(this%name),readSNK0,CHAR(description))
    CASE(1)
      CALL file%fread(path,readSNK1)
      CALL this%initSNK1(CHAR(this%name),readSNK1,CHAR(description))
    CASE(2)
      CALL file%fread(path,readSNK2)
      CALL this%initSNK2(CHAR(this%name),readSNK2,CHAR(description))
    CASE(3)
      CALL file%fread(path,readSNK3)
      CALL this%initSNK3(CHAR(this%name),readSNK3,CHAR(description))
    CASE(4)
      CALL file%fread(path,readSNK4)
      CALL this%initSNK4(CHAR(this%name),readSNK4,CHAR(description))
    CASE(5)
      CALL file%fread(path,readSNK5)
      CALL this%initSNK5(CHAR(this%name),readSNK5,CHAR(description))
    CASE(6)
      CALL file%fread(path,readSNK6)
      CALL this%initSNK6(CHAR(this%name),readSNK6,CHAR(description))
    CASE(7)
      CALL file%fread(path,readSNK7)
      CALL this%initSNK7(CHAR(this%name),readSNK7,CHAR(description))
    ENDSELECT
  CASE('SLK')
    SELECTCASE(rank)
    CASE(0)
      CALL file%fread(path,readSLK0)
      CALL this%initSLK0(CHAR(this%name),readSLK0,CHAR(description))
    CASE(1)
      CALL file%fread(path,readSLK1)
      CALL this%initSLK1(CHAR(this%name),readSLK1,CHAR(description))
    CASE(2)
      CALL file%fread(path,readSLK2)
      CALL this%initSLK2(CHAR(this%name),readSLK2,CHAR(description))
    CASE(3)
      CALL file%fread(path,readSLK3)
      CALL this%initSLK3(CHAR(this%name),readSLK3,CHAR(description))
    CASE(4)
      CALL file%fread(path,readSLK4)
      CALL this%initSLK4(CHAR(this%name),readSLK4,CHAR(description))
    CASE(5)
      CALL file%fread(path,readSLK5)
      CALL this%initSLK5(CHAR(this%name),readSLK5,CHAR(description))
    CASE(6)
      CALL file%fread(path,readSLK6)
      CALL this%initSLK6(CHAR(this%name),readSLK6,CHAR(description))
    CASE(7)
      CALL file%fread(path,readSLK7)
      CALL this%initSLK7(CHAR(this%name),readSLK7,CHAR(description))
    ENDSELECT
  CASE('SSK')
    SELECTCASE(rank)
    CASE(0)
      CALL file%fread(path,readSSK0)
      CALL this%initSSK0(CHAR(this%name),readSSK0,CHAR(description))
    CASE(1)
      CALL file%fread(path,readSSK1)
      CALL this%initSSK1(CHAR(this%name),readSSK1,CHAR(description))
    CASE(2)
      CALL file%fread(path,readSSK2)
      CALL this%initSSK2(CHAR(this%name),readSSK2,CHAR(description))
    CASE(3)
      CALL file%fread(path,readSSK3)
      CALL this%initSSK3(CHAR(this%name),readSSK3,CHAR(description))
    CASE(4)
      CALL file%fread(path,readSSK4)
      CALL this%initSSK4(CHAR(this%name),readSSK4,CHAR(description))
    CASE(5)
      CALL file%fread(path,readSSK5)
      CALL this%initSSK5(CHAR(this%name),readSSK5,CHAR(description))
    CASE(6)
      CALL file%fread(path,readSSK6)
      CALL this%initSSK6(CHAR(this%name),readSSK6,CHAR(description))
    CASE(7)
      CALL file%fread(path,readSSK7)
      CALL this%initSSK7(CHAR(this%name),readSSK7,CHAR(description))
    ENDSELECT
  CASE('SDK')
    SELECTCASE(rank)
    CASE(0)
      CALL file%fread(path,readSDK0)
      CALL this%initSDK0(CHAR(this%name),readSDK0,CHAR(description))
    CASE(1)
      CALL file%fread(path,readSDK1)
      CALL this%initSDK1(CHAR(this%name),readSDK1,CHAR(description))
    CASE(2)
      CALL file%fread(path,readSDK2)
      CALL this%initSDK2(CHAR(this%name),readSDK2,CHAR(description))
    CASE(3)
      CALL file%fread(path,readSDK3)
      CALL this%initSDK3(CHAR(this%name),readSDK3,CHAR(description))
    CASE(4)
      CALL file%fread(path,readSDK4)
      CALL this%initSDK4(CHAR(this%name),readSDK4,CHAR(description))
    CASE(5)
      CALL file%fread(path,readSDK5)
      CALL this%initSDK5(CHAR(this%name),readSDK5,CHAR(description))
    CASE(6)
      CALL file%fread(path,readSDK6)
      CALL this%initSDK6(CHAR(this%name),readSDK6,CHAR(description))
    CASE(7)
      CALL file%fread(path,readSDK7)
      CALL this%initSDK7(CHAR(this%name),readSDK7,CHAR(description))
    ENDSELECT
  CASE('STR')
    SELECTCASE(rank)
    CASE(0)
      CALL file%fread(path,readSTR0)
      IF(readSTR0 == 'T' .OR. readSTR0 == 'F') THEN
        CALL file%fread(path,readSBK0)
        CALL this%initSBK0(CHAR(this%name),readSBK0,CHAR(description))
      ELSE
        CALL this%initSTR0(CHAR(this%name),readSTR0,CHAR(description))
      ENDIF
    CASE(1)
      CALL file%fread(path,readSTR1)
      IF(ALL(readSTR1 == 'T' .OR. readSTR1 == 'F') .AND. SIZE(readSTR1) > 0) THEN
        CALL file%fread(path,readSBK1)
        CALL this%initSBK1(CHAR(this%name),readSBK1,CHAR(description))
      ELSE
        CALL this%initSTR1(CHAR(this%name),readSTR1,CHAR(description))
      ENDIF
    CASE(2)
      CALL file%fread(path,readSTR2)
      IF(ALL(readSTR2 == 'T' .OR. readSTR2 == 'F') .AND. SIZE(readSTR2) > 0) THEN
        CALL file%fread(path,readSBK2)
        CALL this%initSBK2(CHAR(this%name),readSBK2,CHAR(description))
      ELSE
        CALL this%initSTR2(CHAR(this%name),readSTR2,CHAR(description))
      ENDIF
    CASE(3)
      CALL file%fread(path,readSTR3)
      IF(ALL(readSTR3 == 'T' .OR. readSTR3 == 'F') .AND. SIZE(readSTR3) > 0) THEN
        CALL file%fread(path,readSBK3)
        CALL this%initSBK3(CHAR(this%name),readSBK3,CHAR(description))
      ELSE
        CALL this%initSTR3(CHAR(this%name),readSTR3,CHAR(description))
      ENDIF
    ENDSELECT
  ENDSELECT

ENDSUBROUTINE readNodeFromH5