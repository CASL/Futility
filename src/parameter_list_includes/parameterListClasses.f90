!> List of enumerations to delineate various parameter types
INTEGER(SIK),PARAMETER,PUBLIC :: PL_DATA_TYPE_NULL=0_SIK
INTEGER(SIK),PARAMETER,PUBLIC :: PL_DATA_TYPE_TREE=1_SIK
INTEGER(SIK),PARAMETER,PUBLIC :: PL_DATA_TYPE_SBK0=2_SIK
INTEGER(SIK),PARAMETER,PUBLIC :: PL_DATA_TYPE_SBK1=3_SIK
INTEGER(SIK),PARAMETER,PUBLIC :: PL_DATA_TYPE_SBK2=4_SIK
INTEGER(SIK),PARAMETER,PUBLIC :: PL_DATA_TYPE_SBK3=5_SIK
INTEGER(SIK),PARAMETER,PUBLIC :: PL_DATA_TYPE_SBK4=6_SIK
INTEGER(SIK),PARAMETER,PUBLIC :: PL_DATA_TYPE_SBK5=7_SIK
INTEGER(SIK),PARAMETER,PUBLIC :: PL_DATA_TYPE_SBK6=8_SIK
INTEGER(SIK),PARAMETER,PUBLIC :: PL_DATA_TYPE_SBK7=9_SIK
INTEGER(SIK),PARAMETER,PUBLIC :: PL_DATA_TYPE_SNK0=10_SIK
INTEGER(SIK),PARAMETER,PUBLIC :: PL_DATA_TYPE_SNK1=11_SIK
INTEGER(SIK),PARAMETER,PUBLIC :: PL_DATA_TYPE_SNK2=12_SIK
INTEGER(SIK),PARAMETER,PUBLIC :: PL_DATA_TYPE_SNK3=13_SIK
INTEGER(SIK),PARAMETER,PUBLIC :: PL_DATA_TYPE_SNK4=14_SIK
INTEGER(SIK),PARAMETER,PUBLIC :: PL_DATA_TYPE_SNK5=15_SIK
INTEGER(SIK),PARAMETER,PUBLIC :: PL_DATA_TYPE_SNK6=16_SIK
INTEGER(SIK),PARAMETER,PUBLIC :: PL_DATA_TYPE_SNK7=17_SIK
INTEGER(SIK),PARAMETER,PUBLIC :: PL_DATA_TYPE_SLK0=18_SIK
INTEGER(SIK),PARAMETER,PUBLIC :: PL_DATA_TYPE_SLK1=19_SIK
INTEGER(SIK),PARAMETER,PUBLIC :: PL_DATA_TYPE_SLK2=20_SIK
INTEGER(SIK),PARAMETER,PUBLIC :: PL_DATA_TYPE_SLK3=21_SIK
INTEGER(SIK),PARAMETER,PUBLIC :: PL_DATA_TYPE_SLK4=22_SIK
INTEGER(SIK),PARAMETER,PUBLIC :: PL_DATA_TYPE_SLK5=23_SIK
INTEGER(SIK),PARAMETER,PUBLIC :: PL_DATA_TYPE_SLK6=24_SIK
INTEGER(SIK),PARAMETER,PUBLIC :: PL_DATA_TYPE_SLK7=25_SIK
INTEGER(SIK),PARAMETER,PUBLIC :: PL_DATA_TYPE_SSK0=26_SIK
INTEGER(SIK),PARAMETER,PUBLIC :: PL_DATA_TYPE_SSK1=27_SIK
INTEGER(SIK),PARAMETER,PUBLIC :: PL_DATA_TYPE_SSK2=28_SIK
INTEGER(SIK),PARAMETER,PUBLIC :: PL_DATA_TYPE_SSK3=29_SIK
INTEGER(SIK),PARAMETER,PUBLIC :: PL_DATA_TYPE_SSK4=30_SIK
INTEGER(SIK),PARAMETER,PUBLIC :: PL_DATA_TYPE_SSK5=31_SIK
INTEGER(SIK),PARAMETER,PUBLIC :: PL_DATA_TYPE_SSK6=32_SIK
INTEGER(SIK),PARAMETER,PUBLIC :: PL_DATA_TYPE_SSK7=33_SIK
INTEGER(SIK),PARAMETER,PUBLIC :: PL_DATA_TYPE_SDK0=34_SIK
INTEGER(SIK),PARAMETER,PUBLIC :: PL_DATA_TYPE_SDK1=35_SIK
INTEGER(SIK),PARAMETER,PUBLIC :: PL_DATA_TYPE_SDK2=36_SIK
INTEGER(SIK),PARAMETER,PUBLIC :: PL_DATA_TYPE_SDK3=37_SIK
INTEGER(SIK),PARAMETER,PUBLIC :: PL_DATA_TYPE_SDK4=38_SIK
INTEGER(SIK),PARAMETER,PUBLIC :: PL_DATA_TYPE_SDK5=39_SIK
INTEGER(SIK),PARAMETER,PUBLIC :: PL_DATA_TYPE_SDK6=40_SIK
INTEGER(SIK),PARAMETER,PUBLIC :: PL_DATA_TYPE_SDK7=41_SIK
INTEGER(SIK),PARAMETER,PUBLIC :: PL_DATA_TYPE_STR0=42_SIK
INTEGER(SIK),PARAMETER,PUBLIC :: PL_DATA_TYPE_STR1=43_SIK
INTEGER(SIK),PARAMETER,PUBLIC :: PL_DATA_TYPE_STR2=44_SIK
INTEGER(SIK),PARAMETER,PUBLIC :: PL_DATA_TYPE_STR3=45_SIK
INTEGER(SIK),PARAMETER,PUBLIC :: PL_DATA_TYPE_STR4=46_SIK
INTEGER(SIK),PARAMETER,PUBLIC :: PL_DATA_TYPE_STR5=47_SIK
INTEGER(SIK),PARAMETER,PUBLIC :: PL_DATA_TYPE_STR6=48_SIK
INTEGER(SIK),PARAMETER,PUBLIC :: PL_DATA_TYPE_STR7=49_SIK

!> List of names for each data type
CHARACTER(LEN=*),PARAMETER,DIMENSION(0:49),PUBLIC :: paramTypeNames = &
       ['NULL                      ', &
        'TYPE(ParamType)           ', &
        'LOGICAL(SBK)              ', &
        '1-D ARRAY LOGICAL(SBK)    ', &
        '2-D ARRAY LOGICAL(SBK)    ', &
        '3-D ARRAY LOGICAL(SBK)    ', &
        '4-D ARRAY LOGICAL(SBK)    ', &
        '5-D ARRAY LOGICAL(SBK)    ', &
        '6-D ARRAY LOGICAL(SBK)    ', &
        '7-D ARRAY LOGICAL(SBK)    ', &
        'INTEGER(SNK)              ', &
        '1-D ARRAY INTEGER(SNK)    ', &
        '2-D ARRAY INTEGER(SNK)    ', &
        '3-D ARRAY INTEGER(SNK)    ', &
        '4-D ARRAY INTEGER(SNK)    ', &
        '5-D ARRAY INTEGER(SNK)    ', &
        '6-D ARRAY INTEGER(SNK)    ', &
        '7-D ARRAY INTEGER(SNK)    ', &
        'INTEGER(SLK)              ', &
        '1-D ARRAY INTEGER(SLK)    ', &
        '2-D ARRAY INTEGER(SLK)    ', &
        '3-D ARRAY INTEGER(SLK)    ', &
        '4-D ARRAY INTEGER(SLK)    ', &
        '5-D ARRAY INTEGER(SLK)    ', &
        '6-D ARRAY INTEGER(SLK)    ', &
        '7-D ARRAY INTEGER(SLK)    ', &
        'REAL(SSK)                 ', &
        '1-D ARRAY REAL(SSK)       ', &
        '2-D ARRAY REAL(SSK)       ', &
        '3-D ARRAY REAL(SSK)       ', &
        '4-D ARRAY REAL(SSK)       ', &
        '5-D ARRAY REAL(SSK)       ', &
        '6-D ARRAY REAL(SSK)       ', &
        '7-D ARRAY REAL(SSK)       ', &
        'REAL(SDK)                 ', &
        '1-D ARRAY REAL(SDK)       ', &
        '2-D ARRAY REAL(SDK)       ', &
        '3-D ARRAY REAL(SDK)       ', &
        '4-D ARRAY REAL(SDK)       ', &
        '5-D ARRAY REAL(SDK)       ', &
        '6-D ARRAY REAL(SDK)       ', &
        '7-D ARRAY REAL(SDK)       ', &
        'TYPE(StringType)          ', &
        '1-D ARRAY TYPE(StringType)', &
        '2-D ARRAY TYPE(StringType)', &
        '3-D ARRAY TYPE(StringType)', &
        '4-D ARRAY TYPE(StringType)', &
        '5-D ARRAY TYPE(StringType)', &
        '6-D ARRAY TYPE(StringType)', &
        '7-D ARRAY TYPE(StringType)']

!> @brief defines the parameter tree node data extension for Rank-0
TYPE,EXTENDS(Param_Base) :: Param_SBK0
  PRIVATE
  LOGICAL(SBK) :: p
  CONTAINS
    !> @copybrief ParameterLists::editToText_SBK0
    !> @copydetails ParameterLists::editToText_SBK0
    PROCEDURE,PASS,PRIVATE :: editToText => editToText_SBK0
    !> @copybrief ParameterLists::editToH5_SBK0
    !> @copydetails ParameterLists::editToH5_SBK0
    PROCEDURE,PASS,PRIVATE :: editToH5 => editToH5_SBK0
ENDTYPE Param_SBK0

!> @brief defines the parameter tree node data extension for Rank-1
TYPE,EXTENDS(Param_Base) :: Param_SBK1
  PRIVATE
  LOGICAL(SBK),ALLOCATABLE :: p(:)
  CONTAINS
    !> @copybrief ParameterLists::editToText_SBK1
    !> @copydetails ParameterLists::editToText_SBK1
    PROCEDURE,PASS,PRIVATE :: editToText => editToText_SBK1
    !> @copybrief ParameterLists::editToH5_SBK1
    !> @copydetails ParameterLists::editToH5_SBK1
    PROCEDURE,PASS,PRIVATE :: editToH5 => editToH5_SBK1
ENDTYPE Param_SBK1

!> @brief defines the parameter tree node data extension for Rank-2
TYPE,EXTENDS(Param_Base) :: Param_SBK2
  PRIVATE
  LOGICAL(SBK),ALLOCATABLE :: p(:,:)
  CONTAINS
    !> @copybrief ParameterLists::editToText_SBK2
    !> @copydetails ParameterLists::editToText_SBK2
    PROCEDURE,PASS,PRIVATE :: editToText => editToText_SBK2
    !> @copybrief ParameterLists::editToH5_SBK2
    !> @copydetails ParameterLists::editToH5_SBK2
    PROCEDURE,PASS,PRIVATE :: editToH5 => editToH5_SBK2
ENDTYPE Param_SBK2

!> @brief defines the parameter tree node data extension for Rank-3
TYPE,EXTENDS(Param_Base) :: Param_SBK3
  PRIVATE
  LOGICAL(SBK),ALLOCATABLE :: p(:,:,:)
  CONTAINS
    !> @copybrief ParameterLists::editToText_SBK3
    !> @copydetails ParameterLists::editToText_SBK3
    PROCEDURE,PASS,PRIVATE :: editToText => editToText_SBK3
    !> @copybrief ParameterLists::editToH5_SBK3
    !> @copydetails ParameterLists::editToH5_SBK3
    PROCEDURE,PASS,PRIVATE :: editToH5 => editToH5_SBK3
ENDTYPE Param_SBK3

!> @brief defines the parameter tree node data extension for Rank-4
TYPE,EXTENDS(Param_Base) :: Param_SBK4
  PRIVATE
  LOGICAL(SBK),ALLOCATABLE :: p(:,:,:,:)
  CONTAINS
    !> @copybrief ParameterLists::editToText_SBK4
    !> @copydetails ParameterLists::editToText_SBK4
    PROCEDURE,PASS,PRIVATE :: editToText => editToText_SBK4
ENDTYPE Param_SBK4

!> @brief defines the parameter tree node data extension for Rank-5
TYPE,EXTENDS(Param_Base) :: Param_SBK5
  PRIVATE
  LOGICAL(SBK),ALLOCATABLE :: p(:,:,:,:,:)
  CONTAINS
    !> @copybrief ParameterLists::editToText_SBK5
    !> @copydetails ParameterLists::editToText_SBK5
    PROCEDURE,PASS,PRIVATE :: editToText => editToText_SBK5
ENDTYPE Param_SBK5

!> @brief defines the parameter tree node data extension for Rank-6
TYPE,EXTENDS(Param_Base) :: Param_SBK6
  PRIVATE
  LOGICAL(SBK),ALLOCATABLE :: p(:,:,:,:,:,:)
  CONTAINS
    !> @copybrief ParameterLists::editToText_SBK6
    !> @copydetails ParameterLists::editToText_SBK6
    PROCEDURE,PASS,PRIVATE :: editToText => editToText_SBK6
ENDTYPE Param_SBK6

!> @brief defines the parameter tree node data extension for Rank-7
TYPE,EXTENDS(Param_Base) :: Param_SBK7
  PRIVATE
  LOGICAL(SBK),ALLOCATABLE :: p(:,:,:,:,:,:,:)
  CONTAINS
    !> @copybrief ParameterLists::editToText_SBK7
    !> @copydetails ParameterLists::editToText_SBK7
    PROCEDURE,PASS,PRIVATE :: editToText => editToText_SBK7
ENDTYPE Param_SBK7

!> @brief defines the parameter tree node data extension for Rank-0
TYPE,EXTENDS(Param_Base) :: Param_SNK0
  PRIVATE
  INTEGER(SNK) :: p
  CONTAINS
    !> @copybrief ParameterLists::editToText_SNK0
    !> @copydetails ParameterLists::editToText_SNK0
    PROCEDURE,PASS,PRIVATE :: editToText => editToText_SNK0
    !> @copybrief ParameterLists::editToH5_SNK0
    !> @copydetails ParameterLists::editToH5_SNK0
    PROCEDURE,PASS,PRIVATE :: editToH5 => editToH5_SNK0
ENDTYPE Param_SNK0

!> @brief defines the parameter tree node data extension for Rank-1
TYPE,EXTENDS(Param_Base) :: Param_SNK1
  PRIVATE
  INTEGER(SNK),ALLOCATABLE :: p(:)
  CONTAINS
    !> @copybrief ParameterLists::editToText_SNK1
    !> @copydetails ParameterLists::editToText_SNK1
    PROCEDURE,PASS,PRIVATE :: editToText => editToText_SNK1
    !> @copybrief ParameterLists::editToH5_SNK1
    !> @copydetails ParameterLists::editToH5_SNK1
    PROCEDURE,PASS,PRIVATE :: editToH5 => editToH5_SNK1
ENDTYPE Param_SNK1

!> @brief defines the parameter tree node data extension for Rank-2
TYPE,EXTENDS(Param_Base) :: Param_SNK2
  PRIVATE
  INTEGER(SNK),ALLOCATABLE :: p(:,:)
  CONTAINS
    !> @copybrief ParameterLists::editToText_SNK2
    !> @copydetails ParameterLists::editToText_SNK2
    PROCEDURE,PASS,PRIVATE :: editToText => editToText_SNK2
    !> @copybrief ParameterLists::editToH5_SNK2
    !> @copydetails ParameterLists::editToH5_SNK2
    PROCEDURE,PASS,PRIVATE :: editToH5 => editToH5_SNK2
ENDTYPE Param_SNK2

!> @brief defines the parameter tree node data extension for Rank-3
TYPE,EXTENDS(Param_Base) :: Param_SNK3
  PRIVATE
  INTEGER(SNK),ALLOCATABLE :: p(:,:,:)
  CONTAINS
    !> @copybrief ParameterLists::editToText_SNK3
    !> @copydetails ParameterLists::editToText_SNK3
    PROCEDURE,PASS,PRIVATE :: editToText => editToText_SNK3
    !> @copybrief ParameterLists::editToH5_SNK3
    !> @copydetails ParameterLists::editToH5_SNK3
    PROCEDURE,PASS,PRIVATE :: editToH5 => editToH5_SNK3
ENDTYPE Param_SNK3

!> @brief defines the parameter tree node data extension for Rank-4
TYPE,EXTENDS(Param_Base) :: Param_SNK4
  PRIVATE
  INTEGER(SNK),ALLOCATABLE :: p(:,:,:,:)
  CONTAINS
    !> @copybrief ParameterLists::editToText_SNK4
    !> @copydetails ParameterLists::editToText_SNK4
    PROCEDURE,PASS,PRIVATE :: editToText => editToText_SNK4
    !> @copybrief ParameterLists::editToH5_SNK4
    !> @copydetails ParameterLists::editToH5_SNK4
    PROCEDURE,PASS,PRIVATE :: editToH5 => editToH5_SNK4
ENDTYPE Param_SNK4

!> @brief defines the parameter tree node data extension for Rank-5
TYPE,EXTENDS(Param_Base) :: Param_SNK5
  PRIVATE
  INTEGER(SNK),ALLOCATABLE :: p(:,:,:,:,:)
  CONTAINS
    !> @copybrief ParameterLists::editToText_SNK5
    !> @copydetails ParameterLists::editToText_SNK5
    PROCEDURE,PASS,PRIVATE :: editToText => editToText_SNK5
    !> @copybrief ParameterLists::editToH5_SNK5
    !> @copydetails ParameterLists::editToH5_SNK5
    PROCEDURE,PASS,PRIVATE :: editToH5 => editToH5_SNK5
ENDTYPE Param_SNK5

!> @brief defines the parameter tree node data extension for Rank-6
TYPE,EXTENDS(Param_Base) :: Param_SNK6
  PRIVATE
  INTEGER(SNK),ALLOCATABLE :: p(:,:,:,:,:,:)
  CONTAINS
    !> @copybrief ParameterLists::editToText_SNK6
    !> @copydetails ParameterLists::editToText_SNK6
    PROCEDURE,PASS,PRIVATE :: editToText => editToText_SNK6
    !> @copybrief ParameterLists::editToH5_SNK6
    !> @copydetails ParameterLists::editToH5_SNK6
    PROCEDURE,PASS,PRIVATE :: editToH5 => editToH5_SNK6
ENDTYPE Param_SNK6

!> @brief defines the parameter tree node data extension for Rank-7
TYPE,EXTENDS(Param_Base) :: Param_SNK7
  PRIVATE
  INTEGER(SNK),ALLOCATABLE :: p(:,:,:,:,:,:,:)
  CONTAINS
    !> @copybrief ParameterLists::editToText_SNK7
    !> @copydetails ParameterLists::editToText_SNK7
    PROCEDURE,PASS,PRIVATE :: editToText => editToText_SNK7
    !> @copybrief ParameterLists::editToH5_SNK7
    !> @copydetails ParameterLists::editToH5_SNK7
    PROCEDURE,PASS,PRIVATE :: editToH5 => editToH5_SNK7
ENDTYPE Param_SNK7

!> @brief defines the parameter tree node data extension for Rank-0
TYPE,EXTENDS(Param_Base) :: Param_SLK0
  PRIVATE
  INTEGER(SLK) :: p
  CONTAINS
    !> @copybrief ParameterLists::editToText_SLK0
    !> @copydetails ParameterLists::editToText_SLK0
    PROCEDURE,PASS,PRIVATE :: editToText => editToText_SLK0
    !> @copybrief ParameterLists::editToH5_SLK0
    !> @copydetails ParameterLists::editToH5_SLK0
    PROCEDURE,PASS,PRIVATE :: editToH5 => editToH5_SLK0
ENDTYPE Param_SLK0

!> @brief defines the parameter tree node data extension for Rank-1
TYPE,EXTENDS(Param_Base) :: Param_SLK1
  PRIVATE
  INTEGER(SLK),ALLOCATABLE :: p(:)
  CONTAINS
    !> @copybrief ParameterLists::editToText_SLK1
    !> @copydetails ParameterLists::editToText_SLK1
    PROCEDURE,PASS,PRIVATE :: editToText => editToText_SLK1
    !> @copybrief ParameterLists::editToH5_SLK1
    !> @copydetails ParameterLists::editToH5_SLK1
    PROCEDURE,PASS,PRIVATE :: editToH5 => editToH5_SLK1
ENDTYPE Param_SLK1

!> @brief defines the parameter tree node data extension for Rank-2
TYPE,EXTENDS(Param_Base) :: Param_SLK2
  PRIVATE
  INTEGER(SLK),ALLOCATABLE :: p(:,:)
  CONTAINS
    !> @copybrief ParameterLists::editToText_SLK2
    !> @copydetails ParameterLists::editToText_SLK2
    PROCEDURE,PASS,PRIVATE :: editToText => editToText_SLK2
    !> @copybrief ParameterLists::editToH5_SLK2
    !> @copydetails ParameterLists::editToH5_SLK2
    PROCEDURE,PASS,PRIVATE :: editToH5 => editToH5_SLK2
ENDTYPE Param_SLK2

!> @brief defines the parameter tree node data extension for Rank-3
TYPE,EXTENDS(Param_Base) :: Param_SLK3
  PRIVATE
  INTEGER(SLK),ALLOCATABLE :: p(:,:,:)
  CONTAINS
    !> @copybrief ParameterLists::editToText_SLK3
    !> @copydetails ParameterLists::editToText_SLK3
    PROCEDURE,PASS,PRIVATE :: editToText => editToText_SLK3
    !> @copybrief ParameterLists::editToH5_SLK3
    !> @copydetails ParameterLists::editToH5_SLK3
    PROCEDURE,PASS,PRIVATE :: editToH5 => editToH5_SLK3
ENDTYPE Param_SLK3

!> @brief defines the parameter tree node data extension for Rank-4
TYPE,EXTENDS(Param_Base) :: Param_SLK4
  PRIVATE
  INTEGER(SLK),ALLOCATABLE :: p(:,:,:,:)
  CONTAINS
    !> @copybrief ParameterLists::editToText_SLK4
    !> @copydetails ParameterLists::editToText_SLK4
    PROCEDURE,PASS,PRIVATE :: editToText => editToText_SLK4
    !> @copybrief ParameterLists::editToH5_SLK4
    !> @copydetails ParameterLists::editToH5_SLK4
    PROCEDURE,PASS,PRIVATE :: editToH5 => editToH5_SLK4
ENDTYPE Param_SLK4

!> @brief defines the parameter tree node data extension for Rank-5
TYPE,EXTENDS(Param_Base) :: Param_SLK5
  PRIVATE
  INTEGER(SLK),ALLOCATABLE :: p(:,:,:,:,:)
  CONTAINS
    !> @copybrief ParameterLists::editToText_SLK5
    !> @copydetails ParameterLists::editToText_SLK5
    PROCEDURE,PASS,PRIVATE :: editToText => editToText_SLK5
    !> @copybrief ParameterLists::editToH5_SLK5
    !> @copydetails ParameterLists::editToH5_SLK5
    PROCEDURE,PASS,PRIVATE :: editToH5 => editToH5_SLK5
ENDTYPE Param_SLK5

!> @brief defines the parameter tree node data extension for Rank-6
TYPE,EXTENDS(Param_Base) :: Param_SLK6
  PRIVATE
  INTEGER(SLK),ALLOCATABLE :: p(:,:,:,:,:,:)
  CONTAINS
    !> @copybrief ParameterLists::editToText_SLK6
    !> @copydetails ParameterLists::editToText_SLK6
    PROCEDURE,PASS,PRIVATE :: editToText => editToText_SLK6
    !> @copybrief ParameterLists::editToH5_SLK6
    !> @copydetails ParameterLists::editToH5_SLK6
    PROCEDURE,PASS,PRIVATE :: editToH5 => editToH5_SLK6
ENDTYPE Param_SLK6

!> @brief defines the parameter tree node data extension for Rank-7
TYPE,EXTENDS(Param_Base) :: Param_SLK7
  PRIVATE
  INTEGER(SLK),ALLOCATABLE :: p(:,:,:,:,:,:,:)
  CONTAINS
    !> @copybrief ParameterLists::editToText_SLK7
    !> @copydetails ParameterLists::editToText_SLK7
    PROCEDURE,PASS,PRIVATE :: editToText => editToText_SLK7
    !> @copybrief ParameterLists::editToH5_SLK7
    !> @copydetails ParameterLists::editToH5_SLK7
    PROCEDURE,PASS,PRIVATE :: editToH5 => editToH5_SLK7
ENDTYPE Param_SLK7

!> @brief defines the parameter tree node data extension for Rank-0
TYPE,EXTENDS(Param_Base) :: Param_SSK0
  PRIVATE
  REAL(SSK) :: p
  CONTAINS
    !> @copybrief ParameterLists::editToText_SSK0
    !> @copydetails ParameterLists::editToText_SSK0
    PROCEDURE,PASS,PRIVATE :: editToText => editToText_SSK0
    !> @copybrief ParameterLists::editToH5_SSK0
    !> @copydetails ParameterLists::editToH5_SSK0
    PROCEDURE,PASS,PRIVATE :: editToH5 => editToH5_SSK0
ENDTYPE Param_SSK0

!> @brief defines the parameter tree node data extension for Rank-1
TYPE,EXTENDS(Param_Base) :: Param_SSK1
  PRIVATE
  REAL(SSK),ALLOCATABLE :: p(:)
  CONTAINS
    !> @copybrief ParameterLists::editToText_SSK1
    !> @copydetails ParameterLists::editToText_SSK1
    PROCEDURE,PASS,PRIVATE :: editToText => editToText_SSK1
    !> @copybrief ParameterLists::editToH5_SSK1
    !> @copydetails ParameterLists::editToH5_SSK1
    PROCEDURE,PASS,PRIVATE :: editToH5 => editToH5_SSK1
ENDTYPE Param_SSK1

!> @brief defines the parameter tree node data extension for Rank-2
TYPE,EXTENDS(Param_Base) :: Param_SSK2
  PRIVATE
  REAL(SSK),ALLOCATABLE :: p(:,:)
  CONTAINS
    !> @copybrief ParameterLists::editToText_SSK2
    !> @copydetails ParameterLists::editToText_SSK2
    PROCEDURE,PASS,PRIVATE :: editToText => editToText_SSK2
    !> @copybrief ParameterLists::editToH5_SSK2
    !> @copydetails ParameterLists::editToH5_SSK2
    PROCEDURE,PASS,PRIVATE :: editToH5 => editToH5_SSK2
ENDTYPE Param_SSK2

!> @brief defines the parameter tree node data extension for Rank-3
TYPE,EXTENDS(Param_Base) :: Param_SSK3
  PRIVATE
  REAL(SSK),ALLOCATABLE :: p(:,:,:)
  CONTAINS
    !> @copybrief ParameterLists::editToText_SSK3
    !> @copydetails ParameterLists::editToText_SSK3
    PROCEDURE,PASS,PRIVATE :: editToText => editToText_SSK3
    !> @copybrief ParameterLists::editToH5_SSK3
    !> @copydetails ParameterLists::editToH5_SSK3
    PROCEDURE,PASS,PRIVATE :: editToH5 => editToH5_SSK3
ENDTYPE Param_SSK3

!> @brief defines the parameter tree node data extension for Rank-4
TYPE,EXTENDS(Param_Base) :: Param_SSK4
  PRIVATE
  REAL(SSK),ALLOCATABLE :: p(:,:,:,:)
  CONTAINS
    !> @copybrief ParameterLists::editToText_SSK4
    !> @copydetails ParameterLists::editToText_SSK4
    PROCEDURE,PASS,PRIVATE :: editToText => editToText_SSK4
    !> @copybrief ParameterLists::editToH5_SSK4
    !> @copydetails ParameterLists::editToH5_SSK4
    PROCEDURE,PASS,PRIVATE :: editToH5 => editToH5_SSK4
ENDTYPE Param_SSK4

!> @brief defines the parameter tree node data extension for Rank-5
TYPE,EXTENDS(Param_Base) :: Param_SSK5
  PRIVATE
  REAL(SSK),ALLOCATABLE :: p(:,:,:,:,:)
  CONTAINS
    !> @copybrief ParameterLists::editToText_SSK5
    !> @copydetails ParameterLists::editToText_SSK5
    PROCEDURE,PASS,PRIVATE :: editToText => editToText_SSK5
    !> @copybrief ParameterLists::editToH5_SSK5
    !> @copydetails ParameterLists::editToH5_SSK5
    PROCEDURE,PASS,PRIVATE :: editToH5 => editToH5_SSK5
ENDTYPE Param_SSK5

!> @brief defines the parameter tree node data extension for Rank-6
TYPE,EXTENDS(Param_Base) :: Param_SSK6
  PRIVATE
  REAL(SSK),ALLOCATABLE :: p(:,:,:,:,:,:)
  CONTAINS
    !> @copybrief ParameterLists::editToText_SSK6
    !> @copydetails ParameterLists::editToText_SSK6
    PROCEDURE,PASS,PRIVATE :: editToText => editToText_SSK6
    !> @copybrief ParameterLists::editToH5_SSK6
    !> @copydetails ParameterLists::editToH5_SSK6
    PROCEDURE,PASS,PRIVATE :: editToH5 => editToH5_SSK6
ENDTYPE Param_SSK6

!> @brief defines the parameter tree node data extension for Rank-7
TYPE,EXTENDS(Param_Base) :: Param_SSK7
  PRIVATE
  REAL(SSK),ALLOCATABLE :: p(:,:,:,:,:,:,:)
  CONTAINS
    !> @copybrief ParameterLists::editToText_SSK7
    !> @copydetails ParameterLists::editToText_SSK7
    PROCEDURE,PASS,PRIVATE :: editToText => editToText_SSK7
    !> @copybrief ParameterLists::editToH5_SSK7
    !> @copydetails ParameterLists::editToH5_SSK7
    PROCEDURE,PASS,PRIVATE :: editToH5 => editToH5_SSK7
ENDTYPE Param_SSK7

!> @brief defines the parameter tree node data extension for Rank-0
TYPE,EXTENDS(Param_Base) :: Param_SDK0
  PRIVATE
  REAL(SDK) :: p
  CONTAINS
    !> @copybrief ParameterLists::editToText_SDK0
    !> @copydetails ParameterLists::editToText_SDK0
    PROCEDURE,PASS,PRIVATE :: editToText => editToText_SDK0
    !> @copybrief ParameterLists::editToH5_SDK0
    !> @copydetails ParameterLists::editToH5_SDK0
    PROCEDURE,PASS,PRIVATE :: editToH5 => editToH5_SDK0
ENDTYPE Param_SDK0

!> @brief defines the parameter tree node data extension for Rank-1
TYPE,EXTENDS(Param_Base) :: Param_SDK1
  PRIVATE
  REAL(SDK),ALLOCATABLE :: p(:)
  CONTAINS
    !> @copybrief ParameterLists::editToText_SDK1
    !> @copydetails ParameterLists::editToText_SDK1
    PROCEDURE,PASS,PRIVATE :: editToText => editToText_SDK1
    !> @copybrief ParameterLists::editToH5_SDK1
    !> @copydetails ParameterLists::editToH5_SDK1
    PROCEDURE,PASS,PRIVATE :: editToH5 => editToH5_SDK1
ENDTYPE Param_SDK1

!> @brief defines the parameter tree node data extension for Rank-2
TYPE,EXTENDS(Param_Base) :: Param_SDK2
  PRIVATE
  REAL(SDK),ALLOCATABLE :: p(:,:)
  CONTAINS
    !> @copybrief ParameterLists::editToText_SDK2
    !> @copydetails ParameterLists::editToText_SDK2
    PROCEDURE,PASS,PRIVATE :: editToText => editToText_SDK2
    !> @copybrief ParameterLists::editToH5_SDK2
    !> @copydetails ParameterLists::editToH5_SDK2
    PROCEDURE,PASS,PRIVATE :: editToH5 => editToH5_SDK2
ENDTYPE Param_SDK2

!> @brief defines the parameter tree node data extension for Rank-3
TYPE,EXTENDS(Param_Base) :: Param_SDK3
  PRIVATE
  REAL(SDK),ALLOCATABLE :: p(:,:,:)
  CONTAINS
    !> @copybrief ParameterLists::editToText_SDK3
    !> @copydetails ParameterLists::editToText_SDK3
    PROCEDURE,PASS,PRIVATE :: editToText => editToText_SDK3
    !> @copybrief ParameterLists::editToH5_SDK3
    !> @copydetails ParameterLists::editToH5_SDK3
    PROCEDURE,PASS,PRIVATE :: editToH5 => editToH5_SDK3
ENDTYPE Param_SDK3

!> @brief defines the parameter tree node data extension for Rank-4
TYPE,EXTENDS(Param_Base) :: Param_SDK4
  PRIVATE
  REAL(SDK),ALLOCATABLE :: p(:,:,:,:)
  CONTAINS
    !> @copybrief ParameterLists::editToText_SDK4
    !> @copydetails ParameterLists::editToText_SDK4
    PROCEDURE,PASS,PRIVATE :: editToText => editToText_SDK4
    !> @copybrief ParameterLists::editToH5_SDK4
    !> @copydetails ParameterLists::editToH5_SDK4
    PROCEDURE,PASS,PRIVATE :: editToH5 => editToH5_SDK4
ENDTYPE Param_SDK4

!> @brief defines the parameter tree node data extension for Rank-5
TYPE,EXTENDS(Param_Base) :: Param_SDK5
  PRIVATE
  REAL(SDK),ALLOCATABLE :: p(:,:,:,:,:)
  CONTAINS
    !> @copybrief ParameterLists::editToText_SDK5
    !> @copydetails ParameterLists::editToText_SDK5
    PROCEDURE,PASS,PRIVATE :: editToText => editToText_SDK5
    !> @copybrief ParameterLists::editToH5_SDK5
    !> @copydetails ParameterLists::editToH5_SDK5
    PROCEDURE,PASS,PRIVATE :: editToH5 => editToH5_SDK5
ENDTYPE Param_SDK5

!> @brief defines the parameter tree node data extension for Rank-6
TYPE,EXTENDS(Param_Base) :: Param_SDK6
  PRIVATE
  REAL(SDK),ALLOCATABLE :: p(:,:,:,:,:,:)
  CONTAINS
    !> @copybrief ParameterLists::editToText_SDK6
    !> @copydetails ParameterLists::editToText_SDK6
    PROCEDURE,PASS,PRIVATE :: editToText => editToText_SDK6
    !> @copybrief ParameterLists::editToH5_SDK6
    !> @copydetails ParameterLists::editToH5_SDK6
    PROCEDURE,PASS,PRIVATE :: editToH5 => editToH5_SDK6
ENDTYPE Param_SDK6

!> @brief defines the parameter tree node data extension for Rank-7
TYPE,EXTENDS(Param_Base) :: Param_SDK7
  PRIVATE
  REAL(SDK),ALLOCATABLE :: p(:,:,:,:,:,:,:)
  CONTAINS
    !> @copybrief ParameterLists::editToText_SDK7
    !> @copydetails ParameterLists::editToText_SDK7
    PROCEDURE,PASS,PRIVATE :: editToText => editToText_SDK7
    !> @copybrief ParameterLists::editToH5_SDK7
    !> @copydetails ParameterLists::editToH5_SDK7
    PROCEDURE,PASS,PRIVATE :: editToH5 => editToH5_SDK7
ENDTYPE Param_SDK7

!> @brief defines the parameter tree node data extension for Rank-0
TYPE,EXTENDS(Param_Base) :: Param_STR0
  PRIVATE
  TYPE(StringType) :: p
  CONTAINS
    !> @copybrief ParameterLists::editToText_STR0
    !> @copydetails ParameterLists::editToText_STR0
    PROCEDURE,PASS,PRIVATE :: editToText => editToText_STR0
    !> @copybrief ParameterLists::editToH5_STR0
    !> @copydetails ParameterLists::editToH5_STR0
    PROCEDURE,PASS,PRIVATE :: editToH5 => editToH5_STR0
ENDTYPE Param_STR0

!> @brief defines the parameter tree node data extension for Rank-1
TYPE,EXTENDS(Param_Base) :: Param_STR1
  PRIVATE
  TYPE(StringType),ALLOCATABLE :: p(:)
  CONTAINS
    !> @copybrief ParameterLists::editToText_STR1
    !> @copydetails ParameterLists::editToText_STR1
    PROCEDURE,PASS,PRIVATE :: editToText => editToText_STR1
    !> @copybrief ParameterLists::editToH5_STR1
    !> @copydetails ParameterLists::editToH5_STR1
    PROCEDURE,PASS,PRIVATE :: editToH5 => editToH5_STR1
ENDTYPE Param_STR1

!> @brief defines the parameter tree node data extension for Rank-2
TYPE,EXTENDS(Param_Base) :: Param_STR2
  PRIVATE
  TYPE(StringType),ALLOCATABLE :: p(:,:)
  CONTAINS
    !> @copybrief ParameterLists::editToText_STR2
    !> @copydetails ParameterLists::editToText_STR2
    PROCEDURE,PASS,PRIVATE :: editToText => editToText_STR2
    !> @copybrief ParameterLists::editToH5_STR2
    !> @copydetails ParameterLists::editToH5_STR2
    PROCEDURE,PASS,PRIVATE :: editToH5 => editToH5_STR2
ENDTYPE Param_STR2

!> @brief defines the parameter tree node data extension for Rank-3
TYPE,EXTENDS(Param_Base) :: Param_STR3
  PRIVATE
  TYPE(StringType),ALLOCATABLE :: p(:,:,:)
  CONTAINS
    !> @copybrief ParameterLists::editToText_STR3
    !> @copydetails ParameterLists::editToText_STR3
    PROCEDURE,PASS,PRIVATE :: editToText => editToText_STR3
    !> @copybrief ParameterLists::editToH5_STR3
    !> @copydetails ParameterLists::editToH5_STR3
    PROCEDURE,PASS,PRIVATE :: editToH5 => editToH5_STR3
ENDTYPE Param_STR3

!> @brief defines the parameter tree node data extension for Rank-4
TYPE,EXTENDS(Param_Base) :: Param_STR4
  PRIVATE
  TYPE(StringType),ALLOCATABLE :: p(:,:,:,:)
  CONTAINS
    !> @copybrief ParameterLists::editToText_STR4
    !> @copydetails ParameterLists::editToText_STR4
    PROCEDURE,PASS,PRIVATE :: editToText => editToText_STR4
ENDTYPE Param_STR4

!> @brief defines the parameter tree node data extension for Rank-5
TYPE,EXTENDS(Param_Base) :: Param_STR5
  PRIVATE
  TYPE(StringType),ALLOCATABLE :: p(:,:,:,:,:)
  CONTAINS
    !> @copybrief ParameterLists::editToText_STR5
    !> @copydetails ParameterLists::editToText_STR5
    PROCEDURE,PASS,PRIVATE :: editToText => editToText_STR5
ENDTYPE Param_STR5

!> @brief defines the parameter tree node data extension for Rank-6
TYPE,EXTENDS(Param_Base) :: Param_STR6
  PRIVATE
  TYPE(StringType),ALLOCATABLE :: p(:,:,:,:,:,:)
  CONTAINS
    !> @copybrief ParameterLists::editToText_STR6
    !> @copydetails ParameterLists::editToText_STR6
    PROCEDURE,PASS,PRIVATE :: editToText => editToText_STR6
ENDTYPE Param_STR6

!> @brief defines the parameter tree node data extension for Rank-7
TYPE,EXTENDS(Param_Base) :: Param_STR7
  PRIVATE
  TYPE(StringType),ALLOCATABLE :: p(:,:,:,:,:,:,:)
  CONTAINS
    !> @copybrief ParameterLists::editToText_STR7
    !> @copydetails ParameterLists::editToText_STR7
    PROCEDURE,PASS,PRIVATE :: editToText => editToText_STR7
ENDTYPE Param_STR7

