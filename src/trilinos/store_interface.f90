MODULE store_interface
  USE ISO_C_BINDING

  PUBLIC :: MPACT_Trilinos_Init
  PUBLIC :: MPACT_Trilinos_Finalize
  PUBLIC :: ForPETRA_VecInit
  PUBLIC :: ForPETRA_VecSet
  PUBLIC :: ForPETRA_VecGet

!High level interfaces to setup and free data
  INTERFACE
    SUBROUTINE MPACT_Trilinos_Init() bind(C)
    ENDSUBROUTINE

    SUBROUTINE MPACT_Trilinos_Finalize() bind(C)
    ENDSUBROUTINE

!Vector Interfaces
    SUBROUTINE ForPETRA_VecInit(id,n,nlocal,comm) bind(C)
      IMPORT :: C_INT
      INTEGER(C_INT),INTENT(INOUT)    :: id
      INTEGER(C_INT),INTENT(IN),VALUE :: n
      INTEGER(C_INT),INTENT(IN),VALUE :: nlocal
      INTEGER(C_INT),INTENT(IN),VALUE :: comm
    ENDSUBROUTINE

    SUBROUTINE ForPETRA_VecSet(id,i,val) bind(C)
      IMPORT :: C_INT,C_DOUBLE
      INTEGER(C_INT),INTENT(IN),VALUE  :: id
      INTEGER(C_INT),INTENT(IN),VALUE  :: i
      REAL(C_DOUBLE),INTENT(IN),VALUE  :: val
    ENDSUBROUTINE

    SUBROUTINE ForPETRA_VecGet(id,i,val) bind(C)
      IMPORT :: C_INT,C_DOUBLE
      INTEGER(C_INT),INTENT(IN),VALUE  :: id
      INTEGER(C_INT),INTENT(IN),VALUE  :: i
      REAL(C_DOUBLE),INTENT(OUT)       :: val
    ENDSUBROUTINE
  ENDINTERFACE

  CONTAINS

  SUBROUTINE hello_world()
    WRITE(*,*) "Hello"
  ENDSUBROUTINE
ENDMODULE
