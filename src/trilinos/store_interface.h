  !High level interfaces to setup and free data
  INTERFACE
    SUBROUTINE MPACT_Trilinos_Init() bind(C,NAME="MPACT_Trilinos_Init")
    ENDSUBROUTINE

    SUBROUTINE MPACT_Trilinos_Finalize() bind(C,NAME="MPACT_Trilinos_Finalize")
    ENDSUBROUTINE

!Vector Interfaces
    SUBROUTINE ForPETRA_VecInit(id,n,nlocal,comm) bind(C,NAME="ForPETRA_VecInit")
      IMPORT :: C_INT
      INTEGER(C_INT),INTENT(INOUT)    :: id
      INTEGER(C_INT),INTENT(IN),VALUE :: n
      INTEGER(C_INT),INTENT(IN),VALUE :: nlocal
      INTEGER(C_INT),INTENT(IN),VALUE :: comm
    ENDSUBROUTINE

    SUBROUTINE ForPETRA_VecSet(id,i,val) bind(C,NAME="ForPETRA_VecSet")
      IMPORT :: C_INT,C_DOUBLE
      INTEGER(C_INT),INTENT(IN),VALUE  :: id
      INTEGER(C_INT),INTENT(IN),VALUE  :: i
      REAL(C_DOUBLE),INTENT(IN),VALUE  :: val
    ENDSUBROUTINE

    SUBROUTINE ForPETRA_VecGet(id,i,val) bind(C,NAME="ForPETRA_VecGet")
      IMPORT :: C_INT,C_DOUBLE
      INTEGER(C_INT),INTENT(IN),VALUE  :: id
      INTEGER(C_INT),INTENT(IN),VALUE  :: i
      REAL(C_DOUBLE),INTENT(OUT)       :: val
    ENDSUBROUTINE

    SUBROUTINE ForPETRA_VecEdit(id) bind(C,NAME="ForPETRA_VecEdit")
      IMPORT :: C_INT
      INTEGER(C_INT),INTENT(IN),VALUE  :: id
    ENDSUBROUTINE

!Matrix Interfaces
    SUBROUTINE ForPETRA_MatInit(id,n,nlocal,rnnz,comm) bind(C,NAME="ForPETRA_MatInit")
      IMPORT :: C_INT
      INTEGER(C_INT),INTENT(INOUT)    :: id
      INTEGER(C_INT),INTENT(IN),VALUE :: n
      INTEGER(C_INT),INTENT(IN),VALUE :: nlocal
      INTEGER(C_INT),INTENT(IN),VALUE :: rnnz
      INTEGER(C_INT),INTENT(IN),VALUE :: comm
    ENDSUBROUTINE

    SUBROUTINE ForPETRA_MatSet(id,i,nnz,j,val) bind(C,NAME="ForPETRA_MatSet")
      IMPORT :: C_INT,C_DOUBLE
      INTEGER(C_INT),INTENT(IN),VALUE        :: id
      INTEGER(C_INT),INTENT(IN),VALUE        :: i
      INTEGER(C_INT),INTENT(IN),VALUE        :: nnz
      INTEGER(C_INT),DIMENSION(*),INTENT(IN) :: j
      REAL(C_DOUBLE),DIMENSION(*),INTENT(IN) :: val
    ENDSUBROUTINE

!    SUBROUTINE ForPETRA_MatGet(id,i,j,val) bind(C,NAME="ForPETRA_MatGet")
!      IMPORT :: C_INT,C_DOUBLE
!      INTEGER(C_INT),INTENT(IN),VALUE  :: id
!      INTEGER(C_INT),INTENT(IN),VALUE  :: i
!      INTEGER(C_INT),INTENT(IN),VALUE  :: j
!      REAL(C_DOUBLE),INTENT(OUT)       :: val
!    ENDSUBROUTINE

    SUBROUTINE ForPETRA_MatAssemble(id) bind(C,NAME="ForPETRA_MatAssemble")
      IMPORT :: C_INT
      INTEGER(C_INT),INTENT(IN),VALUE  :: id
    ENDSUBROUTINE

    SUBROUTINE ForPETRA_MatEdit(id) bind(C,NAME="ForPETRA_MatEdit")
      IMPORT :: C_INT
      INTEGER(C_INT),INTENT(IN),VALUE  :: id
    ENDSUBROUTINE

    SUBROUTINE ForPETRA_MatNormF(id,val) bind(C,NAME="ForPETRA_MatNormF")
      IMPORT :: C_INT,C_DOUBLE
      INTEGER(C_INT),INTENT(IN),VALUE  :: id
      REAL(C_DOUBLE),INTENT(OUT)       :: val
    ENDSUBROUTINE

    SUBROUTINE test_solve(idLHS,idRHS,idX) bind(C,NAME="test_solve")
      IMPORT :: C_INT
      INTEGER(C_INT),INTENT(IN),VALUE  :: idLHS
      INTEGER(C_INT),INTENT(IN),VALUE  :: idRHS
      INTEGER(C_INT),INTENT(IN),VALUE  :: idX
    ENDSUBROUTINE
  ENDINTERFACE