  !High level interfaces to setup and free data
  INTERFACE
    SUBROUTINE MPACT_Trilinos_Init() bind(C,NAME="MPACT_Trilinos_Init")
    ENDSUBROUTINE

    SUBROUTINE MPACT_Trilinos_Finalize() bind(C,NAME="MPACT_Trilinos_Finalize")
    ENDSUBROUTINE

!-------------------------------------------------------------------------------
! Vector Interfaces
!-------------------------------------------------------------------------------
    SUBROUTINE ForPETRA_VecInit(id,n,nlocal,comm) bind(C,NAME="ForPETRA_VecInit")
      IMPORT :: C_INT
      INTEGER(C_INT),INTENT(INOUT)    :: id
      INTEGER(C_INT),INTENT(IN),VALUE :: n
      INTEGER(C_INT),INTENT(IN),VALUE :: nlocal
      INTEGER(C_INT),INTENT(IN),VALUE :: comm
    ENDSUBROUTINE

    SUBROUTINE ForPETRA_VecDestroy(id) bind(C,NAME="ForPETRA_VecDestroy")
      IMPORT :: C_INT
      INTEGER(C_INT),INTENT(IN),VALUE :: id
    ENDSUBROUTINE

    SUBROUTINE ForPETRA_VecSet(id,i,val) bind(C,NAME="ForPETRA_VecSet")
      IMPORT :: C_INT,C_DOUBLE
      INTEGER(C_INT),INTENT(IN),VALUE  :: id
      INTEGER(C_INT),INTENT(IN),VALUE  :: i
      REAL(C_DOUBLE),INTENT(IN),VALUE  :: val
    ENDSUBROUTINE

    SUBROUTINE ForPETRA_VecSetAll(id,val) bind(C,NAME="ForPETRA_VecSetAll")
      IMPORT :: C_INT,C_DOUBLE
      INTEGER(C_INT),INTENT(IN),VALUE  :: id
      REAL(C_DOUBLE),INTENT(IN),VALUE  :: val
    ENDSUBROUTINE

    SUBROUTINE ForPETRA_VecGet(id,i,val) bind(C,NAME="ForPETRA_VecGet")
      IMPORT :: C_INT,C_DOUBLE
      INTEGER(C_INT),INTENT(IN),VALUE  :: id
      INTEGER(C_INT),INTENT(IN),VALUE  :: i
      REAL(C_DOUBLE),INTENT(OUT)       :: val
    ENDSUBROUTINE

    SUBROUTINE ForPETRA_VecCopy(id,idfrom) bind(C,NAME="ForPETRA_VecCopy")
      IMPORT :: C_INT
      INTEGER(C_INT),INTENT(IN),VALUE  :: id
      INTEGER(C_INT),INTENT(IN),VALUE  :: idfrom
    ENDSUBROUTINE

    SUBROUTINE ForPETRA_VecAXPY(id,idx,a,b) bind(C,NAME="ForPETRA_VecAXPY")
      IMPORT :: C_INT,C_DOUBLE
      INTEGER(C_INT),INTENT(IN),VALUE  :: id
      INTEGER(C_INT),INTENT(IN),VALUE  :: idx
      REAL(C_DOUBLE),INTENT(IN),VALUE  :: a
      REAL(C_DOUBLE),INTENT(IN),VALUE  :: b
    ENDSUBROUTINE

    SUBROUTINE ForPETRA_VecSum(id,val) bind(C,NAME="ForPETRA_VecSum")
      IMPORT :: C_INT,C_DOUBLE
      INTEGER(C_INT),INTENT(IN),VALUE  :: id
      REAL(C_DOUBLE),INTENT(OUT)       :: val
    ENDSUBROUTINE

    SUBROUTINE ForPETRA_VecNorm2(id,val) bind(C,NAME="ForPETRA_VecNorm2")
      IMPORT :: C_INT,C_DOUBLE
      INTEGER(C_INT),INTENT(IN),VALUE  :: id
      REAL(C_DOUBLE),INTENT(OUT)       :: val
    ENDSUBROUTINE

    SUBROUTINE ForPETRA_VecMax(id,val) bind(C,NAME="ForPETRA_VecMax")
      IMPORT :: C_INT,C_DOUBLE
      INTEGER(C_INT),INTENT(IN),VALUE  :: id
      REAL(C_DOUBLE),INTENT(OUT)       :: val
    ENDSUBROUTINE

    SUBROUTINE ForPETRA_VecScale(id,val) bind(C,NAME="ForPETRA_VecScale")
      IMPORT :: C_INT,C_DOUBLE
      INTEGER(C_INT),INTENT(IN),VALUE  :: id
      REAL(C_DOUBLE),INTENT(IN),VALUE  :: val
    ENDSUBROUTINE

    ! When calling from FORTRAN, need to append C_NULL_CHAR to name:
    !    "myfile.out" // c_null_char
    SUBROUTINE ForPETRA_VecEdit(id,name) bind(C,NAME="ForPETRA_VecEdit")
      IMPORT :: C_INT, C_CHAR
      INTEGER(C_INT),INTENT(IN),VALUE  :: id
      CHARACTER(KIND=C_CHAR),INTENT(IN),DIMENSION(*) :: name
    ENDSUBROUTINE

!-------------------------------------------------------------------------------
! Matrix Interfaces
!-------------------------------------------------------------------------------
    SUBROUTINE ForPETRA_MatInit(id,n,nlocal,rnnz,comm) bind(C,NAME="ForPETRA_MatInit")
      IMPORT :: C_INT
      INTEGER(C_INT),INTENT(INOUT)    :: id
      INTEGER(C_INT),INTENT(IN),VALUE :: n
      INTEGER(C_INT),INTENT(IN),VALUE :: nlocal
      INTEGER(C_INT),INTENT(IN),VALUE :: rnnz
      INTEGER(C_INT),INTENT(IN),VALUE :: comm
    ENDSUBROUTINE

    SUBROUTINE ForPETRA_MatDestroy(id) bind(C,NAME="ForPETRA_MatDestroy")
      IMPORT :: C_INT
      INTEGER(C_INT),INTENT(IN),VALUE        :: id
    ENDSUBROUTINE

    SUBROUTINE ForPETRA_MatReset(id) bind(C,NAME="ForPETRA_MatReset")
      IMPORT :: C_INT
      INTEGER(C_INT),INTENT(IN),VALUE        :: id
    ENDSUBROUTINE

    SUBROUTINE ForPETRA_MatSet(id,i,nnz,j,val) bind(C,NAME="ForPETRA_MatSet")
      IMPORT :: C_INT,C_DOUBLE
      INTEGER(C_INT),INTENT(IN),VALUE        :: id
      INTEGER(C_INT),INTENT(IN),VALUE        :: i
      INTEGER(C_INT),INTENT(IN),VALUE        :: nnz
      INTEGER(C_INT),DIMENSION(*),INTENT(IN) :: j
      REAL(C_DOUBLE),DIMENSION(*),INTENT(IN) :: val
    ENDSUBROUTINE

    SUBROUTINE ForPETRA_MatGet(id,i,j,val) bind(C,NAME="ForPETRA_MatGet")
      IMPORT :: C_INT,C_DOUBLE
      INTEGER(C_INT),INTENT(IN),VALUE  :: id
      INTEGER(C_INT),INTENT(IN),VALUE  :: i
      INTEGER(C_INT),INTENT(IN),VALUE  :: j
      REAL(C_DOUBLE),INTENT(OUT)       :: val
    ENDSUBROUTINE

    SUBROUTINE ForPETRA_MatAssemble(id) bind(C,NAME="ForPETRA_MatAssemble")
      IMPORT :: C_INT
      INTEGER(C_INT),INTENT(IN),VALUE  :: id
    ENDSUBROUTINE

    SUBROUTINE ForPETRA_MatMult(idA,trans,idX,idY) bind(C,NAME="ForPETRA_MatMult")
      IMPORT :: C_INT,C_BOOL
      INTEGER(C_INT),INTENT(IN),VALUE        :: idA
      LOGICAL(C_BOOL),INTENT(IN),VALUE       :: trans
      INTEGER(C_INT),INTENT(IN),VALUE        :: idX
      INTEGER(C_INT),INTENT(IN),VALUE        :: idY
    ENDSUBROUTINE

    ! When calling from FORTRAN, need to append C_NULL_CHAR to name:
    !    "myfile.out" // c_null_char
    SUBROUTINE ForPETRA_MatEdit(id,name) bind(C,NAME="ForPETRA_MatEdit")
      IMPORT :: C_INT,C_CHAR
      INTEGER(C_INT),INTENT(IN),VALUE  :: id
      CHARACTER(KIND=C_CHAR),INTENT(IN),DIMENSION(*) :: name
    ENDSUBROUTINE

!-------------------------------------------------------------------------------
! Anasazi Interfaces
!-------------------------------------------------------------------------------
    SUBROUTINE Anasazi_Init(id) bind(C,NAME="Anasazi_Init")
      IMPORT :: C_INT
      INTEGER(C_INT),INTENT(INOUT)    :: id
    ENDSUBROUTINE

    SUBROUTINE Anasazi_Destroy(id) bind(C,NAME="Anasazi_Destroy")
      IMPORT :: C_INT
      INTEGER(C_INT),INTENT(IN),VALUE  :: id
    ENDSUBROUTINE

    SUBROUTINE Anasazi_SetMat(id,idLHS,idRHS) bind(C,NAME="Anasazi_SetMat")
      IMPORT :: C_INT
      INTEGER(C_INT),INTENT(IN),VALUE  :: id
      INTEGER(C_INT),INTENT(IN),VALUE  :: idLHS
      INTEGER(C_INT),INTENT(IN),VALUE  :: idRHS
    ENDSUBROUTINE

    SUBROUTINE Anasazi_SetPC(id,idPC) bind(C,NAME="Anasazi_SetPC")
      IMPORT :: C_INT
      INTEGER(C_INT),INTENT(IN),VALUE  :: id
      INTEGER(C_INT),INTENT(IN),VALUE  :: idPC
    ENDSUBROUTINE

    SUBROUTINE Anasazi_SetX(id,idX) bind(C,NAME="Anasazi_SetX")
      IMPORT :: C_INT
      INTEGER(C_INT),INTENT(IN),VALUE  :: id
      INTEGER(C_INT),INTENT(IN),VALUE  :: idX
    ENDSUBROUTINE

    SUBROUTINE Anasazi_SetConvCrit(id,tol,maxit) bind(C,NAME="Anasazi_SetConvCrit")
      IMPORT :: C_INT,C_DOUBLE
      INTEGER(C_INT),INTENT(IN),VALUE  :: id
      REAL(C_DOUBLE),INTENT(IN),VALUE  :: tol
      INTEGER(C_INT),INTENT(IN),VALUE  :: maxit
    ENDSUBROUTINE

    SUBROUTINE Anasazi_Solve(id) bind(C,NAME="Anasazi_Solve")
      IMPORT :: C_INT
      INTEGER(C_INT),INTENT(IN),VALUE  :: id
    ENDSUBROUTINE

    SUBROUTINE Anasazi_GetEigenvalue(id,k) bind(C,NAME="Anasazi_GetEigenvalue")
      IMPORT :: C_INT,C_DOUBLE
      INTEGER(C_INT),INTENT(IN),VALUE  :: id
      REAL(C_DOUBLE),INTENT(OUT)       :: k
    ENDSUBROUTINE

    SUBROUTINE Anasazi_GetResid(id,resid) bind(C,NAME="Anasazi_GetResid")
      IMPORT :: C_INT,C_DOUBLE
      INTEGER(C_INT),INTENT(IN),VALUE  :: id
      REAL(C_DOUBLE),INTENT(OUT)       :: resid
    ENDSUBROUTINE

    SUBROUTINE Anasazi_GetIterationCount(id,niter) bind(C,NAME="Anasazi_GetIterationCount")
      IMPORT :: C_INT
      INTEGER(C_INT),INTENT(IN),VALUE  :: id
      INTEGER(C_INT),INTENT(OUT)       :: niter
    ENDSUBROUTINE

!-------------------------------------------------------------------------------
! Belos Interfaces
!-------------------------------------------------------------------------------
    SUBROUTINE Belos_Init(id) bind(C,NAME="Belos_Init")
      IMPORT :: C_INT
      INTEGER(C_INT),INTENT(INOUT)    :: id
    ENDSUBROUTINE

    SUBROUTINE Belos_Destroy(id) bind(C,NAME="Belos_Destroy")
      IMPORT :: C_INT
      INTEGER(C_INT),INTENT(IN),VALUE  :: id
    ENDSUBROUTINE

    SUBROUTINE Belos_SetMat(id,idA) bind(C,NAME="Belos_SetMat")
      IMPORT :: C_INT
      INTEGER(C_INT),INTENT(IN),VALUE  :: id
      INTEGER(C_INT),INTENT(IN),VALUE  :: idA
    ENDSUBROUTINE

    SUBROUTINE Belos_SetPC(id,idPC) bind(C,NAME="Belos_SetPC")
      IMPORT :: C_INT
      INTEGER(C_INT),INTENT(IN),VALUE  :: id
      INTEGER(C_INT),INTENT(IN),VALUE  :: idPC
    ENDSUBROUTINE

    SUBROUTINE Belos_SetX(id,idX) bind(C,NAME="Belos_SetX")
      IMPORT :: C_INT
      INTEGER(C_INT),INTENT(IN),VALUE  :: id
      INTEGER(C_INT),INTENT(IN),VALUE  :: idX
    ENDSUBROUTINE

    SUBROUTINE Belos_Setb(id,idb) bind(C,NAME="Belos_Setb")
      IMPORT :: C_INT
      INTEGER(C_INT),INTENT(IN),VALUE  :: id
      INTEGER(C_INT),INTENT(IN),VALUE  :: idb
    ENDSUBROUTINE

    SUBROUTINE Belos_SetConvCrit(id,tol,maxit) bind(C,NAME="Belos_SetConvCrit")
      IMPORT :: C_INT,C_DOUBLE
      INTEGER(C_INT),INTENT(IN),VALUE  :: id
      REAL(C_DOUBLE),INTENT(IN),VALUE  :: tol
      INTEGER(C_INT),INTENT(IN),VALUE  :: maxit
    ENDSUBROUTINE

    SUBROUTINE Belos_Solve(id) bind(C,NAME="Belos_Solve")
      IMPORT :: C_INT
      INTEGER(C_INT),INTENT(IN),VALUE  :: id
    ENDSUBROUTINE

    SUBROUTINE Belos_GetResid(id,resid) bind(C,NAME="Belos_GetResid")
      IMPORT :: C_INT,C_DOUBLE
      INTEGER(C_INT),INTENT(IN),VALUE  :: id
      REAL(C_DOUBLE),INTENT(OUT)       :: resid
    ENDSUBROUTINE

    SUBROUTINE Belos_GetIterationCount(id,niter) bind(C,NAME="Belos_GetIterationCount")
      IMPORT :: C_INT
      INTEGER(C_INT),INTENT(IN),VALUE  :: id
      INTEGER(C_INT),INTENT(OUT)       :: niter
    ENDSUBROUTINE

!-------------------------------------------------------------------------------
! Preconditioner Interfaces
!-------------------------------------------------------------------------------
    SUBROUTINE Preconditioner_Init(id,opt) bind(C,NAME="Preconditioner_Init")
      IMPORT :: C_INT
      INTEGER(C_INT),INTENT(INOUT)    :: id
      INTEGER(C_INT),INTENT(IN),VALUE :: opt
    ENDSUBROUTINE

    SUBROUTINE Preconditioner_Destroy(id) bind(C,NAME="Preconditioner_Destroy")
      IMPORT :: C_INT
      INTEGER(C_INT),INTENT(IN),VALUE :: id
    ENDSUBROUTINE

    SUBROUTINE Preconditioner_Setup(id,idM) bind(C,NAME="Preconditioner_Setup")
      IMPORT :: C_INT
      INTEGER(C_INT),INTENT(IN),VALUE  :: id
      INTEGER(C_INT),INTENT(IN),VALUE  :: idM
    ENDSUBROUTINE

!-------------------------------------------------------------------------------
! Anderson Interfaces
!-------------------------------------------------------------------------------
    SUBROUTINE Anderson_Init(id,depth,beta,start,idv) bind(C,NAME="Anderson_Init")
      IMPORT :: C_INT,C_DOUBLE
      INTEGER(C_INT),INTENT(INOUT)    :: id
      INTEGER(C_INT),INTENT(IN),VALUE :: depth
      REAL(C_DOUBLE),INTENT(IN),VALUE :: beta
      INTEGER(C_INT),INTENT(IN),VALUE :: start
      INTEGER(C_INT),INTENT(IN),VALUE :: idv
    ENDSUBROUTINE

    SUBROUTINE Anderson_Destroy(id) bind(C,NAME="Anderson_Destroy")
      IMPORT :: C_INT
      INTEGER(C_INT),INTENT(IN),VALUE :: id
    ENDSUBROUTINE

    SUBROUTINE Anderson_Update(id) bind(C,NAME="Anderson_Update")
      IMPORT :: C_INT
      INTEGER(C_INT),INTENT(IN),VALUE  :: id
    ENDSUBROUTINE

    SUBROUTINE Anderson_Reset(id) bind(C,NAME="Anderson_Reset")
      IMPORT :: C_INT
      INTEGER(C_INT),INTENT(IN),VALUE  :: id
    ENDSUBROUTINE

!-------------------------------------------------------------------------------
! JFNK Interfaces
!-------------------------------------------------------------------------------
    SUBROUTINE JFNK_Init(id,fptr,idx,idF) bind(C,NAME="JFNK_Init")
      IMPORT :: C_INT,C_DOUBLE,C_FUNPTR
      INTEGER(C_INT),INTENT(INOUT)    :: id
      TYPE(C_FUNPTR),INTENT(IN),VALUE :: fptr
      INTEGER(C_INT),INTENT(IN),VALUE :: idx
      INTEGER(C_INT),INTENT(IN),VALUE :: idF
    ENDSUBROUTINE

    SUBROUTINE JFNK_Destroy(id) bind(C,NAME="JFNK_Destroy")
      IMPORT :: C_INT
      INTEGER(C_INT),INTENT(IN),VALUE :: id
    ENDSUBROUTINE

    SUBROUTINE JFNK_Solve(id) bind(C,NAME="JFNK_Solve")
      IMPORT :: C_INT
      INTEGER(C_INT),INTENT(IN),VALUE  :: id
    ENDSUBROUTINE

  ENDINTERFACE