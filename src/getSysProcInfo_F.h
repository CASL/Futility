INTERFACE
  SUBROUTINE getSysMemInfo(totalPhysMem,totalSwap,totalFreeMem) &
    BIND(C,NAME="getSysMemInfo")
    USE ISO_C_BINDING
    INTEGER(C_LONG_LONG),INTENT(OUT) :: totalPhysMem
    INTEGER(C_LONG_LONG),INTENT(OUT) :: totalSwap
    INTEGER(C_LONG_LONG),INTENT(OUT) :: totalFreeMem
  ENDSUBROUTINE getSysMemInfo
  
  SUBROUTINE getProcMemInfo(curUsage,peakUsage) &
    BIND(C,NAME="getProcMemInfo")
    USE ISO_C_BINDING
    INTEGER(C_LONG_LONG),INTENT(OUT) :: curUsage
    INTEGER(C_LONG_LONG),INTENT(OUT) :: peakUsage
  ENDSUBROUTINE getProcMemInfo

  SUBROUTINE getPWD_c(pwdpath,pathlen,status) &
    BIND(C,NAME="getPWD_c")
    USE ISO_C_BINDING
    CHARACTER(KIND=C_CHAR,LEN=1),INTENT(IN) :: pwdpath(*)
    INTEGER(C_INT),INTENT(IN) :: pathlen
    INTEGER(C_INT),INTENT(OUT) :: status
  ENDSUBROUTINE getPWD_c
ENDINTERFACE
