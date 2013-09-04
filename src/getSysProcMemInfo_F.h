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
ENDINTERFACE
