PROGRAM testQuickSort
#include "UnitTest.h"
  USE UnitTest
  USE IntrType
  USE QuickSort

  IMPLICIT NONE
!
!Check the timer resolution
  CREATE_TEST('QUICKSORT')
!
  REGISTER_SUBTEST('qsort Integer',testIntQSort)
  REGISTER_SUBTEST('qsort Real',testRealQSort)

!  REGISTER_SUBTEST('Speed Test',testSpeed)
  FINALIZE_TEST()
!
!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
    SUBROUTINE testSpeed
      USE Times
      INTEGER(SIK),ALLOCATABLE :: A(:)
      INTEGER(SIK) :: n,i,j
      LOGICAL(SBK) :: bool
      TYPE(TimerType) :: testTimer

      n=100000000

    DO j=1,10
      n=10**j
      ALLOCATE(A(n))
      DO i=1,n
        A(i)=INT(RAND()*REAL(n*100),SIK)
      ENDDO

      CALL testTimer%tic()
      CALL qsort(A)
      CALL testTimer%toc()
      bool=.TRUE.
      DO i=1,n-1
        IF (A(i)>A(i+1)) THEN
          bool=.FALSE.
          EXIT
        ENDIF
      ENDDO
      ASSERT(bool,"qsort speed test")

      WRITE(*,*) n, testTimer%elapsedtime
      DEALLOCATE(A)
    ENDDO

    ENDSUBROUTINE
!
!-------------------------------------------------------------------------------
    SUBROUTINE testIntQSort
      LOGICAL(SBK) :: bool
      INTEGER(SIK) :: tmpintarray(10)

      tmpintarray(1)=-5
      tmpintarray(2)=100
      tmpintarray(3)=60
      tmpintarray(4)=10
      tmpintarray(5)=45
      tmpintarray(6)=-10
      tmpintarray(7)=20
      tmpintarray(8)=5
      tmpintarray(9)=-30
      tmpintarray(10)=20

      CALL qsort(tmpintarray)

      bool=ALL(tmpintarray == (/-30,-10,-5,5,10,20,20,45,60,100/))
      ASSERT(bool,'1-D integer array qsort')
    ENDSUBROUTINE
!
!-------------------------------------------------------------------------------
    SUBROUTINE testRealQSort
      LOGICAL(SBK) :: bool
      REAL(SRK) :: tmprealarray(10)

      tmprealarray(1)=100.0_SRK
      tmprealarray(2)=-5.0_SRK
      tmprealarray(3)=60.0_SRK
      tmprealarray(4)=10.0_SRK
      tmprealarray(5)=45.0_SRK
      tmprealarray(6)=-10.0_SRK
      tmprealarray(7)=20.0_SRK
      tmprealarray(8)=5.0_SRK
      tmprealarray(9)=-30.0_SRK
      tmprealarray(10)=20.0_SRK

      CALL qsort(tmprealarray)

      bool=ALL(tmprealarray .APPROXEQ. (/-30.0_SRK,-10.0_SRK,-5.0_SRK,5.0_SRK,10.0_SRK, &
        20.0_SRK,20.0_SRK,45.0_SRK,60.0_SRK,100.0_SRK/))
      ASSERT(bool,'1-D real array qsort')
    ENDSUBROUTINE
!
ENDPROGRAM testQuickSort
