PROGRAM testQuickSort
#include "UnitTest.h"
  USE UnitTest
  USE IntrType
  USE QuickSort

  IMPLICIT NONE

  LOGICAL(SBK) :: bool
  INTEGER(SIK) :: tmpintarray(10), l
!
!Check the timer resolution
  CREATE_TEST('QUICKSORT')
!
  COMPONENT_TEST('1-D Integer Array')
  tmpintarray(1)=-5
  tmpintarray(2)=100
  tmpintarray(3)=60
  tmpintarray(4)=10
  tmpintarray(5)=45
  tmpintarray(6)=-10
  tmpintarray(7)=20
  tmpintarray(8)=5
  tmpintarray(9)=-30
  tmpintarray(10)=15

  CALL partition_array(tmpintarray,5,l)

  ASSERT(l==8,'1-D array partition')
  FINFO() l, 8

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
  ASSERT(bool,'1-D array qsort')

  REGISTER_SUBTEST('Speed Test',testSpeed)
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
ENDPROGRAM testQuickSort
