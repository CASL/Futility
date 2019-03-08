!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
PROGRAM testSorting
#include "UnitTest.h"
  USE UnitTest
  USE IntrType
  USE Strings
  USE Sorting

  IMPLICIT NONE
!
!Check the timer resolution
  CREATE_TEST('SORTING')
!
  REGISTER_SUBTEST('sort Integer',testIntSort)
  REGISTER_SUBTEST('sort Integer 2D',testInt2DSort)
  REGISTER_SUBTEST('sort Real',testRealSort)

  REGISTER_SUBTEST('bubble sort',testBubbleSort)
  REGISTER_SUBTEST('insertion sort',testInsertSort)

  REGISTER_SUBTEST('Speed Test - sort int',testSpeedInt)
  REGISTER_SUBTEST('Speed Test - sort real',testSpeedReal)

  REGISTER_SUBTEST('Diagonal QSort', testDiagSort)
  FINALIZE_TEST()
!
!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
    SUBROUTINE testSpeedInt()
      USE Times
      USE StochasticSampling
      INTEGER(SIK),ALLOCATABLE :: A(:)
      INTEGER(SIK) :: n,i,j
      LOGICAL(SBK) :: bool
      TYPE(TimerType) :: testTimer
      TYPE(StochasticSamplingType) :: myRNG

      CALL myRNG%init(RNG_LEcuyer2)
      DO j=1,7
        n=10**j
        ALLOCATE(A(n))
        DO i=1,n
          A(i)=INT(myRNG%rng()*REAL(n*100),SIK)
        ENDDO

        CALL testTimer%tic()
        CALL sort(A)
        CALL testTimer%toc()
        bool=.TRUE.
        DO i=1,n-1
          IF (A(i)>A(i+1)) THEN
            bool=.FALSE.
            EXIT
          ENDIF
        ENDDO
        ASSERT(bool,"qsort speed test - int")

        WRITE(*,*) n, testTimer%elapsedtime
        DEALLOCATE(A)
      ENDDO
      CALL myRNG%clear()
    ENDSUBROUTINE testSpeedInt
!
!-------------------------------------------------------------------------------
    SUBROUTINE testSpeedReal()
      USE Times
      USE StochasticSampling
      REAL(SRK),ALLOCATABLE :: A(:)
      INTEGER(SIK) :: n,i,j
      LOGICAL(SBK) :: bool
      TYPE(TimerType) :: testTimer
      TYPE(StochasticSamplingType) :: myRNG

      CALL myRNG%init(RNG_LEcuyer2)
      DO j=1,7
        n=10**j
        ALLOCATE(A(n))
        DO i=1,n
          A(i)=myRNG%rng()*REAL(n*100,SRK)
        ENDDO

        CALL testTimer%tic()
        CALL sort(A)
        CALL testTimer%toc()
        bool=.TRUE.
        DO i=1,n-1
          IF (A(i)>A(i+1)) THEN
            bool=.FALSE.
            EXIT
          ENDIF
        ENDDO
        ASSERT(bool,"qsort speed test - real")

        WRITE(*,*) n, testTimer%elapsedtime
        DEALLOCATE(A)
      ENDDO
      CALL myRNG%clear()
    ENDSUBROUTINE testSpeedReal
!
!-------------------------------------------------------------------------------
    SUBROUTINE testBubbleSort()
      LOGICAL(SBK) :: bool
      INTEGER(SIK) :: tmpintarray(10),tmpint1a(10)
      REAL(SRK) :: tmprealarray(10),tmpreal1a(10)
      TYPE(StringType) :: tmpstrarray(10)

      !Real sorting
      COMPONENT_TEST('Sort 1-D Real array')

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

      CALL bubble_sort(tmprealarray)

      bool=ALL(tmprealarray .APPROXEQ. (/-30.0_SRK,-10.0_SRK,-5.0_SRK,5.0_SRK,10.0_SRK, &
        20.0_SRK,20.0_SRK,45.0_SRK,60.0_SRK,100.0_SRK/))
      ASSERT(bool,'1-D real array')

      COMPONENT_TEST('Sort 1-D Real + 1-D Real array')

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
      tmpreal1a(1)=10.0_SRK
      tmpreal1a(2)=1.0_SRK
      tmpreal1a(3)=15.0_SRK
      tmpreal1a(4)=-5.0_SRK
      tmpreal1a(5)=-12.0_SRK
      tmpreal1a(6)=0.0_SRK
      tmpreal1a(7)=100.0_SRK
      tmpreal1a(8)=45.0_SRK
      tmpreal1a(9)=3.0_SRK
      tmpreal1a(10)=75.0_SRK

      CALL bubble_sort(tmprealarray,tmpreal1a)

      bool=ALL(tmprealarray .APPROXEQ. (/-30.0_SRK,-10.0_SRK,-5.0_SRK,5.0_SRK,10.0_SRK, &
        20.0_SRK,20.0_SRK,45.0_SRK,60.0_SRK,100.0_SRK/))
      ASSERT(bool,'1-D real array')

      bool=ALL(tmpreal1a .APPROXEQ. (/3.0_SRK,0.0_SRK,1.0_SRK,45.0_SRK,-5.0_SRK, &
        100.0_SRK,75.0_SRK,-12.0_SRK,15.0_SRK,10.0_SRK/))
      ASSERT(bool,'1-D real array')

      !Reverse the argument order for a new result
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
      tmpreal1a(1)=10.0_SRK
      tmpreal1a(2)=1.0_SRK
      tmpreal1a(3)=15.0_SRK
      tmpreal1a(4)=-5.0_SRK
      tmpreal1a(5)=-12.0_SRK
      tmpreal1a(6)=0.0_SRK
      tmpreal1a(7)=100.0_SRK
      tmpreal1a(8)=45.0_SRK
      tmpreal1a(9)=3.0_SRK
      tmpreal1a(10)=75.0_SRK

      CALL bubble_sort(tmpreal1a,tmprealarray)

      bool=ALL(tmpreal1a .APPROXEQ. (/-12.0_SRK,-5.0_SRK,0.0_SRK,1.0_SRK,3.0_SRK, &
        10.0_SRK,15.0_SRK,45.0_SRK,75.0_SRK,100.0_SRK/))
      ASSERT(bool,'1-D real array')

      bool=ALL(tmprealarray .APPROXEQ. (/45.0_SRK,10.0_SRK,-10.0_SRK,-5.0_SRK,-30.0_SRK, &
        100.0_SRK,60.0_SRK,5.0_SRK,20.0_SRK,20.0_SRK/))
      ASSERT(bool,'1-D real array')

      COMPONENT_TEST('Sort 1-D Real + 1-D Int array')

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
      tmpint1a(1)=10
      tmpint1a(2)=1
      tmpint1a(3)=15
      tmpint1a(4)=-5
      tmpint1a(5)=-12
      tmpint1a(6)=0
      tmpint1a(7)=100
      tmpint1a(8)=45
      tmpint1a(9)=3
      tmpint1a(10)=75

      CALL bubble_sort(tmprealarray,tmpint1a,.TRUE.)

      bool=ALL(tmprealarray .APPROXEQ. (/-30.0_SRK,-10.0_SRK,-5.0_SRK,5.0_SRK,10.0_SRK, &
        20.0_SRK,20.0_SRK,45.0_SRK,60.0_SRK,100.0_SRK/))
      ASSERT(bool,'1-D real array')

      bool=ALL(tmpint1a == (/3,0,1,45,-5,100,75,-12,15,10/))
      ASSERT(bool,'1-D int array')

      COMPONENT_TEST('Sort 1-D Real + 1-D String array')

      tmpreal1a(1)=9.0_SRK
      tmpreal1a(2)=3.0_SRK
      tmpreal1a(3)=5.0_SRK
      tmpreal1a(4)=0.0_SRK
      tmpreal1a(5)=4.0_SRK
      tmpreal1a(6)=1.0_SRK
      tmpreal1a(7)=2.0_SRK
      tmpreal1a(8)=8.0_SRK
      tmpreal1a(9)=7.0_SRK
      tmpreal1a(10)=6.0_SRK
      tmpstrarray(1)='g'
      tmpstrarray(2)='!'
      tmpstrarray(3)='and'
      tmpstrarray(4)='or'
      tmpstrarray(5)='da'
      tmpstrarray(6)='yes'
      tmpstrarray(7)='si'
      tmpstrarray(8)='oui'
      tmpstrarray(9)='string'
      tmpstrarray(10)=''

      CALL bubble_sort(tmpreal1a,tmpstrarray)

      bool=ALL(tmpreal1a == (/0.0_SRK,1.0_SRK,2.0_SRK,3.0_SRK,4.0_SRK,5.0_SRK,6.0_SRK,7.0_SRK,8.0_SRK,9.0_SRK/))
      ASSERT(bool,'1-D real array')

      ASSERT(tmpstrarray(1) == 'or','1-D string array 1')
      ASSERT(tmpstrarray(2) == 'yes','1-D string array 1')
      ASSERT(tmpstrarray(3) == 'si','1-D string array 1')
      ASSERT(tmpstrarray(4) == '!','1-D string array 1')
      ASSERT(tmpstrarray(5) == 'da','1-D string array 1')
      ASSERT(tmpstrarray(6) == 'and','1-D string array 1')
      ASSERT(tmpstrarray(7) == '','1-D string array 1')
      ASSERT(tmpstrarray(8) == 'string','1-D string array 1')
      ASSERT(tmpstrarray(9) == 'oui','1-D string array 1')
      ASSERT(tmpstrarray(10) == 'g','1-D string array 1')

      !Integer sorting
      COMPONENT_TEST('Sort 1-D Int array')
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

      CALL bubble_sort(tmpintarray)

      bool=ALL(tmpintarray == (/-30,-10,-5,5,10,20,20,45,60,100/))
      ASSERT(bool,'1-D integer array')

      COMPONENT_TEST('Sort 1-D Int + 1-D Real array')

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

      CALL bubble_sort(tmprealarray,tmpintarray,.FALSE.)

      bool=ALL(tmpintarray == (/-30,-10,-5,5,10,20,20,45,60,100/))
      ASSERT(bool,'1-D integer array')

      bool=ALL(tmprealarray .APPROXEQA. (/-30.0_SRK,-10.0_SRK,100.0_SRK,5.0_SRK, &
         10.0_SRK,20.0_SRK,20.0_SRK,45.0_SRK,60.0_SRK,-5.0_SRK/))
      ASSERT(bool,'1-D real array')

      COMPONENT_TEST('Sort 1-D Int + 1-D Int array')
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
      tmpint1a(1)=5
      tmpint1a(2)=8
      tmpint1a(3)=6
      tmpint1a(4)=2
      tmpint1a(5)=0
      tmpint1a(6)=4
      tmpint1a(7)=9
      tmpint1a(8)=7
      tmpint1a(9)=1
      tmpint1a(10)=3

      CALL bubble_sort(tmpintarray,tmpint1a)

      bool=ALL(tmpintarray == (/-30,-10,-5,5,10,20,20,45,60,100/))
      ASSERT(bool,'1-D integer array')

      bool=ALL(tmpint1a == (/1,4,5,7,2,9,3,0,6,8/))
      ASSERT(bool,'1-D integer array')

      !Reverse the argument order for new results
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
      tmpint1a(1)=5
      tmpint1a(2)=8
      tmpint1a(3)=6
      tmpint1a(4)=2
      tmpint1a(5)=0
      tmpint1a(6)=4
      tmpint1a(7)=9
      tmpint1a(8)=7
      tmpint1a(9)=1
      tmpint1a(10)=3

      CALL bubble_sort(tmpint1a,tmpintarray)

      bool=ALL(tmpint1a == (/0,1,2,3,4,5,6,7,8,9/))
      ASSERT(bool,'1-D integer array')

      bool=ALL(tmpintarray == (/45,-30,10,20,-10,-5,60,5,100,20/))
      ASSERT(bool,'1-D integer array')

      COMPONENT_TEST('Sort 1-D Int + 1-D String array')

      tmpint1a(1)=5
      tmpint1a(2)=8
      tmpint1a(3)=6
      tmpint1a(4)=2
      tmpint1a(5)=0
      tmpint1a(6)=4
      tmpint1a(7)=9
      tmpint1a(8)=7
      tmpint1a(9)=1
      tmpint1a(10)=3
      tmpstrarray(1)='g'
      tmpstrarray(2)='!'
      tmpstrarray(3)='and'
      tmpstrarray(4)='or'
      tmpstrarray(5)='da'
      tmpstrarray(6)='yes'
      tmpstrarray(7)='si'
      tmpstrarray(8)='oui'
      tmpstrarray(9)='string'
      tmpstrarray(10)=''


      CALL bubble_sort(tmpint1a,tmpstrarray)

      bool=ALL(tmpint1a == (/0,1,2,3,4,5,6,7,8,9/))
      ASSERT(bool,'1-D integer array')

      ASSERT(tmpstrarray(1) == 'da','1-D string array 1')
      ASSERT(tmpstrarray(2) == 'string','1-D string array 1')
      ASSERT(tmpstrarray(3) == 'or','1-D string array 1')
      ASSERT(tmpstrarray(4) == '','1-D string array 1')
      ASSERT(tmpstrarray(5) == 'yes','1-D string array 1')
      ASSERT(tmpstrarray(6) == 'g','1-D string array 1')
      ASSERT(tmpstrarray(7) == 'and','1-D string array 1')
      ASSERT(tmpstrarray(8) == 'oui','1-D string array 1')
      ASSERT(tmpstrarray(9) == '!','1-D string array 1')
      ASSERT(tmpstrarray(10) == 'si','1-D string array 1')
    ENDSUBROUTINE testBubbleSort
!
!-------------------------------------------------------------------------------
    SUBROUTINE testInsertSort()
      LOGICAL(SBK) :: bool
      INTEGER(SIK) :: tmpintarray(10)
      REAL(SRK) :: tmprealarray(10)

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

      CALL insert_sort(tmpintarray)

      bool=ALL(tmpintarray == (/-30,-10,-5,5,10,20,20,45,60,100/))
      ASSERT(bool,'1-D integer array')

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

      CALL insert_sort(tmprealarray)

      bool=ALL(tmprealarray .APPROXEQ. (/-30.0_SRK,-10.0_SRK,-5.0_SRK,5.0_SRK,10.0_SRK, &
        20.0_SRK,20.0_SRK,45.0_SRK,60.0_SRK,100.0_SRK/))
      ASSERT(bool,'1-D real array')
    ENDSUBROUTINE testInsertSort
!
!-------------------------------------------------------------------------------
    SUBROUTINE testIntSort()
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

      CALL sort(tmpintarray)

      bool=ALL(tmpintarray == (/-30,-10,-5,5,10,20,20,45,60,100/))
      ASSERT(bool,'1-D integer array qsort')
    ENDSUBROUTINE testIntSort
!
!-------------------------------------------------------------------------------
    SUBROUTINE testInt2DSort()
      LOGICAL(SBK) :: bool
      INTEGER(SIK) :: tmpintarray2(5,5)

      tmpintarray2(1,1)=-5
      tmpintarray2(2,1)=100
      tmpintarray2(3,1)=60
      tmpintarray2(4,1)=10
      tmpintarray2(5,1)=45
      tmpintarray2(1,2)=-10
      tmpintarray2(2,2)=20
      tmpintarray2(3,2)=5
      tmpintarray2(4,2)=-30
      tmpintarray2(5,2)=20
      tmpintarray2(1,3)=0
      tmpintarray2(2,3)=-20
      tmpintarray2(3,3)=-15
      tmpintarray2(4,3)=30
      tmpintarray2(5,3)=21
      tmpintarray2(1,4)=4
      tmpintarray2(2,4)=-16
      tmpintarray2(3,4)=-50
      tmpintarray2(4,4)=-30
      tmpintarray2(5,4)=50
      tmpintarray2(1,5)=53
      tmpintarray2(2,5)=77
      tmpintarray2(3,5)=88
      tmpintarray2(4,5)=99
      tmpintarray2(5,5)=35

      CALL sort(tmpintarray2)
      bool=ALL(tmpintarray2 == RESHAPE((/-50,-30,-30,-20,-16,-15,-10,-5,0,4,5,10,20,20,21,30,35,45,50,53,60,77,88,99,100/),(/5,5/)))
      ASSERT(bool,'2-D array sort')
    ENDSUBROUTINE testInt2DSort
!
!-------------------------------------------------------------------------------
    SUBROUTINE testRealSort()
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

      CALL sort(tmprealarray)

      bool=ALL(tmprealarray .APPROXEQ. (/-30.0_SRK,-10.0_SRK,-5.0_SRK,5.0_SRK,10.0_SRK, &
        20.0_SRK,20.0_SRK,45.0_SRK,60.0_SRK,100.0_SRK/))
      ASSERT(bool,'1-D real array qsort')
    ENDSUBROUTINE testRealSort

    SUBROUTINE testDiagSort()
      LOGICAL(SBK) :: bool
      INTEGER(SIK) :: iVals(16), jVals(16), diagRank(16), i
      REAL(SRK) :: values(16)

      ! 9  5  2  0
      ! 12 8  4  1
      ! 14 11 7  3
      ! 15 13 10 6

      DO i=1,16
        jVals(i) = MOD(i-1,4)+1
        iVals(i) = (i-1)/4 + 1
      END DO
      diagRank = jVals + ((jVals - iVals)*(jVals - iVals) &
                 + 7*(jVals - iVals))/2

      values(1) = 9.0_SRK
      values(2) = 5.0_SRK
      values(3) = 2.0_SRK
      values(4) = 0.0_SRK
      values(5) = 12.0_SRK
      values(6) = 8.0_SRK
      values(7) = 4.0_SRK
      values(8) = 1.0_SRK
      values(9) = 14.0_SRK
      values(10) = 11.0_SRK
      values(11) = 7.0_SRK
      values(12) = 3.0_SRK
      values(13) = 15.0_SRK
      values(14) = 13.0_SRK
      values(15) = 10.0_SRK
      values(16) = 6.0_SRK

      CALL diagonal_sort(diagRank, iVals, jVals, values)

      bool=ALL(iVals .EQ. (/4_SIK,3_SIK,4_SIK,2_SIK,3_SIK,4_SIK,1_SIK,2_SIK,3_SIK,4_SIK,1_SIK,2_SIK,3_SIK,1_SIK,2_SIK,1_SIK/))
      ASSERT(bool,'Diagonal sort i values')

      bool=ALL(jVals .EQ. (/1_SIK,1_SIK,2_SIK,1_SIK,2_SIK,3_SIK,1_SIK,2_SIK,3_SIK,4_SIK,2_SIK,3_SIK,4_SIK,3_SIK,4_SIK,4_SIK/))
      ASSERT(bool,'Diagonal sort j values')

      bool=ALL(values .APPROXEQ. (/15._SRK,14._SRK,13._SRK,12._SRK,11._SRK,10._SRK,9._SRK,8._SRK,7._SRK,6._SRK,5._SRK,4._SRK,3._SRK,2._SRK,1._SRK,0._SRK/))
      ASSERT(bool,'Diagonal sort entries')

    END SUBROUTINE testDiagSort
!
ENDPROGRAM testSorting
