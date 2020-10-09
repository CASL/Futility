!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
PROGRAM testArrayUtils
#include "UnitTest.h"
USE UnitTest
USE IntrType
USE Strings
USE ArrayUtils
USE Sorting

IMPLICIT NONE

LOGICAL(SBK) :: bool
REAL(SRK) :: tmprealarray(10)
REAL(SRK),ALLOCATABLE :: tmpr(:)
INTEGER(SIK) :: tmpintarray(10),i
INTEGER(SIK),ALLOCATABLE :: tmpi(:)
TYPE(StringType) :: tmpstr1a(10),tmpstr2a(2,2),str2a(2,3)
TYPE(StringType),ALLOCATABLE :: tmps1(:),tmps2(:,:)
!
!Check the timer resolution
CREATE_TEST('ArrayUtils')
!
REGISTER_SUBTEST('1-D REALS',test1DReals)
REGISTER_SUBTEST('1-D INTEGERS',test1DInts)
REGISTER_SUBTEST('1-D Strings',test1DStrings)
REGISTER_SUBTEST('2-D Strings',test2DStrings)
!REGISTER_SUBTEST('2-D INTEGERS',test2DInts)
REGISTER_SUBTEST('ReplaceEntry',testReplaceEntry)

FINALIZE_TEST()
!
!===============================================================================
CONTAINS
!
!-------------------------------------------------------------------------------
SUBROUTINE test1DReals()
  !
  COMPONENT_TEST('getAbsolute 1-D Array')
  tmprealarray(1)=0.0_SRK !0.0
  tmprealarray(2)=2.0_SRK !2.0
  tmprealarray(3)=4.0_SRK !6.0
  tmprealarray(4)=4.0_SRK !10.0
  tmprealarray(5)=5.0_SRK !15.0
  tmprealarray(6)=5.0_SRK !20.0
  tmprealarray(7)=20.0_SRK !40.0
  tmprealarray(8)=20.0_SRK !60.0
  tmprealarray(9)=5.0_SRK  !65.0
  tmprealarray(10)=35.0_SRK !100.0
  CALL getAbsolute(tmprealarray,tmpr)
  bool=ALL(tmpr .APPROXEQA. (/0.0_SRK,0.0_SRK,2.0_SRK,6.0_SRK,10.0_SRK, &
      15.0_SRK,20.0_SRK,40.0_SRK,60.0_SRK,65.0_SRK,100.0_SRK/))
  ASSERT(bool,'getAbsolute, no XI')
  CALL getAbsolute(tmprealarray(2:10),tmpr,XI=0.0_SRK)
  bool=ALL(tmpr .APPROXEQA. (/0.0_SRK,2.0_SRK,6.0_SRK,10.0_SRK,15.0_SRK, &
      20.0_SRK,40.0_SRK,60.0_SRK,65.0_SRK,100.0_SRK/))
  ASSERT(bool,'getAbsolute, with XI')
  CALL getAbsolute(tmprealarray(2:10),tmpr,XI=-10.0_SRK)
  bool=ALL(tmpr .APPROXEQA. (/-10.0_SRK,-8.0_SRK,-4.0_SRK,0.0_SRK,5.0_SRK, &
      10.0_SRK,30.0_SRK,50.0_SRK,55.0_SRK,90.0_SRK/))
  ASSERT(bool,'getAbsolute, with negative XI')
  !
  COMPONENT_TEST('getDelta 1-D Array')
  tmprealarray(1)=0.0_SRK
  tmprealarray(2)=2.0_SRK
  tmprealarray(3)=6.0_SRK
  tmprealarray(4)=10.0_SRK
  tmprealarray(5)=15.0_SRK
  tmprealarray(6)=20.0_SRK
  tmprealarray(7)=40.0_SRK
  tmprealarray(8)=60.0_SRK
  tmprealarray(9)=65.0_SRK
  tmprealarray(10)=100.0_SRK
  CALL getDelta(tmprealarray,tmpr)
  bool=ALL(tmpr .APPROXEQA. (/2.0_SRK,4.0_SRK,4.0_SRK,5.0_SRK,5.0_SRK,20.0_SRK,20.0_SRK,5.0_SRK,35.0_SRK/))
  ASSERT(bool,'getDelta, no XI')
  !Is this the behavior we want?
  CALL getDelta(tmprealarray(2:10),tmpr,XI=0.0_SRK)
  bool=ALL(tmpr .APPROXEQA. (/2.0_SRK,4.0_SRK,4.0_SRK,5.0_SRK,5.0_SRK,20.0_SRK,20.0_SRK,5.0_SRK,35.0_SRK/))
  ASSERT(bool,'getDelta, with XI')
  CALL getDelta(tmprealarray(2:10),tmpr,XI=-10.0_SRK)
  bool=ALL(tmpr .APPROXEQA. (/12.0_SRK,4.0_SRK,4.0_SRK,5.0_SRK,5.0_SRK,20.0_SRK,20.0_SRK,5.0_SRK,35.0_SRK/))
  ASSERT(bool,'getDelta, with negative XI')
  !
  COMPONENT_TEST('getUnion 1-D Array')

  CALL getUnion((/65.0_SRK,2.0_SRK,10.0_SRK,0.0_SRK/), &
      (/6.0_SRK,40.0_SRK,15.0_SRK,60.0_SRK,20.0_SRK,100.0_SRK/),tmpr)
  ASSERT(ALL(tmpr .APPROXEQA. tmprealarray),'getUnion no deltaout')
  CALL getUnion((/65.0_SRK,2.0_SRK,10.0_SRK,0.0_SRK,0.0000000000000001_SRK,2.0_SRK/), &
      (/6.0_SRK,40.0_SRK,15.0_SRK,60.0_SRK,20.0_SRK,100.0_SRK,6.0_SRK/),tmpr, &
      DELTA1=.FALSE.,DELTA2=.FALSE.,DELTAOUT=.FALSE.)
  ASSERT(ALL(tmpr .APPROXEQA. tmprealarray),'getUnion deltaout=.FALSE.')
  CALL getUnion((/65.0_SRK,2.0_SRK,10.0_SRK,0.0_SRK,0.0000000000000001_SRK,2.0_SRK/), &
      (/6.0_SRK,40.0_SRK,15.0_SRK,60.0_SRK,20.0_SRK,100.0_SRK,6.0_SRK/),tmpr,DELTAOUT=.TRUE.)
  bool=ALL(tmpr .APPROXEQA. (/2.0_SRK,4.0_SRK,4.0_SRK,5.0_SRK,5.0_SRK,20.0_SRK,20.0_SRK,5.0_SRK,35.0_SRK/))
  ASSERT(bool,'getUnion deltaout=.TRUE.')
  CALL getUnion((/65.0_SRK,2.0_SRK,10.0_SRK,0.0_SRK,0.0000000000000001_SRK,2.0_SRK/), &
      (/6.0_SRK,40.0_SRK,15.0_SRK,60.0_SRK,20.0_SRK,100.0_SRK,6.0_SRK/),tmpr,DELTAOUT=.TRUE.,TOL=1.0E-15_SRK)
  bool=ALL(tmpr .APPROXEQA. (/2.0_SRK,4.0_SRK,4.0_SRK,5.0_SRK,5.0_SRK,20.0_SRK,20.0_SRK,5.0_SRK,35.0_SRK/))
  ASSERT(bool,'getUnion deltaout=.TRUE.')
  CALL getUnion((/65.0_SRK,2.0_SRK,10.0_SRK,0.0_SRK,0.0000000000000001_SRK,2.0_SRK/), &
      (/6.0_SRK,40.0_SRK,15.0_SRK,60.0_SRK,20.0_SRK,100.0_SRK,6.0_SRK/),tmpr,DELTAOUT=.TRUE.,TOL=1.0E-08_SRK)
  bool=ALL(tmpr .APPROXEQA. (/2.0_SRK,4.0_SRK,4.0_SRK,5.0_SRK,5.0_SRK,20.0_SRK,20.0_SRK,5.0_SRK,35.0_SRK/))
  ASSERT(bool,'getUnion deltaout=.TRUE.')
  CALL getUnion((/65.0_SRK,2.0_SRK,10.0_SRK,0.0_SRK,0.0000000000000001_SRK,0.0000000001_SRK,2.0_SRK/), &
      (/6.0_SRK,40.0_SRK,15.0_SRK,60.0_SRK,20.0_SRK,100.0_SRK,6.0_SRK/),tmpr,DELTAOUT=.TRUE.,TOL=1.0E-08_SRK)
  bool=ALL(tmpr .APPROXEQA. (/2.0_SRK,4.0_SRK,4.0_SRK,5.0_SRK,5.0_SRK,20.0_SRK,20.0_SRK,5.0_SRK,35.0_SRK/))
  ASSERT(bool,'getUnion deltaout=.TRUE.')
  !
  COMPONENT_TEST('findNUnique 1-D Array')
  tmprealarray(1)=1.0_SRK
  tmprealarray(2)=1.0_SRK
  tmprealarray(3)=1.0000000000000001_SRK
  tmprealarray(4)=1.0000000000100000_SRK
  tmprealarray(5)=2.0_SRK
  tmprealarray(6)=2.0_SRK
  tmprealarray(7)=2.0_SRK
  tmprealarray(8)=2.0_SRK
  tmprealarray(9)=2.0_SRK
  tmprealarray(10)=2.0_SRK
  bool=findNUnique(tmprealarray) == 3
  ASSERT(bool,'findNUnique == 3')
  tmprealarray(4)=1.000000000000100_SRK
  bool=findNUnique(tmprealarray,tol=EPSREAL) == 3
  ASSERT(bool,'findNUnique == 3,tol=EPSREAL')
  tmprealarray(4)=1.000000000001000_SRK
  bool=findNUnique(tmprealarray,tol=EPSREAL*10.0_SRK) == 3
  ASSERT(bool,'findNUnique == 3,tol=EPSREAL*10.0_SRK')
  tmprealarray(4)=1.000000000010000_SRK
  bool=findNUnique(tmprealarray,tol=EPSREAL*100.0_SRK) == 3
  ASSERT(bool,'findNUnique == 3,tol=EPSREAL*100.0_SRK')
  tmprealarray(4)=1.000000000100000_SRK
  bool=findNUnique(tmprealarray,tol=EPSREAL*1000.0_SRK) == 3
  ASSERT(bool,'findNUnique == 3,tol=EPSREAL*1000.0_SRK')
  bool=findNUnique(tmprealarray,tol=EPSREAL*10000.0_SRK) == 3
  ASSERT(bool,'findNUnique == 3,tol=EPSREAL*10000.0_SRK')
  ASSERT(findNUnique(tmprealarray(1:0)) == 0,'0 subset size')
  IF(ALLOCATED(tmpr)) DEALLOCATE(tmpr)
  ALLOCATE(tmpr(0))
  ASSERT(findNUnique(tmpr) == 0,'size 0 array')

  COMPONENT_TEST('getUnique 1-D Array')
  tmprealarray(1)=1.0_SRK
  tmprealarray(2)=1.0_SRK
  tmprealarray(3)=1.0000000000000010_SRK
  tmprealarray(4)=1.0000000000100000_SRK
  tmprealarray(5)=2.0_SRK
  tmprealarray(6)=2.0_SRK
  tmprealarray(7)=2.0_SRK
  tmprealarray(8)=2.0_SRK
  tmprealarray(9)=2.0_SRK
  tmprealarray(10)=2.0_SRK
  CALL getUnique(tmprealarray,tmpr)
  bool=ALL(tmpr == (/1.0_SRK,1.0000000000100000_SRK,2.0_SRK/))
  ASSERT(bool,'getUnique, 3 unique')
  tmprealarray(3)=1.0000000000000100_SRK
  CALL getUnique(tmprealarray,tmpr,TOL=1.E-15_SRK)
  bool=ALL(tmpr == (/1.0_SRK,1.0000000000000100_SRK,1.0000000000100000_SRK,2.0_SRK/))
  ASSERT(bool,'getUnique, 4 unique with TOL')
  tmprealarray(3)=1.0000000000100001_SRK
  CALL getUnique(tmprealarray,tmpr,TOL=1.E-15_SRK)
  bool=ALL(tmpr == (/1.0_SRK,1.0000000000100000_SRK,2.0_SRK/))
  ASSERT(bool,'getUnique, 3 unique with TOL')
  CALL getUnique(tmprealarray(1:0),tmpr)
  ASSERT(.NOT. ALLOCATED(tmpr),'getUnique, size 0 array')

  !
  COMPONENT_TEST('findIndex 1-D Array')
  !Test with 0.0 specified
  tmprealarray(1)=0.0_SRK
  tmprealarray(2)=2.0_SRK
  tmprealarray(3)=6.0_SRK
  tmprealarray(4)=10.0_SRK
  tmprealarray(5)=15.0_SRK
  tmprealarray(6)=20.0_SRK
  tmprealarray(7)=40.0_SRK
  tmprealarray(8)=60.0_SRK
  tmprealarray(9)=65.0_SRK
  tmprealarray(10)=100.0_SRK

  ASSERT(findIndex(tmprealarray,-1.0_SRK,.FALSE.) == -1,'Out of Lower Bounds')
  ASSERT(findIndex(tmprealarray,101.0_SRK,.FALSE.) == -2,'Out of Upper Bounds')
  ASSERT(findIndex(tmprealarray,100.0_SRK,.FALSE.) == -3,'On Mesh Boundary')
  ASSERT(findIndex(tmprealarray,1.0_SRK,.FALSE.) == 1,'index == 1')
  ASSERT(findIndex(tmprealarray,3.0_SRK,.FALSE.) == 2,'index == 2')
  ASSERT(findIndex(tmprealarray,8.0_SRK,.FALSE.) == 3,'index == 3')
  ASSERT(findIndex(tmprealarray,12.0_SRK,.FALSE.) == 4,'index == 4')
  ASSERT(findIndex(tmprealarray,18.0_SRK,.FALSE.) == 5,'index == 5')
  ASSERT(findIndex(tmprealarray,30.0_SRK,.FALSE.) == 6,'index == 6')
  ASSERT(findIndex(tmprealarray,50.0_SRK,.FALSE.) == 7,'index == 7')
  ASSERT(findIndex(tmprealarray,62.0_SRK,.FALSE.) == 8,'index == 8')
  ASSERT(findIndex(tmprealarray,70.0_SRK,.FALSE.) == 9,'index == 9')

  !Check what happens on a mesh boundary.
  ASSERT(findIndex(tmprealarray,0.0_SRK,.FALSE.,INCL=0) == -3,'index == -3')
  ASSERT(findIndex(tmprealarray,0.0_SRK,.FALSE.,INCL=1) == -1,'index == -1')
  ASSERT(findIndex(tmprealarray,0.0_SRK,.FALSE.,INCL=2) == 1,'index == 1')
  ASSERT(findIndex(tmprealarray,2.0_SRK,.FALSE.,INCL=0) == -3,'index == -3')
  ASSERT(findIndex(tmprealarray,2.0_SRK,.FALSE.,INCL=1) == 1,'index == 1')
  ASSERT(findIndex(tmprealarray,2.0_SRK,.FALSE.,INCL=2) == 2,'index == 2')
  ASSERT(findIndex(tmprealarray,100.0_SRK,.FALSE.,INCL=2) == -2,'index == -2')

  !Test without 0.0 specified
  tmprealarray(1)=2.0_SRK
  tmprealarray(2)=6.0_SRK
  tmprealarray(3)=10.0_SRK
  tmprealarray(4)=15.0_SRK
  tmprealarray(5)=20.0_SRK
  tmprealarray(6)=40.0_SRK
  tmprealarray(7)=60.0_SRK
  tmprealarray(8)=65.0_SRK
  tmprealarray(9)=100.0_SRK
  tmprealarray(10)=110.0_SRK

  ASSERT(findIndex(tmprealarray,-1.0_SRK,.FALSE.) == -1,'Out of Lower Bounds')
  ASSERT(findIndex(tmprealarray,111.0_SRK,.FALSE.) == -2,'Out of Upper Bounds')
  ASSERT(findIndex(tmprealarray,110.0_SRK,.FALSE.) == -3,'On Mesh Boundary')
  ASSERT(findIndex(tmprealarray,1.0_SRK,.FALSE.) == -1,'index == -1')
  ASSERT(findIndex(tmprealarray,3.0_SRK,.FALSE.) == 1,'index == 1')
  ASSERT(findIndex(tmprealarray,8.0_SRK,.FALSE.) == 2,'index == 2')
  ASSERT(findIndex(tmprealarray,12.0_SRK,.FALSE.) == 3,'index == 3')
  ASSERT(findIndex(tmprealarray,18.0_SRK,.FALSE.) == 4,'index == 4')
  ASSERT(findIndex(tmprealarray,30.0_SRK,.FALSE.) == 5,'index == 5')
  ASSERT(findIndex(tmprealarray,50.0_SRK,.FALSE.) == 6,'index == 6')
  ASSERT(findIndex(tmprealarray,62.0_SRK,.FALSE.) == 7,'index == 7')
  ASSERT(findIndex(tmprealarray,70.0_SRK,.FALSE.) == 8,'index == 8')
  ASSERT(findIndex(tmprealarray,105.0_SRK,.FALSE.) == 9,'index == 9')

  !Check what happens on a mesh boundary.
  ASSERT(findIndex(tmprealarray,2.0_SRK,.FALSE.,INCL=0) == -3,'index == -3')
  ASSERT(findIndex(tmprealarray,2.0_SRK,.FALSE.,INCL=1) == -1,'index == -1')
  ASSERT(findIndex(tmprealarray,2.0_SRK,.FALSE.,INCL=2) == 1,'index == 1')
  ASSERT(findIndex(tmprealarray,6.0_SRK,.FALSE.,INCL=0) == -3,'index == -3')
  ASSERT(findIndex(tmprealarray,6.0_SRK,.FALSE.,INCL=1) == 1,'index == 1')
  ASSERT(findIndex(tmprealarray,6.0_SRK,.FALSE.,INCL=2) == 2,'index == 2')
  ASSERT(findIndex(tmprealarray,110.0_SRK,.FALSE.,INCL=2) == -2,'index == -2')

  !Test with negative lower bound specified
  tmprealarray(1)=0.0_SRK
  tmprealarray(2)=2.0_SRK
  tmprealarray(3)=6.0_SRK
  tmprealarray(4)=10.0_SRK
  tmprealarray(5)=15.0_SRK
  tmprealarray(6)=20.0_SRK
  tmprealarray(7)=40.0_SRK
  tmprealarray(8)=60.0_SRK
  tmprealarray(9)=65.0_SRK
  tmprealarray(10)=100.0_SRK

  ASSERT(findIndex((/-10.0_SRK,tmprealarray/),-11.0_SRK,.FALSE.) == -1,'Out of Lower Bounds')
  ASSERT(findIndex((/-10.0_SRK,tmprealarray/),101.0_SRK,.FALSE.) == -2,'Out of Upper Bounds')
  ASSERT(findIndex((/-10.0_SRK,tmprealarray/),100.0_SRK,.FALSE.) == -3,'On Mesh Boundary')
  ASSERT(findIndex((/-10.0_SRK,tmprealarray/),-1.0_SRK,.FALSE.) == 1,'index == 1')
  ASSERT(findIndex((/-10.0_SRK,tmprealarray/),1.0_SRK,.FALSE.) == 2,'index == 2')
  ASSERT(findIndex((/-10.0_SRK,tmprealarray/),3.0_SRK,.FALSE.) == 3,'index == 3')
  ASSERT(findIndex((/-10.0_SRK,tmprealarray/),8.0_SRK,.FALSE.) == 4,'index == 4')
  ASSERT(findIndex((/-10.0_SRK,tmprealarray/),12.0_SRK,.FALSE.) == 5,'index == 5')
  ASSERT(findIndex((/-10.0_SRK,tmprealarray/),18.0_SRK,.FALSE.) == 6,'index == 6')
  ASSERT(findIndex((/-10.0_SRK,tmprealarray/),30.0_SRK,.FALSE.) == 7,'index == 7')
  ASSERT(findIndex((/-10.0_SRK,tmprealarray/),50.0_SRK,.FALSE.) == 8,'index == 8')
  ASSERT(findIndex((/-10.0_SRK,tmprealarray/),62.0_SRK,.FALSE.) == 9,'index == 9')

  !Check what happens on a mesh boundary.
  ASSERT(findIndex((/-10.0_SRK,tmprealarray/),-10.0_SRK,.FALSE.,INCL=0) == -3,'index == -3')
  ASSERT(findIndex((/-10.0_SRK,tmprealarray/),-10.0_SRK,.FALSE.,INCL=1) == -1,'index == -1')
  ASSERT(findIndex((/-10.0_SRK,tmprealarray/),-10.0_SRK,.FALSE.,INCL=2) == 1,'index == 1')
  ASSERT(findIndex((/-10.0_SRK,tmprealarray/),0.0_SRK,.FALSE.,INCL=0) == -3,'index == -3')
  ASSERT(findIndex((/-10.0_SRK,tmprealarray/),0.0_SRK,.FALSE.,INCL=1) == 1,'index == 1')
  ASSERT(findIndex((/-10.0_SRK,tmprealarray/),0.0_SRK,.FALSE.,INCL=2) == 2,'index == 2')
  ASSERT(findIndex((/-10.0_SRK,tmprealarray/),100.0_SRK,.FALSE.,INCL=2) == -2,'index == -2')

  !Test with 0.0 specified using DELTA
  tmprealarray(1)=0.0_SRK !0.0
  tmprealarray(2)=2.0_SRK !2.0
  tmprealarray(3)=4.0_SRK !6.0
  tmprealarray(4)=4.0_SRK !10.0
  tmprealarray(5)=5.0_SRK !15.0
  tmprealarray(6)=5.0_SRK !20.0
  tmprealarray(7)=20.0_SRK !40.0
  tmprealarray(8)=20.0_SRK !60.0
  tmprealarray(9)=5.0_SRK  !65.0
  tmprealarray(10)=35.0_SRK !100.0

  ASSERT(findIndex(tmprealarray,-1.0_SRK,.TRUE.) == -1,'Out of Lower Bounds')
  ASSERT(findIndex(tmprealarray,101.0_SRK,.TRUE.) == -2,'Out of Upper Bounds')
  ASSERT(findIndex(tmprealarray,1.0_SRK,.TRUE.) == 1,'index == 1')
  ASSERT(findIndex(tmprealarray,3.0_SRK,.TRUE.) == 2,'index == 2')
  ASSERT(findIndex(tmprealarray,8.0_SRK,.TRUE.) == 3,'index == 3')
  ASSERT(findIndex(tmprealarray,12.0_SRK,.TRUE.) == 4,'index == 4')
  ASSERT(findIndex(tmprealarray,18.0_SRK,.TRUE.) == 5,'index == 5')
  ASSERT(findIndex(tmprealarray,30.0_SRK,.TRUE.) == 6,'index == 6')
  ASSERT(findIndex(tmprealarray,50.0_SRK,.TRUE.) == 7,'index == 7')
  ASSERT(findIndex(tmprealarray,62.0_SRK,.TRUE.) == 8,'index == 8')
  ASSERT(findIndex(tmprealarray,70.0_SRK,.TRUE.) == 9,'index == 9')

  !Check what happens on a mesh boundary.
  ASSERT(findIndex(tmprealarray,0.0_SRK,.TRUE.,INCL=0) == -3,'index == -3')
  ASSERT(findIndex(tmprealarray,0.0_SRK,.TRUE.,INCL=1) == -1,'index == -1')
  ASSERT(findIndex(tmprealarray,0.0_SRK,.TRUE.,INCL=2) == 1,'index == 1')
  ASSERT(findIndex(tmprealarray,2.0_SRK,.TRUE.,INCL=0) == -3,'index == -3')
  ASSERT(findIndex(tmprealarray,2.0_SRK,.TRUE.,INCL=1) == 1,'index == 1')
  ASSERT(findIndex(tmprealarray,2.0_SRK,.TRUE.,INCL=2) == 2,'index == 2')
  ASSERT(findIndex(tmprealarray,100.0_SRK,.TRUE.,INCL=2) == -2,'index == -2')

  !Test without 0.0 specified using DELTA
  tmprealarray(1)=2.0_SRK !2.0
  tmprealarray(2)=4.0_SRK !6.0
  tmprealarray(3)=4.0_SRK !10.0
  tmprealarray(4)=5.0_SRK !15.0
  tmprealarray(5)=5.0_SRK !20.0
  tmprealarray(6)=20.0_SRK !40.0
  tmprealarray(7)=20.0_SRK !60.0
  tmprealarray(8)=5.0_SRK  !65.0
  tmprealarray(9)=35.0_SRK !100.0
  tmprealarray(10)=10.0_SRK !110.0

  ASSERT(findIndex((/0.0_SRK,tmprealarray/),-1.0_SRK,.TRUE.) == -1,'Out of Lower Bounds')
  ASSERT(findIndex((/0.0_SRK,tmprealarray/),111.0_SRK,.TRUE.) == -2,'Out of Upper Bounds')
  ASSERT(findIndex((/0.0_SRK,tmprealarray/),1.0_SRK,.TRUE.) == 1,'index == 1')
  ASSERT(findIndex((/0.0_SRK,tmprealarray/),3.0_SRK,.TRUE.) == 2,'index == 2')
  ASSERT(findIndex((/0.0_SRK,tmprealarray/),8.0_SRK,.TRUE.) == 3,'index == 3')
  ASSERT(findIndex((/0.0_SRK,tmprealarray/),12.0_SRK,.TRUE.) == 4,'index == 4')
  ASSERT(findIndex((/0.0_SRK,tmprealarray/),18.0_SRK,.TRUE.) == 5,'index == 5')
  ASSERT(findIndex((/0.0_SRK,tmprealarray/),30.0_SRK,.TRUE.) == 6,'index == 6')
  ASSERT(findIndex((/0.0_SRK,tmprealarray/),50.0_SRK,.TRUE.) == 7,'index == 7')
  ASSERT(findIndex((/0.0_SRK,tmprealarray/),62.0_SRK,.TRUE.) == 8,'index == 8')
  ASSERT(findIndex((/0.0_SRK,tmprealarray/),70.0_SRK,.TRUE.) == 9,'index == 9')
  ASSERT(findIndex((/0.0_SRK,tmprealarray/),105.0_SRK,.TRUE.) == 10,'index == 10')

  !Check what happens on a mesh boundary.
  ASSERT(findIndex((/0.0_SRK,tmprealarray/),0.0_SRK,.TRUE.,INCL=0) == -3,'index == -3')
  ASSERT(findIndex((/0.0_SRK,tmprealarray/),0.0_SRK,.TRUE.,INCL=1) == -1,'index == -1')
  ASSERT(findIndex((/0.0_SRK,tmprealarray/),0.0_SRK,.TRUE.,INCL=2) == 1,'index == 1')
  ASSERT(findIndex((/0.0_SRK,tmprealarray/),2.0_SRK,.TRUE.,INCL=0) == -3,'index == -3')
  ASSERT(findIndex((/0.0_SRK,tmprealarray/),2.0_SRK,.TRUE.,INCL=1) == 1,'index == 1')
  ASSERT(findIndex((/0.0_SRK,tmprealarray/),2.0_SRK,.TRUE.,INCL=2) == 2,'index == 2')
  ASSERT(findIndex((/0.0_SRK,tmprealarray/),110.0_SRK,.TRUE.,INCL=2) == -2,'index == -2')

  !Test with negative lower bound specified using DELTA
  tmprealarray(1)=-10.0_SRK !-10.0
  tmprealarray(2)=10.0_SRK !0.0
  tmprealarray(3)=2.0_SRK !2.0
  tmprealarray(4)=4.0_SRK !6.0
  tmprealarray(5)=4.0_SRK !10.0
  tmprealarray(6)=5.0_SRK !15.0
  tmprealarray(7)=5.0_SRK !20.0
  tmprealarray(8)=20.0_SRK !40.0
  tmprealarray(9)=20.0_SRK !60.0
  tmprealarray(10)=5.0_SRK  !65.0

  ASSERT(findIndex(tmprealarray,-11.0_SRK,.TRUE.) == -1,'Out of Lower Bounds')
  ASSERT(findIndex(tmprealarray,101.0_SRK,.TRUE.) == -2,'Out of Upper Bounds')
  ASSERT(findIndex(tmprealarray,-1.0_SRK,.TRUE.) == 1,'index == 1')
  ASSERT(findIndex(tmprealarray,1.0_SRK,.TRUE.) == 2,'index == 2')
  ASSERT(findIndex(tmprealarray,3.0_SRK,.TRUE.) == 3,'index == 3')
  ASSERT(findIndex(tmprealarray,8.0_SRK,.TRUE.) == 4,'index == 4')
  ASSERT(findIndex(tmprealarray,12.0_SRK,.TRUE.) == 5,'index == 5')
  ASSERT(findIndex(tmprealarray,18.0_SRK,.TRUE.) == 6,'index == 6')
  ASSERT(findIndex(tmprealarray,30.0_SRK,.TRUE.) == 7,'index == 7')
  ASSERT(findIndex(tmprealarray,50.0_SRK,.TRUE.) == 8,'index == 8')
  ASSERT(findIndex(tmprealarray,62.0_SRK,.TRUE.) == 9,'index == 9')

  !Check what happens on a mesh boundary.
  ASSERT(findIndex(tmprealarray,-10.0_SRK,.TRUE.,INCL=0) == -3,'index == -3')
  ASSERT(findIndex(tmprealarray,-10.0_SRK,.TRUE.,INCL=1) == -1,'index == -1')
  ASSERT(findIndex(tmprealarray,-10.0_SRK,.TRUE.,INCL=2) == 1,'index == 1')
  ASSERT(findIndex(tmprealarray,0.0_SRK,.TRUE.,INCL=0) == -3,'index == -3')
  ASSERT(findIndex(tmprealarray,0.0_SRK,.TRUE.,INCL=1) == 1,'index == 1')
  ASSERT(findIndex(tmprealarray,0.0_SRK,.TRUE.,INCL=2) == 2,'index == 2')
  ASSERT(findIndex(tmprealarray,65.0_SRK,.TRUE.,INCL=2) == -2,'index == -2')

  !Check monotonicity functions
  COMPONENT_TEST('isIncreasing 1-D Array')
  DO i=1,10
    tmprealarray(i)=REAL(i,SRK)
    tmpintarray(i)=i
  END DO

  ASSERT(isStrictlyIncreasing(tmprealarray),'Failure to identify strictly increasing real data')
  ASSERT(isStrictlyIncreasing(tmpintarray),'Failure to identify strictly increasing int data')
  ASSERT(isStrictlyIncDec(tmprealarray),'Failure to identify strictly increasing real data')
  ASSERT(isStrictlyIncDec(tmpintarray),'Failure to identify strictly increasing int data')
  ASSERT(isIncreasing(tmprealarray),'Failure to identify monotonically increasing real data')
  ASSERT(isIncreasing(tmpintarray),'Failure to identify monotonically increasing int data')
  ASSERT(isMonotonic(tmprealarray),'Failure to identify monotonically increasing real data')
  ASSERT(isMonotonic(tmpintarray),'Failure to identify monotonically increasing int data')
  tmprealarray(4)=3.0_SRK
  tmpintarray(4)=3
  ASSERT(.NOT.isStrictlyIncreasing(tmprealarray),'Failure to identify nonstrictly increasing real data')
  ASSERT(.NOT.isStrictlyIncreasing(tmpintarray),'Failure to identify nonstrictly increasing int data')
  ASSERT(.NOT.isStrictlyIncDec(tmprealarray),'Failure to identify nonstrictly increasing real data')
  ASSERT(.NOT.isStrictlyIncDec(tmpintarray),'Failure to identify nonstrictly increasing int data')
  ASSERT(isIncreasing(tmprealarray),'Failure to identify monotonically increasing real data')
  ASSERT(isIncreasing(tmpintarray),'Failure to identify monotonically increasing int data')
  ASSERT(isMonotonic(tmprealarray),'Failure to identify monotonically increasing real data')
  ASSERT(isMonotonic(tmpintarray),'Failure to identify monotonically increasing int data')
  tmprealarray(4)=20.0_SRK
  tmpintarray(4)=20
  ASSERT(.NOT.isIncreasing(tmprealarray),'Failure to identify monotonically increasing real data')
  ASSERT(.NOT.isIncreasing(tmpintarray),'Failure to identify monotonically increasing int data')
  ASSERT(.NOT.isMonotonic(tmprealarray),'Failure to identify monotonically increasing real data')
  ASSERT(.NOT.isMonotonic(tmpintarray),'Failure to identify monotonically increasing int data')

  COMPONENT_TEST('isDecreasing 1-D Array')
  DO i=1,10
    tmprealarray(i)=11.0_SRK-REAL(i,SRK)
    tmpintarray(i)=INT(tmprealarray(i))
  END DO

  ASSERT(isStrictlyDecreasing(tmprealarray),'Failure to identify strictly Decreasing real data')
  ASSERT(isStrictlyDecreasing(tmpintarray),'Failure to identify strictly Decreasing int data')
  ASSERT(isStrictlyIncDec(tmprealarray),'Failure to identify strictly Decreasing real data')
  ASSERT(isStrictlyIncDec(tmpintarray),'Failure to identify strictly Decreasing int data')
  ASSERT(isDecreasing(tmprealarray),'Failure to identify monotonically Decreasing real data')
  ASSERT(isDecreasing(tmpintarray),'Failure to identify monotonically Decreasing int data')
  ASSERT(isMonotonic(tmprealarray),'Failure to identify monotonically Decreasing real data')
  ASSERT(isMonotonic(tmpintarray),'Failure to identify monotonically Decreasing int data')
  tmprealarray(7)=5.0_SRK
  tmpintarray(7)=5
  ASSERT(.NOT.isStrictlyDecreasing(tmprealarray),'Failure to identify nonstrictly Decreasing real data')
  ASSERT(.NOT.isStrictlyDecreasing(tmpintarray),'Failure to identify nonstrictly Decreasing int data')
  ASSERT(.NOT.isStrictlyIncDec(tmprealarray),'Failure to identify nonstrictly Decreasing real data')
  ASSERT(.NOT.isStrictlyIncDec(tmpintarray),'Failure to identify nonstrictly Decreasing int data')
  ASSERT(isDecreasing(tmprealarray),'Failure to identify monotonically Decreasing real data')
  ASSERT(isDecreasing(tmpintarray),'Failure to identify monotonically Decreasing int data')
  ASSERT(isMonotonic(tmprealarray),'Failure to identify monotonically Decreasing real data')
  ASSERT(isMonotonic(tmpintarray),'Failure to identify monotonically Decreasing int data')
  tmprealarray(4)=-20.0_SRK
  tmpintarray(4)=-20
  ASSERT(.NOT.isDecreasing(tmprealarray),'Failure to identify monotonically Decreasing real data')
  ASSERT(.NOT.isDecreasing(tmpintarray),'Failure to identify monotonically Decreasing int data')
  ASSERT(.NOT.isMonotonic(tmprealarray),'Failure to identify monotonically Decreasing real data')
  ASSERT(.NOT.isMonotonic(tmpintarray),'Failure to identify monotonically Decreasing int data')

  COMPONENT_TEST('hasAnyRemainder 1-D Array')
  tmprealarray(1)=3.3351111111111151_SRK
  tmprealarray(2)=2.555111_SRK
  tmprealarray(3)=4.00505_SRK
  tmprealarray(4)=2.4444_SRK
  tmprealarray(5)=3.333_SRK
  tmprealarray(6)=2.333_SRK
  tmprealarray(7)=3.2_SRK
  tmprealarray(8)=2.55_SRK
  tmprealarray(9)=2.661_SRK
  tmprealarray(10)=10.0_SRK/3.0_SRK
  ASSERT(hasAnyRemainder(tmprealarray,1.0E-1_SRK),'0.1')
  ASSERT(hasAnyRemainder(tmprealarray,1.0E-2_SRK),'0.01')
  ASSERT(hasAnyRemainder(tmprealarray,1.0E-3_SRK),'0.001')
  ASSERT(hasAnyRemainder(tmprealarray,1.0E-4_SRK),'0.0001')
  ASSERT(hasAnyRemainder(tmprealarray,1.0E-5_SRK),'0.00001')
  ASSERT(hasAnyRemainder(tmprealarray,1.0E-6_SRK),'0.000001')
  ASSERT(hasAnyRemainder(tmprealarray,1.0E-7_SRK),'0.0000001')
  ASSERT(hasAnyRemainder(tmprealarray,1.0E-8_SRK),'0.00000001')
  ASSERT(hasAnyRemainder(tmprealarray,1.0E-9_SRK),'0.000000001')
  ASSERT(hasAnyRemainder(tmprealarray,1.0E-10_SRK),'0.0000000001')
  ASSERT(hasAnyRemainder(tmprealarray,1.0E-11_SRK),'0.00000000001')
  ASSERT(hasAnyRemainder(tmprealarray,1.0E-12_SRK),'0.000000000001')
  ASSERT(hasAnyRemainder(tmprealarray,1.0E-13_SRK),'0.0000000000001')


  tmprealarray(1)=3.3_SRK
  ASSERT(.NOT. hasAnyRemainder(tmprealarray(1:1),1.0E-1_SRK),'0.1')
  tmprealarray(1)=3.34_SRK
  ASSERT(.NOT. hasAnyRemainder(tmprealarray(1:1),1.0E-2_SRK),'0.01')
  tmprealarray(1)=3.345_SRK
  ASSERT(.NOT. hasAnyRemainder(tmprealarray(1:1),1.0E-3_SRK),'0.001')
  tmprealarray(1)=3.3456_SRK
  ASSERT(.NOT. hasAnyRemainder(tmprealarray(1:1),1.0E-4_SRK),'0.0001')
  tmprealarray(1)=3.34567_SRK
  ASSERT(.NOT. hasAnyRemainder(tmprealarray(1:1),1.0E-5_SRK),'0.00001')
  tmprealarray(1)=3.345678_SRK
  ASSERT(.NOT. hasAnyRemainder(tmprealarray(1:1),1.0E-6_SRK),'0.000001')
  tmprealarray(1)=3.3456789_SRK
  ASSERT(.NOT. hasAnyRemainder(tmprealarray(1:1),1.0E-7_SRK),'0.0000001')
  tmprealarray(1)=3.34567891_SRK
  ASSERT(.NOT. hasAnyRemainder(tmprealarray(1:1),1.0E-8_SRK),'0.00000001')
  tmprealarray(1)=3.345678912_SRK
  ASSERT(.NOT. hasAnyRemainder(tmprealarray(1:1),1.0E-9_SRK),'0.000000001')
  tmprealarray(1)=3.3456789123_SRK
  ASSERT(.NOT. hasAnyRemainder(tmprealarray(1:1),1.0E-10_SRK),'0.0000000001')
  tmprealarray(1)=3.34567891234_SRK
  ASSERT(.NOT. hasAnyRemainder(tmprealarray(1:1),1.0E-11_SRK),'0.00000000001')
  tmprealarray(1)=3.345678912345_SRK
  ASSERT(.NOT. hasAnyRemainder(tmprealarray(1:1),1.0E-12_SRK),'0.000000000001')
  tmprealarray(1)=3.3456789123456_SRK
  ASSERT(.NOT. hasAnyRemainder(tmprealarray(1:1),1.0E-13_SRK),'0.0000000000001')

ENDSUBROUTINE test1DReals
!
!-------------------------------------------------------------------------------
SUBROUTINE test1DInts()
  !
  COMPONENT_TEST('getAbsolute 1-D Array')
  tmpintarray(1)=0
  tmpintarray(2)=2  !2
  tmpintarray(3)=1  !3
  tmpintarray(4)=2  !5
  tmpintarray(5)=7  !12
  tmpintarray(6)=8  !20
  tmpintarray(7)=5  !25
  tmpintarray(8)=15 !40
  tmpintarray(9)=20 !60
  tmpintarray(10)=40 !100
  !getAbsolute_1DInt
  CALL getAbsolute(tmpintarray,tmpi)
  bool=ALL(tmpi == (/0,0,2,3,5,12,20,25,40,60,100/))
  ASSERT(bool,'getAbsolute no Xi ')
  CALL getAbsolute(tmpintarray(3:10),tmpi)
  bool=ALL(tmpi == (/0,1,3,10,18,23,38,58,98/))
  ASSERT(bool,'getAbsolute, partial array, no Xi ')
  CALL getAbsolute(tmpintarray(2:10),tmpi,XI=0)
  bool=ALL(tmpi == (/0,2,3,5,12,20,25,40,60,100/))
  ASSERT(bool,'getAbsolute, partial array, with Xi ')
  !
  COMPONENT_TEST('findIndex 1-D Array')
  tmpintarray(1)=0
  tmpintarray(2)=2
  tmpintarray(3)=3
  tmpintarray(4)=5
  tmpintarray(5)=12
  tmpintarray(6)=20
  tmpintarray(7)=25
  tmpintarray(8)=40
  tmpintarray(9)=60
  tmpintarray(10)=100
  ASSERT(findIndex(tmpintarray,-1,.FALSE.) == -1,'Out of Lower Bounds')
  ASSERT(findIndex(tmpintarray,101,.FALSE.) == -2,'Out of Upper Bounds')
  ASSERT(findIndex(tmpintarray,0,.FALSE.) == -3,'On mesh boundary')
  !
  COMPONENT_TEST('findNUnique 1-D Array')
  tmpintarray(1)=0
  tmpintarray(2)=2
  tmpintarray(3)=3
  tmpintarray(4)=5
  tmpintarray(5)=12
  tmpintarray(6)=20
  tmpintarray(7)=25
  tmpintarray(8)=40
  tmpintarray(9)=60
  tmpintarray(10)=100
  ASSERT(findNUnique(tmpintarray) == 10,'No duplicates')
  tmpintarray(3)=2
  tmpintarray(5)=20
  tmpintarray(9)=100
  ASSERT(findNUnique(tmpintarray) == 7,'3 duplicates')
  ASSERT(findNUnique(tmpintarray(1:0)) == 0,'0 subset size')
  IF(ALLOCATED(tmpi)) DEALLOCATE(tmpi)
  ALLOCATE(tmpi(0))
  ASSERT(findNUnique(tmpi) == 0,'size 0 array')
  !
  COMPONENT_TEST('getUnique 1-D Array')
  tmpintarray(1)=0
  tmpintarray(2)=2
  tmpintarray(3)=3
  tmpintarray(4)=5
  tmpintarray(5)=12
  tmpintarray(6)=20
  tmpintarray(7)=25
  tmpintarray(8)=40
  tmpintarray(9)=60
  tmpintarray(10)=100
  CALL getUnique(tmpintarray,tmpi)
  bool=ALL(tmpi == tmpintarray)
  ASSERT(bool,'getUnique, all unique')
  tmpintarray(3)=2
  tmpintarray(5)=20
  tmpintarray(9)=100
  CALL getUnique(tmpintarray,tmpi)
  bool=ALL(tmpi == (/0,2,5,20,25,40,100/))
  ASSERT(bool,'getUnique, 3 duplicates')
  CALL getUnique(tmpintarray(1:0),tmpi)
  ASSERT(.NOT. ALLOCATED(tmpi),'getUnique, size 0 array')

ENDSUBROUTINE test1DInts
!
!-------------------------------------------------------------------------------
SUBROUTINE test1DStrings()
  !findNUnique_1DStrings
  COMPONENT_TEST('findNUnique 1-D Array')
  tmpstr1a=''
  ASSERT(findNUnique(tmpstr1a) == 0,'empty array')
  tmpstr1a(1)='one'
  tmpstr1a(2)='two'
  tmpstr1a(3)='three'
  tmpstr1a(4)='four'
  tmpstr1a(5)='five'
  tmpstr1a(6)='six'
  tmpstr1a(7)='seven'
  tmpstr1a(8)='eight'
  tmpstr1a(9)='nine'
  tmpstr1a(10)='ten'
  ASSERT(findNUnique(tmpstr1a) == 10,'all unique array')
  tmpstr1a(1)='two'
  tmpstr1a(5)='four'
  tmpstr1a(10)='nine'
  ASSERT(findNUnique(tmpstr1a) == 7,'3 duplicates array')
  tmpstr1a(1)=''
  ASSERT(findNUnique(tmpstr1a) == 7,'2 duplicates, one null array')
  ASSERT(findNUnique(tmpstr1a(1:0)) == 0,'0 subset size')
  IF(ALLOCATED(tmps1)) DEALLOCATE(tmps1)
  ALLOCATE(tmps1(0))
  ASSERT(findNUnique(tmps1) == 0,'size 0 array')

  !getUnique_1DStrings
  COMPONENT_TEST('getUnique 1-D Array')
  tmpstr1a=''
  IF(ALLOCATED(tmps1)) DEALLOCATE(tmps1)
  CALL getUnique(tmpstr1a,tmps1)
  ASSERT(.NOT.ALLOCATED(tmps1),'empty array')
  tmpstr1a(1)='one'
  tmpstr1a(2)='two'
  tmpstr1a(3)='three'
  tmpstr1a(4)='four'
  tmpstr1a(5)='five'
  tmpstr1a(6)='six'
  tmpstr1a(7)='seven'
  tmpstr1a(8)='eight'
  tmpstr1a(9)='nine'
  tmpstr1a(10)='ten'
  IF(ALLOCATED(tmps1)) DEALLOCATE(tmps1)
  CALL getUnique(tmpstr1a,tmps1)
  bool=tmps1(1) == 'one' .AND. tmps1(2) == 'two' .AND. tmps1(3) == 'three' .AND. &
      tmps1(4) == 'four' .AND. tmps1(5) == 'five' .AND. tmps1(6) == 'six' .AND. &
      tmps1(7) == 'seven' .AND. tmps1(8) == 'eight' .AND. tmps1(9) == 'nine' .AND. tmps1(10) == 'ten'
  ASSERT(bool,'all unique array')
  tmpstr1a(1)='two'
  tmpstr1a(5)='four'
  tmpstr1a(10)='nine'
  IF(ALLOCATED(tmps1)) DEALLOCATE(tmps1)
  CALL getUnique(tmpstr1a,tmps1)
  bool=tmps1(1) == 'two' .AND. tmps1(2) == 'three' .AND. &
      tmps1(3) == 'four' .AND.  tmps1(4) == 'six' .AND. &
      tmps1(5) == 'seven' .AND. tmps1(6) == 'eight' .AND. tmps1(7) == 'nine'
  ASSERT(bool,'3 duplicates unique array')
  tmpstr1a(1)=''
  bool=tmps1(1) == 'two' .AND. tmps1(2) == 'three' .AND. &
      tmps1(3) == 'four' .AND.  tmps1(4) == 'six' .AND. &
      tmps1(5) == 'seven' .AND. tmps1(6) == 'eight' .AND. tmps1(7) == 'nine'
  ASSERT(bool,'2 duplicates, one null unique array')

ENDSUBROUTINE test1DStrings
!
!-------------------------------------------------------------------------------
SUBROUTINE test2DStrings()
  !findNUnique_2DStrings
  COMPONENT_TEST('findNUnique 2-D Array')
  tmpstr1a=''
  ASSERT(findNUnique(tmpstr2a) == 0,'empty array')
  tmpstr2a(1,1)='one'
  tmpstr2a(1,2)='two'
  tmpstr2a(2,1)='three'
  tmpstr2a(2,2)='four'
  ASSERT(findNUnique(tmpstr2a) == 4,'all unique array')
  tmpstr2a(1,1)='two'
  ASSERT(findNUnique(tmpstr2a) == 3,'1 duplicates array')

  str2a='one'; str2a(2,3)='two'
  ASSERT(findNUnique(str2a) == 2,'one unique array')

  ASSERT(findNUnique(tmpstr2a(1:0,1:0)) == 0,'0 subset size')
  IF(ALLOCATED(tmps2)) DEALLOCATE(tmps2)
  ALLOCATE(tmps2(0,0))
  ASSERT(findNUnique(tmps2) == 0,'size 0 array')
  DEALLOCATE(tmps2)

  !getUnique_1DStrings
  COMPONENT_TEST('getUnique 2-D Array')
  tmpstr2a=''
  IF(ALLOCATED(tmps1)) DEALLOCATE(tmps1)
  CALL getUnique(tmpstr2a,tmps1)
  ASSERT(.NOT.ALLOCATED(tmps1),'empty array')
  tmpstr2a(1,1)='one'
  tmpstr2a(1,2)='three'
  tmpstr2a(2,1)='two'
  tmpstr2a(2,2)='four'
  IF(ALLOCATED(tmps1)) DEALLOCATE(tmps1)
  CALL getUnique(tmpstr2a,tmps1)
  bool=tmps1(1) == 'one' .AND. tmps1(2) == 'two' .AND. tmps1(3) == 'three' .AND. &
      tmps1(4) == 'four'
  ASSERT(bool,'all unique array')
  FINFO() tmps1(1)//' '//tmps1(2)//' '//tmps1(3)//' '//tmps1(4)
  tmpstr2a(1,1)='two'
  IF(ALLOCATED(tmps1)) DEALLOCATE(tmps1)
  CALL getUnique(tmpstr2a,tmps1)
  bool=tmps1(1) == 'two' .AND. tmps1(2) == 'three' .AND. &
      tmps1(3) == 'four'
  ASSERT(bool,'3 duplicates unique array')
  DEALLOCATE(tmps1)
ENDSUBROUTINE test2DStrings
!
!-------------------------------------------------------------------------------
!SUBROUTINE test2DInts()
  !findNUnique_2DInt
  !getUnique_2DInt
!ENDSUBROUTINE test2DInts
!
!-------------------------------------------------------------------------------
SUBROUTINE testReplaceEntry()
  INTEGER(SIK) :: index
  TYPE(StringType),ALLOCATABLE :: strlist(:),strsublist(:)

  COMPONENT_TEST('1D Strings')
  ALLOCATE(strlist(3))
  strlist(1)='string1'
  strlist(2)='string 2'
  strlist(3)='string  3'
  ALLOCATE(strsublist(0))
  CALL replaceEntry(strlist,strsublist,2)
  ASSERT_EQ(SIZE(strlist),2,'0 list replacement')
  ASSERT_EQ(CHAR(strlist(1)),'string1','strlist(1)')
  ASSERT_EQ(CHAR(strlist(2)),'string  3','strlist(2)')
  DEALLOCATE(strsublist)
  ALLOCATE(strsublist(2))
  strsublist(1)='test string 1'
  strsublist(2)='test string  2'
  CALL replaceEntry(strlist,strsublist,2)
  ASSERT_EQ(SIZE(strlist),3,'size strlist')
  ASSERT_EQ(CHAR(strlist(1)),'string1','strlist(1)')
  ASSERT_EQ(CHAR(strlist(2)),'test string 1','strlist(2)')
  ASSERT_EQ(CHAR(strlist(3)),'test string  2','strlist(3)')
  CALL replaceEntry(strlist,strsublist,1)
  ASSERT_EQ(SIZE(strlist),4,'size strlist')
  ASSERT_EQ(CHAR(strlist(1)),'test string 1','strlist(1)')
  ASSERT_EQ(CHAR(strlist(2)),'test string  2','strlist(2)')
  ASSERT_EQ(CHAR(strlist(3)),'test string 1','strlist(3)')
  ASSERT_EQ(CHAR(strlist(4)),'test string  2','strlist(4)')
  CALL replaceEntry(strlist,strsublist,2)
  ASSERT_EQ(SIZE(strlist),5,'size strlist')
  ASSERT_EQ(CHAR(strlist(1)),'test string 1','strlist(1)')
  ASSERT_EQ(CHAR(strlist(2)),'test string 1','strlist(2)')
  ASSERT_EQ(CHAR(strlist(3)),'test string  2','strlist(3)')
  ASSERT_EQ(CHAR(strlist(4)),'test string 1','strlist(4)')
  ASSERT_EQ(CHAR(strlist(5)),'test string  2','strlist(5)')
  CALL replaceEntry(strlist,strsublist,5)
  ASSERT_EQ(SIZE(strlist),6,'size strlist')
  ASSERT_EQ(CHAR(strlist(1)),'test string 1','strlist(1)')
  ASSERT_EQ(CHAR(strlist(2)),'test string 1','strlist(2)')
  ASSERT_EQ(CHAR(strlist(3)),'test string  2','strlist(3)')
  ASSERT_EQ(CHAR(strlist(4)),'test string 1','strlist(4)')
  ASSERT_EQ(CHAR(strlist(5)),'test string 1','strlist(5)')
  ASSERT_EQ(CHAR(strlist(6)),'test string  2','strlist(6)')
  
ENDSUBROUTINE testReplaceEntry
!
ENDPROGRAM testArrayUtils
