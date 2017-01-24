!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
PROGRAM testBLAS
#include "UnitTest.h"
  USE UnitTest
  USE IntrType
  USE BLAS

  IMPLICIT NONE

  CREATE_TEST('test BLAS')

  REGISTER_SUBTEST('Test BLAS 1',testBLAS1)
  REGISTER_SUBTEST('Test BLAS 2',testBLAS2)
  REGISTER_SUBTEST('Test BLAS 3',testBLAS3)

  FINALIZE_TEST()
!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
    SUBROUTINE testBLAS1()
      INTEGER(SIK) :: iresult
      REAL(SDK) :: da(200),da2(20,20),dresult,dalpha(200)
      REAL(SDK) :: db(200),db2(20,20)
      REAL(SSK) :: sa(200),sa2(20,20),sresult,salpha(200)
      REAL(SSK) :: sb(200),sb2(20,20)
      LOGICAL(SBK) :: bool
  !
  !Test _swap
      da=1.0_SDK
      db=0.0_SDK
      CALL BLAS_swap(10,da,1,db,1)
      bool = ALL(da(1:10) == 0.0_SDK) .AND. ALL(db(1:10) == 1.0_SDK)
      ASSERT(bool, 'BLAS_swap(10,da,1,db,1)')
      CALL BLAS_swap(10,da(11:20),db(11:200))
      bool = ALL(da(1:20) == 0.0_SDK) .AND. ALL(db(1:20) == 1.0_SDK)
      ASSERT(bool, 'BLAS_swap(10,da(11:20),db(11:200))')
      CALL BLAS_swap(da(21:30),1,db(21:30),1)
      bool = ALL(da(1:30) == 0.0_SDK) .AND. ALL(db(1:30) == 1.0_SDK)
      ASSERT(bool, 'BLAS_swap(da(21:30),1,db(21:30),1)')
      CALL BLAS_swap(da(31:200),db(31:200))
      bool = ALL(da == 0.0_SDK) .AND. ALL(db == 1.0_SDK)
      ASSERT(bool, 'BLAS_swap(da(31:200),db(31:200))')
      !Test swap on "2-D vectors"
      da2=1.0_SDK
      db2=0.0_SDK
      CALL BLAS_swap(400,da2(:,1),1,db2(:,1),1) !swap the whole thing
      bool = ALL(da2 == 0.0_SDK) .AND. ALL(db2 == 1.0_SDK)
      ASSERT(bool, 'BLAS_swap(400,da2(:,1),1,db2(:,1),1)')
      CALL BLAS_swap(20,da2(:,1),20,db2(:,1),20) !swap first rows
      bool = ALL(da2(1,:) == 1.0_SDK) .AND. ALL(db2(1,:) == 0.0_SDK)
      ASSERT(bool, 'BLAS_swap(20,da2(:,1),20,db2(:,1),20)')
      CALL BLAS_swap(20,da2(:,1),da2(5:,1),20) !swap the 1st and 5th rows
      bool = ALL(da2(1,:) == 0.0_SDK) .AND. ALL(da2(5,:) == 1.0_SDK)
      ASSERT(bool, 'BLAS_swap(20,da2(:,1),20,da2(5:,1),20)')
      !By not including n the rows are only partly swapped because da2(5:,1) is
      !shorter than da2(:,1)
      CALL BLAS_swap(da2(:,1),da2(5:,1),20)
      bool = ALL(da2(1,1:16) == 1.0_SDK) .AND. ALL(da2(1,17:20) == 0.0_SDK) &
          .AND. ALL(da2(5,1:16) == 0.0_SDK) .AND. ALL(da2(5,17:20) == 1.0_SDK)
      ASSERT(bool, 'BLAS_swap(da2(:,1),20,da2(5:,1),20)')
      sa=1.0_SSK
      sb=0.0_SSK
      CALL BLAS_swap(10,sa,1,sb,1)
      bool = ALL(sa(1:10) == 0.0_SSK) .AND. ALL(sb(1:10) == 1.0_SSK)
      ASSERT(bool, 'BLAS_swap(10,sa,1,sb,1)')
      CALL BLAS_swap(10,sa(11:20),sb(11:200))
      bool = ALL(sa(1:20) == 0.0_SSK) .AND. ALL(sb(1:20) == 1.0_SSK)
      ASSERT(bool, 'BLAS_swap(10,sa(11:20),sb(11:200))')
      CALL BLAS_swap(sa(21:30),1,sb(21:30),1)
      bool = ALL(sa(1:30) == 0.0_SSK) .AND. ALL(sb(1:30) == 1.0_SSK)
      ASSERT(bool, 'BLAS_swap(sa(21:30),1,sb(21:30),1)')
      CALL BLAS_swap(sa(31:200),sb(31:200))
      bool = ALL(sa == 0.0_SSK) .AND. ALL(sb == 1.0_SSK)
      ASSERT(bool, 'BLAS_swap(sa(31:200),sb(31:200))')
      !Test swap on "2-D vectors"
      sa2=1.0_SSK
      sb2=0.0_SSK
      CALL BLAS_swap(400,sa2(:,1),1,sb2(:,1),1) !swap the whole thing
      bool = ALL(sa2 == 0.0_SSK) .AND. ALL(sb2 == 1.0_SSK)
      ASSERT(bool, 'BLAS_swap(400,sa2(:,1),1,sb2(:,1),1)')
      CALL BLAS_swap(20,sa2(:,1),20,sb2(:,1),20) !swap first rows
      bool = ALL(sa2(1,:) == 1.0_SSK) .AND. ALL(sb2(1,:) == 0.0_SSK)
      ASSERT(bool, 'BLAS_swap(20,sa2(:,1),20,sb2(:,1),20)')
      CALL BLAS_swap(20,sa2(:,1),sa2(5:,1),20) !swap the 1st and 5th rows
      bool = ALL(sa2(1,:) == 0.0_SSK) .AND. ALL(sa2(5,:) == 1.0_SSK)
      ASSERT(bool, 'BLAS_swap(20,sa2(:,1),20,sa2(5:,1),20)')
      !By not including n the rows are only partly swapped because sa2(5:,1) is
      !shorter than sa2(:,1)
      CALL BLAS_swap(sa2(:,1),sa2(5:,1),20)
      bool = ALL(sa2(1,1:16) == 1.0_SSK) .AND. ALL(sa2(1,17:20) == 0.0_SSK) &
          .AND. ALL(sa2(5,1:16) == 0.0_SSK) .AND. ALL(sa2(5,17:20) == 1.0_SSK)
      ASSERT(bool, 'BLAS_swap(sa2(:,1),20,sa2(5:,1),20)')
      WRITE(*,*) '  Passed: CALL BLAS_swap(...)'
  !
  !Test _copy
      da=1.0_SDK
      db=0.0_SDK
      CALL BLAS_copy(10,da,1,db,1)
      bool = ALL(db(1:10) == 1.0_SDK) .AND. ALL(db(11:200) == 0.0_SDK)
      ASSERT(bool, 'BLAS_copy(10,da,1,db,1)')
      CALL BLAS_copy(10,da(11:20),db(11:200))
      bool = ALL(db(1:20) == 1.0_SDK) .AND. ALL(db(21:200) == 0.0_SDK)
      ASSERT(bool, 'BLAS_copy(10,da(11:20),db(11:200))')
      CALL BLAS_copy(da(21:30),1,db(21:30),1)
      bool = ALL(db(1:30) == 1.0_SDK) .AND. ALL(db(31:200) == 0.0_SDK)
      ASSERT(bool, 'BLAS_copy(da(21:30),1,db(21:30),1)')
      CALL BLAS_copy(da(31:40),db(31:40),1)
      bool = ALL(db(1:40) == 1.0_SDK) .AND. ALL(db(41:200) == 0.0_SDK)
      ASSERT(bool, 'BLAS_copy(da(31:40),db(31:40),1)')
      CALL BLAS_copy(da(41:200),db(41:200))
      bool = ALL(db == 1.0_SDK)
      ASSERT(bool, 'BLAS_copy(da(41:200),db(41:200))')
      !Test swap on "2-D vectors"
      da2=1.0_SDK
      db2=0.0_SDK
      CALL BLAS_copy(400,da2(:,1),1,db2(:,1),1) !copy the whole thing
      bool = ALL(db2 == 1.0_SDK)
      ASSERT(bool, 'BLAS_copy(400,da2(:,1),1,db2(:,1),1)')
      da2(1,:)=0.0_SDK
      CALL BLAS_copy(20,da2(:,1),db2(:,1),20) !copy first row
      bool = ALL(db2(1,:) == 0.0_SDK) .AND. ALL(db2(2:20,:) == 1.0_SDK)
      ASSERT(bool, 'BLAS_copy(20,da2(:,1),20,db2(:,1),20)')
      sa=1.0_SSK
      sb=0.0_SSK
      CALL BLAS_copy(10,sa,1,sb,1)
      bool = ALL(sb(1:10) == 1.0_SSK) .AND. ALL(sb(11:200) == 0.0_SSK)
      ASSERT(bool, 'BLAS_copy(10,sa,1,sb,1)')
      CALL BLAS_copy(10,sa(11:20),sb(11:200))
      bool = ALL(sb(1:20) == 1.0_SSK) .AND. ALL(sb(21:200) == 0.0_SSK)
      ASSERT(bool, 'BLAS_copy(10,sa(11:20),sb(11:200))')
      CALL BLAS_copy(sa(21:30),1,sb(21:30),1)
      bool = ALL(sb(1:30) == 1.0_SSK) .AND. ALL(sb(31:200) == 0.0_SSK)
      ASSERT(bool, 'BLAS_copy(sa(21:30),1,sb(21:30),1)')
      CALL BLAS_copy(sa(31:40),sb(31:40),1)
      bool = ALL(sb(1:40) == 1.0_SSK) .AND. ALL(sb(41:200) == 0.0_SSK)
      ASSERT(bool, 'BLAS_copy(sa(31:40),sb(31:40),1)')
      CALL BLAS_copy(sa(41:200),sb(41:200))
      bool = ALL(sb == 1.0_SSK)
      ASSERT(bool, 'BLAS_copy(sa(41:200),sb(41:200))')
      !Test swap on "2-D vectors"
      sa2=1.0_SSK
      sb2=0.0_SSK
      CALL BLAS_copy(400,sa2(:,1),1,sb2(:,1),1) !copy the whole thing
      bool = ALL(sb2 == 1.0_SSK)
      ASSERT(bool, 'BLAS_copy(400,sa2(:,1),1,sb2(:,1),1)')
      sa2(1,:)=0.0_SSK
      CALL BLAS_copy(20,sa2(:,1),sb2(:,1),20) !copy first row
      bool = ALL(sb2(1,:) == 0.0_SSK) .AND. ALL(sb2(2:20,:) == 1.0_SSK)
      ASSERT(bool, 'BLAS_copy(20,sa2(:,1),20,sb2(:,1),20)')
      WRITE(*,*) '  Passed: CALL BLAS_copy(...)'
  !
  !Test _nrm2
      da=0.5_SDK
      da(1)=-0.5_SDK
      dresult=BLAS_nrm2(da(1:169),1)
      bool = (dresult .APPROXEQ. 6.5_SDK)
      ASSERT(bool, 'BLAS_nrm2(da)')
      dresult=BLAS_nrm2(da)
      bool = (dresult .APPROXEQ. SQRT(50.0_SDK))
      ASSERT(bool, 'BLAS_nrm2(da)')
      dresult=BLAS_nrm2(1,da)
      bool = (dresult .APPROXEQ. 0.5_SDK)
      ASSERT(bool, 'BLAS_nrm2(1,da)')
      dresult=BLAS_nrm2(-10,da)
      bool = (dresult .APPROXEQ. 0.0_SDK)
      ASSERT(bool, 'BLAS_nrm2(-10,da)')
      da(1:20:2)=0.25_SDK
      da(5)=0.0_SDK
      da(7)=0.25_SDK*SQRT(2._SDK)
      dresult=BLAS_nrm2(10,da,2)
      bool = (dresult .APPROXEQ. SQRT(0.625_SDK))
      ASSERT(bool, 'BLAS_nrm2(10,da,2)')
      da=1.0_SDK
      da(1)=0.0_SDK
      dresult=BLAS_nrm2(10,da(1:10),1)
      bool = (dresult .APPROXEQ. 3.0_SDK)
      ASSERT(bool, 'BLAS_nrm2(10,da(1:10),1)')
      sa=0.5_SSK
      sa(1)=-0.5_SSK
      sresult=BLAS_nrm2(sa(1:169),1)
      bool = (sresult .APPROXEQ. 6.5_SSK)
      ASSERT(bool, 'BLAS_nrm2(sa)')
      sresult=BLAS_nrm2(sa)
      bool = (sresult .APPROXEQ. SQRT(50.0_SSK))
      ASSERT(bool, 'BLAS_nrm2(sa)')
      sresult=BLAS_nrm2(1,sa)
      bool = (sresult .APPROXEQ. 0.5_SSK)
      ASSERT(bool, 'BLAS_nrm2(1,sa)')
      sresult=BLAS_nrm2(-10,sa)
      bool = (sresult .APPROXEQ. 0.0_SSK)
      ASSERT(bool, 'BLAS_nrm2(-10,sa)')
      sa(1:20:2)=0.25_SSK
      sa(5)=0.0_SSK
      sa(7)=0.25_SSK*SQRT(2._SSK)
      sresult=BLAS_nrm2(10,sa,2)
      bool = (sresult .APPROXEQ. SQRT(0.625_SSK))
      ASSERT(bool, 'BLAS_nrm2(10,sa,2)')
      sa=1.0_SSK
      sa(1)=0.0_SSK
      sresult=BLAS_nrm2(10,sa(1:10),1)
      bool = (sresult .APPROXEQ. 3.0_SSK)
      ASSERT(bool, 'BLAS_nrm2(10,sa(1:10),1)')
      WRITE(*,*) '  Passed: BLAS_nrm2(...)'
  !
  !Test _asum
      da=0.5_SDK
      da(1)=-0.5
      dresult=BLAS_asum(da)
      bool = (dresult .APPROXEQ. 100.0_SDK)
      ASSERT(bool, 'BLAS_asum(da)')
      dresult=BLAS_asum(10,da)
      bool = (dresult .APPROXEQ. 5.0_SDK)
      ASSERT(bool, 'BLAS_asum(10,da)')
      dresult=BLAS_asum(0,da,1)
      bool = (dresult .APPROXEQ. 0.0_SDK)
      ASSERT(bool, 'BLAS_asum(0,da,1)')
      dresult=BLAS_asum(10,da,0)
      bool = (dresult .APPROXEQ. 0.0_SDK)
      ASSERT(bool, 'BLAS_asum(10,da,0)')
      da(1:20:2)=0.25_SDK
      dresult=BLAS_asum(da(1:10),2)
      bool = (dresult .APPROXEQ. 2.5_SDK)
      ASSERT(bool, 'BLAS_asum(10,da)')
      sa=0.5_SSK
      sa(1)=-0.5
      sresult=BLAS_asum(sa)
      bool = (sresult .APPROXEQ. 100.0_SSK)
      ASSERT(bool, 'BLAS_asum(sa)')
      sresult=BLAS_asum(10,sa)
      bool = (sresult .APPROXEQ. 5.0_SSK)
      ASSERT(bool, 'BLAS_asum(10,sa)')
      sresult=BLAS_asum(0,sa,1)
      bool = (sresult .APPROXEQ. 0.0_SSK)
      ASSERT(bool, 'BLAS_asum(0,sa,1)')
      sresult=BLAS_asum(10,sa,0)
      bool = (sresult .APPROXEQ. 0.0_SSK)
      ASSERT(bool, 'BLAS_asum(10,sa,0)')
      sa(1:20:2)=0.25_SSK
      sresult=BLAS_asum(sa(1:10),2)
      bool = (sresult .APPROXEQ. 2.5_SSK)
      ASSERT(bool, 'BLAS_asum(10,sa)')
      WRITE(*,*) '  Passed: BLAS_asum(...)'
  !
  !Test i_amin
      da=1.0_SDK
      da(100)=-5.0_SDK
      da(50)=0.5_SDK
      da(60)=0.5_SDK
      da(70)=5.0_SDK
      da(80)=5.0_SDK
      iresult=BLAS_iamin(da)
      bool = iresult == 50
      ASSERT(bool, 'BLAS_iamin(da)')
      iresult=BLAS_iamin(10,da(20:200),20)
      bool = iresult == 3
      ASSERT(bool, 'BLAS_iamin(10,da(20:200),20)')
      iresult=BLAS_iamin(da,0)
      bool = iresult == 0
      ASSERT(bool, 'BLAS_iamin(da,0)')
      iresult=BLAS_iamin(0,da(1:10))
      bool = iresult == 0
      ASSERT(bool, 'BLAS_iamin(0,da(1:10))')
      iresult=BLAS_iamin(1,da)
      bool = iresult == 1
      ASSERT(bool, 'BLAS_iamin(1,da)')
      sa=1.0_SSK
      sa(100)=-5.0_SSK
      sa(50)=0.5_SSK
      sa(60)=0.5_SSK
      sa(70)=5.0_SSK
      sa(80)=5.0_SSK
      iresult=BLAS_iamin(sa)
      bool = iresult == 50
      ASSERT(bool, 'BLAS_iamin(sa)')
      iresult=BLAS_iamin(10,sa(20:200),20)
      bool = iresult == 3
      ASSERT(bool, 'BLAS_iamin(10,sa(20:200),20)')
      iresult=BLAS_iamin(sa,0)
      bool = iresult == 0
      ASSERT(bool, 'BLAS_iamin(sa,0)')
      iresult=BLAS_iamin(0,sa(1:10))
      bool = iresult == 0
      ASSERT(bool, 'BLAS_iamin(0,sa(1:10))')
      iresult=BLAS_iamin(1,sa)
      bool = iresult == 1
      ASSERT(bool, 'BLAS_iamin(1,sa)')
      WRITE(*,*) '  Passed: BLAS_iamin(...)'
  !
  !Test i_amax
      iresult=BLAS_iamax(da)
      bool = iresult == 70
      ASSERT(bool, 'BLAS_iamax(da)')
      iresult=BLAS_iamax(10,da(20:200),20)
      bool = iresult == 4
      ASSERT(bool, 'BLAS_iamax(10,da(20:200),20)')
      iresult=BLAS_iamax(da,0)
      bool = iresult == 0
      ASSERT(bool, 'BLAS_iamax(da,0)')
      iresult=BLAS_iamax(0,da(1:10))
      bool = iresult == 0
      ASSERT(bool, 'BLAS_iamax(0,da(1:10))')
      iresult=BLAS_iamax(1,da)
      bool = iresult == 1
      ASSERT(bool, 'BLAS_iamax(1,da)')
      iresult=BLAS_iamax(sa)
      bool = iresult == 70
      ASSERT(bool, 'BLAS_iamax(sa)')
      iresult=BLAS_iamax(10,sa(20:200),20)
      bool = iresult == 4
      ASSERT(bool, 'BLAS_iamax(10,sa(20:200),20)')
      iresult=BLAS_iamax(sa,0)
      bool = iresult == 0
      ASSERT(bool, 'BLAS_iamax(sa,0)')
      iresult=BLAS_iamax(0,sa(1:10))
      bool = iresult == 0
      ASSERT(bool, 'BLAS_iamax(0,sa(1:10))')
      iresult=BLAS_iamax(1,sa)
      bool = iresult == 1
      ASSERT(bool, 'BLAS_iamax(1,sa)')
      WRITE(*,*) '  Passed: BLAS_iamax(...)'
  !
  !Test _dot
      da=2.0_SDK
      db=0.5_SDK
      dresult=BLAS_dot(da(1:199),db(1:199))
      bool = (dresult .APPROXEQ. 199.0_SDK)
      ASSERT(bool, 'BLAS_dot(da,db)')
      dresult=BLAS_dot(10,da,db,20)
      bool = (dresult .APPROXEQ. 10.0_SDK)
      ASSERT(bool, 'BLAS_dot(10,da,db,20)')
      dresult=BLAS_dot(5,da,10,db,20)
      bool = (dresult .APPROXEQ. 5.0_SDK)
      ASSERT(bool, 'BLAS_dot(5,da,10,db,20)')
      dresult=BLAS_dot(da(1:12),1,db(1:12),2)
      bool = (dresult .APPROXEQ. 12.0_SDK)
      ASSERT(bool, 'BLAS_dot(da(1:12),1,db(1:12),2)')
      dresult=BLAS_dot(da(51:61),db(51:56),-2)
      bool = (dresult .APPROXEQ. 6.0_SDK)
      ASSERT(bool, 'BLAS_dot(da(51:61),1,db(51:60),-2)')
      dresult=BLAS_dot(-10,da,db)
      bool = (dresult .APPROXEQ. 0.0_SDK)
      ASSERT(bool, 'BLAS_dot(-10,da,db)')
      sa=2.0_SSK
      sb=0.5_SSK
      sresult=BLAS_dot(sa(1:199),sb(1:199))
      bool = (sresult .APPROXEQ. 199.0_SSK)
      ASSERT(bool, 'BLAS_dot(sa,sb)')
      sresult=BLAS_dot(10,sa,sb,20)
      bool = (sresult .APPROXEQ. 10.0_SSK)
      ASSERT(bool, 'BLAS_dot(10,sa,sb,20)')
      sresult=BLAS_dot(5,sa,10,sb,20)
      bool = (sresult .APPROXEQ. 5.0_SSK)
      ASSERT(bool, 'BLAS_dot(5,sa,10,sb,20)')
      sresult=BLAS_dot(sa(1:12),1,sb(1:12),2)
      bool = (sresult .APPROXEQ. 12.0_SSK)
      ASSERT(bool, 'BLAS_dot(sa(1:12),1,sb(1:12),2)')
      sresult=BLAS_dot(sa(51:61),sb(51:56),-2)
      bool = (sresult .APPROXEQ. 6.0_SSK)
      ASSERT(bool, 'BLAS_dot(sa(51:61),1,sb(51:60),-2)')
      sresult=BLAS_dot(-10,sa,sb)
      bool = (sresult .APPROXEQ. 0.0_SSK)
      ASSERT(bool, 'BLAS_dot(-10,sa,sb)')
      WRITE(*,*) '  Passed: BLAS_dot(...)'
  !
  !Test _scal
      da=2.0_SDK
      da(200)=1.0_SDK
      CALL BLAS_scal(0.5_SDK,da(1:199))
      bool = ALL((da .APPROXEQ. 1.0_SDK))
      ASSERT(bool, 'BLAS_scal(0.5_SDK,da)')
      CALL BLAS_scal(0.5_SDK,da,-2)
      bool = ALL((da .APPROXEQ. 1.0_SDK))
      ASSERT(bool, 'BLAS_scal(0.5_SDK,da,-2)')
      CALL BLAS_scal(-5,0.5_SDK,da)
      bool = ALL((da .APPROXEQ. 1.0_SDK))
      ASSERT(bool, 'BLAS_scal(-5,0.5_SDK,da)')
      CALL BLAS_scal(100,0.5_SDK,da,2)
      bool = ALL((da(1:200:2) .APPROXEQ. 0.5_SDK)) .AND. &
        ALL((da(2:200:2) .APPROXEQ. 1.0_SDK))
      ASSERT(bool, 'BLAS_scal(100,0.5_SDK,da,2)')
      CALL BLAS_scal(50,0.5_SDK,da,1)
      bool = ALL((da(1:50:2) .APPROXEQ. 0.25_SDK)) .AND. &
        ALL((da(2:50:2) .APPROXEQ. 0.5_SDK))
      ASSERT(bool, 'BLAS_scal(50,0.5_SDK,da,1)')
      !Test vector extensions
      da=2.0_SDK
      db(1:200:2)=-0.5_SDK
      db(2:200:2)=0.5_SDK
      da(200)=1.0_SDK
      CALL BLAS_scal(db(1:199),da(1:199))
      bool = ALL((da(1:200:2) .APPROXEQ. -1.0_SDK)) .AND. &
        ALL((da(2:200:2) .APPROXEQ. 1.0_SDK))
      ASSERT(bool, 'BLAS_scal(db,da)')
      CALL BLAS_scal(db,da,-2)
      bool = ALL((da(1:200:2) .APPROXEQ. -1.0_SDK)) .AND. &
        ALL((da(2:200:2) .APPROXEQ. 1.0_SDK))
      ASSERT(bool, 'BLAS_scal(db,da,-2)')
      CALL BLAS_scal(0,db,da)
      bool = ALL((da(1:200:2) .APPROXEQ. -1.0_SDK)) .AND. &
        ALL((da(2:200:2) .APPROXEQ. 1.0_SDK))
      ASSERT(bool, 'BLAS_scal(0,db,da)')
      CALL BLAS_scal(db(1:10),da)
      bool = ALL((da(1:200:2) .APPROXEQ. -1.0_SDK)) .AND. &
        ALL((da(2:200:2) .APPROXEQ. 1.0_SDK))
      ASSERT(bool, 'BLAS_scal(0,db,da)')
      CALL BLAS_scal(100,db,da,2)
      bool = ALL((da(1:200:2) .APPROXEQ. 0.5_SDK)) .AND. &
        ALL((da(2:200:2) .APPROXEQ. 1.0_SDK))
      ASSERT(bool, 'BLAS_scal(100,db,da,2)')
      CALL BLAS_scal(50,db,da,1)
      bool = ALL((da(1:50:2) .APPROXEQ. -0.25_SDK)) .AND. &
        ALL((da(2:50:2) .APPROXEQ. 0.5_SDK))
      ASSERT(bool, 'BLAS_scal(50,db,da,1)')
      sa=2.0_SSK
      sa(200)=1.0_SSK
      CALL BLAS_scal(0.5_SSK,sa(1:199))
      bool = ALL((sa .APPROXEQ. 1.0_SSK))
      ASSERT(bool, 'BLAS_scal(0.5_SSK,sa)')
      CALL BLAS_scal(0.5_SSK,sa,-2)
      bool = ALL((sa .APPROXEQ. 1.0_SSK))
      ASSERT(bool, 'BLAS_scal(0.5_SSK,sa,-2)')
      CALL BLAS_scal(-5,0.5_SSK,sa)
      bool = ALL((sa .APPROXEQ. 1.0_SSK))
      ASSERT(bool, 'BLAS_scal(-5,0.5_SSK,sa)')
      CALL BLAS_scal(100,0.5_SSK,sa,2)
      bool = ALL((sa(1:200:2) .APPROXEQ. 0.5_SSK)) .AND. &
        ALL((sa(2:200:2) .APPROXEQ. 1.0_SSK))
      ASSERT(bool, 'BLAS_scal(100,0.5_SSK,sa,2)')
      CALL BLAS_scal(50,0.5_SSK,sa,1)
      bool = ALL((sa(1:50:2) .APPROXEQ. 0.25_SSK)) .AND. &
        ALL((sa(2:50:2) .APPROXEQ. 0.5_SSK))
      ASSERT(bool, 'BLAS_scal(50,0.5_SSK,sa,1)')
      !Test vector extensions
      sa=2.0_SSK
      sb(1:200:2)=-0.5_SSK
      sb(2:200:2)=0.5_SSK
      sa(200)=1.0_SSK
      CALL BLAS_scal(sb(1:199),sa(1:199))
      bool = ALL((sa(1:200:2) .APPROXEQ. -1.0_SSK)) .AND. &
        ALL((sa(2:200:2) .APPROXEQ. 1.0_SSK))
      ASSERT(bool, 'BLAS_scal(sb,sa)')
      CALL BLAS_scal(sb,sa,-2)
      bool = ALL((sa(1:200:2) .APPROXEQ. -1.0_SSK)) .AND. &
        ALL((sa(2:200:2) .APPROXEQ. 1.0_SSK))
      ASSERT(bool, 'BLAS_scal(sb,sa,-2)')
      CALL BLAS_scal(0,sb,sa)
      bool = ALL((sa(1:200:2) .APPROXEQ. -1.0_SSK)) .AND. &
        ALL((sa(2:200:2) .APPROXEQ. 1.0_SSK))
      ASSERT(bool, 'BLAS_scal(0,sb,sa)')
      CALL BLAS_scal(sb(1:10),sa)
      bool = ALL((sa(1:200:2) .APPROXEQ. -1.0_SSK)) .AND. &
        ALL((sa(2:200:2) .APPROXEQ. 1.0_SSK))
      ASSERT(bool, 'BLAS_scal(0,sb,sa)')
      CALL BLAS_scal(100,sb,sa,2)
      bool = ALL((sa(1:200:2) .APPROXEQ. 0.5_SSK)) .AND. &
        ALL((sa(2:200:2) .APPROXEQ. 1.0_SSK))
      ASSERT(bool, 'BLAS_scal(100,sb,sa,2)')
      CALL BLAS_scal(50,sb,sa,1)
      bool = ALL((sa(1:50:2) .APPROXEQ. -0.25_SSK)) .AND. &
        ALL((sa(2:50:2) .APPROXEQ. 0.5_SSK))
      ASSERT(bool, 'BLAS_scal(50,sb,sa,1)')
      WRITE(*,*) '  Passed: CALL BLAS_scal(...)'
  !
  !Test _axpy
      da=0.5_SDK
      db=1.0_SDK
      db(200)=2.0_SDK
      CALL BLAS_axpy(2.0_SDK,da(1:199),db(1:199))
      bool = ALL((db .APPROXEQ. 2.0_SDK))
      ASSERT(bool, 'BLAS_axpy(2.0_SDK,da(1:199),db(1:199))')
      CALL BLAS_axpy(0.0_SDK,da,db,-2)
      bool = ALL((db .APPROXEQ. 2.0_SDK))
      ASSERT(bool, 'BLAS_axpy(0.0_SDK,da,db,-2)')
      CALL BLAS_axpy(-10,5.0_SDK,da,db)
      bool = ALL((db .APPROXEQ. 2.0_SDK))
      ASSERT(bool, 'BLAS_axpy(-10,5.0_SDK,da,db)')
      CALL BLAS_axpy(5,5.0_SDK,da(1:10),db(1:10),2)
      bool = ALL((db(1:10:2) .APPROXEQ. 4.5_SDK)) .AND. &
        ALL((db(2:10:2) .APPROXEQ. 2.0_SDK))
      ASSERT(bool, 'BLAS_axpy(5,5.0_SDK,da,db,2)')
      CALL BLAS_axpy(10,2.0_SDK,da(1:10),1,db(1:10),1)
      bool = ALL((db(1:10:2) .APPROXEQ. 5.5_SDK)) .AND. &
        ALL((db(2:10:2) .APPROXEQ. 3.0_SDK))
      ASSERT(bool, 'BLAS_axpy(5,2.0_SDK,da(1:10),2,db(1:10),1)')
      da=0.5_SDK
      db=1.0_SDK
      db(200)=1.5_SDK
      CALL BLAS_axpy(da(1:199),db(1:199))
      bool = ALL((db .APPROXEQ. 1.5_SDK))
      ASSERT(bool, 'BLAS_axpy(da(1:199),db(1:199))')
      CALL BLAS_axpy(-2,da,db)
      bool = ALL((db .APPROXEQ. 1.5_SDK))
      ASSERT(bool, 'BLAS_axpy(-2,da,db)')
      CALL BLAS_axpy(5,da(1:10),db(1:10),2)
      bool = ALL((db(1:10:2) .APPROXEQ. 2.0_SDK)) .AND. &
        ALL((db(2:10:2) .APPROXEQ. 1.5_SDK))
      ASSERT(bool, 'BLAS_axpy(5,da(1:10),db(1:10),2)')
      CALL BLAS_axpy(10,da(1:10),1,db(1:10),1)
      bool = ALL((db(1:10:2) .APPROXEQ. 2.5_SDK)) .AND. &
        ALL((db(2:10:2) .APPROXEQ. 2.0_SDK))
      ASSERT(bool, 'BLAS_axpy(10,da(1:10),1,db(1:10),1)')
      da=0.5_SDK
      db=1.0_SDK
      dalpha=2.0_SDK
      dalpha(1)=0.0_SDK
      db(200)=2.0_SDK
      CALL BLAS_axpy(dalpha(1:199),da(1:199),db(1:199))
      bool = ALL((db(2:200) .APPROXEQ. 2.0_SDK)) .AND. (db(1) .APPROXEQ. 1.0_SDK)
      ASSERT(bool, 'BLAS_axpy(dalpha(1:199),da(1:199),db(1:199))')
      dalpha=0.0_SDK
      CALL BLAS_axpy(dalpha,da,db,-2)
      bool = ALL((db(2:200) .APPROXEQ. 2.0_SDK)) .AND. (db(1) .APPROXEQ. 1.0_SDK)
      ASSERT(bool, 'BLAS_axpy(dalpha,da,db,-2)')
      CALL BLAS_axpy(dalpha(1:10),da,db)
      bool = ALL((db(2:200) .APPROXEQ. 2.0_SDK)) .AND. (db(1) .APPROXEQ. 1.0_SDK)
      ASSERT(bool, 'BLAS_axpy(dalpha(1:10),da,db)')
      CALL BLAS_axpy(-10,dalpha,da,db)
      bool = ALL((db(2:200) .APPROXEQ. 2.0_SDK)) .AND. (db(1) .APPROXEQ. 1.0_SDK)
      ASSERT(bool, 'BLAS_axpy(-10,dalpha,da,db)')
      db(1)=2.0_SDK
      dalpha(1:10:2)=5.0_SDK
      CALL BLAS_axpy(5,dalpha(1:10),da(1:10),db(1:10),2)
      bool = ALL((db(1:10:2) .APPROXEQ. 4.5_SDK)) .AND. ALL((db(2:10:2) .APPROXEQ. 2.0_SDK))
      ASSERT(bool, 'BLAS_axpy(5,dalpha(1:10),da(1:10),db(1:10),2)')
      dalpha(1:10)=2.0_SDK
      CALL BLAS_axpy(10,dalpha(1:10),da(1:10),1,db(1:10),1)
      bool = ALL((db(1:10:2) .APPROXEQ. 5.5_SDK)) .AND. ALL((db(2:10:2) .APPROXEQ. 3.0_SDK))
      ASSERT(bool, 'BLAS_axpy(10,dalpha(1:10),da(1:10),1,db(1:10),1)')
      sa=0.5_SSK
      sb=1.0_SSK
      sb(200)=2.0_SSK
      CALL BLAS_axpy(2.0_SSK,sa(1:199),sb(1:199))
      bool = ALL((sb .APPROXEQ. 2.0_SSK))
      ASSERT(bool, 'BLAS_axpy(2.0_SSK,sa(1:199),sb(1:199))')
      CALL BLAS_axpy(0.0_SSK,sa,sb,-2)
      bool = ALL((sb .APPROXEQ. 2.0_SSK))
      ASSERT(bool, 'BLAS_axpy(0.0_SSK,sa,sb,-2)')
      CALL BLAS_axpy(-10,5.0_SSK,sa,sb)
      bool = ALL((sb .APPROXEQ. 2.0_SSK))
      ASSERT(bool, 'BLAS_axpy(-10,5.0_SSK,sa,sb)')
      CALL BLAS_axpy(5,5.0_SSK,sa(1:10),sb(1:10),2)
      bool = ALL((sb(1:10:2) .APPROXEQ. 4.5_SSK)) .AND. &
        ALL((sb(2:10:2) .APPROXEQ. 2.0_SSK))
      ASSERT(bool, 'BLAS_axpy(5,5.0_SSK,sa,sb,2)')
      CALL BLAS_axpy(10,2.0_SSK,sa(1:10),1,sb(1:10),1)
      bool = ALL((sb(1:10:2) .APPROXEQ. 5.5_SSK)) .AND. &
        ALL((sb(2:10:2) .APPROXEQ. 3.0_SSK))
      ASSERT(bool, 'BLAS_axpy(5,2.0_SSK,sa(1:10),2,sb(1:10),1)')
      sa=0.5_SSK
      sb=1.0_SSK
      sb(200)=1.5_SSK
      CALL BLAS_axpy(sa(1:199),sb(1:199))
      bool = ALL((sb .APPROXEQ. 1.5_SSK))
      ASSERT(bool, 'BLAS_axpy(sa(1:199),sb(1:199))')
      CALL BLAS_axpy(-2,sa,sb)
      bool = ALL((sb .APPROXEQ. 1.5_SSK))
      ASSERT(bool, 'BLAS_axpy(-2,sa,sb)')
      CALL BLAS_axpy(5,sa(1:10),sb(1:10),2)
      bool = ALL((sb(1:10:2) .APPROXEQ. 2.0_SSK)) .AND. &
        ALL((sb(2:10:2) .APPROXEQ. 1.5_SSK))
      ASSERT(bool, 'BLAS_axpy(5,sa(1:10),sb(1:10),2)')
      CALL BLAS_axpy(10,sa(1:10),1,sb(1:10),1)
      bool = ALL((sb(1:10:2) .APPROXEQ. 2.5_SSK)) .AND. &
        ALL((sb(2:10:2) .APPROXEQ. 2.0_SSK))
      ASSERT(bool, 'BLAS_axpy(10,sa(1:10),1,sb(1:10),1)')
      sa=0.5_SSK
      sb=1.0_SSK
      salpha=2.0_SSK
      salpha(1)=0.0_SSK
      sb(200)=2.0_SSK
      CALL BLAS_axpy(salpha(1:199),sa(1:199),sb(1:199))
      bool = ALL((sb(2:200) .APPROXEQ. 2.0_SSK)) .AND. (sb(1) .APPROXEQ. 1.0_SSK)
      ASSERT(bool, 'BLAS_axpy(salpha(1:199),sa(1:199),sb(1:199))')
      salpha=0.0_SSK
      CALL BLAS_axpy(salpha,sa,sb,-2)
      bool = ALL((sb(2:200) .APPROXEQ. 2.0_SSK)) .AND. (sb(1) .APPROXEQ. 1.0_SSK)
      ASSERT(bool, 'BLAS_axpy(salpha,sa,sb,-2)')
      CALL BLAS_axpy(salpha(1:10),sa,sb)
      bool = ALL((sb(2:200) .APPROXEQ. 2.0_SSK)) .AND. (sb(1) .APPROXEQ. 1.0_SSK)
      ASSERT(bool, 'BLAS_axpy(salpha(1:10),sa,sb)')
      CALL BLAS_axpy(-10,salpha,sa,sb)
      bool = ALL((sb(2:200) .APPROXEQ. 2.0_SSK)) .AND. (sb(1) .APPROXEQ. 1.0_SSK)
      ASSERT(bool, 'BLAS_axpy(-10,salpha,sa,sb)')
      sb(1)=2.0_SSK
      salpha(1:10:2)=5.0_SSK
      CALL BLAS_axpy(5,salpha(1:10),sa(1:10),sb(1:10),2)
      bool = ALL((sb(1:10:2) .APPROXEQ. 4.5_SSK)) .AND. &
        ALL((sb(2:10:2) .APPROXEQ. 2.0_SSK))
      ASSERT(bool, 'BLAS_axpy(5,salpha(1:10),sa(1:10),sb(1:10),2)')
      salpha(1:10)=2.0_SSK
      CALL BLAS_axpy(10,salpha(1:10),sa(1:10),1,sb(1:10),1)
      bool = ALL((sb(1:10:2) .APPROXEQ. 5.5_SSK)) .AND. &
        ALL((sb(2:10:2) .APPROXEQ. 3.0_SSK))
      ASSERT(bool, 'BLAS_axpy(10,salpha(1:10),sa(1:10),1,sb(1:10),1)')
      WRITE(*,*) '  Passed: CALL BLAS_axpy(...)'
    ENDSUBROUTINE testBLAS1
!
!-------------------------------------------------------------------------------
    SUBROUTINE testBLAS2()
      INTEGER(SIK) :: ia(5),ja(7),ia2(10),ja2(10)
      REAL(SSK) :: sa(128,128),sx(128),sy(128),salpha,sbeta,saa(7)
      REAL(SDK) :: da(128,128),dx(128),dy(128),dalpha,dbeta,daa(7)
      LOGICAL(SBK) :: bool
!
!Test sgemv interfaces
      sa=2.0_SSK
      sx=0.5_SSK
      salpha=3.0_SSK
      sy=0.25_SSK
      sbeta=4.0_SSK

      sx(1)=0.0_SSK
      sa(9,1)=0.0_SSK
      !sgemv_all(trans,m,n,alpha,a,lda,x,incx,beta,y,incy)
      CALL BLAS_matvec('N',4,2,salpha,sa,8,sx,2,sbeta,sy,4)
      bool = (sy(1) .APPROXEQ. 1.0_SSK) .AND. ALL((sy(2:4) .APPROXEQ. 0.25_SSK)) .AND. &
             ALL((sy(5:13:4) .APPROXEQ. 4.0_SSK)) .AND. ALL((sy(6:8) .APPROXEQ. 0.25_SSK)) .AND. &
             ALL((sy(10:12) .APPROXEQ. 0.25_SSK)) .AND. ALL((sy(14:128) .APPROXEQ. 0.25_SSK))
      ASSERT(bool, 'BLAS_matvec')
      FINFO() '(N',4,2,salpha,sa,8,sx,1,sbeta,sy,4,')'
      sy=0.25_SSK
      CALL BLAS_matvec('t',4,2,salpha,sa,8,sx,2,sbeta,sy,5)
      bool = ALL(((/sy(1),sy(6)/) .APPROXEQ. 10.0_SSK)) .AND. &
             ALL((sy(7:) .APPROXEQ. 0.25_SSK)) .AND. &
             ALL((sy(2:5) .APPROXEQ. 0.25_SSK))
      ASSERT(bool, 'BLAS_matvec')
      FINFO() '(t',4,2,salpha,sa,8,sx,2,sbeta,sy,5,')'

      sx(1)=0.5_SSK
      sa(9,1)=2.0_SSK
      sy=0.25_SSK
      !sgemv_tmnaaxby(trans,m,n,alpha,a,x,beta,y)
      CALL BLAS_matvec('T',10,128,salpha,sa,sx,0.0_SSK,sy)
      bool = ALL((sy .APPROXEQ. 30._SSK))
      ASSERT(bool, 'BLAS_matvec')
      FINFO() '(T',10,128,salpha,sa,sx,0.0_SSK,sy,')'
      sy=0.25_SSK
      CALL BLAS_matvec('N',10,128,salpha,sa,sx,1.0_SSK,sy)
      bool = ALL((sy(1:10) .APPROXEQ. 384.25_SSK)) .OR. &
        ALL((sy(11:) .APPROXEQ. 384.25_SSK))
      ASSERT(bool, 'BLAS_matvec')
      FINFO() '(N',10,128,salpha,sa,sx,1.0_SSK,sy,')'

      sy=0.25_SSK
      !sgemv_mnaaxby(m,n,alpha,a,x,beta,y)
      CALL BLAS_matvec(128,128,0.0_SSK,sa,sx,sbeta,sy)
      bool = ALL((sy .APPROXEQ. 1.0_SSK))
      ASSERT(bool, 'BLAS_matvec(128,128,salpha,sa,sx,sbeta,sy)')

      !sgemv_tnaaxby(trans,n,alpha,a,x,beta,y)
      CALL BLAS_matvec('n',32,0.0_SSK,sa,sx,0.0_SSK,sy)
      bool = ALL((sy(1:32) .APPROXEQ. 0.0_SSK)) .AND. &
        ALL((sy(33:128) .APPROXEQ. 1.0_SSK))
      ASSERT(bool, 'BLAS_matvec')
      FINFO() '(n',32,0.0_SSK,sa,sx,0.0_SSK,sy,')'

      !sgemv_naaxby(n,alpha,a,x,beta,y)
      sy=0.5_SSK
      CALL BLAS_matvec(32,0.0_SSK,sa,sx,2.0_SSK,sy)
      bool = ALL((sy(1:32) .APPROXEQ. 1.0_SSK)) .AND. &
        ALL((sy(33:128) .APPROXEQ. 0.5_SSK))
      ASSERT(bool, 'BLAS_matvec(32,0.0_SSK,sa,sx,2.0_SSK,sy)')

      !sgemv_aaxby(alpha,a,x,beta,y)
      sy=3.0_SSK
      CALL BLAS_matvec(0.0_SSK,sa,sx,1.0_SSK,sy)
      bool = ALL((sy .APPROXEQ. 3.0_SSK))
      ASSERT(bool, 'BLAS_matvec(0.0_SSK,sa,sx,1.0_SSK,sy)')
!
      sa=2.0_SSK
      sx=0.5_SSK
      salpha=3.0_SSK
      sy=0.25_SSK
      sbeta=4.0_SSK

      sx(1)=0.0_SSK
      sa(9,1)=0.0_SSK
      !sgemv_noalpha(trans,m,n,a,lda,x,incx,beta,y,incy)
      CALL BLAS_matvec('N',4,2,sa,8,sx,2,sbeta,sy,4)
      bool = (sy(1) .APPROXEQ. 1.0_SSK) .AND. ALL((sy(2:4) .APPROXEQ. 0.25_SSK)) .AND. &
        ALL((sy(5:13:4) .APPROXEQ. 2.0_SSK)) .AND. ALL((sy(6:8) .APPROXEQ. 0.25_SSK)) .AND. &
          ALL((sy(10:12) .APPROXEQ. 0.25_SSK)) .AND. ALL((sy(14:128) .APPROXEQ. 0.25_SSK))
      ASSERT(bool, 'BLAS_matvec')
      FINFO() '(N',4,2,sa,8,sx,2,sbeta,sy,4,')'
      sy=0.25_SSK
      CALL BLAS_matvec('t',4,2,sa,8,sx,2,sbeta,sy,5)
      bool = ALL(((/sy(1),sy(6)/) .APPROXEQ. 4.0_SSK)) .AND. &
             ALL((sy(7:) .APPROXEQ. 0.25_SSK)) .AND. &
             ALL((sy(2:5) .APPROXEQ. 0.25_SSK))
      ASSERT(bool, 'BLAS_matvec')
      FINFO() '(t',4,2,sa,8,sx,2,sbeta,sy,5,')'

      sx(1)=0.5_SSK
      sa(9,1)=2.0_SSK
      sy=0.25_SSK
      !sgemv_tmnaxby(trans,m,n,a,x,beta,y)
      CALL BLAS_matvec('T',10,128,sa,sx,0.0_SSK,sy)
      bool = ALL((sy .APPROXEQ. 10._SSK))
      ASSERT(bool, 'BLAS_matvec')
      FINFO() '(T',10,128,sa,sx,0.0_SSK,sy,')'
      sy=0.25_SSK
      CALL BLAS_matvec('N',10,128,sa,sx,1.0_SSK,sy)
      bool = ALL((sy(1:10) .APPROXEQ. 128.25_SSK)) .OR. &
        ALL((sy(11:) .APPROXEQ. 128.25_SSK))
      ASSERT(bool, 'BLAS_matvec')
      FINFO() '(N',10,128,sa,sx,1.0_SSK,sy,')'

      sy=0.25_SSK
      sx=0.0_SSK
      !sgemv_mnaxby(m,n,a,x,beta,y)
      CALL BLAS_matvec(128,128,sa,sx,sbeta,sy)
      bool = ALL((sy .APPROXEQ. 1.0_SSK))
      ASSERT(bool, 'BLAS_matvec(128,128,sa,sx,sbeta,sy)')

      !sgemv_tnaxby(trans,n,a,x,beta,y)
      CALL BLAS_matvec('n',32,sa,sx,0.0_SSK,sy)
      bool = ALL((sy(1:32) .APPROXEQ. 0.0_SSK)) .AND. &
        ALL((sy(33:128) .APPROXEQ. 1.0_SSK))
      ASSERT(bool, 'BLAS_matvec')
      FINFO() '(n',32,sa,sx,0.0_SSK,sy,')'

      !sgemv_naxby(n,a,x,beta,y)
      sy=0.5_SSK
      CALL BLAS_matvec(32,sa,sx,2.0_SSK,sy)
      bool = ALL((sy(1:32) .APPROXEQ. 1.0_SSK)) .AND. &
        ALL((sy(33:128) .APPROXEQ. 0.5_SSK))
      ASSERT(bool, 'BLAS_matvec(32,sa,sx,2.0_SSK,sy)')

      !sgemv_axby(a,x,beta,y)
      sy=3.0_SSK
      CALL BLAS_matvec(sa,sx,1.0_SSK,sy)
      bool = ALL((sy .APPROXEQ. 3.0_SSK))
      ASSERT(bool, 'BLAS_matvec(sa,sx,1.0_SSK,sy)')

      sa=2.0_SSK
      sx=0.5_SSK
      sy=1.0_SSK
      salpha=3.0_SSK

      sx(1)=0.0_SSK
      sa(9,1)=0.0_SSK
      !sgemv_nobeta(trans,m,n,alpha,a,lda,x,incx,y,incy)
      CALL BLAS_matvec('N',4,2,salpha,sa,8,sx,2,sy,4)
      bool = (sy(1) .APPROXEQ. 1.0_SSK) .AND. ALL((sy(2:4) .APPROXEQ. 1.0_SSK)) .AND. &
             ALL((sy(5:13:4) .APPROXEQ. 4.0_SSK)) .AND. ALL((sy(6:8) .APPROXEQ. 1.0_SSK)) .AND. &
             ALL((sy(10:12) .APPROXEQ. 1.0_SSK)) .AND. ALL((sy(14:128) .APPROXEQ. 1.0_SSK))
      ASSERT(bool, 'BLAS_matvec')
      FINFO() '(N',4,2,salpha,sa,8,sx,2,sy,4,')'
      sy=1.0_SSK
      CALL BLAS_matvec('t',4,2,salpha,sa,8,sx,2,sy,5)
      bool = ALL(((/sy(1),sy(6)/) .APPROXEQ. 10.0_SSK)) .AND. &
             ALL((sy(7:) .APPROXEQ. 1.0_SSK)) .AND. &
             ALL((sy(2:5) .APPROXEQ. 1.0_SSK))
      ASSERT(bool, 'BLAS_matvec')
      FINFO() '(t',4,2,salpha,sa,8,sx,2,sy,5,')'

      sx(1)=0.5_SSK
      sa(9,1)=2.0_SSK
      sy=0.0_SSK
      !sgemv_tmnaaxy(trans,m,n,alpha,a,x,y)
      CALL BLAS_matvec('T',10,128,salpha,sa,sx,sy)
      bool = ALL((sy .APPROXEQ. 30._SSK))
      ASSERT(bool, 'BLAS_matvec')
      FINFO() '(T',10,128,salpha,sa,sx,sy,')'
      sy=0.25_SSK
      CALL BLAS_matvec('N',10,128,salpha,sa,sx,sy)
      bool = ALL((sy(1:10) .APPROXEQ. 384.25_SSK)) .OR. &
        ALL((sy(11:) .APPROXEQ. 384.25_SSK))
      ASSERT(bool, 'BLAS_matvec')
      FINFO() '(N',10,128,salpha,sa,sx,sy,')'

      sy=1.0_SSK
      !sgemv_mnaaxy(m,n,alpha,a,x,y)
      CALL BLAS_matvec(128,128,0.0_SSK,sa,sx,sy)
      bool = ALL((sy .APPROXEQ. 1.0_SSK))
      ASSERT(bool, 'BLAS_matvec(128,128,0.0_SSK,sa,sx,sy)')

      !sgemv_tnaaxy(trans,n,alpha,a,x,y)
      CALL BLAS_matvec('n',32,0.0_SSK,sa,sx,sy)
      bool = ALL((sy .APPROXEQ. 1.0_SSK))
      ASSERT(bool, 'BLAS_matvec')
      FINFO() '(n',32,0.0_SSK,sa,sx,sy,')'

      !sgemv_naaxy(n,alpha,a,x,y)
      sy=0.5_SSK
      CALL BLAS_matvec(32,1.0_SSK,sa,sx,sy)
      bool = ALL((sy(1:32) .APPROXEQ. 32.5_SSK)) .AND. &
        ALL((sy(33:128) .APPROXEQ. 0.5_SSK))
      ASSERT(bool, 'BLAS_matvec(32,0.0_SSK,sa,sx,sy)')

      !sgemv_aaxy(alpha,a,x,y)
      sy=3.0_SSK
      CALL BLAS_matvec(0.0_SSK,sa,sx,sy)
      bool = ALL((sy .APPROXEQ. 3.0_SSK))
      ASSERT(bool, 'BLAS_matvec(0.0_SSK,sa,sx,sy)')

      sa=2.0_SSK
      sx=0.5_SSK
      sy=1.0_SSK

      sx(1)=0.0_SSK
      sa(9,1)=0.0_SSK
      !sgemv_noalphabeta(trans,m,n,a,lda,x,incx,y,incy)
      CALL BLAS_matvec('N',4,2,sa,8,sx,2,sy,4)
      bool = (sy(1) .APPROXEQ. 1.0_SSK) .AND. ALL((sy(2:4) .APPROXEQ. 1.0_SSK)) .AND. &
             ALL((sy(5:13:4) .APPROXEQ. 2.0_SSK)) .AND. ALL((sy(6:8) .APPROXEQ. 1.0_SSK)) .AND. &
             ALL((sy(10:12) .APPROXEQ. 1.0_SSK)) .AND. ALL((sy(14:128) .APPROXEQ. 1.0_SSK))
      ASSERT(bool, 'BLAS_matvec')
      FINFO() '(N',4,2,sa,8,sx,2,sy,4,')'
      sy=1.0_SSK
      CALL BLAS_matvec('t',4,2,sa,8,sx,2,sy,5)
      bool = ALL(((/sy(1),sy(6)/) .APPROXEQ. 4.0_SSK)) .AND. &
             ALL((sy(7:) .APPROXEQ. 1.0_SSK)) .AND. &
             ALL((sy(2:5) .APPROXEQ. 1.0_SSK))
      ASSERT(bool, 'BLAS_matvec')
      FINFO() '(t',4,2,sa,8,sx,2,sy,5,')'

      sx(1)=0.5_SSK
      sa(9,1)=2.0_SSK
      sy=0.0_SSK
      !sgemv_tmnaxy(trans,m,n,alpha,a,x,y)
      CALL BLAS_matvec('T',10,128,sa,sx,sy)
      bool = ALL((sy .APPROXEQ. 10._SSK))
      ASSERT(bool, 'BLAS_matvec')
      FINFO() '(T',10,128,sa,sx,sy,')'
      sy=0.25_SSK
      CALL BLAS_matvec('N',10,128,sa,sx,sy)
      bool = ALL((sy(1:10) .APPROXEQ. 128.25_SSK)) .OR. &
        ALL((sy(11:) .APPROXEQ. 0.25_SSK))
      ASSERT(bool, 'BLAS_matvec')
      FINFO() '(N',10,128,sa,sx,sy,')'

      sy=1.0_SSK
      !sgemv_mnaxy(m,n,alpha,a,x,y)
      CALL BLAS_matvec(128,128,sa,sx,sy)
      bool = ALL((sy .APPROXEQ. 129.0_SSK))
      ASSERT(bool, 'BLAS_matvec(128,128,sa,sx,sy)')

      sy=0.5_SSK
      !sgemv_tnaxy(trans,n,alpha,a,x,y)
      CALL BLAS_matvec('n',32,sa,sx,sy)
      bool = ALL((sy(1:32) .APPROXEQ. 32.5_SSK)) .AND. &
        ALL((sy(33:128) .APPROXEQ. 0.5_SSK))
      ASSERT(bool, 'BLAS_matvec')
      FINFO() '(n',32,sa,sx,sy,')'

      !sgemv_naxy(n,alpha,a,x,y)
      sy=0.5_SSK
      CALL BLAS_matvec(32,sa,sx,sy)
      bool = ALL((sy(1:32) .APPROXEQ. 32.5_SSK)) .AND. &
        ALL((sy(33:128) .APPROXEQ. 0.5_SSK))
      ASSERT(bool, 'BLAS_matvec(32,sa,sx,sy)')

      !sgemv_axy(alpha,a,x,y)
      sy=3.0_SSK
      sx=0.0_SSK
      CALL BLAS_matvec(sa,sx,sy)
      bool = ALL((sy .APPROXEQ. 3.0_SSK))
      ASSERT(bool, 'BLAS_matvec(sa,sx,sy)')

      sx=0.5_SSK
      !sgemv_tax(trans,a,x)
      CALL BLAS_matvec('t',sa(1:121,1:121),sx(1:121))
      bool = ALL((sx(1:121) .APPROXEQ. 121.0_SSK))
      ASSERT(bool, 'BLAS_matvec')
      FINFO() '(t',sa(1:121,1:121),sx(1:121),')'
      CALL BLAS_matvec('T',sa(122:128,122:128),sx(122:128))
      bool = ALL((sx(122:128) .APPROXEQ. 7.0_SSK))
      ASSERT(bool, 'BLAS_matvec')
      FINFO() '(T',sa(122:128,122:128),sx(122:128),')'

      sx=0.5_SSK
      !sgemv_ax(a,x)
      CALL BLAS_matvec(sa,sx)
      bool = ALL((sx .APPROXEQ. 128.0_SSK))
      ASSERT(bool, 'BLAS_matvec')
      FINFO() '(t',sa,sx,')'
      WRITE(*,*) '  Passed: CALL BLAS_matvec(...) sgemv'
!
!Test dgemv interfaces
      da=2.0_SDK
      dx=0.5_SDK
      dalpha=3.0_SDK
      dy=0.25_SDK
      dbeta=4.0_SDK

      dx(1)=0.0_SDK
      da(9,1)=0.0_SDK
      !dgemv_all(trans,m,n,alpha,a,lda,x,incx,beta,y,incy)
      CALL BLAS_matvec('N',4,2,dalpha,da,8,dx,2,dbeta,dy,4)
      bool = (dy(1) .APPROXEQ. 1.0_SDK) .AND. ALL((dy(2:4) .APPROXEQ. 0.25_SDK)) .AND. &
             ALL((dy(5:13:4) .APPROXEQ. 4.0_SDK)) .AND. ALL((dy(6:8) .APPROXEQ. 0.25_SDK)) .AND. &
             ALL((dy(10:12) .APPROXEQ. 0.25_SDK)) .AND. ALL((dy(14:128) .APPROXEQ. 0.25_SDK))
      ASSERT(bool, "BLAS_matvec('N',4,2,dalpha,da,8,dx,1,dbeta,dy,4)")
      dy=0.25_SDK
      CALL BLAS_matvec('t',4,2,dalpha,da,8,dx,2,dbeta,dy,5)
      bool = ALL(((/dy(1),dy(6)/) .APPROXEQ. 10.0_SDK)) .AND. &
             ALL((dy(7:) .APPROXEQ. 0.25_SDK)) .AND. &
             ALL((dy(2:5) .APPROXEQ. 0.25_SDK))
      ASSERT(bool, "BLAS_matvec('t',4,2,dalpha,da,8,dx,2,dbeta,dy,5)")

      dx(1)=0.5_SDK
      da(9,1)=2.0_SDK
      dy=0.25_SDK
      !dgemv_tmnaaxby(trans,m,n,alpha,a,x,beta,y)
      CALL BLAS_matvec('T',10,128,dalpha,da,dx,0.0_SDK,dy)
      bool = ALL((dy .APPROXEQ. 30._SDK))
      ASSERT(bool, "BLAS_matvec('T',10,128,dalpha,da,dx,0.0_SDK,dy)")
      dy=0.25_SDK
      CALL BLAS_matvec('N',10,128,dalpha,da,dx,1.0_SDK,dy)
      bool = ALL((dy(1:10) .APPROXEQ. 384.25_SDK)) .OR. &
             ALL((dy(11:) .APPROXEQ. 384.25_SDK))
      ASSERT(bool, "BLAS_matvec('N',10,128,dalpha,da,dx,1.0_SDK,dy)")

      dy=0.25_SDK
      !dgemv_mnaaxby(m,n,alpha,a,x,beta,y)
      CALL BLAS_matvec(128,128,0.0_SDK,da,dx,dbeta,dy)
      bool = ALL((dy .APPROXEQ. 1.0_SDK))
      ASSERT(bool, "BLAS_matvec(128,128,dalpha,da,dx,dbeta,dy)")

      !dgemv_tnaaxby(trans,n,alpha,a,x,beta,y)
      CALL BLAS_matvec('n',32,0.0_SDK,da,dx,0.0_SDK,dy)
      bool = ALL((dy(1:32) .APPROXEQ. 0.0_SDK)) .AND. &
             ALL((dy(33:128) .APPROXEQ. 1.0_SDK))
      ASSERT(bool, "BLAS_matvec('n',32,0.0_SDK,da,dx,0.0_SDK,dy)")

      !dgemv_naaxby(n,alpha,a,x,beta,y)
      dy=0.5_SDK
      CALL BLAS_matvec(32,0.0_SDK,da,dx,2.0_SDK,dy)
      bool = ALL((dy(1:32) .APPROXEQ. 1.0_SDK)) .AND. &
             ALL((dy(33:128) .APPROXEQ. 0.5_SDK))
      ASSERT(bool, 'BLAS_matvec(32,0.0_SDK,da,dx,2.0_SDK,dy)')

      !dgemv_aaxby(alpha,a,x,beta,y)
      dy=3.0_SDK
      CALL BLAS_matvec(0.0_SDK,da,dx,1.0_SDK,dy)
      bool = ALL((dy .APPROXEQ. 3.0_SDK))
      ASSERT(bool, 'BLAS_matvec(0.0_SDK,da,dx,1.0_SDK,dy)')
!
      da=2.0_SDK
      dx=0.5_SDK
      dalpha=3.0_SDK
      dy=0.25_SDK
      dbeta=4.0_SDK

      dx(1)=0.0_SDK
      da(9,1)=0.0_SDK
      !dgemv_noalpha(trans,m,n,a,lda,x,incx,beta,y,incy)
      CALL BLAS_matvec('N',4,2,da,8,dx,2,dbeta,dy,4)
      bool = (dy(1) .APPROXEQ. 1.0_SDK) .AND. ALL((dy(2:4) .APPROXEQ. 0.25_SDK)) .AND. &
             ALL((dy(5:13:4) .APPROXEQ. 2.0_SDK)) .AND. ALL((dy(6:8) .APPROXEQ. 0.25_SDK)) .AND. &
             ALL((dy(10:12) .APPROXEQ. 0.25_SDK)) .AND. ALL((dy(14:128) .APPROXEQ. 0.25_SDK))
      ASSERT(bool, "BLAS_matvec('N',4,2,da,8,dx,2,dbeta,dy,4)")
      dy=0.25_SDK
      CALL BLAS_matvec('t',4,2,da,8,dx,2,dbeta,dy,5)
      bool = ALL(((/dy(1),dy(6)/) .APPROXEQ. 4.0_SDK)) .AND. &
             ALL((dy(7:) .APPROXEQ. 0.25_SDK)) .AND. &
             ALL((dy(2:5) .APPROXEQ. 0.25_SDK))
      ASSERT(bool, "BLAS_matvec('t',4,2,da,8,dx,2,dbeta,dy,5)")

      dx(1)=0.5_SDK
      da(9,1)=2.0_SDK
      dy=0.25_SDK
      !dgemv_tmnaxby(trans,m,n,a,x,beta,y)
      CALL BLAS_matvec('T',10,128,da,dx,0.0_SDK,dy)
      bool = ALL((dy .APPROXEQ. 10._SDK))
      ASSERT(bool, "BLAS_matvec('T',10,128,da,dx,0.0_SDK,dy)")
      dy=0.25_SDK
      CALL BLAS_matvec('N',10,128,da,dx,1.0_SDK,dy)
      bool = ALL((dy(1:10) .APPROXEQ. 128.25_SDK)) .OR. &
             ALL((dy(11:) .APPROXEQ. 128.25_SDK))
      ASSERT(bool, "BLAS_matvec('N',10,128,da,dx,1.0_SDK,dy)")

      dy=0.25_SDK
      dx=0.0_SDK
      !dgemv_mnaxby(m,n,a,x,beta,y)
      CALL BLAS_matvec(128,128,da,dx,dbeta,dy)
      bool = ALL((dy .APPROXEQ. 1.0_SDK))
      ASSERT(bool, 'BLAS_matvec(128,128,da,dx,dbeta,dy)')

      !dgemv_tnaxby(trans,n,a,x,beta,y)
      CALL BLAS_matvec('n',32,da,dx,0.0_SDK,dy)
      bool = ALL((dy(1:32) .APPROXEQ. 0.0_SDK)) .AND. &
        ALL((dy(33:128) .APPROXEQ. 1.0_SDK))
      ASSERT(bool, "BLAS_matvec('n',32,da,dx,0.0_SDK,dy)")

      !dgemv_naxby(n,a,x,beta,y)
      dy=0.5_SDK
      CALL BLAS_matvec(32,da,dx,2.0_SDK,dy)
      bool = ALL((dy(1:32) .APPROXEQ. 1.0_SDK)) .AND. &
             ALL((dy(33:128) .APPROXEQ. 0.5_SDK))
      ASSERT(bool, 'BLAS_matvec(32,da,dx,2.0_SDK,dy)')

      !dgemv_axby(a,x,beta,y)
      dy=3.0_SDK
      CALL BLAS_matvec(da,dx,1.0_SDK,dy)
      bool = ALL((dy .APPROXEQ. 3.0_SDK))
      ASSERT(bool, 'BLAS_matvec(da,dx,1.0_SDK,dy)')

      da=2.0_SDK
      dx=0.5_SDK
      dy=1.0_SDK
      dalpha=3.0_SDK

      dx(1)=0.0_SDK
      da(9,1)=0.0_SDK
      !dgemv_nobeta(trans,m,n,alpha,a,lda,x,incx,y,incy)
      CALL BLAS_matvec('N',4,2,dalpha,da,8,dx,2,dy,4)
      bool = (dy(1) .APPROXEQ. 1.0_SDK) .AND. ALL((dy(2:4) .APPROXEQ. 1.0_SDK)) .AND. &
             ALL((dy(5:13:4) .APPROXEQ. 4.0_SDK)) .AND. ALL((dy(6:8) .APPROXEQ. 1.0_SDK)) .AND. &
             ALL((dy(10:12) .APPROXEQ. 1.0_SDK)) .AND. ALL((dy(14:128) .APPROXEQ. 1.0_SDK))
      ASSERT(bool, "BLAS_matvec('N',4,2,dalpha,da,8,dx,2,dy,4)")
      dy=1.0_SDK
      CALL BLAS_matvec('t',4,2,dalpha,da,8,dx,2,dy,5)
      bool = ALL(((/dy(1),dy(6)/) .APPROXEQ. 10.0_SDK)) .AND. &
             ALL((dy(7:) .APPROXEQ. 1.0_SDK)) .AND. &
             ALL((dy(2:5) .APPROXEQ. 1.0_SDK))
      ASSERT(bool, "BLAS_matvec('t',4,2,dalpha,da,8,dx,2,dy,5)")

      dx(1)=0.5_SDK
      da(9,1)=2.0_SDK
      dy=0.0_SDK
      !dgemv_tmnaaxy(trans,m,n,alpha,a,x,y)
      CALL BLAS_matvec('T',10,128,dalpha,da,dx,dy)
      bool = ALL((dy .APPROXEQ. 30._SDK))
      ASSERT(bool, "BLAS_matvec('T',10,128,dalpha,da,dx,dy)")
      dy=0.25_SDK
      CALL BLAS_matvec('N',10,128,dalpha,da,dx,dy)
      bool = ALL((dy(1:10) .APPROXEQ. 384.25_SDK)) .OR. &
             ALL((dy(11:) .APPROXEQ. 384.25_SDK))
      ASSERT(bool, "BLAS_matvec('N',10,128,dalpha,da,dx,dy)")

      dy=1.0_SDK
      !dgemv_mnaaxy(m,n,alpha,a,x,y)
      CALL BLAS_matvec(128,128,0.0_SDK,da,dx,dy)
      bool = ALL((dy .APPROXEQ. 1.0_SDK))
      ASSERT(bool, 'BLAS_matvec(128,128,0.0_SDK,da,dx,dy)')

      !dgemv_tnaaxy(trans,n,alpha,a,x,y)
      CALL BLAS_matvec('n',32,0.0_SDK,da,dx,dy)
      bool = ALL((dy .APPROXEQ. 1.0_SDK))
      ASSERT(bool, "BLAS_matvec('n',32,0.0_SDK,da,dx,dy)")

      !dgemv_naaxy(n,alpha,a,x,y)
      dy=0.5_SDK
      CALL BLAS_matvec(32,1.0_SDK,da,dx,dy)
      bool = ALL((dy(1:32) .APPROXEQ. 32.5_SDK)) .AND. &
             ALL((dy(33:128) .APPROXEQ. 0.5_SDK))

      !dgemv_aaxy(alpha,a,x,y)
      dy=3.0_SDK
      CALL BLAS_matvec(0.0_SDK,da,dx,dy)
      bool = ALL((dy .APPROXEQ. 3.0_SDK))
      ASSERT(bool, 'BLAS_matvec(0.0_SDK,da,dx,dy)')

      da=2.0_SDK
      dx=0.5_SDK
      dy=1.0_SDK

      dx(1)=0.0_SDK
      da(9,1)=0.0_SDK
      !dgemv_noalphabeta(trans,m,n,a,lda,x,incx,y,incy)
      CALL BLAS_matvec('N',4,2,da,8,dx,2,dy,4)
      bool = (dy(1) .APPROXEQ. 1.0_SDK) .AND. ALL((dy(2:4) .APPROXEQ. 1.0_SDK)) .AND. &
             ALL((dy(5:13:4) .APPROXEQ. 2.0_SDK)) .AND. ALL((dy(6:8) .APPROXEQ. 1.0_SDK)) .AND. &
             ALL((dy(10:12) .APPROXEQ. 1.0_SDK)) .AND. ALL((dy(14:128) .APPROXEQ. 1.0_SDK))
      ASSERT(bool, "BLAS_matvec('N',4,2,da,8,dx,2,dy,4)")
      dy=1.0_SDK
      CALL BLAS_matvec('t',4,2,da,8,dx,2,dy,5)
      bool = ALL(((/dy(1),dy(6)/) .APPROXEQ. 4.0_SDK)) .AND. &
             ALL((dy(7:) .APPROXEQ. 1.0_SDK)) .AND. &
             ALL((dy(2:5) .APPROXEQ. 1.0_SDK))
      ASSERT(bool, "BLAS_matvec('t',4,2,da,8,dx,2,dy,5)")

      dx(1)=0.5_SDK
      da(9,1)=2.0_SDK
      dy=0.0_SDK
      !dgemv_tmnaxy(trans,m,n,alpha,a,x,y)
      CALL BLAS_matvec('T',10,128,da,dx,dy)
      bool = ALL((dy .APPROXEQ. 10._SDK))
      ASSERT(bool, "BLAS_matvec('T',10,128,da,dx,dy)")
      dy=0.25_SDK
      CALL BLAS_matvec('N',10,128,da,dx,dy)
      bool = ALL((dy(1:10) .APPROXEQ. 128.25_SDK)) .OR. &
             ALL((dy(11:) .APPROXEQ. 0.25_SDK))
      ASSERT(bool, "BLAS_matvec('N',10,128,da,dx,dy)")

      dy=1.0_SDK
      !dgemv_mnaxy(m,n,alpha,a,x,y)
      CALL BLAS_matvec(128,128,da,dx,dy)
      bool = ALL((dy .APPROXEQ. 129.0_SDK))
      ASSERT(bool, 'BLAS_matvec(128,128,da,dx,dy)')

      dy=0.5_SDK
      !dgemv_tnaxy(trans,n,alpha,a,x,y)
      CALL BLAS_matvec('n',32,da,dx,dy)
      bool = ALL((dy(1:32) .APPROXEQ. 32.5_SDK)) .AND. &
             ALL((dy(33:128) .APPROXEQ. 0.5_SDK))
      ASSERT(bool, "BLAS_matvec('n',32,da,dx,dy)")

      !dgemv_naxy(n,alpha,a,x,y)
      dy=0.5_SDK
      CALL BLAS_matvec(32,da,dx,dy)
      bool = ALL((dy(1:32) .APPROXEQ. 32.5_SDK)) .AND. &
             ALL((dy(33:128) .APPROXEQ. 0.5_SDK))

      !dgemv_axy(alpha,a,x,y)
      dy=3.0_SDK
      dx=0.0_SDK
      CALL BLAS_matvec(da,dx,dy)
      bool = ALL((dy .APPROXEQ. 3.0_SDK))
      ASSERT(bool, 'BLAS_matvec(da,dx,dy)')

      dx=0.5_SDK
      !dgemv_tax(trans,a,x)
      CALL BLAS_matvec('t',da(1:121,1:121),dx(1:121))
      bool = ALL((dx(1:121) .APPROXEQ. 121.0_SDK))
      ASSERT(bool, "BLAS_matvec('t',da(1:121,1:121),dx(1:121))")
      CALL BLAS_matvec('T',da(122:128,122:128),dx(122:128))
      bool = ALL((dx(122:128) .APPROXEQ. 7.0_SDK))
      ASSERT(bool, "BLAS_matvec('T',da(122:128,122:128),dx(122:128))")

      dx=0.5_SDK
      !dgemv_ax(a,x)
      CALL BLAS_matvec(da,dx)
      bool = ALL((dx .APPROXEQ. 128.0_SDK))
      ASSERT(bool, "BLAS_matvec('t',da,dx)")
      WRITE(*,*) '  Passed: CALL BLAS_matvec(...) dgemv'
!
!Test CSR matvec routines
      ia=(/1,3,4,6,8/)
      ja=(/1,3,2,3,4,1,4/)
      saa=(/1._SSK,6._SSK,2._SSK,3._SSK,5._SSK,7._SSK,4._SSK/)
      salpha=0.5_SSK
      sbeta=2._SSK
      sx=2.0_SSK
      sy=1.5_SSK

      !scsrmv_all
      CALL BLAS_matvec(-4,7,ia,ja,saa,salpha,sx,sbeta,sy)
      bool = ALL(sy == 1.5_SSK)
      ASSERT(bool, 'BLAS_matvec(-4,7,ia,ja,saa,salpha,sx,sbeta,sy)')
      sy=23.5_SSK
      CALL BLAS_matvec(4,2,ia,ja,saa,salpha,sx,sbeta,sy)
      bool = ALL(sy == 23.5_SSK)
      ASSERT(bool, 'BLAS_matvec(4,2,ia,ja,saa,salpha,sx,sbeta,sy)')
      sy=0.5_SSK
      CALL BLAS_matvec(4,7,ia,ja,saa,0.0_SSK,sx,1.0_SSK,sy)
      bool = ALL(sy == 0.5_SSK)
      ASSERT(bool, 'BLAS_matvec(4,7,ia,ja,saa,0.0_SSK,sx,1.0_SSK,sy)')
      CALL BLAS_matvec(4,7,ia,ja,saa,salpha,sx,sbeta,sy)
      bool = ALL((sy(1:4) .APPROXEQ. (/8._SSK,3._SSK,9._SSK,12._SSK/))) .AND. &
             ALL(sy(5:128) == 0.5_SSK)
      ASSERT(bool, 'BLAS_matvec(4,7,ia,ja,saa,salpha,sx,sbeta,sy)')

      !scsrmv_noNNZ
      sy=0.5_SSK
      CALL BLAS_matvec(4,ia,ja(1:2),saa,salpha,sx,sbeta,sy)
      bool = ALL(sy == 0.5_SSK)
      ASSERT(bool, 'BLAS_matvec(4,ia,ja(1:2),saa,salpha,sx,sbeta,sy)')
      CALL BLAS_matvec(4,ia,ja,saa,salpha,sx,sbeta,sy)
      bool = ALL((sy(1:4) .APPROXEQ. (/8._SSK,3._SSK,9._SSK,12._SSK/))) .AND. &
             ALL(sy(5:128) == 0.5_SSK)
      ASSERT(bool, 'BLAS_matvec(4,ia,ja,saa,salpha,sx,sbeta,sy)')

      !scsrmv_noNNNZ
      sy=1.0_SSK
      CALL BLAS_matvec(ia,ja(1:2),saa,salpha,sx(1:4),sbeta,sy(1:4))
      bool = ALL(sy == 1.0_SSK)
      ASSERT(bool, 'BLAS_matvec(ia,ja(1:2),saa,salpha,sx(1:4),sbeta,sy(1:4))')
      CALL BLAS_matvec(ia,ja,saa,salpha,sx,sbeta,sy(1:4))
      bool = ALL(sy == 1.0_SSK)
      ASSERT(bool, 'BLAS_matvec(ia,ja,saa,salpha,sx,sbeta,sy(1:4))')
      CALL BLAS_matvec(ia,ja,saa,salpha,sx,sbeta,sy)
      bool = ALL(sy == 1.0_SSK)
      ASSERT(bool, 'BLAS_matvec(ia,ja,saa,salpha,sx,sbeta,sy)')
      CALL BLAS_matvec(ia,ja,saa,salpha,sx(1:4),1.0_SSK,sy(1:4))
      bool = ALL((sy(1:4) .APPROXEQ. (/8._SSK,3._SSK,9._SSK,12._SSK/))) .AND. &
             ALL(sy(5:128) == 1.0_SSK)
      ASSERT(bool, 'BLAS_matvec(ia,ja,saa,salpha,sx(1:4),sbeta,sy(1:4))')

      !scsrmv_noAlpha
      sy=0.5_SSK
      sx=1.0_SSK
      CALL BLAS_matvec(0,7,ia,ja,saa,sx,sbeta,sy)
      bool = ALL(sy == 0.5_SSK)
      ASSERT(bool, 'BLAS_matvec(0,7,ia,ja,saa,sx,sbeta,sy)')
      CALL BLAS_matvec(4,2,ia,ja,saa,sx,sbeta,sy)
      bool = ALL(sy == 0.5_SSK)
      ASSERT(bool, 'BLAS_matvec(4,2,ia,ja,saa,sx,sbeta,sy)')
      CALL BLAS_matvec(4,7,ia,ja,saa,sx,sbeta,sy)
      bool = ALL((sy(1:4) .APPROXEQ. (/8._SSK,3._SSK,9._SSK,12._SSK/))) .AND. &
             ALL(sy(5:128) == 0.5_SSK)
      ASSERT(bool, 'BLAS_matvec(4,7,ia,ja,saa,sx,sbeta,sy)')

      !scsrmv_noAlphaNNZ
      sy=0.5_SSK
      CALL BLAS_matvec(4,ia,ja(1:2),saa,sx,sbeta,sy)
      bool = ALL(sy == 0.5_SSK)
      ASSERT(bool, 'BLAS_matvec(4,ia,ja(1:2),saa,sx,sbeta,sy)')
      CALL BLAS_matvec(4,ia,ja,saa,sx,sbeta,sy)
      bool = ALL((sy(1:4) .APPROXEQ. (/8._SSK,3._SSK,9._SSK,12._SSK/))) .AND. &
             ALL(sy(5:128) == 0.5_SSK)
      ASSERT(bool, 'BLAS_matvec(4,ia,ja,saa,sx,sbeta,sy)')

      !scsrmv_noAlphaNNNZ
      sy=1.0_SSK
      CALL BLAS_matvec(ia,ja(1:2),saa,sx(1:4),sbeta,sy(1:4))
      bool = ALL(sy == 1.0_SSK)
      ASSERT(bool, 'BLAS_matvec(ia,ja(1:2),saa,sx(1:4),sbeta,sy(1:4))')
      CALL BLAS_matvec(ia,ja,saa,sx,sbeta,sy(1:4))
      bool = ALL(sy == 1.0_SSK)
      ASSERT(bool, 'BLAS_matvec(ia,ja,saa,sx,sbeta,sy(1:4))')
      CALL BLAS_matvec(ia,ja,saa,sx,sbeta,sy)
      bool = ALL(sy == 1.0_SSK)
      ASSERT(bool, 'BLAS_matvec(ia,ja,saa,sx,sbeta,sy)')
      CALL BLAS_matvec(ia,ja,saa,sx(1:4),1.0_SSK,sy(1:4))
      bool = ALL((sy(1:4) .APPROXEQ. (/8._SSK,3._SSK,9._SSK,12._SSK/))) .AND. &
             ALL(sy(5:128) == 1.0_SSK)
      ASSERT(bool, 'BLAS_matvec(ia,ja,saa,sx(1:4),1.0_SSK,sy(1:4))')

      !scsrmv_noBeta
      sy=1.5_SSK
      sx=2.0_SSK
      CALL BLAS_matvec(-4,7,ia,ja,saa,salpha,sx,sy)
      bool = ALL(sy == 1.5_SSK)
      ASSERT(bool, 'BLAS_matvec(-4,7,ia,ja,saa,salpha,sx,sy)')
      sy=23.5_SSK
      CALL BLAS_matvec(4,2,ia,ja,saa,salpha,sx,sy)
      bool = ALL(sy == 23.5_SSK)
      ASSERT(bool, 'BLAS_matvec(4,2,ia,ja,saa,salpha,sx,sy)')
      sy=1.0_SSK
      CALL BLAS_matvec(4,7,ia,ja,saa,0.0_SSK,sx,sy)
      bool = ALL(sy == 1.0_SSK)
      ASSERT(bool, 'BLAS_matvec(4,7,ia,ja,saa,0.0_SSK,sx,sy)')
      CALL BLAS_matvec(4,7,ia,ja,saa,salpha,sx,sy)
      bool = ALL((sy(1:4) .APPROXEQ. (/8._SSK,3._SSK,9._SSK,12._SSK/))) .AND. &
             ALL(sy(5:128) == 1.0_SSK)
      ASSERT(bool, 'BLAS_matvec(4,7,ia,ja,saa,salpha,sx,sy)')

      !scsrmv_noBetaNNZ
      sy=1.0_SSK
      CALL BLAS_matvec(4,ia,ja(1:2),saa,salpha,sx,sy)
      bool = ALL(sy == 1.0_SSK)
      ASSERT(bool, 'BLAS_matvec(4,ia,ja(1:2),saa,salpha,sx,sy)')
      CALL BLAS_matvec(4,ia,ja,saa,salpha,sx,sy)
      bool = ALL((sy(1:4) .APPROXEQ. (/8._SSK,3._SSK,9._SSK,12._SSK/))) .AND. &
             ALL(sy(5:128) == 1.0_SSK)
      ASSERT(bool, 'BLAS_matvec(4,ia,ja,saa,salpha,sx,sy)')

      !scsrmv_noBetaNNNZ
      sy=1.0_SSK
      CALL BLAS_matvec(ia,ja(1:2),saa,salpha,sx(1:4),sy(1:4))
      bool = ALL(sy == 1.0_SSK)
      ASSERT(bool, 'BLAS_matvec(ia,ja(1:2),saa,salpha,sx(1:4),sy(1:4))')
      CALL BLAS_matvec(ia,ja,saa,salpha,sx,sy(1:4))
      bool = ALL(sy == 1.0_SSK)
      ASSERT(bool, 'BLAS_matvec(ia,ja,saa,salpha,sx,sy(1:4))')
      CALL BLAS_matvec(ia,ja,saa,salpha,sx,sy)
      bool = ALL(sy == 1.0_SSK)
      ASSERT(bool, 'BLAS_matvec(ia,ja,saa,salpha,sx,sy)')
      CALL BLAS_matvec(ia,ja,saa,salpha,sx(1:4),sy(1:4))
      bool = ALL((sy(1:4) .APPROXEQ. (/8._SSK,3._SSK,9._SSK,12._SSK/))) .AND. &
             ALL(sy(5:128) == 1.0_SSK)
      ASSERT(bool, 'BLAS_matvec(ia,ja,saa,salpha,sx(1:4),sy(1:4))')

      !scsrmv_noAlphaBeta
      sy=1.5_SSK
      sx=1.0_SSK
      CALL BLAS_matvec(-4,7,ia,ja,saa,sx,sy)
      bool = ALL(sy == 1.5_SSK)
      ASSERT(bool, 'BLAS_matvec(-4,7,ia,ja,saa,sx,sy)')
      sy=23.5_SSK
      CALL BLAS_matvec(4,2,ia,ja,saa,sx,sy)
      bool = ALL(sy == 23.5_SSK)
      ASSERT(bool, 'BLAS_matvec(4,2,ia,ja,saa,sx,sy)')
      sy=1.0_SSK
      CALL BLAS_matvec(4,7,ia,ja,saa,sx,sy)
      bool = ALL((sy(1:4) .APPROXEQ. (/8._SSK,3._SSK,9._SSK,12._SSK/))) .AND. &
             ALL(sy(5:128) == 1.0_SSK)
      ASSERT(bool, 'BLAS_matvec(4,7,ia,ja,saa,sx,sy)')

      !scsrmv_noAlphaBetaNNZ
      sy=1.0_SSK
      CALL BLAS_matvec(4,ia,ja(1:2),saa,sx,sy)
      bool = ALL(sy == 1.0_SSK)
      ASSERT(bool, 'BLAS_matvec(4,ia,ja(1:2),saa,sx,sy)')
      CALL BLAS_matvec(4,ia,ja,saa,sx,sy)
      bool = ALL((sy(1:4) .APPROXEQ. (/8._SSK,3._SSK,9._SSK,12._SSK/))) .AND. &
             ALL(sy(5:128) == 1.0_SSK)
      ASSERT(bool, 'BLAS_matvec(4,ia,ja,saa,sx,sy)')

      !scsrmv_noAlphaBetaNNNZ
      sy=1.0_SSK
      CALL BLAS_matvec(ia,ja(1:2),saa,sx(1:4),sy(1:4))
      bool = ALL(sy == 1.0_SSK)
      ASSERT(bool, 'BLAS_matvec(ia,ja(1:2),saa,sx(1:4),sy(1:4))')
      CALL BLAS_matvec(ia,ja,saa,sx,sy(1:4))
      bool = ALL(sy == 1.0_SSK)
      ASSERT(bool, 'BLAS_matvec(ia,ja,saa,sx,sy(1:4))')
      CALL BLAS_matvec(ia,ja,saa,sx,sy)
      bool = ALL(sy == 1.0_SSK)
      ASSERT(bool, 'BLAS_matvec(ia,ja,saa,sx,sy)')
      CALL BLAS_matvec(ia,ja,saa,sx(1:4),sy(1:4))
      bool = ALL((sy(1:4) .APPROXEQ. (/8._SSK,3._SSK,9._SSK,12._SSK/))) .AND. &
             ALL(sy(5:128) == 1.0_SSK)
      ASSERT(bool, 'BLAS_matvec(ia,ja,saa,sx(1:4),sy(1:4))')

      ia=(/1,3,4,6,8/)
      ja=(/1,3,2,3,4,1,4/)
      daa=(/1._SDK,6._SDK,2._SDK,3._SDK,5._SDK,7._SDK,4._SDK/)
      dalpha=0.5_SDK
      dbeta=2._SDK
      dx=2.0_SDK
      dy=1.5_SDK

      !dcsrmv_all
      CALL BLAS_matvec(-4,7,ia,ja,daa,dalpha,dx,dbeta,dy)
      bool = ALL(dy == 1.5_SDK)
      ASSERT(bool, 'BLAS_matvec(-4,7,ia,ja,daa,dalpha,dx,dbeta,dy)')
      dy=23.5_SDK
      CALL BLAS_matvec(4,2,ia,ja,daa,dalpha,dx,dbeta,dy)
      bool = ALL(dy == 23.5_SDK)
      ASSERT(bool, 'BLAS_matvec(4,2,ia,ja,daa,dalpha,dx,dbeta,dy)')
      dy=0.5_SDK
      CALL BLAS_matvec(4,7,ia,ja,daa,0.0_SDK,dx,1.0_SDK,dy)
      bool = ALL(dy == 0.5_SDK)
      ASSERT(bool, 'BLAS_matvec(4,7,ia,ja,daa,0.0_SDK,dx,1.0_SDK,dy)')
      CALL BLAS_matvec(4,7,ia,ja,daa,dalpha,dx,dbeta,dy)
      bool = ALL((dy(1:4) .APPROXEQ. (/8._SDK,3._SDK,9._SDK,12._SDK/))) .AND. &
             ALL(dy(5:128) == 0.5_SDK)
      ASSERT(bool, 'BLAS_matvec(4,7,ia,ja,daa,dalpha,dx,dbeta,dy)')

      !dcsrmv_noNNZ
      dy=0.5_SDK
      CALL BLAS_matvec(4,ia,ja(1:2),daa,dalpha,dx,dbeta,dy)
      bool = ALL(dy == 0.5_SDK)
      ASSERT(bool, 'BLAS_matvec(4,ia,ja(1:2),daa,dalpha,dx,dbeta,dy)')
      CALL BLAS_matvec(4,ia,ja,daa,dalpha,dx,dbeta,dy)
      bool = ALL((dy(1:4) .APPROXEQ. (/8._SDK,3._SDK,9._SDK,12._SDK/))) .AND. &
             ALL(dy(5:128) == 0.5_SDK)
      ASSERT(bool, 'BLAS_matvec(4,ia,ja,daa,dalpha,dx,dbeta,dy)')

      !dcsrmv_noNNNZ
      dy=1.0_SDK
      CALL BLAS_matvec(ia,ja(1:2),daa,dalpha,dx(1:4),dbeta,dy(1:4))
      bool = ALL(dy == 1.0_SDK)
      ASSERT(bool, 'BLAS_matvec(ia,ja(1:2),daa,dalpha,dx(1:4),dbeta,dy(1:4))')
      CALL BLAS_matvec(ia,ja,daa,dalpha,dx,dbeta,dy(1:4))
      bool = ALL(dy == 1.0_SDK)
      ASSERT(bool, 'BLAS_matvec(ia,ja,daa,dalpha,dx,dbeta,dy(1:4))')
      CALL BLAS_matvec(ia,ja,daa,dalpha,dx,dbeta,dy)
      bool = ALL(dy == 1.0_SDK)
      ASSERT(bool, 'BLAS_matvec(ia,ja,daa,dalpha,dx,dbeta,dy)')
      CALL BLAS_matvec(ia,ja,daa,dalpha,dx(1:4),1.0_SDK,dy(1:4))
      bool = ALL((dy(1:4) .APPROXEQ. (/8._SDK,3._SDK,9._SDK,12._SDK/))) .AND. &
             ALL(dy(5:128) == 1.0_SDK)
      ASSERT(bool, 'BLAS_matvec(ia,ja,daa,dalpha,dx(1:4),dbeta,dy(1:4))')

      !dcsrmv_noAlpha
      dy=0.5_SDK
      dx=1.0_SDK
      CALL BLAS_matvec(0,7,ia,ja,daa,dx,dbeta,dy)
      bool = ALL(dy == 0.5_SDK)
      ASSERT(bool, 'BLAS_matvec(0,7,ia,ja,daa,dx,dbeta,dy)')
      CALL BLAS_matvec(4,2,ia,ja,daa,dx,dbeta,dy)
      bool = ALL(dy == 0.5_SDK)
      ASSERT(bool, 'BLAS_matvec(4,2,ia,ja,daa,dx,dbeta,dy)')
      CALL BLAS_matvec(4,7,ia,ja,daa,dx,dbeta,dy)
      bool = ALL((dy(1:4) .APPROXEQ. (/8._SDK,3._SDK,9._SDK,12._SDK/))) .AND. &
             ALL(dy(5:128) == 0.5_SDK)
      ASSERT(bool, 'BLAS_matvec(4,7,ia,ja,daa,dx,dbeta,dy)')

      !dcsrmv_noAlphaNNZ
      dy=0.5_SDK
      CALL BLAS_matvec(4,ia,ja(1:2),daa,dx,dbeta,dy)
      bool = ALL(dy == 0.5_SDK)
      ASSERT(bool, 'BLAS_matvec(4,ia,ja(1:2),daa,dx,dbeta,dy)')
      CALL BLAS_matvec(4,ia,ja,daa,dx,dbeta,dy)
      bool = ALL((dy(1:4) .APPROXEQ. (/8._SDK,3._SDK,9._SDK,12._SDK/))) .AND. &
        ALL(dy(5:128) == 0.5_SDK)
      ASSERT(bool, 'BLAS_matvec(4,ia,ja,daa,dx,dbeta,dy)')

      !dcsrmv_noAlphaNNNZ
      dy=1.0_SDK
      CALL BLAS_matvec(ia,ja(1:2),daa,dx(1:4),dbeta,dy(1:4))
      bool = ALL(dy == 1.0_SDK)
      ASSERT(bool, 'BLAS_matvec(ia,ja(1:2),daa,dx(1:4),dbeta,dy(1:4))')
      CALL BLAS_matvec(ia,ja,daa,dx,dbeta,dy(1:4))
      bool = ALL(dy == 1.0_SDK)
      ASSERT(bool, 'BLAS_matvec(ia,ja,daa,dx,dbeta,dy(1:4))')
      CALL BLAS_matvec(ia,ja,daa,dx,dbeta,dy)
      bool = ALL(dy == 1.0_SDK)
      ASSERT(bool, 'BLAS_matvec(ia,ja,daa,dx,dbeta,dy)')
      CALL BLAS_matvec(ia,ja,daa,dx(1:4),1.0_SDK,dy(1:4))
      bool = ALL((dy(1:4) .APPROXEQ. (/8._SDK,3._SDK,9._SDK,12._SDK/))) .AND. &
             ALL(dy(5:128) == 1.0_SDK)
      ASSERT(bool, 'BLAS_matvec(ia,ja,daa,dx(1:4),1.0_SDK,dy(1:4))')

      !dcsrmv_noBeta
      dy=1.5_SDK
      dx=2.0_SDK
      CALL BLAS_matvec(-4,7,ia,ja,daa,dalpha,dx,dy)
      bool = ALL(dy == 1.5_SDK)
      ASSERT(bool, 'BLAS_matvec(-4,7,ia,ja,daa,dalpha,dx,dy)')
      dy=23.5_SDK
      CALL BLAS_matvec(4,2,ia,ja,daa,dalpha,dx,dy)
      bool = ALL(dy == 23.5_SDK)
      ASSERT(bool, 'BLAS_matvec(4,2,ia,ja,daa,dalpha,dx,dy)')
      dy=1.0_SDK
      CALL BLAS_matvec(4,7,ia,ja,daa,0.0_SDK,dx,dy)
      bool = ALL(dy == 1.0_SDK)
      ASSERT(bool, 'BLAS_matvec(4,7,ia,ja,daa,0.0_SDK,dx,dy)')
      CALL BLAS_matvec(4,7,ia,ja,daa,dalpha,dx,dy)
      bool = ALL((dy(1:4) .APPROXEQ. (/8._SDK,3._SDK,9._SDK,12._SDK/))) .AND. &
             ALL(dy(5:128) == 1.0_SDK)
      ASSERT(bool, 'BLAS_matvec(4,7,ia,ja,daa,dalpha,dx,dy)')

      !dcsrmv_noBetaNNZ
      dy=1.0_SDK
      CALL BLAS_matvec(4,ia,ja(1:2),daa,dalpha,dx,dy)
      bool = ALL(dy == 1.0_SDK)
      ASSERT(bool, 'BLAS_matvec(4,ia,ja(1:2),daa,dalpha,dx,dy)')
      CALL BLAS_matvec(4,ia,ja,daa,dalpha,dx,dy)
      bool = ALL((dy(1:4) .APPROXEQ. (/8._SDK,3._SDK,9._SDK,12._SDK/))) .AND. &
             ALL(dy(5:128) == 1.0_SDK)
      ASSERT(bool, 'BLAS_matvec(4,ia,ja,daa,dalpha,dx,dy)')

      !dcsrmv_noBetaNNNZ
      dy=1.0_SDK
      CALL BLAS_matvec(ia,ja(1:2),daa,dalpha,dx(1:4),dy(1:4))
      bool = ALL(dy == 1.0_SDK)
      ASSERT(bool, 'BLAS_matvec(ia,ja(1:2),daa,dalpha,dx(1:4),dy(1:4))')
      CALL BLAS_matvec(ia,ja,daa,dalpha,dx,dy(1:4))
      bool = ALL(dy == 1.0_SDK)
      ASSERT(bool, 'BLAS_matvec(ia,ja,daa,dalpha,dx,dy(1:4))')
      CALL BLAS_matvec(ia,ja,daa,dalpha,dx,dy)
      bool = ALL(dy == 1.0_SDK)
      ASSERT(bool, 'BLAS_matvec(ia,ja,daa,dalpha,dx,dy)')
      CALL BLAS_matvec(ia,ja,daa,dalpha,dx(1:4),dy(1:4))
      bool = ALL((dy(1:4) .APPROXEQ. (/8._SDK,3._SDK,9._SDK,12._SDK/))) .AND. &
             ALL(dy(5:128) == 1.0_SDK)
      ASSERT(bool, 'BLAS_matvec(ia,ja,daa,dalpha,dx(1:4),dy(1:4))')

      !dcsrmv_noAlphaBeta
      dy=1.5_SDK
      dx=1.0_SDK
      CALL BLAS_matvec(-4,7,ia,ja,daa,dx,dy)
      bool = ALL(dy == 1.5_SDK)
      ASSERT(bool, 'BLAS_matvec(-4,7,ia,ja,daa,dx,dy)')
      dy=23.5_SDK
      CALL BLAS_matvec(4,2,ia,ja,daa,dx,dy)
      bool = ALL(dy == 23.5_SDK)
      ASSERT(bool, 'BLAS_matvec(4,2,ia,ja,daa,dx,dy)')
      dy=1.0_SDK
      CALL BLAS_matvec(4,7,ia,ja,daa,dx,dy)
      bool = ALL((dy(1:4) .APPROXEQ. (/8._SDK,3._SDK,9._SDK,12._SDK/))) .AND. &
             ALL(dy(5:128) == 1.0_SDK)
      ASSERT(bool, 'BLAS_matvec(4,7,ia,ja,daa,dx,dy)')

      !dcsrmv_noAlphaBetaNNZ
      dy=1.0_SDK
      CALL BLAS_matvec(4,ia,ja(1:2),daa,dx,dy)
      bool = ALL(dy == 1.0_SDK)
      ASSERT(bool, 'BLAS_matvec(4,ia,ja(1:2),daa,dx,dy)')
      CALL BLAS_matvec(4,ia,ja,daa,dx,dy)
      bool = ALL((dy(1:4) .APPROXEQ. (/8._SDK,3._SDK,9._SDK,12._SDK/))) .AND. &
             ALL(dy(5:128) == 1.0_SDK)
      ASSERT(bool, 'BLAS_matvec(4,ia,ja,daa,dx,dy)')

      !dcsrmv_noAlphaBetaNNNZ
      dy=1.0_SDK
      CALL BLAS_matvec(ia,ja(1:2),daa,dx(1:4),dy(1:4))
      bool = ALL(dy == 1.0_SDK)
      ASSERT(bool, 'BLAS_matvec(ia,ja(1:2),daa,dx(1:4),dy(1:4))')
      CALL BLAS_matvec(ia,ja,daa,dx,dy(1:4))
      bool = ALL(dy == 1.0_SDK)
      ASSERT(bool, 'BLAS_matvec(ia,ja,daa,dx,dy(1:4))')
      CALL BLAS_matvec(ia,ja,daa,dx,dy)
      bool = ALL(dy == 1.0_SDK)
      ASSERT(bool, 'BLAS_matvec(ia,ja,daa,dx,dy)')
      CALL BLAS_matvec(ia,ja,daa,dx(1:4),dy(1:4))
      bool = ALL((dy(1:4) .APPROXEQ. (/8._SDK,3._SDK,9._SDK,12._SDK/))) .AND. &
             ALL(dy(5:128) == 1.0_SDK)
      ASSERT(bool, 'BLAS_matvec(ia,ja,daa,dx(1:4),dy(1:4))')

      !strsv_all
      !Normal, lower triangular, non-unity diagonal
      sa(1:4,1:4)=RESHAPE((/1.0_SSK,2.0_SSK,3.0_SSK,4.0_SSK,0.0_SSK,2.0_SSK,3.0_SSK,4.0_SSK, &
        0.0_SSK,0.0_SSK,3.0_SSK,4.0_SSK,0.0_SSK,0.0_SSK,0.0_SSK,4.0_SSK/),(/4,4/))
      sx(1:4)=1.0_SSK
      sx(5:8)=(/1.0000000000_SSK,-0.500000000_SSK,-0.16666666666667_SSK,-0.08333333333333_SSK/)
      CALL BLAS_matvec('L','N','N',sa(1:4,1:4),sx(1:4))
      ASSERT(ALL(sx(1:4) .APPROXEQA. sx(5:8)),'CALL BLAS_matvec(''L'',''N'',''N'',sa(1:4,1:4),sx(1:4)')
      FINFO() 'Solution: ',sx(5:8),'  Result: ',sx(1:4)
      !Increment > 1, lower triangular, non-unity diagonal
      sa(1:4,1:4)=RESHAPE((/1.0_SSK,2.0_SSK,3.0_SSK,4.0_SSK,0.0_SSK,2.0_SSK,3.0_SSK,4.0_SSK, &
        0.0_SSK,0.0_SSK,3.0_SSK,4.0_SSK,0.0_SSK,0.0_SSK,0.0_SSK,4.0_SSK/),(/4,4/))
      sx(9:16)=1.0_SSK
      sx(5:8)=(/1.0000000000_SSK,-0.500000000_SSK,-0.16666666666667_SSK,-0.08333333333333_SSK/)
      CALL BLAS_matvec('L','N','N',sa(1:4,1:4),sx(9:16),2_SIK)
      ASSERT(ALL(sx(9:16:2) .APPROXEQA. sx(5:8)),'CALL BLAS_matvec(''L'',''N'',''N'',sa(1:4,1:4),sx(1:4),2')
      FINFO() 'Solution: ',sx(5:8),'  Result: ',sx(9:16:2)
      !Increment < 0, lower triangular, non-unity diagonal
      sa(1:4,1:4)=RESHAPE((/1.0_SSK,2.0_SSK,3.0_SSK,4.0_SSK,0.0_SSK,2.0_SSK,3.0_SSK,4.0_SSK, &
        0.0_SSK,0.0_SSK,3.0_SSK,4.0_SSK,0.0_SSK,0.0_SSK,0.0_SSK,4.0_SSK/),(/4,4/))
      sx(9:16)=1.0_SSK
      sx(5:8)=(/1.0000000000_SSK,-0.500000000_SSK,-0.16666666666667_SSK,-0.08333333333333_SSK/)
      CALL BLAS_matvec('L','N','N',sa(1:4,1:4),sx(9:16),-2_SIK)
      ASSERT(ALL(sx(15:9:-2) .APPROXEQA. sx(5:8)),'CALL BLAS_matvec(''L'',''N'',''N'',sa(1:4,1:4),sx(1:4),-2')
      FINFO() 'Solution: ',sx(5:8),'  Result: ',sx(15:9:-2)
      !Transposed, upper triangular non-unity diagonal
      sa(1:4,1:4)=RESHAPE((/1.0_SSK,2.0_SSK,3.0_SSK,4.0_SSK,0.0_SSK,2.0_SSK,3.0_SSK,4.0_SSK, &
        0.0_SSK,0.0_SSK,3.0_SSK,4.0_SSK,0.0_SSK,0.0_SSK,0.0_SSK,4.0_SSK/),(/4,4/),ORDER=(/2,1/))
      sx(1:4)=1.0_SSK
      CALL BLAS_matvec('U','T','N',sa(1:4,1:4),sx(1:4))
      ASSERT(ALL(sx(1:4) .APPROXEQA. sx(5:8)),'CALL BLAS_matvec(''U'',''T'',''N'',sa(1:4,1:4),sx(1:4)')
      FINFO() 'Solution: ',sx(5:8),'  Result: ',sx(1:4)
      !Transposed, Incrememnt > 1, upper triangular, non-unity diagonal
      sa(1:4,1:4)=RESHAPE((/1.0_SSK,2.0_SSK,3.0_SSK,4.0_SSK,0.0_SSK,2.0_SSK,3.0_SSK,4.0_SSK, &
        0.0_SSK,0.0_SSK,3.0_SSK,4.0_SSK,0.0_SSK,0.0_SSK,0.0_SSK,4.0_SSK/),(/4,4/),ORDER=(/2,1/))
      sx(9:16)=1.0_SSK
      CALL BLAS_matvec('U','T','N',sa(1:4,1:4),sx(9:16),2_SIK)
      ASSERT(ALL(sx(9:16:2) .APPROXEQA. sx(5:8)),'CALL BLAS_matvec(''U'',''T'',''N'',sa(1:4,1:4),sx(1:4),2')
      FINFO() 'Solution: ',sx(5:8),'  Result: ',sx(9:16:2)
      !Transposed, Increment < 0, upper triangular, non-unity diagonal
      sa(1:4,1:4)=RESHAPE((/1.0_SSK,2.0_SSK,3.0_SSK,4.0_SSK,0.0_SSK,2.0_SSK,3.0_SSK,4.0_SSK, &
        0.0_SSK,0.0_SSK,3.0_SSK,4.0_SSK,0.0_SSK,0.0_SSK,0.0_SSK,4.0_SSK/),(/4,4/),ORDER=(/2,1/))
      sx(9:16)=1.0_SSK
      CALL BLAS_matvec('U','T','N',sa(1:4,1:4),sx(9:16),-2_SIK)
      ASSERT(ALL(sx(15:9:-2) .APPROXEQA. sx(5:8)),'CALL BLAS_matvec(''U'',''T'',''N'',sa(1:4,1:4),sx(1:4),-2')
      FINFO() 'Solution: ',sx(5:8),'  Result: ',sx(15:9:-2)
      !Normal, Upper triangular, non-unity diagonal
      sa(1:4,1:4)=RESHAPE((/1.0_SSK,2.0_SSK,3.0_SSK,4.0_SSK,0.0_SSK,2.0_SSK,3.0_SSK,4.0_SSK, &
        0.0_SSK,0.0_SSK,3.0_SSK,4.0_SSK,0.0_SSK,0.0_SSK,0.0_SSK,4.0_SSK/),(/4,4/),ORDER=(/2,1/))
      sx(1:4)=1.0_SSK
      sx(5:8)=(/0.0000000_SSK,0.0000000_SSK,0.0000000_SSK,0.2500000_SSK/)
      CALL BLAS_matvec('U','N','N',sa(1:4,1:4),sx(1:4))
      ASSERT(ALL(sx(1:4) .APPROXEQA. sx(5:8)),'CALL BLAS_matvec(''U'',''N'',''N'',sa(1:4,1:4),sx(1:4)')
      FINFO() 'Solution: ',sx(5:8),'  Result: ',sx(1:4)
      !Increment > 1, Upper triangular, non-unity diagonal
      sa(1:4,1:4)=RESHAPE((/1.0_SSK,2.0_SSK,3.0_SSK,4.0_SSK,0.0_SSK,2.0_SSK,3.0_SSK,4.0_SSK, &
        0.0_SSK,0.0_SSK,3.0_SSK,4.0_SSK,0.0_SSK,0.0_SSK,0.0_SSK,4.0_SSK/),(/4,4/),ORDER=(/2,1/))
      sx(9:16)=1.0_SSK
      sx(5:8)=(/0.0000000_SSK,0.0000000_SSK,0.0000000_SSK,0.2500000_SSK/)
      CALL BLAS_matvec('U','N','N',sa(1:4,1:4),sx(9:16),2_SIK)
      ASSERT(ALL(sx(9:16:2) .APPROXEQA. sx(5:8)),'CALL BLAS_matvec(''U'',''N'',''N'',sa(1:4,1:4),sx(1:4),2')
      FINFO() 'Solution: ',sx(5:8),'  Result: ',sx(9:16:2)
      !Increment < 0, Upper triangular, non-unity diagonal
      sa(1:4,1:4)=RESHAPE((/1.0_SSK,2.0_SSK,3.0_SSK,4.0_SSK,0.0_SSK,2.0_SSK,3.0_SSK,4.0_SSK, &
        0.0_SSK,0.0_SSK,3.0_SSK,4.0_SSK,0.0_SSK,0.0_SSK,0.0_SSK,4.0_SSK/),(/4,4/),ORDER=(/2,1/))
      sx(9:16)=1.0_SSK
      sx(5:8)=(/0.0000000_SSK,0.0000000_SSK,0.0000000_SSK,0.2500000_SSK/)
      CALL BLAS_matvec('U','N','N',sa(1:4,1:4),sx(9:16),-2_SIK)
      ASSERT(ALL(sx(15:9:-2) .APPROXEQA. sx(5:8)),'CALL BLAS_matvec(''U'',''N'',''N'',sa(1:4,1:4),sx(1:4),-2')
      FINFO() 'Solution: ',sx(5:8),'  Result: ',sx(15:9:-2)
      !Transposed, lower triangular, non-unity diagonal
      sa(1:4,1:4)=RESHAPE((/1.0_SSK,2.0_SSK,3.0_SSK,4.0_SSK,0.0_SSK,2.0_SSK,3.0_SSK,4.0_SSK, &
        0.0_SSK,0.0_SSK,3.0_SSK,4.0_SSK,0.0_SSK,0.0_SSK,0.0_SSK,4.0_SSK/),(/4,4/))
      sx(1:4)=1.0_SSK
      CALL BLAS_matvec('L','T','N',sa(1:4,1:4),sx(1:4))
      ASSERT(ALL(sx(1:4) .APPROXEQA. sx(5:8)),'CALL BLAS_matvec(''L'',''T'',''N'',sa(1:4,1:4),sx(1:4)')
      FINFO() 'Solution: ',sx(5:8),'  Result: ',sx(1:4)
      !Transposed, increment > 1, lower triangular, non-unity diagonal
      sa(1:4,1:4)=RESHAPE((/1.0_SSK,2.0_SSK,3.0_SSK,4.0_SSK,0.0_SSK,2.0_SSK,3.0_SSK,4.0_SSK, &
        0.0_SSK,0.0_SSK,3.0_SSK,4.0_SSK,0.0_SSK,0.0_SSK,0.0_SSK,4.0_SSK/),(/4,4/))
      sx(9:16)=1.0_SSK
      CALL BLAS_matvec('L','T','N',sa(1:4,1:4),sx(9:16),2_SIK)
      ASSERT(ALL(sx(9:16:2) .APPROXEQA. sx(5:8)),'CALL BLAS_matvec(''L'',''T'',''N'',sa(1:4,1:4),sx(1:4),2')
      FINFO() 'Solution: ',sx(5:8),'  Result: ',sx(9:16:2)
      !Transposed, increment < 0, lower triangular, non-unity diagonal
      sa(1:4,1:4)=RESHAPE((/1.0_SSK,2.0_SSK,3.0_SSK,4.0_SSK,0.0_SSK,2.0_SSK,3.0_SSK,4.0_SSK, &
        0.0_SSK,0.0_SSK,3.0_SSK,4.0_SSK,0.0_SSK,0.0_SSK,0.0_SSK,4.0_SSK/),(/4,4/))
      sx(9:16)=1.0_SSK
      CALL BLAS_matvec('L','T','N',sa(1:4,1:4),sx(9:16),-2_SIK)
      ASSERT(ALL(sx(15:9:-2) .APPROXEQA. sx(5:8)),'CALL BLAS_matvec(''L'',''T'',''N'',sa(1:4,1:4),sx(1:4),-2')
      FINFO() 'Solution: ',sx(5:8),'  Result: ',sx(15:9:2)

      !dtrsv_all
      !Normal, lower triangular, non-unity diagonal
      da(1:4,1:4)=RESHAPE((/1.0_SDK,2.0_SDK,3.0_SDK,4.0_SDK,0.0_SDK,2.0_SDK,3.0_SDK,4.0_SDK, &
        0.0_SDK,0.0_SDK,3.0_SDK,4.0_SDK,0.0_SDK,0.0_SDK,0.0_SDK,4.0_SDK/),(/4,4/))
      dx(1:4)=1.0_SDK
      dx(5:8)=(/1.0000000000_SDK,-0.500000000_SDK,-0.16666666666667_SDK,-0.08333333333333_SDK/)
      CALL BLAS_matvec('L','N','N',da(1:4,1:4),dx(1:4))
      ASSERT(ALL(dx(1:4) .APPROXEQA. dx(5:8)),'CALL BLAS_matvec(''L'',''N'',''N'',da(1:4,1:4),dx(1:4)')
      FINFO() 'Solution: ',dx(5:8),'  Result: ',dx(1:4)
      !Increment > 1, lower triangular, non-unity diagonal
      da(1:4,1:4)=RESHAPE((/1.0_SDK,2.0_SDK,3.0_SDK,4.0_SDK,0.0_SDK,2.0_SDK,3.0_SDK,4.0_SDK, &
        0.0_SDK,0.0_SDK,3.0_SDK,4.0_SDK,0.0_SDK,0.0_SDK,0.0_SDK,4.0_SDK/),(/4,4/))
      dx(9:16)=1.0_SDK
      dx(5:8)=(/1.0000000000_SDK,-0.500000000_SDK,-0.16666666666667_SDK,-0.08333333333333_SDK/)
      CALL BLAS_matvec('L','N','N',da(1:4,1:4),dx(9:16),2_SIK)
      ASSERT(ALL(dx(9:16:2) .APPROXEQA. dx(5:8)),'CALL BLAS_matvec(''L'',''N'',''N'',da(1:4,1:4),dx(1:4),2')
      FINFO() 'Solution: ',dx(5:8),'  Result: ',dx(9:16:2)
      !Increment < 0, lower triangular, non-unity diagonal
      da(1:4,1:4)=RESHAPE((/1.0_SDK,2.0_SDK,3.0_SDK,4.0_SDK,0.0_SDK,2.0_SDK,3.0_SDK,4.0_SDK, &
        0.0_SDK,0.0_SDK,3.0_SDK,4.0_SDK,0.0_SDK,0.0_SDK,0.0_SDK,4.0_SDK/),(/4,4/))
      dx(9:16)=1.0_SDK
      dx(5:8)=(/1.0000000000_SDK,-0.500000000_SDK,-0.16666666666667_SDK,-0.08333333333333_SDK/)
      CALL BLAS_matvec('L','N','N',da(1:4,1:4),dx(9:16),-2_SIK)
      ASSERT(ALL(dx(15:9:-2) .APPROXEQA. dx(5:8)),'CALL BLAS_matvec(''L'',''N'',''N'',da(1:4,1:4),dx(1:4),-2')
      FINFO() 'Solution: ',dx(5:8),'  Result: ',dx(15:9:-2)
      !Transposed, upper triangular non-unity diagonal
      da(1:4,1:4)=RESHAPE((/1.0_SDK,2.0_SDK,3.0_SDK,4.0_SDK,0.0_SDK,2.0_SDK,3.0_SDK,4.0_SDK, &
        0.0_SDK,0.0_SDK,3.0_SDK,4.0_SDK,0.0_SDK,0.0_SDK,0.0_SDK,4.0_SDK/),(/4,4/),ORDER=(/2,1/))
      dx(1:4)=1.0_SDK
      CALL BLAS_matvec('U','T','N',da(1:4,1:4),dx(1:4))
      ASSERT(ALL(dx(1:4) .APPROXEQA. dx(5:8)),'CALL BLAS_matvec(''U'',''T'',''N'',da(1:4,1:4),dx(1:4)')
      FINFO() 'Solution: ',dx(5:8),'  Result: ',dx(1:4)
      !Transposed, Incrememnt > 1, upper triangular, non-unity diagonal
      da(1:4,1:4)=RESHAPE((/1.0_SDK,2.0_SDK,3.0_SDK,4.0_SDK,0.0_SDK,2.0_SDK,3.0_SDK,4.0_SDK, &
        0.0_SDK,0.0_SDK,3.0_SDK,4.0_SDK,0.0_SDK,0.0_SDK,0.0_SDK,4.0_SDK/),(/4,4/),ORDER=(/2,1/))
      dx(9:16)=1.0_SDK
      CALL BLAS_matvec('U','T','N',da(1:4,1:4),dx(9:16),2_SIK)
      ASSERT(ALL(dx(9:16:2) .APPROXEQA. dx(5:8)),'CALL BLAS_matvec(''U'',''T'',''N'',da(1:4,1:4),dx(1:4),2')
      FINFO() 'Solution: ',dx(5:8),'  Result: ',dx(9:16:2)
      !Transposed, Increment < 0, upper triangular, non-unity diagonal
      da(1:4,1:4)=RESHAPE((/1.0_SDK,2.0_SDK,3.0_SDK,4.0_SDK,0.0_SDK,2.0_SDK,3.0_SDK,4.0_SDK, &
        0.0_SDK,0.0_SDK,3.0_SDK,4.0_SDK,0.0_SDK,0.0_SDK,0.0_SDK,4.0_SDK/),(/4,4/),ORDER=(/2,1/))
      dx(9:16)=1.0_SDK
      CALL BLAS_matvec('U','T','N',da(1:4,1:4),dx(9:16),-2_SIK)
      ASSERT(ALL(dx(15:9:-2) .APPROXEQA. dx(5:8)),'CALL BLAS_matvec(''U'',''T'',''N'',da(1:4,1:4),dx(1:4),-2')
      FINFO() 'Solution: ',dx(5:8),'  Result: ',dx(15:9:-2)
      !Normal, Upper triangular, non-unity diagonal
      da(1:4,1:4)=RESHAPE((/1.0_SDK,2.0_SDK,3.0_SDK,4.0_SDK,0.0_SDK,2.0_SDK,3.0_SDK,4.0_SDK, &
        0.0_SDK,0.0_SDK,3.0_SDK,4.0_SDK,0.0_SDK,0.0_SDK,0.0_SDK,4.0_SDK/),(/4,4/),ORDER=(/2,1/))
      dx(1:4)=1.0_SDK
      dx(5:8)=(/0.0000000_SDK,0.0000000_SDK,0.0000000_SDK,0.2500000_SDK/)
      CALL BLAS_matvec('U','N','N',da(1:4,1:4),dx(1:4))
      ASSERT(ALL(dx(1:4) .APPROXEQA. dx(5:8)),'CALL BLAS_matvec(''U'',''N'',''N'',da(1:4,1:4),dx(1:4)')
      FINFO() 'Solution: ',dx(5:8),'  Result: ',dx(1:4)
      !Increment > 1, Upper triangular, non-unity diagonal
      da(1:4,1:4)=RESHAPE((/1.0_SDK,2.0_SDK,3.0_SDK,4.0_SDK,0.0_SDK,2.0_SDK,3.0_SDK,4.0_SDK, &
        0.0_SDK,0.0_SDK,3.0_SDK,4.0_SDK,0.0_SDK,0.0_SDK,0.0_SDK,4.0_SDK/),(/4,4/),ORDER=(/2,1/))
      dx(9:16)=1.0_SDK
      dx(5:8)=(/0.0000000_SDK,0.0000000_SDK,0.0000000_SDK,0.2500000_SDK/)
      CALL BLAS_matvec('U','N','N',da(1:4,1:4),dx(9:16),2_SIK)
      ASSERT(ALL(dx(9:16:2) .APPROXEQA. dx(5:8)),'CALL BLAS_matvec(''U'',''N'',''N'',da(1:4,1:4),dx(1:4),2')
      FINFO() 'Solution: ',dx(5:8),'  Result: ',dx(9:16:2)
      !Increment < 0, Upper triangular, non-unity diagonal
      da(1:4,1:4)=RESHAPE((/1.0_SDK,2.0_SDK,3.0_SDK,4.0_SDK,0.0_SDK,2.0_SDK,3.0_SDK,4.0_SDK, &
        0.0_SDK,0.0_SDK,3.0_SDK,4.0_SDK,0.0_SDK,0.0_SDK,0.0_SDK,4.0_SDK/),(/4,4/),ORDER=(/2,1/))
      dx(9:16)=1.0_SDK
      dx(5:8)=(/0.0000000_SDK,0.0000000_SDK,0.0000000_SDK,0.2500000_SDK/)
      CALL BLAS_matvec('U','N','N',da(1:4,1:4),dx(9:16),-2_SIK)
      ASSERT(ALL(dx(15:9:-2) .APPROXEQA. dx(5:8)),'CALL BLAS_matvec(''U'',''N'',''N'',da(1:4,1:4),dx(1:4),-2')
      FINFO() 'Solution: ',dx(5:8),'  Result: ',dx(15:9:-2)
      !Transposed, lower triangular, non-unity diagonal
      da(1:4,1:4)=RESHAPE((/1.0_SDK,2.0_SDK,3.0_SDK,4.0_SDK,0.0_SDK,2.0_SDK,3.0_SDK,4.0_SDK, &
        0.0_SDK,0.0_SDK,3.0_SDK,4.0_SDK,0.0_SDK,0.0_SDK,0.0_SDK,4.0_SDK/),(/4,4/))
      dx(1:4)=1.0_SDK
      CALL BLAS_matvec('L','T','N',da(1:4,1:4),dx(1:4))
      ASSERT(ALL(dx(1:4) .APPROXEQA. dx(5:8)),'CALL BLAS_matvec(''L'',''T'',''N'',da(1:4,1:4),dx(1:4)')
      FINFO() 'Solution: ',dx(5:8),'  Result: ',dx(1:4)
      !Transposed, increment > 1, lower triangular, non-unity diagonal
      da(1:4,1:4)=RESHAPE((/1.0_SDK,2.0_SDK,3.0_SDK,4.0_SDK,0.0_SDK,2.0_SDK,3.0_SDK,4.0_SDK, &
        0.0_SDK,0.0_SDK,3.0_SDK,4.0_SDK,0.0_SDK,0.0_SDK,0.0_SDK,4.0_SDK/),(/4,4/))
      dx(9:16)=1.0_SDK
      CALL BLAS_matvec('L','T','N',da(1:4,1:4),dx(9:16),2_SIK)
      ASSERT(ALL(dx(9:16:2) .APPROXEQA. dx(5:8)),'CALL BLAS_matvec(''L'',''T'',''N'',da(1:4,1:4),dx(1:4),2')
      FINFO() 'Solution: ',dx(5:8),'  Result: ',dx(9:16:2)
      !Transposed, increment < 0, lower triangular, non-unity diagonal
      da(1:4,1:4)=RESHAPE((/1.0_SDK,2.0_SDK,3.0_SDK,4.0_SDK,0.0_SDK,2.0_SDK,3.0_SDK,4.0_SDK, &
        0.0_SDK,0.0_SDK,3.0_SDK,4.0_SDK,0.0_SDK,0.0_SDK,0.0_SDK,4.0_SDK/),(/4,4/))
      dx(9:16)=1.0_SDK
      CALL BLAS_matvec('L','T','N',da(1:4,1:4),dx(9:16),-2_SIK)
      ASSERT(ALL(dx(15:9:-2) .APPROXEQA. dx(5:8)),'CALL BLAS_matvec(''L'',''T'',''N'',da(1:4,1:4),dx(1:4),-2')
      FINFO() 'Solution: ',dx(5:8),'  Result: ',dx(15:9:2)

    ENDSUBROUTINE testBLAS2
!
!-------------------------------------------------------------------------------
    SUBROUTINE testBLAS3()
      REAL(SSK) :: sa(128,128),sb(128,128),sc(128,128),salpha,sbeta
      REAL(SDK) :: da(128,128),db(128,128),dc(128,128),dalpha,dbeta
      REAL(SSK) :: sa2(128,128),sb2(128,128),sc2(128,128),salpha2,sbeta2
      REAL(SDK) :: da2(128,128),db2(128,128),dc2(128,128),dalpha2,dbeta2
      LOGICAL(SBK) :: bool
!
!Test sgemm interfaces
      sa=2.0_SSK
      sb=3.0_SSK
      sc=4.0_SSK
      salpha=0.0_SSK
      sbeta=0.0_SSK

!Secondary Test Variables
      sa2=2.0_SSK
      sb2=3.0_SSK
      sc2=4.0_SSK
      salpha2=2.0_SSK
      sbeta2=2.5_SSK

      !sgemm_all(transa,transb,m,n,k,alpha,a,lda,b,ldb,beta,c,ldc)
      !Check beta=0 and alpha=0 returns all zeros for the bounds specified
      CALL BLAS_matmat('N','n',4,4,4,salpha,sa,4,sb,4,sbeta,sc,4)
      bool = ALL((sc(1:16,1) .APPROXEQ. 0.0_SSK)) .OR. &
             ALL((sc(17:128,1) .APPROXEQ. 4.0_SSK)) .OR. &
             ALL((sc(:,2:128) .APPROXEQ. 4.0_SSK))
      ASSERT(bool, "BLAS_matmat('N','N',4,4,4,salpha,sa,4,sb,4,sbeta,sc,4)")
      !Check beta=0 and alpha=0 returns all zeros for the bounds specified, n*m>10000
      CALL BLAS_matmat('n','N',101,101,101,salpha,sa,101,sb,101,sbeta,sc,101)
      bool = ALL((sc(1:128,1:79) .APPROXEQ. 0.0_SSK)) .OR. &
             ALL((sc(1:89,80) .APPROXEQ. 0.0_SSK)) .OR. &
             ALL((sc(90:128,80) .APPROXEQ. 4.0_SSK)) .OR. &
             ALL((sc(90:128,80) .APPROXEQ. 4.0_SSK))
      ASSERT(bool, "BLAS_matmat('N','N',101,101,101,salpha,sa,101,sb,101,sbeta,sc,101))")
      !Check beta=4 and alpha=0 returns 16.0 for the bounds specified
      sc=4.0_SSK
      sbeta=4.0_SSK
      CALL BLAS_matmat('N','N',4,2,4,salpha,sa,4,sb,4,sbeta,sc,4)
      bool = ALL((sc(1:8,1) .APPROXEQ. 16.0_SSK)) .OR. &
             ALL((sc(17:128,1) .APPROXEQ. 4.0_SSK)) .OR. &
             ALL((sc(:,2:128) .APPROXEQ. 4.0_SSK))
      ASSERT(bool, "BLAS_matmat('N','N',4,4,4,salpha,sa,4,sb,4,sbeta,sc,4)")
      !Check beta=4 and alpha=0 returns 16.0 for the bounds specified, n*m>10000
      sc=4.0_SSK
      CALL BLAS_matmat('N','N',101,101,101,salpha,sa,101,sb,101,sbeta,sc,101)
      bool = ALL((sc(1:128,1:79) .APPROXEQ. 16.0_SSK)) .OR. &
             ALL((sc(1:89,80) .APPROXEQ. 16.0_SSK)) .OR. &
             ALL((sc(90:128,80) .APPROXEQ. 4.0_SSK)) .OR. &
             ALL((sc(90:128,80) .APPROXEQ. 4.0_SSK))
      ASSERT(bool, "BLAS_matmat('N','N',101,101,101,salpha,sa,101,sb,101,sbeta,sc,101)")
      !Check beta=0 and alpha=2 returns 36.0 for the bounds specified
      salpha=2.0_SSK
      sc=4.0_SSK
      sbeta=0.0_SSK
      CALL BLAS_matmat('N','N',3,5,3,salpha,sa,3,sb,5,sbeta,sc,3)
      bool = ALL((sc(1:15,1) .APPROXEQ. 36.0_SSK)) .OR. &
             ALL((sc(16:128,1) .APPROXEQ. 4.0_SSK)) .OR. &
             ALL((sc(:,2:128) .APPROXEQ. 4.0_SSK))
      ASSERT(bool, "BLAS_matmat('N','N',3,5,3,salpha,sa,101,sb,5,sbeta,sc,3)")
      !Check beta=2.5 and alpha=2 returns 46.0 for the bounds specified
      sc=4.0_SSK
      sbeta=2.5_SSK
      CALL BLAS_matmat('N','N',3,5,3,salpha,sa,3,sb,3,sbeta,sc,3)
      bool = ALL((sc(1:15,1) .APPROXEQ. 46.0_SSK)) .OR. &
             ALL((sc(16:128,1) .APPROXEQ. 4.0_SSK)) .OR. &
             ALL((sc(:,2:128) .APPROXEQ. 4.0_SSK))
      ASSERT(bool, "BLAS_matmat('N','N',3,5,3,salpha,sa,3,sb,3,sbeta,sc,3)")

      !Check Single Transpose
      sc=4.0_SSK
      sbeta=2.5_SSK
      CALL BLAS_matmat('t','N',3,3,3,salpha,sa,3,sb,3,sbeta,sc,3)
      bool = ALL((sc(1:15,1) .APPROXEQ. 46.0_SSK)) .OR. &
             ALL((sc(16:128,1) .APPROXEQ. 4.0_SSK)) .OR. &
             ALL((sc(:,2:128) .APPROXEQ. 4.0_SSK))
      ASSERT(bool, "BLAS_matmat('T','N',3,3,3,salpha,sa,3,sb,3,sbeta,sc,3)")
      !Check Second Single Transpose
      sc=4.0_SSK
      sbeta=2.5_SSK
      CALL BLAS_matmat('N','T',3,3,3,salpha,sa,3,sb,3,sbeta,sc,3)
      bool = ALL((sc(1:15,1) .APPROXEQ. 46.0_SSK)) .OR. &
             ALL((sc(16:128,1) .APPROXEQ. 4.0_SSK)) .OR. &
             ALL((sc(:,2:128) .APPROXEQ. 4.0_SSK))
      ASSERT(bool, "BLAS_matmat('N','T',3,3,3,salpha,sa,3,sb,3,sbeta,sc,3)")
      !Check Both Transpose
      sc=4.0_SSK
      sbeta=2.5_SSK
      CALL BLAS_matmat('T','t',3,3,3,salpha,sa,3,sb,3,sbeta,sc,3)
      bool = ALL((sc(1:15,1) .APPROXEQ. 46.0_SSK)) .OR. &
             ALL((sc(16:128,1) .APPROXEQ. 4.0_SSK)) .OR. &
             ALL((sc(:,2:128) .APPROXEQ. 4.0_SSK))
      ASSERT(bool, "BLAS_matmat('T','T',3,3,3,salpha,sa,3,sb,3,sbeta,sc,3)")

      !With Beta = 0
      !Check Single Transpose
      sc=4.0_SSK
      sbeta=0.0_SSK
      CALL BLAS_matmat('T','N',3,3,3,salpha,sa,3,sb,3,sbeta,sc,3)
      bool = ALL((sc(1:15,1) .APPROXEQ. 46.0_SSK)) .OR. &
             ALL((sc(16:128,1) .APPROXEQ. 4.0_SSK)) .OR. &
             ALL((sc(:,2:128) .APPROXEQ. 4.0_SSK))
      ASSERT(bool, "BLAS_matmat('T','N',3,3,3,salpha,sa,3,sb,3,sbeta,sc,3)")
      !Check Second Single Transpose
      sc=4.0_SSK
      CALL BLAS_matmat('N','T',3,3,3,salpha,sa,3,sb,3,sbeta,sc,3)
      bool = ALL((sc(1:15,1) .APPROXEQ. 46.0_SSK)) .OR. &
             ALL((sc(16:128,1) .APPROXEQ. 4.0_SSK)) .OR. &
             ALL((sc(:,2:128) .APPROXEQ. 4.0_SSK))
      ASSERT(bool, "BLAS_matmat('N','T',3,3,3,salpha,sa,3,sb,3,sbeta,sc,3)")
      !Check Both Transpose
      sc=4.0_SSK
      CALL BLAS_matmat('T','T',3,3,3,salpha,sa,3,sb,3,sbeta,sc,3)
      bool = ALL((sc(1:15,1) .APPROXEQ. 46.0_SSK)) .OR. &
             ALL((sc(16:128,1) .APPROXEQ. 4.0_SSK)) .OR. &
             ALL((sc(:,2:128) .APPROXEQ. 4.0_SSK))
      ASSERT(bool, "BLAS_matmat('T','T',3,3,3,salpha,sa,3,sb,3,sbeta,sc,3)")

      sc=4.0_SSK
      sbeta=2.5_SSK
      CALL BLAS_matmat('N','N',3,5,3,salpha,sa,3,sb,3,sbeta,sc,3)
      !Check sgemm_ttmnkaabbc
      CALL BLAS_matmat('N','N',3,5,3,salpha2,sa2,sb2,sbeta2,sc2)
      bool = ALL((sc(:,:) .APPROXEQ. sc2(:,:)))
      ASSERT(bool, "BLAS_matmat('N','N',3,5,3,salpha2,sa2,sb2,sbeta2,sc2)")

      !Check sgemm_ttmnkabbc
      sa= 2.0_SSK; sb= 3.0_SSK; sc= 4.0_SSK; salpha= 1.0_SSK; sbeta= 2.5_SSK
      sa2=2.0_SSK; sb2=3.0_SSK; sc2=4.0_SSK; salpha2=2.0_SSK; sbeta2=2.5_SSK

      CALL BLAS_matmat('N','N',3,5,3,salpha,sa,3,sb,3,sbeta,sc,3)

      CALL BLAS_matmat('N','N',3,5,3,sa2,sb2,sbeta2,sc2)
      bool = ALL((sc(:,:) .APPROXEQ. sc2(:,:)))
      ASSERT(bool, "BLAS_matmat('N','N',3,5,3,sa2,sb2,sbeta2,sc2)")

      !Check sgemm_ttmnkaabc
      sc= 4.0_SSK; salpha= 2.0_SSK; sbeta= 1.0_SSK
      sc2=4.0_SSK; salpha2=2.0_SSK; sbeta2=1.0_SSK

      CALL BLAS_matmat('N','N',3,5,3,salpha,sa,3,sb,3,sbeta,sc,3)

      CALL BLAS_matmat('N','N',3,5,3,salpha,sa2,sb2,sc2)
      bool = ALL((sc(:,:) .APPROXEQ. sc2(:,:)))
      ASSERT(bool, "BLAS_matmat('N','N',3,5,3,salpha2,sa2,sb2,sc2)")

      !Check sgemm_ttmnkabc
      sc= 4.0_SSK; salpha= 1.0_SSK; sbeta= 1.0_SSK
      sc2=4.0_SSK; salpha2=1.0_SSK; sbeta2=1.0_SSK

      CALL BLAS_matmat('n','N',3,5,3,salpha,sa,3,sb,3,sbeta,sc,3)

      CALL BLAS_matmat('N','n',3,5,3,sa2,sb2,sc2)
      bool = ALL((sc(:,:) .APPROXEQ. sc2(:,:)))
      ASSERT(bool, "BLAS_matmat('N','N',3,5,3,sa2,sb2,sc2)")

      !Check sgemm_mnkaabbc
      sc= 4.0_SSK; salpha= 2.0_SSK; sbeta= 2.5_SSK
      sc2=4.0_SSK; salpha2=2.0_SSK; sbeta2=2.5_SSK

      CALL BLAS_matmat('N','N',3,5,3,salpha,sa,3,sb,3,sbeta,sc,1)

      CALL BLAS_matmat(3,5,3,salpha2,sa2,sb2,sbeta2,sc2)
      bool = ALL((sc(:,:) .APPROXEQ. sc2(:,:)))
      ASSERT(bool, "BLAS_matmat('N','N',3,5,3,sa2,sb2,sc2)")

      !Check sgemm_ttaabbc
      sc= 4.0_SSK; salpha= 2.0_SSK; sbeta= 2.5_SSK
      sc2=4.0_SSK; salpha2=2.0_SSK; sbeta2=2.5_SSK

      CALL BLAS_matmat('N','N',SIZE(sc,DIM=1),SIZE(sc,DIM=2),SIZE(sb,DIM=1),salpha, &
        sa,SIZE(sc,DIM=1),sb,SIZE(sb,DIM=1),sbeta,sc,SIZE(sc,DIM=1))

      CALL BLAS_matmat('N','N',salpha2,sa2,sb2,sbeta2,sc2)
      bool = ALL((sc(:,:) .APPROXEQ. sc2(:,:)))
      ASSERT(bool, "BLAS_matmat('N','N',salpha2,sa2,sb2,sbeta2,sc2)")

      !Check sgemm_aabbc
      sc= 4.0_SSK; salpha= 2.0_SSK; sbeta= 2.5_SSK
      sc2=4.0_SSK; salpha2=2.0_SSK; sbeta2=2.5_SSK

      CALL BLAS_matmat('N','N',SIZE(sc,DIM=1),SIZE(sc,DIM=2),SIZE(sb,DIM=1),salpha, &
        sa,SIZE(sc,DIM=1),sb,SIZE(sb,DIM=1),sbeta,sc,1)

      CALL BLAS_matmat(salpha2,sa2,sb2,sbeta2,sc2)
      bool = ALL((sc(:,:) .APPROXEQ. sc2(:,:)))
      ASSERT(bool, "BLAS_matmat(salpha2,sa2,sb2,sbeta2,sc2)")

      !Check sgemm_abbc
      sc= 4.0_SSK; salpha= 1.0_SSK; sbeta= 2.5_SSK
      sc2=4.0_SSK; salpha2=1.0_SSK; sbeta2=2.5_SSK

      CALL BLAS_matmat('N','N',SIZE(sc,DIM=1),SIZE(sc,DIM=2),SIZE(sb,DIM=1),salpha, &
        sa,SIZE(sc,DIM=1),sb,SIZE(sb,DIM=1),sbeta,sc,1)

      CALL BLAS_matmat(sa2,sb2,sbeta2,sc2)
      bool = ALL((sc(:,:) .APPROXEQ. sc2(:,:)))
      ASSERT(bool, "BLAS_matmat(salpha2,sa2,sb2,sbeta2,sc2)")

      !Check sgemm_aabc
      sc= 4.0_SSK; salpha= 2.0_SSK; sbeta= 1.0_SSK
      sc2=4.0_SSK; salpha2=2.0_SSK; sbeta2=1.0_SSK

      CALL BLAS_matmat('N','N',SIZE(sc,DIM=1),SIZE(sc,DIM=2),SIZE(sb,DIM=1),salpha, &
        sa,SIZE(sc,DIM=1),sb,SIZE(sb,DIM=1),sbeta,sc,1)

      CALL BLAS_matmat(salpha2,sa2,sb2,sc2)
      bool = ALL((sc(:,:) .APPROXEQ. sc2(:,:)))
      ASSERT(bool, 'BLAS_matmat(salpha2,sa2,sb2,sbeta2,sc2)')

      !Check sgemm_abc
      sc= 4.0_SSK; salpha= 1.0_SSK; sbeta= 1.0_SSK
      sc2=4.0_SSK; salpha2=1.0_SSK; sbeta2=1.0_SSK

      CALL BLAS_matmat('N','N',SIZE(sc,DIM=1),SIZE(sc,DIM=2),SIZE(sb,DIM=1),salpha, &
        sa,SIZE(sc,DIM=1),sb,SIZE(sb,DIM=1),sbeta,sc,1)

      CALL BLAS_matmat(sa2,sb2,sc2)
      bool = ALL((sc(:,:) .APPROXEQ. sc2(:,:)))
      ASSERT(bool, 'BLAS_matmat(salpha2,sa2,sb2,sbeta2,sc2)')


!Test dgemm interfaces
      da=2.0_SDK
      db=3.0_SDK
      dc=4.0_SDK
      dalpha=0.0_SDK
      dbeta=0.0_SDK

!Secondary Test Variables
      da2=2.0_SDK
      db2=3.0_SDK
      dc2=4.0_SDK
      dalpha2=2.0_SDK
      dbeta2=2.5_SDK



      !dgemm_all(transa,transb,m,n,k,alpha,a,lda,b,ldb,beta,c,ldc)
      !Check beta=0 and alpha=0 returns all zeros for the bounds specified
      CALL BLAS_matmat('N','n',4,4,4,dalpha,da,4,db,4,dbeta,dc,4)
      bool = ALL((dc(1:16,1) .APPROXEQ. 0.0_SDK)) .OR. &
             ALL((dc(17:128,1) .APPROXEQ. 4.0_SDK)) .OR. &
             ALL((dc(:,2:128) .APPROXEQ. 4.0_SDK))
      ASSERT(bool, "BLAS_matmat('N','N',4,4,4,dalpha,da,4,db,4,dbeta,dc,4)")
      !Check beta=0 and alpha=0 returns all zeros for the bounds specified, n*m>10000
      CALL BLAS_matmat('n','N',101,101,101,dalpha,da,101,db,101,dbeta,dc,101)
      bool = ALL((dc(1:128,1:79) .APPROXEQ. 0.0_SDK)) .OR. &
             ALL((dc(1:89,80) .APPROXEQ. 0.0_SDK)) .OR. &
             ALL((dc(90:128,80) .APPROXEQ. 4.0_SDK)) .OR. &
             ALL((dc(90:128,80) .APPROXEQ. 4.0_SDK))
      ASSERT(bool, "BLAS_matmat('N','N',101,101,101,dalpha,da,101,db,101,dbeta,dc,101))")
      !Check beta=4 and alpha=0 returns 16.0 for the bounds specified
      dc=4.0_SDK
      dbeta=4.0_SDK
      CALL BLAS_matmat('N','N',4,2,4,dalpha,da,4,db,4,dbeta,dc,4)
      bool = ALL((dc(1:8,1) .APPROXEQ. 16.0_SDK)) .OR. &
             ALL((dc(17:128,1) .APPROXEQ. 4.0_SDK)) .OR. &
             ALL((dc(:,2:128) .APPROXEQ. 4.0_SDK))
      ASSERT(bool, "BLAS_matmat('N','N',4,4,4,dalpha,da,4,db,4,dbeta,dc,4)")
      !Check beta=4 and alpha=0 returns 16.0 for the bounds specified, n*m>10000
      dc=4.0_SDK
      CALL BLAS_matmat('N','N',101,101,101,dalpha,da,101,db,101,dbeta,dc,101)
      bool = ALL((dc(1:128,1:79) .APPROXEQ. 16.0_SDK)) .OR. &
             ALL((dc(1:89,80) .APPROXEQ. 16.0_SDK)) .OR. &
             ALL((dc(90:128,80) .APPROXEQ. 4.0_SDK)) .OR. &
             ALL((dc(90:128,80) .APPROXEQ. 4.0_SDK))
      ASSERT(bool, "BLAS_matmat('N','N',101,101,101,dalpha,da,101,db,101,dbeta,dc,101)")
      !Check beta=0 and alpha=2 returns 36.0 for the bounds specified
      dalpha=2.0_SDK
      dc=4.0_SDK
      dbeta=0.0_SDK
      CALL BLAS_matmat('N','N',3,5,3,dalpha,da,3,db,5,dbeta,dc,3)
      bool = ALL((dc(1:15,1) .APPROXEQ. 36.0_SDK)) .OR. &
             ALL((dc(16:128,1) .APPROXEQ. 4.0_SDK)) .OR. &
             ALL((dc(:,2:128) .APPROXEQ. 4.0_SDK))
      ASSERT(bool, "BLAS_matmat('N','N',101,101,101,dalpha,da,101,db,101,dbeta,dc,101)")
      !Check beta=2.5 and alpha=2 returns 46.0 for the bounds specified
      dc=4.0_SDK
      dbeta=2.5_SDK
      CALL BLAS_matmat('N','N',3,5,3,dalpha,da,3,db,3,dbeta,dc,3)
      bool = ALL((dc(1:15,1) .APPROXEQ. 46.0_SDK)) .OR. &
             ALL((dc(16:128,1) .APPROXEQ. 4.0_SDK)) .OR. &
             ALL((dc(:,2:128) .APPROXEQ. 4.0_SDK))
      ASSERT(bool, "BLAS_matmat('N','N',101,101,101,dalpha,da,101,db,101,dbeta,dc,101)")

      !Check Single Transpose
      dc=4.0_SDK
      dbeta=2.5_SDK
      CALL BLAS_matmat('T','N',3,3,3,dalpha,da,3,db,3,dbeta,dc,3)
      bool = ALL((dc(1:15,1) .APPROXEQ. 46.0_SDK)) .OR. &
             ALL((dc(16:128,1) .APPROXEQ. 4.0_SDK)) .OR. &
             ALL((dc(:,2:128) .APPROXEQ. 4.0_SDK))
      ASSERT(bool, "BLAS_matmat('T','N',3,3,3,dalpha,da,3,db,3,dbeta,dc,3)")
      !Check Second Single Transpose
      dc=4.0_SDK
      dbeta=2.5_SDK
      CALL BLAS_matmat('N','t',3,3,3,dalpha,da,3,db,3,dbeta,dc,3)
      bool = ALL((dc(1:15,1) .APPROXEQ. 46.0_SDK)) .OR. &
             ALL((dc(16:128,1) .APPROXEQ. 4.0_SDK)) .OR. &
             ALL((dc(:,2:128) .APPROXEQ. 4.0_SDK))
      ASSERT(bool, "BLAS_matmat('N','T',3,3,3,dalpha,da,3,db,3,dbeta,dc,3)")
      !Check Both Transpose
      dc=4.0_SDK
      dbeta=2.5_SDK
      CALL BLAS_matmat('t','T',3,3,3,dalpha,da,3,db,3,dbeta,dc,3)
      bool = ALL((dc(1:15,1) .APPROXEQ. 46.0_SDK)) .OR. &
             ALL((dc(16:128,1) .APPROXEQ. 4.0_SDK)) .OR. &
             ALL((dc(:,2:128) .APPROXEQ. 4.0_SDK))
      ASSERT(bool, "BLAS_matmat('T','T',3,3,3,dalpha,da,3,db,3,dbeta,dc,3)")
      !With Beta = 0
      !Check Single Transpose
      dc=4.0_SDK
      dbeta=0.0_SDK
      CALL BLAS_matmat('T','N',3,3,3,dalpha,da,3,db,3,dbeta,dc,3)
      bool = ALL((dc(1:15,1) .APPROXEQ. 46.0_SDK)) .OR. &
             ALL((dc(16:128,1) .APPROXEQ. 4.0_SDK)) .OR. &
             ALL((dc(:,2:128) .APPROXEQ. 4.0_SDK))
      ASSERT(bool, "BLAS_matmat('T','N',3,3,3,dalpha,da,3,db,3,dbeta,dc,3)")
      !Check Second Single Transpose
      dc=4.0_SDK
      CALL BLAS_matmat('N','T',3,3,3,dalpha,da,3,db,3,dbeta,dc,3)
      bool = ALL((dc(1:15,1) .APPROXEQ. 46.0_SDK)) .OR. &
             ALL((dc(16:128,1) .APPROXEQ. 4.0_SDK)) .OR. &
             ALL((dc(:,2:128) .APPROXEQ. 4.0_SDK))
      ASSERT(bool, "BLAS_matmat('N','T',3,3,3,dalpha,da,3,db,3,dbeta,dc,3)")
      !Check Both Transpose
      dc=4.0_SDK
      CALL BLAS_matmat('T','T',3,3,3,dalpha,da,3,db,3,dbeta,dc,3)
      bool = ALL((dc(1:15,1) .APPROXEQ. 46.0_SDK)) .OR. &
             ALL((dc(16:128,1) .APPROXEQ. 4.0_SDK)) .OR. &
             ALL((dc(:,2:128) .APPROXEQ. 4.0_SDK))
      ASSERT(bool, "BLAS_matmat('T','T',3,3,3,dalpha,da,3,db,3,dbeta,dc,3)")


      dc=4.0_SDK
      dbeta=2.5_SDK
      CALL BLAS_matmat('N','N',3,5,3,dalpha,da,3,db,3,dbeta,dc,3)
      !Check dgemm_ttmnkaabbc
      CALL BLAS_matmat('N','N',3,5,3,dalpha2,da2,db2,dbeta2,dc2)
      bool = ALL((dc(:,:) .APPROXEQ. dc2(:,:)))
      ASSERT(bool, "BLAS_matmat('N','N',3,5,3,dalpha2,da2,db2,dbeta2,dc2)")

      !Check dgemm_ttmnkabbc
      da= 2.0_SDK; db= 3.0_SDK; dc= 4.0_SDK; dalpha= 1.0_SDK; dbeta= 2.5_SDK
      da2=2.0_SDK; db2=3.0_SDK; dc2=4.0_SDK; dalpha2=2.0_SDK; dbeta2=2.5_SDK

      CALL BLAS_matmat('N','N',3,5,3,dalpha,da,3,db,3,dbeta,dc,3)

      CALL BLAS_matmat('N','N',3,5,3,da2,db2,dbeta2,dc2)
      bool = ALL((dc(:,:) .APPROXEQ. dc2(:,:)))
      ASSERT(bool, "BLAS_matmat('N','N',3,5,3,da2,db2,dbeta2,dc2)")

      !Check dgemm_ttmnkaabc
      dc= 4.0_SDK; dalpha= 2.0_SDK; dbeta= 1.0_SDK
      dc2=4.0_SDK; dalpha2=2.0_SDK; dbeta2=1.0_SDK

      CALL BLAS_matmat('N','N',3,5,3,dalpha,da,3,db,3,dbeta,dc,3)

      CALL BLAS_matmat('N','N',3,5,3,dalpha,da2,db2,dc2)
      bool = ALL((dc(:,:) .APPROXEQ. dc2(:,:)))
      ASSERT(bool, "BLAS_matmat('N','N',3,5,3,dalpha2,da2,db2,dc2)")

      !Check dgemm_ttmnkabc
      dc= 4.0_SDK; dalpha= 1.0_SDK; dbeta= 1.0_SDK
      dc2=4.0_SDK; dalpha2=1.0_SDK; dbeta2=1.0_SDK

      CALL BLAS_matmat('N','N',3,5,3,dalpha,da,3,db,3,dbeta,dc,3)

      CALL BLAS_matmat('N','N',3,5,3,da2,db2,dc2)
      bool = ALL((dc(:,:) .APPROXEQ. dc2(:,:)))
      ASSERT(bool, "BLAS_matmat('N','N',3,5,3,da2,db2,dc2)")

      !Check dgemm_mnkaabbc
      dc= 4.0_SDK; dalpha= 2.0_SDK; dbeta= 2.5_SDK
      dc2=4.0_SDK; dalpha2=2.0_SDK; dbeta2=2.5_SDK

      CALL BLAS_matmat('N','N',3,5,3,dalpha,da,3,db,3,dbeta,dc,1)

      CALL BLAS_matmat(3,5,3,dalpha2,da2,db2,dbeta2,dc2)
      bool = ALL((dc(:,:) .APPROXEQ. dc2(:,:)))
      ASSERT(bool, "BLAS_matmat('N','N',3,5,3,da2,db2,dc2)")

      !Check dgemm_ttaabbc
      dc= 4.0_SDK; dalpha= 2.0_SDK; dbeta= 2.5_SDK
      dc2=4.0_SDK; dalpha2=2.0_SDK; dbeta2=2.5_SDK

      CALL BLAS_matmat('N','N',SIZE(dc,DIM=1),SIZE(dc,DIM=2),SIZE(db,DIM=1),dalpha, &
        da,SIZE(dc,DIM=1),db,SIZE(db,DIM=1),dbeta,dc,SIZE(dc,DIM=1))

      CALL BLAS_matmat('N','N',dalpha2,da2,db2,dbeta2,dc2)
      bool = ALL((dc(:,:) .APPROXEQ. dc2(:,:)))
      ASSERT(bool, "BLAS_matmat('N','N',dalpha2,da2,db2,dbeta2,dc2)")

      !Check dgemm_aabbc
      dc= 4.0_SDK; dalpha= 2.0_SDK; dbeta= 2.5_SDK
      dc2=4.0_SDK; dalpha2=2.0_SDK; dbeta2=2.5_SDK

      CALL BLAS_matmat('N','N',SIZE(dc,DIM=1),SIZE(dc,DIM=2),SIZE(db,DIM=1),dalpha, &
        da,SIZE(dc,DIM=1),db,SIZE(db,DIM=1),dbeta,dc,SIZE(dc,DIM=1))

      CALL BLAS_matmat(dalpha2,da2,db2,dbeta2,dc2)
      bool = ALL((dc(:,:) .APPROXEQ. dc2(:,:)))
      ASSERT(bool, "BLAS_matmat(dalpha2,da2,db2,dbeta2,dc2)")

      !Check dgemm_abbc
      dc= 4.0_SDK; dalpha= 1.0_SDK; dbeta= 2.5_SDK
      dc2=4.0_SDK; dalpha2=1.0_SDK; dbeta2=2.5_SDK

      CALL BLAS_matmat('N','N',SIZE(da,DIM=1),SIZE(db,DIM=2),SIZE(da,DIM=2),dalpha, &
        da,SIZE(da,DIM=1),db,SIZE(db,DIM=1),dbeta,dc,SIZE(dc,DIM=1))

      CALL BLAS_matmat(da2,db2,dbeta2,dc2)
      bool = ALL((dc(:,:) .APPROXEQ. dc2(:,:)))
      ASSERT(bool, 'BLAS_matmat(da2,db2,dbeta2,dc2)')

      !Check dgemm_aabc
      dc= 4.0_SDK; dalpha= 2.0_SDK; dbeta= 1.0_SDK
      dc2=4.0_SDK; dalpha2=2.0_SDK; dbeta2=1.0_SDK

      CALL BLAS_matmat('N','N',SIZE(dc,DIM=1),SIZE(dc,DIM=2),SIZE(db,DIM=1),dalpha, &
        da,SIZE(dc,DIM=1),db,SIZE(db,DIM=1),dbeta,dc,1)

      CALL BLAS_matmat(dalpha2,da2,db2,dc2)
      bool = ALL((dc(:,:) .APPROXEQ. dc2(:,:)))
      ASSERT(bool, 'BLAS_matmat(dalpha2,da2,db2,dc2)')

      !Check dgemm_abc
      dc= 4.0_SDK; dalpha= 1.0_SDK; dbeta= 1.0_SDK
      dc2=4.0_SDK; dalpha2=1.0_SDK; dbeta2=1.0_SDK

      CALL BLAS_matmat('N','N',SIZE(dc,DIM=1),SIZE(dc,DIM=2),SIZE(db,DIM=1),dalpha, &
        da,SIZE(dc,DIM=1),db,SIZE(db,DIM=1),dbeta,dc,1)

      CALL BLAS_matmat(da2,db2,dc2)
      bool = ALL((dc(:,:) .APPROXEQ. dc2(:,:)))
      ASSERT(bool, 'BLAS_matmat(da2,db2,dc2)')
    ENDSUBROUTINE testBLAS3
ENDPROGRAM testBLAS
