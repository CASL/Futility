!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
PROGRAM testExpTables
#include "UnitTest.h"
  USE UnitTest
  USE IntrType
  USE ExceptionHandler
  USE Allocs
  USE ParameterLists
  USE ExpTables

  IMPLICIT NONE

  TYPE(ExceptionHandlerType),TARGET :: e
  REAL(SRK) :: x,ans,err(5),xtest,x1,x2,x3,y1,y2,y3
  REAL(SRK) :: reftbl(-10000:0),reftbl2(-10000:0),reftbl3(-10000:0)
  REAL(SRK) :: reftbl4(1:1000)
  INTEGER(SIK) :: i
  TYPE(ExpTableType),SAVE :: testET1,testET2(5)
  TYPE(ParamType),SAVE :: PL
  LOGICAL(SBK) :: bool

  CREATE_TEST('Test ExpTables')

  CALL e%setStopOnError(.FALSE.)
  CALL e%setQuietMode(.TRUE.)
  CALL eExpTable%addSurrogate(e)
  CALL eParams%addSurrogate(e)

  WRITE(*,*) '==================================================='
  WRITE(*,*) 'TESTING EXPTABLES...'
  WRITE(*,*) '==================================================='
  WRITE(*,*) 'TESTING EXPONENTTABLETYPE'


  !Error checking
  CALL PL%clear()
  CALL testET1%initialize(PL)
  CALL PL%add('ExpTables -> tabletype',0)
  CALL testET1%clear()
  CALL testET1%initialize(PL)
  CALL PL%set('ExpTables -> tabletype',2)
  CALL PL%add('ExpTables -> minval',1)
  CALL testET1%clear()
  CALL testET1%initialize(PL)
  CALL PL%set('ExpTables -> minval',-10)
  CALL PL%add('ExpTables -> maxval',1)
  CALL testET1%clear()
  CALL testET1%initialize(PL)
  CALL PL%set('ExpTables -> maxval',-10)
  CALL testET1%clear()
  CALL testET1%initialize(PL)
  CALL PL%set('ExpTables -> maxval',0)
  CALL PL%add('ExpTables -> nintervals',-1)
  CALL testET1%clear()
  CALL testET1%initialize(PL)
  CALL PL%set('ExpTables -> nintervals',100)
  CALL PL%add('ExpTables -> errorflag',.TRUE.)
  CALL testET1%clear()
  CALL testET1%initialize(PL)
  CALL PL%add('ExpTables -> error',0.005_SRK)
  CALL testET1%clear()
  CALL testET1%initialize(PL)
  CALL testET1%initialize(PL)
  CALL testET1%clear()
  !For the coverage
  CALL PL%set('ExpTables -> tabletype',5)
  CALL PL%set('ExpTables -> nintervals',1000)
  CALL PL%set('ExpTables -> error',1e-7_SRK)
  CALL testET1%initialize(PL)
  x=testET1%maxVal-0.5_SRK*testET1%dx
  ans=testET1%EXPT(x)

  CALL PL%set('ExpTables -> errorflag',.FALSE.)
  CALL PL%remove('ExpTables -> error')
  x=-1.234567891012_SRK
  DO i=1,5
    CALL PL%set('ExpTables -> tabletype',i)
    CALL testET2(i)%initialize(PL)
    ans=EXPT(testET2(i),x)
    err(i)=ans-(1._SRK-exp(x))
  ENDDO

  !For exact exponent function
  bool = (testET2(1)%isinit .OR. testET2(1)%tableType == 1 &
         .OR.testET2(1)%nintervals == -1 .OR. (testET2(1)%dx .APPROXEQ. 0._SRK) &
         .OR.(testET2(1)%dx2rd .APPROXEQ. 0._SRK) .OR. (testET2(1)%rdx .APPROXEQ. 0._SRK) &
         .OR.(testET2(1)%rdx2rd .APPROXEQ. 0._SRK) .OR. (testET2(1)%minVal == 0) &
         .OR.(testET2(1)%maxVal == 0) .OR. (testET2(1)%tableErr .APPROXEQ. 0._SRK))
  ASSERT(bool, 'testET2(1)%initialize(...)')
  WRITE(*,*) '  Passed: CALL testET2(1)%initialize(...) EXACT'
  !For SINGLE_LEVEL_EXP_TABLE exponent function
  bool = (testET2(2)%isinit .OR. testET2(2)%tableType == 2 &
         .OR. testET2(2)%nintervals == 1000 .OR. (testET2(2)%dx .APPROXEQ. 1.e-3_SRK) &
         .OR. (testET2(2)%dx2rd .APPROXEQ. 0._SRK) .OR. (testET2(2)%rdx .APPROXEQ. 1000._SRK) &
         .OR. (testET2(2)%rdx2rd .APPROXEQ. 0._SRK) .OR. (testET2(2)%minVal == -10) &
         .OR. (testET2(2)%maxVal == 0) .OR. (testET2(2)%tableErr .APPROXEQ. 0.5e-3_SRK))
  ASSERT(bool, 'testET2(2)%initialize(...)')
  xtest=-10._SRK
  DO i=-10000,0
    reftbl(i)=1._SRK-exp(xtest)
    xtest=xtest+1.e-3_SRK
    ASSERT(reftbl(i) .APPROXEQ. testET2(2)%table(i), 'testET2(2)%initialize(...)')
    FINFO() i
  ENDDO
  !Failed in intel release due to compiler optimizations
!  bool = (ALL(reftbl .APPROXEQ. testET2(2)%table))
!  ASSERT(bool, 'testET2(2)%initialize(...)')
  WRITE(*,*) '  Passed: CALL testET2(2)%initialize(...) SINGLE_LEVEL'
  !For TWO_LEVEL_EXP_TABLE exponent function
  bool = (testET2(3)%isinit .OR. testET2(3)%tableType == 3 &
         .OR. testET2(3)%nintervals == 1000 .OR. (testET2(3)%dx .APPROXEQ. 1.e-3_SRK) &
         .OR. (testET2(3)%dx2rd .APPROXEQ. 1.e-6_SRK) .OR. (testET2(3)%rdx .APPROXEQ. 1000._SRK) &
         .OR. (testET2(3)%rdx2rd .APPROXEQ. 1.e6_SRK) .OR. (testET2(3)%minVal == -10) &
         .OR. (testET2(3)%maxVal == 0) .OR. (testET2(3)%tableErr .APPROXEQ. 0.5e-6_SRK))
  ASSERT(bool, 'testET2(3)%initialize(...)')
  xtest=-10._SRK
  DO i=-10000,0
    reftbl(i)=exp(xtest)
    xtest=xtest+1.e-3_SRK
    ASSERT(reftbl(i) .APPROXEQ. testET2(3)%table(i), 'testET2(3)%initialize(...)')
    FINFO() i
  ENDDO
  !Failed in intel release due to compiler optimizations
  !bool = (ALL(reftbl .APPROXEQ. testET2(3)%table))
  !ASSERT(bool, 'testET2(3)%initialize(...)')
  FINFO() testET2(3)%table
  FINFO() reftbl
  xtest=0._SRK
  DO i=1,1000
    xtest=xtest+1.e-6_SRK
    reftbl4(i)=exp(xtest)
  ENDDO
  bool = (ALL(reftbl4 .APPROXEQ. testET2(3)%table2rd))
  ASSERT(bool, 'testET2(3)%initialize(...)')
  WRITE(*,*) '  Passed: CALL testET2(3)%initialize(...) TWO_LEVEL'
  !For LINEAR_EXP_TABLE exponent function
  bool = (testET2(4)%isinit .OR. testET2(4)%tableType == 4 &
         .OR. testET2(4)%nintervals == 1000 .OR. (testET2(4)%dx .APPROXEQ. 1.e-3_SRK) &
         .OR. (testET2(4)%dx2rd .APPROXEQ. 0._SRK) .OR. (testET2(4)%rdx .APPROXEQ. 1000._SRK) &
         .OR. (testET2(4)%rdx2rd .APPROXEQ. 0._SRK) .OR. (testET2(4)%minVal == -10) &
         .OR. (testET2(4)%maxVal == 0) .OR. (testET2(4)%tableErr .APPROXEQ. 1.25e-7_SRK))
  ASSERT(bool, 'testET2(4)%initialize(...)')
  x1=-10._SRK
  y1=1._SRK-EXP(x1)
  DO i=-10000,0
    x2=x1+1e-3_SRK
    y2=1._SRK-EXP(x2)
    reftbl(i)=(y2-y1)*1000._SRK
    reftbl2(i)=y1-reftbl(i)*x1
    x1=x2
    y1=y2
  ENDDO
  DO i=-10000,0
    bool = (reftbl(i) .APPROXEQ. testET2(4)%table2D(1,i))
    ASSERT(bool, 'testET2(4)%initialize(...)')
    FINFO() i,reftbl(i),testET2(4)%table2D(1,i)
  ENDDO
  DO i=-10000,0
    bool = (reftbl2(i) .APPROXEQ. testET2(4)%table2D(2,i))
    ASSERT(bool, 'testET2(4)%initialize(...)')
    FINFO() i,reftbl2(i),testET2(4)%table2D(2,i)
  ENDDO
  WRITE(*,*) '  Passed: CALL testET2(4)%initialize(...) LINEAR'

  !For ORDER2_EXP_TABLE exponent function
  bool = (testET2(5)%isinit .OR. testET2(5)%tableType == 5 &
         .OR. testET2(5)%nintervals == 1000 .OR. (testET2(5)%dx .APPROXEQ. 1.e-3_SRK) &
         .OR. (testET2(5)%dx2rd .APPROXEQ. 0._SRK) .OR. (testET2(5)%rdx .APPROXEQ. 1000._SRK) &
         .OR. (testET2(5)%rdx2rd .APPROXEQ. 0._SRK) .OR. (testET2(5)%minVal == -10) &
         .OR. (testET2(5)%tableErr .APPROXEQ. 9.630017699314371e-012_SRK) &
         .OR. (testET2(5)%maxVal == 0))
  ASSERT(bool, 'testET2(5)%initialize(...)')
  x1=-10._SRK
  y1=1._SRK-EXP(x1)
  x2=x1+1.0e-3_SRK
  y2=1._SRK-EXP(x2)
  DO i=-10000,0
    x3=x2+1.0e-3_SRK
    y3=1._SRK-EXP(x3)
    reftbl(i)=((y1+y3)*0.5_SRK-y2)*1.0e6_SRK
    reftbl2(i)=(-((x2+x3)*y1+(x1+x2)*y3)*0.5_SRK+(x1+x3)*y2)*1.0e6_SRK
    reftbl3(i)=((x3*y1+x1*y3)*x2*0.5_SRK-x1*x3*y2)*1.0e6_SRK
    x1=x2
    x2=x3
    y1=y2
    y2=y3
  ENDDO
  DO i=-10000,0
    bool = (reftbl(i) .APPROXEQ. testET2(5)%table(i))
    ASSERT(bool, 'testET2(5)%initialize(...)')
    FINFO() i,reftbl(i),testET2(5)%table(i)
  ENDDO
  DO i=-10000,0
    bool = (reftbl2(i) .APPROXEQ. testET2(5)%table2rd(i))
    ASSERT(bool, 'testET2(5)%initialize(...)')
    FINFO() i,reftbl2(i),testET2(5)%table2rd(i)
  ENDDO

  DO i=-10000,0
    bool = SOFTEQ(reftbl3(i),testET2(5)%table3rd(i),1.0e-7_SRK)
    ASSERT(bool, 'testET2(5)%initialize(...)')
    FINFO() i,reftbl3(i),testET2(5)%table3rd(i)
  ENDDO
  WRITE(*,*) '  Passed: CALL testET2(5)%initialize(...) ORDER2'

  err(:)=0.0_SRK
  CALL PL%set('ExpTables -> nintervals',100)
  CALL PL%set('ExpTables -> errorFlag',.TRUE.)
  CALL PL%add('ExpTables -> error',9.99e-7_SRK)
  DO i=1,5
    CALL testET2(i)%clear()
    CALL PL%set('ExpTables -> tabletype',i)
    CALL testET2(i)%initialize(PL)
    WRITE(*,*) getMemUsageChar()
    ans=testET2(i)%EXPT(x)
    err(i)=ans-(1._SRK-EXP(x))
  ENDDO
  CALL testET2(5)%clear()
  bool = ALL(ABS(err) <= 1e-6_SRK)
  ASSERT(bool, 'tableET%EXPT(x)')
  WRITE(*,*) '  Passed: tableET%EXPT(x)'

  CALL testET1%clear()
  bool = (.NOT.testET1%isinit .OR. testET1%tableType == -1 &
         .OR. testET1%nintervals == -1 .OR. (testET1%dx .APPROXEQ. 0._SRK) &
         .OR. (testET1%dx2rd .APPROXEQ. 0._SRK) .OR. (testET1%rdx .APPROXEQ. 0._SRK) &
         .OR. (testET1%rdx2rd .APPROXEQ. 0._SRK) .OR. (testET1%minVal == 0) &
         .OR. (testET1%maxVal == 0) .OR. (testET1%tableErr .APPROXEQ. 0._SRK))
  ASSERT(bool, 'testET1%clear()')
  FINALIZE_TEST()

  CREATE_TEST("subtest POLAR_EXP_TABLE")

  REGISTER_SUBTEST("testPOLAR_EXP_TABLE", testPOLAR_EXP_TABLE)

  WRITE(*,*) '  Passed: CALL testET1%clear()'
  WRITE(*,*) '==================================================='
  WRITE(*,*) 'TESTING EXPTABLES COMPLETE!'
  WRITE(*,*) '==================================================='
  CALL PL%clear()
  CALL ErrCheck()
  !CALL perftest()
  DO i=1,5
    CALL testET2(i)%clear()
  ENDDO
  CALL ExpTables_Clear_ValidParams()
  !CALL bitEXP()

  FINALIZE_TEST()

!
!===============================================================================
CONTAINS
!
!-------------------------------------------------------------------------------
!> This routine computes the numerical error of the tables which can be used to
!> compare to the analytic expressions for verification.
    SUBROUTINE ErrCheck()
      REAL(SRK) :: x(1000),ans(1000),err_result(1000),maxerr,dx
      INTEGER(SIK) :: j,i,ntype,nintervals,itype,maxi
      TYPE(ExpTableType) :: testET

      ntype=5_SIK

      !DO i=2,6
      !  nintervals(i)=nintervals(i-1)*10_SIK
      !ENDDO
      WRITE(*,*)
      WRITE(*,*) '+------------+------------+---------------+'
      WRITE(*,*) '| Table Type | nIntervals | Max Table Err |'
      WRITE(*,*) '+------------+------------+---------------+'
      DO itype=2,5 !ntype
        nintervals=1_SIK
        DO j=2,6
          nintervals=nintervals*10
          CALL PL%set('ExpTables -> tabletype',itype)
          CALL PL%set('ExpTables -> nintervals',nintervals)
          CALL testET%initialize(PL)
          x(1)=0.0_SRK
          IF(itype/=3) THEN
            dx=testET%dx/1000._SRK
          ELSE
            dx=testET%dx2rd/1000._SRK
          ENDIF
          DO i=2,1000
            x(i)=x(i-1)-dx
          ENDDO
          DO i=2,999
            ans(i)=1._SRK-EXP(x(i))
            ans(1)=EXPT(testET,x(i))
            err_result(i)=ABS(ans(i)-ans(1))
          ENDDO
          maxerr=0._SRK
          DO i=2,999
            IF(err_result(i)>maxerr) THEN
              maxerr=err_result(i)
              maxi=i
            ENDIF
          ENDDO
          WRITE(*,'(a,i12,a,i12,a,es15.6,a)') &
            ' |',itype,'|',testET%nintervals,'|',maxerr,'|'
!          WRITE(*,*) itype,testET%nintervals,'Err:',maxi,x(maxi),maxerr
          CALL testET%clear()
        ENDDO
      ENDDO
      WRITE(*,*) '+------------+------------+---------------+'
    ENDSUBROUTINE ErrCheck
!
!-------------------------------------------------------------------------------
!This routine was used to generate the performance data with TAU to show
!that the linear interpolation has favorable performance.
    SUBROUTINE perftest()
#ifdef TAU
      INTEGER(SIK),SAVE :: profiler1(2)=0
      INTEGER(SIK),SAVE :: profiler2(2)=0
      INTEGER(SIK),SAVE :: profiler3(2)=0
      INTEGER(SIK),SAVE :: profiler4(2)=0
      INTEGER(SIK),SAVE :: profiler5(2)=0
      INTEGER(SIK),SAVE :: profiler6(2)=0
      INTEGER(SIK) :: nval,neval,j,ix
      REAL(SRK),ALLOCATABLE :: xval(:),ans(:)
      TYPE(ExpTableType) :: myTable

      WRITE(*,*)
      WRITE(*,*)
      WRITE(*,*) ' Getting performance measurements...'

      !Set test input for number of evaluations and number of values to evaluate
      neval=100000
      nval=300

      !Setup test input
      ALLOCATE(ans(1:nval))
      ALLOCATE(xval(1:nval))
      CALL RANDOM_NUMBER(xval)
      xval=xval*(-10._SRK)

      CALL TAU_PROFILE_INIT()
      CALL TAU_PROFILE_TIMER(profiler1, 'EXACT EXP()')
      DO i=1,neval
        ans=REAL(i,SRK)
        CALL TAU_PROFILE_START(profiler1)
        DO j=1,nval
          ans(j)=ans(j)+EXP(xval(j))
        ENDDO
        CALL TAU_PROFILE_STOP(profiler1)
      ENDDO

      CALL TAU_PROFILE_TIMER(profiler2, 'EXACT EXP() Array Form')
      DO i=1,neval
        ans=REAL(i,SRK)
        CALL TAU_PROFILE_START(profiler2)
        ans=ans+EXP(xval)
        CALL TAU_PROFILE_STOP(profiler2)
      ENDDO

      CALL TAU_PROFILE_TIMER(profiler3, 'EXACT myTable%EXPT()')
      CALL myTable%initialize(EXACT_EXP_TABLE)
      DO i=1,neval
        ans=REAL(i,SRK)
        CALL TAU_PROFILE_START(profiler3)
        DO j=1,nval
          ans(j)=ans(j)+myTable%EXPT(xval(j))
        ENDDO
        CALL TAU_PROFILE_STOP(profiler3)
      ENDDO
      CALL myTable%clear()

      CALL TAU_PROFILE_TIMER(profiler4, 'LINEAR myTable%EXPT()')
      CALL myTable%initialize(LINEAR_EXP_TABLE)
      DO i=1,neval
        ans=REAL(i,SRK)
        CALL TAU_PROFILE_START(profiler4)
        DO j=1,nval
          ans(j)=ans(j)+myTable%EXPT(xval(j))
        ENDDO
        CALL TAU_PROFILE_STOP(profiler4)
      ENDDO
      CALL myTable%clear()

      CALL TAU_PROFILE_TIMER(profiler5, 'LINEAR EXPLICIT')
      CALL myTable%initialize(LINEAR_EXP_TABLE)
      DO i=1,neval
        ans=REAL(i,SRK)
        CALL TAU_PROFILE_START(profiler5)
        DO j=1,nval
          ix=FLOOR(xval(j)*myTable%rdx)
          ans(j)=ans(j)+myTable%table2D(1,ix)*xval(j)+myTable%table2D(2,ix)
        ENDDO
        CALL TAU_PROFILE_STOP(profiler5)
      ENDDO
      CALL myTable%clear()

      WRITE(*,*) ' Performance measurements completed'
#else
      USE Times
      USE StochasticSampling

      INTEGER(SIK) :: nval,neval,j
      REAL(SRK),ALLOCATABLE :: xval(:),ans(:)
      TYPE(ExpTableType) :: myTable
      TYPE(TimerType) :: testTimer
      TYPE(StochasticSamplingType) :: myRNG

      CALL testTimer%setTimerHiResMode(.TRUE.)
      CALL myRNG%init(RNG_LEcuyer2)
      WRITE(*,*)
      WRITE(*,*)
      WRITE(*,*) ' Getting performance measurements without TAU...'

      !Set test input for number of evaluations and number of values to evaluate
      neval=1000000
      nval=3000

      !Setup test input
      ALLOCATE(ans(1:nval))
      ALLOCATE(xval(1:nval))
      DO j=1,nval
        xval(j)=myRNG%rng()
      ENDDO
      xval=xval*(-10._SRK)

      WRITE(*,*) 'EXACT myTable%EXPT()'
      CALL PL%add('ExpTables -> tabletype',EXACT_EXP_TABLE)
      CALL myTable%initialize(PL)
      CALL testTimer%tic()
      DO i=1,neval
        ans=REAL(i,SRK)
        DO j=1,nval
          ans(j)=ans(j)+myTable%EXPT(xval(j))
        ENDDO
      ENDDO
      CALL testTimer%toc()
      WRITE(*,*) "Took: ", testTimer%elapsedtime
      CALL myTable%clear()

      WRITE(*,*) 'LINEAR myTable%EXPT()'
      CALL PL%set('ExpTables -> tabletype',LINEAR_EXP_TABLE)
      CALL myTable%initialize(PL)
      CALL testTimer%ResetTimer()
      DO i=1,neval
        ans=REAL(i,SRK)
        DO j=1,nval
          ans(j)=ans(j)+myTable%EXPT(xval(j))
        ENDDO
      ENDDO
      CALL testTimer%toc()
      WRITE(*,*) "Took: ", testTimer%elapsedtime
      CALL myTable%clear()
      CALL PL%clear()
      CALL myRNG%clear()

      WRITE(*,*) ' Performance measurements completed'
#endif
    ENDSUBROUTINE perftest
!
!-------------------------------------------------------------------------------
!This routine is here to test the performance and feasability of calculating
!the function EXP(x) based on bitwise comparison and operations to see if there
!is any noticable speedup.
    SUBROUTINE bitEXP()
      REAL(SSK) :: x,y,xinit
      INTEGER(SNK) :: xint,yint,xinitint
      EQUIVALENCE(x,xint)
      EQUIVALENCE(y,yint)
      EQUIVALENCE(xinit,xinitint)
      INTEGER(SIK) :: i
      !REAL(SSK),ALLOCATABLE :: ans(:),xval(:)
      REAL(SSK) :: logvals(27)

      !Set test input for number of evaluations and number of values to evaluate
      !neval=1
      !nval=1

      !Setup test input
      !ALLOCATE(ans(1:nval))
      !ALLOCATE(xval(1:nval))
      !CALL RANDOM_NUMBER(xval)
      !xval=xval*(-10._SSK)

      !Trial Negative values
      !logvals(1)=LOG(1.0_SSK/256.0_SSK)
      !logvals(2)=LOG(1.0_SSK/16.0_SSK)
      !logvals(3)=LOG(1.0_SSK/4.0_SSK)
      !logvals(4)=LOG(1.0_SSK/2.0_SSK)
      !logvals(5)=LOG(1.0_SSK/1.5_SSK)
      !logvals(6)=LOG(1.0_SSK/1.25_SSK)
      !logvals(7)=LOG(1.0_SSK/1.125_SSK)
      !logvals(8)=LOG(1.0_SSK/1.0625_SSK)
      !logvals(9)=LOG(1.0_SSK/1.03125_SSK)
      !logvals(10)=LOG(1.0_SSK/1.015625_SSK)
      !logvals(11)=LOG(1.0_SSK/1.007813_SSK)
      !logvals(12)=LOG(1.0_SSK/1.003906_SSK)
      !logvals(13)=LOG(1.0_SSK/1.001953_SSK)
      !logvals(14)=LOG(1.0_SSK/1.000977_SSK)
      !logvals(15)=LOG(1.0_SSK/1.000488_SSK)
      !logvals(16)=LOG(1.0_SSK/1.000244_SSK)
      !logvals(17)=LOG(1.0_SSK/1.000122_SSK)
      !logvals(18)=LOG(1.0_SSK/1.000061_SSK)
      !logvals(19)=LOG(1.0_SSK/1.000031_SSK)
      !logvals(20)=LOG(1.0_SSK/1.000015_SSK)
      !logvals(21)=LOG(1.0_SSK/1.000008_SSK)
      !logvals(22)=LOG(1.0_SSK/1.000004_SSK)
      !logvals(23)=LOG(1.0_SSK/1.000002_SSK)
      !Positive Values
      logvals(1)=LOG(256.00000_SSK)
      logvals(2)=LOG(16.000000_SSK)
      logvals(3)=LOG(4.0000000_SSK)
      logvals(4)=LOG(2.0000000_SSK)
      logvals(5)=LOG(1.5000000_SSK)
      logvals(6)=LOG(1.2500000_SSK)
      logvals(7)=LOG(1.1250000_SSK)
      logvals(8)=LOG(1.0625000_SSK)
      logvals(9)=LOG(1.0312500_SSK)
      logvals(10)=LOG(1.0156250_SSK)
      logvals(11)=LOG(1.0078125_SSK)
      logvals(12)=LOG(1.0039063_SSK)
      logvals(13)=LOG(1.0019531_SSK)
      logvals(14)=LOG(1.0009766_SSK)
      logvals(15)=LOG(1.0004883_SSK)
      logvals(16)=LOG(1.0002441_SSK)
      logvals(17)=LOG(1.0001221_SSK)
      logvals(18)=LOG(1.0000610_SSK)
      logvals(19)=LOG(1.0000305_SSK)
      logvals(20)=LOG(1.0000153_SSK)
      logvals(21)=LOG(1.0000076_SSK)
      logvals(22)=LOG(1.0000038_SSK)
      logvals(23)=LOG(1.0000019_SSK)
      logvals(24)=LOG(1.0000010_SSK)
      logvals(25)=LOG(1.0000005_SSK)
      logvals(26)=LOG(1.0000002_SSK)
      logvals(27)=LOG(1.0000001_SSK)

      x=-10._SSK
      xinit=EXP(x)
      y=4.54E-5_SSK
      WRITE(*,*) 'x=',x,'y=',y,'EXP(x)=',xinit
      WRITE(*,*) 'Printing X...'
      CALL printBit(xint)
      WRITE(*,*) 'Printing Y...'
      CALL printBit(yint)

      !Get it to work for positive numbers first.

      x=0.5_SSK
      xinit=EXP(x)
      y=1.0_SRK
      !yint=1073741824_SIK
      !yint=65536_SIK
      WRITE(*,*) 'x=',x,'y=',y,'EXP(x)=',xinit
      WRITE(*,*) 'Printing EXP(Xinitial)...'
      CALL printBit(xinitint)
      WRITE(*,*) 'Printing Y...'
      CALL printBit(yint)
      DO i=1,5
        IF(x >= logvals(i)) THEN
          x=x-logvals(i); yint=IBSET(yint,27-i)
        ENDIF
        WRITE(*,*) 'x=',x,'y=',y,'EXP(x)=',xinit
        WRITE(*,*) 'Printing EXP(Xinitial)...'
        CALL printBit(xinitint)
        WRITE(*,*) 'Printing Y...'
        CALL printBit(yint)
      ENDDO
      DO i=6,24
        IF(x >= logvals(i)) THEN
          x=x-logvals(i); yint=IBSET(yint,27-i)
        ENDIF
        WRITE(*,*) 'x=',x,'y=',y,'EXP(x)=',xinit
        WRITE(*,*) 'Printing EXP(Xinitial)...'
        CALL printBit(xinitint)
        WRITE(*,*) 'Printing Y...'
        CALL printBit(yint)
      ENDDO
      !DO i=1,10
      !  !Add 1._SSK to x
      !  x=x-1.0_SSK
      !  WRITE(*,*) 'x=',x,'y=',y,'EXP(x)=',EXP(x)
      !  CALL printBit(xint)
      !ENDDO

      !DO i=1,neval
      !
      !ENDDO


    ENDSUBROUTINE bitEXP
!
!-------------------------------------------------------------------------------
!Functionalizing the Bit Printing I'll probably need mulitple times.
!Prints the bit number from 31-0 for 32-bit values, prints the s,x,m
!notation for IEEE-754 single precision reals, and then prints the bits in the
!corresponding order.  (Haven't expanded this to take into account endian-ness)
!
!The integer specified by the input should be "equivalenced" (shares the same
!memory location with) a single precision real number.  Bit-wise operations are
!only available through integers.
!
    SUBROUTINE printBit(int)
      INTEGER(SNK),INTENT(INOUT) :: int
      INTEGER(SIK) :: i,bprint(32)
      LOGICAL(SBK) :: flag(32)

      bprint=0_SIK
      DO i=1,32
        !Apparently, there is a zero bit, who knew.
        flag(i)=BTEST(int,i-1)
        IF(flag(i)) bprint(i)=1_SIK
      ENDDO
      WRITE(*,*) '      10987654321098765432109876543210'
      WRITE(*,*) '      sxxxxxxxxmmmmmmmmmmmmmmmmmmmmmmm'
      WRITE(*,'(a,32i1)') '       ',(bprint(i),i=32,1,-1)
      WRITE(*,*) '--------------------------------------'
    ENDSUBROUTINE printBit
!
!-------------------------------------------------------------------------------
!This routine is here to test the performance and feasability of calculating
!the function EXP(x) based on bitwise comparison and operations to see if there
!is any noticable speedup.
    SUBROUTINE testPOLAR_EXP_TABLE()
      TYPE(ExpTableType) :: testET
      TYPE(ParamType),SAVE :: PL
      REAL(SRK),ALLOCATABLE :: rsinpol(:)
      INTEGER(SIK) :: ipol,i
      REAL(SRK) :: y,result,x

      CALL PL%clear()
      !Set names for required parameters
      !Set defaults for optional parameters
      CALL PL%add('ExpTables -> tabletype',POLAR_EXP_TABLE)
      CALL PL%add('ExpTables -> minval',-10._SRK)
      CALL PL%add('ExpTables -> maxval',0._SRK)
      CALL PL%add('ExpTables -> nintervals',1000)
      CALL PL%add('ExpTables -> error',0.0005_SRK)
      CALL PL%add('ExpTables -> errorflag',.FALSE.)
      ALLOCATE(rsinpol(3))
      rsinpol=(/6.000672075_SRK,1.859748897_SRK,1.071864208_SRK/)
      CALL PL%add('ExpTables -> RSIN_polars',rsinpol)

      !Initialize the exponential table
      CALL testET%initialize(PL)
      !Error Checking
      ASSERT(testET%nintervals==1000,"nintervals")
      ASSERT(testET%tableType==POLAR_EXP_TABLE,"tableType")
      ASSERT(testET%maxVal==0._SRK,"maxVal")
      ASSERT(testET%minVal==-10._SRK,"minVal")
      ASSERT(testET%minTable==-10000,"minTable")
      FINFO() "minTable",testET%minTable
      FINFO() "minTable_ref",-1000
      ASSERT(testET%rdx==1000,"rdx")
      ASSERT(testET%dx==0.001_SRK,"dx")
      ASSERT(SOFTEQ(testET%tableErr,1.249999999E-007_SRK,1e-6_SRK),"tableErr")
      FINFO() "tableErr",testET%tableErr
      FINFO() "tableErr_ref",1.249999999E-007_SRk

      DO ipol=1,3
        x=-10.001_SRK
        DO i=-10000,0
          x=x+0.001_SRK
          y=1._SRK-EXP(x*rsinpol(ipol))
          result=testET%EXPT(x,ipol)
          !Comparing the table lookup result with the system function evaluation
          !instead of checking the coeff in the table themselves
          ASSERT(SOFTEQ(result,y,1.249999999E-007_SRK),"resultOfFunctionCall")
          FINFO() "tableLookUp",result,i,ipol
          FINFO() "reference",y
        ENDDO
      ENDDO

      DEALLOCATE(rsinpol)
      CALL testET%clear()
      CALL PL%clear()
  ENDSUBROUTINE testPOLAR_EXP_TABLE
!
ENDPROGRAM testExpTables
