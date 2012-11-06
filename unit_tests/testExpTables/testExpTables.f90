!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                              Copyright (C) 2012                              !
!                   The Regents of the University of Michigan                  !
!              MPACT Development Group and Prof. Thomas J. Downar              !
!                             All rights reserved.                             !
!                                                                              !
! Copyright is reserved to the University of Michigan for purposes of          !
! controlled dissemination, commercialization through formal licensing, or     !
! other disposition. The University of Michigan nor any of their employees,    !
! makes any warranty, express or implied, or assumes any liability or          !
! responsibility for the accuracy, completeness, or usefulness of any          !
! information, apparatus, product, or process disclosed, or represents that    !
! its use would not infringe privately owned rights. Reference herein to any   !
! specific commercial products, process, or service by trade name, trademark,  !
! manufacturer, or otherwise, does not necessarily constitute or imply its     !
! endorsement, recommendation, or favoring by the University of Michigan.      !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
PROGRAM testExpTables
  USE IntrType
  USE Allocs
  USE ExceptionHandler
  USE ExpTables
  USE ParameterLists
  IMPLICIT NONE
  
  TYPE(ExceptionHandlerType),TARGET :: e
  REAL(SRK) :: x,ans,err(5),xtest,x1,x2,x3,y1,y2,y3
  REAL(SRK) :: reftbl(-10000:0),reftbl2(-10000:0),reftbl3(-10000:0)
  REAL(SRK) :: reftbl4(1:1000)
  INTEGER(SIK) :: i
  CHARACTER(LEN=6) :: istr
  TYPE(ExpTableType),SAVE :: testET1,testET2(5)
  TYPE(ParamType),SAVE :: PL

  eExpTable => e
  WRITE(*,*) '==================================================='
  WRITE(*,*) 'TESTING EXPTABLES...'
  WRITE(*,*) '==================================================='
  WRITE(*,*) 'TESTING EXPONENTTABLETYPE'
  
  CALL e%setStopOnError(.FALSE.)
  CALL e%setQuietMode(.TRUE.)
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
  IF(.NOT.(testET2(1)%isinit .AND. testET2(1)%tableType == 1 &
    .AND. testET2(1)%nintervals == -1 .AND. (testET2(1)%dx .APPROXEQ. 0._SRK) &
      .AND. (testET2(1)%dx2rd .APPROXEQ. 0._SRK) .AND. (testET2(1)%rdx .APPROXEQ. 0._SRK) &
        .AND. (testET2(1)%rdx2rd .APPROXEQ. 0._SRK) .AND. (testET2(1)%minVal == 0) &
          .AND. (testET2(1)%maxVal == 0) .AND. (testET2(1)%tableErr .APPROXEQ. 0._SRK))) THEN
    WRITE(*,*) 'CALL testET2(1)%initialize(...) FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: CALL testET2(1)%initialize(...) EXACT'
  ENDIF
  !For SINGLE_LEVEL_EXP_TABLE exponent function
  IF(.NOT.(testET2(2)%isinit .AND. testET2(2)%tableType == 2 &
    .AND. testET2(2)%nintervals == 1000 .AND. (testET2(2)%dx .APPROXEQ. 1.e-3_SRK) &
      .AND. (testET2(2)%dx2rd .APPROXEQ. 0._SRK) .AND. (testET2(2)%rdx .APPROXEQ. 1000._SRK) &
        .AND. (testET2(2)%rdx2rd .APPROXEQ. 0._SRK) .AND. (testET2(2)%minVal == -10) &
          .AND. (testET2(2)%maxVal == 0) .AND. (testET2(2)%tableErr .APPROXEQ. 0.5e-3_SRK))) THEN
    WRITE(*,*) 'CALL testET2(2)%initialize(...) FAILED!'
    STOP 666
  ENDIF
  xtest=-10._SRK
  DO i=-10000,0
    reftbl(i)=1._SRK-exp(xtest)
    xtest=xtest+1.e-3_SRK
  ENDDO
  IF(.NOT.(ANY(reftbl .APPROXEQ. testET2(2)%table))) THEN
    WRITE(*,*) 'CALL testET2(2)%initialize(...) FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: CALL testET2(2)%initialize(...) SINGLE_LEVEL'
  ENDIF
  !For TWO_LEVEL_EXP_TABLE exponent function
  IF(.NOT.(testET2(3)%isinit .AND. testET2(3)%tableType == 3 &
    .AND. testET2(3)%nintervals == 1000 .AND. (testET2(3)%dx .APPROXEQ. 1.e-3_SRK) &
      .AND. (testET2(3)%dx2rd .APPROXEQ. 1.e-6_SRK) .AND. (testET2(3)%rdx .APPROXEQ. 1000._SRK) &
        .AND. (testET2(3)%rdx2rd .APPROXEQ. 1.e6_SRK) .AND. (testET2(3)%minVal == -10) &
          .AND. (testET2(3)%maxVal == 0) .AND. (testET2(3)%tableErr .APPROXEQ. 0.5e-6_SRK))) THEN
    WRITE(*,*) 'CALL testET2(3)%initialize(...) FAILED!'
    STOP 666
  ENDIF
  xtest=-10._SRK
  DO i=-10000,0
    reftbl(i)=exp(xtest)
    xtest=xtest+1.e-3_SRK
  ENDDO
  IF(.NOT.(ANY(reftbl .APPROXEQ. testET2(3)%table))) THEN
    WRITE(*,*) 'CALL testET2(3)%initialize(...) FAILED!'
    STOP 666
  ENDIF
  xtest=0._SRK
  DO i=1,1000
    xtest=xtest+1.e-6_SRK
    reftbl4(i)=exp(xtest)
  ENDDO
  IF(.NOT.(ANY(reftbl4 .APPROXEQ. testET2(3)%table2rd))) THEN
    WRITE(*,*) 'CALL testET2(3)%initialize(...) FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: CALL testET2(3)%initialize(...) TWO_LEVEL'
  ENDIF
  !For LINEAR_EXP_TABLE exponent function
  IF(.NOT.(testET2(4)%isinit .AND. testET2(4)%tableType == 4 &
    .AND. testET2(4)%nintervals == 1000 .AND. (testET2(4)%dx .APPROXEQ. 1.e-3_SRK) &
      .AND. (testET2(4)%dx2rd .APPROXEQ. 0._SRK) .AND. (testET2(4)%rdx .APPROXEQ. 1000._SRK) &
        .AND. (testET2(4)%rdx2rd .APPROXEQ. 0._SRK) .AND. (testET2(4)%minVal == -10) &
          .AND. (testET2(4)%maxVal == 0) .AND. (testET2(4)%tableErr .APPROXEQ. 1.25e-7_SRK))) THEN
    WRITE(*,*) 'CALL testET2(4)%initialize(...) FAILED!'
    STOP 666
  ENDIF
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
    IF(.NOT.(reftbl(i) .APPROXEQ. testET2(4)%table2D(1,i))) THEN
      WRITE(istr,"(i6)")i;istr=ADJUSTL(istr)
      WRITE(*,*) 'CALL testET2(4)%initialize(...) FAILED! testET2(4)%table('//TRIM(istr)//')'
      STOP 666
    ENDIF
  ENDDO
  DO i=-10000,0
    IF(.NOT.(reftbl2(i) .APPROXEQ. testET2(4)%table2D(2,i))) THEN
      WRITE(istr,"(i6)")i;istr=ADJUSTL(istr)
      WRITE(*,*) 'CALL testET2(4)%initialize(...) FAILED! testET2(4)%table2rd('//TRIM(istr)//')'
      STOP 666
    ENDIF
  ENDDO
  WRITE(*,*) '  Passed: CALL testET2(4)%initialize(...) LINEAR'
  
  !For ORDER2_EXP_TABLE exponent function
  IF(.NOT.(testET2(5)%isinit .AND. testET2(5)%tableType == 5 &
    .AND. testET2(5)%nintervals == 1000 .AND. (testET2(5)%dx .APPROXEQ. 1.e-3_SRK) &
      .AND. (testET2(5)%dx2rd .APPROXEQ. 0._SRK) .AND. (testET2(5)%rdx .APPROXEQ. 1000._SRK) &
        .AND. (testET2(5)%rdx2rd .APPROXEQ. 0._SRK) .AND. (testET2(5)%minVal == -10) &
          .AND. (testET2(5)%tableErr .APPROXEQ. 9.630017699314371e-012_SRK) &
            .AND. (testET2(5)%maxVal == 0))) THEN
    WRITE(*,*) 'CALL testET2(5)%initialize(...) FAILED!'
    STOP 666
  ENDIF
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
    IF(.NOT.(reftbl(i) .APPROXEQ. testET2(5)%table(i))) THEN
      WRITE(istr,"(i6)")i;istr=ADJUSTL(istr)
      WRITE(*,*) 'CALL testET2(5)%initialize(...) FAILED! testET2(5)%table('//TRIM(istr)//')'
      STOP 666
    ENDIF
  ENDDO
  DO i=-10000,0
    IF(.NOT.(reftbl2(i) .APPROXEQ. testET2(5)%table2rd(i))) THEN
      WRITE(istr,"(i6)")i;istr=ADJUSTL(istr)
      WRITE(*,*) 'CALL testET2(5)%initialize(...) FAILED! testET2(5)%table2rd('//TRIM(istr)//')'
      STOP 666
    ENDIF
  ENDDO
  DO i=-10000,0
    IF(.NOT.(reftbl3(i) .APPROXEQ. testET2(5)%table3rd(i))) THEN
      WRITE(istr,"(i6)")i;istr=ADJUSTL(istr)
      WRITE(*,*) 'CALL testET2(5)%initialize(...) FAILED! testET2(5)%table3rd('//TRIM(istr)//')'
      STOP 666
    ENDIF
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
  IF(ANY(ABS(err) > 1e-6_SRK)) THEN
    WRITE(*,*) 'tableET%EXPT(x) FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: tableET%EXPT(x)'
  ENDIF
  
  CALL testET1%clear()
  IF(.NOT.(.NOT.testET1%isinit .AND. testET1%tableType == -1 &
    .AND. testET1%nintervals == -1 .AND. (testET1%dx .APPROXEQ. 0._SRK) &
      .AND. (testET1%dx2rd .APPROXEQ. 0._SRK) .AND. (testET1%rdx .APPROXEQ. 0._SRK) &
        .AND. (testET1%rdx2rd .APPROXEQ. 0._SRK) .AND. (testET1%minVal == 0) &
          .AND. (testET1%maxVal == 0) .AND. (testET1%tableErr .APPROXEQ. 0._SRK))) THEN
    WRITE(*,*) 'CALL testET1%clear() FAILED!'
    STOP 666
  ENDIF
  WRITE(*,*) '  Passed: CALL testET1%clear()'
  WRITE(*,*) '==================================================='
  WRITE(*,*) 'TESTING EXPTABLES COMPLETE!'
  WRITE(*,*) '==================================================='
  CALL ErrCheck()
  CALL perftest()
  DO i=1,5
    CALL testET2(i)%clear()
  ENDDO
  !CALL bitEXP()
!
!===============================================================================
CONTAINS
!
!-------------------------------------------------------------------------------
!> This routine computes the numerical error of the tables which can be used to
!> compare to the analytic expressions for verification.
    SUBROUTINE ErrCheck()
      REAL(SRK) :: x(1000),ans(1000),err_input,err_result(1000),maxerr,dx
      INTEGER(SIK) :: nloops,j,i,ntype,nintervals,itype,maxi
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
      INTEGER(SIK) :: xfixed,neval,nval,i
      !REAL(SSK),ALLOCATABLE :: ans(:),xval(:)
      REAL(SSK) :: ans,xval,logvals(27)
      
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
ENDPROGRAM testExpTables
