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
PROGRAM testVectorTypes
  
  USE IntrType
  USE ExceptionHandler
  USE BLAS
  USE ParameterLists
  USE ParallelEnv
  USE VectorTypes
  IMPLICIT NONE
  
#ifdef HAVE_PETSC
#include <finclude/petsc.h>
#undef IS
 
  PetscErrorCode  :: ierr
  
#endif
  INTEGER(SIK) :: iverr
  TYPE(ExceptionHandlerType),POINTER :: e
  
  !Configure exception handler for test
  ALLOCATE(e)
  CALL e%setStopOnError(.FALSE.)
  CALL e%setQuietMode(.TRUE.)
  eVectorType => e
  
#ifdef HAVE_PETSC    
  CALL PetscInitialize(PETSC_NULL_CHARACTER,ierr)
#endif

  WRITE(*,*) '==================================================='
  WRITE(*,*) 'TESTING VECTOR TYPES...'
  WRITE(*,*) '==================================================='
  
  CALL testVector()
  
  !Edit the reference lists
  !CALL reqParamsRealVT%edit(666)
  !WRITE(666,*)
  !WRITE(666,*) '==================================================='
  !WRITE(666,*)
  !CALL optParamsRealVT%edit(666)
  !CALL reqParamsPETScVT%edit(666)
  !WRITE(666,*)
  !WRITE(666,*) '==================================================='
  !WRITE(666,*)
  !CALL optParamsPETScVT%edit(666)
  
  CALL testBLAS1Interface()
  
  WRITE(*,*) '==================================================='
  WRITE(*,*) 'TESTING VECTOR TYPES PASSED!'
  WRITE(*,*) '==================================================='
  DEALLOCATE(e)
  CALL VectorType_Clear_ValidParams()
  
#ifdef HAVE_PETSC    
  CALL PetscFinalize(ierr)
#endif
!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
    SUBROUTINE testVector()
      CLASS(VectorType),ALLOCATABLE :: thisVector
      INTEGER(SIK) :: i,vecsize
      REAL(SRK),ALLOCATABLE :: testvec(:),testvec2(:),dummyvec(:)
      REAL(SRK) :: dummy
      TYPE(ParamType) :: pList

!Test for real vectors      
      !Perform test of clear function
      !make vector without using untested init
      ALLOCATE(RealVectorType :: thisVector)
      SELECTTYPE(thisVector)
        TYPE IS(RealVectorType)
          thisVector%isInit=.TRUE.
          thisVector%n=100
          ALLOCATE(thisVector%b(100))
      ENDSELECT
        
      !clear it
      CALL thisVector%clear()
        
      SELECTTYPE(thisVector)
        TYPE IS(RealVectorType)
          !check for success
          IF((thisVector%isInit).OR.(thisVector%n /= 0)) THEN
            WRITE(*,*) 'CALL realvec%clear() FAILED!'
            STOP 666
          ENDIF
          IF(ALLOCATED(thisVector%b)) THEN
            WRITE(*,*) 'CALL realvec%clear() FAILED!'
            STOP 666
          ENDIF
          WRITE(*,*) '  Passed: CALL realvec%clear()'
      ENDSELECT
      
      !Perform test of init function
      !first check intended init path (m provided)
      eVectorType => NULL()
      CALL pList%add('VectorType->n',10)
      CALL thisVector%init(pList)
      eVectorType => e
      SELECTTYPE(thisVector)
        TYPE IS(RealVectorType)
          !check for success
          IF((.NOT.thisVector%isInit).AND.(thisVector%n /= 10)) THEN
            WRITE(*,*) 'CALL realvec%init(...) FAILED!'
            STOP 666
          ENDIF
          IF((SIZE(thisVector%b) /= 10)) THEN
            WRITE(*,*) 'CALL realvec%init(...) FAILED!'
            STOP 666
          ENDIF
      ENDSELECT  
      CALL thisVector%clear()
      CALL pList%clear()
      
      !now check init without m being provided
      CALL pList%add('VectorType->n',-10)
      CALL thisVector%init(pList) !expect exception
      IF(thisVector%isInit) THEN
        WRITE(*,*) 'CALL realvec%init(...) FAILED!'
        STOP 666
      ENDIF
      CALL thisVector%clear()
      CALL pList%clear()
      
      !init it twice so on 2nd init, isInit==.TRUE.
      CALL pList%add('VectorType->n',10)
      CALL thisVector%init(pList)
      SELECTTYPE(thisVector)
        TYPE IS(RealVectorType); thisVector%n=1
      ENDSELECT
      CALL thisVector%init(pList)
      SELECTTYPE(thisVector)
        TYPE IS(RealVectorType)
          IF(thisVector%n/=1) THEN !n/=1 implies it was changed, and thus fail
            WRITE(*,*) 'CALL realvec%init(...) FAILED!' !expect exception
            STOP 666
          ENDIF
      ENDSELECT
     !init with n<1
      CALL thisVector%clear()
      CALL pList%clear()
      CALL pList%add('VectorType->n',-1)
      CALL thisVector%init(pList) !expect exception
      IF(thisVector%isInit) THEN
        WRITE(*,*) 'CALL realvec%init(...) FAILED!'
        STOP 666
      ENDIF
      CALL thisVector%clear()
      !n<1, and m not provided
      CALL thisVector%init(pList) !expect exception
      IF(thisVector%isInit) THEN
        WRITE(*,*) 'CALL realvec%init(...) FAILED!'
        STOP 666
      ENDIF
      CALL thisVector%clear()
      
      !init with m<1
      CALL thisVector%clear()
      CALL pList%clear()
      CALL pList%add('VectorType->n',-10)
      CALL thisVector%init(pList) !expect exception
      IF(thisVector%isInit) THEN
        WRITE(*,*) 'CALL realvec%init(...) FAILED!'
        STOP 666
      ENDIF
      CALL thisVector%clear()
      WRITE(*,*) '  Passed: CALL realvec%init(...)'
      
      !Perform test of set function
      !use set to update the values
      CALL pList%clear()
      CALL pList%add('VectorType->n',6)
      CALL thisVector%init(pList)
      CALL thisVector%set(1,1._SRK)
      CALL thisVector%set(2,2._SRK)
      CALL thisVector%set(3,3._SRK)
      CALL thisVector%set(4,4._SRK)
      CALL thisVector%set(5,5._SRK)
      CALL thisVector%set(6,6._SRK,iverr)
      SELECTTYPE(thisVector)
        TYPE IS(RealVectorType)
          !now compare actual values with expected
          DO i=1,6
            IF((thisVector%b(i) /= i) .AND. iverr /= 0) THEN
              WRITE(*,*) 'CALL realvec%setOne(...) FAILED!'
              STOP 666
            ENDIF
          ENDDO
      ENDSELECT
      
      !set uninit matrix.
      CALL thisVector%clear()
      SELECTTYPE(thisVector)
        TYPE IS(RealVectorType)
          CALL thisVector%set(1,1._SRK,iverr) !since isInit=.FALSE. expect no change
          IF(iverr /= -1) THEN ! -1 means not initialized
            WRITE(*,*) 'CALL realvec%setOne(...) FAILED!'
            STOP 666
          ENDIF
      ENDSELECT
      
      !pass out-of bounds i and j
      CALL thisVector%clear()
      CALL pList%clear()
      CALL pList%add('VectorType->n',6)
      CALL thisVector%init(pList)   
      CALL thisVector%set(-1,1._SRK,iverr)
      SELECTTYPE(thisVector)
        TYPE IS(RealVectorType)
          IF(iverr /= -2) THEN ! -2 means out of bounds
            WRITE(*,*) 'CALL realvec%setOne(...) FAILED!'
            STOP 666
          ENDIF
      ENDSELECT
      CALL thisVector%set(7,1._SRK,iverr)
      SELECTTYPE(thisVector)
        TYPE IS(RealVectorType)
          IF(iverr /= -2) THEN ! -2 means out of bounds
            WRITE(*,*) 'CALL realvec%setOne(...) FAILED!'
            STOP 666
          ENDIF      
      ENDSELECT

      CALL thisVector%clear()
      CALL pList%clear()
      CALL pList%add('VectorType->n',6)
      CALL thisVector%init(pList)
        
      SELECTTYPE(thisVector)
        TYPE IS(RealVectorType)
          DO i=1,SIZE(thisVector%b)
            IF(thisVector%b(i) == 1._SRK) THEN
              WRITE(*,*) 'CALL realvec%setOne(...) FAILED!'
              STOP 666
            ENDIF
          ENDDO
          WRITE(*,*) '  Passed: CALL realvec%setOne(...)'
      ENDSELECT
      
      !Perform test of set function
      !use set to update all values at once (scalar)
      CALL thisVector%init(pList)
      CALL thisVector%set(10._SRK,iverr)
      SELECTTYPE(thisVector)
        TYPE IS(RealVectorType)
          !now compare actual values with expected
          DO i=1,6
            IF((thisVector%b(i) /= 10._SRK .AND. iverr /= 0)) THEN
              WRITE(*,*) 'CALL realvec%setAll_scalar(...) FAILED!'
              STOP 666
            ENDIF
          ENDDO
      ENDSELECT
      
      !set uninit
      CALL thisVector%clear()
      CALL thisVector%set(1._SRK,iverr) !since isInit=.FALSE. expect no change
      SELECTTYPE(thisVector)
        TYPE IS(RealVectorType)
          IF(iverr /= -1) THEN ! -1 means not initialized
            WRITE(*,*) 'CALL realvec%setAll_scalar(...) FAILED!'
            STOP 666
          ENDIF
      ENDSELECT
      
      CALL pList%clear()
      CALL pList%add('VectorType->n',6)
      CALL thisVector%init(pList)
        
      SELECTTYPE(thisVector)
        TYPE IS(RealVectorType)
          DO i=1,SIZE(thisVector%b)
            IF(thisVector%b(i) == 1._SRK) THEN
              WRITE(*,*) 'CALL realvec%setAll_scalar(...) FAILED!'
              STOP 666
            ENDIF
          ENDDO
          WRITE(*,*) '  Passed: CALL realvec%setAll_scalar(...)'
      ENDSELECT
      
      !Perform test of set function
      !use set to update all values at once (array)
      CALL pList%clear()
      CALL pList%add('VectorType->n',6)
      CALL thisVector%init(pList)
      ALLOCATE(testvec(6))
      ALLOCATE(testvec2(6))
      testvec(1)=2._SRK
      testvec(2)=4._SRK
      testvec(3)=6._SRK
      testvec(4)=8._SRK
      testvec(5)=10._SRK
      testvec(6)=12._SRK
      CALL thisVector%set(testvec,iverr)
      SELECTTYPE(thisVector)
        TYPE IS(RealVectorType)
          !now compare actual values with expected
          DO i=1,6
            IF((thisVector%b(i) /= testvec(i)) .AND. iverr /= 0) THEN
              WRITE(*,*) 'CALL realvec%setAll_array(...) FAILED!'
              STOP 666
            ENDIF
          ENDDO
      ENDSELECT
      
      !set uninit
      CALL thisVector%clear()
      testvec2(1)=1._SRK
      testvec2(2)=3._SRK
      testvec2(3)=5._SRK
      testvec2(4)=7._SRK
      testvec2(5)=9._SRK
      testvec2(6)=11._SRK
      CALL thisVector%set(testvec2,iverr) !since isInit=.FALSE. expect no change
      SELECTTYPE(thisVector)
        TYPE IS(RealVectorType)
          IF(iverr /= -1) THEN ! -1 means not initialized
            WRITE(*,*) 'CALL realvec%setAll_array(...) FAILED!'
            STOP 666
          ENDIF
      ENDSELECT
      
      !set unequal size
      CALL thisVector%clear()
      CALL pList%clear()
      CALL pList%add('VectorType->n',7)
      CALL thisVector%init(pList)
      CALL thisVector%set(testvec2,iverr)
      SELECTTYPE(thisVector)
        TYPE IS(RealVectorType)
          IF(iverr /= -3) THEN ! -3 means testvec2 and thisVector not equal sizes
            WRITE(*,*) 'CALL realvec%setAll_array(...) FAILED!'
            STOP 666
          ENDIF
      ENDSELECT
      
      
      CALL thisVector%clear()
      CALL pList%clear()
      CALL pList%add('VectorType->n',6)
      CALL thisVector%init(pList)
        
      SELECTTYPE(thisVector)
        TYPE IS(RealVectorType)
          DO i=1,SIZE(thisVector%b)
            IF(thisVector%b(i) == testvec2(i)) THEN
              WRITE(*,*) 'CALL realvec%setAll_array(...) FAILED!'
              STOP 666
            ENDIF
          ENDDO
          WRITE(*,*) '  Passed: CALL realvec%setAll_array(...)'
      ENDSELECT
      
      !Perform test of set function
      !use set to update all values at once (scalar)
      CALL thisVector%init(pList)
      CALL thisVector%set(4,6,10._SRK,iverr)
      SELECTTYPE(thisVector)
        TYPE IS(RealVectorType)
          !now compare actual values with expected
          DO i=1,3
            IF((thisVector%b(i) /= 0._SRK) .AND. iverr /= 0) THEN
              WRITE(*,*) 'CALL realvec%setRange_scalar(...) FAILED!'
              STOP 666
            ENDIF
          ENDDO
          DO i=4,6
            IF((thisVector%b(i) /= 10._SRK) .AND. iverr /= 0) THEN
              WRITE(*,*) 'CALL realvec%setRange_scalar(...) FAILED!'
              STOP 666
            ENDIF
          ENDDO
      ENDSELECT
      
      !set uninit
      CALL thisVector%clear()
      CALL thisVector%set(4,6,1._SRK,iverr) !since isInit=.FALSE. expect no change
      SELECTTYPE(thisVector)
        TYPE IS(RealVectorType)
          IF(iverr /= -1) THEN ! -1 means not initialized
            WRITE(*,*) 'CALL realvec%setRange_scalar(...) FAILED!'
            STOP 666
          ENDIF
      ENDSELECT
      
      !set out of bounds
      CALL thisVector%clear()
      CALL pList%clear()
      CALL pList%add('VectorType->n',6)
      CALL thisVector%init(pList)
      CALL thisVector%set(5,7,1._SRK,iverr) !since isInit=.FALSE. expect no change
      SELECTTYPE(thisVector)
        TYPE IS(RealVectorType)
          IF(iverr /= -2) THEN ! -2  means out of bounds
            WRITE(*,*) 'CALL realvec%setRange_scalar(...) FAILED!'
            STOP 666
          ENDIF
      ENDSELECT
      CALL thisVector%clear()
      CALL pList%clear()
      CALL pList%add('VectorType->n',6)
      CALL thisVector%init(pList)
      CALL thisVector%set(0,3,1._SRK,iverr) !since isInit=.FALSE. expect no change
      SELECTTYPE(thisVector)
        TYPE IS(RealVectorType)
          IF(iverr /= -2) THEN ! -1 means out of bounds
            WRITE(*,*) 'CALL realvec%setRange_scalar(...) FAILED!'
            STOP 666
          ENDIF
      ENDSELECT
      
      CALL thisVector%init(pList)
        
      SELECTTYPE(thisVector)
        TYPE IS(RealVectorType) 
          DO i=1,SIZE(thisVector%b)
            IF(thisVector%b(i) == 1._SRK) THEN
              WRITE(*,*) 'CALL realvec%setRange_scalar(...) FAILED!'
              STOP 666
            ENDIF
          ENDDO
          WRITE(*,*) '  Passed: CALL realvec%setRange_scalar(...)'
      ENDSELECT
      
      !Perform test of set function
      !use set to update all values at once (array)
      CALL pList%clear()
      CALL pList%add('VectorType->n',6)
      CALL thisVector%init(pList)
      DEALLOCATE(testvec,testvec2)
      ALLOCATE(testvec(3))
      ALLOCATE(testvec2(3))
      testvec(1)=2._SRK
      testvec(2)=4._SRK
      testvec(3)=6._SRK
      CALL thisVector%set(4,6,testvec,iverr)
      SELECTTYPE(thisVector)
        TYPE IS(RealVectorType)
          !now compare actual values with expected
          DO i=1,3
            IF((thisVector%b(i) /= 0._SRK) .AND. iverr /= 0) THEN
              WRITE(*,*) 'CALL realvec%setRange_array(...) FAILED!'
              STOP 666
            ENDIF
          ENDDO
          DO i=4,6
            IF((thisVector%b(i) /= testvec(i-3)) .AND. iverr /= 0) THEN
              WRITE(*,*) 'CALL realvec%setRange_array(...) FAILED!'
              STOP 666
            ENDIF
          ENDDO
      ENDSELECT
      
      !set uninit
      CALL thisVector%clear()
      testvec2(1)=1._SRK
      testvec2(2)=3._SRK
      testvec2(3)=5._SRK
      SELECTTYPE(thisVector)
        TYPE IS(RealVectorType)
          CALL thisVector%set(4,6,testvec2,iverr) !since isInit=.FALSE. expect no change
          IF(iverr /= -1) THEN ! -1 means not initialized
            WRITE(*,*) 'CALL realvec%setRange_array(...) FAILED!'
            STOP 666
          ENDIF
      ENDSELECT
      
      CALL thisVector%clear()
      CALL pList%clear()
      CALL pList%add('VectorType->n',6)
      CALL thisVector%init(pList)
      SELECTTYPE(thisVector)
        TYPE IS(RealVectorType)
          CALL thisVector%set(5,7,testvec2,iverr)
          IF(iverr /= -2) THEN ! -2 means out of bounds
            WRITE(*,*) 'CALL realvec%setRange_array(...) FAILED!'
            STOP 666
          ENDIF
      ENDSELECT
      
      CALL thisVector%clear()
      CALL pList%clear()
      CALL pList%add('VectorType->n',6)
      CALL thisVector%init(pList)
      SELECTTYPE(thisVector)
        TYPE IS(RealVectorType)
          CALL thisVector%set(0,2,testvec2,iverr) 
          IF(iverr /= -2) THEN ! -2 means out of bounds
            WRITE(*,*) 'CALL realvec%setRange_array(...) FAILED!'
            STOP 666
          ENDIF
      ENDSELECT
      
      CALL thisVector%clear()
      CALL pList%clear()
      CALL pList%add('VectorType->n',6)
      CALL thisVector%init(pList)
      SELECTTYPE(thisVector)
        TYPE IS(RealVectorType)
          CALL thisVector%set(2,6,testvec2,iverr) 
          IF(iverr /= -3) THEN ! -3 testvec2 and range not equal
            WRITE(*,*) 'CALL realvec%setRange_array(...) FAILED!'
            STOP 666
          ENDIF
      ENDSELECT
      
      CALL thisVector%init(pList)
        
      SELECTTYPE(thisVector)
        TYPE IS(RealVectorType)
          DO i=1,3
            IF((thisVector%b(i) /= 0._SRK)) THEN
              WRITE(*,*) 'CALL realvec%setRange_array(...) FAILED!'
              STOP 666
            ENDIF
          ENDDO
          DO i=4,6
            IF(thisVector%b(i) == testvec2(i-3)) THEN
              WRITE(*,*) 'CALL realvec%setRange_array(...) FAILED!'
              STOP 666
            ENDIF
          ENDDO
          WRITE(*,*) '  Passed: CALL realvec%setRange_array(...)'
      ENDSELECT
      
      !Perform test of getOne function
      ![1 5 8 9 3 7 2]
      CALL thisVector%clear()
      CALL pList%clear()
      CALL pList%add('VectorType->n',7)
      CALL thisVector%init(pList)
      SELECTTYPE(thisVector)
        TYPE IS(RealVectorType)
          CALL thisVector%set(1,1._SRK)
          CALL thisVector%set(2,5._SRK)
          CALL thisVector%set(3,8._SRK)
          CALL thisVector%set(4,9._SRK)
          CALL thisVector%set(5,3._SRK)
          CALL thisVector%set(6,7._SRK)
          CALL thisVector%set(7,2._SRK)
          IF(ALLOCATED(dummyvec)) DEALLOCATE(dummyvec)
          ALLOCATE(dummyvec(thisVector%n))
          CALL thisVector%get(dummyvec,iverr)
          IF((dummyvec(1) /= 1._SRK) .OR. &
             (dummyvec(2) /= 5._SRK) .OR. &
             (dummyvec(3) /= 8._SRK) .OR. &
             (dummyvec(4) /= 9._SRK) .OR. &
             (dummyvec(5) /= 3._SRK) .OR. &
             (dummyvec(6) /= 7._SRK) .OR. &
             (dummyvec(7) /= 2._SRK) .OR. &
             iverr /= 0) THEN
            WRITE(*,*) 'CALL realvec%getOne(...) FAILED!' 
            STOP 666
          ENDIF
      ENDSELECT
      !test with out of bounds, make sure no crash.
      SELECTTYPE(thisVector)
        TYPE IS(RealVectorType)
          CALL thisVector%get(8,dummy,iverr)
          IF(iverr /= -2) THEN
            WRITE(*,*) 'CALL realvec%getOne(...) FAILED!'
            STOP 666
          ENDIF
          CALL thisVector%get(-1,dummy,iverr)
          IF(iverr /= -2) THEN
            WRITE(*,*) 'CALL realvec%getOne(...) FAILED!'
            STOP 666
          ENDIF
      ENDSELECT
      !test get with uninit, make sure no crash.
      CALL thisVector%clear()
      SELECTTYPE(thisVector)
        TYPE IS(RealVectorType)      
          CALL thisVector%get(1,dummy,iverr)
          IF(iverr /= -1) THEN
            WRITE(*,*) 'CALL realvec%getOne(...) FAILED!'
            STOP 666
          ENDIF
      ENDSELECT
      WRITE(*,*) '  Passed: CALL realvec%getOne(...)'
      
      CALL thisVector%clear()
      CALL pList%clear()
      CALL pList%add('VectorType->n',7)
      CALL thisVector%init(pList)
      DEALLOCATE(testvec)
      ALLOCATE(testvec(7))
      testvec=0._SRK
      SELECTTYPE(thisVector)
        TYPE IS(RealVectorType)
          CALL thisVector%set(1,1._SRK)
          CALL thisVector%set(2,5._SRK)
          CALL thisVector%set(3,8._SRK)
          CALL thisVector%set(4,9._SRK)
          CALL thisVector%set(5,3._SRK)
          CALL thisVector%set(6,7._SRK)
          CALL thisVector%set(7,2._SRK)
          CALL thisVector%get(testvec,iverr)
          IF((testvec(1) /= 1._SRK) .OR. &
             (testvec(2) /= 5._SRK) .OR. &
             (testvec(3) /= 8._SRK) .OR. &
             (testvec(4) /= 9._SRK) .OR. &
             (testvec(5) /= 3._SRK) .OR. &
             (testvec(6) /= 7._SRK) .OR. &
             (testvec(7) /= 2._SRK) .OR. &
             iverr /= 0) THEN
            WRITE(*,*) 'CALL realvec%getAll(...) FAILED!' 
            STOP 666
          ENDIF
      ENDSELECT
      !test get with uninit, make sure no crash.
      CALL thisVector%clear()
      testvec=0._SRK
      SELECTTYPE(thisVector)
        TYPE IS(RealVectorType)    
          CALL thisVector%get(testvec,iverr)
          IF(iverr /= -1) THEN
            WRITE(*,*) 'CALL realvec%getAll(...) FAILED!'
            STOP 666
          ENDIF
      ENDSELECT
      WRITE(*,*) '  Passed: CALL realvec%getAll(...)'
      
      CALL thisVector%clear()
      CALL pList%clear()
      CALL pList%add('VectorType->n',7)
      CALL thisVector%init(pList)
      CALL pList%clear()
      DEALLOCATE(testvec)
      ALLOCATE(testvec(3))
      testvec=0._SRK
      SELECTTYPE(thisVector)
        TYPE IS(RealVectorType)
          CALL thisVector%set(1,1._SRK)
          CALL thisVector%set(2,5._SRK)
          CALL thisVector%set(3,8._SRK)
          CALL thisVector%set(4,9._SRK)
          CALL thisVector%set(5,3._SRK)
          CALL thisVector%set(6,7._SRK)
          CALL thisVector%set(7,2._SRK)
          CALL thisVector%get(5,7,testvec,iverr)
          IF((testvec(1) /= 3._SRK) .OR. &
             (testvec(2) /= 7._SRK) .OR. &
             (testvec(3) /= 2._SRK) .OR. &
             iverr /= 0) THEN
            WRITE(*,*) 'CALL realvec%getRange(...) FAILED1!' 
            STOP 666
          ENDIF
      ENDSELECT
      !test get with uninit, make sure no crash.
      CALL thisVector%clear()
      testvec=0._SRK
      SELECTTYPE(thisVector)
        TYPE IS(RealVectorType)      
          CALL thisVector%get(5,7,testvec,iverr)
          DO i=1,3
            IF(iverr /= -1) THEN
              WRITE(*,*) 'CALL realvec%getRange(...) FAILED2!'
              STOP 666
            ENDIF
          ENDDO
      ENDSELECT
      WRITE(*,*) '  Passed: CALL realvec%getRange(...)'
      DEALLOCATE(thisVector)
 
!Test for PETSc vectors (if necessary)
#ifdef HAVE_PETSC    
      
      !Perform test of clear function
      !make vector without using untested init
      ALLOCATE(PETScVectorType :: thisVector)
      SELECTTYPE(thisVector)
        TYPE IS(PetscVectorType)
          thisVector%isInit=.TRUE.
          thisVector%n=100
          CALL VecCreate(MPI_COMM_WORLD,thisVector%b,ierr)
          CALL VecSetSizes(thisVector%b,PETSC_DECIDE,thisVector%n,ierr)
          CALL VecSetType(thisVector%b,VECMPI,ierr)
          CALL VecSetFromOptions(thisVector%b,ierr)
      ENDSELECT
        
      !clear it
      CALL thisVector%clear()
        
      SELECTTYPE(thisVector)
        TYPE IS(PETScVectorType)
          !check for success
          IF((thisVector%isInit).OR.(thisVector%n /= 0)) THEN
            WRITE(*,*) 'CALL petscvec%clear() FAILED!'
            STOP 666
          ENDIF
!          !check if pointer for b is null (not supported till 3.3)
!          !if not, clear did not destroy it
!          IF(thisVector%b /= PETSC_NULL_REAL) THEN
!            WRITE(*,*) 'CALL petscvec%clear() FAILED!'
!            STOP 666
!          ENDIF
          WRITE(*,*) '  Passed: CALL petscvec%clear()'
      ENDSELECT
      
      !Perform test of init function
      !first check intended init path (m provided)
      eVectorType => NULL()
      CALL pList%clear()
      CALL pList%add('VectorType->n',10)
      CALL pList%add('VectorType->MPI_Comm_ID',PE_COMM_SELF)
      CALL thisVector%init(pList)
      eVectorType => e
      SELECTTYPE(thisVector)
        TYPE IS(PETScVectorType)
          !check for success
          IF((.NOT.thisVector%isInit).AND.(thisVector%n /= 10)) THEN
            WRITE(*,*) 'CALL petscvec%init(...) FAILED!'
            STOP 666
          ENDIF
          CALL VecGetSize(thisVector%b,i,ierr)
          IF(i /= 10) THEN
            WRITE(*,*) 'CALL petscvec%init(...) FAILED!'
            STOP 666
          ENDIF
      ENDSELECT
      CALL thisVector%clear()
        
      !now check init without m being provided
      CALL pList%clear()
      CALL pList%add('VectorType->n',-10)
      CALL pList%add('VectorType->MPI_Comm_ID',PE_COMM_SELF)
      CALL thisVector%init(pList) !expect exception
      IF(thisVector%isInit) THEN
        WRITE(*,*) 'CALL petscvec%init(...) FAILED!'
        STOP 666
      ENDIF
      CALL thisVector%clear()
        
      !init it twice so on 2nd init, isInit==.TRUE.
      CALL pList%clear()
      CALL pList%add('VectorType->n',10)
      CALL pList%add('VectorType->MPI_Comm_ID',PE_COMM_SELF)
      CALL thisVector%init(pList)
      SELECTTYPE(thisVector)
        TYPE IS(PETScVectorType); thisVector%n=1
      ENDSELECT
      CALL thisVector%init(pList)
      SELECTTYPE(thisVector)
        TYPE IS(PETScVectorType)
          IF(thisVector%n/=1) THEN !n/=1 implies it was changed, and thus fail
            WRITE(*,*) 'CALL petscvec%init(...) FAILED!' !expect exception
            STOP 666
          ENDIF
      ENDSELECT
     !init with n<1
      CALL thisVector%clear()
      CALL pList%clear()
      CALL pList%add('VectorType->n',-1)
      CALL pList%add('VectorType->MPI_Comm_ID',PE_COMM_SELF)
      CALL thisVector%init(pList) !expect exception
      IF(thisVector%isInit) THEN
        WRITE(*,*) 'CALL petscvec%init(...) FAILED!'
        STOP 666
      ENDIF
      CALL thisVector%clear()
      !n<1, and m not provided
      CALL pList%clear()
      CALL pList%add('VectorType->n',-1)
      CALL pList%add('VectorType->MPI_Comm_ID',PE_COMM_SELF)
      CALL thisVector%init(pList) !expect exception
      IF(thisVector%isInit) THEN
        WRITE(*,*) 'CALL petscvec%init(...) FAILED!'
        STOP 666
      ENDIF
      CALL thisVector%clear()
      !init with m<1
      CALL thisVector%clear()
      CALL pList%clear()
      CALL pList%add('VectorType->n',-10)
      CALL pList%add('VectorType->MPI_Comm_ID',PE_COMM_SELF)
      CALL thisVector%init(pList) !expect exception
      IF(thisVector%isInit) THEN
        WRITE(*,*) 'CALL petscvec%init(...) FAILED!'
        STOP 666
      ENDIF
      CALL thisVector%clear()
      WRITE(*,*) '  Passed: CALL petscvec%init(...)'
      
      !Perform test of set function
      !use set to update the values
      CALL pList%clear()
      CALL pList%add('VectorType->n',6)
      CALL pList%add('VectorType->MPI_Comm_ID',PE_COMM_SELF)
      CALL thisVector%init(pList)
      CALL thisVector%set(1,1._SRK)
      CALL thisVector%set(2,2._SRK)
      CALL thisVector%set(3,3._SRK)
      CALL thisVector%set(4,4._SRK)
      CALL thisVector%set(5,5._SRK)
      CALL thisVector%set(6,6._SRK,iverr)
      SELECTTYPE(thisVector)
        TYPE IS(PETScVectorType)
          ! manually assemble vector
          CALL VecAssemblyBegin(thisVector%b,ierr)
          CALL VecAssemblyEnd(thisVector%b,ierr)
          thisVector%isAssembled=.TRUE.
          
          !now compare actual values with expected
          DO i=1,6
            CALL VecGetValues(thisVector%b,1,i-1,dummy,ierr)
            IF(dummy /= i .AND. iverr /= 0) THEN
              WRITE(*,*) 'CALL petscvec%setOne(...) FAILED!'
              STOP 666
            ENDIF
          ENDDO
      ENDSELECT
      
      !set uninit matrix.
      CALL thisVector%clear()
      CALL thisVector%set(1,1._SRK,iverr) !since isInit=.FALSE. expect no change
      IF(iverr /= -1) THEN
        WRITE(*,*) 'CALL petscvec%setOne(...) FAILED!'
        STOP 666
      ENDIF
      
      !pass out-of bounds i and j
      CALL thisVector%clear()
      CALL pList%clear()
      CALL pList%add('VectorType->n',6)
      CALL pList%add('VectorType->MPI_Comm_ID',PE_COMM_SELF)
      CALL thisVector%init(pList)   
      CALL thisVector%set(-1,1._SRK,iverr)
      IF(iverr /= -2) THEN
        WRITE(*,*) 'CALL petscvec%setOne(...) FAILED!'
        STOP 666
      ENDIF
      CALL thisVector%set(7,1._SRK,iverr)
      IF(iverr /= -2) THEN
        WRITE(*,*) 'CALL petscvec%setOne(...) FAILED!'
        STOP 666
      ENDIF

      CALL thisVector%clear()
      CALL pList%clear()
      CALL pList%add('VectorType->n',6)
      CALL pList%add('VectorType->MPI_Comm_ID',PE_COMM_SELF)
      CALL thisVector%init(pList)
        
      SELECTTYPE(thisVector)
        TYPE IS(PETScVectorType)
          ! manually assemble vector
          CALL VecAssemblyBegin(thisVector%b,ierr)
          CALL VecAssemblyEnd(thisVector%b,ierr)
          thisVector%isAssembled=.TRUE.
          
          CALL VecGetSize(thisVector%b,vecsize,ierr)
          DO i=1,vecsize
            CALL VecGetValues(thisVector%b,1,i-1,dummy,ierr)
            IF(dummy == 1._SRK) THEN
              WRITE(*,*) 'CALL petscvec%setOne(...) FAILED!'
              STOP 666
            ENDIF
          ENDDO
          WRITE(*,*) '  Passed: CALL petscvec%setOne(...)'
      ENDSELECT
      
      !Perform test of set function
      !use set to update all values at once (scalar)
      CALL thisVector%init(pList)
      CALL thisVector%set(10._SRK,iverr)
      SELECTTYPE(thisVector)
        TYPE IS(PETScVectorType)
          !now compare actual values with expected
          DO i=1,6
            CALL thisVector%get(i,dummy)
            IF((dummy /= 10._SRK) .AND. iverr /= 0) THEN
              WRITE(*,*) 'CALL petscvec%setAll_scalar(...) FAILED!'
              STOP 666
            ENDIF
          ENDDO
      ENDSELECT
      
      !set uninit matrix.
      CALL thisVector%clear()
      CALL thisVector%set(1._SRK,iverr) !since isInit=.FALSE. expect no change
      IF((dummy /= 10._SRK) .AND. iverr /= -1) THEN
        WRITE(*,*) 'CALL petscvec%setAll_scalar(...) FAILED!'
        STOP 666
      ENDIF
      
      CALL pList%clear()
      CALL pList%add('VectorType->n',6)
      CALL pList%add('VectorType->MPI_Comm_ID',PE_COMM_SELF)
      CALL thisVector%init(pList)
        
      SELECTTYPE(thisVector)
        TYPE IS(PETScVectorType)
          ! manually assemble vector
          CALL VecAssemblyBegin(thisVector%b,ierr)
          CALL VecAssemblyEnd(thisVector%b,ierr)
          thisVector%isAssembled=.TRUE.
          
          DO i=1,thisVector%n
            CALL thisVector%get(i,dummy)
            IF(dummy == 1._SRK) THEN
              WRITE(*,*) 'CALL petscvec%setAll_scalar(...) FAILED!'
              STOP 666
            ENDIF
          ENDDO
          WRITE(*,*) '  Passed: CALL petscvec%setAll_scalar(...)'
      ENDSELECT
      
      !Perform test of set function 
      !use set to update all values at once (array)
      CALL pList%clear()
      CALL pList%add('VectorType->n',6)
      CALL pList%add('VectorType->MPI_Comm_ID',PE_COMM_SELF)
      CALL thisVector%init(pList)
      DEALLOCATE(testvec)
      DEALLOCATE(testvec2)
      ALLOCATE(testvec(6))
      ALLOCATE(testvec2(6))
      testvec(1)=2._SRK
      testvec(2)=4._SRK
      testvec(3)=6._SRK
      testvec(4)=8._SRK
      testvec(5)=10._SRK
      testvec(6)=12._SRK
      CALL thisVector%set(testvec,iverr)
      SELECTTYPE(thisVector)
        TYPE IS(PETScVectorType)
          !now compare actual values with expected
          DO i=1,6
            CALL thisVector%get(i,dummy)
            IF((dummy /= testvec(i)) .AND. iverr /= 0) THEN
              WRITE(*,*) 'CALL petscvec%setAll_array(...) FAILED!'
              STOP 666
            ENDIF
          ENDDO
      ENDSELECT
      
      !set uninit matrix.
      CALL thisVector%clear()
      testvec2(1)=1._SRK
      testvec2(2)=3._SRK
      testvec2(3)=5._SRK
      testvec2(4)=7._SRK
      testvec2(5)=9._SRK
      testvec2(6)=11._SRK
      CALL thisVector%set(testvec2,iverr) !since isInit=.FALSE. expect no change
      IF(iverr /= -1) THEN
        WRITE(*,*) 'CALL petscvec%setAll_array(...) FAILED!'
        STOP 666
      ENDIF
      
      !set improper size
      CALL thisVector%clear()
      CALL pList%clear()
      CALL pList%add('VectorType->n',7)
      CALL pList%add('VectorType->MPI_Comm_ID',PE_COMM_SELF)
      CALL thisVector%init(pList)
      testvec2(1)=1._SRK
      testvec2(2)=3._SRK
      testvec2(3)=5._SRK
      testvec2(4)=7._SRK
      testvec2(5)=9._SRK
      testvec2(6)=11._SRK
      CALL thisVector%set(testvec2,iverr) !since isInit=.FALSE. expect no change
      IF(iverr /= -3) THEN
        WRITE(*,*) 'CALL petscvec%setAll_array(...) FAILED!'
        STOP 666
      ENDIF
      
      CALL thisVector%clear()
      CALL pList%clear()
      CALL pList%add('VectorType->n',6)
      CALL pList%add('VectorType->MPI_Comm_ID',PE_COMM_SELF)
      CALL thisVector%init(pList)
      SELECTTYPE(thisVector)
        TYPE IS(PETScVectorType)
          ! manually assemble vector
          CALL VecAssemblyBegin(thisVector%b,ierr)
          CALL VecAssemblyEnd(thisVector%b,ierr)
          thisVector%isAssembled=.TRUE.
          
          DO i=1,thisVector%n
            CALL thisVector%get(i,dummy)
            IF(dummy == testvec2(i)) THEN
              WRITE(*,*) 'CALL petscvec%setAll_array(...) FAILED!'
              STOP 666
            ENDIF
          ENDDO
          WRITE(*,*) '  Passed: CALL petscvec%setAll_array(...)'
      ENDSELECT
      
      !Perform test of set function
      !use set to update all values at once (scalar)
      CALL pList%clear()
      CALL pList%add('VectorType->n',6)
      CALL pList%add('VectorType->MPI_Comm_ID',PE_COMM_SELF)
      CALL thisVector%init(pList)
      CALL thisVector%set(4,6,10._SRK,iverr)
      SELECTTYPE(thisVector)
        TYPE IS(PETScVectorType)
          !now compare actual values with expected
          DO i=1,3
            CALL thisVector%get(i,dummy)
            IF((dummy /= 0._SRK)) THEN
              WRITE(*,*) 'CALL petscvec%setRange_scalar(...) FAILED!'
              STOP 666
            ENDIF
          ENDDO
          DO i=4,6
            CALL thisVector%get(i,dummy)
            IF((dummy /= 10._SRK)) THEN
              WRITE(*,*) 'CALL petscvec%setRange_scalar(...) FAILED!'
              STOP 666
            ENDIF
          ENDDO
      ENDSELECT
      
      !set uninit matrix.
      CALL thisVector%clear()
      CALL thisVector%set(4,6,1._SRK,iverr) !since isInit=.FALSE. expect no change
      IF((iverr /= -1._SRK)) THEN
        WRITE(*,*) 'CALL petscvec%setRange_scalar(...) FAILED!'
        STOP 666
      ENDIF
      
      !set out of bounds
      CALL thisVector%clear()
      CALL pList%clear()
      CALL pList%add('VectorType->n',6)
      CALL pList%add('VectorType->MPI_Comm_ID',PE_COMM_SELF)
      CALL thisVector%init(pList)
      CALL thisVector%set(4,7,1._SRK,iverr) 
      IF((iverr /= -2._SRK)) THEN
        WRITE(*,*) 'CALL petscvec%setRange_scalar(...) FAILED!'
        STOP 666
      ENDIF
      
      !set out of bounds
      CALL thisVector%clear()
      CALL pList%clear()
      CALL pList%add('VectorType->n',6)
      CALL pList%add('VectorType->MPI_Comm_ID',PE_COMM_SELF)
      CALL thisVector%init(pList)
      CALL thisVector%set(0,3,1._SRK,iverr)
      IF((iverr /= -2._SRK)) THEN
        WRITE(*,*) 'CALL petscvec%setRange_scalar(...) FAILED!'
        STOP 666
      ENDIF
      
      CALL thisVector%clear()
      CALL pList%clear()
      CALL pList%add('VectorType->n',6)
      CALL pList%add('VectorType->MPI_Comm_ID',PE_COMM_SELF)
      CALL thisVector%init(pList)
        
      SELECTTYPE(thisVector)
        TYPE IS(PETScVectorType)
          ! manually assemble vector
          CALL VecAssemblyBegin(thisVector%b,ierr)
          CALL VecAssemblyEnd(thisVector%b,ierr)
          thisVector%isAssembled=.TRUE.
          
          DO i=1,thisVector%n
            CALL thisVector%get(i,dummy)
            IF(dummy == 1._SRK) THEN
              WRITE(*,*) 'CALL petscvec%setRange_scalar(...) FAILED!'
              STOP 666
            ENDIF
          ENDDO
          WRITE(*,*) '  Passed: CALL petscvec%setRange_scalar(...)'
      ENDSELECT
      
      !Perform test of set function
      !use set to update all values at once (array)
      CALL pList%clear()
      CALL pList%add('VectorType->n',6)
      CALL pList%add('VectorType->MPI_Comm_ID',PE_COMM_SELF)
      CALL thisVector%init(pList)
      DEALLOCATE(testvec)
      DEALLOCATE(testvec2)
      ALLOCATE(testvec(3))
      testvec(1)=1._SRK
      testvec(2)=3._SRK
      testvec(3)=5._SRK
      CALL thisVector%set(4,6,testvec,iverr)
      SELECTTYPE(thisVector)
        TYPE IS(PETScVectorType)
          !now compare actual values with expected
          DO i=1,3
            CALL thisVector%get(i,dummy)
            IF((dummy /= 0._SRK) .AND. iverr /= 0) THEN
              WRITE(*,*) 'CALL petscvec%setRange_array(...) FAILED!'
              STOP 666
            ENDIF
          ENDDO
          DO i=4,6
            CALL thisVector%get(i,dummy)
            IF((dummy /= testvec(i-3)) .AND. iverr /= 0) THEN
              WRITE(*,*) 'CALL petscvec%setRange_array(...) FAILED!'
              STOP 666
            ENDIF
          ENDDO
      ENDSELECT
      
      !set uninit matrix.
      CALL thisVector%clear()
      ALLOCATE(testvec2(3))
      testvec2(1)=2._SRK
      testvec2(2)=4._SRK
      testvec2(3)=6._SRK
      CALL thisVector%set(4,6,testvec2,iverr) !since isInit=.FALSE. expect no change
      IF(iverr /= -1) THEN
        WRITE(*,*) 'CALL petscvec%setRange_array(...) FAILED!'
        STOP 666
      ENDIF
      DEALLOCATE(testvec2)
      
      !set size not equal to range
      CALL thisVector%clear()
      CALL pList%clear()
      CALL pList%add('VectorType->n',6)
      CALL pList%add('VectorType->MPI_Comm_ID',PE_COMM_SELF)
      CALL thisVector%init(pList)
      ALLOCATE(testvec2(3))
      testvec2(1)=2._SRK
      testvec2(2)=4._SRK
      testvec2(3)=6._SRK
      CALL thisVector%set(2,6,testvec2,iverr)
      IF(iverr /= -3) THEN
        WRITE(*,*) 'CALL petscvec%setRange_array(...) FAILED!'
        STOP 666
      ENDIF
            
      CALL thisVector%clear()
      CALL pList%clear()
      CALL pList%add('VectorType->n',6)
      CALL pList%add('VectorType->MPI_Comm_ID',PE_COMM_SELF)
      CALL thisVector%init(pList)
        
      SELECTTYPE(thisVector)
        TYPE IS(PETScVectorType)
          ! manually assemble vector
          CALL VecAssemblyBegin(thisVector%b,ierr)
          CALL VecAssemblyEnd(thisVector%b,ierr)
          thisVector%isAssembled=.TRUE.
          
          DO i=4,6
            CALL thisVector%get(i,dummy)
            IF(dummy == testvec2(i-3)) THEN
              WRITE(*,*) 'CALL petscvec%setRange_array(...) FAILED!'
              STOP 666
            ENDIF
          ENDDO
          WRITE(*,*) '  Passed: CALL petscvec%setRange_array(...)'
      ENDSELECT

      !Perform test of get function
      ![1 5 8 9 3 7 2]
      CALL thisVector%clear()
      CALL pList%clear()
      CALL pList%add('VectorType->n',7)
      CALL pList%add('VectorType->MPI_Comm_ID',PE_COMM_SELF)
      CALL thisVector%init(pList)
      SELECTTYPE(thisVector)
        TYPE IS(PETScVectorType)
          CALL thisVector%set(1,1._SRK)
          CALL thisVector%set(2,5._SRK)
          CALL thisVector%set(3,8._SRK)
          CALL thisVector%set(4,9._SRK)
          CALL thisVector%set(5,3._SRK)
          CALL thisVector%set(6,7._SRK)
          CALL thisVector%set(7,2._SRK)
          IF(ALLOCATED(dummyvec)) DEALLOCATE(dummyvec)
          ALLOCATE(dummyvec(thisVector%n))
          CALL thisVector%get(dummyvec)
          IF((dummyvec(1) /= 1._SRK) .OR. &
             (dummyvec(2) /= 5._SRK) .OR. &
             (dummyvec(3) /= 8._SRK) .OR. &
             (dummyvec(4) /= 9._SRK) .OR. &
             (dummyvec(5) /= 3._SRK) .OR. &
             (dummyvec(6) /= 7._SRK) .OR. &
             (dummyvec(7) /= 2._SRK)) THEN
            WRITE(*,*) 'CALL petscvec%getOne(...) FAILED!' 
            STOP 666
          ENDIF
      ENDSELECT
      !test with out of bounds, make sure no crash.
      SELECTTYPE(thisVector)
        TYPE IS(PETScVectorType)
          CALL thisVector%get(8,dummy,iverr)
          IF(iverr /= -2) THEN
            WRITE(*,*) 'CALL petscvec%getOne(...) FAILED!'
            STOP 666
          ENDIF
          CALL thisVector%get(-1,dummy)
          IF(iverr /= -2) THEN
            WRITE(*,*) 'CALL petscvec%getOne(...) FAILED!'
            STOP 666
          ENDIF
      ENDSELECT
      !test get with uninit, make sure no crash.
      CALL thisVector%clear()
      SELECTTYPE(thisVector)
        TYPE IS(PETScVectorType)      
          CALL thisVector%get(1,dummy,iverr)
          IF(iverr /= -1) THEN
            WRITE(*,*) 'CALL petscvec%getOne(...) FAILED!'
            STOP 666
          ENDIF
      ENDSELECT
      CALL thisVector%clear()
      WRITE(*,*) '  Passed: CALL petscvec%getOne(...)'
      
      !Perform test of get function
      ![1 5 8 9 3 7 2]
      CALL thisVector%clear()
      CALL pList%clear()
      CALL pList%add('VectorType->n',7)
      CALL pList%add('VectorType->MPI_Comm_ID',PE_COMM_SELF)
      CALL thisVector%init(pList)
      DEALLOCATE(testvec)
      ALLOCATE(testvec(7))
      SELECTTYPE(thisVector)
        TYPE IS(PETScVectorType)
          CALL thisVector%set(1,1._SRK)
          CALL thisVector%set(2,5._SRK)
          CALL thisVector%set(3,8._SRK)
          CALL thisVector%set(4,9._SRK)
          CALL thisVector%set(5,3._SRK)
          CALL thisVector%set(6,7._SRK)
          CALL thisVector%set(7,2._SRK)
          CALL thisVector%get(testvec)
          IF((testvec(1) /= 1._SRK) .OR. &
             (testvec(2) /= 5._SRK) .OR. &
             (testvec(3) /= 8._SRK) .OR. &
             (testvec(4) /= 9._SRK) .OR. &
             (testvec(5) /= 3._SRK) .OR. &
             (testvec(6) /= 7._SRK) .OR. &
             (testvec(7) /= 2._SRK)) THEN
            WRITE(*,*) 'CALL petscvec%getAll(...) FAILED!' 
            STOP 666
          ENDIF
      ENDSELECT
      !test get with uninit, make sure no crash.
      CALL thisVector%clear()
      SELECTTYPE(thisVector)
        TYPE IS(PETScVectorType)      
          CALL thisVector%get(testvec,iverr)
          IF(iverr /= -1) THEN
            WRITE(*,*) 'CALL petscvec%getAll(...) FAILED!'
            STOP 666
          ENDIF
      ENDSELECT
      CALL thisVector%clear()
      WRITE(*,*) '  Passed: CALL petscvec%getAll(...)'
      
      !Perform test of get function
      ![1 5 8 9 3 7 2]
      CALL thisVector%clear()
      CALL pList%clear()
      CALL pList%add('VectorType->n',7)
      CALL pList%add('VectorType->MPI_Comm_ID',PE_COMM_SELF)
      CALL thisVector%init(pList)
      DEALLOCATE(testvec)
      ALLOCATE(testvec(3))
      SELECTTYPE(thisVector)
        TYPE IS(PETScVectorType)
          CALL thisVector%set(1,1._SRK)
          CALL thisVector%set(2,5._SRK)
          CALL thisVector%set(3,8._SRK)
          CALL thisVector%set(4,9._SRK)
          CALL thisVector%set(5,3._SRK)
          CALL thisVector%set(6,7._SRK)
          CALL thisVector%set(7,2._SRK)
          CALL thisVector%get(5,7,testvec)
          IF((testvec(1) /= 3._SRK) .OR. &
             (testvec(2) /= 7._SRK) .OR. &
             (testvec(3) /= 2._SRK)) THEN
            WRITE(*,*) 'CALL petscvec%getRange(...) FAILED!' 
            STOP 666
          ENDIF
      ENDSELECT
      !test with out of bounds, make sure no crash.
      SELECTTYPE(thisVector)
        TYPE IS(PETScVectorType)
          CALL thisVector%get(2,8,testvec,iverr)
          IF(iverr /= -2) THEN
            WRITE(*,*) 'CALL petscvec%getRange(...) FAILED!'
            STOP 666
          ENDIF
          CALL thisVector%get(-1,2,testvec,iverr)
          IF(iverr /= -2) THEN
            WRITE(*,*) 'CALL petscvec%getRange(...) FAILED!'
            STOP 666
          ENDIF
      ENDSELECT
      !test get with uninit, make sure no crash.
      CALL thisVector%clear()
      SELECTTYPE(thisVector)
        TYPE IS(PETScVectorType)      
          CALL thisVector%get(1,3,testvec,iverr)
          IF(iverr /= -1) THEN
            WRITE(*,*) 'CALL petscvec%getRange(...) FAILED!'
            STOP 666
          ENDIF
      ENDSELECT
      CALL thisVector%clear()
      WRITE(*,*) '  Passed: CALL petscvec%getRange(...)'
      
      DEALLOCATE(thisVector)
#else
  ! Never mind, these are fatal errors apparently.
  !    !Call PETSc objects for code coverage.  
  !    !All of these calls should produce errors
  !    ALLOCATE(PETScVectorType :: thisVector)
  !    !clear it
  !    CALL thisVector%clear()
  !    !Set one scalar
  !    CALL thisVector%set(1,1._SRK)
  !    !Set all scalar
  !    CALL thisVector%set(1._SRK)
  !    !Set all vector
  !    CALL thisVector%set((/1._SRK,1._SRK/))
  !    !Set range scalar
  !    CALL thisVector%set(1,1,1._SRK)
  !    !Set range vector
  !    CALL thisVector%set(1,1,(/1._SRK,1._SRK/))
  !    !Get one scalar
  !    CALL thisVector%get(1,dummy)
  !    !Get all vector
  !    CALL thisVector%get(dummyvec)
  !    !Get range vector
  !    CALL thisVector%get(1,1,dummyvec)
#endif
      
    ENDSUBROUTINE testVector
!
!-------------------------------------------------------------------------------  
    SUBROUTINE testBLAS1Interface()
    
    CLASS(VectorType),ALLOCATABLE :: xVector, yVector, aVector
    REAL(SRK) :: r, a
    REAL(SRK),ALLOCATABLE :: dummyvec(:),dummyvec2(:)
    INTEGER(SIK) :: r_index
    TYPE(ParamType) :: pList
    ! test with real vectors
    ALLOCATE(RealVectorType :: xVector)
    ALLOCATE(RealVectorType :: yVector)
    ALLOCATE(RealVectorType :: aVector)
    CALL pList%clear()
    CALL pList%add('VectorType->n',3)
    CALL xVector%init(pList)
    CALL yVector%init(pList)
    CALL aVector%init(pList)
    CALL pList%clear()

    !Test BLAS_asum (absolute value summation)
    CALL xVector%set(1, 5.0_SRK)
    CALL xVector%set(2,-2.0_SRK)
    CALL xVector%set(3, 7.0_SRK)
    r=0.0_SRK
    r = BLAS_asum(THISVECTOR=xVector,N=xVector%n,INCX=1)
    IF(.NOT.(r .APPROXEQ. 14.0_SRK)) THEN
      WRITE(*,*) "CALL BLAS_asum(THISVECTOR=xVector,N=xVector%n,INCX=1) [REAL] FAILED!"
      STOP 666
    ENDIF
    r=0.0_SRK
    r = BLAS_asum(THISVECTOR=xVector,INCX=1)
    IF(.NOT.(r .APPROXEQ. 14.0_SRK)) THEN
      WRITE(*,*) "CALL BLAS_asum(THISVECTOR=xVector,INCX=1) [REAL] FAILED!"
      STOP 666
    ENDIF
    r=0.0_SRK
    r = BLAS_asum(THISVECTOR=xVector,N=xVector%n)
    IF(.NOT.(r .APPROXEQ. 14.0_SRK)) THEN
      WRITE(*,*) "CALL BLAS_asum(THISVECTOR=xVector,N=xVector%n) [REAL] FAILED!"
      STOP 666
    ENDIF
    r=0.0_SRK
    r = BLAS_asum(THISVECTOR=xVector)
    IF(.NOT.(r .APPROXEQ. 14.0_SRK)) THEN
      WRITE(*,*) "CALL BLAS_asum(THISVECTOR=xVector) [REAL] FAILED!"
      STOP 666
    ENDIF
    WRITE(*,*) '  Passed: CALL BLAS_asum(...) [REAL]'
    
    !Test BLAS_axpy (y=y+ax) [scalar a]
    a = 5
    CALL xVector%set(1, 5.0_SRK)
    CALL xVector%set(2,-2.0_SRK)
    CALL xVector%set(3, 7.0_SRK)
    CALL yVector%set(1, 3.0_SRK)
    CALL yVector%set(2, 5.0_SRK)
    CALL yVector%set(3, 1.0_SRK)
    CALL BLAS_axpy(THISVECTOR=xVector,NEWVECTOR=yVector,A=a,N=xVector%n,INCX=1,INCY=1)
    IF(ALLOCATED(dummyvec)) DEALLOCATE(dummyvec)
    ALLOCATE(dummyvec(yVector%n))
    CALL yVector%get(dummyvec)
    IF((.NOT.((dummyvec(1) .APPROXEQ. 28._SRK) .AND. &
              (dummyvec(2) .APPROXEQ. -5._SRK) .AND. &
              (dummyvec(3) .APPROXEQ. 36._SRK)))) THEN
      WRITE(*,*) "CALL BLAS_axpy(THISVECTOR=xVector,NEWVECTOR=yVector,A=a,N=xVector%n,INCX=1,INCY=1) -scalar_a [REAL] FAILED1!"
      STOP 666
    ENDIF
    CALL yVector%set(1,3.0_SRK)
    CALL yVector%set(2,5.0_SRK)
    CALL yVector%set(3,1.0_SRK)
    CALL BLAS_axpy(THISVECTOR=xVector,NEWVECTOR=yVector,A=a,N=xVector%n,INCX=1)
    CALL yVector%get(dummyvec)
    IF((.NOT.((dummyvec(1) .APPROXEQ. 28._SRK) .AND. &
              (dummyvec(2) .APPROXEQ. -5._SRK) .AND. &
              (dummyvec(3) .APPROXEQ. 36._SRK)))) THEN
      WRITE(*,*) "CALL BLAS_axpy(THISVECTOR=xVector,NEWVECTOR=yVector,A=a,N=xVector%n,INCX=1) -scalar_a [REAL] FAILED2!"
      STOP 666
    ENDIF
    CALL yVector%set(1,3.0_SRK)
    CALL yVector%set(2,5.0_SRK)
    CALL yVector%set(3,1.0_SRK)
    CALL BLAS_axpy(THISVECTOR=xVector,NEWVECTOR=yVector,A=a,N=xVector%n,INCY=1)
    CALL yVector%get(dummyvec)
    IF((.NOT.((dummyvec(1) .APPROXEQ. 28._SRK) .AND. &
              (dummyvec(2) .APPROXEQ. -5._SRK) .AND. &
              (dummyvec(3) .APPROXEQ. 36._SRK)))) THEN
      WRITE(*,*) "CALL BLAS_axpy(THISVECTOR=xVector,NEWVECTOR=yVector,A=a,N=xVector%n,INCY=1) -scalar_a [REAL] FAILED3!"
      STOP 666
    ENDIF
    CALL yVector%set(1,3.0_SRK)
    CALL yVector%set(2,5.0_SRK)
    CALL yVector%set(3,1.0_SRK)
    CALL BLAS_axpy(THISVECTOR=xVector,NEWVECTOR=yVector,A=a,N=xVector%n)
    CALL yVector%get(dummyvec)
    IF((.NOT.((dummyvec(1) .APPROXEQ. 28._SRK) .AND. &
              (dummyvec(2) .APPROXEQ. -5._SRK) .AND. &
              (dummyvec(3) .APPROXEQ. 36._SRK)))) THEN
      WRITE(*,*) "CALL BLAS_axpy(THISVECTOR=xVector,NEWVECTOR=yVector,A=a,N=xVector%n) -scalar_a [REAL] FAILED4!"
      STOP 666
    ENDIF
    CALL yVector%set(1,3.0_SRK)
    CALL yVector%set(2,5.0_SRK)
    CALL yVector%set(3,1.0_SRK)
    CALL BLAS_axpy(THISVECTOR=xVector,NEWVECTOR=yVector,A=a)
    CALL yVector%get(dummyvec)
    IF((.NOT.((dummyvec(1) .APPROXEQ. 28._SRK) .AND. &
              (dummyvec(2) .APPROXEQ. -5._SRK) .AND. &
              (dummyvec(3) .APPROXEQ. 36._SRK)))) THEN
      WRITE(*,*) "CALL BLAS_axpy(THISVECTOR=xVector,NEWVECTOR=yVector,A=a) -scalar_a [REAL] FAILED5!"
      STOP 666
    ENDIF
    WRITE(*,*) '  Passed: CALL BLAS_axpy(...) -scalar_a [REAL]'
    
    !Test BLAS_axpy (y=y+ax) [vector a]
    a = 3
    CALL xVector%set(1,5.0_SRK)
    CALL xVector%set(2,-2.0_SRK)
    CALL xVector%set(3,7.0_SRK)
    CALL yVector%set(1,3.0_SRK)
    CALL yVector%set(2,5.0_SRK)
    CALL yVector%set(3,1.0_SRK)
    CALL aVector%set(1,6.0_SRK)
    CALL aVector%set(2,4.0_SRK)
    CALL aVector%set(3,2.0_SRK)
    CALL BLAS_axpy(THISVECTOR=xVector,NEWVECTOR=yVector,AVECTOR=aVector,N=xVector%n,INCX=1,INCY=1)
    IF(ALLOCATED(dummyvec)) DEALLOCATE(dummyvec)
    ALLOCATE(dummyvec(yVector%n))
    CALL yVector%get(dummyvec)
    IF((.NOT.((dummyvec(1) .APPROXEQ. 33._SRK) .AND. &
              (dummyvec(2) .APPROXEQ. -3._SRK) .AND. &
              (dummyvec(3) .APPROXEQ. 15._SRK)))) THEN
      WRITE(*,*) "CALL BLAS_axpy(THISVECTOR=xVector,NEWVECTOR=yVector,AVECTOR=aVector,N=xVector%n,INCX=1,INCY=1)", &
                   "-vector_a [REAL] FAILED!"
      STOP 666
    ENDIF
    CALL yVector%set(1,3.0_SRK)
    CALL yVector%set(2,5.0_SRK)
    CALL yVector%set(3,1.0_SRK)
    CALL BLAS_axpy(THISVECTOR=xVector,NEWVECTOR=yVector,AVECTOR=aVector,N=xVector%n,INCX=1)
    CALL yVector%get(dummyvec)
    IF((.NOT.((dummyvec(1) .APPROXEQ. 33._SRK) .AND. &
              (dummyvec(2) .APPROXEQ. -3._SRK) .AND. &
              (dummyvec(3) .APPROXEQ. 15._SRK)))) THEN
      WRITE(*,*) "CALL BLAS_axpy(THISVECTOR=xVector,NEWVECTOR=yVector,AVECTOR=aVector,N=xVector%n,INCX=1) -vector_a [REAL] FAILED!"
      STOP 666
    ENDIF
    CALL yVector%set(1,3.0_SRK)
    CALL yVector%set(2,5.0_SRK)
    CALL yVector%set(3,1.0_SRK)
    CALL BLAS_axpy(THISVECTOR=xVector,NEWVECTOR=yVector,AVECTOR=aVector,N=xVector%n,INCY=1)
    CALL yVector%get(dummyvec)
    IF((.NOT.((dummyvec(1) .APPROXEQ. 33._SRK) .AND. &
              (dummyvec(2) .APPROXEQ. -3._SRK) .AND. &
              (dummyvec(3) .APPROXEQ. 15._SRK)))) THEN
      WRITE(*,*) "CALL BLAS_axpy(THISVECTOR=xVector,NEWVECTOR=yVector,AVECTOR=aVector,N=xVector%n,INCY=1) -vector_a [REAL] FAILED!"
      STOP 666
    ENDIF
    CALL yVector%set(1,3.0_SRK)
    CALL yVector%set(2,5.0_SRK)
    CALL yVector%set(3,1.0_SRK)
    CALL BLAS_axpy(THISVECTOR=xVector,NEWVECTOR=yVector,AVECTOR=aVector,N=xVector%n)
    CALL yVector%get(dummyvec)
    IF((.NOT.((dummyvec(1) .APPROXEQ. 33._SRK) .AND. &
              (dummyvec(2) .APPROXEQ. -3._SRK) .AND. &
              (dummyvec(3) .APPROXEQ. 15._SRK)))) THEN
      WRITE(*,*) "CALL BLAS_axpy(THISVECTOR=xVector,NEWVECTOR=yVector,AVECTOR=aVector,N=xVector%n) -vector_a [REAL] FAILED!"
      STOP 666
    ENDIF
    CALL yVector%set(1,3.0_SRK)
    CALL yVector%set(2,5.0_SRK)
    CALL yVector%set(3,1.0_SRK)
    CALL BLAS_axpy(THISVECTOR=xVector,NEWVECTOR=yVector,AVECTOR=aVector)
    CALL yVector%get(dummyvec)
    IF((.NOT.((dummyvec(1) .APPROXEQ. 33._SRK) .AND. &
              (dummyvec(2) .APPROXEQ. -3._SRK) .AND. &
              (dummyvec(3) .APPROXEQ. 15._SRK)))) THEN
      WRITE(*,*) "CALL BLAS_axpy(THISVECTOR=xVector,NEWVECTOR=yVector,AVECTOR=aVector) -vector_a [REAL] FAILED!"
      STOP 666
    ENDIF
    WRITE(*,*) '  Passed: CALL BLAS_axpy(...) -vector_a [REAL]'
    
    !Test BLAS_copy
    CALL xVector%set(1,1.0_SRK)
    CALL xVector%set(2,3.0_SRK)
    CALL xVector%set(3,7.0_SRK)
    CALL yVector%set(0.0_SRK)
    CALL BLAS_copy(THISVECTOR=xVector,NEWVECTOR=yVector,N=xVector%n,INCX=1,INCY=1)
    IF(ALLOCATED(dummyvec)) DEALLOCATE(dummyvec)
    ALLOCATE(dummyvec(yVector%n))
    CALL yVector%get(dummyvec)
    IF((.NOT.((dummyvec(1) .APPROXEQ. 1.0_SRK) .AND. &
              (dummyvec(2) .APPROXEQ. 3.0_SRK) .AND. &
              (dummyvec(3) .APPROXEQ. 7.0_SRK)))) THEN
      WRITE(*,*) "CALL BLAS_copy(THISVECTOR=xVector,NEWVECTOR=yVector,N=xVector%n,INCX=1,INCY=1) [REAL] FAILED!"
      STOP 666
    ENDIF
    CALL yVector%set(0.0_SRK)
    CALL BLAS_copy(THISVECTOR=xVector,NEWVECTOR=yVector,INCX=1,INCY=1)
    CALL yVector%get(dummyvec)
    IF((.NOT.((dummyvec(1) .APPROXEQ. 1.0_SRK) .AND. &
              (dummyvec(2) .APPROXEQ. 3.0_SRK) .AND. &
              (dummyvec(3) .APPROXEQ. 7.0_SRK)))) THEN
      WRITE(*,*) "CALL BLAS_copy(THISVECTOR=xVector,NEWVECTOR=yVector,INCX=1,INCY=1) [REAL] FAILED!"
      STOP 666
    ENDIF
    CALL yVector%set(0.0_SRK)
    CALL BLAS_copy(THISVECTOR=xVector,NEWVECTOR=yVector,N=xVector%n,INCX=1)
    CALL yVector%get(dummyvec)
    IF((.NOT.((dummyvec(1) .APPROXEQ. 1.0_SRK) .AND. &
              (dummyvec(2) .APPROXEQ. 3.0_SRK) .AND. &
              (dummyvec(3) .APPROXEQ. 7.0_SRK)))) THEN
      WRITE(*,*) "CALL BLAS_copy(THISVECTOR=xVector,NEWVECTOR=yVector,N=xVector%n,INCX=1) [REAL] FAILED!"
      STOP 666
    ENDIF
    CALL yVector%set(0.0_SRK)
    CALL BLAS_copy(THISVECTOR=xVector,NEWVECTOR=yVector,N=xVector%n,INCY=1)
    CALL yVector%get(dummyvec)
    IF((.NOT.((dummyvec(1) .APPROXEQ. 1.0_SRK) .AND. &
              (dummyvec(2) .APPROXEQ. 3.0_SRK) .AND. &
              (dummyvec(3) .APPROXEQ. 7.0_SRK)))) THEN
      WRITE(*,*) "CALL BLAS_copy(THISVECTOR=xVector,NEWVECTOR=yVector,N=xVector%n,INCY=1) [REAL] FAILED!"
      STOP 666
    ENDIF
    CALL yVector%set(0.0_SRK)
    CALL BLAS_copy(THISVECTOR=xVector,NEWVECTOR=yVector,INCX=1)
    CALL yVector%get(dummyvec)
    IF((.NOT.((dummyvec(1) .APPROXEQ. 1.0_SRK) .AND. &
              (dummyvec(2) .APPROXEQ. 3.0_SRK) .AND. &
              (dummyvec(3) .APPROXEQ. 7.0_SRK)))) THEN
      WRITE(*,*) "CALL BLAS_copy(THISVECTOR=xVector,NEWVECTOR=yVector,INCX=1) [REAL] FAILED!"
      STOP 666
    ENDIF
    CALL yVector%set(0.0_SRK)
    CALL BLAS_copy(THISVECTOR=xVector,NEWVECTOR=yVector,INCY=1)
    CALL yVector%get(dummyvec)
    IF((.NOT.((dummyvec(1) .APPROXEQ. 1.0_SRK) .AND. &
              (dummyvec(2) .APPROXEQ. 3.0_SRK) .AND. &
              (dummyvec(3) .APPROXEQ. 7.0_SRK)))) THEN
      WRITE(*,*) "CALL BLAS_copy(THISVECTOR=xVector,NEWVECTOR=yVector,INCY=1) [REAL] FAILED!"
      STOP 666
    ENDIF
    CALL yVector%set(0.0_SRK)
    CALL BLAS_copy(THISVECTOR=xVector,NEWVECTOR=yVector,N=xVector%n)
    CALL yVector%get(dummyvec)
    IF((.NOT.((dummyvec(1) .APPROXEQ. 1.0_SRK) .AND. &
              (dummyvec(2) .APPROXEQ. 3.0_SRK) .AND. &
              (dummyvec(3) .APPROXEQ. 7.0_SRK)))) THEN
      WRITE(*,*) "CALL BLAS_copy(THISVECTOR=xVector,NEWVECTOR=yVector,N=xVector%n) [REAL] FAILED!"
      STOP 666
    ENDIF
    CALL yVector%set(0.0_SRK)
    CALL BLAS_copy(THISVECTOR=xVector,NEWVECTOR=yVector)
    CALL yVector%get(dummyvec)
    IF((.NOT.((dummyvec(1) .APPROXEQ. 1.0_SRK) .AND. &
              (dummyvec(2) .APPROXEQ. 3.0_SRK) .AND. &
              (dummyvec(3) .APPROXEQ. 7.0_SRK)))) THEN
      WRITE(*,*) "CALL BLAS_copy(THISVECTOR=xVector,NEWVECTOR=yVector) [REAL] FAILED!"
      STOP 666
    ENDIF
    WRITE(*,*) '  Passed: CALL BLAS_copy(...) [REAL]'
    
    !Test BLAS_dot
    CALL xVector%set(1,1.0_SRK)
    CALL xVector%set(2,3.0_SRK)
    CALL xVector%set(3,7.0_SRK)
    CALL yVector%set(1,2.0_SRK)
    CALL yVector%set(2,4.0_SRK)
    CALL yVector%set(3,6.0_SRK)
    r=0.0_SRK
    r = BLAS_dot(THISVECTOR=xVector,THATVECTOR=yVector,N=xVector%n,INCX=1,INCY=1)
    IF(.NOT.(r .APPROXEQ. 56.0_SRK)) THEN
      WRITE(*,*) "CALL BLAS_dot(THISVECTOR=xVector,THATVECTOR=yVector,N=xVector%n,INCX=1,INCY=1) [REAL] FAILED!"
      STOP 666
    ENDIF
    r=0.0_SRK
    r = BLAS_dot(THISVECTOR=xVector,THATVECTOR=yVector,INCX=1,INCY=1)
    IF(.NOT.(r .APPROXEQ. 56.0_SRK)) THEN
      WRITE(*,*) "CALL BLAS_dot(THISVECTOR=xVector,THATVECTOR=yVector,INCX=1,INCY=1) [REAL] FAILED!"
      STOP 666
    ENDIF
    r=0.0_SRK
    r = BLAS_dot(THISVECTOR=xVector,THATVECTOR=yVector,N=xVector%n,INCX=1)
    IF(.NOT.(r .APPROXEQ. 56.0_SRK)) THEN
      WRITE(*,*) "CALL BLAS_dot(THISVECTOR=xVector,THATVECTOR=yVector,N=xVector%n,INCX=1) [REAL] FAILED!"
      STOP 666
    ENDIF
    r=0.0_SRK
    r = BLAS_dot(THISVECTOR=xVector,THATVECTOR=yVector,N=xVector%n,INCY=1)
    IF(.NOT.(r .APPROXEQ. 56.0_SRK)) THEN
      WRITE(*,*) "CALL BLAS_dot(THISVECTOR=xVector,THATVECTOR=yVector,N=xVector%n,INCY=1) [REAL] FAILED!"
      STOP 666
    ENDIF
    r=0.0_SRK
    r = BLAS_dot(THISVECTOR=xVector,THATVECTOR=yVector,N=xVector%n)
    IF(.NOT.(r .APPROXEQ. 56.0_SRK)) THEN
      WRITE(*,*) "CALL BLAS_dot(THISVECTOR=xVector,THATVECTOR=yVector,N=xVector%n) [REAL] FAILED!"
      STOP 666
    ENDIF
    r=0.0_SRK
    r = BLAS_dot(THISVECTOR=xVector,THATVECTOR=yVector,INCX=1)
    IF(.NOT.(r .APPROXEQ. 56.0_SRK)) THEN
      WRITE(*,*) "CALL BLAS_dot(THISVECTOR=xVector,THATVECTOR=yVector,INCX=1) [REAL] FAILED!"
      STOP 666
    ENDIF
    r=0.0_SRK
    r = BLAS_dot(THISVECTOR=xVector,THATVECTOR=yVector,INCY=1)
    IF(.NOT.(r .APPROXEQ. 56.0_SRK)) THEN
      WRITE(*,*) "CALL BLAS_dot(THISVECTOR=xVector,THATVECTOR=yVector,INCY=1) [REAL] FAILED!"
      STOP 666
    ENDIF
    r=0.0_SRK
    r = BLAS_dot(THISVECTOR=xVector,THATVECTOR=yVector)
    IF(.NOT.(r .APPROXEQ. 56.0_SRK)) THEN
      WRITE(*,*) "CALL BLAS_dot(THISVECTOR=xVector,THATVECTOR=yVector) [REAL] FAILED!"
      STOP 666
    ENDIF
    WRITE(*,*) '  Passed: CALL BLAS_dot(...) [REAL]'
    
    !Test BLAS_iamax
    ! first test with all positive values
    CALL xVector%set(1,12.0_SRK)
    CALL xVector%set(2, 3.0_SRK)
    CALL xVector%set(3,17.0_SRK)
    r_index = 0_SIK
    r_index = BLAS_iamax(THISVECTOR=xVector,N=xVector%n,INCX=1)
    IF(r_index /= 3_SIK) THEN
      WRITE(*,*) "CALL BLAS_iamax(THISVECTOR=xVector,N=xVector%n,INCX=1) [REAL] FAILED!"
      STOP 666
    ENDIF
    r_index = 0_SIK
    r_index = BLAS_iamax(THISVECTOR=xVector,INCX=1)
    IF(r_index /= 3_SIK) THEN
      WRITE(*,*) "CALL BLAS_iamax(THISVECTOR=xVector,INCX=1) [REAL] FAILED!"
      STOP 666
    ENDIF
    r_index = 0_SIK
    r_index = BLAS_iamax(THISVECTOR=xVector,N=xVector%n)
    IF(r_index /= 3_SIK) THEN
      WRITE(*,*) "CALL BLAS_iamax(THISVECTOR=xVector,N=xVector%n) [REAL] FAILED!"
      STOP 666
    ENDIF
    r_index = 0_SIK
    r_index = BLAS_iamax(THISVECTOR=xVector)
    IF(r_index /= 3_SIK) THEN
      WRITE(*,*) "CALL BLAS_iamax(THISVECTOR=xVector) [REAL] FAILED!"
      STOP 666
    ENDIF
    ! next test with a negative value
    CALL xVector%set(1, 12.0_SRK)
    CALL xVector%set(2,-30.0_SRK)
    CALL xVector%set(3, 17.0_SRK)
    r_index = 0_SIK
    r_index = BLAS_iamax(THISVECTOR=xVector,N=xVector%n,INCX=1)
    IF(r_index /= 2_SIK) THEN
      WRITE(*,*) "CALL BLAS_iamax(THISVECTOR=xVector,N=xVector%n,INCX=1) [REAL] FAILED!"
      STOP 666
    ENDIF
    r_index = 0_SIK
    r_index = BLAS_iamax(THISVECTOR=xVector,INCX=1)
    IF(r_index /= 2_SIK) THEN
      WRITE(*,*) "CALL BLAS_iamax(THISVECTOR=xVector,INCX=1) [REAL] FAILED!"
      STOP 666
    ENDIF
    r_index = 0_SIK
    r_index = BLAS_iamax(THISVECTOR=xVector,N=xVector%n)
    IF(r_index /= 2_SIK) THEN
      WRITE(*,*) "CALL BLAS_iamax(THISVECTOR=xVector,N=xVector%n) [REAL] FAILED!"
      STOP 666
    ENDIF
    r_index = 0_SIK
    r_index = BLAS_iamax(THISVECTOR=xVector)
    IF(r_index /= 2_SIK) THEN
      WRITE(*,*) "CALL BLAS_iamax(THISVECTOR=xVector) [REAL] FAILED!"
      STOP 666
    ENDIF
    WRITE(*,*) '  Passed: CALL BLAS_iamax(...) [REAL]'
    
    !Test BLAS_iamin
    ! first test with all positive values
    CALL xVector%set(1,12.0_SRK)
    CALL xVector%set(2, 3.0_SRK)
    CALL xVector%set(3,17.0_SRK)
    r_index = 0_SIK
    r_index = BLAS_iamin(THISVECTOR=xVector,N=xVector%n,INCX=1)
    IF(r_index /= 2_SIK) THEN
      WRITE(*,*) "CALL BLAS_iamin(THISVECTOR=xVector,N=xVector%n,INCX=1) [REAL] FAILED!"
      STOP 666
    ENDIF
    r_index = 0_SIK
    r_index = BLAS_iamin(THISVECTOR=xVector,INCX=1)
    IF(r_index /= 2_SIK) THEN
      WRITE(*,*) "CALL BLAS_iamin(THISVECTOR=xVector,INCX=1) [REAL] FAILED!"
      STOP 666
    ENDIF
    r_index = 0_SIK
    r_index = BLAS_iamin(THISVECTOR=xVector,N=xVector%n)
    IF(r_index /= 2_SIK) THEN
      WRITE(*,*) "CALL BLAS_iamin(THISVECTOR=xVector,N=xVector%n) [REAL] FAILED!"
      STOP 666
    ENDIF
    r_index = 0_SIK
    r_index = BLAS_iamin(THISVECTOR=xVector)
    IF(r_index /= 2_SIK) THEN
      WRITE(*,*) "CALL BLAS_iamin(THISVECTOR=xVector) [REAL] FAILED!"
      STOP 666
    ENDIF
    ! next test with a negative value
    CALL xVector%set(1, 12.0_SRK)
    CALL xVector%set(2,-30.0_SRK)
    CALL xVector%set(3, 17.0_SRK)
    r_index = 0_SIK
    r_index = BLAS_iamin(THISVECTOR=xVector,N=xVector%n,INCX=1)
    IF(r_index /= 1_SIK) THEN
      WRITE(*,*) "CALL BLAS_iamin(THISVECTOR=xVector,N=xVector%n,INCX=1) [REAL] FAILED!"
      STOP 666
    ENDIF
    r_index = 0_SIK
    r_index = BLAS_iamin(THISVECTOR=xVector,INCX=1)
    IF(r_index /= 1_SIK) THEN
      WRITE(*,*) "CALL BLAS_iamin(THISVECTOR=xVector,INCX=1) [REAL] FAILED!"
      STOP 666
    ENDIF
    r_index = 0_SIK
    r_index = BLAS_iamin(THISVECTOR=xVector,N=xVector%n)
    IF(r_index /= 1_SIK) THEN
      WRITE(*,*) "CALL BLAS_iamin(THISVECTOR=xVector,N=xVector%n) [REAL] FAILED!"
      STOP 666
    ENDIF
    r_index = 0_SIK
    r_index = BLAS_iamin(THISVECTOR=xVector)
    IF(r_index /= 1_SIK) THEN
      WRITE(*,*) "CALL BLAS_iamin(THISVECTOR=xVector) [REAL] FAILED!"
      STOP 666
    ENDIF
    WRITE(*,*) '  Passed: CALL BLAS_iamin(...) [REAL]'
    
    !Test BLAS_nrm2
    CALL xVector%set(1,0.0_SRK)
    CALL xVector%set(2,3.0_SRK)
    CALL xVector%set(3,4.0_SRK)
    r = 0.0_SRK
    r = BLAS_nrm2(THISVECTOR=xVector,N=xVector%n,INCX=1)
    IF(.NOT.(r .APPROXEQ. 5._SRK)) THEN
      WRITE(*,*) "CALL BLAS_nrm2(THISVECTOR=xVector,N=xVector%n,INCX=1) [REAL] FAILED!"
      STOP 666
    ENDIF
    r = 0.0_SRK
    r = BLAS_nrm2(THISVECTOR=xVector,INCX=1)
    IF(.NOT.(r .APPROXEQ. 5._SRK)) THEN
      WRITE(*,*) "CALL BLAS_nrm2(THISVECTOR=xVector,INCX=1) [REAL] FAILED!"
      STOP 666
    ENDIF
    r = 0.0_SRK
    r = BLAS_nrm2(THISVECTOR=xVector,N=xVector%n)
    IF(.NOT.(r .APPROXEQ. 5._SRK)) THEN
      WRITE(*,*) "CALL BLAS_nrm2(THISVECTOR=xVector,N=xVector%n) [REAL] FAILED!"
      STOP 666
    ENDIF
    r = 0.0_SRK
    r = BLAS_nrm2(THISVECTOR=xVector)
    IF(.NOT.(r .APPROXEQ. 5._SRK)) THEN
      WRITE(*,*) "CALL BLAS_nrm2(THISVECTOR=xVector) [REAL] FAILED!"
      STOP 666
    ENDIF
    WRITE(*,*) '  Passed: CALL BLAS_nrm2(...) [REAL]'
    
    !Test BLAS_scal [scalar a]
    a = 3
    CALL xVector%set(1,1.0_SRK)
    CALL xVector%set(2,3.0_SRK)
    CALL xVector%set(3,5.0_SRK)
    CALL BLAS_scal(THISVECTOR=xVector,A=a,N=xVector%n,INCX=1)
    IF(ALLOCATED(dummyvec)) DEALLOCATE(dummyvec)
    ALLOCATE(dummyvec(xVector%n))
    CALL xVector%get(dummyvec)
    IF((.NOT.((dummyvec(1) .APPROXEQ.  3._SRK) .AND. &
              (dummyvec(2) .APPROXEQ.  9._SRK) .AND. &
              (dummyvec(3) .APPROXEQ. 15._SRK)))) THEN
      WRITE(*,*) "CALL BLAS_scal(THISVECTOR=xVector,A=a,N=xVector%n,INCX=1) -scalar_a [REAL] FAILED!"
      STOP 666
    ENDIF
    CALL xVector%set(1,1.0_SRK)
    CALL xVector%set(2,3.0_SRK)
    CALL xVector%set(3,5.0_SRK)
    CALL BLAS_scal(THISVECTOR=xVector,A=a,INCX=1)
    CALL xVector%get(dummyvec)
    IF((.NOT.((dummyvec(1) .APPROXEQ.  3._SRK) .AND. &
              (dummyvec(2) .APPROXEQ.  9._SRK) .AND. &
              (dummyvec(3) .APPROXEQ. 15._SRK)))) THEN
      WRITE(*,*) "CALL BLAS_scal(THISVECTOR=xVector,A=a,INCX=1) -scalar_a [REAL] FAILED!"
      STOP 666
    ENDIF
    CALL xVector%set(1,1.0_SRK)
    CALL xVector%set(2,3.0_SRK)
    CALL xVector%set(3,5.0_SRK)
    CALL BLAS_scal(THISVECTOR=xVector,A=a,N=xVector%n)
    CALL xVector%get(dummyvec)
    IF((.NOT.((dummyvec(1) .APPROXEQ.  3._SRK) .AND. &
              (dummyvec(2) .APPROXEQ.  9._SRK) .AND. &
              (dummyvec(3) .APPROXEQ. 15._SRK)))) THEN
      WRITE(*,*) "CALL BLAS_scal(THISVECTOR=xVector,A=a,N=xVector%n) -scalar_a [REAL] FAILED!"
      STOP 666
    ENDIF
    CALL xVector%set(1,1.0_SRK)
    CALL xVector%set(2,3.0_SRK)
    CALL xVector%set(3,5.0_SRK)
    CALL BLAS_scal(THISVECTOR=xVector,A=a)
    CALL xVector%get(dummyvec)
    IF((.NOT.((dummyvec(1) .APPROXEQ.  3._SRK) .AND. &
              (dummyvec(2) .APPROXEQ.  9._SRK) .AND. &
              (dummyvec(3) .APPROXEQ. 15._SRK)))) THEN
      WRITE(*,*) "CALL BLAS_scal(THISVECTOR=xVector,A=a) -scalar_a [REAL] FAILED!"
      STOP 666
    ENDIF
    WRITE(*,*) '  Passed: CALL BLAS_scal(...) -scalar_a [REAL]'
    
    !Test BLAS_scal [vector a]
    CALL aVector%set(1,2.0_SRK)
    CALL aVector%set(2,4.0_SRK)
    CALL aVector%set(3,1.0_SRK)
    CALL xVector%set(1,1.0_SRK)
    CALL xVector%set(2,3.0_SRK)
    CALL xVector%set(3,5.0_SRK)
    CALL BLAS_scal(THISVECTOR=xVector,AVECTOR=aVector,N=xVector%n,INCX=1)
    IF(ALLOCATED(dummyvec)) DEALLOCATE(dummyvec)
    ALLOCATE(dummyvec(xVector%n))
    CALL xVector%get(dummyvec)
    IF((.NOT.((dummyvec(1) .APPROXEQ.  2._SRK) .AND. &
              (dummyvec(2) .APPROXEQ. 12._SRK) .AND. &
              (dummyvec(3) .APPROXEQ.  5._SRK)))) THEN
      WRITE(*,*) "CALL BLAS_scal(THISVECTOR=xVector,AVECTOR=aVector,N=xVector%n,INCX=1) -vector_a [REAL] FAILED!"
      STOP 666
    ENDIF
    CALL xVector%set(1,1.0_SRK)
    CALL xVector%set(2,3.0_SRK)
    CALL xVector%set(3,5.0_SRK)
    CALL BLAS_scal(THISVECTOR=xVector,AVECTOR=aVector,INCX=1)
    CALL xVector%get(dummyvec)
    IF((.NOT.((dummyvec(1) .APPROXEQ.  2._SRK) .AND. &
              (dummyvec(2) .APPROXEQ. 12._SRK) .AND. &
              (dummyvec(3) .APPROXEQ.  5._SRK)))) THEN
      WRITE(*,*) "CALL BLAS_scal(THISVECTOR=xVector,AVECTOR=aVector,INCX=1) -vector_a [REAL] FAILED!"
      STOP 666
    ENDIF
    CALL xVector%set(1,1.0_SRK)
    CALL xVector%set(2,3.0_SRK)
    CALL xVector%set(3,5.0_SRK)
    CALL BLAS_scal(THISVECTOR=xVector,AVECTOR=aVector,N=xVector%n)
    CALL xVector%get(dummyvec)
    IF((.NOT.((dummyvec(1) .APPROXEQ.  2._SRK) .AND. &
              (dummyvec(2) .APPROXEQ. 12._SRK) .AND. &
              (dummyvec(3) .APPROXEQ.  5._SRK)))) THEN
      WRITE(*,*) "CALL BLAS_scal(THISVECTOR=xVector,AVECTOR=aVector,N=xVector%n) -vector_a [REAL] FAILED!"
      STOP 666
    ENDIF
    CALL xVector%set(1,1.0_SRK)
    CALL xVector%set(2,3.0_SRK)
    CALL xVector%set(3,5.0_SRK)
    CALL BLAS_scal(THISVECTOR=xVector,AVECTOR=aVector)
    CALL xVector%get(dummyvec)
    IF((.NOT.((dummyvec(1) .APPROXEQ.  2._SRK) .AND. &
              (dummyvec(2) .APPROXEQ. 12._SRK) .AND. &
              (dummyvec(3) .APPROXEQ.  5._SRK)))) THEN
      WRITE(*,*) "CALL BLAS_scal(THISVECTOR=xVector,AVECTOR=aVector) -vector_a [REAL] FAILED!"
      STOP 666
    ENDIF
    WRITE(*,*) '  Passed: CALL BLAS_scal(...) -vector_a [REAL]'
    
    !Test BLAS_swap
    CALL xVector%set(1,1.0_SRK)
    CALL xVector%set(2,3.0_SRK)
    CALL xVector%set(3,5.0_SRK)
    CALL yVector%set(1,2.0_SRK)
    CALL yVector%set(2,4.0_SRK)
    CALL yVector%set(3,6.0_SRK)
    CALL BLAS_swap(THISVECTOR=xVector,THATVECTOR=yVector,N=xVector%n,INCX=1,INCY=1)
    IF(ALLOCATED(dummyvec)) DEALLOCATE(dummyvec)
    IF(ALLOCATED(dummyvec2)) DEALLOCATE(dummyvec2)
    ALLOCATE(dummyvec(xVector%n))
    ALLOCATE(dummyvec2(yVector%n))
    CALL xVector%get(dummyvec)
    CALL yVector%get(dummyvec2)
    IF((.NOT.((dummyvec(1) .APPROXEQ. 2._SRK) .AND. &
              (dummyvec(2) .APPROXEQ. 4._SRK) .AND. &
              (dummyvec(3) .APPROXEQ. 6._SRK) .AND. &
              (dummyvec2(1) .APPROXEQ. 1._SRK) .AND. &
              (dummyvec2(2) .APPROXEQ. 3._SRK) .AND. &
              (dummyvec2(3) .APPROXEQ. 5._SRK)))) THEN
      WRITE(*,*) "CALL BLAS_swap(THISVECTOR=xVector,THATVECTOR=yVector,N=xVector%n,INCX=1,INCY=1) [REAL] FAILED!"
      STOP 666
    ENDIF
    CALL xVector%set(1,1.0_SRK)
    CALL xVector%set(2,3.0_SRK)
    CALL xVector%set(3,5.0_SRK)
    CALL yVector%set(1,2.0_SRK)
    CALL yVector%set(2,4.0_SRK)
    CALL yVector%set(3,6.0_SRK)
    CALL BLAS_swap(THISVECTOR=xVector,THATVECTOR=yVector,INCX=1,INCY=1)
    CALL xVector%get(dummyvec)
    CALL yVector%get(dummyvec2)
    IF((.NOT.((dummyvec(1) .APPROXEQ. 2._SRK) .AND. &
              (dummyvec(2) .APPROXEQ. 4._SRK) .AND. &
              (dummyvec(3) .APPROXEQ. 6._SRK) .AND. &
              (dummyvec2(1) .APPROXEQ. 1._SRK) .AND. &
              (dummyvec2(2) .APPROXEQ. 3._SRK) .AND. &
              (dummyvec2(3) .APPROXEQ. 5._SRK)))) THEN
      WRITE(*,*) "CALL BLAS_swap(THISVECTOR=xVector,THATVECTOR=yVector,INCX=1,INCY=1) [REAL] FAILED!"
      STOP 666
    ENDIF
    CALL xVector%set(1,1.0_SRK)
    CALL xVector%set(2,3.0_SRK)
    CALL xVector%set(3,5.0_SRK)
    CALL yVector%set(1,2.0_SRK)
    CALL yVector%set(2,4.0_SRK)
    CALL yVector%set(3,6.0_SRK)
    CALL BLAS_swap(THISVECTOR=xVector,THATVECTOR=yVector,N=xVector%n,INCX=1)
    CALL xVector%get(dummyvec)
    CALL yVector%get(dummyvec2)
    IF((.NOT.((dummyvec(1) .APPROXEQ. 2._SRK) .AND. &
              (dummyvec(2) .APPROXEQ. 4._SRK) .AND. &
              (dummyvec(3) .APPROXEQ. 6._SRK) .AND. &
              (dummyvec2(1) .APPROXEQ. 1._SRK) .AND. &
              (dummyvec2(2) .APPROXEQ. 3._SRK) .AND. &
              (dummyvec2(3) .APPROXEQ. 5._SRK)))) THEN
      WRITE(*,*) "CALL BLAS_swap(THISVECTOR=xVector,THATVECTOR=yVector,N=xVector%n,INCX=1) [REAL] FAILED!"
      STOP 666
    ENDIF
    CALL xVector%set(1,1.0_SRK)
    CALL xVector%set(2,3.0_SRK)
    CALL xVector%set(3,5.0_SRK)
    CALL yVector%set(1,2.0_SRK)
    CALL yVector%set(2,4.0_SRK)
    CALL yVector%set(3,6.0_SRK)
    CALL BLAS_swap(THISVECTOR=xVector,THATVECTOR=yVector,N=xVector%n,INCY=1)
    CALL xVector%get(dummyvec)
    CALL yVector%get(dummyvec2)
    IF((.NOT.((dummyvec(1) .APPROXEQ. 2._SRK) .AND. &
              (dummyvec(2) .APPROXEQ. 4._SRK) .AND. &
              (dummyvec(3) .APPROXEQ. 6._SRK) .AND. &
              (dummyvec2(1) .APPROXEQ. 1._SRK) .AND. &
              (dummyvec2(2) .APPROXEQ. 3._SRK) .AND. &
              (dummyvec2(3) .APPROXEQ. 5._SRK)))) THEN
      WRITE(*,*) "CALL BLAS_swap(THISVECTOR=xVector,THATVECTOR=yVector,N=xVector%n,INCY=1) [REAL] FAILED!"
      STOP 666
    ENDIF
    CALL xVector%set(1,1.0_SRK)
    CALL xVector%set(2,3.0_SRK)
    CALL xVector%set(3,5.0_SRK)
    CALL yVector%set(1,2.0_SRK)
    CALL yVector%set(2,4.0_SRK)
    CALL yVector%set(3,6.0_SRK)
    CALL BLAS_swap(THISVECTOR=xVector,THATVECTOR=yVector,INCX=1)
    CALL xVector%get(dummyvec)
    CALL yVector%get(dummyvec2)
    IF((.NOT.((dummyvec(1) .APPROXEQ. 2._SRK) .AND. &
              (dummyvec(2) .APPROXEQ. 4._SRK) .AND. &
              (dummyvec(3) .APPROXEQ. 6._SRK) .AND. &
              (dummyvec2(1) .APPROXEQ. 1._SRK) .AND. &
              (dummyvec2(2) .APPROXEQ. 3._SRK) .AND. &
              (dummyvec2(3) .APPROXEQ. 5._SRK)))) THEN
      WRITE(*,*) "CALL BLAS_swap(THISVECTOR=xVector,THATVECTOR=yVector,INCX=1) [REAL] FAILED!"
      STOP 666
    ENDIF
    CALL xVector%set(1,1.0_SRK)
    CALL xVector%set(2,3.0_SRK)
    CALL xVector%set(3,5.0_SRK)
    CALL yVector%set(1,2.0_SRK)
    CALL yVector%set(2,4.0_SRK)
    CALL yVector%set(3,6.0_SRK)
    CALL BLAS_swap(THISVECTOR=xVector,THATVECTOR=yVector,INCY=1)
    CALL xVector%get(dummyvec)
    CALL yVector%get(dummyvec2)
    IF((.NOT.((dummyvec(1) .APPROXEQ. 2._SRK) .AND. &
              (dummyvec(2) .APPROXEQ. 4._SRK) .AND. &
              (dummyvec(3) .APPROXEQ. 6._SRK) .AND. &
              (dummyvec2(1) .APPROXEQ. 1._SRK) .AND. &
              (dummyvec2(2) .APPROXEQ. 3._SRK) .AND. &
              (dummyvec2(3) .APPROXEQ. 5._SRK)))) THEN
      WRITE(*,*) "CALL BLAS_swap(THISVECTOR=xVector,THATVECTOR=yVector,INCY=1) [REAL] FAILED!"
      STOP 666
    ENDIF
    CALL xVector%set(1,1.0_SRK)
    CALL xVector%set(2,3.0_SRK)
    CALL xVector%set(3,5.0_SRK)
    CALL yVector%set(1,2.0_SRK)
    CALL yVector%set(2,4.0_SRK)
    CALL yVector%set(3,6.0_SRK)
    CALL BLAS_swap(THISVECTOR=xVector,THATVECTOR=yVector,N=xVector%n)
    CALL xVector%get(dummyvec)
    CALL yVector%get(dummyvec2)
    IF((.NOT.((dummyvec(1) .APPROXEQ. 2._SRK) .AND. &
              (dummyvec(2) .APPROXEQ. 4._SRK) .AND. &
              (dummyvec(3) .APPROXEQ. 6._SRK) .AND. &
              (dummyvec2(1) .APPROXEQ. 1._SRK) .AND. &
              (dummyvec2(2) .APPROXEQ. 3._SRK) .AND. &
              (dummyvec2(3) .APPROXEQ. 5._SRK)))) THEN
      WRITE(*,*) "CALL BLAS_swap(THISVECTOR=xVector,THATVECTOR=yVector,N=xVector%n) [REAL] FAILED!"
      STOP 666
    ENDIF
    CALL xVector%set(1,1.0_SRK)
    CALL xVector%set(2,3.0_SRK)
    CALL xVector%set(3,5.0_SRK)
    CALL yVector%set(1,2.0_SRK)
    CALL yVector%set(2,4.0_SRK)
    CALL yVector%set(3,6.0_SRK)
    CALL BLAS_swap(THISVECTOR=xVector,THATVECTOR=yVector)
    CALL xVector%get(dummyvec)
    CALL yVector%get(dummyvec2)
    IF((.NOT.((dummyvec(1) .APPROXEQ. 2._SRK) .AND. &
              (dummyvec(2) .APPROXEQ. 4._SRK) .AND. &
              (dummyvec(3) .APPROXEQ. 6._SRK) .AND. &
              (dummyvec2(1) .APPROXEQ. 1._SRK) .AND. &
              (dummyvec2(2) .APPROXEQ. 3._SRK) .AND. &
              (dummyvec2(3) .APPROXEQ. 5._SRK)))) THEN
      WRITE(*,*) "CALL BLAS_swap(THISVECTOR=xVector,THATVECTOR=yVector) [REAL] FAILED!"
      STOP 666
    ENDIF
    CALL xVector%clear()
    CALL yVector%clear()
    CALL aVector%clear()
    DEALLOCATE(xVector)
    DEALLOCATE(yVector)
    DEALLOCATE(aVector)
    WRITE(*,*) '  Passed: CALL BLAS_swap(...) [REAL]'
    
#ifdef HAVE_PETSC
    ! test with PETSc vectors
    ALLOCATE(PETScVectorType :: xVector)
    ALLOCATE(PETScVectorType :: yVector)
    ALLOCATE(PETScVectorType :: aVector)
    CALL pList%clear()
    CALL pList%add('VectorType->n',3)
    CALL pList%add('VectorType->MPI_Comm_ID',PE_COMM_SELF)
    CALL xVector%init(pList)
    CALL yVector%init(pList)
    CALL aVector%init(pList)
    
    !Test BLAS_asum (absolute value summation)
    CALL xVector%set(1, 5.0_SRK)
    CALL xVector%set(2,-2.0_SRK)
    CALL xVector%set(3, 7.0_SRK)
    r=0.0_SRK
    r = BLAS_asum(THISVECTOR=xVector,N=xVector%n,INCX=1)
    IF(.NOT.(r .APPROXEQ. 14.0_SRK)) THEN
      WRITE(*,*) "CALL BLAS_asum(THISVECTOR=xVector,N=xVector%n,INCX=1) [PETSC] FAILED!"
      STOP 666
    ENDIF
    r=0.0_SRK
    r = BLAS_asum(THISVECTOR=xVector,INCX=1)
    IF(.NOT.(r .APPROXEQ. 14.0_SRK)) THEN
      WRITE(*,*) "CALL BLAS_asum(THISVECTOR=xVector,INCX=1) [PETSC] FAILED!"
      STOP 666
    ENDIF
    r=0.0_SRK
    r = BLAS_asum(THISVECTOR=xVector,N=xVector%n)
    IF(.NOT.(r .APPROXEQ. 14.0_SRK)) THEN
      WRITE(*,*) "CALL BLAS_asum(THISVECTOR=xVector,N=xVector%n) [PETSC] FAILED!"
      STOP 666
    ENDIF
    r=0.0_SRK
    r = BLAS_asum(THISVECTOR=xVector)
    IF(.NOT.(r .APPROXEQ. 14.0_SRK)) THEN
      WRITE(*,*) "CALL BLAS_asum(THISVECTOR=xVector) [PETSC] FAILED!"
      STOP 666
    ENDIF
    WRITE(*,*) '  Passed: CALL BLAS_asum(...) [PETSC]'
    
    !Test BLAS_axpy (y=y+ax) [scalar a]
    a = 5
    CALL xVector%set(1, 5.0_SRK)
    CALL xVector%set(2,-2.0_SRK)
    CALL xVector%set(3, 7.0_SRK)
    CALL yVector%set(1, 3.0_SRK)
    CALL yVector%set(2, 5.0_SRK)
    CALL yVector%set(3, 1.0_SRK)
    CALL BLAS_axpy(THISVECTOR=xVector,NEWVECTOR=yVector,A=a,N=xVector%n,INCX=1,INCY=1)
    IF(ALLOCATED(dummyvec)) DEALLOCATE(dummyvec)
    ALLOCATE(dummyvec(yVector%n))
    CALL yVector%get(dummyvec)
    IF((.NOT.((dummyvec(1) .APPROXEQ. 28._SRK) .AND. &
              (dummyvec(2) .APPROXEQ. -5._SRK) .AND. &
              (dummyvec(3) .APPROXEQ. 36._SRK)))) THEN
      WRITE(*,*) "CALL BLAS_axpy(THISVECTOR=xVector,NEWVECTOR=yVector,A=a,N=xVector%n,INCX=1,INCY=1) -scalar_a [PETSC] FAILED!"
      STOP 666
    ENDIF
    CALL yVector%set(1,3.0_SRK)
    CALL yVector%set(2,5.0_SRK)
    CALL yVector%set(3,1.0_SRK)
    CALL BLAS_axpy(THISVECTOR=xVector,NEWVECTOR=yVector,A=a,N=xVector%n,INCX=1)
    CALL yVector%get(dummyvec)
    IF((.NOT.((dummyvec(1) .APPROXEQ. 28._SRK) .AND. &
              (dummyvec(2) .APPROXEQ. -5._SRK) .AND. &
              (dummyvec(3) .APPROXEQ. 36._SRK)))) THEN
      WRITE(*,*) "CALL BLAS_axpy(THISVECTOR=xVector,NEWVECTOR=yVector,A=a,N=xVector%n,INCX=1) -scalar_a [PETSC] FAILED!"
      STOP 666
    ENDIF
    CALL yVector%set(1,3.0_SRK)
    CALL yVector%set(2,5.0_SRK)
    CALL yVector%set(3,1.0_SRK)
    CALL BLAS_axpy(THISVECTOR=xVector,NEWVECTOR=yVector,A=a,N=xVector%n,INCY=1)
    CALL yVector%get(dummyvec)
    IF((.NOT.((dummyvec(1) .APPROXEQ. 28._SRK) .AND. &
              (dummyvec(2) .APPROXEQ. -5._SRK) .AND. &
              (dummyvec(3) .APPROXEQ. 36._SRK)))) THEN
      WRITE(*,*) "CALL BLAS_axpy(THISVECTOR=xVector,NEWVECTOR=yVector,A=a,N=xVector%n,INCY=1) -scalar_a [PETSC] FAILED!"
      STOP 666
    ENDIF
    CALL yVector%set(1,3.0_SRK)
    CALL yVector%set(2,5.0_SRK)
    CALL yVector%set(3,1.0_SRK)
    CALL BLAS_axpy(THISVECTOR=xVector,NEWVECTOR=yVector,A=a,N=xVector%n)
    CALL yVector%get(dummyvec)
    IF((.NOT.((dummyvec(1) .APPROXEQ. 28._SRK) .AND. &
              (dummyvec(2) .APPROXEQ. -5._SRK) .AND. &
              (dummyvec(3) .APPROXEQ. 36._SRK)))) THEN
      WRITE(*,*) "CALL BLAS_axpy(THISVECTOR=xVector,NEWVECTOR=yVector,A=a,N=xVector%n) -scalar_a [PETSC] FAILED!"
      STOP 666
    ENDIF
    CALL yVector%set(1,3.0_SRK)
    CALL yVector%set(2,5.0_SRK)
    CALL yVector%set(3,1.0_SRK)
    CALL BLAS_axpy(THISVECTOR=xVector,NEWVECTOR=yVector,A=a)
    CALL yVector%get(dummyvec)
    IF((.NOT.((dummyvec(1) .APPROXEQ. 28._SRK) .AND. &
              (dummyvec(2) .APPROXEQ. -5._SRK) .AND. &
              (dummyvec(3) .APPROXEQ. 36._SRK)))) THEN
      WRITE(*,*) "CALL BLAS_axpy(THISVECTOR=xVector,NEWVECTOR=yVector,A=a) -scalar_a [PETSC] FAILED!"
      STOP 666
    ENDIF
    WRITE(*,*) '  Passed: CALL BLAS_axpy(...) -scalar_a [PETSC]'
    
    !Test BLAS_axpy (y=y+ax) [vector a]
    a = 3
    CALL xVector%set(1,5.0_SRK)
    CALL xVector%set(2,-2.0_SRK)
    CALL xVector%set(3,7.0_SRK)
    CALL yVector%set(1,3.0_SRK)
    CALL yVector%set(2,5.0_SRK)
    CALL yVector%set(3,1.0_SRK)
    CALL aVector%set(1,6.0_SRK)
    CALL aVector%set(2,4.0_SRK)
    CALL aVector%set(3,2.0_SRK)
    CALL BLAS_axpy(THISVECTOR=xVector,NEWVECTOR=yVector,AVECTOR=aVector,N=xVector%n,INCX=1,INCY=1)
    IF(ALLOCATED(dummyvec)) DEALLOCATE(dummyvec)
    ALLOCATE(dummyvec(yVector%n))
    CALL yVector%get(dummyvec)
    IF((.NOT.((dummyvec(1) .APPROXEQ. 33._SRK) .AND. &
              (dummyvec(2) .APPROXEQ. -3._SRK) .AND. &
              (dummyvec(3) .APPROXEQ. 15._SRK)))) THEN
      WRITE(*,*) "CALL BLAS_axpy(THISVECTOR=xVector,NEWVECTOR=yVector,AVECTOR=aVector,N=xVector%n,INCX=1,INCY=1)", &
                      "-vector_a [PETSC] FAILED!"
      STOP 666
    ENDIF
    CALL yVector%set(1,3.0_SRK)
    CALL yVector%set(2,5.0_SRK)
    CALL yVector%set(3,1.0_SRK)
    CALL BLAS_axpy(THISVECTOR=xVector,NEWVECTOR=yVector,AVECTOR=aVector,N=xVector%n,INCX=1)
    CALL yVector%get(dummyvec)
    IF((.NOT.((dummyvec(1) .APPROXEQ. 33._SRK) .AND. &
              (dummyvec(2) .APPROXEQ. -3._SRK) .AND. &
              (dummyvec(3) .APPROXEQ. 15._SRK)))) THEN
      WRITE(*,*) "CALL BLAS_axpy(THISVECTOR=xVector,NEWVECTOR=yVector,AVECTOR=aVector,N=xVector%n,INCX=1) -vector_a [PETSC] FAILED!"
      STOP 666
    ENDIF
    CALL yVector%set(1,3.0_SRK)
    CALL yVector%set(2,5.0_SRK)
    CALL yVector%set(3,1.0_SRK)
    CALL BLAS_axpy(THISVECTOR=xVector,NEWVECTOR=yVector,AVECTOR=aVector,N=xVector%n,INCY=1)
    CALL yVector%get(dummyvec)
    IF((.NOT.((dummyvec(1) .APPROXEQ. 33._SRK) .AND. &
              (dummyvec(2) .APPROXEQ. -3._SRK) .AND. &
              (dummyvec(3) .APPROXEQ. 15._SRK)))) THEN
      WRITE(*,*) "CALL BLAS_axpy(THISVECTOR=xVector,NEWVECTOR=yVector,AVECTOR=aVector,N=xVector%n,INCY=1) -vector_a [PETSC] FAILED!"
      STOP 666
    ENDIF
    CALL yVector%set(1,3.0_SRK)
    CALL yVector%set(2,5.0_SRK)
    CALL yVector%set(3,1.0_SRK)
    CALL BLAS_axpy(THISVECTOR=xVector,NEWVECTOR=yVector,AVECTOR=aVector,N=xVector%n)
    CALL yVector%get(dummyvec)
    IF((.NOT.((dummyvec(1) .APPROXEQ. 33._SRK) .AND. &
              (dummyvec(2) .APPROXEQ. -3._SRK) .AND. &
              (dummyvec(3) .APPROXEQ. 15._SRK)))) THEN
      WRITE(*,*) "CALL BLAS_axpy(THISVECTOR=xVector,NEWVECTOR=yVector,AVECTOR=aVector,N=xVector%n) -vector_a [PETSC] FAILED!"
      STOP 666
    ENDIF
    CALL yVector%set(1,3.0_SRK)
    CALL yVector%set(2,5.0_SRK)
    CALL yVector%set(3,1.0_SRK)
    CALL BLAS_axpy(THISVECTOR=xVector,NEWVECTOR=yVector,AVECTOR=aVector)
    CALL yVector%get(dummyvec)
    IF((.NOT.((dummyvec(1) .APPROXEQ. 33._SRK) .AND. &
              (dummyvec(2) .APPROXEQ. -3._SRK) .AND. &
              (dummyvec(3) .APPROXEQ. 15._SRK)))) THEN
      WRITE(*,*) "CALL BLAS_axpy(THISVECTOR=xVector,NEWVECTOR=yVector,AVECTOR=aVector) -vector_a [PETSC] FAILED!"
      STOP 666
    ENDIF
    WRITE(*,*) '  Passed: CALL BLAS_axpy(...) -vector_a [PETSC]'
    
    !Test BLAS_copy
    CALL xVector%set(1,1.0_SRK)
    CALL xVector%set(2,3.0_SRK)
    CALL xVector%set(3,7.0_SRK)
    CALL yVector%set(0.0_SRK,iverr)
    CALL BLAS_copy(THISVECTOR=xVector,NEWVECTOR=yVector,N=xVector%n,INCX=1,INCY=1)
    IF(ALLOCATED(dummyvec)) DEALLOCATE(dummyvec)
    ALLOCATE(dummyvec(yVector%n))
    CALL yVector%get(dummyvec)
    IF((.NOT.((dummyvec(1) .APPROXEQ. 1.0_SRK) .AND. &
              (dummyvec(2) .APPROXEQ. 3.0_SRK) .AND. &
              (dummyvec(3) .APPROXEQ. 7.0_SRK)))) THEN
      WRITE(*,*) "CALL BLAS_copy(THISVECTOR=xVector,NEWVECTOR=yVector,N=xVector%n,INCX=1,INCY=1) [PETSC] FAILED!"
      STOP 666
    ENDIF
    CALL yVector%set(0.0_SRK)
    CALL BLAS_copy(THISVECTOR=xVector,NEWVECTOR=yVector,INCX=1,INCY=1)
    CALL yVector%get(dummyvec)
    IF((.NOT.((dummyvec(1) .APPROXEQ. 1.0_SRK) .AND. &
              (dummyvec(2) .APPROXEQ. 3.0_SRK) .AND. &
              (dummyvec(3) .APPROXEQ. 7.0_SRK)))) THEN
      WRITE(*,*) "CALL BLAS_copy(THISVECTOR=xVector,NEWVECTOR=yVector,INCX=1,INCY=1) [PETSC] FAILED!"
      STOP 666
    ENDIF
    CALL yVector%set(0.0_SRK)
    CALL BLAS_copy(THISVECTOR=xVector,NEWVECTOR=yVector,N=xVector%n,INCX=1)
    CALL yVector%get(dummyvec)
    IF((.NOT.((dummyvec(1) .APPROXEQ. 1.0_SRK) .AND. &
              (dummyvec(2) .APPROXEQ. 3.0_SRK) .AND. &
              (dummyvec(3) .APPROXEQ. 7.0_SRK)))) THEN
      WRITE(*,*) "CALL BLAS_copy(THISVECTOR=xVector,NEWVECTOR=yVector,N=xVector%n,INCX=1) [PETSC] FAILED!"
      STOP 666
    ENDIF
    CALL yVector%set(0.0_SRK)
    CALL BLAS_copy(THISVECTOR=xVector,NEWVECTOR=yVector,N=xVector%n,INCY=1)
    CALL yVector%get(dummyvec)
    IF((.NOT.((dummyvec(1) .APPROXEQ. 1.0_SRK) .AND. &
              (dummyvec(2) .APPROXEQ. 3.0_SRK) .AND. &
              (dummyvec(3) .APPROXEQ. 7.0_SRK)))) THEN
      WRITE(*,*) "CALL BLAS_copy(THISVECTOR=xVector,NEWVECTOR=yVector,N=xVector%n,INCY=1) [PETSC] FAILED!"
      STOP 666
    ENDIF
    CALL yVector%set(0.0_SRK)
    CALL BLAS_copy(THISVECTOR=xVector,NEWVECTOR=yVector,INCX=1)
    CALL yVector%get(dummyvec)
    IF((.NOT.((dummyvec(1) .APPROXEQ. 1.0_SRK) .AND. &
              (dummyvec(2) .APPROXEQ. 3.0_SRK) .AND. &
              (dummyvec(3) .APPROXEQ. 7.0_SRK)))) THEN
      WRITE(*,*) "CALL BLAS_copy(THISVECTOR=xVector,NEWVECTOR=yVector,INCX=1) [PETSC] FAILED!"
      STOP 666
    ENDIF
    CALL yVector%set(0.0_SRK)
    CALL BLAS_copy(THISVECTOR=xVector,NEWVECTOR=yVector,INCY=1)
    CALL yVector%get(dummyvec)
    IF((.NOT.((dummyvec(1) .APPROXEQ. 1.0_SRK) .AND. &
              (dummyvec(2) .APPROXEQ. 3.0_SRK) .AND. &
              (dummyvec(3) .APPROXEQ. 7.0_SRK)))) THEN
      WRITE(*,*) "CALL BLAS_copy(THISVECTOR=xVector,NEWVECTOR=yVector,INCY=1) [PETSC] FAILED!" 
      STOP 666
    ENDIF
    CALL yVector%set(0.0_SRK)
    CALL BLAS_copy(THISVECTOR=xVector,NEWVECTOR=yVector,N=xVector%n)
    CALL yVector%get(dummyvec)
    IF((.NOT.((dummyvec(1) .APPROXEQ. 1.0_SRK) .AND. &
              (dummyvec(2) .APPROXEQ. 3.0_SRK) .AND. &
              (dummyvec(3) .APPROXEQ. 7.0_SRK)))) THEN
      WRITE(*,*) "CALL BLAS_copy(THISVECTOR=xVector,NEWVECTOR=yVector,N=xVector%n) [PETSC] FAILED!"
      STOP 666
    ENDIF
    CALL yVector%set(0.0_SRK)
    CALL BLAS_copy(THISVECTOR=xVector,NEWVECTOR=yVector)
    CALL yVector%get(dummyvec)
    IF((.NOT.((dummyvec(1) .APPROXEQ. 1.0_SRK) .AND. &
              (dummyvec(2) .APPROXEQ. 3.0_SRK) .AND. &
              (dummyvec(3) .APPROXEQ. 7.0_SRK)))) THEN
      WRITE(*,*) "CALL BLAS_copy(THISVECTOR=xVector,NEWVECTOR=yVector) [PETSC] FAILED!"
      STOP 666
    ENDIF
    WRITE(*,*) '  Passed: CALL BLAS_copy(...) [PETSC]'
    
    !Test BLAS_dot
    CALL xVector%set(1,1.0_SRK)
    CALL xVector%set(2,3.0_SRK)
    CALL xVector%set(3,7.0_SRK)
    CALL yVector%set(1,2.0_SRK)
    CALL yVector%set(2,4.0_SRK)
    CALL yVector%set(3,6.0_SRK)
    r=0.0_SRK
    r = BLAS_dot(THISVECTOR=xVector,THATVECTOR=yVector,N=xVector%n,INCX=1,INCY=1)
    IF(.NOT.(r .APPROXEQ. 56.0_SRK)) THEN
      WRITE(*,*) "CALL BLAS_dot(THISVECTOR=xVector,THATVECTOR=yVector,N=xVector%n,INCX=1,INCY=1) [PETSC] FAILED!"
      STOP 666
    ENDIF
    r=0.0_SRK
    r = BLAS_dot(THISVECTOR=xVector,THATVECTOR=yVector,INCX=1,INCY=1)
    IF(.NOT.(r .APPROXEQ. 56.0_SRK)) THEN
      WRITE(*,*) "CALL BLAS_dot(THISVECTOR=xVector,THATVECTOR=yVector,INCX=1,INCY=1) [PETSC] FAILED!"
      STOP 666
    ENDIF
    r=0.0_SRK
    r = BLAS_dot(THISVECTOR=xVector,THATVECTOR=yVector,N=xVector%n,INCX=1)
    IF(.NOT.(r .APPROXEQ. 56.0_SRK)) THEN
      WRITE(*,*) "CALL BLAS_dot(THISVECTOR=xVector,THATVECTOR=yVector,N=xVector%n,INCX=1) [PETSC] FAILED!"
      STOP 666
    ENDIF
    r=0.0_SRK
    r = BLAS_dot(THISVECTOR=xVector,THATVECTOR=yVector,N=xVector%n,INCY=1)
    IF(.NOT.(r .APPROXEQ. 56.0_SRK)) THEN
      WRITE(*,*) "CALL BLAS_dot(THISVECTOR=xVector,THATVECTOR=yVector,N=xVector%n,INCY=1) [PETSC] FAILED!"
      STOP 666
    ENDIF
    r=0.0_SRK
    r = BLAS_dot(THISVECTOR=xVector,THATVECTOR=yVector,N=xVector%n)
    IF(.NOT.(r .APPROXEQ. 56.0_SRK)) THEN
      WRITE(*,*) "CALL BLAS_dot(THISVECTOR=xVector,THATVECTOR=yVector,N=xVector%n) [PETSC] FAILED!"
      STOP 666
    ENDIF
    r=0.0_SRK
    r = BLAS_dot(THISVECTOR=xVector,THATVECTOR=yVector,INCX=1)
    IF(.NOT.(r .APPROXEQ. 56.0_SRK)) THEN
      WRITE(*,*) "CALL BLAS_dot(THISVECTOR=xVector,THATVECTOR=yVector,INCX=1) [PETSC] FAILED!"
      STOP 666
    ENDIF
    r=0.0_SRK
    r = BLAS_dot(THISVECTOR=xVector,THATVECTOR=yVector,INCY=1)
    IF(.NOT.(r .APPROXEQ. 56.0_SRK)) THEN
      WRITE(*,*) "CALL BLAS_dot(THISVECTOR=xVector,THATVECTOR=yVector,INCY=1) [PETSC] FAILED!"
      STOP 666
    ENDIF
    r=0.0_SRK
    r = BLAS_dot(THISVECTOR=xVector,THATVECTOR=yVector)
    IF(.NOT.(r .APPROXEQ. 56.0_SRK)) THEN
      WRITE(*,*) "CALL BLAS_dot(THISVECTOR=xVector,THATVECTOR=yVector) [PETSC] FAILED!"
      STOP 666
    ENDIF
    WRITE(*,*) '  Passed: CALL BLAS_dot(...) [PETSC]'
    
    !Test BLAS_iamax
    ! first test with all positive values
    CALL xVector%set(1,12.0_SRK)
    CALL xVector%set(2, 3.0_SRK)
    CALL xVector%set(3,17.0_SRK)
    r_index = 0_SIK
    r_index = BLAS_iamax(THISVECTOR=xVector,N=xVector%n,INCX=1)
    IF(r_index /= 3_SIK) THEN
      WRITE(*,*) "CALL BLAS_iamax(THISVECTOR=xVector,N=xVector%n,INCX=1) [PETSC] FAILED!"
      STOP 666
    ENDIF
    r_index = 0_SIK
    r_index = BLAS_iamax(THISVECTOR=xVector,INCX=1)
    IF(r_index /= 3_SIK) THEN
      WRITE(*,*) "CALL BLAS_iamax(THISVECTOR=xVector,INCX=1) [PETSC] FAILED!"
      STOP 666
    ENDIF
    r_index = 0_SIK
    r_index = BLAS_iamax(THISVECTOR=xVector,N=xVector%n)
    IF(r_index /= 3_SIK) THEN
      WRITE(*,*) "CALL BLAS_iamax(THISVECTOR=xVector,N=xVector%n) [PETSC] FAILED!"
      STOP 666
    ENDIF
    r_index = 0_SIK
    r_index = BLAS_iamax(THISVECTOR=xVector)
    IF(r_index /= 3_SIK) THEN
      WRITE(*,*) "CALL BLAS_iamax(THISVECTOR=xVector) [PETSC] FAILED!"
      STOP 666
    ENDIF
    ! next test with a negative value
    CALL xVector%set(1, 12.0_SRK)
    CALL xVector%set(2,-30.0_SRK)
    CALL xVector%set(3, 17.0_SRK)
    r_index = 0_SIK
    r_index = BLAS_iamax(THISVECTOR=xVector,N=xVector%n,INCX=1)
    IF(r_index /= 2_SIK) THEN
      WRITE(*,*) "CALL BLAS_iamax(THISVECTOR=xVector,N=xVector%n,INCX=1) [PETSC] FAILED!"
      STOP 666
    ENDIF
    r_index = 0_SIK
    r_index = BLAS_iamax(THISVECTOR=xVector,INCX=1)
    IF(r_index /= 2_SIK) THEN
      WRITE(*,*) "CALL BLAS_iamax(THISVECTOR=xVector,INCX=1) [PETSC] FAILED!"
      STOP 666
    ENDIF
    r_index = 0_SIK
    r_index = BLAS_iamax(THISVECTOR=xVector,N=xVector%n)
    IF(r_index /= 2_SIK) THEN
      WRITE(*,*) "CALL BLAS_iamax(THISVECTOR=xVector,N=xVector%n) [PETSC] FAILED!"
      STOP 666
    ENDIF
    r_index = 0_SIK
    r_index = BLAS_iamax(THISVECTOR=xVector)
    IF(r_index /= 2_SIK) THEN
      WRITE(*,*) "CALL BLAS_iamax(THISVECTOR=xVector) [PETSC] FAILED!"
      STOP 666
    ENDIF
    WRITE(*,*) '  Passed: CALL BLAS_iamax(...) [PETSC]'
    
    !Test BLAS_iamin
    ! first test with all positive values
    CALL xVector%set(1,12.0_SRK)
    CALL xVector%set(2, 3.0_SRK)
    CALL xVector%set(3,17.0_SRK)
    r_index = 0_SIK
    r_index = BLAS_iamin(THISVECTOR=xVector,N=xVector%n,INCX=1)
    IF(r_index /= 2_SIK) THEN
      WRITE(*,*) "CALL BLAS_iamin(THISVECTOR=xVector,N=xVector%n,INCX=1) [PETSC] FAILED!"
      STOP 666
    ENDIF
    r_index = 0_SIK
    r_index = BLAS_iamin(THISVECTOR=xVector,INCX=1)
    IF(r_index /= 2_SIK) THEN
      WRITE(*,*) "CALL BLAS_iamin(THISVECTOR=xVector,INCX=1) [PETSC] FAILED!"
      STOP 666
    ENDIF
    r_index = 0_SIK
    r_index = BLAS_iamin(THISVECTOR=xVector,N=xVector%n)
    IF(r_index /= 2_SIK) THEN
      WRITE(*,*) "CALL BLAS_iamin(THISVECTOR=xVector,N=xVector%n) [PETSC] FAILED!"
      STOP 666
    ENDIF
    r_index = 0_SIK
    r_index = BLAS_iamin(THISVECTOR=xVector)
    IF(r_index /= 2_SIK) THEN
      WRITE(*,*) "CALL BLAS_iamin(THISVECTOR=xVector) [PETSC] FAILED!"
      STOP 666
    ENDIF
    ! next test with a negative value
    CALL xVector%set(1, 12.0_SRK)
    CALL xVector%set(2,-30.0_SRK)
    CALL xVector%set(3, 17.0_SRK)
    r_index = 0_SIK
    r_index = BLAS_iamin(THISVECTOR=xVector,N=xVector%n,INCX=1)
    IF(r_index /= 1_SIK) THEN
      WRITE(*,*) "CALL BLAS_iamin(THISVECTOR=xVector,N=xVector%n,INCX=1) [PETSC] FAILED!"
      STOP 666
    ENDIF
    r_index = 0_SIK
    r_index = BLAS_iamin(THISVECTOR=xVector,INCX=1)
    IF(r_index /= 1_SIK) THEN
      WRITE(*,*) "CALL BLAS_iamin(THISVECTOR=xVector,INCX=1) [PETSC] FAILED!"
      STOP 666
    ENDIF
    r_index = 0_SIK
    r_index = BLAS_iamin(THISVECTOR=xVector,N=xVector%n)
    IF(r_index /= 1_SIK) THEN
      WRITE(*,*) "CALL BLAS_iamin(THISVECTOR=xVector,N=xVector%n) [PETSC] FAILED!"
      STOP 666
    ENDIF
    r_index = 0_SIK
    r_index = BLAS_iamin(THISVECTOR=xVector)
    IF(r_index /= 1_SIK) THEN
      WRITE(*,*) "CALL BLAS_iamin(THISVECTOR=xVector) [PETSC] FAILED!"
      STOP 666
    ENDIF
    WRITE(*,*) '  Passed: CALL BLAS_iamin(...) [PETSC]'
    
    !Test BLAS_nrm2
    CALL xVector%set(1,0.0_SRK)
    CALL xVector%set(2,3.0_SRK)
    CALL xVector%set(3,4.0_SRK)
    r = 0.0_SRK
    r = BLAS_nrm2(THISVECTOR=xVector,N=xVector%n,INCX=1)
    IF(.NOT.(r .APPROXEQ. 5._SRK)) THEN
      WRITE(*,*) "CALL BLAS_nrm2(THISVECTOR=xVector,N=xVector%n,INCX=1) [PETSC] FAILED!"
      STOP 666
    ENDIF
    r = 0.0_SRK
    r = BLAS_nrm2(THISVECTOR=xVector,INCX=1)
    IF(.NOT.(r .APPROXEQ. 5._SRK)) THEN
      WRITE(*,*) "CALL BLAS_nrm2(THISVECTOR=xVector,INCX=1) [PETSC] FAILED!"
      STOP 666
    ENDIF
    r = 0.0_SRK
    r = BLAS_nrm2(THISVECTOR=xVector,N=xVector%n)
    IF(.NOT.(r .APPROXEQ. 5._SRK)) THEN
      WRITE(*,*) "CALL BLAS_nrm2(THISVECTOR=xVector,N=xVector%n) [PETSC] FAILED!"
      STOP 666
    ENDIF
    r = 0.0_SRK
    r = BLAS_nrm2(THISVECTOR=xVector)
    IF(.NOT.(r .APPROXEQ. 5._SRK)) THEN
      WRITE(*,*) "CALL BLAS_nrm2(THISVECTOR=xVector) [PETSC] FAILED!"
      STOP 666
    ENDIF
    WRITE(*,*) '  Passed: CALL BLAS_nrm2(...) [PETSC]'
    
    !Test BLAS_scal [scalar a]
    a = 3
    CALL xVector%set(1,1.0_SRK)
    CALL xVector%set(2,3.0_SRK)
    CALL xVector%set(3,5.0_SRK)
    CALL BLAS_scal(THISVECTOR=xVector,A=a,N=xVector%n,INCX=1)
    IF(ALLOCATED(dummyvec)) DEALLOCATE(dummyvec)
    ALLOCATE(dummyvec(xVector%n))
    CALL xVector%get(dummyvec)
    IF((.NOT.((dummyvec(1) .APPROXEQ.  3._SRK) .AND. &
              (dummyvec(2) .APPROXEQ.  9._SRK) .AND. &
              (dummyvec(3) .APPROXEQ. 15._SRK)))) THEN
      WRITE(*,*) "CALL BLAS_scal(THISVECTOR=xVector,A=a,N=xVector%n,INCX=1) -scalar_a [PETSC] FAILED!"
      STOP 666
    ENDIF
    CALL xVector%set(1,1.0_SRK)
    CALL xVector%set(2,3.0_SRK)
    CALL xVector%set(3,5.0_SRK)
    CALL BLAS_scal(THISVECTOR=xVector,A=a,INCX=1)
    CALL xVector%get(dummyvec)
    IF((.NOT.((dummyvec(1) .APPROXEQ.  3._SRK) .AND. &
              (dummyvec(2) .APPROXEQ.  9._SRK) .AND. &
              (dummyvec(3) .APPROXEQ. 15._SRK)))) THEN
      WRITE(*,*) "CALL BLAS_scal(THISVECTOR=xVector,A=a,INCX=1) -scalar_a [PETSC] FAILED!"
      STOP 666
    ENDIF
    CALL xVector%set(1,1.0_SRK)
    CALL xVector%set(2,3.0_SRK)
    CALL xVector%set(3,5.0_SRK)
    CALL BLAS_scal(THISVECTOR=xVector,A=a,N=xVector%n)
    CALL xVector%get(dummyvec)
    IF((.NOT.((dummyvec(1) .APPROXEQ.  3._SRK) .AND. &
              (dummyvec(2) .APPROXEQ.  9._SRK) .AND. &
              (dummyvec(3) .APPROXEQ. 15._SRK)))) THEN
      WRITE(*,*) "CALL BLAS_scal(THISVECTOR=xVector,A=a,N=xVector%n) -scalar_a [PETSC] FAILED!"
      STOP 666
    ENDIF
    CALL xVector%set(1,1.0_SRK)
    CALL xVector%set(2,3.0_SRK)
    CALL xVector%set(3,5.0_SRK)
    CALL BLAS_scal(THISVECTOR=xVector,A=a)
    CALL xVector%get(dummyvec)
    IF((.NOT.((dummyvec(1) .APPROXEQ.  3._SRK) .AND. &
              (dummyvec(2) .APPROXEQ.  9._SRK) .AND. &
              (dummyvec(3) .APPROXEQ. 15._SRK)))) THEN
      WRITE(*,*) "CALL BLAS_scal(THISVECTOR=xVector,A=a) -scalar_a [PETSC] FAILED!"
      STOP 666
    ENDIF
    WRITE(*,*) '  Passed: CALL BLAS_scal(...) -scalar_a [PETSC]'
    
    !Test BLAS_scal [vector a]
    CALL aVector%set(1,2.0_SRK)
    CALL aVector%set(2,4.0_SRK)
    CALL aVector%set(3,1.0_SRK)
    CALL xVector%set(1,1.0_SRK)
    CALL xVector%set(2,3.0_SRK)
    CALL xVector%set(3,5.0_SRK)
    CALL BLAS_scal(THISVECTOR=xVector,AVECTOR=aVector,N=xVector%n,INCX=1)
    IF(ALLOCATED(dummyvec)) DEALLOCATE(dummyvec)
    ALLOCATE(dummyvec(xVector%n))
    CALL xVector%get(dummyvec)
    IF((.NOT.((dummyvec(1) .APPROXEQ.  2._SRK) .AND. &
              (dummyvec(2) .APPROXEQ. 12._SRK) .AND. &
              (dummyvec(3) .APPROXEQ.  5._SRK)))) THEN
      WRITE(*,*) "CALL BLAS_scal(THISVECTOR=xVector,AVECTOR=aVector,N=xVector%n,INCX=1) -vector_a [PETSC] FAILED!"
      STOP 666
    ENDIF
    CALL xVector%set(1,1.0_SRK)
    CALL xVector%set(2,3.0_SRK)
    CALL xVector%set(3,5.0_SRK)
    CALL BLAS_scal(THISVECTOR=xVector,AVECTOR=aVector,INCX=1)
    CALL xVector%get(dummyvec)
    IF((.NOT.((dummyvec(1) .APPROXEQ.  2._SRK) .AND. &
              (dummyvec(2) .APPROXEQ. 12._SRK) .AND. &
              (dummyvec(3) .APPROXEQ.  5._SRK)))) THEN
      WRITE(*,*) "CALL BLAS_scal(THISVECTOR=xVector,AVECTOR=aVector,INCX=1) -vector_a [PETSC] FAILED!"
      STOP 666
    ENDIF
    CALL xVector%set(1,1.0_SRK)
    CALL xVector%set(2,3.0_SRK)
    CALL xVector%set(3,5.0_SRK)
    CALL BLAS_scal(THISVECTOR=xVector,AVECTOR=aVector,N=xVector%n)
    CALL xVector%get(dummyvec)
    IF((.NOT.((dummyvec(1) .APPROXEQ.  2._SRK) .AND. &
              (dummyvec(2) .APPROXEQ. 12._SRK) .AND. &
              (dummyvec(3) .APPROXEQ.  5._SRK)))) THEN
      WRITE(*,*) "CALL BLAS_scal(THISVECTOR=xVector,AVECTOR=aVector,N=xVector%n) -vector_a [PETSC] FAILED!"
      STOP 666
    ENDIF
    CALL xVector%set(1,1.0_SRK)
    CALL xVector%set(2,3.0_SRK)
    CALL xVector%set(3,5.0_SRK)
    CALL BLAS_scal(THISVECTOR=xVector,AVECTOR=aVector)
    CALL xVector%get(dummyvec)
    IF((.NOT.((dummyvec(1) .APPROXEQ.  2._SRK) .AND. &
              (dummyvec(2) .APPROXEQ. 12._SRK) .AND. &
              (dummyvec(3) .APPROXEQ.  5._SRK)))) THEN
      WRITE(*,*) "CALL BLAS_scal(THISVECTOR=xVector,AVECTOR=aVector) -vector_a [PETSC] FAILED!"
      STOP 666
    ENDIF
    WRITE(*,*) '  Passed: CALL BLAS_scal(...) -vector_a [PETSC]'
    
    !Test BLAS_swap
    CALL xVector%set(1,1.0_SRK)
    CALL xVector%set(2,3.0_SRK)
    CALL xVector%set(3,5.0_SRK)
    CALL yVector%set(1,2.0_SRK)
    CALL yVector%set(2,4.0_SRK)
    CALL yVector%set(3,6.0_SRK)
    CALL BLAS_swap(THISVECTOR=xVector,THATVECTOR=yVector,N=xVector%n,INCX=1,INCY=1)
    IF(ALLOCATED(dummyvec)) DEALLOCATE(dummyvec)
    IF(ALLOCATED(dummyvec2)) DEALLOCATE(dummyvec2)
    ALLOCATE(dummyvec(xVector%n))
    ALLOCATE(dummyvec2(yVector%n))
    CALL xVector%get(dummyvec)
    CALL yVector%get(dummyvec2)
    IF((.NOT.((dummyvec(1) .APPROXEQ. 2._SRK) .AND. &
              (dummyvec(2) .APPROXEQ. 4._SRK) .AND. &
              (dummyvec(3) .APPROXEQ. 6._SRK) .AND. &
              (dummyvec2(1) .APPROXEQ. 1._SRK) .AND. &
              (dummyvec2(2) .APPROXEQ. 3._SRK) .AND. &
              (dummyvec2(3) .APPROXEQ. 5._SRK)))) THEN
      WRITE(*,*) "CALL BLAS_swap(THISVECTOR=xVector,THATVECTOR=yVector,N=xVector%n,INCX=1,INCY=1) [PETSC] FAILED!"
      STOP 666
    ENDIF
    CALL xVector%set(1,1.0_SRK)
    CALL xVector%set(2,3.0_SRK)
    CALL xVector%set(3,5.0_SRK)
    CALL yVector%set(1,2.0_SRK)
    CALL yVector%set(2,4.0_SRK)
    CALL yVector%set(3,6.0_SRK)
    CALL BLAS_swap(THISVECTOR=xVector,THATVECTOR=yVector,INCX=1,INCY=1)
    CALL xVector%get(dummyvec)
    CALL yVector%get(dummyvec2)
    IF((.NOT.((dummyvec(1) .APPROXEQ. 2._SRK) .AND. &
              (dummyvec(2) .APPROXEQ. 4._SRK) .AND. &
              (dummyvec(3) .APPROXEQ. 6._SRK) .AND. &
              (dummyvec2(1) .APPROXEQ. 1._SRK) .AND. &
              (dummyvec2(2) .APPROXEQ. 3._SRK) .AND. &
              (dummyvec2(3) .APPROXEQ. 5._SRK)))) THEN
      WRITE(*,*) "CALL BLAS_swap(THISVECTOR=xVector,THATVECTOR=yVector,INCX=1,INCY=1) [PETSC] FAILED!"
      STOP 666
    ENDIF
    CALL xVector%set(1,1.0_SRK)
    CALL xVector%set(2,3.0_SRK)
    CALL xVector%set(3,5.0_SRK)
    CALL yVector%set(1,2.0_SRK)
    CALL yVector%set(2,4.0_SRK)
    CALL yVector%set(3,6.0_SRK)
    CALL BLAS_swap(THISVECTOR=xVector,THATVECTOR=yVector,N=xVector%n,INCX=1)
    CALL xVector%get(dummyvec)
    CALL yVector%get(dummyvec2)
    IF((.NOT.((dummyvec(1) .APPROXEQ. 2._SRK) .AND. &
              (dummyvec(2) .APPROXEQ. 4._SRK) .AND. &
              (dummyvec(3) .APPROXEQ. 6._SRK) .AND. &
              (dummyvec2(1) .APPROXEQ. 1._SRK) .AND. &
              (dummyvec2(2) .APPROXEQ. 3._SRK) .AND. &
              (dummyvec2(3) .APPROXEQ. 5._SRK)))) THEN
      WRITE(*,*) "CALL BLAS_swap(THISVECTOR=xVector,THATVECTOR=yVector,N=xVector%n,INCX=1) [PETSC] FAILED!"
      STOP 666
    ENDIF
    CALL xVector%set(1,1.0_SRK)
    CALL xVector%set(2,3.0_SRK)
    CALL xVector%set(3,5.0_SRK)
    CALL yVector%set(1,2.0_SRK)
    CALL yVector%set(2,4.0_SRK)
    CALL yVector%set(3,6.0_SRK)
    CALL BLAS_swap(THISVECTOR=xVector,THATVECTOR=yVector,N=xVector%n,INCY=1)
    CALL xVector%get(dummyvec)
    CALL yVector%get(dummyvec2)
    IF((.NOT.((dummyvec(1) .APPROXEQ. 2._SRK) .AND. &
              (dummyvec(2) .APPROXEQ. 4._SRK) .AND. &
              (dummyvec(3) .APPROXEQ. 6._SRK) .AND. &
              (dummyvec2(1) .APPROXEQ. 1._SRK) .AND. &
              (dummyvec2(2) .APPROXEQ. 3._SRK) .AND. &
              (dummyvec2(3) .APPROXEQ. 5._SRK)))) THEN
      WRITE(*,*) "CALL BLAS_swap(THISVECTOR=xVector,THATVECTOR=yVector,N=xVector%n,INCY=1) [PETSC] FAILED!"
      STOP 666
    ENDIF
    CALL xVector%set(1,1.0_SRK)
    CALL xVector%set(2,3.0_SRK)
    CALL xVector%set(3,5.0_SRK)
    CALL yVector%set(1,2.0_SRK)
    CALL yVector%set(2,4.0_SRK)
    CALL yVector%set(3,6.0_SRK)
    CALL BLAS_swap(THISVECTOR=xVector,THATVECTOR=yVector,INCX=1)
    CALL xVector%get(dummyvec)
    CALL yVector%get(dummyvec2)
    IF((.NOT.((dummyvec(1) .APPROXEQ. 2._SRK) .AND. &
              (dummyvec(2) .APPROXEQ. 4._SRK) .AND. &
              (dummyvec(3) .APPROXEQ. 6._SRK) .AND. &
              (dummyvec2(1) .APPROXEQ. 1._SRK) .AND. &
              (dummyvec2(2) .APPROXEQ. 3._SRK) .AND. &
              (dummyvec2(3) .APPROXEQ. 5._SRK)))) THEN
      WRITE(*,*) "CALL BLAS_swap(THISVECTOR=xVector,THATVECTOR=yVector,INCX=1) [PETSC] FAILED!"
      STOP 666
    ENDIF
    CALL xVector%set(1,1.0_SRK)
    CALL xVector%set(2,3.0_SRK)
    CALL xVector%set(3,5.0_SRK)
    CALL yVector%set(1,2.0_SRK)
    CALL yVector%set(2,4.0_SRK)
    CALL yVector%set(3,6.0_SRK)
    CALL BLAS_swap(THISVECTOR=xVector,THATVECTOR=yVector,INCY=1)
    CALL xVector%get(dummyvec)
    CALL yVector%get(dummyvec2)
    IF((.NOT.((dummyvec(1) .APPROXEQ. 2._SRK) .AND. &
              (dummyvec(2) .APPROXEQ. 4._SRK) .AND. &
              (dummyvec(3) .APPROXEQ. 6._SRK) .AND. &
              (dummyvec2(1) .APPROXEQ. 1._SRK) .AND. &
              (dummyvec2(2) .APPROXEQ. 3._SRK) .AND. &
              (dummyvec2(3) .APPROXEQ. 5._SRK)))) THEN
      WRITE(*,*) "CALL BLAS_swap(THISVECTOR=xVector,THATVECTOR=yVector,INCY=1) [PETSC] FAILED!"
      STOP 666
    ENDIF
    CALL xVector%set(1,1.0_SRK)
    CALL xVector%set(2,3.0_SRK)
    CALL xVector%set(3,5.0_SRK)
    CALL yVector%set(1,2.0_SRK)
    CALL yVector%set(2,4.0_SRK)
    CALL yVector%set(3,6.0_SRK)
    CALL BLAS_swap(THISVECTOR=xVector,THATVECTOR=yVector,N=xVector%n)
    CALL xVector%get(dummyvec)
    CALL yVector%get(dummyvec2)
    IF((.NOT.((dummyvec(1) .APPROXEQ. 2._SRK) .AND. &
              (dummyvec(2) .APPROXEQ. 4._SRK) .AND. &
              (dummyvec(3) .APPROXEQ. 6._SRK) .AND. &
              (dummyvec2(1) .APPROXEQ. 1._SRK) .AND. &
              (dummyvec2(2) .APPROXEQ. 3._SRK) .AND. &
              (dummyvec2(3) .APPROXEQ. 5._SRK)))) THEN
      WRITE(*,*) "CALL BLAS_swap(THISVECTOR=xVector,THATVECTOR=yVector,N=xVector%n) [PETSC] FAILED!"
      STOP 666
    ENDIF
    CALL xVector%set(1,1.0_SRK)
    CALL xVector%set(2,3.0_SRK)
    CALL xVector%set(3,5.0_SRK)
    CALL yVector%set(1,2.0_SRK)
    CALL yVector%set(2,4.0_SRK)
    CALL yVector%set(3,6.0_SRK)
    CALL BLAS_swap(THISVECTOR=xVector,THATVECTOR=yVector)
    CALL xVector%get(dummyvec)
    CALL yVector%get(dummyvec2)
    IF((.NOT.((dummyvec(1) .APPROXEQ. 2._SRK) .AND. &
              (dummyvec(2) .APPROXEQ. 4._SRK) .AND. &
              (dummyvec(3) .APPROXEQ. 6._SRK) .AND. &
              (dummyvec2(1) .APPROXEQ. 1._SRK) .AND. &
              (dummyvec2(2) .APPROXEQ. 3._SRK) .AND. &
              (dummyvec2(3) .APPROXEQ. 5._SRK)))) THEN
      WRITE(*,*) "CALL BLAS_swap(THISVECTOR=xVector,THATVECTOR=yVector) [PETSC] FAILED!"
      STOP 666
    ENDIF
    WRITE(*,*) '  Passed: CALL BLAS_swap(...) [PETSC]'
    CALL xVector%clear()
    CALL yVector%clear()
    CALL aVector%clear()
    DEALLOCATE(xVector)
    DEALLOCATE(yVector)
    DEALLOCATE(aVector)
#endif
    ENDSUBROUTINE testBLAS1Interface
!
ENDPROGRAM testVectorTypes
