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
  IMPLICIT NONE
  
#ifdef HAVE_PETSC
#include <finclude/petsc.h>
#undef IS
 
  PetscErrorCode  :: ierr
  
#endif
  
  TYPE(ExceptionHandlerType),POINTER :: e
  
  !Configure exception handler for test
  ALLOCATE(e)
  CALL e%setStopOnError(.FALSE.)
  CALL e%setQuietMode(.TRUE.)
  eMatrixType => e
  
#ifdef HAVE_PETSC    
      CALL PetscInitialize(PETSC_NULL_CHARACTER,ierr)
#endif

  WRITE(*,*) '==================================================='
  WRITE(*,*) 'TESTING VECTOR TYPES...'
  WRITE(*,*) '==================================================='
  
  CALL testVector()
  
  CALL testBLAS1Interface()
  
  WRITE(*,*) '==================================================='
  WRITE(*,*) 'TESTING VECTOR TYPES PASSED!'
  WRITE(*,*) '==================================================='
  DEALLOCATE(e)
  
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
      REAL(SRK),ALLOCATABLE :: testvec(:),testvec2(:) 
      REAL(SRK) :: dummy

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
      CALL thisVector%init(10)
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
        
      !now check init without m being provided
      CALL thisVector%init(-10) !expect exception
      IF(thisVector%isInit) THEN
        WRITE(*,*) 'CALL realvec%init(...) FAILED!'
        STOP 666
      ENDIF
      CALL thisVector%clear()
        
      !init it twice so on 2nd init, isInit==.TRUE.
      CALL thisVector%init(10)
      SELECTTYPE(thisVector)
        TYPE IS(RealVectorType); thisVector%n=1
      ENDSELECT
      CALL thisVector%init(10)
      SELECTTYPE(thisVector)
        TYPE IS(RealVectorType)
          IF(thisVector%n/=1) THEN !n/=1 implies it was changed, and thus fail
            WRITE(*,*) 'CALL realvec%init(...) FAILED!' !expect exception
            STOP 666
          ENDIF
      ENDSELECT
     !init with n<1
      CALL thisVector%clear()
      CALL thisVector%init(-1) !expect exception
      IF(thisVector%isInit) THEN
        WRITE(*,*) 'CALL realvec%init(...) FAILED!'
        STOP 666
      ENDIF
      CALL thisVector%clear()
      !n<1, and m not provided
      CALL thisVector%init(-1) !expect exception
      IF(thisVector%isInit) THEN
        WRITE(*,*) 'CALL realvec%init(...) FAILED!'
        STOP 666
      ENDIF
      CALL thisVector%clear()
      !init with m<1
      CALL thisVector%clear()
      CALL thisVector%init(-10) !expect exception
      IF(thisVector%isInit) THEN
        WRITE(*,*) 'CALL realvec%init(...) FAILED!'
        STOP 666
      ENDIF
      CALL thisVector%clear()
      WRITE(*,*) '  Passed: CALL realvec%init(...)'
      
      !Perform test of set function
      !use set to update the values
      CALL thisVector%init(6)
      CALL thisVector%set(1,1._SRK)
      CALL thisVector%set(2,2._SRK)
      CALL thisVector%set(3,3._SRK)
      CALL thisVector%set(4,4._SRK)
      CALL thisVector%set(5,5._SRK)
      CALL thisVector%set(6,6._SRK)
      SELECTTYPE(thisVector)
        TYPE IS(RealVectorType)
          !now compare actual values with expected
          DO i=1,6
            IF((thisVector%b(i) /= i)) THEN
              WRITE(*,*) 'CALL realvec%setOne(...) FAILED!'
              STOP 666
            ENDIF
          ENDDO
      ENDSELECT
      
      !set uninit matrix.
      CALL thisVector%clear()
      CALL thisVector%set(1,1._SRK) !since isInit=.FALSE. expect no change
      
      !pass out-of bounds i and j
      CALL thisVector%clear()
      CALL thisVector%init(6)   
      CALL thisVector%set(-1,1._SRK)
      CALL thisVector%set(7,1._SRK)

      CALL thisVector%clear()
      CALL thisVector%init(6)
        
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
      CALL thisVector%init(6)
      CALL thisVector%set(10._SRK)
      SELECTTYPE(thisVector)
        TYPE IS(RealVectorType)
          !now compare actual values with expected
          DO i=1,6
            IF((thisVector%b(i) /= 10._SRK)) THEN
              WRITE(*,*) 'CALL realvec%setAll_scalar(...) FAILED!'
              STOP 666
            ENDIF
          ENDDO
      ENDSELECT
      
      !set uninit
      CALL thisVector%clear()
      CALL thisVector%set(1._SRK) !since isInit=.FALSE. expect no change
      
      CALL thisVector%init(6)
        
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
      CALL thisVector%init(6)
      ALLOCATE(testvec(6))
      ALLOCATE(testvec2(6))
      testvec(1)=2._SRK
      testvec(2)=4._SRK
      testvec(3)=6._SRK
      testvec(4)=8._SRK
      testvec(5)=10._SRK
      testvec(6)=12._SRK
      CALL thisVector%set(testvec)
      SELECTTYPE(thisVector)
        TYPE IS(RealVectorType)
          !now compare actual values with expected
          DO i=1,6
            IF((thisVector%b(i) /= testvec(i))) THEN
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
      CALL thisVector%set(testvec2) !since isInit=.FALSE. expect no change
      
      CALL thisVector%init(6)
        
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
      CALL thisVector%init(6)
      CALL thisVector%set(4,6,10._SRK)
      SELECTTYPE(thisVector)
        TYPE IS(RealVectorType)
          !now compare actual values with expected
          DO i=1,3
            IF((thisVector%b(i) /= 0._SRK)) THEN
              WRITE(*,*) 'CALL realvec%setRange_scalar(...) FAILED!'
              STOP 666
            ENDIF
          ENDDO
          DO i=4,6
            IF((thisVector%b(i) /= 10._SRK)) THEN
              WRITE(*,*) 'CALL realvec%setRange_scalar(...) FAILED!'
              STOP 666
            ENDIF
          ENDDO
      ENDSELECT
      
      !set uninit
      CALL thisVector%clear()
      CALL thisVector%set(4,6,1._SRK) !since isInit=.FALSE. expect no change
      
      CALL thisVector%init(6)
        
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
      CALL thisVector%init(6)
      DEALLOCATE(testvec,testvec2)
      ALLOCATE(testvec(3))
      ALLOCATE(testvec2(3))
      testvec(1)=2._SRK
      testvec(2)=4._SRK
      testvec(3)=6._SRK
      CALL thisVector%set(4,6,testvec)
      SELECTTYPE(thisVector)
        TYPE IS(RealVectorType)
          !now compare actual values with expected
          DO i=1,3
            IF((thisVector%b(i) /= 0._SRK)) THEN
              WRITE(*,*) 'CALL realvec%setRange_array(...) FAILED!'
              STOP 666
            ENDIF
          ENDDO
          DO i=4,6
            IF((thisVector%b(i) /= testvec(i-3))) THEN
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
      CALL thisVector%set(4,6,testvec2) !since isInit=.FALSE. expect no change
      
      CALL thisVector%init(6)
        
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
      CALL thisVector%init(7)
      SELECTTYPE(thisVector)
        TYPE IS(RealVectorType)
          CALL thisVector%set(1,1._SRK)
          CALL thisVector%set(2,5._SRK)
          CALL thisVector%set(3,8._SRK)
          CALL thisVector%set(4,9._SRK)
          CALL thisVector%set(5,3._SRK)
          CALL thisVector%set(6,7._SRK)
          CALL thisVector%set(7,2._SRK)
          IF((thisVector%get(1) /= 1._SRK) .OR. &
             (thisVector%get(2) /= 5._SRK) .OR. &
             (thisVector%get(3) /= 8._SRK) .OR. &
             (thisVector%get(4) /= 9._SRK) .OR. &
             (thisVector%get(5) /= 3._SRK) .OR. &
             (thisVector%get(6) /= 7._SRK) .OR. &
             (thisVector%get(7) /= 2._SRK)) THEN
            WRITE(*,*) 'CALL realvec%getOne(...) FAILED!' 
            STOP 666
          ENDIF
      ENDSELECT
      !test with out of bounds, make sure no crash.
      SELECTTYPE(thisVector)
        TYPE IS(RealVectorType)
          dummy=thisVector%get(8)
          IF(dummy /= -1051._SRK) THEN
            WRITE(*,*) 'CALL realvec%getOne(...) FAILED!'
            STOP 666
          ENDIF
          dummy=thisVector%get(-1)
          IF(dummy/=-1051._SRK) THEN
            WRITE(*,*) 'CALL realvec%getOne(...) FAILED!'
            STOP 666
          ENDIF
      ENDSELECT
      !test get with uninit, make sure no crash.
      CALL thisVector%clear()
      SELECTTYPE(thisVector)
        TYPE IS(RealVectorType)      
          dummy=thisVector%get(1)
          IF(dummy /= 0.0_SRK) THEN
            WRITE(*,*) 'CALL realvec%getOne(...) FAILED!'
            STOP 666
          ENDIF
      ENDSELECT
      WRITE(*,*) '  Passed: CALL realvec%getOne(...)'
      
      CALL thisVector%clear()
      CALL thisVector%init(7)
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
          testvec=thisVector%get()
          IF((testvec(1) /= 1._SRK) .OR. &
             (testvec(2) /= 5._SRK) .OR. &
             (testvec(3) /= 8._SRK) .OR. &
             (testvec(4) /= 9._SRK) .OR. &
             (testvec(5) /= 3._SRK) .OR. &
             (testvec(6) /= 7._SRK) .OR. &
             (testvec(7) /= 2._SRK)) THEN
            WRITE(*,*) 'CALL realvec%getAll(...) FAILED!' 
            STOP 666
          ENDIF
      ENDSELECT
      !test get with uninit, make sure no crash.
      CALL thisVector%clear()
      testvec=0._SRK
      SELECTTYPE(thisVector)
        TYPE IS(RealVectorType)    
          testvec=thisVector%get()
          IF(testvec(1)/=-1051._SRK) THEN
            WRITE(*,*) 'CALL realvec%getAll(...) FAILED!'
            STOP 666
          ENDIF
      ENDSELECT
      WRITE(*,*) '  Passed: CALL realvec%getAll(...)'
      
      CALL thisVector%clear()
      CALL thisVector%init(7)  
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
          testvec=thisVector%get(5,7)
          IF((testvec(1) /= 3._SRK) .OR. &
             (testvec(2) /= 7._SRK) .OR. &
             (testvec(3) /= 2._SRK)) THEN
            WRITE(*,*) 'CALL realvec%getRange(...) FAILED!' 
            STOP 666
          ENDIF
      ENDSELECT
      !test get with uninit, make sure no crash.
      CALL thisVector%clear()
      testvec=0._SRK
      SELECTTYPE(thisVector)
        TYPE IS(RealVectorType)      
          testvec=thisVector%get(5,7)
          DO i=1,3
            IF(testvec(i) /=-1051.0_SRK) THEN
              WRITE(*,*) 'CALL realvec%getRange(...) FAILED!'
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
          !check if pointer for b is null
          !if not, clear did not destroy it
          IF(thisVector%b /= PETSC_NULL_REAL) THEN
            WRITE(*,*) 'CALL petscvec%clear() FAILED!'
            STOP 666
          ENDIF
          WRITE(*,*) '  Passed: CALL petscvec%clear()'
      ENDSELECT
      
      !Perform test of init function
      !first check intended init path (m provided)
      eVectorType => NULL()
      CALL thisVector%init(10)
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
      CALL thisVector%init(-10) !expect exception
      IF(thisVector%isInit) THEN
        WRITE(*,*) 'CALL petscvec%init(...) FAILED!'
        STOP 666
      ENDIF
      CALL thisVector%clear()
        
      !init it twice so on 2nd init, isInit==.TRUE.
      CALL thisVector%init(10)
      SELECTTYPE(thisVector)
        TYPE IS(PETScVectorType); thisVector%n=1
      ENDSELECT
      CALL thisVector%init(10)
      SELECTTYPE(thisVector)
        TYPE IS(PETScVectorType)
          IF(thisVector%n/=1) THEN !n/=1 implies it was changed, and thus fail
            WRITE(*,*) 'CALL petscvec%init(...) FAILED!' !expect exception
            STOP 666
          ENDIF
      ENDSELECT
     !init with n<1
      CALL thisVector%clear()
      CALL thisVector%init(-1) !expect exception
      IF(thisVector%isInit) THEN
        WRITE(*,*) 'CALL petscvec%init(...) FAILED!'
        STOP 666
      ENDIF
      CALL thisVector%clear()
      !n<1, and m not provided
      CALL thisVector%init(-1) !expect exception
      IF(thisVector%isInit) THEN
        WRITE(*,*) 'CALL petscvec%init(...) FAILED!'
        STOP 666
      ENDIF
      CALL thisVector%clear()
      !init with m<1
      CALL thisVector%clear()
      CALL thisVector%init(-10) !expect exception
      IF(thisVector%isInit) THEN
        WRITE(*,*) 'CALL petscvec%init(...) FAILED!'
        STOP 666
      ENDIF
      CALL thisVector%clear()
      WRITE(*,*) '  Passed: CALL petscvec%init(...)'
      
      !Perform test of set function
      !use set to update the values
      CALL thisVector%init(6)
      CALL thisVector%set(1,1._SRK)
      CALL thisVector%set(2,2._SRK)
      CALL thisVector%set(3,3._SRK)
      CALL thisVector%set(4,4._SRK)
      CALL thisVector%set(5,5._SRK)
      CALL thisVector%set(6,6._SRK)
      SELECTTYPE(thisVector)
        TYPE IS(PETScVectorType)
          ! manually assemble vector
          CALL VecAssemblyBegin(thisVector%b,ierr)
          CALL VecAssemblyEnd(thisVector%b,ierr)
          thisVector%isAssembled=.TRUE.
          
          !now compare actual values with expected
          DO i=1,6
            CALL VecGetValues(thisVector%b,1,i-1,dummy,ierr)
            IF(dummy /= i) THEN
              WRITE(*,*) 'CALL petscvec%setOne(...) FAILED!'
              STOP 666
            ENDIF
          ENDDO
      ENDSELECT
      
      !set uninit matrix.
      CALL thisVector%clear()
      CALL thisVector%set(1,1._SRK) !since isInit=.FALSE. expect no change
      
      !pass out-of bounds i and j
      CALL thisVector%clear()
      CALL thisVector%init(6)   
      CALL thisVector%set(-1,1._SRK)
      CALL thisVector%set(7,1._SRK)

      CALL thisVector%clear()
      CALL thisVector%init(6)
        
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
      CALL thisVector%init(6)
      CALL thisVector%set(10._SRK)
      SELECTTYPE(thisVector)
        TYPE IS(PETScVectorType)
          !now compare actual values with expected
          DO i=1,6
            IF((thisVector%get(i) /= 10._SRK)) THEN
              WRITE(*,*) 'CALL petscvec%setAll_scalar(...) FAILED!'
              STOP 666
            ENDIF
          ENDDO
      ENDSELECT
      
      !set uninit matrix.
      CALL thisVector%clear()
      CALL thisVector%set(1._SRK) !since isInit=.FALSE. expect no change
      
      CALL thisVector%init(6)
        
      SELECTTYPE(thisVector)
        TYPE IS(PETScVectorType)
          ! manually assemble vector
          CALL VecAssemblyBegin(thisVector%b,ierr)
          CALL VecAssemblyEnd(thisVector%b,ierr)
          thisVector%isAssembled=.TRUE.
          
          DO i=1,thisVector%n
            IF(thisVector%get(i) == 1._SRK) THEN
              WRITE(*,*) 'CALL petscvec%setAll_scalar(...) FAILED!'
              STOP 666
            ENDIF
          ENDDO
          WRITE(*,*) '  Passed: CALL petscvec%setAll_scalar(...)'
      ENDSELECT
      
      !Perform test of set function 
      !use set to update all values at once (array)
      CALL thisVector%init(6)
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
      CALL thisVector%set(testvec)
      SELECTTYPE(thisVector)
        TYPE IS(PETScVectorType)
          !now compare actual values with expected
          DO i=1,6
            IF((thisVector%get(i) /= testvec(i))) THEN
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
      CALL thisVector%set(testvec2) !since isInit=.FALSE. expect no change
      CALL thisVector%init(6)
      SELECTTYPE(thisVector)
        TYPE IS(PETScVectorType)
          ! manually assemble vector
          CALL VecAssemblyBegin(thisVector%b,ierr)
          CALL VecAssemblyEnd(thisVector%b,ierr)
          thisVector%isAssembled=.TRUE.
          
          DO i=1,thisVector%n
            IF(thisVector%get(i) == testvec2(i)) THEN
              WRITE(*,*) 'CALL petscvec%setAll_array(...) FAILED!'
              STOP 666
            ENDIF
          ENDDO
          WRITE(*,*) '  Passed: CALL petscvec%setAll_array(...)'
      ENDSELECT
      
      !Perform test of set function
      !use set to update all values at once (scalar)
      CALL thisVector%init(6)
      CALL thisVector%set(4,6,10._SRK)
      SELECTTYPE(thisVector)
        TYPE IS(PETScVectorType)
          !now compare actual values with expected
          DO i=1,3
            IF((thisVector%get(i) /= 0._SRK)) THEN
              WRITE(*,*) 'CALL petscvec%setRange_scalar(...) FAILED!'
              STOP 666
            ENDIF
          ENDDO
          DO i=4,6
            IF((thisVector%get(i) /= 10._SRK)) THEN
              WRITE(*,*) 'CALL petscvec%setRange_scalar(...) FAILED!'
              STOP 666
            ENDIF
          ENDDO
      ENDSELECT
      
      !set uninit matrix.
      CALL thisVector%clear()
      CALL thisVector%set(4,6,1._SRK) !since isInit=.FALSE. expect no change
      
      CALL thisVector%init(6)
        
      SELECTTYPE(thisVector)
        TYPE IS(PETScVectorType)
          ! manually assemble vector
          CALL VecAssemblyBegin(thisVector%b,ierr)
          CALL VecAssemblyEnd(thisVector%b,ierr)
          thisVector%isAssembled=.TRUE.
          
          DO i=1,thisVector%n
            IF(thisVector%get(i) == 1._SRK) THEN
              WRITE(*,*) 'CALL petscvec%setRange_scalar(...) FAILED!'
              STOP 666
            ENDIF
          ENDDO
          WRITE(*,*) '  Passed: CALL petscvec%setRange_scalar(...)'
      ENDSELECT
      
      !Perform test of set function
      !use set to update all values at once (array)
      CALL thisVector%init(6)
      DEALLOCATE(testvec)
      DEALLOCATE(testvec2)
      ALLOCATE(testvec(3))
      testvec(1)=1._SRK
      testvec(2)=3._SRK
      testvec(3)=5._SRK
      CALL thisVector%set(4,6,testvec)
      SELECTTYPE(thisVector)
        TYPE IS(PETScVectorType)
          !now compare actual values with expected
          DO i=1,3
            IF((thisVector%get(i) /= 0._SRK)) THEN
              WRITE(*,*) 'CALL petscvec%setRange_array(...) FAILED!'
              STOP 666
            ENDIF
          ENDDO
          DO i=4,6
            IF((thisVector%get(i) /= testvec(i-3))) THEN
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
      CALL thisVector%set(4,6,testvec2) !since isInit=.FALSE. expect no change
      CALL thisVector%init(6)
        
      SELECTTYPE(thisVector)
        TYPE IS(PETScVectorType)
          ! manually assemble vector
          CALL VecAssemblyBegin(thisVector%b,ierr)
          CALL VecAssemblyEnd(thisVector%b,ierr)
          thisVector%isAssembled=.TRUE.
          
          DO i=4,6
            IF(thisVector%get(i) == testvec2(i-3)) THEN
              WRITE(*,*) 'CALL petscvec%setRange_array(...) FAILED!'
              STOP 666
            ENDIF
          ENDDO
          WRITE(*,*) '  Passed: CALL petscvec%setRange_array(...)'
      ENDSELECT

      !Perform test of get function
      ![1 5 8 9 3 7 2]
      CALL thisVector%clear()
      CALL thisVector%init(7)
      SELECTTYPE(thisVector)
        TYPE IS(PETScVectorType)
          CALL thisVector%set(1,1._SRK)
          CALL thisVector%set(2,5._SRK)
          CALL thisVector%set(3,8._SRK)
          CALL thisVector%set(4,9._SRK)
          CALL thisVector%set(5,3._SRK)
          CALL thisVector%set(6,7._SRK)
          CALL thisVector%set(7,2._SRK)
          IF((thisVector%get(1) /= 1._SRK) .OR. &
             (thisVector%get(2) /= 5._SRK) .OR. &
             (thisVector%get(3) /= 8._SRK) .OR. &
             (thisVector%get(4) /= 9._SRK) .OR. &
             (thisVector%get(5) /= 3._SRK) .OR. &
             (thisVector%get(6) /= 7._SRK) .OR. &
             (thisVector%get(7) /= 2._SRK)) THEN
            WRITE(*,*) 'CALL petscvec%getOne(...) FAILED!' 
            STOP 666
          ENDIF
      ENDSELECT
      !test with out of bounds, make sure no crash.
      SELECTTYPE(thisVector)
        TYPE IS(PETScVectorType)
          dummy=thisVector%get(8)
          IF(dummy /= -1051._SRK) THEN
            WRITE(*,*) 'CALL petscvec%getOne(...) FAILED!'
            STOP 666
          ENDIF
          dummy=thisVector%get(-1)
          IF(dummy/=-1051._SRK) THEN
            WRITE(*,*) 'CALL petscvec%getOne(...) FAILED!'
            STOP 666
          ENDIF
      ENDSELECT
      !test get with uninit, make sure no crash.
      CALL thisVector%clear()
      SELECTTYPE(thisVector)
        TYPE IS(PETScVectorType)      
          dummy=thisVector%get(1)
          IF(dummy /= 0.0_SRK) THEN
            WRITE(*,*) 'CALL petscvec%getOne(...) FAILED!'
            STOP 666
          ENDIF
      ENDSELECT
      CALL thisVector%clear()
      WRITE(*,*) '  Passed: CALL petscvec%getOne(...)'
      
      !Perform test of get function
      ![1 5 8 9 3 7 2]
      CALL thisVector%clear()
      CALL thisVector%init(7)
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
          testvec=thisVector%get()
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
          testvec=thisVector%get()
          IF(testvec(1)/=-1051.0_SRK) THEN
            WRITE(*,*) 'CALL petscvec%getAll(...) FAILED!'
            STOP 666
          ENDIF
      ENDSELECT
      CALL thisVector%clear()
      WRITE(*,*) '  Passed: CALL petscvec%getAll(...)'
      
      !Perform test of get function
      ![1 5 8 9 3 7 2]
      CALL thisVector%clear()
      CALL thisVector%init(7)
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
          testvec=thisVector%get(5,7)
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
          testvec=thisVector%get(2,8)
          IF(testvec(1)/= -1051._SRK) THEN
            WRITE(*,*) 'CALL petscvec%getRange(...) FAILED!'
            STOP 666
          ENDIF
          testvec=thisVector%get(-1,2)
          IF(testvec(1)/=-1051._SRK) THEN
            WRITE(*,*) 'CALL petscvec%getRange(...) FAILED!'
            STOP 666
          ENDIF
      ENDSELECT
      !test get with uninit, make sure no crash.
      CALL thisVector%clear()
      SELECTTYPE(thisVector)
        TYPE IS(PETScVectorType)      
          testvec=thisVector%get(1,3)
          IF(testvec(1)/=-1051._SRK) THEN
            WRITE(*,*) 'CALL petscvec%getRange(...) FAILED!'
            STOP 666
          ENDIF
      ENDSELECT
      CALL thisVector%clear()
      WRITE(*,*) '  Passed: CALL petscvec%getRange(...)'
      
      DEALLOCATE(thisVector)
#endif
      
    ENDSUBROUTINE testVector
!
!-------------------------------------------------------------------------------  
    SUBROUTINE testBLAS1Interface()
    
    CLASS(VectorType),ALLOCATABLE :: xVector, yVector, aVector
    REAL(SRK) :: r, a
    INTEGER(SIK) :: r_index

    ALLOCATE(RealVectorType :: xVector)
    ALLOCATE(RealVectorType :: yVector)
    ALLOCATE(RealVectorType :: aVector)
    CALL xVector%init(3)
    CALL yVector%init(3)
    CALL aVector%init(3)
    
    !Test BLAS_asum (absolute value summation)
    CALL xVector%set(1, 5.0_SRK)
    CALL xVector%set(2,-2.0_SRK)
    CALL xVector%set(3, 7.0_SRK)
    r=0.0_SRK
    r = BLAS_asum(THISVECTOR=xVector,N=xVector%n,INCX=1)
    IF(.NOT.(r .APPROXEQ. 14.0_SRK)) THEN
      WRITE(*,*) "CALL BLAS_asum(THISVECTOR=xVector,N=xVector%n,INCX=1) FAILED!"
      STOP 666
    ENDIF
    r=0.0_SRK
    r = BLAS_asum(THISVECTOR=xVector,INCX=1)
    IF(.NOT.(r .APPROXEQ. 14.0_SRK)) THEN
      WRITE(*,*) "CALL BLAS_asum(THISVECTOR=xVector,INCX=1) FAILED!"
      STOP 666
    ENDIF
    r=0.0_SRK
    r = BLAS_asum(THISVECTOR=xVector,N=xVector%n)
    IF(.NOT.(r .APPROXEQ. 14.0_SRK)) THEN
      WRITE(*,*) "CALL BLAS_asum(THISVECTOR=xVector,N=xVector%n) FAILED!"
      STOP 666
    ENDIF
    r=0.0_SRK
    r = BLAS_asum(THISVECTOR=xVector)
    IF(.NOT.(r .APPROXEQ. 14.0_SRK)) THEN
      WRITE(*,*) "CALL BLAS_asum(THISVECTOR=xVector) FAILED!"
      STOP 666
    ENDIF
    WRITE(*,*) '  Passed: CALL BLAS_asum(...)'
    
    !Test BLAS_axpy (y=y+ax) [scalar a]
    a = 5
    CALL xVector%set(1, 5.0_SRK)
    CALL xVector%set(2,-2.0_SRK)
    CALL xVector%set(3, 7.0_SRK)
    CALL yVector%set(1, 3.0_SRK)
    CALL yVector%set(2, 5.0_SRK)
    CALL yVector%set(3, 1.0_SRK)
    CALL BLAS_axpy(THISVECTOR=xVector,NEWVECTOR=yVector,A=a,N=xVector%n,INCX=1,INCY=1)
    IF((.NOT.((yVector%get(1) .APPROXEQ. 28._SRK) .AND. &
              (yVector%get(2) .APPROXEQ. -5._SRK) .AND. &
              (yVector%get(3) .APPROXEQ. 36._SRK)))) THEN
      WRITE(*,*) "CALL BLAS_axpy(THISVECTOR=xVector,NEWVECTOR=yVector,A=a,N=xVector%n,INCX=1,INCY=1) -scalar_a FAILED!"
      STOP 666
    ENDIF
    CALL yVector%set(1,3.0_SRK)
    CALL yVector%set(2,5.0_SRK)
    CALL yVector%set(3,1.0_SRK)
    CALL BLAS_axpy(THISVECTOR=xVector,NEWVECTOR=yVector,A=a,N=xVector%n,INCX=1)
    IF((.NOT.((yVector%get(1) .APPROXEQ. 28._SRK) .AND. &
              (yVector%get(2) .APPROXEQ. -5._SRK) .AND. &
              (yVector%get(3) .APPROXEQ. 36._SRK)))) THEN
      WRITE(*,*) "CALL BLAS_axpy(THISVECTOR=xVector,NEWVECTOR=yVector,A=a,N=xVector%n,INCX=1) -scalar_a FAILED!"
      STOP 666
    ENDIF
    CALL yVector%set(1,3.0_SRK)
    CALL yVector%set(2,5.0_SRK)
    CALL yVector%set(3,1.0_SRK)
    CALL BLAS_axpy(THISVECTOR=xVector,NEWVECTOR=yVector,A=a,N=xVector%n,INCY=1)
    IF((.NOT.((yVector%get(1) .APPROXEQ. 28._SRK) .AND. &
              (yVector%get(2) .APPROXEQ. -5._SRK) .AND. &
              (yVector%get(3) .APPROXEQ. 36._SRK)))) THEN
      WRITE(*,*) "CALL BLAS_axpy(THISVECTOR=xVector,NEWVECTOR=yVector,A=a,N=xVector%n,INCY=1) -scalar_a FAILED!"
      STOP 666
    ENDIF
    CALL yVector%set(1,3.0_SRK)
    CALL yVector%set(2,5.0_SRK)
    CALL yVector%set(3,1.0_SRK)
    CALL BLAS_axpy(THISVECTOR=xVector,NEWVECTOR=yVector,A=a,N=xVector%n)
    IF((.NOT.((yVector%get(1) .APPROXEQ. 28._SRK) .AND. &
              (yVector%get(2) .APPROXEQ. -5._SRK) .AND. &
              (yVector%get(3) .APPROXEQ. 36._SRK)))) THEN
      WRITE(*,*) "CALL BLAS_axpy(THISVECTOR=xVector,NEWVECTOR=yVector,A=a,N=xVector%n) -scalar_a FAILED!"
      STOP 666
    ENDIF
    CALL yVector%set(1,3.0_SRK)
    CALL yVector%set(2,5.0_SRK)
    CALL yVector%set(3,1.0_SRK)
    CALL BLAS_axpy(THISVECTOR=xVector,NEWVECTOR=yVector,A=a)
    IF((.NOT.((yVector%get(1) .APPROXEQ. 28._SRK) .AND. &
              (yVector%get(2) .APPROXEQ. -5._SRK) .AND. &
              (yVector%get(3) .APPROXEQ. 36._SRK)))) THEN
      WRITE(*,*) "CALL BLAS_axpy(THISVECTOR=xVector,NEWVECTOR=yVector,A=a) -scalar_a FAILED!"
      STOP 666
    ENDIF
    WRITE(*,*) '  Passed: CALL BLAS_axpy(...) -scalar_a'
    
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
    IF((.NOT.((yVector%get(1) .APPROXEQ. 33._SRK) .AND. &
              (yVector%get(2) .APPROXEQ. -3._SRK) .AND. &
              (yVector%get(3) .APPROXEQ. 15._SRK)))) THEN
      WRITE(*,*) "CALL BLAS_axpy(THISVECTOR=xVector,NEWVECTOR=yVector,AVECTOR=aVector,N=xVector%n,INCX=1,INCY=1) -vector_a FAILED!"
      STOP 666
    ENDIF
    CALL yVector%set(1,3.0_SRK)
    CALL yVector%set(2,5.0_SRK)
    CALL yVector%set(3,1.0_SRK)
    CALL BLAS_axpy(THISVECTOR=xVector,NEWVECTOR=yVector,AVECTOR=aVector,N=xVector%n,INCX=1)
    IF((.NOT.((yVector%get(1) .APPROXEQ. 33._SRK) .AND. &
              (yVector%get(2) .APPROXEQ. -3._SRK) .AND. &
              (yVector%get(3) .APPROXEQ. 15._SRK)))) THEN
      WRITE(*,*) "CALL BLAS_axpy(THISVECTOR=xVector,NEWVECTOR=yVector,AVECTOR=aVector,N=xVector%n,INCX=1) -vector_a FAILED!"
      STOP 666
    ENDIF
    CALL yVector%set(1,3.0_SRK)
    CALL yVector%set(2,5.0_SRK)
    CALL yVector%set(3,1.0_SRK)
    CALL BLAS_axpy(THISVECTOR=xVector,NEWVECTOR=yVector,AVECTOR=aVector,N=xVector%n,INCY=1)
    IF((.NOT.((yVector%get(1) .APPROXEQ. 33._SRK) .AND. &
              (yVector%get(2) .APPROXEQ. -3._SRK) .AND. &
              (yVector%get(3) .APPROXEQ. 15._SRK)))) THEN
      WRITE(*,*) "CALL BLAS_axpy(THISVECTOR=xVector,NEWVECTOR=yVector,AVECTOR=aVector,N=xVector%n,INCY=1) -vector_a FAILED!"
      STOP 666
    ENDIF
    CALL yVector%set(1,3.0_SRK)
    CALL yVector%set(2,5.0_SRK)
    CALL yVector%set(3,1.0_SRK)
    CALL BLAS_axpy(THISVECTOR=xVector,NEWVECTOR=yVector,AVECTOR=aVector,N=xVector%n)
    IF((.NOT.((yVector%get(1) .APPROXEQ. 33._SRK) .AND. &
              (yVector%get(2) .APPROXEQ. -3._SRK) .AND. &
              (yVector%get(3) .APPROXEQ. 15._SRK)))) THEN
      WRITE(*,*) "CALL BLAS_axpy(THISVECTOR=xVector,NEWVECTOR=yVector,AVECTOR=aVector,N=xVector%n) -vector_a FAILED!"
      STOP 666
    ENDIF
    CALL yVector%set(1,3.0_SRK)
    CALL yVector%set(2,5.0_SRK)
    CALL yVector%set(3,1.0_SRK)
    CALL BLAS_axpy(THISVECTOR=xVector,NEWVECTOR=yVector,AVECTOR=aVector)
    IF((.NOT.((yVector%get(1) .APPROXEQ. 33._SRK) .AND. &
              (yVector%get(2) .APPROXEQ. -3._SRK) .AND. &
              (yVector%get(3) .APPROXEQ. 15._SRK)))) THEN
      WRITE(*,*) "CALL BLAS_axpy(THISVECTOR=xVector,NEWVECTOR=yVector,AVECTOR=aVector) -vector_a FAILED!"
      STOP 666
    ENDIF
    WRITE(*,*) '  Passed: CALL BLAS_axpy(...) -vector_a'
    
    !Test BLAS_copy
    CALL xVector%set(1,1.0_SRK)
    CALL xVector%set(2,3.0_SRK)
    CALL xVector%set(3,7.0_SRK)
    CALL yVector%set(0.0_SRK)
    CALL BLAS_copy(THISVECTOR=xVector,NEWVECTOR=yVector,N=xVector%n,INCX=1,INCY=1)
    IF((.NOT.((yVector%get(1) .APPROXEQ. 1.0_SRK) .AND. &
              (yVector%get(2) .APPROXEQ. 3.0_SRK) .AND. &
              (yVector%get(3) .APPROXEQ. 7.0_SRK)))) THEN
      WRITE(*,*) "CALL BLAS_copy(THISVECTOR=xVector,NEWVECTOR=yVector,N=xVector%n,INCX=1,INCY=1) FAILED!"
      STOP 666
    ENDIF
    CALL yVector%set(0.0_SRK)
    CALL BLAS_copy(THISVECTOR=xVector,NEWVECTOR=yVector,INCX=1,INCY=1)
    IF((.NOT.((yVector%get(1) .APPROXEQ. 1.0_SRK) .AND. &
              (yVector%get(2) .APPROXEQ. 3.0_SRK) .AND. &
              (yVector%get(3) .APPROXEQ. 7.0_SRK)))) THEN
      WRITE(*,*) "CALL BLAS_copy(THISVECTOR=xVector,NEWVECTOR=yVector,INCX=1,INCY=1) FAILED!"
      STOP 666
    ENDIF
    CALL yVector%set(0.0_SRK)
    CALL BLAS_copy(THISVECTOR=xVector,NEWVECTOR=yVector,N=xVector%n,INCX=1)
    IF((.NOT.((yVector%get(1) .APPROXEQ. 1.0_SRK) .AND. &
              (yVector%get(2) .APPROXEQ. 3.0_SRK) .AND. &
              (yVector%get(3) .APPROXEQ. 7.0_SRK)))) THEN
      WRITE(*,*) "CALL BLAS_copy(THISVECTOR=xVector,NEWVECTOR=yVector,N=xVector%n,INCX=1) FAILED!"
      STOP 666
    ENDIF
    CALL yVector%set(0.0_SRK)
    CALL BLAS_copy(THISVECTOR=xVector,NEWVECTOR=yVector,N=xVector%n,INCY=1)
    IF((.NOT.((yVector%get(1) .APPROXEQ. 1.0_SRK) .AND. &
              (yVector%get(2) .APPROXEQ. 3.0_SRK) .AND. &
              (yVector%get(3) .APPROXEQ. 7.0_SRK)))) THEN
      WRITE(*,*) "CALL BLAS_copy(THISVECTOR=xVector,NEWVECTOR=yVector,N=xVector%n,INCY=1) FAILED!"
      STOP 666
    ENDIF
    CALL yVector%set(0.0_SRK)
    CALL BLAS_copy(THISVECTOR=xVector,NEWVECTOR=yVector,INCX=1)
    IF((.NOT.((yVector%get(1) .APPROXEQ. 1.0_SRK) .AND. &
              (yVector%get(2) .APPROXEQ. 3.0_SRK) .AND. &
              (yVector%get(3) .APPROXEQ. 7.0_SRK)))) THEN
      WRITE(*,*) "CALL BLAS_copy(THISVECTOR=xVector,NEWVECTOR=yVector,INCX=1) FAILED!"
      STOP 666
    ENDIF
    CALL yVector%set(0.0_SRK)
    CALL BLAS_copy(THISVECTOR=xVector,NEWVECTOR=yVector,INCY=1)
    IF((.NOT.((yVector%get(1) .APPROXEQ. 1.0_SRK) .AND. &
              (yVector%get(2) .APPROXEQ. 3.0_SRK) .AND. &
              (yVector%get(3) .APPROXEQ. 7.0_SRK)))) THEN
      WRITE(*,*) "CALL BLAS_copy(THISVECTOR=xVector,NEWVECTOR=yVector,INCY=1) FAILED!"
      STOP 666
    ENDIF
    CALL yVector%set(0.0_SRK)
    CALL BLAS_copy(THISVECTOR=xVector,NEWVECTOR=yVector,N=xVector%n)
    IF((.NOT.((yVector%get(1) .APPROXEQ. 1.0_SRK) .AND. &
              (yVector%get(2) .APPROXEQ. 3.0_SRK) .AND. &
              (yVector%get(3) .APPROXEQ. 7.0_SRK)))) THEN
      WRITE(*,*) "CALL BLAS_copy(THISVECTOR=xVector,NEWVECTOR=yVector,N=xVector%n) FAILED!"
      STOP 666
    ENDIF
    CALL yVector%set(0.0_SRK)
    CALL BLAS_copy(THISVECTOR=xVector,NEWVECTOR=yVector)
    IF((.NOT.((yVector%get(1) .APPROXEQ. 1.0_SRK) .AND. &
              (yVector%get(2) .APPROXEQ. 3.0_SRK) .AND. &
              (yVector%get(3) .APPROXEQ. 7.0_SRK)))) THEN
      WRITE(*,*) "CALL BLAS_copy(THISVECTOR=xVector,NEWVECTOR=yVector) FAILED!"
      STOP 666
    ENDIF
    WRITE(*,*) '  Passed: CALL BLAS_copy(...)'
    
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
      WRITE(*,*) "CALL BLAS_dot(THISVECTOR=xVector,THATVECTOR=yVector,N=xVector%n,INCX=1,INCY=1) FAILED!"
      STOP 666
    ENDIF
    r=0.0_SRK
    r = BLAS_dot(THISVECTOR=xVector,THATVECTOR=yVector,INCX=1,INCY=1)
    IF(.NOT.(r .APPROXEQ. 56.0_SRK)) THEN
      WRITE(*,*) "CALL BLAS_dot(THISVECTOR=xVector,THATVECTOR=yVector,INCX=1,INCY=1) FAILED!"
      STOP 666
    ENDIF
    r=0.0_SRK
    r = BLAS_dot(THISVECTOR=xVector,THATVECTOR=yVector,N=xVector%n,INCX=1)
    IF(.NOT.(r .APPROXEQ. 56.0_SRK)) THEN
      WRITE(*,*) "CALL BLAS_dot(THISVECTOR=xVector,THATVECTOR=yVector,N=xVector%n,INCX=1) FAILED!"
      STOP 666
    ENDIF
    r=0.0_SRK
    r = BLAS_dot(THISVECTOR=xVector,THATVECTOR=yVector,N=xVector%n,INCY=1)
    IF(.NOT.(r .APPROXEQ. 56.0_SRK)) THEN
      WRITE(*,*) "CALL BLAS_dot(THISVECTOR=xVector,THATVECTOR=yVector,N=xVector%n,INCY=1) FAILED!"
      STOP 666
    ENDIF
    r=0.0_SRK
    r = BLAS_dot(THISVECTOR=xVector,THATVECTOR=yVector,N=xVector%n)
    IF(.NOT.(r .APPROXEQ. 56.0_SRK)) THEN
      WRITE(*,*) "CALL BLAS_dot(THISVECTOR=xVector,THATVECTOR=yVector,N=xVector%n) FAILED!"
      STOP 666
    ENDIF
    r=0.0_SRK
    r = BLAS_dot(THISVECTOR=xVector,THATVECTOR=yVector,INCX=1)
    IF(.NOT.(r .APPROXEQ. 56.0_SRK)) THEN
      WRITE(*,*) "CALL BLAS_dot(THISVECTOR=xVector,THATVECTOR=yVector,INCX=1) FAILED!"
      STOP 666
    ENDIF
    r=0.0_SRK
    r = BLAS_dot(THISVECTOR=xVector,THATVECTOR=yVector,INCY=1)
    IF(.NOT.(r .APPROXEQ. 56.0_SRK)) THEN
      WRITE(*,*) "CALL BLAS_dot(THISVECTOR=xVector,THATVECTOR=yVector,INCY=1) FAILED!"
      STOP 666
    ENDIF
    r=0.0_SRK
    r = BLAS_dot(THISVECTOR=xVector,THATVECTOR=yVector)
    IF(.NOT.(r .APPROXEQ. 56.0_SRK)) THEN
      WRITE(*,*) "CALL BLAS_dot(THISVECTOR=xVector,THATVECTOR=yVector) FAILED!"
      STOP 666
    ENDIF
    WRITE(*,*) '  Passed: CALL BLAS_dot(...)'
    
    !Test BLAS_iamax
    ! first test with all positive values
    CALL xVector%set(1,12.0_SRK)
    CALL xVector%set(2, 3.0_SRK)
    CALL xVector%set(3,17.0_SRK)
    r_index = 0_SIK
    r_index = BLAS_iamax(THISVECTOR=xVector,N=xVector%n,INCX=1)
    IF(r_index /= 3_SIK) THEN
      WRITE(*,*) "CALL BLAS_iamax(THISVECTOR=xVector,N=xVector%n,INCX=1) FAILED!"
      STOP 666
    ENDIF
    r_index = 0_SIK
    r_index = BLAS_iamax(THISVECTOR=xVector,INCX=1)
    IF(r_index /= 3_SIK) THEN
      WRITE(*,*) "CALL BLAS_iamax(THISVECTOR=xVector,INCX=1) FAILED!"
      STOP 666
    ENDIF
    r_index = 0_SIK
    r_index = BLAS_iamax(THISVECTOR=xVector,N=xVector%n)
    IF(r_index /= 3_SIK) THEN
      WRITE(*,*) "CALL BLAS_iamax(THISVECTOR=xVector,N=xVector%n) FAILED!"
      STOP 666
    ENDIF
    r_index = 0_SIK
    r_index = BLAS_iamax(THISVECTOR=xVector)
    IF(r_index /= 3_SIK) THEN
      WRITE(*,*) "CALL BLAS_iamax(THISVECTOR=xVector) FAILED!"
      STOP 666
    ENDIF
    ! next test with a negative value
    CALL xVector%set(1, 12.0_SRK)
    CALL xVector%set(2,-30.0_SRK)
    CALL xVector%set(3, 17.0_SRK)
    r_index = 0_SIK
    r_index = BLAS_iamax(THISVECTOR=xVector,N=xVector%n,INCX=1)
    IF(r_index /= 2_SIK) THEN
      WRITE(*,*) "CALL BLAS_iamax(THISVECTOR=xVector,N=xVector%n,INCX=1) FAILED!"
      STOP 666
    ENDIF
    r_index = 0_SIK
    r_index = BLAS_iamax(THISVECTOR=xVector,INCX=1)
    IF(r_index /= 2_SIK) THEN
      WRITE(*,*) "CALL BLAS_iamax(THISVECTOR=xVector,INCX=1) FAILED!"
      STOP 666
    ENDIF
    r_index = 0_SIK
    r_index = BLAS_iamax(THISVECTOR=xVector,N=xVector%n)
    IF(r_index /= 2_SIK) THEN
      WRITE(*,*) "CALL BLAS_iamax(THISVECTOR=xVector,N=xVector%n) FAILED!"
      STOP 666
    ENDIF
    r_index = 0_SIK
    r_index = BLAS_iamax(THISVECTOR=xVector)
    IF(r_index /= 2_SIK) THEN
      WRITE(*,*) "CALL BLAS_iamax(THISVECTOR=xVector) FAILED!"
      STOP 666
    ENDIF
    WRITE(*,*) '  Passed: CALL BLAS_iamax(...)'
    
    !Test BLAS_iamin
    ! first test with all positive values
    CALL xVector%set(1,12.0_SRK)
    CALL xVector%set(2, 3.0_SRK)
    CALL xVector%set(3,17.0_SRK)
    r_index = 0_SIK
    r_index = BLAS_iamin(THISVECTOR=xVector,N=xVector%n,INCX=1)
    IF(r_index /= 2_SIK) THEN
      WRITE(*,*) "CALL BLAS_iamin(THISVECTOR=xVector,N=xVector%n,INCX=1) FAILED!"
      STOP 666
    ENDIF
    r_index = 0_SIK
    r_index = BLAS_iamin(THISVECTOR=xVector,INCX=1)
    IF(r_index /= 2_SIK) THEN
      WRITE(*,*) "CALL BLAS_iamin(THISVECTOR=xVector,INCX=1) FAILED!"
      STOP 666
    ENDIF
    r_index = 0_SIK
    r_index = BLAS_iamin(THISVECTOR=xVector,N=xVector%n)
    IF(r_index /= 2_SIK) THEN
      WRITE(*,*) "CALL BLAS_iamin(THISVECTOR=xVector,N=xVector%n) FAILED!"
      STOP 666
    ENDIF
    r_index = 0_SIK
    r_index = BLAS_iamin(THISVECTOR=xVector)
    IF(r_index /= 2_SIK) THEN
      WRITE(*,*) "CALL BLAS_iamin(THISVECTOR=xVector) FAILED!"
      STOP 666
    ENDIF
    ! next test with a negative value
    CALL xVector%set(1, 12.0_SRK)
    CALL xVector%set(2,-30.0_SRK)
    CALL xVector%set(3, 17.0_SRK)
    r_index = 0_SIK
    r_index = BLAS_iamin(THISVECTOR=xVector,N=xVector%n,INCX=1)
    IF(r_index /= 1_SIK) THEN
      WRITE(*,*) "CALL BLAS_iamin(THISVECTOR=xVector,N=xVector%n,INCX=1) FAILED!"
      STOP 666
    ENDIF
    r_index = 0_SIK
    r_index = BLAS_iamin(THISVECTOR=xVector,INCX=1)
    IF(r_index /= 1_SIK) THEN
      WRITE(*,*) "CALL BLAS_iamin(THISVECTOR=xVector,INCX=1) FAILED!"
      STOP 666
    ENDIF
    r_index = 0_SIK
    r_index = BLAS_iamin(THISVECTOR=xVector,N=xVector%n)
    IF(r_index /= 1_SIK) THEN
      WRITE(*,*) "CALL BLAS_iamin(THISVECTOR=xVector,N=xVector%n) FAILED!"
      STOP 666
    ENDIF
    r_index = 0_SIK
    r_index = BLAS_iamin(THISVECTOR=xVector)
    IF(r_index /= 1_SIK) THEN
      WRITE(*,*) "CALL BLAS_iamin(THISVECTOR=xVector) FAILED!"
      STOP 666
    ENDIF
    WRITE(*,*) '  Passed: CALL BLAS_iamin(...)'
    
    !Test BLAS_nrm2
    CALL xVector%set(1,0.0_SRK)
    CALL xVector%set(2,3.0_SRK)
    CALL xVector%set(3,4.0_SRK)
    r = 0.0_SRK
    r = BLAS_nrm2(THISVECTOR=xVector,N=xVector%n,INCX=1)
    IF(.NOT.(r .APPROXEQ. 5._SRK)) THEN
      WRITE(*,*) "CALL BLAS_nrm2(THISVECTOR=xVector,N=xVector%n,INCX=1) FAILED!"
      STOP 666
    ENDIF
    r = 0.0_SRK
    r = BLAS_nrm2(THISVECTOR=xVector,INCX=1)
    IF(.NOT.(r .APPROXEQ. 5._SRK)) THEN
      WRITE(*,*) "CALL BLAS_nrm2(THISVECTOR=xVector,INCX=1) FAILED!"
      STOP 666
    ENDIF
    r = 0.0_SRK
    r = BLAS_nrm2(THISVECTOR=xVector,N=xVector%n)
    IF(.NOT.(r .APPROXEQ. 5._SRK)) THEN
      WRITE(*,*) "CALL BLAS_nrm2(THISVECTOR=xVector,N=xVector%n) FAILED!"
      STOP 666
    ENDIF
    r = 0.0_SRK
    r = BLAS_nrm2(THISVECTOR=xVector)
    IF(.NOT.(r .APPROXEQ. 5._SRK)) THEN
      WRITE(*,*) "CALL BLAS_nrm2(THISVECTOR=xVector) FAILED!"
      STOP 666
    ENDIF
    WRITE(*,*) '  Passed: CALL BLAS_nrm2(...)'
    
    !Test BLAS_scal [scalar a]
    a = 3
    CALL xVector%set(1,1.0_SRK)
    CALL xVector%set(2,3.0_SRK)
    CALL xVector%set(3,5.0_SRK)
    CALL BLAS_scal(THISVECTOR=xVector,A=a,N=xVector%n,INCX=1)
    IF((.NOT.((xVector%get(1) .APPROXEQ.  3._SRK) .AND. &
              (xVector%get(2) .APPROXEQ.  9._SRK) .AND. &
              (xVector%get(3) .APPROXEQ. 15._SRK)))) THEN
      WRITE(*,*) "CALL BLAS_scal(THISVECTOR=xVector,A=a,N=xVector%n,INCX=1) -scalar_a FAILED!"
      STOP 666
    ENDIF
    CALL xVector%set(1,1.0_SRK)
    CALL xVector%set(2,3.0_SRK)
    CALL xVector%set(3,5.0_SRK)
    CALL BLAS_scal(THISVECTOR=xVector,A=a,INCX=1)
    IF((.NOT.((xVector%get(1) .APPROXEQ.  3._SRK) .AND. &
              (xVector%get(2) .APPROXEQ.  9._SRK) .AND. &
              (xVector%get(3) .APPROXEQ. 15._SRK)))) THEN
      WRITE(*,*) "CALL BLAS_scal(THISVECTOR=xVector,A=a,INCX=1) -scalar_a FAILED!"
      STOP 666
    ENDIF
    CALL xVector%set(1,1.0_SRK)
    CALL xVector%set(2,3.0_SRK)
    CALL xVector%set(3,5.0_SRK)
    CALL BLAS_scal(THISVECTOR=xVector,A=a,N=xVector%n)
    IF((.NOT.((xVector%get(1) .APPROXEQ.  3._SRK) .AND. &
              (xVector%get(2) .APPROXEQ.  9._SRK) .AND. &
              (xVector%get(3) .APPROXEQ. 15._SRK)))) THEN
      WRITE(*,*) "CALL BLAS_scal(THISVECTOR=xVector,A=a,N=xVector%n) -scalar_a FAILED!"
      STOP 666
    ENDIF
    CALL xVector%set(1,1.0_SRK)
    CALL xVector%set(2,3.0_SRK)
    CALL xVector%set(3,5.0_SRK)
    CALL BLAS_scal(THISVECTOR=xVector,A=a)
    IF((.NOT.((xVector%get(1) .APPROXEQ.  3._SRK) .AND. &
              (xVector%get(2) .APPROXEQ.  9._SRK) .AND. &
              (xVector%get(3) .APPROXEQ. 15._SRK)))) THEN
      WRITE(*,*) "CALL BLAS_scal(THISVECTOR=xVector,A=a) -scalar_a FAILED!"
      STOP 666
    ENDIF
    WRITE(*,*) '  Passed: CALL BLAS_scal(...) -scalar_a'
    
    !Test BLAS_scal [vector a]
    CALL aVector%set(1,2.0_SRK)
    CALL aVector%set(2,4.0_SRK)
    CALL aVector%set(3,1.0_SRK)
    CALL xVector%set(1,1.0_SRK)
    CALL xVector%set(2,3.0_SRK)
    CALL xVector%set(3,5.0_SRK)
    CALL BLAS_scal(THISVECTOR=xVector,AVECTOR=aVector,N=xVector%n,INCX=1)
    IF((.NOT.((xVector%get(1) .APPROXEQ.  2._SRK) .AND. &
              (xVector%get(2) .APPROXEQ. 12._SRK) .AND. &
              (xVector%get(3) .APPROXEQ.  5._SRK)))) THEN
      WRITE(*,*) "CALL BLAS_scal(THISVECTOR=xVector,AVECTOR=aVector,N=xVector%n,INCX=1) -vector_a FAILED!"
      WRITE(*,*) xVector%get(1),xVector%get(2),xVector%get(3)
      STOP 666
    ENDIF
    CALL xVector%set(1,1.0_SRK)
    CALL xVector%set(2,3.0_SRK)
    CALL xVector%set(3,5.0_SRK)
    CALL BLAS_scal(THISVECTOR=xVector,AVECTOR=aVector,INCX=1)
    IF((.NOT.((xVector%get(1) .APPROXEQ.  2._SRK) .AND. &
              (xVector%get(2) .APPROXEQ. 12._SRK) .AND. &
              (xVector%get(3) .APPROXEQ.  5._SRK)))) THEN
      WRITE(*,*) "CALL BLAS_scal(THISVECTOR=xVector,AVECTOR=aVector,INCX=1) -vector_a FAILED!"
      STOP 666
    ENDIF
    CALL xVector%set(1,1.0_SRK)
    CALL xVector%set(2,3.0_SRK)
    CALL xVector%set(3,5.0_SRK)
    CALL BLAS_scal(THISVECTOR=xVector,AVECTOR=aVector,N=xVector%n)
    IF((.NOT.((xVector%get(1) .APPROXEQ.  2._SRK) .AND. &
              (xVector%get(2) .APPROXEQ. 12._SRK) .AND. &
              (xVector%get(3) .APPROXEQ.  5._SRK)))) THEN
      WRITE(*,*) "CALL BLAS_scal(THISVECTOR=xVector,AVECTOR=aVector,N=xVector%n) -vector_a FAILED!"
      STOP 666
    ENDIF
    CALL xVector%set(1,1.0_SRK)
    CALL xVector%set(2,3.0_SRK)
    CALL xVector%set(3,5.0_SRK)
    CALL BLAS_scal(THISVECTOR=xVector,AVECTOR=aVector)
    IF((.NOT.((xVector%get(1) .APPROXEQ.  2._SRK) .AND. &
              (xVector%get(2) .APPROXEQ. 12._SRK) .AND. &
              (xVector%get(3) .APPROXEQ.  5._SRK)))) THEN
      WRITE(*,*) "CALL BLAS_scal(THISVECTOR=xVector,AVECTOR=aVector) -vector_a FAILED!"
      STOP 666
    ENDIF
    WRITE(*,*) '  Passed: CALL BLAS_scal(...) -vector_a'
    
    !Test BLAS_swap
    CALL xVector%set(1,1.0_SRK)
    CALL xVector%set(2,3.0_SRK)
    CALL xVector%set(3,5.0_SRK)
    CALL yVector%set(1,2.0_SRK)
    CALL yVector%set(2,4.0_SRK)
    CALL yVector%set(3,6.0_SRK)
    CALL BLAS_swap(THISVECTOR=xVector,THATVECTOR=yVector,N=xVector%n,INCX=1,INCY=1)
    IF((.NOT.((xVector%get(1) .APPROXEQ. 2._SRK) .AND. &
              (xVector%get(2) .APPROXEQ. 4._SRK) .AND. &
              (xVector%get(3) .APPROXEQ. 6._SRK) .AND. &
              (yVector%get(1) .APPROXEQ. 1._SRK) .AND. &
              (yVector%get(2) .APPROXEQ. 3._SRK) .AND. &
              (yVector%get(3) .APPROXEQ. 5._SRK)))) THEN
      WRITE(*,*) "CALL BLAS_swap(THISVECTOR=xVector,THATVECTOR=yVector,N=xVector%n,INCX=1,INCY=1) FAILED!"
      STOP 666
    ENDIF
    CALL xVector%set(1,1.0_SRK)
    CALL xVector%set(2,3.0_SRK)
    CALL xVector%set(3,5.0_SRK)
    CALL yVector%set(1,2.0_SRK)
    CALL yVector%set(2,4.0_SRK)
    CALL yVector%set(3,6.0_SRK)
    CALL BLAS_swap(THISVECTOR=xVector,THATVECTOR=yVector,INCX=1,INCY=1)
    IF((.NOT.((xVector%get(1) .APPROXEQ. 2._SRK) .AND. &
              (xVector%get(2) .APPROXEQ. 4._SRK) .AND. &
              (xVector%get(3) .APPROXEQ. 6._SRK) .AND. &
              (yVector%get(1) .APPROXEQ. 1._SRK) .AND. &
              (yVector%get(2) .APPROXEQ. 3._SRK) .AND. &
              (yVector%get(3) .APPROXEQ. 5._SRK)))) THEN
      WRITE(*,*) "CALL BLAS_swap(THISVECTOR=xVector,THATVECTOR=yVector,INCX=1,INCY=1) FAILED!"
      STOP 666
    ENDIF
    CALL xVector%set(1,1.0_SRK)
    CALL xVector%set(2,3.0_SRK)
    CALL xVector%set(3,5.0_SRK)
    CALL yVector%set(1,2.0_SRK)
    CALL yVector%set(2,4.0_SRK)
    CALL yVector%set(3,6.0_SRK)
    CALL BLAS_swap(THISVECTOR=xVector,THATVECTOR=yVector,N=xVector%n,INCX=1)
    IF((.NOT.((xVector%get(1) .APPROXEQ. 2._SRK) .AND. &
              (xVector%get(2) .APPROXEQ. 4._SRK) .AND. &
              (xVector%get(3) .APPROXEQ. 6._SRK) .AND. &
              (yVector%get(1) .APPROXEQ. 1._SRK) .AND. &
              (yVector%get(2) .APPROXEQ. 3._SRK) .AND. &
              (yVector%get(3) .APPROXEQ. 5._SRK)))) THEN
      WRITE(*,*) "CALL BLAS_swap(THISVECTOR=xVector,THATVECTOR=yVector,N=xVector%n,INCX=1) FAILED!"
      STOP 666
    ENDIF
    CALL xVector%set(1,1.0_SRK)
    CALL xVector%set(2,3.0_SRK)
    CALL xVector%set(3,5.0_SRK)
    CALL yVector%set(1,2.0_SRK)
    CALL yVector%set(2,4.0_SRK)
    CALL yVector%set(3,6.0_SRK)
    CALL BLAS_swap(THISVECTOR=xVector,THATVECTOR=yVector,N=xVector%n,INCY=1)
    IF((.NOT.((xVector%get(1) .APPROXEQ. 2._SRK) .AND. &
              (xVector%get(2) .APPROXEQ. 4._SRK) .AND. &
              (xVector%get(3) .APPROXEQ. 6._SRK) .AND. &
              (yVector%get(1) .APPROXEQ. 1._SRK) .AND. &
              (yVector%get(2) .APPROXEQ. 3._SRK) .AND. &
              (yVector%get(3) .APPROXEQ. 5._SRK)))) THEN
      WRITE(*,*) "CALL BLAS_swap(THISVECTOR=xVector,THATVECTOR=yVector,N=xVector%n,INCY=1) FAILED!"
      STOP 666
    ENDIF
    CALL xVector%set(1,1.0_SRK)
    CALL xVector%set(2,3.0_SRK)
    CALL xVector%set(3,5.0_SRK)
    CALL yVector%set(1,2.0_SRK)
    CALL yVector%set(2,4.0_SRK)
    CALL yVector%set(3,6.0_SRK)
    CALL BLAS_swap(THISVECTOR=xVector,THATVECTOR=yVector,INCX=1)
    IF((.NOT.((xVector%get(1) .APPROXEQ. 2._SRK) .AND. &
              (xVector%get(2) .APPROXEQ. 4._SRK) .AND. &
              (xVector%get(3) .APPROXEQ. 6._SRK) .AND. &
              (yVector%get(1) .APPROXEQ. 1._SRK) .AND. &
              (yVector%get(2) .APPROXEQ. 3._SRK) .AND. &
              (yVector%get(3) .APPROXEQ. 5._SRK)))) THEN
      WRITE(*,*) "CALL BLAS_swap(THISVECTOR=xVector,THATVECTOR=yVector,INCX=1) FAILED!"
      STOP 666
    ENDIF
    CALL xVector%set(1,1.0_SRK)
    CALL xVector%set(2,3.0_SRK)
    CALL xVector%set(3,5.0_SRK)
    CALL yVector%set(1,2.0_SRK)
    CALL yVector%set(2,4.0_SRK)
    CALL yVector%set(3,6.0_SRK)
    CALL BLAS_swap(THISVECTOR=xVector,THATVECTOR=yVector,INCY=1)
    IF((.NOT.((xVector%get(1) .APPROXEQ. 2._SRK) .AND. &
              (xVector%get(2) .APPROXEQ. 4._SRK) .AND. &
              (xVector%get(3) .APPROXEQ. 6._SRK) .AND. &
              (yVector%get(1) .APPROXEQ. 1._SRK) .AND. &
              (yVector%get(2) .APPROXEQ. 3._SRK) .AND. &
              (yVector%get(3) .APPROXEQ. 5._SRK)))) THEN
      WRITE(*,*) "CALL BLAS_swap(THISVECTOR=xVector,THATVECTOR=yVector,INCY=1) FAILED!"
      STOP 666
    ENDIF
    CALL xVector%set(1,1.0_SRK)
    CALL xVector%set(2,3.0_SRK)
    CALL xVector%set(3,5.0_SRK)
    CALL yVector%set(1,2.0_SRK)
    CALL yVector%set(2,4.0_SRK)
    CALL yVector%set(3,6.0_SRK)
    CALL BLAS_swap(THISVECTOR=xVector,THATVECTOR=yVector,N=xVector%n)
    IF((.NOT.((xVector%get(1) .APPROXEQ. 2._SRK) .AND. &
              (xVector%get(2) .APPROXEQ. 4._SRK) .AND. &
              (xVector%get(3) .APPROXEQ. 6._SRK) .AND. &
              (yVector%get(1) .APPROXEQ. 1._SRK) .AND. &
              (yVector%get(2) .APPROXEQ. 3._SRK) .AND. &
              (yVector%get(3) .APPROXEQ. 5._SRK)))) THEN
      WRITE(*,*) "CALL BLAS_swap(THISVECTOR=xVector,THATVECTOR=yVector,N=xVector%n) FAILED!"
      STOP 666
    ENDIF
    CALL xVector%set(1,1.0_SRK)
    CALL xVector%set(2,3.0_SRK)
    CALL xVector%set(3,5.0_SRK)
    CALL yVector%set(1,2.0_SRK)
    CALL yVector%set(2,4.0_SRK)
    CALL yVector%set(3,6.0_SRK)
    CALL BLAS_swap(THISVECTOR=xVector,THATVECTOR=yVector)
    IF((.NOT.((xVector%get(1) .APPROXEQ. 2._SRK) .AND. &
              (xVector%get(2) .APPROXEQ. 4._SRK) .AND. &
              (xVector%get(3) .APPROXEQ. 6._SRK) .AND. &
              (yVector%get(1) .APPROXEQ. 1._SRK) .AND. &
              (yVector%get(2) .APPROXEQ. 3._SRK) .AND. &
              (yVector%get(3) .APPROXEQ. 5._SRK)))) THEN
      WRITE(*,*) "CALL BLAS_swap(THISVECTOR=xVector,THATVECTOR=yVector) FAILED!"
      STOP 666
    ENDIF
    WRITE(*,*) '  Passed: CALL BLAS_swap(...)'
    
    ENDSUBROUTINE testBLAS1Interface
ENDPROGRAM testVectorTypes
