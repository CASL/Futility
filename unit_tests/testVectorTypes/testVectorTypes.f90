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
  USE VectorTypes
  IMPLICIT NONE
  
#ifdef HAVE_PETSC
#include <finclude/petsc.h>
#define IS IS !petscisdef.h defines the keyword IS, and it needs to be reset
 
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
              WRITE(*,*) 'CALL realvec%set(...) FAILED!'
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
              WRITE(*,*) 'CALL realvec%set(...) FAILED!'
              STOP 666
            ENDIF
          ENDDO
          WRITE(*,*) '  Passed: CALL realvec%set(...)'
      ENDSELECT
      
      !Perform test of get function
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
            WRITE(*,*) 'CALL realvec%get(...) FAILED!' 
            STOP 666
          ENDIF
      ENDSELECT
      !test with out of bounds, make sure no crash.
      SELECTTYPE(thisVector)
        TYPE IS(RealVectorType)
          dummy=thisVector%get(8)
          IF(dummy /= -1051._SRK) THEN
            WRITE(*,*) 'CALL realvec%get(...) FAILED!'
            STOP 666
          ENDIF
          dummy=thisVector%get(-1)
          IF(dummy/=-1051._SRK) THEN
            WRITE(*,*) 'CALL realvec%get(...) FAILED!'
            STOP 666
          ENDIF
      ENDSELECT
      !test get with uninit, make sure no crash.
      CALL thisVector%clear()
      SELECTTYPE(thisVector)
        TYPE IS(RealVectorType)      
          dummy=thisVector%get(1)
          IF(dummy /= 0.0_SRK) THEN
            WRITE(*,*) 'CALL realvec%get(...) FAILED!'
            STOP 666
          ENDIF
      ENDSELECT
      CALL thisVector%clear()
      WRITE(*,*) '  Passed: CALL realvec%get(...)'
      
      DEALLOCATE(thisVector)
 
!Test for PETSc vectors (if necessary)
#ifdef HAVE_PETSC    

!      CALL PetscInitialize(PETSC_NULL_CHARACTER,ierr)
      
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
          !now compare actual values with expected
          DO i=1,6
            CALL VecGetValues(thisVector%b,1,i-1,dummy,ierr)
            IF(dummy /= i) THEN
              WRITE(*,*) 'CALL petscvec%set(...) FAILED!'
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
          CALL VecGetSize(thisVector%b,vecsize,ierr)
          DO i=1,vecsize
            CALL VecGetValues(thisVector%b,1,i-1,dummy,ierr)
            IF(dummy == 1._SRK) THEN
              WRITE(*,*) 'CALL petscvec%set(...) FAILED!'
              STOP 666
            ENDIF
          ENDDO
          WRITE(*,*) '  Passed: CALL petscvec%set(...)'
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
            WRITE(*,*) 'CALL petscvec%get(...) FAILED!' 
            STOP 666
          ENDIF
      ENDSELECT
      !test with out of bounds, make sure no crash.
      SELECTTYPE(thisVector)
        TYPE IS(PETScVectorType)
          dummy=thisVector%get(8)
          IF(dummy /= -1051._SRK) THEN
            WRITE(*,*) 'CALL petscvec%get(...) FAILED!'
            STOP 666
          ENDIF
          dummy=thisVector%get(-1)
          IF(dummy/=-1051._SRK) THEN
            WRITE(*,*) 'CALL petscvec%get(...) FAILED!'
            STOP 666
          ENDIF
      ENDSELECT
      !test get with uninit, make sure no crash.
      CALL thisVector%clear()
      SELECTTYPE(thisVector)
        TYPE IS(PETScVectorType)      
          dummy=thisVector%get(1)
          IF(dummy /= 0.0_SRK) THEN
            WRITE(*,*) 'CALL petscvec%get(...) FAILED!'
            STOP 666
          ENDIF
      ENDSELECT
      CALL thisVector%clear()
      WRITE(*,*) '  Passed: CALL petscvec%get(...)'
      
      DEALLOCATE(thisVector)
!      CALL PETScFinalize(ierr)
#endif
      
    ENDSUBROUTINE testVector
ENDPROGRAM testVectorTypes
