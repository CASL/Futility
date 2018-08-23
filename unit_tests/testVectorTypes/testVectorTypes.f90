!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
PROGRAM testVectorTypes
#include "UnitTest.h"
  USE ISO_C_BINDING
  USE ISO_FORTRAN_ENV
  USE UnitTest
  USE IntrType
  USE ExceptionHandler
  USE ParameterLists
  USE ParallelEnv
  USE VectorTypes

#ifdef FUTILITY_HAVE_ForTrilinos
#include "ForTrilinos.h"
  use forteuchos
  use fortpetra
#endif

  IMPLICIT NONE

#ifdef FUTILITY_HAVE_PETSC
#include <finclude/petsc.h>
#undef IS
  PetscErrorCode  :: ierr
#else
#ifdef HAVE_MPI
#include <mpif.h>
  INTEGER :: ierr
#endif
#endif
  INTEGER(SIK) :: iverr
  TYPE(ExceptionHandlerType),POINTER :: e

  CREATE_TEST('Test Vector Types')

  !Configure exception handler for test
  ALLOCATE(e)
  CALL e%setStopOnError(.FALSE.)
  CALL e%setQuietMode(.TRUE.)
  CALL eVectorType%addSurrogate(e)

#ifdef FUTILITY_HAVE_PETSC
  CALL PetscInitialize(PETSC_NULL_CHARACTER,ierr)
#else
#ifdef HAVE_MPI
  CALL MPI_Init(ierr)
#endif
#endif

  REGISTER_SUBTEST("Vector Types",testVector)

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

  REGISTER_SUBTEST("BLAS interface",testBLAS1Interface)

  REGISTER_SUBTEST("Vector factory",testVectorFactory)

  DEALLOCATE(e)
  CALL VectorType_Clear_ValidParams()

#ifdef FUTILITY_HAVE_PETSC
  CALL PetscFinalize(ierr)
#else
#ifdef HAVE_MPI
  CALL MPI_Finalize(ierr)
#endif
#endif
  FINALIZE_TEST()

!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
    SUBROUTINE testVector()
      CLASS(VectorType),ALLOCATABLE :: thisVector
      INTEGER(SIK) :: i
      REAL(SRK),ALLOCATABLE :: testvec(:),testvec2(:),dummyvec(:)
      REAL(SRK) :: dummy
      TYPE(ParamType) :: pList
      LOGICAL(SBK) :: bool
#ifdef FUTILITY_HAVE_PETSC
      INTEGER(SIK) :: vecsize
#endif

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
          bool = .NOT.((thisVector%isInit).OR.(thisVector%n /= 0))
          ASSERT(bool, 'realvec%clear()')
          ASSERT(.NOT.ALLOCATED(thisVector%b), 'realvec%clear()')
          WRITE(*,*) '  Passed: CALL realvec%clear()'
      ENDSELECT

      !Perform test of init function
      !first check intended init path (m provided)
      CALL pList%add('VectorType->n',10)
      CALL thisVector%init(pList)
      SELECTTYPE(thisVector)
        TYPE IS(RealVectorType)
          !check for success
          bool = .NOT.((.NOT.thisVector%isInit).AND.(thisVector%n /= 10))
          ASSERT(bool, 'realvec%init(...)')
          ASSERT(SIZE(thisVector%b) == 10, 'realvec%init(...)')
      ENDSELECT
      CALL thisVector%clear()
      CALL pList%clear()

      !now check init without m being provided
      CALL pList%add('VectorType->n',-10)
      CALL thisVector%init(pList) !expect exception
      ASSERT(.NOT.thisVector%isInit, 'realvec%init(...)')
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
          ASSERT(thisVector%n == 1, 'realvec%init(...)')
      ENDSELECT
     !init with n<1
      CALL thisVector%clear()
      CALL pList%clear()
      CALL pList%add('VectorType->n',-1)
      CALL thisVector%init(pList) !expect exception
      ASSERT(.NOT.thisVector%isInit, 'realvec%init(...)')
      CALL thisVector%clear()
      !n<1, and m not provided
      CALL thisVector%init(pList) !expect exception
      ASSERT(.NOT.thisVector%isInit, 'realvec%init(...)')
      CALL thisVector%clear()

      !init with m<1
      CALL thisVector%clear()
      CALL pList%clear()
      CALL pList%add('VectorType->n',-10)
      CALL thisVector%init(pList) !expect exception
      ASSERT(.NOT.thisVector%isInit, 'realvec%init(...)')
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
            bool = .NOT.((thisVector%b(i) /= i) .AND. iverr /= 0)
            ASSERT(bool, 'realvec%setOne(...)')
          ENDDO
      ENDSELECT

      !set uninit matrix.
      CALL thisVector%clear()
      SELECTTYPE(thisVector)
        TYPE IS(RealVectorType)
          CALL thisVector%set(1,1._SRK,iverr) !since isInit=.FALSE. expect no change
          ASSERT(iverr == -1, 'realvec%setOne(...)')
      ENDSELECT

      !pass out-of bounds i and j
      CALL thisVector%clear()
      CALL pList%clear()
      CALL pList%add('VectorType->n',6)
      CALL thisVector%init(pList)
      CALL thisVector%set(-1,1._SRK,iverr)
      SELECTTYPE(thisVector)
        TYPE IS(RealVectorType)
          ASSERT(iverr == -2, 'realvec%setOne(...)')
      ENDSELECT
      CALL thisVector%set(7,1._SRK,iverr)
      SELECTTYPE(thisVector)
        TYPE IS(RealVectorType)
          ASSERT(iverr == -2, 'realvec%setOne(...)')
      ENDSELECT

      CALL thisVector%clear()
      CALL pList%clear()
      CALL pList%add('VectorType->n',6)
      CALL thisVector%init(pList)

      SELECTTYPE(thisVector)
        TYPE IS(RealVectorType)
          DO i=1,SIZE(thisVector%b)
            ASSERT(thisVector%b(i) /= 1._SRK, 'realvec%setOne(...)')
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
            bool = .NOT.((thisVector%b(i) /= 10._SRK .AND. iverr /= 0))
            ASSERT(bool, 'realvec%setAll_scalar(...)')
          ENDDO
      ENDSELECT

      !set uninit
      CALL thisVector%clear()
      CALL thisVector%set(1._SRK,iverr) !since isInit=.FALSE. expect no change
      SELECTTYPE(thisVector)
        TYPE IS(RealVectorType)
          ASSERT(iverr == -1, 'realvec%setAll_scalar(...)')
      ENDSELECT

      CALL pList%clear()
      CALL pList%add('VectorType->n',6)
      CALL thisVector%init(pList)

      SELECTTYPE(thisVector)
        TYPE IS(RealVectorType)
          DO i=1,SIZE(thisVector%b)
            ASSERT(thisVector%b(i) /= 1._SRK, 'realvec%setAll_scalar(...)')
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
            bool = .NOT.((thisVector%b(i) /= testvec(i)) .AND. iverr /= 0)
            ASSERT(bool, 'realvec%setAll_array(...)')
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
          ASSERT(iverr == -1, 'realvec%setAll_array(...)')
      ENDSELECT

      !set unequal size
      CALL thisVector%clear()
      CALL pList%clear()
      CALL pList%add('VectorType->n',7)
      CALL thisVector%init(pList)
      CALL thisVector%set(testvec2,iverr)
      SELECTTYPE(thisVector)
        TYPE IS(RealVectorType)
          ASSERT(iverr == -3, 'realvec%setAll_array(...)')
      ENDSELECT


      CALL thisVector%clear()
      CALL pList%clear()
      CALL pList%add('VectorType->n',6)
      CALL thisVector%init(pList)

      SELECTTYPE(thisVector)
        TYPE IS(RealVectorType)
          DO i=1,SIZE(thisVector%b)
            bool = thisVector%b(i) /= testvec2(i)
            ASSERT(bool, 'realvec%setAll_array(...)')
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
            bool = .NOT.((thisVector%b(i) /= 0._SRK) .AND. iverr /= 0)
            ASSERT(bool, 'realvec%setRange_scalar(...)')
          ENDDO
          DO i=4,6
            bool = .NOT.((thisVector%b(i) /= 10._SRK) .AND. iverr /= 0)
            ASSERT(bool, 'realvec%setRange_scalar(...) FAILED!')
          ENDDO
      ENDSELECT

      !set uninit
      CALL thisVector%clear()
      CALL thisVector%set(4,6,1._SRK,iverr) !since isInit=.FALSE. expect no change
      SELECTTYPE(thisVector)
        TYPE IS(RealVectorType)
          ASSERT(iverr == -1, 'realvec%setRange_scalar(...)')
      ENDSELECT

      !set out of bounds
      CALL thisVector%clear()
      CALL pList%clear()
      CALL pList%add('VectorType->n',6)
      CALL thisVector%init(pList)
      CALL thisVector%set(5,7,1._SRK,iverr) !since isInit=.FALSE. expect no change
      SELECTTYPE(thisVector)
        TYPE IS(RealVectorType)
          ASSERT(iverr == -2, 'realvec%setRange_scalar(...)')
      ENDSELECT
      CALL thisVector%clear()
      CALL pList%clear()
      CALL pList%add('VectorType->n',6)
      CALL thisVector%init(pList)
      CALL thisVector%set(0,3,1._SRK,iverr) !since isInit=.FALSE. expect no change
      SELECTTYPE(thisVector)
        TYPE IS(RealVectorType)
          ASSERT(iverr == -2, 'realvec%setRange_scalar(...)')
      ENDSELECT

      CALL thisVector%init(pList)

      SELECTTYPE(thisVector)
        TYPE IS(RealVectorType)
          DO i=1,SIZE(thisVector%b)
            ASSERT(thisVector%b(i) /= 1._SRK, 'realvec%setRange_scalar(...)')
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
            bool = .NOT.((thisVector%b(i) /= 0._SRK) .AND. iverr /= 0)
            ASSERT(bool, 'realvec%setRange_array(...)')
          ENDDO
          DO i=4,6
            bool = .NOT.((thisVector%b(i) /= testvec(i-3)) .AND. iverr /= 0)
            ASSERT(bool, 'realvec%setRange_array(...)')
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
          ASSERT(iverr == -1, 'realvec%setRange_array(...)')
      ENDSELECT

      CALL thisVector%clear()
      CALL pList%clear()
      CALL pList%add('VectorType->n',6)
      CALL thisVector%init(pList)
      SELECTTYPE(thisVector)
        TYPE IS(RealVectorType)
          CALL thisVector%set(5,7,testvec2,iverr)
          ASSERT(iverr == -2, 'realvec%setRange_array(...)')
      ENDSELECT

      CALL thisVector%clear()
      CALL pList%clear()
      CALL pList%add('VectorType->n',6)
      CALL thisVector%init(pList)
      SELECTTYPE(thisVector)
        TYPE IS(RealVectorType)
          CALL thisVector%set(0,2,testvec2,iverr)
          ASSERT(iverr == -2, 'realvec%setRange_array(...)')
      ENDSELECT

      CALL thisVector%clear()
      CALL pList%clear()
      CALL pList%add('VectorType->n',6)
      CALL thisVector%init(pList)
      SELECTTYPE(thisVector)
        TYPE IS(RealVectorType)
          CALL thisVector%set(2,6,testvec2,iverr)
          ASSERT(iverr == -3, 'realvec%setRange_array(...)')
      ENDSELECT

      CALL thisVector%init(pList)

      SELECTTYPE(thisVector)
        TYPE IS(RealVectorType)
          DO i=1,3
            ASSERT(thisVector%b(i) == 0._SRK, 'realvec%setRange_array(...)')
          ENDDO
          DO i=4,6
            bool = thisVector%b(i) /= testvec2(i-3)
            ASSERT(bool, 'realvec%setRange_array(...)')
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
          bool = .NOT.((dummyvec(1) /= 1._SRK) .OR. &
                       (dummyvec(2) /= 5._SRK) .OR. &
                       (dummyvec(3) /= 8._SRK) .OR. &
                       (dummyvec(4) /= 9._SRK) .OR. &
                       (dummyvec(5) /= 3._SRK) .OR. &
                       (dummyvec(6) /= 7._SRK) .OR. &
                       (dummyvec(7) /= 2._SRK) .OR. &
                       iverr /= 0)
          ASSERT(bool, 'realvec%getOne(...)')
      ENDSELECT
      !test with out of bounds, make sure no crash.
      SELECTTYPE(thisVector)
        TYPE IS(RealVectorType)
          CALL thisVector%get(8,dummy,iverr)
          ASSERT(iverr == -2, 'realvec%getOne(...)')
          CALL thisVector%get(-1,dummy,iverr)
          ASSERT(iverr == -2, 'realvec%getOne(...)')
      ENDSELECT
      !test get with uninit, make sure no crash.
      CALL thisVector%clear()
      SELECTTYPE(thisVector)
        TYPE IS(RealVectorType)
          CALL thisVector%get(1,dummy,iverr)
          ASSERT(iverr == -1, 'realvec%getOne(...)')
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
          bool = .NOT.((testvec(1) /= 1._SRK) .OR. &
                       (testvec(2) /= 5._SRK) .OR. &
                       (testvec(3) /= 8._SRK) .OR. &
                       (testvec(4) /= 9._SRK) .OR. &
                       (testvec(5) /= 3._SRK) .OR. &
                       (testvec(6) /= 7._SRK) .OR. &
                       (testvec(7) /= 2._SRK) .OR. &
                       iverr /= 0)
          ASSERT(bool, 'realvec%getAll(...)')
      ENDSELECT
      !test get with uninit, make sure no crash.
      CALL thisVector%clear()
      testvec=0._SRK
      SELECTTYPE(thisVector)
        TYPE IS(RealVectorType)
          CALL thisVector%get(testvec,iverr)
          ASSERT(iverr == -1, 'realvec%getAll(...)')
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
          bool = .NOT.((testvec(1) /= 3._SRK) .OR. &
                       (testvec(2) /= 7._SRK) .OR. &
                       (testvec(3) /= 2._SRK) .OR. &
                       iverr /= 0)
          ASSERT(bool, 'realvec%getRange(...)')
      ENDSELECT
      !test get with uninit, make sure no crash.
      CALL thisVector%clear()
      testvec=0._SRK
      SELECTTYPE(thisVector)
        TYPE IS(RealVectorType)
          CALL thisVector%get(5,7,testvec,iverr)
          DO i=1,3
            ASSERT(iverr == -1, 'realvec%getRange(...)')
          ENDDO
      ENDSELECT
      WRITE(*,*) '  Passed: CALL realvec%getRange(...)'
      DEALLOCATE(thisVector)

!Test for PETSc vectors (if necessary)
#ifdef FUTILITY_HAVE_PETSC

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
          bool = .NOT.((thisVector%isInit).OR.(thisVector%n /= 0))
          ASSERT(bool, 'petscvec%clear()')

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
      CALL pList%clear()
      CALL pList%add('VectorType->n',10)
      CALL pList%add('VectorType->MPI_Comm_ID',PE_COMM_SELF)
      CALL thisVector%init(pList)
      SELECTTYPE(thisVector)
        TYPE IS(PETScVectorType)
          !check for success
          bool = .NOT.((.NOT.thisVector%isInit).AND.(thisVector%n /= 10))
          ASSERT(bool, 'petscvec%init(...)')
          CALL VecGetSize(thisVector%b,i,ierr)
          ASSERT(i == 10, 'petscvec%init(...)')
      ENDSELECT
      CALL thisVector%clear()

      !now check init without m being provided
      CALL pList%clear()
      CALL pList%add('VectorType->n',-10)
      CALL pList%add('VectorType->MPI_Comm_ID',PE_COMM_SELF)
      CALL thisVector%init(pList) !expect exception
      ASSERT(.NOT.thisVector%isInit, 'petscvec%init(...)')
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
          ASSERT(thisVector%n == 1, 'petscvec%init(...)') !n==1 implies it was changed, and thus fail
      ENDSELECT
     !init with n<1
      CALL thisVector%clear()
      CALL pList%clear()
      CALL pList%add('VectorType->n',-1)
      CALL pList%add('VectorType->MPI_Comm_ID',PE_COMM_SELF)
      CALL thisVector%init(pList) !expect exception
      ASSERT(.NOT.thisVector%isInit, 'petscvec%init(...)')
      CALL thisVector%clear()
      !n<1, and m not provided
      CALL pList%clear()
      CALL pList%add('VectorType->n',-1)
      CALL pList%add('VectorType->MPI_Comm_ID',PE_COMM_SELF)
      CALL thisVector%init(pList) !expect exception
      ASSERT(.NOT.thisVector%isInit, 'petscvec%init(...)')
      CALL thisVector%clear()
      !init with m<1
      CALL thisVector%clear()
      CALL pList%clear()
      CALL pList%add('VectorType->n',-10)
      CALL pList%add('VectorType->MPI_Comm_ID',PE_COMM_SELF)
      CALL thisVector%init(pList) !expect exception
      ASSERT(.NOT.thisVector%isInit, 'petscvec%init(...)')
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
            bool = .NOT.(dummy /= i .AND. iverr /= 0)
            ASSERT(bool, 'petscvec%setOne(...)')
          ENDDO
      ENDSELECT

      !set uninit matrix.
      CALL thisVector%clear()
      CALL thisVector%set(1,1._SRK,iverr) !since isInit=.FALSE. expect no change
      ASSERT(iverr == -1, 'petscvec%setOne(...)')

      !pass out-of bounds i and j
      CALL thisVector%clear()
      CALL pList%clear()
      CALL pList%add('VectorType->n',6)
      CALL pList%add('VectorType->MPI_Comm_ID',PE_COMM_SELF)
      CALL thisVector%init(pList)
      CALL thisVector%set(-1,1._SRK,iverr)
      ASSERT(iverr == -2, 'petscvec%setOne(...)')
      CALL thisVector%set(7,1._SRK,iverr)
      ASSERT(iverr == -2, 'petscvec%setOne(...)')

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
            ASSERT(dummy /= 1._SRK, 'petscvec%setOne(...)')
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
            bool = .NOT.((dummy /= 10._SRK) .AND. iverr /= 0)
            ASSERT(bool, 'petscvec%setAll_scalar(...)')
          ENDDO
      ENDSELECT

      !set uninit matrix.
      CALL thisVector%clear()
      CALL thisVector%set(1._SRK,iverr) !since isInit=.FALSE. expect no change
      bool = .NOT.((dummy /= 10._SRK) .AND. iverr /= -1)
      ASSERT(bool, 'petscvec%setAll_scalar(...)')

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
            ASSERT(dummy /= 1._SRK, 'petscvec%setAll_scalar(...)')
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
            bool = .NOT.((dummy /= testvec(i)) .AND. iverr /= 0)
            ASSERT(bool, 'petscvec%setAll_array(...)')
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
      ASSERT(iverr == -1, 'petscvec%setAll_array(...)')

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
      ASSERT(iverr == -3, 'petscvec%setAll_array(...)')

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
            ASSERT(dummy /= testvec2(i), 'petscvec%setAll_array(...)')
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
            ASSERT(dummy == 0._SRK, 'petscvec%setRange_scalar(...)')
          ENDDO
          DO i=4,6
            CALL thisVector%get(i,dummy)
            ASSERT(dummy == 10._SRK, 'petscvec%setRange_scalar(...)')
          ENDDO
      ENDSELECT

      !set uninit matrix.
      CALL thisVector%clear()
      CALL thisVector%set(4,6,1._SRK,iverr) !since isInit=.FALSE. expect no change
      ASSERT(iverr == -1._SRK, 'petscvec%setRange_scalar(...)')

      !set out of bounds
      CALL thisVector%clear()
      CALL pList%clear()
      CALL pList%add('VectorType->n',6)
      CALL pList%add('VectorType->MPI_Comm_ID',PE_COMM_SELF)
      CALL thisVector%init(pList)
      CALL thisVector%set(4,7,1._SRK,iverr)
      ASSERT(iverr == -2._SRK, 'petscvec%setRange_scalar(...)')

      !set out of bounds
      CALL thisVector%clear()
      CALL pList%clear()
      CALL pList%add('VectorType->n',6)
      CALL pList%add('VectorType->MPI_Comm_ID',PE_COMM_SELF)
      CALL thisVector%init(pList)
      CALL thisVector%set(0,3,1._SRK,iverr)
      ASSERT(iverr == -2._SRK, 'petscvec%setRange_scalar(...)')

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
            ASSERT(dummy /= 1._SRK, 'petscvec%setRange_scalar(...)')
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
            bool = .NOT.((dummy /= 0._SRK) .AND. iverr /= 0)
            ASSERT(bool, 'petscvec%setRange_array(...)')
          ENDDO
          DO i=4,6
            CALL thisVector%get(i,dummy)
            bool = .NOT.((dummy /= testvec(i-3)) .AND. iverr /= 0)
            ASSERT(bool, 'petscvec%setRange_array(...)')
          ENDDO
      ENDSELECT

      !set uninit matrix.
      CALL thisVector%clear()
      ALLOCATE(testvec2(3))
      testvec2(1)=2._SRK
      testvec2(2)=4._SRK
      testvec2(3)=6._SRK
      CALL thisVector%set(4,6,testvec2,iverr) !since isInit=.FALSE. expect no change
      ASSERT(iverr == -1, 'petscvec%setRange_array(...)')
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
      ASSERT(iverr == -3, 'petscvec%setRange_array(...)')

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
            ASSERT(dummy /= testvec2(i-3), 'petscvec%setRange_array(...)')
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
          bool = .NOT.((dummyvec(1) /= 1._SRK) .OR. &
                 (dummyvec(2) /= 5._SRK) .OR. &
                 (dummyvec(3) /= 8._SRK) .OR. &
                 (dummyvec(4) /= 9._SRK) .OR. &
                 (dummyvec(5) /= 3._SRK) .OR. &
                 (dummyvec(6) /= 7._SRK) .OR. &
                 (dummyvec(7) /= 2._SRK))
          ASSERT(bool, 'petscvec%getOne(...)')
      ENDSELECT
      !test with out of bounds, make sure no crash.
      SELECTTYPE(thisVector)
        TYPE IS(PETScVectorType)
          CALL thisVector%get(8,dummy,iverr)
          ASSERT(iverr == -2, 'petscvec%getOne(...)')
          CALL thisVector%get(-1,dummy)
          ASSERT(iverr == -2, 'petscvec%getOne(...)')
      ENDSELECT
      !test get with uninit, make sure no crash.
      CALL thisVector%clear()
      SELECTTYPE(thisVector)
        TYPE IS(PETScVectorType)
          CALL thisVector%get(1,dummy,iverr)
          ASSERT(iverr == -1, 'petscvec%getOne(...)')
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
          bool = .NOT.((testvec(1) /= 1._SRK) .OR. &
                       (testvec(2) /= 5._SRK) .OR. &
                       (testvec(3) /= 8._SRK) .OR. &
                       (testvec(4) /= 9._SRK) .OR. &
                       (testvec(5) /= 3._SRK) .OR. &
                       (testvec(6) /= 7._SRK) .OR. &
                       (testvec(7) /= 2._SRK))
          ASSERT(bool, 'petscvec%getAll(...)')
      ENDSELECT
      !test get with uninit, make sure no crash.
      CALL thisVector%clear()
      SELECTTYPE(thisVector)
        TYPE IS(PETScVectorType)
          CALL thisVector%get(testvec,iverr)
          ASSERT(iverr == -1, 'petscvec%getAll(...)')
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
          bool = .NOT.((testvec(1) /= 3._SRK) .OR. &
                       (testvec(2) /= 7._SRK) .OR. &
                       (testvec(3) /= 2._SRK))
          ASSERT(bool, 'petscvec%getRange(...)')
      ENDSELECT
      !test with out of bounds, make sure no crash.
      SELECTTYPE(thisVector)
        TYPE IS(PETScVectorType)
          CALL thisVector%get(2,8,testvec,iverr)
          ASSERT(iverr == -2, 'petscvec%getRange(...)')
          CALL thisVector%get(-1,2,testvec,iverr)
          ASSERT(iverr == -2, 'petscvec%getRange(...)')
      ENDSELECT
      !test get with uninit, make sure no crash.
      CALL thisVector%clear()
      SELECTTYPE(thisVector)
        TYPE IS(PETScVectorType)
          CALL thisVector%get(1,3,testvec,iverr)
          ASSERT(iverr == -1, 'petscvec%getRange(...)')
      ENDSELECT
      CALL thisVector%clear()
      WRITE(*,*) '  Passed: CALL petscvec%getRange(...)'

      DEALLOCATE(thisVector)
      CALL pList%clear()
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


#ifdef FUTILITY_HAVE_ForTrilinos
      ALLOCATE(TrilinosVectorType :: thisVector)
      !Perform test of init function
      !first check intended init path (m provided)
      CALL pList%clear()
      CALL pList%add('VectorType->n',10)
      CALL pList%add('VectorType->MPI_Comm_ID',PE_COMM_SELF)
      CALL thisVector%init(pList)
      SELECTTYPE(thisVector)
        TYPE IS(TrilinosVectorType)
          !check for success
          bool = .NOT.((.NOT.thisVector%isInit).AND.(thisVector%n /= 10))
          ASSERT(bool, 'Trilinosvec%init(...)')
      ENDSELECT
      CALL thisVector%clear()

      !now check init without m being provided
      CALL pList%clear()
      CALL pList%add('VectorType->n',-10)
      CALL pList%add('VectorType->MPI_Comm_ID',PE_COMM_SELF)
      CALL thisVector%init(pList) !expect exception
      ASSERT(.NOT.thisVector%isInit, 'Trilinosvec%init(...)')
      CALL thisVector%clear()

      !init it twice so on 2nd init, isInit==.TRUE.
      CALL pList%clear()
      CALL pList%add('VectorType->n',10)
      CALL pList%add('VectorType->MPI_Comm_ID',PE_COMM_SELF)
      CALL thisVector%init(pList)
      SELECTTYPE(thisVector)
        TYPE IS(TrilinosVectorType); thisVector%n=1
      ENDSELECT
      CALL thisVector%init(pList)
      SELECTTYPE(thisVector)
        TYPE IS(TrilinosVectorType)
          ASSERT(thisVector%n == 1, 'Trilinosvec%init(...)') !n==1 implies it was changed, and thus fail
      ENDSELECT
     !init with n<1
      CALL thisVector%clear()
      CALL pList%clear()
      CALL pList%add('VectorType->n',-1)
      CALL pList%add('VectorType->MPI_Comm_ID',PE_COMM_SELF)
      CALL thisVector%init(pList) !expect exception
      ASSERT(.NOT.thisVector%isInit, 'Trilinosvec%init(...)')
      CALL thisVector%clear()
      !n<1, and m not provided
      CALL pList%clear()
      CALL pList%add('VectorType->n',-1)
      CALL pList%add('VectorType->MPI_Comm_ID',PE_COMM_SELF)
      CALL thisVector%init(pList) !expect exception
      ASSERT(.NOT.thisVector%isInit, 'Trilinosvec%init(...)')
      CALL thisVector%clear()
      !init with m<1
      CALL thisVector%clear()
      CALL pList%clear()
      CALL pList%add('VectorType->n',-10)
      CALL pList%add('VectorType->MPI_Comm_ID',PE_COMM_SELF)
      CALL thisVector%init(pList) !expect exception
      ASSERT(.NOT.thisVector%isInit, 'Trilinosvec%init(...)')
      CALL thisVector%clear()
      WRITE(*,*) '  Passed: CALL Trilinosvec%init(...)'

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
        TYPE IS(TrilinosVectorType)
          !now compare actual values with expected
          DO i=1,6
            CALL thisVector%get(i,dummy)
            bool = .NOT.(dummy /= i .AND. iverr /= 0)
            ASSERT(bool, 'Trilinosvec%setOne(...)')
          ENDDO
      ENDSELECT

      !set uninit matrix.
      CALL thisVector%clear()
      CALL thisVector%set(1,1._SRK,iverr) !since isInit=.FALSE. expect no change
      ASSERT(iverr == -1, 'Trilinosvec%setOne(...)')

      !pass out-of bounds i and j
      CALL thisVector%clear()
      CALL pList%clear()
      CALL pList%add('VectorType->n',6)
      CALL pList%add('VectorType->MPI_Comm_ID',PE_COMM_SELF)
      CALL thisVector%init(pList)
      CALL thisVector%set(-1,1._SRK,iverr)
      ASSERT(iverr == -2, 'Trilinosvec%setOne(...)')
      CALL thisVector%set(7,1._SRK,iverr)
      ASSERT(iverr == -2, 'Trilinosvec%setOne(...)')

      CALL thisVector%clear()
      CALL pList%clear()
      CALL pList%add('VectorType->n',6)
      CALL pList%add('VectorType->MPI_Comm_ID',PE_COMM_SELF)
      CALL thisVector%init(pList)

      SELECTTYPE(thisVector)
        TYPE IS(TrilinosVectorType)
          DO i=1,6
            CALL thisVector%get(i,dummy)
            ASSERT(dummy /= 1._SRK, 'Trilinosvec%setOne(...)')
          ENDDO
          WRITE(*,*) '  Passed: CALL Trilinosvec%setOne(...)'
      ENDSELECT

      !Perform test of set function
      !use set to update all values at once (scalar)
      CALL thisVector%init(pList)
      CALL thisVector%set(10._SRK,iverr)
      SELECTTYPE(thisVector)
        TYPE IS(TrilinosVectorType)
          !now compare actual values with expected
          DO i=1,6
            CALL thisVector%get(i,dummy)
            bool = .NOT.((dummy /= 10._SRK) .AND. iverr /= 0)
            ASSERT(bool, 'Trilinosvec%setAll_scalar(...)')
            FINFO() i, dummy
          ENDDO
      ENDSELECT

      !set uninit matrix.
      CALL thisVector%clear()
      CALL thisVector%set(1._SRK,iverr) !since isInit=.FALSE. expect no change
      bool = .NOT.(iverr /= -1)
      ASSERT(bool, 'Trilinosvec%setAll_scalar(...)')

      CALL pList%clear()
      CALL pList%add('VectorType->n',6)
      CALL pList%add('VectorType->MPI_Comm_ID',PE_COMM_SELF)
      CALL thisVector%init(pList)

      SELECTTYPE(thisVector)
        TYPE IS(TrilinosVectorType)

          DO i=1,thisVector%n
            CALL thisVector%get(i,dummy)
            ASSERT(dummy /= 1._SRK, 'Trilinosvec%setAll_scalar(...)')
          ENDDO
          WRITE(*,*) '  Passed: CALL Trilinosvec%setAll_scalar(...)'
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
! TODO: this interface has yet to be written
!      CALL thisVector%set(testvec,iverr)
!      SELECTTYPE(thisVector)
!        TYPE IS(TrilinosVectorType)
!          !now compare actual values with expected
!          DO i=1,6
!            CALL thisVector%get(i,dummy)
!            bool = .NOT.((dummy /= testvec(i)) .AND. iverr /= 0)
!            ASSERT(bool, 'Trilinosvec%setAll_array(...)')
!          ENDDO
!      ENDSELECT

      !set uninit matrix.
      CALL thisVector%clear()
      testvec2(1)=1._SRK
      testvec2(2)=3._SRK
      testvec2(3)=5._SRK
      testvec2(4)=7._SRK
      testvec2(5)=9._SRK
      testvec2(6)=11._SRK
! TODO: this interface has yet to be written
!      CALL thisVector%set(testvec2,iverr) !since isInit=.FALSE. expect no change
!      ASSERT(iverr == -1, 'Trilinosvec%setAll_array(...)')

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
! TODO: this interface has yet to be written
!      CALL thisVector%set(testvec2,iverr) !since isInit=.FALSE. expect no change
!      ASSERT(iverr == -3, 'Trilinosvec%setAll_array(...)')
!
!      CALL thisVector%clear()
!      CALL pList%clear()
!      CALL pList%add('VectorType->n',6)
!      CALL pList%add('VectorType->MPI_Comm_ID',PE_COMM_SELF)
!      CALL thisVector%init(pList)
!      SELECTTYPE(thisVector)
!        TYPE IS(TrilinosVectorType)
!          DO i=1,thisVector%n
!            CALL thisVector%get(i,dummy)
!            ASSERT(dummy /= testvec2(i), 'Trilinosvec%setAll_array(...)')
!          ENDDO
!          WRITE(*,*) '  Passed: CALL Trilinosvec%setAll_array(...)'
!      ENDSELECT

      !Perform test of set function
      !use set to update all values at once (scalar)
      CALL pList%clear()
      CALL pList%add('VectorType->n',6)
      CALL pList%add('VectorType->MPI_Comm_ID',PE_COMM_SELF)
      CALL thisVector%init(pList)
      CALL thisVector%set(4,6,10._SRK,iverr)
      SELECTTYPE(thisVector)
        TYPE IS(TrilinosVectorType)
          !now compare actual values with expected
          DO i=1,3
            CALL thisVector%get(i,dummy)
            ASSERT(dummy == 0._SRK, 'Trilinosvec%setRange_scalar(...)')
          ENDDO
          DO i=4,6
            CALL thisVector%get(i,dummy)
            ASSERT(dummy == 10._SRK, 'Trilinosvec%setRange_scalar(...)')
          ENDDO
      ENDSELECT

      !set uninit matrix.
      CALL thisVector%clear()
      CALL thisVector%set(4,6,1._SRK,iverr) !since isInit=.FALSE. expect no change
      ASSERT(iverr == -1._SRK, 'Trilinosvec%setRange_scalar(...)')

      !set out of bounds
      CALL thisVector%clear()
      CALL pList%clear()
      CALL pList%add('VectorType->n',6)
      CALL pList%add('VectorType->MPI_Comm_ID',PE_COMM_SELF)
      CALL thisVector%init(pList)
      CALL thisVector%set(4,7,1._SRK,iverr)
      ASSERT(iverr == -2._SRK, 'Trilinosvec%setRange_scalar(...)')

      !set out of bounds
      CALL thisVector%clear()
      CALL pList%clear()
      CALL pList%add('VectorType->n',6)
      CALL pList%add('VectorType->MPI_Comm_ID',PE_COMM_SELF)
      CALL thisVector%init(pList)
      CALL thisVector%set(0,3,1._SRK,iverr)
      ASSERT(iverr == -2._SRK, 'Trilinosvec%setRange_scalar(...)')

      CALL thisVector%clear()
      CALL pList%clear()
      CALL pList%add('VectorType->n',6)
      CALL pList%add('VectorType->MPI_Comm_ID',PE_COMM_SELF)
      CALL thisVector%init(pList)

      SELECTTYPE(thisVector)
        TYPE IS(TrilinosVectorType)
          DO i=1,thisVector%n
            CALL thisVector%get(i,dummy)
            ASSERT(dummy /= 1._SRK, 'Trilinosvec%setRange_scalar(...)')
          ENDDO
          WRITE(*,*) '  Passed: CALL Trilinosvec%setRange_scalar(...)'
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
        TYPE IS(TrilinosVectorType)
          !now compare actual values with expected
          DO i=1,3
            CALL thisVector%get(i,dummy)
            bool = .NOT.((dummy /= 0._SRK) .AND. iverr /= 0)
            ASSERT(bool, 'Trilinosvec%setRange_array(...)')
          ENDDO
          DO i=4,6
            CALL thisVector%get(i,dummy)
            bool = .NOT.((dummy /= testvec(i-3)) .AND. iverr /= 0)
            ASSERT(bool, 'Trilinosvec%setRange_array(...)')
          ENDDO
      ENDSELECT

      !set uninit matrix.
      CALL thisVector%clear()
      ALLOCATE(testvec2(3))
      testvec2(1)=2._SRK
      testvec2(2)=4._SRK
      testvec2(3)=6._SRK
      CALL thisVector%set(4,6,testvec2,iverr) !since isInit=.FALSE. expect no change
      ASSERT(iverr == -1, 'Trilinosvec%setRange_array(...)')
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
      ASSERT(iverr == -3, 'Trilinosvec%setRange_array(...)')

      CALL thisVector%clear()
      CALL pList%clear()
      CALL pList%add('VectorType->n',6)
      CALL pList%add('VectorType->MPI_Comm_ID',PE_COMM_SELF)
      CALL thisVector%init(pList)

      SELECTTYPE(thisVector)
        TYPE IS(TrilinosVectorType)
          DO i=4,6
            CALL thisVector%get(i,dummy)
            ASSERT(dummy /= testvec2(i-3), 'Trilinosvec%setRange_array(...)')
          ENDDO
          WRITE(*,*) '  Passed: CALL Trilinosvec%setRange_array(...)'
      ENDSELECT

      !Perform test of get function
      ![1 5 8 9 3 7 2]
      CALL thisVector%clear()
      CALL pList%clear()
      CALL pList%add('VectorType->n',7)
      CALL pList%add('VectorType->MPI_Comm_ID',PE_COMM_SELF)
      CALL thisVector%init(pList)
      SELECTTYPE(thisVector)
        TYPE IS(TrilinosVectorType)
          CALL thisVector%set(1,1._SRK)
          CALL thisVector%set(2,5._SRK)
          CALL thisVector%set(3,8._SRK)
          CALL thisVector%set(4,9._SRK)
          CALL thisVector%set(5,3._SRK)
          CALL thisVector%set(6,7._SRK)
          CALL thisVector%set(7,2._SRK)
          IF(ALLOCATED(dummyvec)) DEALLOCATE(dummyvec)
          ALLOCATE(dummyvec(thisVector%n))
          DO i=1,7
            CALL thisVector%get(i,dummyvec(i))
          ENDDO
          bool = .NOT.((dummyvec(1) /= 1._SRK) .OR. &
                 (dummyvec(2) /= 5._SRK) .OR. &
                 (dummyvec(3) /= 8._SRK) .OR. &
                 (dummyvec(4) /= 9._SRK) .OR. &
                 (dummyvec(5) /= 3._SRK) .OR. &
                 (dummyvec(6) /= 7._SRK) .OR. &
                 (dummyvec(7) /= 2._SRK))
          ASSERT(bool, 'Trilinosvec%getOne(...)')
      ENDSELECT
      !test with out of bounds, make sure no crash.
      SELECTTYPE(thisVector)
        TYPE IS(TrilinosVectorType)
          CALL thisVector%get(8,dummy,iverr)
          ASSERT(iverr == -2, 'Trilinosvec%getOne(...)')
          CALL thisVector%get(-1,dummy)
          ASSERT(iverr == -2, 'Trilinosvec%getOne(...)')
      ENDSELECT
      !test get with uninit, make sure no crash.
      CALL thisVector%clear()
      SELECTTYPE(thisVector)
        TYPE IS(TrilinosVectorType)
          CALL thisVector%get(1,dummy,iverr)
          ASSERT(iverr == -1, 'Trilinosvec%getOne(...)')
      ENDSELECT
      CALL thisVector%clear()
      WRITE(*,*) '  Passed: CALL Trilinosvec%getOne(...)'

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
        TYPE IS(TrilinosVectorType)
          CALL thisVector%set(1,1._SRK)
          CALL thisVector%set(2,5._SRK)
          CALL thisVector%set(3,8._SRK)
          CALL thisVector%set(4,9._SRK)
          CALL thisVector%set(5,3._SRK)
          CALL thisVector%set(6,7._SRK)
          CALL thisVector%set(7,2._SRK)
! TODO: this interface has yet to be written
!          CALL thisVector%get(testvec)
!          bool = .NOT.((testvec(1) /= 1._SRK) .OR. &
!                       (testvec(2) /= 5._SRK) .OR. &
!                       (testvec(3) /= 8._SRK) .OR. &
!                       (testvec(4) /= 9._SRK) .OR. &
!                       (testvec(5) /= 3._SRK) .OR. &
!                       (testvec(6) /= 7._SRK) .OR. &
!                       (testvec(7) /= 2._SRK))
!          ASSERT(bool, 'Trilinosvec%getAll(...)')
      ENDSELECT
      !test get with uninit, make sure no crash.
      CALL thisVector%clear()
      SELECTTYPE(thisVector)
        TYPE IS(TrilinosVectorType)
          CALL thisVector%get(testvec,iverr)
          ASSERT(iverr == -1, 'Trilinosvec%getAll(...)')
      ENDSELECT
      CALL thisVector%clear()
      WRITE(*,*) '  Passed: CALL Trilinosvec%getAll(...)'

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
        TYPE IS(TrilinosVectorType)
          CALL thisVector%set(1,1._SRK)
          CALL thisVector%set(2,5._SRK)
          CALL thisVector%set(3,8._SRK)
          CALL thisVector%set(4,9._SRK)
          CALL thisVector%set(5,3._SRK)
          CALL thisVector%set(6,7._SRK)
          CALL thisVector%set(7,2._SRK)
          CALL thisVector%get(5,7,testvec)
          bool = .NOT.((testvec(1) /= 3._SRK) .OR. &
                       (testvec(2) /= 7._SRK) .OR. &
                       (testvec(3) /= 2._SRK))
          ASSERT(bool, 'Trilinosvec%getRange(...)')
      ENDSELECT
      !test with out of bounds, make sure no crash.
      SELECTTYPE(thisVector)
        TYPE IS(TrilinosVectorType)
          CALL thisVector%get(2,8,testvec,iverr)
          ASSERT(iverr == -2, 'Trilinosvec%getRange(...)')
          CALL thisVector%get(-1,2,testvec,iverr)
          ASSERT(iverr == -2, 'Trilinosvec%getRange(...)')
      ENDSELECT
      !test get with uninit, make sure no crash.
      CALL thisVector%clear()
      SELECTTYPE(thisVector)
        TYPE IS(TrilinosVectorType)
          CALL thisVector%get(1,3,testvec,iverr)
          ASSERT(iverr == -1, 'Trilinosvec%getRange(...)')
      ENDSELECT
      CALL thisVector%clear()
      WRITE(*,*) '  Passed: CALL Trilinosvec%getRange(...)'

      DEALLOCATE(thisVector)
      CALL pList%clear()
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
    LOGICAL(SBK) :: bool
    INTEGER(SIK) :: i

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
    ASSERT(r .APPROXEQ. 14.0_SRK, 'BLAS_asum(THISVECTOR=xVector,N=xVector%n,INCX=1) [REAL]')
    r=0.0_SRK
    r = BLAS_asum(THISVECTOR=xVector,INCX=1)
    ASSERT(r .APPROXEQ. 14.0_SRK, 'BLAS_asum(THISVECTOR=xVector,INCX=1) [REAL]')
    r=0.0_SRK
    r = BLAS_asum(THISVECTOR=xVector,N=xVector%n)
    ASSERT(r .APPROXEQ. 14.0_SRK, 'BLAS_asum(THISVECTOR=xVector,N=xVector%n) [REAL]')
    r=0.0_SRK
    r = BLAS_asum(THISVECTOR=xVector)
    ASSERT(r .APPROXEQ. 14.0_SRK, 'BLAS_asum(THISVECTOR=xVector) [REAL]')
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
    bool = .NOT.((.NOT.((dummyvec(1) .APPROXEQ. 28._SRK) .AND. &
                (dummyvec(2) .APPROXEQ. -5._SRK) .AND. &
                (dummyvec(3) .APPROXEQ. 36._SRK))))
    ASSERT(bool, 'BLAS_axpy(THISVECTOR=xVector,NEWVECTOR=yVector,A=a,N=xVector%n,INCX=1,INCY=1) -scalar_a [REAL]')
    CALL yVector%set(1,3.0_SRK)
    CALL yVector%set(2,5.0_SRK)
    CALL yVector%set(3,1.0_SRK)
    CALL BLAS_axpy(THISVECTOR=xVector,NEWVECTOR=yVector,A=a,N=xVector%n,INCX=1)
    CALL yVector%get(dummyvec)
    bool = ((dummyvec(1) .APPROXEQ. 28._SRK) .AND. &
            (dummyvec(2) .APPROXEQ. -5._SRK) .AND. &
            (dummyvec(3) .APPROXEQ. 36._SRK))
    ASSERT(bool, 'BLAS_axpy(THISVECTOR=xVector,NEWVECTOR=yVector,A=a,N=xVector%n,INCX=1) -scalar_a [REAL]')
    CALL yVector%set(1,3.0_SRK)
    CALL yVector%set(2,5.0_SRK)
    CALL yVector%set(3,1.0_SRK)
    CALL BLAS_axpy(THISVECTOR=xVector,NEWVECTOR=yVector,A=a,N=xVector%n,INCY=1)
    CALL yVector%get(dummyvec)

    bool = ((dummyvec(1) .APPROXEQ. 28._SRK) .AND. &
            (dummyvec(2) .APPROXEQ. -5._SRK) .AND. &
            (dummyvec(3) .APPROXEQ. 36._SRK))
    ASSERT(bool, 'BLAS_axpy(THISVECTOR=xVector,NEWVECTOR=yVector,A=a,N=xVector%n,INCY=1) -scalar_a [REAL]')
    CALL yVector%set(1,3.0_SRK)
    CALL yVector%set(2,5.0_SRK)
    CALL yVector%set(3,1.0_SRK)
    CALL BLAS_axpy(THISVECTOR=xVector,NEWVECTOR=yVector,A=a,N=xVector%n)
    CALL yVector%get(dummyvec)
    bool = ((dummyvec(1) .APPROXEQ. 28._SRK) .AND. &
            (dummyvec(2) .APPROXEQ. -5._SRK) .AND. &
            (dummyvec(3) .APPROXEQ. 36._SRK))
    ASSERT(bool, 'BLAS_axpy(THISVECTOR=xVector,NEWVECTOR=yVector,A=a,N=xVector%n) -scalar_a [REAL]')
    CALL yVector%set(1,3.0_SRK)
    CALL yVector%set(2,5.0_SRK)
    CALL yVector%set(3,1.0_SRK)
    CALL BLAS_axpy(THISVECTOR=xVector,NEWVECTOR=yVector,A=a)
    CALL yVector%get(dummyvec)
    bool = ((dummyvec(1) .APPROXEQ. 28._SRK) .AND. &
            (dummyvec(2) .APPROXEQ. -5._SRK) .AND. &
            (dummyvec(3) .APPROXEQ. 36._SRK))
    ASSERT(bool, 'BLAS_axpy(THISVECTOR=xVector,NEWVECTOR=yVector,A=a) -scalar_a [REAL]')
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
    bool = ((dummyvec(1) .APPROXEQ. 33._SRK) .AND. &
            (dummyvec(2) .APPROXEQ. -3._SRK) .AND. &
            (dummyvec(3) .APPROXEQ. 15._SRK))
    ASSERT(bool, "BLAS_axpy(THISVECTOR=xVector,NEWVECTOR=yVector,AVECTOR=aVector,N=xVector%n,INCX=1,INCY=1)")
    FINFO() "-vector_a [REAL]"
    CALL yVector%set(1,3.0_SRK)
    CALL yVector%set(2,5.0_SRK)
    CALL yVector%set(3,1.0_SRK)
    CALL BLAS_axpy(THISVECTOR=xVector,NEWVECTOR=yVector,AVECTOR=aVector,N=xVector%n,INCX=1)
    CALL yVector%get(dummyvec)
    bool = ((dummyvec(1) .APPROXEQ. 33._SRK) .AND. &
            (dummyvec(2) .APPROXEQ. -3._SRK) .AND. &
            (dummyvec(3) .APPROXEQ. 15._SRK))
    ASSERT(bool, "BLAS_axpy(THISVECTOR=xVector,NEWVECTOR=yVector,AVECTOR=aVector,N=xVector%n,INCX=1)")
    FINFO() "-vector_a [REAL]"
    CALL yVector%set(1,3.0_SRK)
    CALL yVector%set(2,5.0_SRK)
    CALL yVector%set(3,1.0_SRK)
    CALL BLAS_axpy(THISVECTOR=xVector,NEWVECTOR=yVector,AVECTOR=aVector,N=xVector%n,INCY=1)
    CALL yVector%get(dummyvec)
    bool = ((dummyvec(1) .APPROXEQ. 33._SRK) .AND. &
            (dummyvec(2) .APPROXEQ. -3._SRK) .AND. &
            (dummyvec(3) .APPROXEQ. 15._SRK))
    ASSERT(bool, "BLAS_axpy(THISVECTOR=xVector,NEWVECTOR=yVector,AVECTOR=aVector,N=xVector%n,INCY=1)")
    FINFO() "-vector_a [REAL]"
    CALL yVector%set(1,3.0_SRK)
    CALL yVector%set(2,5.0_SRK)
    CALL yVector%set(3,1.0_SRK)
    CALL BLAS_axpy(THISVECTOR=xVector,NEWVECTOR=yVector,AVECTOR=aVector,N=xVector%n)
    CALL yVector%get(dummyvec)
    bool = ((dummyvec(1) .APPROXEQ. 33._SRK) .AND. &
            (dummyvec(2) .APPROXEQ. -3._SRK) .AND. &
            (dummyvec(3) .APPROXEQ. 15._SRK))
    ASSERT(bool, 'BLAS_axpy(THISVECTOR=xVector,NEWVECTOR=yVector,AVECTOR=aVector,N=xVector%n) -vector_a [REAL]')
    CALL yVector%set(1,3.0_SRK)
    CALL yVector%set(2,5.0_SRK)
    CALL yVector%set(3,1.0_SRK)
    CALL BLAS_axpy(THISVECTOR=xVector,NEWVECTOR=yVector,AVECTOR=aVector)
    CALL yVector%get(dummyvec)
    bool = ((dummyvec(1) .APPROXEQ. 33._SRK) .AND. &
            (dummyvec(2) .APPROXEQ. -3._SRK) .AND. &
            (dummyvec(3) .APPROXEQ. 15._SRK))
    ASSERT(bool, 'BLAS_axpy(THISVECTOR=xVector,NEWVECTOR=yVector,AVECTOR=aVector) -vector_a [REAL]')
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
    bool = ((dummyvec(1) .APPROXEQ. 1.0_SRK) .AND. &
            (dummyvec(2) .APPROXEQ. 3.0_SRK) .AND. &
            (dummyvec(3) .APPROXEQ. 7.0_SRK))
    ASSERT(bool, 'BLAS_copy(THISVECTOR=xVector,NEWVECTOR=yVector,N=xVector%n,INCX=1,INCY=1) [REAL]')
    CALL yVector%set(0.0_SRK)
    CALL BLAS_copy(THISVECTOR=xVector,NEWVECTOR=yVector,INCX=1,INCY=1)
    CALL yVector%get(dummyvec)
    bool = ((dummyvec(1) .APPROXEQ. 1.0_SRK) .AND. &
            (dummyvec(2) .APPROXEQ. 3.0_SRK) .AND. &
            (dummyvec(3) .APPROXEQ. 7.0_SRK))
    ASSERT(bool, 'BLAS_copy(THISVECTOR=xVector,NEWVECTOR=yVector,INCX=1,INCY=1) [REAL]')
    CALL yVector%set(0.0_SRK)
    CALL BLAS_copy(THISVECTOR=xVector,NEWVECTOR=yVector,N=xVector%n,INCX=1)
    CALL yVector%get(dummyvec)
    bool = ((dummyvec(1) .APPROXEQ. 1.0_SRK) .AND. &
            (dummyvec(2) .APPROXEQ. 3.0_SRK) .AND. &
            (dummyvec(3) .APPROXEQ. 7.0_SRK))
    ASSERT(bool, 'BLAS_copy(THISVECTOR=xVector,NEWVECTOR=yVector,N=xVector%n,INCX=1) [REAL]')
    CALL yVector%set(0.0_SRK)
    CALL BLAS_copy(THISVECTOR=xVector,NEWVECTOR=yVector,N=xVector%n,INCY=1)
    CALL yVector%get(dummyvec)
    bool = ((dummyvec(1) .APPROXEQ. 1.0_SRK) .AND. &
            (dummyvec(2) .APPROXEQ. 3.0_SRK) .AND. &
            (dummyvec(3) .APPROXEQ. 7.0_SRK))
    ASSERT(bool, 'BLAS_copy(THISVECTOR=xVector,NEWVECTOR=yVector,N=xVector%n,INCY=1) [REAL]')
    CALL yVector%set(0.0_SRK)
    CALL BLAS_copy(THISVECTOR=xVector,NEWVECTOR=yVector,INCX=1)
    CALL yVector%get(dummyvec)
    bool = ((dummyvec(1) .APPROXEQ. 1.0_SRK) .AND. &
            (dummyvec(2) .APPROXEQ. 3.0_SRK) .AND. &
            (dummyvec(3) .APPROXEQ. 7.0_SRK))
    ASSERT(bool, 'BLAS_copy(THISVECTOR=xVector,NEWVECTOR=yVector,INCX=1) [REAL]')
    CALL yVector%set(0.0_SRK)
    CALL BLAS_copy(THISVECTOR=xVector,NEWVECTOR=yVector,INCY=1)
    CALL yVector%get(dummyvec)
    bool = ((dummyvec(1) .APPROXEQ. 1.0_SRK) .AND. &
            (dummyvec(2) .APPROXEQ. 3.0_SRK) .AND. &
            (dummyvec(3) .APPROXEQ. 7.0_SRK))
    ASSERT(bool, 'BLAS_copy(THISVECTOR=xVector,NEWVECTOR=yVector,INCY=1) [REAL]')
    CALL yVector%set(0.0_SRK)
    CALL BLAS_copy(THISVECTOR=xVector,NEWVECTOR=yVector,N=xVector%n)
    CALL yVector%get(dummyvec)
    bool = ((dummyvec(1) .APPROXEQ. 1.0_SRK) .AND. &
              (dummyvec(2) .APPROXEQ. 3.0_SRK) .AND. &
              (dummyvec(3) .APPROXEQ. 7.0_SRK))
    ASSERT(bool, 'BLAS_copy(THISVECTOR=xVector,NEWVECTOR=yVector,N=xVector%n) [REAL]')
    CALL yVector%set(0.0_SRK)
    CALL BLAS_copy(THISVECTOR=xVector,NEWVECTOR=yVector)
    CALL yVector%get(dummyvec)
    bool = ((dummyvec(1) .APPROXEQ. 1.0_SRK) .AND. &
            (dummyvec(2) .APPROXEQ. 3.0_SRK) .AND. &
            (dummyvec(3) .APPROXEQ. 7.0_SRK))
    ASSERT(bool, 'BLAS_copy(THISVECTOR=xVector,NEWVECTOR=yVector) [REAL]')
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
    bool = r .APPROXEQ. 56.0_SRK
    ASSERT(bool, 'BLAS_dot(THISVECTOR=xVector,THATVECTOR=yVector,N=xVector%n,INCX=1,INCY=1) [REAL]')
    r=0.0_SRK
    r = BLAS_dot(THISVECTOR=xVector,THATVECTOR=yVector,INCX=1,INCY=1)
    bool = r .APPROXEQ. 56.0_SRK
    ASSERT(bool, 'BLAS_dot(THISVECTOR=xVector,THATVECTOR=yVector,INCX=1,INCY=1) [REAL]')
    r=0.0_SRK
    r = BLAS_dot(THISVECTOR=xVector,THATVECTOR=yVector,N=xVector%n,INCX=1)
    bool = r .APPROXEQ. 56.0_SRK
    ASSERT(bool, 'BLAS_dot(THISVECTOR=xVector,THATVECTOR=yVector,N=xVector%n,INCX=1) [REAL]')
    r=0.0_SRK
    r = BLAS_dot(THISVECTOR=xVector,THATVECTOR=yVector,N=xVector%n,INCY=1)
    bool = r .APPROXEQ. 56.0_SRK
    ASSERT(bool, 'BLAS_dot(THISVECTOR=xVector,THATVECTOR=yVector,N=xVector%n,INCY=1) [REAL]')
    r=0.0_SRK
    r = BLAS_dot(THISVECTOR=xVector,THATVECTOR=yVector,N=xVector%n)
    bool = r .APPROXEQ. 56.0_SRK
    ASSERT(bool, 'BLAS_dot(THISVECTOR=xVector,THATVECTOR=yVector,N=xVector%n) [REAL]')
    r=0.0_SRK
    r = BLAS_dot(THISVECTOR=xVector,THATVECTOR=yVector,INCX=1)
    bool = r .APPROXEQ. 56.0_SRK
    ASSERT(bool, 'BLAS_dot(THISVECTOR=xVector,THATVECTOR=yVector,INCX=1) [REAL]')
    r=0.0_SRK
    r = BLAS_dot(THISVECTOR=xVector,THATVECTOR=yVector,INCY=1)
    bool = r .APPROXEQ. 56.0_SRK
    ASSERT(bool, 'BLAS_dot(THISVECTOR=xVector,THATVECTOR=yVector,INCY=1) [REAL]')
    r=0.0_SRK
    r = BLAS_dot(THISVECTOR=xVector,THATVECTOR=yVector)
    bool = r .APPROXEQ. 56.0_SRK
    ASSERT(bool, 'BLAS_dot(THISVECTOR=xVector,THATVECTOR=yVector) [REAL]')
    WRITE(*,*) '  Passed: CALL BLAS_dot(...) [REAL]'

    !Test BLAS_iamax
    ! first test with all positive values
    CALL xVector%set(1,12.0_SRK)
    CALL xVector%set(2, 3.0_SRK)
    CALL xVector%set(3,17.0_SRK)
    r_index = 0_SIK
    r_index = BLAS_iamax(THISVECTOR=xVector,N=xVector%n,INCX=1)
    ASSERT(r_index == 3_SIK, 'BLAS_iamax(THISVECTOR=xVector,N=xVector%n,INCX=1) [REAL]')
    r_index = 0_SIK
    r_index = BLAS_iamax(THISVECTOR=xVector,INCX=1)
    ASSERT(r_index == 3_SIK, 'BLAS_iamax(THISVECTOR=xVector,INCX=1) [REAL]')
    r_index = 0_SIK
    r_index = BLAS_iamax(THISVECTOR=xVector,N=xVector%n)
    ASSERT(r_index == 3_SIK, 'BLAS_iamax(THISVECTOR=xVector,N=xVector%n) [REAL]')
    r_index = 0_SIK
    r_index = BLAS_iamax(THISVECTOR=xVector)
    ASSERT(r_index == 3_SIK, 'BLAS_iamax(THISVECTOR=xVector) [REAL]')
    ! next test with a negative value
    CALL xVector%set(1, 12.0_SRK)
    CALL xVector%set(2,-30.0_SRK)
    CALL xVector%set(3, 17.0_SRK)
    r_index = 0_SIK
    r_index = BLAS_iamax(THISVECTOR=xVector,N=xVector%n,INCX=1)
    ASSERT(r_index == 2_SIK, 'BLAS_iamax(THISVECTOR=xVector,N=xVector%n,INCX=1) [REAL]')
    r_index = 0_SIK
    r_index = BLAS_iamax(THISVECTOR=xVector,INCX=1)
    ASSERT(r_index == 2_SIK, 'BLAS_iamax(THISVECTOR=xVector,INCX=1) [REAL]')
    r_index = 0_SIK
    r_index = BLAS_iamax(THISVECTOR=xVector,N=xVector%n)
    ASSERT(r_index == 2_SIK, 'BLAS_iamax(THISVECTOR=xVector,N=xVector%n) [REAL]')
    r_index = 0_SIK
    r_index = BLAS_iamax(THISVECTOR=xVector)
    ASSERT(r_index == 2_SIK, 'BLAS_iamax(THISVECTOR=xVector) [REAL]')
    WRITE(*,*) '  Passed: CALL BLAS_iamax(...) [REAL]'

    !Test BLAS_iamin
    ! first test with all positive values
    CALL xVector%set(1,12.0_SRK)
    CALL xVector%set(2, 3.0_SRK)
    CALL xVector%set(3,17.0_SRK)
    r_index = 0_SIK
    r_index = BLAS_iamin(THISVECTOR=xVector,N=xVector%n,INCX=1)
    ASSERT(r_index == 2_SIK, 'BLAS_iamin(THISVECTOR=xVector,N=xVector%n,INCX=1) [REAL]')
    r_index = 0_SIK
    r_index = BLAS_iamin(THISVECTOR=xVector,INCX=1)
    ASSERT(r_index == 2_SIK, 'BLAS_iamin(THISVECTOR=xVector,INCX=1) [REAL]')
    r_index = 0_SIK
    r_index = BLAS_iamin(THISVECTOR=xVector,N=xVector%n)
    ASSERT(r_index == 2_SIK, 'BLAS_iamin(THISVECTOR=xVector,N=xVector%n) [REAL]')
    r_index = 0_SIK
    r_index = BLAS_iamin(THISVECTOR=xVector)
    ASSERT(r_index == 2_SIK, 'BLAS_iamin(THISVECTOR=xVector) [REAL]')
    ! next test with a negative value
    CALL xVector%set(1, 12.0_SRK)
    CALL xVector%set(2,-30.0_SRK)
    CALL xVector%set(3, 17.0_SRK)
    r_index = 0_SIK
    r_index = BLAS_iamin(THISVECTOR=xVector,N=xVector%n,INCX=1)
    ASSERT(r_index == 1_SIK, 'BLAS_iamin(THISVECTOR=xVector,N=xVector%n,INCX=1) [REAL]')
    r_index = 0_SIK
    r_index = BLAS_iamin(THISVECTOR=xVector,INCX=1)
    ASSERT(r_index == 1_SIK, 'BLAS_iamin(THISVECTOR=xVector,INCX=1) [REAL]')
    r_index = 0_SIK
    r_index = BLAS_iamin(THISVECTOR=xVector,N=xVector%n)
    ASSERT(r_index == 1_SIK, 'BLAS_iamin(THISVECTOR=xVector,N=xVector%n) [REAL]')
    r_index = 0_SIK
    r_index = BLAS_iamin(THISVECTOR=xVector)
    ASSERT(r_index == 1_SIK, 'BLAS_iamin(THISVECTOR=xVector) [REAL]')
    WRITE(*,*) '  Passed: CALL BLAS_iamin(...) [REAL]'

    !Test BLAS_nrm2
    CALL xVector%set(1,0.0_SRK)
    CALL xVector%set(2,3.0_SRK)
    CALL xVector%set(3,4.0_SRK)
    r = 0.0_SRK
    r = BLAS_nrm2(THISVECTOR=xVector,N=xVector%n,INCX=1)
    bool = r .APPROXEQ. 5._SRK
    ASSERT(bool, 'BLAS_nrm2(THISVECTOR=xVector,N=xVector%n,INCX=1) [REAL]')
    r = 0.0_SRK
    r = BLAS_nrm2(THISVECTOR=xVector,INCX=1)
    bool = r .APPROXEQ. 5._SRK
    ASSERT(bool, 'BLAS_nrm2(THISVECTOR=xVector,INCX=1) [REAL]')
    r = 0.0_SRK
    r = BLAS_nrm2(THISVECTOR=xVector,N=xVector%n)
    bool = r .APPROXEQ. 5._SRK
    ASSERT(bool, 'BLAS_nrm2(THISVECTOR=xVector,N=xVector%n) [REAL]')
    r = 0.0_SRK
    r = BLAS_nrm2(THISVECTOR=xVector)
    bool = r .APPROXEQ. 5._SRK
    ASSERT(bool, 'BLAS_nrm2(THISVECTOR=xVector) [REAL]')
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
    bool = ((dummyvec(1) .APPROXEQ.  3._SRK) .AND. &
            (dummyvec(2) .APPROXEQ.  9._SRK) .AND. &
            (dummyvec(3) .APPROXEQ. 15._SRK))
    ASSERT(bool, 'BLAS_scal(THISVECTOR=xVector,A=a,N=xVector%n,INCX=1) -scalar_a [REAL]')
    CALL xVector%set(1,1.0_SRK)
    CALL xVector%set(2,3.0_SRK)
    CALL xVector%set(3,5.0_SRK)
    CALL BLAS_scal(THISVECTOR=xVector,A=a,INCX=1)
    CALL xVector%get(dummyvec)
    bool = ((dummyvec(1) .APPROXEQ.  3._SRK) .AND. &
            (dummyvec(2) .APPROXEQ.  9._SRK) .AND. &
            (dummyvec(3) .APPROXEQ. 15._SRK))
    ASSERT(bool, 'BLAS_scal(THISVECTOR=xVector,A=a,INCX=1) -scalar_a [REAL]')
    CALL xVector%set(1,1.0_SRK)
    CALL xVector%set(2,3.0_SRK)
    CALL xVector%set(3,5.0_SRK)
    CALL BLAS_scal(THISVECTOR=xVector,A=a,N=xVector%n)
    CALL xVector%get(dummyvec)
    bool = ((dummyvec(1) .APPROXEQ.  3._SRK) .AND. &
            (dummyvec(2) .APPROXEQ.  9._SRK) .AND. &
            (dummyvec(3) .APPROXEQ. 15._SRK))
    ASSERT(bool, 'BLAS_scal(THISVECTOR=xVector,A=a,N=xVector%n) -scalar_a [REAL]')
    CALL xVector%set(1,1.0_SRK)
    CALL xVector%set(2,3.0_SRK)
    CALL xVector%set(3,5.0_SRK)
    CALL BLAS_scal(THISVECTOR=xVector,A=a)
    CALL xVector%get(dummyvec)
    bool = ((dummyvec(1) .APPROXEQ.  3._SRK) .AND. &
            (dummyvec(2) .APPROXEQ.  9._SRK) .AND. &
            (dummyvec(3) .APPROXEQ. 15._SRK))
    ASSERT(bool, 'BLAS_scal(THISVECTOR=xVector,A=a) -scalar_a [REAL]')
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
    bool = ((dummyvec(1) .APPROXEQ.  2._SRK) .AND. &
            (dummyvec(2) .APPROXEQ. 12._SRK) .AND. &
            (dummyvec(3) .APPROXEQ.  5._SRK))
    ASSERT(bool, 'BLAS_scal(THISVECTOR=xVector,AVECTOR=aVector,N=xVector%n,INCX=1) -vector_a [REAL]')
    CALL xVector%set(1,1.0_SRK)
    CALL xVector%set(2,3.0_SRK)
    CALL xVector%set(3,5.0_SRK)
    CALL BLAS_scal(THISVECTOR=xVector,AVECTOR=aVector,INCX=1)
    CALL xVector%get(dummyvec)
    bool = ((dummyvec(1) .APPROXEQ.  2._SRK) .AND. &
            (dummyvec(2) .APPROXEQ. 12._SRK) .AND. &
            (dummyvec(3) .APPROXEQ.  5._SRK))
    ASSERT(bool, 'BLAS_scal(THISVECTOR=xVector,AVECTOR=aVector,INCX=1) -vector_a [REAL]')
    CALL xVector%set(1,1.0_SRK)
    CALL xVector%set(2,3.0_SRK)
    CALL xVector%set(3,5.0_SRK)
    CALL BLAS_scal(THISVECTOR=xVector,AVECTOR=aVector,N=xVector%n)
    CALL xVector%get(dummyvec)
    bool = ((dummyvec(1) .APPROXEQ.  2._SRK) .AND. &
            (dummyvec(2) .APPROXEQ. 12._SRK) .AND. &
            (dummyvec(3) .APPROXEQ.  5._SRK))
    ASSERT(bool, 'BLAS_scal(THISVECTOR=xVector,AVECTOR=aVector,N=xVector%n) -vector_a [REAL]')
    CALL xVector%set(1,1.0_SRK)
    CALL xVector%set(2,3.0_SRK)
    CALL xVector%set(3,5.0_SRK)
    CALL BLAS_scal(THISVECTOR=xVector,AVECTOR=aVector)
    CALL xVector%get(dummyvec)
    bool = ((dummyvec(1) .APPROXEQ.  2._SRK) .AND. &
            (dummyvec(2) .APPROXEQ. 12._SRK) .AND. &
            (dummyvec(3) .APPROXEQ.  5._SRK))
    ASSERT(bool, 'BLAS_scal(THISVECTOR=xVector,AVECTOR=aVector) -vector_a [REAL]')
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
    bool = ((dummyvec(1) .APPROXEQ. 2._SRK) .AND. &
            (dummyvec(2) .APPROXEQ. 4._SRK) .AND. &
            (dummyvec(3) .APPROXEQ. 6._SRK) .AND. &
            (dummyvec2(1) .APPROXEQ. 1._SRK) .AND. &
            (dummyvec2(2) .APPROXEQ. 3._SRK) .AND. &
            (dummyvec2(3) .APPROXEQ. 5._SRK))
    ASSERT(bool, 'BLAS_swap(THISVECTOR=xVector,THATVECTOR=yVector,N=xVector%n,INCX=1,INCY=1) [REAL]')
    CALL xVector%set(1,1.0_SRK)
    CALL xVector%set(2,3.0_SRK)
    CALL xVector%set(3,5.0_SRK)
    CALL yVector%set(1,2.0_SRK)
    CALL yVector%set(2,4.0_SRK)
    CALL yVector%set(3,6.0_SRK)
    CALL BLAS_swap(THISVECTOR=xVector,THATVECTOR=yVector,INCX=1,INCY=1)
    CALL xVector%get(dummyvec)
    CALL yVector%get(dummyvec2)
    bool = ((dummyvec(1) .APPROXEQ. 2._SRK) .AND. &
            (dummyvec(2) .APPROXEQ. 4._SRK) .AND. &
            (dummyvec(3) .APPROXEQ. 6._SRK) .AND. &
            (dummyvec2(1) .APPROXEQ. 1._SRK) .AND. &
            (dummyvec2(2) .APPROXEQ. 3._SRK) .AND. &
            (dummyvec2(3) .APPROXEQ. 5._SRK))
    ASSERT(bool, 'BLAS_swap(THISVECTOR=xVector,THATVECTOR=yVector,INCX=1,INCY=1) [REAL]')
    CALL xVector%set(1,1.0_SRK)
    CALL xVector%set(2,3.0_SRK)
    CALL xVector%set(3,5.0_SRK)
    CALL yVector%set(1,2.0_SRK)
    CALL yVector%set(2,4.0_SRK)
    CALL yVector%set(3,6.0_SRK)
    CALL BLAS_swap(THISVECTOR=xVector,THATVECTOR=yVector,N=xVector%n,INCX=1)
    CALL xVector%get(dummyvec)
    CALL yVector%get(dummyvec2)
    bool = ((dummyvec(1) .APPROXEQ. 2._SRK) .AND. &
            (dummyvec(2) .APPROXEQ. 4._SRK) .AND. &
            (dummyvec(3) .APPROXEQ. 6._SRK) .AND. &
            (dummyvec2(1) .APPROXEQ. 1._SRK) .AND. &
            (dummyvec2(2) .APPROXEQ. 3._SRK) .AND. &
            (dummyvec2(3) .APPROXEQ. 5._SRK))
    ASSERT(bool, 'BLAS_swap(THISVECTOR=xVector,THATVECTOR=yVector,N=xVector%n,INCX=1) [REAL]')
    CALL xVector%set(1,1.0_SRK)
    CALL xVector%set(2,3.0_SRK)
    CALL xVector%set(3,5.0_SRK)
    CALL yVector%set(1,2.0_SRK)
    CALL yVector%set(2,4.0_SRK)
    CALL yVector%set(3,6.0_SRK)
    CALL BLAS_swap(THISVECTOR=xVector,THATVECTOR=yVector,N=xVector%n,INCY=1)
    CALL xVector%get(dummyvec)
    CALL yVector%get(dummyvec2)
    bool = ((dummyvec(1) .APPROXEQ. 2._SRK) .AND. &
            (dummyvec(2) .APPROXEQ. 4._SRK) .AND. &
            (dummyvec(3) .APPROXEQ. 6._SRK) .AND. &
            (dummyvec2(1) .APPROXEQ. 1._SRK) .AND. &
            (dummyvec2(2) .APPROXEQ. 3._SRK) .AND. &
            (dummyvec2(3) .APPROXEQ. 5._SRK))
    ASSERT(bool, 'BLAS_swap(THISVECTOR=xVector,THATVECTOR=yVector,N=xVector%n,INCY=1) [REAL]')
    CALL xVector%set(1,1.0_SRK)
    CALL xVector%set(2,3.0_SRK)
    CALL xVector%set(3,5.0_SRK)
    CALL yVector%set(1,2.0_SRK)
    CALL yVector%set(2,4.0_SRK)
    CALL yVector%set(3,6.0_SRK)
    CALL BLAS_swap(THISVECTOR=xVector,THATVECTOR=yVector,INCX=1)
    CALL xVector%get(dummyvec)
    CALL yVector%get(dummyvec2)
    bool = ((dummyvec(1) .APPROXEQ. 2._SRK) .AND. &
            (dummyvec(2) .APPROXEQ. 4._SRK) .AND. &
            (dummyvec(3) .APPROXEQ. 6._SRK) .AND. &
            (dummyvec2(1) .APPROXEQ. 1._SRK) .AND. &
            (dummyvec2(2) .APPROXEQ. 3._SRK) .AND. &
            (dummyvec2(3) .APPROXEQ. 5._SRK))
    ASSERT(bool, 'BLAS_swap(THISVECTOR=xVector,THATVECTOR=yVector,INCX=1) [REAL]')
    CALL xVector%set(1,1.0_SRK)
    CALL xVector%set(2,3.0_SRK)
    CALL xVector%set(3,5.0_SRK)
    CALL yVector%set(1,2.0_SRK)
    CALL yVector%set(2,4.0_SRK)
    CALL yVector%set(3,6.0_SRK)
    CALL BLAS_swap(THISVECTOR=xVector,THATVECTOR=yVector,INCY=1)
    CALL xVector%get(dummyvec)
    CALL yVector%get(dummyvec2)
    bool = ((dummyvec(1) .APPROXEQ. 2._SRK) .AND. &
            (dummyvec(2) .APPROXEQ. 4._SRK) .AND. &
            (dummyvec(3) .APPROXEQ. 6._SRK) .AND. &
            (dummyvec2(1) .APPROXEQ. 1._SRK) .AND. &
            (dummyvec2(2) .APPROXEQ. 3._SRK) .AND. &
            (dummyvec2(3) .APPROXEQ. 5._SRK))
    ASSERT(bool, 'BLAS_swap(THISVECTOR=xVector,THATVECTOR=yVector,INCY=1) [REAL]')
    CALL xVector%set(1,1.0_SRK)
    CALL xVector%set(2,3.0_SRK)
    CALL xVector%set(3,5.0_SRK)
    CALL yVector%set(1,2.0_SRK)
    CALL yVector%set(2,4.0_SRK)
    CALL yVector%set(3,6.0_SRK)
    CALL BLAS_swap(THISVECTOR=xVector,THATVECTOR=yVector,N=xVector%n)
    CALL xVector%get(dummyvec)
    CALL yVector%get(dummyvec2)
    bool = ((dummyvec(1) .APPROXEQ. 2._SRK) .AND. &
            (dummyvec(2) .APPROXEQ. 4._SRK) .AND. &
            (dummyvec(3) .APPROXEQ. 6._SRK) .AND. &
            (dummyvec2(1) .APPROXEQ. 1._SRK) .AND. &
            (dummyvec2(2) .APPROXEQ. 3._SRK) .AND. &
            (dummyvec2(3) .APPROXEQ. 5._SRK))
    ASSERT(bool, 'BLAS_swap(THISVECTOR=xVector,THATVECTOR=yVector,N=xVector%n) [REAL]')
    CALL xVector%set(1,1.0_SRK)
    CALL xVector%set(2,3.0_SRK)
    CALL xVector%set(3,5.0_SRK)
    CALL yVector%set(1,2.0_SRK)
    CALL yVector%set(2,4.0_SRK)
    CALL yVector%set(3,6.0_SRK)
    CALL BLAS_swap(THISVECTOR=xVector,THATVECTOR=yVector)
    CALL xVector%get(dummyvec)
    CALL yVector%get(dummyvec2)
    bool = ((dummyvec(1) .APPROXEQ. 2._SRK) .AND. &
            (dummyvec(2) .APPROXEQ. 4._SRK) .AND. &
            (dummyvec(3) .APPROXEQ. 6._SRK) .AND. &
            (dummyvec2(1) .APPROXEQ. 1._SRK) .AND. &
            (dummyvec2(2) .APPROXEQ. 3._SRK) .AND. &
            (dummyvec2(3) .APPROXEQ. 5._SRK))
    ASSERT(bool, 'BLAS_swap(THISVECTOR=xVector,THATVECTOR=yVector) [REAL]')
    CALL xVector%clear()
    CALL yVector%clear()
    CALL aVector%clear()
    DEALLOCATE(xVector)
    DEALLOCATE(yVector)
    DEALLOCATE(aVector)
    WRITE(*,*) '  Passed: CALL BLAS_swap(...) [REAL]'

#ifdef FUTILITY_HAVE_PETSC
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
    ASSERT(r .APPROXEQ. 14.0_SRK, 'BLAS_asum(THISVECTOR=xVector,N=xVector%n,INCX=1) [PETSC]')
    r=0.0_SRK
    r = BLAS_asum(THISVECTOR=xVector,INCX=1)
    ASSERT(r .APPROXEQ. 14.0_SRK, 'BLAS_asum(THISVECTOR=xVector,INCX=1) [PETSC]')
    r=0.0_SRK
    r = BLAS_asum(THISVECTOR=xVector,N=xVector%n)
    ASSERT(r .APPROXEQ. 14.0_SRK, 'BLAS_asum(THISVECTOR=xVector,N=xVector%n) [PETSC]')
    r=0.0_SRK
    r = BLAS_asum(THISVECTOR=xVector)
    ASSERT(r .APPROXEQ. 14.0_SRK, 'BLAS_asum(THISVECTOR=xVector) [PETSC]')
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
    bool = ((dummyvec(1) .APPROXEQ. 28._SRK) .AND. &
            (dummyvec(2) .APPROXEQ. -5._SRK) .AND. &
            (dummyvec(3) .APPROXEQ. 36._SRK))
    ASSERT(bool, 'BLAS_axpy')
    FINFO() '(THISVECTOR=xVector,NEWVECTOR=yVector,A=a,N=xVector%n,INCX=1,INCY=1) -scalar_a [PETSC]'
    CALL yVector%set(1,3.0_SRK)
    CALL yVector%set(2,5.0_SRK)
    CALL yVector%set(3,1.0_SRK)
    CALL BLAS_axpy(THISVECTOR=xVector,NEWVECTOR=yVector,A=a,N=xVector%n,INCX=1)
    CALL yVector%get(dummyvec)
    bool = (dummyvec(1) .APPROXEQ. 28._SRK) .AND. &
            (dummyvec(2) .APPROXEQ. -5._SRK) .AND. &
            (dummyvec(3) .APPROXEQ. 36._SRK)
    ASSERT(bool, 'BLAS_axpy')
    FINFO() '(THISVECTOR=xVector,NEWVECTOR=yVector,A=a,N=xVector%n,INCX=1) -scalar_a [PETSC]'
    CALL yVector%set(1,3.0_SRK)
    CALL yVector%set(2,5.0_SRK)
    CALL yVector%set(3,1.0_SRK)
    CALL BLAS_axpy(THISVECTOR=xVector,NEWVECTOR=yVector,A=a,N=xVector%n,INCY=1)
    CALL yVector%get(dummyvec)
    bool = ((dummyvec(1) .APPROXEQ. 28._SRK) .AND. &
            (dummyvec(2) .APPROXEQ. -5._SRK) .AND. &
            (dummyvec(3) .APPROXEQ. 36._SRK))
    ASSERT(bool, 'BLAS_axpy')
    FINFO() '(THISVECTOR=xVector,NEWVECTOR=yVector,A=a,N=xVector%n,INCY=1) -scalar_a [PETSC]'
    CALL yVector%set(1,3.0_SRK)
    CALL yVector%set(2,5.0_SRK)
    CALL yVector%set(3,1.0_SRK)
    CALL BLAS_axpy(THISVECTOR=xVector,NEWVECTOR=yVector,A=a,N=xVector%n)
    CALL yVector%get(dummyvec)
    bool = ((dummyvec(1) .APPROXEQ. 28._SRK) .AND. &
            (dummyvec(2) .APPROXEQ. -5._SRK) .AND. &
            (dummyvec(3) .APPROXEQ. 36._SRK))
    ASSERT(bool, 'BLAS_axpy')
    FINFO() '(THISVECTOR=xVector,NEWVECTOR=yVector,A=a,N=xVector%n) -scalar_a [PETSC]'
    CALL yVector%set(1,3.0_SRK)
    CALL yVector%set(2,5.0_SRK)
    CALL yVector%set(3,1.0_SRK)
    CALL BLAS_axpy(THISVECTOR=xVector,NEWVECTOR=yVector,A=a)
    CALL yVector%get(dummyvec)
    bool = ((dummyvec(1) .APPROXEQ. 28._SRK) .AND. &
            (dummyvec(2) .APPROXEQ. -5._SRK) .AND. &
            (dummyvec(3) .APPROXEQ. 36._SRK))
    ASSERT(bool, 'BLAS_axpy(THISVECTOR=xVector,NEWVECTOR=yVector,A=a) -scalar_a [PETSC]')
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
    bool = ((dummyvec(1) .APPROXEQ. 33._SRK) .AND. &
            (dummyvec(2) .APPROXEQ. -3._SRK) .AND. &
            (dummyvec(3) .APPROXEQ. 15._SRK))
    ASSERT(bool, "BLAS_axpy")
    FINFO() "(THISVECTOR=xVector,NEWVECTOR=yVector,AVECTOR=aVector,N=xVector%n,INCX=1,INCY=1)"
    FINFO() " -vector_a [PETSC]"
    CALL yVector%set(1,3.0_SRK)
    CALL yVector%set(2,5.0_SRK)
    CALL yVector%set(3,1.0_SRK)
    CALL BLAS_axpy(THISVECTOR=xVector,NEWVECTOR=yVector,AVECTOR=aVector,N=xVector%n,INCX=1)
    CALL yVector%get(dummyvec)
    bool = ((dummyvec(1) .APPROXEQ. 33._SRK) .AND. &
            (dummyvec(2) .APPROXEQ. -3._SRK) .AND. &
            (dummyvec(3) .APPROXEQ. 15._SRK))
    ASSERT(bool, 'BLAS_axpy')
    FINFO() '(THISVECTOR=xVector,NEWVECTOR=yVector,AVECTOR=aVector,N=xVector%n,INCX=1) -vector_a [PETSC]'
    CALL yVector%set(1,3.0_SRK)
    CALL yVector%set(2,5.0_SRK)
    CALL yVector%set(3,1.0_SRK)
    CALL BLAS_axpy(THISVECTOR=xVector,NEWVECTOR=yVector,AVECTOR=aVector,N=xVector%n,INCY=1)
    CALL yVector%get(dummyvec)
    bool = (dummyvec(1) .APPROXEQ. 33._SRK) .AND. &
            (dummyvec(2) .APPROXEQ. -3._SRK) .AND. &
            (dummyvec(3) .APPROXEQ. 15._SRK)
    ASSERT(bool, 'BLAS_axpy')
    FINFO() '(THISVECTOR=xVector,NEWVECTOR=yVector,AVECTOR=aVector,N=xVector%n,INCY=1) -vector_a [PETSC]'
    CALL yVector%set(1,3.0_SRK)
    CALL yVector%set(2,5.0_SRK)
    CALL yVector%set(3,1.0_SRK)
    CALL BLAS_axpy(THISVECTOR=xVector,NEWVECTOR=yVector,AVECTOR=aVector,N=xVector%n)
    CALL yVector%get(dummyvec)
    bool = ((dummyvec(1) .APPROXEQ. 33._SRK) .AND. &
            (dummyvec(2) .APPROXEQ. -3._SRK) .AND. &
            (dummyvec(3) .APPROXEQ. 15._SRK))
    ASSERT(bool, 'BLAS_axpy')
    FINFO() '(THISVECTOR=xVector,NEWVECTOR=yVector,AVECTOR=aVector,N=xVector%n) -vector_a [PETSC]'
    CALL yVector%set(1,3.0_SRK)
    CALL yVector%set(2,5.0_SRK)
    CALL yVector%set(3,1.0_SRK)
    CALL BLAS_axpy(THISVECTOR=xVector,NEWVECTOR=yVector,AVECTOR=aVector)
    CALL yVector%get(dummyvec)
    bool = (dummyvec(1) .APPROXEQ. 33._SRK) .AND. &
            (dummyvec(2) .APPROXEQ. -3._SRK) .AND. &
            (dummyvec(3) .APPROXEQ. 15._SRK)
    ASSERT(bool, 'BLAS_axpy')
    FINFO() '(THISVECTOR=xVector,NEWVECTOR=yVector,AVECTOR=aVector) -vector_a [PETSC]'
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
    bool = (dummyvec(1) .APPROXEQ. 1.0_SRK) .AND. &
            (dummyvec(2) .APPROXEQ. 3.0_SRK) .AND. &
            (dummyvec(3) .APPROXEQ. 7.0_SRK)
    ASSERT(bool, 'BLAS_copy')
    FINFO() '(THISVECTOR=xVector,NEWVECTOR=yVector,N=xVector%n,INCX=1,INCY=1) [PETSC]'
    CALL yVector%set(0.0_SRK)
    CALL BLAS_copy(THISVECTOR=xVector,NEWVECTOR=yVector,INCX=1,INCY=1)
    CALL yVector%get(dummyvec)
    bool = (dummyvec(1) .APPROXEQ. 1.0_SRK) .AND. &
            (dummyvec(2) .APPROXEQ. 3.0_SRK) .AND. &
            (dummyvec(3) .APPROXEQ. 7.0_SRK)
    ASSERT(bool, 'BLAS_copy(THISVECTOR=xVector,NEWVECTOR=yVector,INCX=1,INCY=1) [PETSC]')
    CALL yVector%set(0.0_SRK)
    CALL BLAS_copy(THISVECTOR=xVector,NEWVECTOR=yVector,N=xVector%n,INCX=1)
    CALL yVector%get(dummyvec)
    bool = (dummyvec(1) .APPROXEQ. 1.0_SRK) .AND. &
            (dummyvec(2) .APPROXEQ. 3.0_SRK) .AND. &
            (dummyvec(3) .APPROXEQ. 7.0_SRK)
    ASSERT(bool, 'BLAS_copy(THISVECTOR=xVector,NEWVECTOR=yVector,N=xVector%n,INCX=1) [PETSC]')
    CALL yVector%set(0.0_SRK)
    CALL BLAS_copy(THISVECTOR=xVector,NEWVECTOR=yVector,N=xVector%n,INCY=1)
    CALL yVector%get(dummyvec)
    bool = (dummyvec(1) .APPROXEQ. 1.0_SRK) .AND. &
           (dummyvec(2) .APPROXEQ. 3.0_SRK) .AND. &
           (dummyvec(3) .APPROXEQ. 7.0_SRK)
    ASSERT(bool, 'BLAS_copy(THISVECTOR=xVector,NEWVECTOR=yVector,N=xVector%n,INCY=1) [PETSC]')
    CALL yVector%set(0.0_SRK)
    CALL BLAS_copy(THISVECTOR=xVector,NEWVECTOR=yVector,INCX=1)
    CALL yVector%get(dummyvec)
    bool = (dummyvec(1) .APPROXEQ. 1.0_SRK) .AND. &
           (dummyvec(2) .APPROXEQ. 3.0_SRK) .AND. &
           (dummyvec(3) .APPROXEQ. 7.0_SRK)
    ASSERT(bool, 'BLAS_copy(THISVECTOR=xVector,NEWVECTOR=yVector,INCX=1) [PETSC]')
    CALL yVector%set(0.0_SRK)
    CALL BLAS_copy(THISVECTOR=xVector,NEWVECTOR=yVector,INCY=1)
    CALL yVector%get(dummyvec)
    bool = ((dummyvec(1) .APPROXEQ. 1.0_SRK) .AND. &
            (dummyvec(2) .APPROXEQ. 3.0_SRK) .AND. &
            (dummyvec(3) .APPROXEQ. 7.0_SRK))
    ASSERT(bool, 'BLAS_copy(THISVECTOR=xVector,NEWVECTOR=yVector,INCY=1) [PETSC]')
    CALL yVector%set(0.0_SRK)
    CALL BLAS_copy(THISVECTOR=xVector,NEWVECTOR=yVector,N=xVector%n)
    CALL yVector%get(dummyvec)
    bool = (dummyvec(1) .APPROXEQ. 1.0_SRK) .AND. &
           (dummyvec(2) .APPROXEQ. 3.0_SRK) .AND. &
           (dummyvec(3) .APPROXEQ. 7.0_SRK)
    ASSERT(bool, 'BLAS_copy(THISVECTOR=xVector,NEWVECTOR=yVector,N=xVector%n) [PETSC]')
    CALL yVector%set(0.0_SRK)
    CALL BLAS_copy(THISVECTOR=xVector,NEWVECTOR=yVector)
    CALL yVector%get(dummyvec)
    bool = (dummyvec(1) .APPROXEQ. 1.0_SRK) .AND. &
           (dummyvec(2) .APPROXEQ. 3.0_SRK) .AND. &
           (dummyvec(3) .APPROXEQ. 7.0_SRK)
    ASSERT(bool, 'BLAS_copy(THISVECTOR=xVector,NEWVECTOR=yVector) [PETSC]')
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
    bool = r .APPROXEQ. 56.0_SRK
    ASSERT(bool, 'BLAS_dot(THISVECTOR=xVector,THATVECTOR=yVector,N=xVector%n,INCX=1,INCY=1) [PETSC]')
    r=0.0_SRK
    r = BLAS_dot(THISVECTOR=xVector,THATVECTOR=yVector,INCX=1,INCY=1)
    bool = r .APPROXEQ. 56.0_SRK
    ASSERT(bool, 'BLAS_dot(THISVECTOR=xVector,THATVECTOR=yVector,INCX=1,INCY=1) [PETSC]')
    r=0.0_SRK
    r = BLAS_dot(THISVECTOR=xVector,THATVECTOR=yVector,N=xVector%n,INCX=1)
    bool = r .APPROXEQ. 56.0_SRK
    ASSERT(bool, 'BLAS_dot(THISVECTOR=xVector,THATVECTOR=yVector,N=xVector%n,INCX=1) [PETSC]')
    r=0.0_SRK
    r = BLAS_dot(THISVECTOR=xVector,THATVECTOR=yVector,N=xVector%n,INCY=1)
    bool = r .APPROXEQ. 56.0_SRK
    ASSERT(bool, 'BLAS_dot(THISVECTOR=xVector,THATVECTOR=yVector,N=xVector%n,INCY=1) [PETSC]')
    r=0.0_SRK
    r = BLAS_dot(THISVECTOR=xVector,THATVECTOR=yVector,N=xVector%n)
    bool = r .APPROXEQ. 56.0_SRK
    ASSERT(bool, 'BLAS_dot(THISVECTOR=xVector,THATVECTOR=yVector,N=xVector%n) [PETSC]')
    r=0.0_SRK
    r = BLAS_dot(THISVECTOR=xVector,THATVECTOR=yVector,INCX=1)
    bool = r .APPROXEQ. 56.0_SRK
    ASSERT(bool, 'BLAS_dot(THISVECTOR=xVector,THATVECTOR=yVector,INCX=1) [PETSC]')
    r=0.0_SRK
    r = BLAS_dot(THISVECTOR=xVector,THATVECTOR=yVector,INCY=1)
    bool = r .APPROXEQ. 56.0_SRK
    ASSERT(bool, 'BLAS_dot(THISVECTOR=xVector,THATVECTOR=yVector,INCY=1) [PETSC]')
    r=0.0_SRK
    r = BLAS_dot(THISVECTOR=xVector,THATVECTOR=yVector)
    bool = r .APPROXEQ. 56.0_SRK
    ASSERT(bool, 'BLAS_dot(THISVECTOR=xVector,THATVECTOR=yVector) [PETSC]')
    WRITE(*,*) '  Passed: CALL BLAS_dot(...) [PETSC]'

    !Test BLAS_iamax
    ! first test with all positive values
    CALL xVector%set(1,12.0_SRK)
    CALL xVector%set(2, 3.0_SRK)
    CALL xVector%set(3,17.0_SRK)
    r_index = 0_SIK
    r_index = BLAS_iamax(THISVECTOR=xVector,N=xVector%n,INCX=1)
    ASSERT(r_index == 3_SIK, 'BLAS_iamax(THISVECTOR=xVector,N=xVector%n,INCX=1) [PETSC]')
    r_index = 0_SIK
    r_index = BLAS_iamax(THISVECTOR=xVector,INCX=1)
    ASSERT(r_index == 3_SIK, 'BLAS_iamax(THISVECTOR=xVector,INCX=1) [PETSC]')
    r_index = 0_SIK
    r_index = BLAS_iamax(THISVECTOR=xVector,N=xVector%n)
    ASSERT(r_index == 3_SIK, 'BLAS_iamax(THISVECTOR=xVector,N=xVector%n) [PETSC]')
    r_index = 0_SIK
    r_index = BLAS_iamax(THISVECTOR=xVector)
    ASSERT(r_index == 3_SIK, 'BLAS_iamax(THISVECTOR=xVector) [PETSC]')
    ! next test with a negative value
    CALL xVector%set(1, 12.0_SRK)
    CALL xVector%set(2,-30.0_SRK)
    CALL xVector%set(3, 17.0_SRK)
    r_index = 0_SIK
    r_index = BLAS_iamax(THISVECTOR=xVector,N=xVector%n,INCX=1)
    ASSERT(r_index == 2_SIK, 'BLAS_iamax(THISVECTOR=xVector,N=xVector%n,INCX=1) [PETSC]')
    r_index = 0_SIK
    r_index = BLAS_iamax(THISVECTOR=xVector,INCX=1)
    ASSERT(r_index == 2_SIK, 'BLAS_iamax(THISVECTOR=xVector,INCX=1) [PETSC]')
    r_index = 0_SIK
    r_index = BLAS_iamax(THISVECTOR=xVector,N=xVector%n)
    ASSERT(r_index == 2_SIK, 'BLAS_iamax(THISVECTOR=xVector,N=xVector%n) [PETSC]')
    r_index = 0_SIK
    r_index = BLAS_iamax(THISVECTOR=xVector)
    ASSERT(r_index == 2_SIK, 'BLAS_iamax(THISVECTOR=xVector) [PETSC]')
    WRITE(*,*) '  Passed: CALL BLAS_iamax(...) [PETSC]'

    !Test BLAS_iamin
    ! first test with all positive values
    CALL xVector%set(1,12.0_SRK)
    CALL xVector%set(2, 3.0_SRK)
    CALL xVector%set(3,17.0_SRK)
    r_index = 0_SIK
    r_index = BLAS_iamin(THISVECTOR=xVector,N=xVector%n,INCX=1)
    ASSERT(r_index == 2_SIK, 'BLAS_iamin(THISVECTOR=xVector,N=xVector%n,INCX=1) [PETSC]')
    r_index = 0_SIK
    r_index = BLAS_iamin(THISVECTOR=xVector,INCX=1)
    ASSERT(r_index == 2_SIK, 'BLAS_iamin(THISVECTOR=xVector,INCX=1) [PETSC]')
    r_index = 0_SIK
    r_index = BLAS_iamin(THISVECTOR=xVector,N=xVector%n)
    ASSERT(r_index == 2_SIK, 'BLAS_iamin(THISVECTOR=xVector,N=xVector%n) [PETSC]')
    r_index = 0_SIK
    r_index = BLAS_iamin(THISVECTOR=xVector)
    ASSERT(r_index == 2_SIK, 'BLAS_iamin(THISVECTOR=xVector) [PETSC]')
    ! next test with a negative value
    CALL xVector%set(1, 12.0_SRK)
    CALL xVector%set(2,-30.0_SRK)
    CALL xVector%set(3, 17.0_SRK)
    r_index = 0_SIK
    r_index = BLAS_iamin(THISVECTOR=xVector,N=xVector%n,INCX=1)
    ASSERT(r_index == 1_SIK, 'BLAS_iamin(THISVECTOR=xVector,N=xVector%n,INCX=1) [PETSC]')
    r_index = 0_SIK
    r_index = BLAS_iamin(THISVECTOR=xVector,INCX=1)
    ASSERT(r_index == 1_SIK, 'BLAS_iamin(THISVECTOR=xVector,INCX=1) [PETSC]')
    r_index = 0_SIK
    r_index = BLAS_iamin(THISVECTOR=xVector,N=xVector%n)
    ASSERT(r_index == 1_SIK, 'BLAS_iamin(THISVECTOR=xVector,N=xVector%n) [PETSC]')
    r_index = 0_SIK
    r_index = BLAS_iamin(THISVECTOR=xVector)
    ASSERT(r_index == 1_SIK, 'BLAS_iamin(THISVECTOR=xVector) [PETSC]')
    WRITE(*,*) '  Passed: CALL BLAS_iamin(...) [PETSC]'

    !Test BLAS_nrm2
    CALL xVector%set(1,0.0_SRK)
    CALL xVector%set(2,3.0_SRK)
    CALL xVector%set(3,4.0_SRK)
    r = 0.0_SRK
    r = BLAS_nrm2(THISVECTOR=xVector,N=xVector%n,INCX=1)
    bool = r .APPROXEQ. 5._SRK
    ASSERT(bool, 'BLAS_nrm2(THISVECTOR=xVector,N=xVector%n,INCX=1) [PETSC]')
    r = 0.0_SRK
    r = BLAS_nrm2(THISVECTOR=xVector,INCX=1)
    bool = r .APPROXEQ. 5._SRK
    ASSERT(bool, 'BLAS_nrm2(THISVECTOR=xVector,INCX=1) [PETSC]')
    r = 0.0_SRK
    r = BLAS_nrm2(THISVECTOR=xVector,N=xVector%n)
    bool = r .APPROXEQ. 5._SRK
    ASSERT(bool, 'BLAS_nrm2(THISVECTOR=xVector,N=xVector%n) [PETSC]')
    r = 0.0_SRK
    r = BLAS_nrm2(THISVECTOR=xVector)
    bool = r .APPROXEQ. 5._SRK
    ASSERT(bool, 'BLAS_nrm2(THISVECTOR=xVector) [PETSC]')
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
    bool = (dummyvec(1) .APPROXEQ.  3._SRK) .AND. &
            (dummyvec(2) .APPROXEQ.  9._SRK) .AND. &
            (dummyvec(3) .APPROXEQ. 15._SRK)
    ASSERT(bool, 'BLAS_scal(THISVECTOR=xVector,A=a,N=xVector%n,INCX=1) -scalar_a [PETSC]')
    CALL xVector%set(1,1.0_SRK)
    CALL xVector%set(2,3.0_SRK)
    CALL xVector%set(3,5.0_SRK)
    CALL BLAS_scal(THISVECTOR=xVector,A=a,INCX=1)
    CALL xVector%get(dummyvec)
    bool = (dummyvec(1) .APPROXEQ.  3._SRK) .AND. &
            (dummyvec(2) .APPROXEQ.  9._SRK) .AND. &
            (dummyvec(3) .APPROXEQ. 15._SRK)
    ASSERT(bool, 'BLAS_scal(THISVECTOR=xVector,A=a,INCX=1) -scalar_a [PETSC]')
    CALL xVector%set(1,1.0_SRK)
    CALL xVector%set(2,3.0_SRK)
    CALL xVector%set(3,5.0_SRK)
    CALL BLAS_scal(THISVECTOR=xVector,A=a,N=xVector%n)
    CALL xVector%get(dummyvec)
    bool = (dummyvec(1) .APPROXEQ.  3._SRK) .AND. &
            (dummyvec(2) .APPROXEQ.  9._SRK) .AND. &
            (dummyvec(3) .APPROXEQ. 15._SRK)
    ASSERT(bool, 'BLAS_scal(THISVECTOR=xVector,A=a,N=xVector%n) -scalar_a [PETSC]')
    CALL xVector%set(1,1.0_SRK)
    CALL xVector%set(2,3.0_SRK)
    CALL xVector%set(3,5.0_SRK)
    CALL BLAS_scal(THISVECTOR=xVector,A=a)
    CALL xVector%get(dummyvec)
    bool = (dummyvec(1) .APPROXEQ.  3._SRK) .AND. &
            (dummyvec(2) .APPROXEQ.  9._SRK) .AND. &
            (dummyvec(3) .APPROXEQ. 15._SRK)
    ASSERT(bool, 'BLAS_scal(THISVECTOR=xVector,A=a) -scalar_a [PETSC]')
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
    bool = (dummyvec(1) .APPROXEQ.  2._SRK) .AND. &
           (dummyvec(2) .APPROXEQ. 12._SRK) .AND. &
           (dummyvec(3) .APPROXEQ.  5._SRK)
    ASSERT(bool, 'BLAS_scal(THISVECTOR=xVector,AVECTOR=aVector,N=xVector%n,INCX=1) -vector_a [PETSC]')
    CALL xVector%set(1,1.0_SRK)
    CALL xVector%set(2,3.0_SRK)
    CALL xVector%set(3,5.0_SRK)
    CALL BLAS_scal(THISVECTOR=xVector,AVECTOR=aVector,INCX=1)
    CALL xVector%get(dummyvec)
    bool = (dummyvec(1) .APPROXEQ.  2._SRK) .AND. &
           (dummyvec(2) .APPROXEQ. 12._SRK) .AND. &
           (dummyvec(3) .APPROXEQ.  5._SRK)
    ASSERT(bool, 'BLAS_scal(THISVECTOR=xVector,AVECTOR=aVector,INCX=1) -vector_a [PETSC]')
    CALL xVector%set(1,1.0_SRK)
    CALL xVector%set(2,3.0_SRK)
    CALL xVector%set(3,5.0_SRK)
    CALL BLAS_scal(THISVECTOR=xVector,AVECTOR=aVector,N=xVector%n)
    CALL xVector%get(dummyvec)
    bool = (dummyvec(1) .APPROXEQ.  2._SRK) .AND. &
           (dummyvec(2) .APPROXEQ. 12._SRK) .AND. &
           (dummyvec(3) .APPROXEQ.  5._SRK)
    ASSERT(bool, 'BLAS_scal(THISVECTOR=xVector,AVECTOR=aVector,N=xVector%n) -vector_a [PETSC]')
    CALL xVector%set(1,1.0_SRK)
    CALL xVector%set(2,3.0_SRK)
    CALL xVector%set(3,5.0_SRK)
    CALL BLAS_scal(THISVECTOR=xVector,AVECTOR=aVector)
    CALL xVector%get(dummyvec)
    bool = (dummyvec(1) .APPROXEQ.  2._SRK) .AND. &
           (dummyvec(2) .APPROXEQ. 12._SRK) .AND. &
           (dummyvec(3) .APPROXEQ.  5._SRK)
    ASSERT(bool, 'BLAS_scal(THISVECTOR=xVector,AVECTOR=aVector) -vector_a [PETSC]')
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
    bool = (dummyvec(1) .APPROXEQ. 2._SRK) .AND. &
           (dummyvec(2) .APPROXEQ. 4._SRK) .AND. &
           (dummyvec(3) .APPROXEQ. 6._SRK) .AND. &
           (dummyvec2(1) .APPROXEQ. 1._SRK) .AND. &
           (dummyvec2(2) .APPROXEQ. 3._SRK) .AND. &
           (dummyvec2(3) .APPROXEQ. 5._SRK)
    ASSERT(bool, 'BLAS_swap(THISVECTOR=xVector,THATVECTOR=yVector,N=xVector%n,INCX=1,INCY=1) [PETSC]')
    CALL xVector%set(1,1.0_SRK)
    CALL xVector%set(2,3.0_SRK)
    CALL xVector%set(3,5.0_SRK)
    CALL yVector%set(1,2.0_SRK)
    CALL yVector%set(2,4.0_SRK)
    CALL yVector%set(3,6.0_SRK)
    CALL BLAS_swap(THISVECTOR=xVector,THATVECTOR=yVector,INCX=1,INCY=1)
    CALL xVector%get(dummyvec)
    CALL yVector%get(dummyvec2)
    bool = (dummyvec(1) .APPROXEQ. 2._SRK) .AND. &
           (dummyvec(2) .APPROXEQ. 4._SRK) .AND. &
           (dummyvec(3) .APPROXEQ. 6._SRK) .AND. &
           (dummyvec2(1) .APPROXEQ. 1._SRK) .AND. &
           (dummyvec2(2) .APPROXEQ. 3._SRK) .AND. &
           (dummyvec2(3) .APPROXEQ. 5._SRK)
    ASSERT(bool, 'BLAS_swap(THISVECTOR=xVector,THATVECTOR=yVector,INCX=1,INCY=1) [PETSC]')
    CALL xVector%set(1,1.0_SRK)
    CALL xVector%set(2,3.0_SRK)
    CALL xVector%set(3,5.0_SRK)
    CALL yVector%set(1,2.0_SRK)
    CALL yVector%set(2,4.0_SRK)
    CALL yVector%set(3,6.0_SRK)
    CALL BLAS_swap(THISVECTOR=xVector,THATVECTOR=yVector,N=xVector%n,INCX=1)
    CALL xVector%get(dummyvec)
    CALL yVector%get(dummyvec2)
    bool = (dummyvec(1) .APPROXEQ. 2._SRK) .AND. &
           (dummyvec(2) .APPROXEQ. 4._SRK) .AND. &
           (dummyvec(3) .APPROXEQ. 6._SRK) .AND. &
           (dummyvec2(1) .APPROXEQ. 1._SRK) .AND. &
           (dummyvec2(2) .APPROXEQ. 3._SRK) .AND. &
           (dummyvec2(3) .APPROXEQ. 5._SRK)
    ASSERT(bool, 'BLAS_swap(THISVECTOR=xVector,THATVECTOR=yVector,N=xVector%n,INCX=1) [PETSC]')
    CALL xVector%set(1,1.0_SRK)
    CALL xVector%set(2,3.0_SRK)
    CALL xVector%set(3,5.0_SRK)
    CALL yVector%set(1,2.0_SRK)
    CALL yVector%set(2,4.0_SRK)
    CALL yVector%set(3,6.0_SRK)
    CALL BLAS_swap(THISVECTOR=xVector,THATVECTOR=yVector,N=xVector%n,INCY=1)
    CALL xVector%get(dummyvec)
    CALL yVector%get(dummyvec2)
    bool = (dummyvec(1) .APPROXEQ. 2._SRK) .AND. &
           (dummyvec(2) .APPROXEQ. 4._SRK) .AND. &
           (dummyvec(3) .APPROXEQ. 6._SRK) .AND. &
           (dummyvec2(1) .APPROXEQ. 1._SRK) .AND. &
           (dummyvec2(2) .APPROXEQ. 3._SRK) .AND. &
           (dummyvec2(3) .APPROXEQ. 5._SRK)
    ASSERT(bool, 'BLAS_swap(THISVECTOR=xVector,THATVECTOR=yVector,N=xVector%n,INCY=1) [PETSC]')
    CALL xVector%set(1,1.0_SRK)
    CALL xVector%set(2,3.0_SRK)
    CALL xVector%set(3,5.0_SRK)
    CALL yVector%set(1,2.0_SRK)
    CALL yVector%set(2,4.0_SRK)
    CALL yVector%set(3,6.0_SRK)
    CALL BLAS_swap(THISVECTOR=xVector,THATVECTOR=yVector,INCX=1)
    CALL xVector%get(dummyvec)
    CALL yVector%get(dummyvec2)
    bool = (dummyvec(1) .APPROXEQ. 2._SRK) .AND. &
           (dummyvec(2) .APPROXEQ. 4._SRK) .AND. &
           (dummyvec(3) .APPROXEQ. 6._SRK) .AND. &
           (dummyvec2(1) .APPROXEQ. 1._SRK) .AND. &
           (dummyvec2(2) .APPROXEQ. 3._SRK) .AND. &
           (dummyvec2(3) .APPROXEQ. 5._SRK)
    ASSERT(bool, 'BLAS_swap(THISVECTOR=xVector,THATVECTOR=yVector,INCX=1) [PETSC]')
    CALL xVector%set(1,1.0_SRK)
    CALL xVector%set(2,3.0_SRK)
    CALL xVector%set(3,5.0_SRK)
    CALL yVector%set(1,2.0_SRK)
    CALL yVector%set(2,4.0_SRK)
    CALL yVector%set(3,6.0_SRK)
    CALL BLAS_swap(THISVECTOR=xVector,THATVECTOR=yVector,INCY=1)
    CALL xVector%get(dummyvec)
    CALL yVector%get(dummyvec2)
    bool = (dummyvec(1) .APPROXEQ. 2._SRK) .AND. &
           (dummyvec(2) .APPROXEQ. 4._SRK) .AND. &
           (dummyvec(3) .APPROXEQ. 6._SRK) .AND. &
           (dummyvec2(1) .APPROXEQ. 1._SRK) .AND. &
           (dummyvec2(2) .APPROXEQ. 3._SRK) .AND. &
           (dummyvec2(3) .APPROXEQ. 5._SRK)
    ASSERT(bool, 'BLAS_swap(THISVECTOR=xVector,THATVECTOR=yVector,INCY=1) [PETSC]')
    CALL xVector%set(1,1.0_SRK)
    CALL xVector%set(2,3.0_SRK)
    CALL xVector%set(3,5.0_SRK)
    CALL yVector%set(1,2.0_SRK)
    CALL yVector%set(2,4.0_SRK)
    CALL yVector%set(3,6.0_SRK)
    CALL BLAS_swap(THISVECTOR=xVector,THATVECTOR=yVector,N=xVector%n)
    CALL xVector%get(dummyvec)
    CALL yVector%get(dummyvec2)
    bool = (dummyvec(1) .APPROXEQ. 2._SRK) .AND. &
           (dummyvec(2) .APPROXEQ. 4._SRK) .AND. &
           (dummyvec(3) .APPROXEQ. 6._SRK) .AND. &
           (dummyvec2(1) .APPROXEQ. 1._SRK) .AND. &
           (dummyvec2(2) .APPROXEQ. 3._SRK) .AND. &
           (dummyvec2(3) .APPROXEQ. 5._SRK)
    ASSERT(bool, 'BLAS_swap(THISVECTOR=xVector,THATVECTOR=yVector,N=xVector%n) [PETSC]')
    CALL xVector%set(1,1.0_SRK)
    CALL xVector%set(2,3.0_SRK)
    CALL xVector%set(3,5.0_SRK)
    CALL yVector%set(1,2.0_SRK)
    CALL yVector%set(2,4.0_SRK)
    CALL yVector%set(3,6.0_SRK)
    CALL BLAS_swap(THISVECTOR=xVector,THATVECTOR=yVector)
    CALL xVector%get(dummyvec)
    CALL yVector%get(dummyvec2)
    bool = (dummyvec(1) .APPROXEQ. 2._SRK) .AND. &
           (dummyvec(2) .APPROXEQ. 4._SRK) .AND. &
           (dummyvec(3) .APPROXEQ. 6._SRK) .AND. &
           (dummyvec2(1) .APPROXEQ. 1._SRK) .AND. &
           (dummyvec2(2) .APPROXEQ. 3._SRK) .AND. &
           (dummyvec2(3) .APPROXEQ. 5._SRK)
    ASSERT(bool, 'BLAS_swap(THISVECTOR=xVector,THATVECTOR=yVector) [PETSC]')
    WRITE(*,*) '  Passed: CALL BLAS_swap(...) [PETSC]'
    CALL xVector%clear()
    CALL yVector%clear()
    CALL aVector%clear()
    DEALLOCATE(xVector)
    DEALLOCATE(yVector)
    DEALLOCATE(aVector)
    CALL pList%clear()
#endif

#ifdef FUTILITY_HAVE_ForTrilinos
    ! test with Trilinos vectors
    ALLOCATE(TrilinosVectorType :: xVector)
    ALLOCATE(TrilinosVectorType :: yVector)
    ALLOCATE(TrilinosVectorType :: aVector)
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
    ASSERT(r .APPROXEQ. 14.0_SRK, 'BLAS_asum(THISVECTOR=xVector,N=xVector%n,INCX=1) [Trilinos]')
    r=0.0_SRK
    r = BLAS_asum(THISVECTOR=xVector,INCX=1)
    ASSERT(r .APPROXEQ. 14.0_SRK, 'BLAS_asum(THISVECTOR=xVector,INCX=1) [Trilinos]')
    r=0.0_SRK
    r = BLAS_asum(THISVECTOR=xVector,N=xVector%n)
    ASSERT(r .APPROXEQ. 14.0_SRK, 'BLAS_asum(THISVECTOR=xVector,N=xVector%n) [Trilinos]')
    r=0.0_SRK
    r = BLAS_asum(THISVECTOR=xVector)
    ASSERT(r .APPROXEQ. 14.0_SRK, 'BLAS_asum(THISVECTOR=xVector) [Trilinos]')
    WRITE(*,*) '  Passed: CALL BLAS_asum(...) [Trilinos]'

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
    DO i=1,3
      CALL yVector%get(i,dummyvec(i))
    ENDDO
    bool = ((dummyvec(1) .APPROXEQ. 28._SRK) .AND. &
            (dummyvec(2) .APPROXEQ. -5._SRK) .AND. &
            (dummyvec(3) .APPROXEQ. 36._SRK))
    ASSERT(bool, 'BLAS_axpy')
    FINFO() '(THISVECTOR=xVector,NEWVECTOR=yVector,A=a,N=xVector%n,INCX=1,INCY=1) -scalar_a [Trilinos]'
    CALL yVector%set(1,3.0_SRK)
    CALL yVector%set(2,5.0_SRK)
    CALL yVector%set(3,1.0_SRK)
    CALL BLAS_axpy(THISVECTOR=xVector,NEWVECTOR=yVector,A=a,N=xVector%n,INCX=1)
    DO i=1,3
      CALL yVector%get(i,dummyvec(i))
    ENDDO
    bool = (dummyvec(1) .APPROXEQ. 28._SRK) .AND. &
            (dummyvec(2) .APPROXEQ. -5._SRK) .AND. &
            (dummyvec(3) .APPROXEQ. 36._SRK)
    ASSERT(bool, 'BLAS_axpy')
    FINFO() '(THISVECTOR=xVector,NEWVECTOR=yVector,A=a,N=xVector%n,INCX=1) -scalar_a [Trilinos]'
    CALL yVector%set(1,3.0_SRK)
    CALL yVector%set(2,5.0_SRK)
    CALL yVector%set(3,1.0_SRK)
    CALL BLAS_axpy(THISVECTOR=xVector,NEWVECTOR=yVector,A=a,N=xVector%n,INCY=1)
    DO i=1,3
      CALL yVector%get(i,dummyvec(i))
    ENDDO
    bool = ((dummyvec(1) .APPROXEQ. 28._SRK) .AND. &
            (dummyvec(2) .APPROXEQ. -5._SRK) .AND. &
            (dummyvec(3) .APPROXEQ. 36._SRK))
    ASSERT(bool, 'BLAS_axpy')
    FINFO() '(THISVECTOR=xVector,NEWVECTOR=yVector,A=a,N=xVector%n,INCY=1) -scalar_a [Trilinos]'
    CALL yVector%set(1,3.0_SRK)
    CALL yVector%set(2,5.0_SRK)
    CALL yVector%set(3,1.0_SRK)
    CALL BLAS_axpy(THISVECTOR=xVector,NEWVECTOR=yVector,A=a,N=xVector%n)
    DO i=1,3
      CALL yVector%get(i,dummyvec(i))
    ENDDO
    bool = ((dummyvec(1) .APPROXEQ. 28._SRK) .AND. &
            (dummyvec(2) .APPROXEQ. -5._SRK) .AND. &
            (dummyvec(3) .APPROXEQ. 36._SRK))
    ASSERT(bool, 'BLAS_axpy')
    FINFO() '(THISVECTOR=xVector,NEWVECTOR=yVector,A=a,N=xVector%n) -scalar_a [Trilinos]'
    CALL yVector%set(1,3.0_SRK)
    CALL yVector%set(2,5.0_SRK)
    CALL yVector%set(3,1.0_SRK)
    CALL BLAS_axpy(THISVECTOR=xVector,NEWVECTOR=yVector,A=a)
    DO i=1,3
      CALL yVector%get(i,dummyvec(i))
    ENDDO
    bool = ((dummyvec(1) .APPROXEQ. 28._SRK) .AND. &
            (dummyvec(2) .APPROXEQ. -5._SRK) .AND. &
            (dummyvec(3) .APPROXEQ. 36._SRK))
    ASSERT(bool, 'BLAS_axpy(THISVECTOR=xVector,NEWVECTOR=yVector,A=a) -scalar_a [Trilinos]')
    WRITE(*,*) '  Passed: CALL BLAS_axpy(...) -scalar_a [Trilinos]'

    !Test BLAS_axpy (y=y+ax) [vector a]
! TODO: this function currently isn't implemented
!    a = 3
!    CALL xVector%set(1,5.0_SRK)
!    CALL xVector%set(2,-2.0_SRK)
!    CALL xVector%set(3,7.0_SRK)
!    CALL yVector%set(1,3.0_SRK)
!    CALL yVector%set(2,5.0_SRK)
!    CALL yVector%set(3,1.0_SRK)
!    CALL aVector%set(1,6.0_SRK)
!    CALL aVector%set(2,4.0_SRK)
!    CALL aVector%set(3,2.0_SRK)
!    CALL BLAS_axpy(THISVECTOR=xVector,NEWVECTOR=yVector,AVECTOR=aVector,N=xVector%n,INCX=1,INCY=1)
!    IF(ALLOCATED(dummyvec)) DEALLOCATE(dummyvec)
!    ALLOCATE(dummyvec(yVector%n))
!    DO i=1,yVector%n
!      CALL yVector%get(i,dummyvec(i))
!    ENDDO
!    bool = ((dummyvec(1) .APPROXEQ. 33._SRK) .AND. &
!            (dummyvec(2) .APPROXEQ. -3._SRK) .AND. &
!            (dummyvec(3) .APPROXEQ. 15._SRK))
!    ASSERT(bool, "BLAS_axpy")
!    FINFO() "(THISVECTOR=xVector,NEWVECTOR=yVector,AVECTOR=aVector,N=xVector%n,INCX=1,INCY=1)"
!    FINFO() " -vector_a [Trilinos]"
!    CALL yVector%set(1,3.0_SRK)
!    CALL yVector%set(2,5.0_SRK)
!    CALL yVector%set(3,1.0_SRK)
!    CALL BLAS_axpy(THISVECTOR=xVector,NEWVECTOR=yVector,AVECTOR=aVector,N=xVector%n,INCX=1)
!    DO i=1,yVector%n
!      CALL yVector%get(i,dummyvec(i))
!    ENDDO
!    bool = ((dummyvec(1) .APPROXEQ. 33._SRK) .AND. &
!            (dummyvec(2) .APPROXEQ. -3._SRK) .AND. &
!            (dummyvec(3) .APPROXEQ. 15._SRK))
!    ASSERT(bool, 'BLAS_axpy')
!    FINFO() '(THISVECTOR=xVector,NEWVECTOR=yVector,AVECTOR=aVector,N=xVector%n,INCX=1) -vector_a [Trilinos]'
!    CALL yVector%set(1,3.0_SRK)
!    CALL yVector%set(2,5.0_SRK)
!    CALL yVector%set(3,1.0_SRK)
!    CALL BLAS_axpy(THISVECTOR=xVector,NEWVECTOR=yVector,AVECTOR=aVector,N=xVector%n,INCY=1)
!    DO i=1,yVector%n
!      CALL yVector%get(i,dummyvec(i))
!    ENDDO
!    bool = (dummyvec(1) .APPROXEQ. 33._SRK) .AND. &
!            (dummyvec(2) .APPROXEQ. -3._SRK) .AND. &
!            (dummyvec(3) .APPROXEQ. 15._SRK)
!    ASSERT(bool, 'BLAS_axpy')
!    FINFO() '(THISVECTOR=xVector,NEWVECTOR=yVector,AVECTOR=aVector,N=xVector%n,INCY=1) -vector_a [Trilinos]'
!    CALL yVector%set(1,3.0_SRK)
!    CALL yVector%set(2,5.0_SRK)
!    CALL yVector%set(3,1.0_SRK)
!    CALL BLAS_axpy(THISVECTOR=xVector,NEWVECTOR=yVector,AVECTOR=aVector,N=xVector%n)
!    DO i=1,yVector%n
!      CALL yVector%get(i,dummyvec(i))
!    ENDDO
!    bool = ((dummyvec(1) .APPROXEQ. 33._SRK) .AND. &
!            (dummyvec(2) .APPROXEQ. -3._SRK) .AND. &
!            (dummyvec(3) .APPROXEQ. 15._SRK))
!    ASSERT(bool, 'BLAS_axpy')
!    FINFO() '(THISVECTOR=xVector,NEWVECTOR=yVector,AVECTOR=aVector,N=xVector%n) -vector_a [Trilinos]'
!    CALL yVector%set(1,3.0_SRK)
!    CALL yVector%set(2,5.0_SRK)
!    CALL yVector%set(3,1.0_SRK)
!    CALL BLAS_axpy(THISVECTOR=xVector,NEWVECTOR=yVector,AVECTOR=aVector)
!    DO i=1,yVector%n
!      CALL yVector%get(i,dummyvec(i))
!    ENDDO
!    bool = (dummyvec(1) .APPROXEQ. 33._SRK) .AND. &
!            (dummyvec(2) .APPROXEQ. -3._SRK) .AND. &
!            (dummyvec(3) .APPROXEQ. 15._SRK)
!    ASSERT(bool, 'BLAS_axpy')
!    FINFO() '(THISVECTOR=xVector,NEWVECTOR=yVector,AVECTOR=aVector) -vector_a [Trilinos]'
!    WRITE(*,*) '  Passed: CALL BLAS_axpy(...) -vector_a [Trilinos]'

    !Test BLAS_copy
    CALL xVector%set(1,1.0_SRK)
    CALL xVector%set(2,3.0_SRK)
    CALL xVector%set(3,7.0_SRK)
    CALL yVector%set(0.0_SRK,iverr)

    CALL BLAS_copy(THISVECTOR=xVector,NEWVECTOR=yVector,N=xVector%n,INCX=1,INCY=1)
    IF(ALLOCATED(dummyvec)) DEALLOCATE(dummyvec)
    ALLOCATE(dummyvec(yVector%n))
    DO i=1,yVector%n
      CALL yVector%get(i,dummyvec(i))
    ENDDO
    bool = (dummyvec(1) .APPROXEQ. 1.0_SRK) .AND. &
            (dummyvec(2) .APPROXEQ. 3.0_SRK) .AND. &
            (dummyvec(3) .APPROXEQ. 7.0_SRK)
    ASSERT(bool, 'BLAS_copy')
    FINFO() '(THISVECTOR=xVector,NEWVECTOR=yVector,N=xVector%n,INCX=1,INCY=1) [Trilinos]'
    FINFO() dummyvec
    CALL yVector%set(0.0_SRK)
    CALL BLAS_copy(THISVECTOR=xVector,NEWVECTOR=yVector,INCX=1,INCY=1)
    DO i=1,yVector%n
      CALL yVector%get(i,dummyvec(i))
    ENDDO
    bool = (dummyvec(1) .APPROXEQ. 1.0_SRK) .AND. &
            (dummyvec(2) .APPROXEQ. 3.0_SRK) .AND. &
            (dummyvec(3) .APPROXEQ. 7.0_SRK)
    ASSERT(bool, 'BLAS_copy(THISVECTOR=xVector,NEWVECTOR=yVector,INCX=1,INCY=1) [Trilinos]')
    FINFO()dummyvec
    CALL yVector%set(0.0_SRK)
    CALL BLAS_copy(THISVECTOR=xVector,NEWVECTOR=yVector,N=xVector%n,INCX=1)
    DO i=1,yVector%n
      CALL yVector%get(i,dummyvec(i))
    ENDDO
    bool = (dummyvec(1) .APPROXEQ. 1.0_SRK) .AND. &
            (dummyvec(2) .APPROXEQ. 3.0_SRK) .AND. &
            (dummyvec(3) .APPROXEQ. 7.0_SRK)
    ASSERT(bool, 'BLAS_copy(THISVECTOR=xVector,NEWVECTOR=yVector,N=xVector%n,INCX=1) [Trilinos]')
    CALL yVector%set(0.0_SRK)
    CALL BLAS_copy(THISVECTOR=xVector,NEWVECTOR=yVector,N=xVector%n,INCY=1)
    DO i=1,yVector%n
      CALL yVector%get(i,dummyvec(i))
    ENDDO
    bool = (dummyvec(1) .APPROXEQ. 1.0_SRK) .AND. &
           (dummyvec(2) .APPROXEQ. 3.0_SRK) .AND. &
           (dummyvec(3) .APPROXEQ. 7.0_SRK)
    ASSERT(bool, 'BLAS_copy(THISVECTOR=xVector,NEWVECTOR=yVector,N=xVector%n,INCY=1) [Trilinos]')
    CALL yVector%set(0.0_SRK)
    CALL BLAS_copy(THISVECTOR=xVector,NEWVECTOR=yVector,INCX=1)
    DO i=1,yVector%n
      CALL yVector%get(i,dummyvec(i))
    ENDDO
    bool = (dummyvec(1) .APPROXEQ. 1.0_SRK) .AND. &
           (dummyvec(2) .APPROXEQ. 3.0_SRK) .AND. &
           (dummyvec(3) .APPROXEQ. 7.0_SRK)
    ASSERT(bool, 'BLAS_copy(THISVECTOR=xVector,NEWVECTOR=yVector,INCX=1) [Trilinos]')
    CALL yVector%set(0.0_SRK)
    CALL BLAS_copy(THISVECTOR=xVector,NEWVECTOR=yVector,INCY=1)
    DO i=1,yVector%n
      CALL yVector%get(i,dummyvec(i))
    ENDDO
    bool = ((dummyvec(1) .APPROXEQ. 1.0_SRK) .AND. &
            (dummyvec(2) .APPROXEQ. 3.0_SRK) .AND. &
            (dummyvec(3) .APPROXEQ. 7.0_SRK))
    ASSERT(bool, 'BLAS_copy(THISVECTOR=xVector,NEWVECTOR=yVector,INCY=1) [Trilinos]')
    CALL yVector%set(0.0_SRK)
    CALL BLAS_copy(THISVECTOR=xVector,NEWVECTOR=yVector,N=xVector%n)
    DO i=1,yVector%n
      CALL yVector%get(i,dummyvec(i))
    ENDDO
    bool = (dummyvec(1) .APPROXEQ. 1.0_SRK) .AND. &
           (dummyvec(2) .APPROXEQ. 3.0_SRK) .AND. &
           (dummyvec(3) .APPROXEQ. 7.0_SRK)
    ASSERT(bool, 'BLAS_copy(THISVECTOR=xVector,NEWVECTOR=yVector,N=xVector%n) [Trilinos]')
    CALL yVector%set(0.0_SRK)
    CALL BLAS_copy(THISVECTOR=xVector,NEWVECTOR=yVector)
    DO i=1,yVector%n
      CALL yVector%get(i,dummyvec(i))
    ENDDO
    bool = (dummyvec(1) .APPROXEQ. 1.0_SRK) .AND. &
           (dummyvec(2) .APPROXEQ. 3.0_SRK) .AND. &
           (dummyvec(3) .APPROXEQ. 7.0_SRK)
    ASSERT(bool, 'BLAS_copy(THISVECTOR=xVector,NEWVECTOR=yVector) [Trilinos]')
    WRITE(*,*) '  Passed: CALL BLAS_copy(...) [Trilinos]'

    !Test BLAS_dot
! TODO: this function currently isn't implemented
!    CALL xVector%set(1,1.0_SRK)
!    CALL xVector%set(2,3.0_SRK)
!    CALL xVector%set(3,7.0_SRK)
!    CALL yVector%set(1,2.0_SRK)
!    CALL yVector%set(2,4.0_SRK)
!    CALL yVector%set(3,6.0_SRK)
!    r=0.0_SRK
!    r = BLAS_dot(THISVECTOR=xVector,THATVECTOR=yVector,N=xVector%n,INCX=1,INCY=1)
!    bool = r .APPROXEQ. 56.0_SRK
!    ASSERT(bool, 'BLAS_dot(THISVECTOR=xVector,THATVECTOR=yVector,N=xVector%n,INCX=1,INCY=1) [Trilinos]')
!    r=0.0_SRK
!    r = BLAS_dot(THISVECTOR=xVector,THATVECTOR=yVector,INCX=1,INCY=1)
!    bool = r .APPROXEQ. 56.0_SRK
!    ASSERT(bool, 'BLAS_dot(THISVECTOR=xVector,THATVECTOR=yVector,INCX=1,INCY=1) [Trilinos]')
!    r=0.0_SRK
!    r = BLAS_dot(THISVECTOR=xVector,THATVECTOR=yVector,N=xVector%n,INCX=1)
!    bool = r .APPROXEQ. 56.0_SRK
!    ASSERT(bool, 'BLAS_dot(THISVECTOR=xVector,THATVECTOR=yVector,N=xVector%n,INCX=1) [Trilinos]')
!    r=0.0_SRK
!    r = BLAS_dot(THISVECTOR=xVector,THATVECTOR=yVector,N=xVector%n,INCY=1)
!    bool = r .APPROXEQ. 56.0_SRK
!    ASSERT(bool, 'BLAS_dot(THISVECTOR=xVector,THATVECTOR=yVector,N=xVector%n,INCY=1) [Trilinos]')
!    r=0.0_SRK
!    r = BLAS_dot(THISVECTOR=xVector,THATVECTOR=yVector,N=xVector%n)
!    bool = r .APPROXEQ. 56.0_SRK
!    ASSERT(bool, 'BLAS_dot(THISVECTOR=xVector,THATVECTOR=yVector,N=xVector%n) [Trilinos]')
!    r=0.0_SRK
!    r = BLAS_dot(THISVECTOR=xVector,THATVECTOR=yVector,INCX=1)
!    bool = r .APPROXEQ. 56.0_SRK
!    ASSERT(bool, 'BLAS_dot(THISVECTOR=xVector,THATVECTOR=yVector,INCX=1) [Trilinos]')
!    r=0.0_SRK
!    r = BLAS_dot(THISVECTOR=xVector,THATVECTOR=yVector,INCY=1)
!    bool = r .APPROXEQ. 56.0_SRK
!    ASSERT(bool, 'BLAS_dot(THISVECTOR=xVector,THATVECTOR=yVector,INCY=1) [Trilinos]')
!    r=0.0_SRK
!    r = BLAS_dot(THISVECTOR=xVector,THATVECTOR=yVector)
!    bool = r .APPROXEQ. 56.0_SRK
!    ASSERT(bool, 'BLAS_dot(THISVECTOR=xVector,THATVECTOR=yVector) [Trilinos]')
!    WRITE(*,*) '  Passed: CALL BLAS_dot(...) [Trilinos]'

    !Test BLAS_iamax
! TODO: this function currently isn't implemented
!    ! first test with all positive values
!    CALL xVector%set(1,12.0_SRK)
!    CALL xVector%set(2, 3.0_SRK)
!    CALL xVector%set(3,17.0_SRK)
!    r_index = 0_SIK
!    r_index = BLAS_iamax(THISVECTOR=xVector,N=xVector%n,INCX=1)
!    ASSERT(r_index == 3_SIK, 'BLAS_iamax(THISVECTOR=xVector,N=xVector%n,INCX=1) [Trilinos]')
!    r_index = 0_SIK
!    r_index = BLAS_iamax(THISVECTOR=xVector,INCX=1)
!    ASSERT(r_index == 3_SIK, 'BLAS_iamax(THISVECTOR=xVector,INCX=1) [Trilinos]')
!    r_index = 0_SIK
!    r_index = BLAS_iamax(THISVECTOR=xVector,N=xVector%n)
!    ASSERT(r_index == 3_SIK, 'BLAS_iamax(THISVECTOR=xVector,N=xVector%n) [Trilinos]')
!    r_index = 0_SIK
!    r_index = BLAS_iamax(THISVECTOR=xVector)
!    ASSERT(r_index == 3_SIK, 'BLAS_iamax(THISVECTOR=xVector) [Trilinos]')
!    ! next test with a negative value
!    CALL xVector%set(1, 12.0_SRK)
!    CALL xVector%set(2,-30.0_SRK)
!    CALL xVector%set(3, 17.0_SRK)
!    r_index = 0_SIK
!    r_index = BLAS_iamax(THISVECTOR=xVector,N=xVector%n,INCX=1)
!    ASSERT(r_index == 2_SIK, 'BLAS_iamax(THISVECTOR=xVector,N=xVector%n,INCX=1) [Trilinos]')
!    r_index = 0_SIK
!    r_index = BLAS_iamax(THISVECTOR=xVector,INCX=1)
!    ASSERT(r_index == 2_SIK, 'BLAS_iamax(THISVECTOR=xVector,INCX=1) [Trilinos]')
!    r_index = 0_SIK
!    r_index = BLAS_iamax(THISVECTOR=xVector,N=xVector%n)
!    ASSERT(r_index == 2_SIK, 'BLAS_iamax(THISVECTOR=xVector,N=xVector%n) [Trilinos]')
!    r_index = 0_SIK
!    r_index = BLAS_iamax(THISVECTOR=xVector)
!    ASSERT(r_index == 2_SIK, 'BLAS_iamax(THISVECTOR=xVector) [Trilinos]')
!    WRITE(*,*) '  Passed: CALL BLAS_iamax(...) [Trilinos]'

    !Test BLAS_iamin
! TODO: this function currently isn't implemented
!    ! first test with all positive values
!    CALL xVector%set(1,12.0_SRK)
!    CALL xVector%set(2, 3.0_SRK)
!    CALL xVector%set(3,17.0_SRK)
!    r_index = 0_SIK
!    r_index = BLAS_iamin(THISVECTOR=xVector,N=xVector%n,INCX=1)
!    ASSERT(r_index == 2_SIK, 'BLAS_iamin(THISVECTOR=xVector,N=xVector%n,INCX=1) [Trilinos]')
!    r_index = 0_SIK
!    r_index = BLAS_iamin(THISVECTOR=xVector,INCX=1)
!    ASSERT(r_index == 2_SIK, 'BLAS_iamin(THISVECTOR=xVector,INCX=1) [Trilinos]')
!    r_index = 0_SIK
!    r_index = BLAS_iamin(THISVECTOR=xVector,N=xVector%n)
!    ASSERT(r_index == 2_SIK, 'BLAS_iamin(THISVECTOR=xVector,N=xVector%n) [Trilinos]')
!    r_index = 0_SIK
!    r_index = BLAS_iamin(THISVECTOR=xVector)
!    ASSERT(r_index == 2_SIK, 'BLAS_iamin(THISVECTOR=xVector) [Trilinos]')
!    ! next test with a negative value
!    CALL xVector%set(1, 12.0_SRK)
!    CALL xVector%set(2,-30.0_SRK)
!    CALL xVector%set(3, 17.0_SRK)
!    r_index = 0_SIK
!    r_index = BLAS_iamin(THISVECTOR=xVector,N=xVector%n,INCX=1)
!    ASSERT(r_index == 1_SIK, 'BLAS_iamin(THISVECTOR=xVector,N=xVector%n,INCX=1) [Trilinos]')
!    r_index = 0_SIK
!    r_index = BLAS_iamin(THISVECTOR=xVector,INCX=1)
!    ASSERT(r_index == 1_SIK, 'BLAS_iamin(THISVECTOR=xVector,INCX=1) [Trilinos]')
!    r_index = 0_SIK
!    r_index = BLAS_iamin(THISVECTOR=xVector,N=xVector%n)
!    ASSERT(r_index == 1_SIK, 'BLAS_iamin(THISVECTOR=xVector,N=xVector%n) [Trilinos]')
!    r_index = 0_SIK
!    r_index = BLAS_iamin(THISVECTOR=xVector)
!    ASSERT(r_index == 1_SIK, 'BLAS_iamin(THISVECTOR=xVector) [Trilinos]')
!    WRITE(*,*) '  Passed: CALL BLAS_iamin(...) [Trilinos]'

    !Test BLAS_nrm2
    CALL xVector%set(1,0.0_SRK)
    CALL xVector%set(2,3.0_SRK)
    CALL xVector%set(3,4.0_SRK)
    r = 0.0_SRK
    r = BLAS_nrm2(THISVECTOR=xVector,N=xVector%n,INCX=1)
    bool = r .APPROXEQ. 5._SRK
    ASSERT(bool, 'BLAS_nrm2(THISVECTOR=xVector,N=xVector%n,INCX=1) [Trilinos]')
    r = 0.0_SRK
    r = BLAS_nrm2(THISVECTOR=xVector,INCX=1)
    bool = r .APPROXEQ. 5._SRK
    ASSERT(bool, 'BLAS_nrm2(THISVECTOR=xVector,INCX=1) [Trilinos]')
    r = 0.0_SRK
    r = BLAS_nrm2(THISVECTOR=xVector,N=xVector%n)
    bool = r .APPROXEQ. 5._SRK
    ASSERT(bool, 'BLAS_nrm2(THISVECTOR=xVector,N=xVector%n) [Trilinos]')
    r = 0.0_SRK
    r = BLAS_nrm2(THISVECTOR=xVector)
    bool = r .APPROXEQ. 5._SRK
    ASSERT(bool, 'BLAS_nrm2(THISVECTOR=xVector) [Trilinos]')
    WRITE(*,*) '  Passed: CALL BLAS_nrm2(...) [Trilinos]'

    !Test BLAS_scal [scalar a]
    a = 3
    CALL xVector%set(1,1.0_SRK)
    CALL xVector%set(2,3.0_SRK)
    CALL xVector%set(3,5.0_SRK)
    CALL BLAS_scal(THISVECTOR=xVector,A=a,N=xVector%n,INCX=1)
    IF(ALLOCATED(dummyvec)) DEALLOCATE(dummyvec)
    ALLOCATE(dummyvec(xVector%n))
    DO i=1,xVector%n
      CALL xVector%get(i,dummyvec(i))
    ENDDO
    bool = (dummyvec(1) .APPROXEQ.  3._SRK) .AND. &
            (dummyvec(2) .APPROXEQ.  9._SRK) .AND. &
            (dummyvec(3) .APPROXEQ. 15._SRK)
    ASSERT(bool, 'BLAS_scal(THISVECTOR=xVector,A=a,N=xVector%n,INCX=1) -scalar_a [Trilinos]')
    CALL xVector%set(1,1.0_SRK)
    CALL xVector%set(2,3.0_SRK)
    CALL xVector%set(3,5.0_SRK)
    CALL BLAS_scal(THISVECTOR=xVector,A=a,INCX=1)
    DO i=1,xVector%n
      CALL xVector%get(i,dummyvec(i))
    ENDDO
    bool = (dummyvec(1) .APPROXEQ.  3._SRK) .AND. &
            (dummyvec(2) .APPROXEQ.  9._SRK) .AND. &
            (dummyvec(3) .APPROXEQ. 15._SRK)
    ASSERT(bool, 'BLAS_scal(THISVECTOR=xVector,A=a,INCX=1) -scalar_a [Trilinos]')
    CALL xVector%set(1,1.0_SRK)
    CALL xVector%set(2,3.0_SRK)
    CALL xVector%set(3,5.0_SRK)
    CALL BLAS_scal(THISVECTOR=xVector,A=a,N=xVector%n)
    DO i=1,xVector%n
      CALL xVector%get(i,dummyvec(i))
    ENDDO
    bool = (dummyvec(1) .APPROXEQ.  3._SRK) .AND. &
            (dummyvec(2) .APPROXEQ.  9._SRK) .AND. &
            (dummyvec(3) .APPROXEQ. 15._SRK)
    ASSERT(bool, 'BLAS_scal(THISVECTOR=xVector,A=a,N=xVector%n) -scalar_a [Trilinos]')
    CALL xVector%set(1,1.0_SRK)
    CALL xVector%set(2,3.0_SRK)
    CALL xVector%set(3,5.0_SRK)
    CALL BLAS_scal(THISVECTOR=xVector,A=a)
    DO i=1,xVector%n
      CALL xVector%get(i,dummyvec(i))
    ENDDO
    bool = (dummyvec(1) .APPROXEQ.  3._SRK) .AND. &
            (dummyvec(2) .APPROXEQ.  9._SRK) .AND. &
            (dummyvec(3) .APPROXEQ. 15._SRK)
    ASSERT(bool, 'BLAS_scal(THISVECTOR=xVector,A=a) -scalar_a [Trilinos]')
    WRITE(*,*) '  Passed: CALL BLAS_scal(...) -scalar_a [Trilinos]'

    !Test BLAS_scal [vector a]
! TODO: this function currently isn't implemented
!    CALL aVector%set(1,2.0_SRK)
!    CALL aVector%set(2,4.0_SRK)
!    CALL aVector%set(3,1.0_SRK)
!    CALL xVector%set(1,1.0_SRK)
!    CALL xVector%set(2,3.0_SRK)
!    CALL xVector%set(3,5.0_SRK)
!    CALL BLAS_scal(THISVECTOR=xVector,AVECTOR=aVector,N=xVector%n,INCX=1)
!    IF(ALLOCATED(dummyvec)) DEALLOCATE(dummyvec)
!    ALLOCATE(dummyvec(xVector%n))
!    CALL xVector%get(dummyvec)
!    bool = (dummyvec(1) .APPROXEQ.  2._SRK) .AND. &
!           (dummyvec(2) .APPROXEQ. 12._SRK) .AND. &
!           (dummyvec(3) .APPROXEQ.  5._SRK)
!    ASSERT(bool, 'BLAS_scal(THISVECTOR=xVector,AVECTOR=aVector,N=xVector%n,INCX=1) -vector_a [Trilinos]')
!    CALL xVector%set(1,1.0_SRK)
!    CALL xVector%set(2,3.0_SRK)
!    CALL xVector%set(3,5.0_SRK)
!    CALL BLAS_scal(THISVECTOR=xVector,AVECTOR=aVector,INCX=1)
!    CALL xVector%get(dummyvec)
!    bool = (dummyvec(1) .APPROXEQ.  2._SRK) .AND. &
!           (dummyvec(2) .APPROXEQ. 12._SRK) .AND. &
!           (dummyvec(3) .APPROXEQ.  5._SRK)
!    ASSERT(bool, 'BLAS_scal(THISVECTOR=xVector,AVECTOR=aVector,INCX=1) -vector_a [Trilinos]')
!    CALL xVector%set(1,1.0_SRK)
!    CALL xVector%set(2,3.0_SRK)
!    CALL xVector%set(3,5.0_SRK)
!    CALL BLAS_scal(THISVECTOR=xVector,AVECTOR=aVector,N=xVector%n)
!    CALL xVector%get(dummyvec)
!    bool = (dummyvec(1) .APPROXEQ.  2._SRK) .AND. &
!           (dummyvec(2) .APPROXEQ. 12._SRK) .AND. &
!           (dummyvec(3) .APPROXEQ.  5._SRK)
!    ASSERT(bool, 'BLAS_scal(THISVECTOR=xVector,AVECTOR=aVector,N=xVector%n) -vector_a [Trilinos]')
!    CALL xVector%set(1,1.0_SRK)
!    CALL xVector%set(2,3.0_SRK)
!    CALL xVector%set(3,5.0_SRK)
!    CALL BLAS_scal(THISVECTOR=xVector,AVECTOR=aVector)
!    CALL xVector%get(dummyvec)
!    bool = (dummyvec(1) .APPROXEQ.  2._SRK) .AND. &
!           (dummyvec(2) .APPROXEQ. 12._SRK) .AND. &
!           (dummyvec(3) .APPROXEQ.  5._SRK)
!    ASSERT(bool, 'BLAS_scal(THISVECTOR=xVector,AVECTOR=aVector) -vector_a [Trilinos]')
!    WRITE(*,*) '  Passed: CALL BLAS_scal(...) -vector_a [Trilinos]'

    !Test BLAS_swap
! TODO: this function currently isn't implemented
!    CALL xVector%set(1,1.0_SRK)
!    CALL xVector%set(2,3.0_SRK)
!    CALL xVector%set(3,5.0_SRK)
!    CALL yVector%set(1,2.0_SRK)
!    CALL yVector%set(2,4.0_SRK)
!    CALL yVector%set(3,6.0_SRK)
!    CALL BLAS_swap(THISVECTOR=xVector,THATVECTOR=yVector,N=xVector%n,INCX=1,INCY=1)
!    IF(ALLOCATED(dummyvec)) DEALLOCATE(dummyvec)
!    IF(ALLOCATED(dummyvec2)) DEALLOCATE(dummyvec2)
!    ALLOCATE(dummyvec(xVector%n))
!    ALLOCATE(dummyvec2(yVector%n))
!    CALL xVector%get(dummyvec)
!    CALL yVector%get(dummyvec2)
!    bool = (dummyvec(1) .APPROXEQ. 2._SRK) .AND. &
!           (dummyvec(2) .APPROXEQ. 4._SRK) .AND. &
!           (dummyvec(3) .APPROXEQ. 6._SRK) .AND. &
!           (dummyvec2(1) .APPROXEQ. 1._SRK) .AND. &
!           (dummyvec2(2) .APPROXEQ. 3._SRK) .AND. &
!           (dummyvec2(3) .APPROXEQ. 5._SRK)
!    ASSERT(bool, 'BLAS_swap(THISVECTOR=xVector,THATVECTOR=yVector,N=xVector%n,INCX=1,INCY=1) [Trilinos]')
!    CALL xVector%set(1,1.0_SRK)
!    CALL xVector%set(2,3.0_SRK)
!    CALL xVector%set(3,5.0_SRK)
!    CALL yVector%set(1,2.0_SRK)
!    CALL yVector%set(2,4.0_SRK)
!    CALL yVector%set(3,6.0_SRK)
!    CALL BLAS_swap(THISVECTOR=xVector,THATVECTOR=yVector,INCX=1,INCY=1)
!    CALL xVector%get(dummyvec)
!    CALL yVector%get(dummyvec2)
!    bool = (dummyvec(1) .APPROXEQ. 2._SRK) .AND. &
!           (dummyvec(2) .APPROXEQ. 4._SRK) .AND. &
!           (dummyvec(3) .APPROXEQ. 6._SRK) .AND. &
!           (dummyvec2(1) .APPROXEQ. 1._SRK) .AND. &
!           (dummyvec2(2) .APPROXEQ. 3._SRK) .AND. &
!           (dummyvec2(3) .APPROXEQ. 5._SRK)
!    ASSERT(bool, 'BLAS_swap(THISVECTOR=xVector,THATVECTOR=yVector,INCX=1,INCY=1) [Trilinos]')
!    CALL xVector%set(1,1.0_SRK)
!    CALL xVector%set(2,3.0_SRK)
!    CALL xVector%set(3,5.0_SRK)
!    CALL yVector%set(1,2.0_SRK)
!    CALL yVector%set(2,4.0_SRK)
!    CALL yVector%set(3,6.0_SRK)
!    CALL BLAS_swap(THISVECTOR=xVector,THATVECTOR=yVector,N=xVector%n,INCX=1)
!    CALL xVector%get(dummyvec)
!    CALL yVector%get(dummyvec2)
!    bool = (dummyvec(1) .APPROXEQ. 2._SRK) .AND. &
!           (dummyvec(2) .APPROXEQ. 4._SRK) .AND. &
!           (dummyvec(3) .APPROXEQ. 6._SRK) .AND. &
!           (dummyvec2(1) .APPROXEQ. 1._SRK) .AND. &
!           (dummyvec2(2) .APPROXEQ. 3._SRK) .AND. &
!           (dummyvec2(3) .APPROXEQ. 5._SRK)
!    ASSERT(bool, 'BLAS_swap(THISVECTOR=xVector,THATVECTOR=yVector,N=xVector%n,INCX=1) [Trilinos]')
!    CALL xVector%set(1,1.0_SRK)
!    CALL xVector%set(2,3.0_SRK)
!    CALL xVector%set(3,5.0_SRK)
!    CALL yVector%set(1,2.0_SRK)
!    CALL yVector%set(2,4.0_SRK)
!    CALL yVector%set(3,6.0_SRK)
!    CALL BLAS_swap(THISVECTOR=xVector,THATVECTOR=yVector,N=xVector%n,INCY=1)
!    CALL xVector%get(dummyvec)
!    CALL yVector%get(dummyvec2)
!    bool = (dummyvec(1) .APPROXEQ. 2._SRK) .AND. &
!           (dummyvec(2) .APPROXEQ. 4._SRK) .AND. &
!           (dummyvec(3) .APPROXEQ. 6._SRK) .AND. &
!           (dummyvec2(1) .APPROXEQ. 1._SRK) .AND. &
!           (dummyvec2(2) .APPROXEQ. 3._SRK) .AND. &
!           (dummyvec2(3) .APPROXEQ. 5._SRK)
!    ASSERT(bool, 'BLAS_swap(THISVECTOR=xVector,THATVECTOR=yVector,N=xVector%n,INCY=1) [Trilinos]')
!    CALL xVector%set(1,1.0_SRK)
!    CALL xVector%set(2,3.0_SRK)
!    CALL xVector%set(3,5.0_SRK)
!    CALL yVector%set(1,2.0_SRK)
!    CALL yVector%set(2,4.0_SRK)
!    CALL yVector%set(3,6.0_SRK)
!    CALL BLAS_swap(THISVECTOR=xVector,THATVECTOR=yVector,INCX=1)
!    CALL xVector%get(dummyvec)
!    CALL yVector%get(dummyvec2)
!    bool = (dummyvec(1) .APPROXEQ. 2._SRK) .AND. &
!           (dummyvec(2) .APPROXEQ. 4._SRK) .AND. &
!           (dummyvec(3) .APPROXEQ. 6._SRK) .AND. &
!           (dummyvec2(1) .APPROXEQ. 1._SRK) .AND. &
!           (dummyvec2(2) .APPROXEQ. 3._SRK) .AND. &
!           (dummyvec2(3) .APPROXEQ. 5._SRK)
!    ASSERT(bool, 'BLAS_swap(THISVECTOR=xVector,THATVECTOR=yVector,INCX=1) [Trilinos]')
!    CALL xVector%set(1,1.0_SRK)
!    CALL xVector%set(2,3.0_SRK)
!    CALL xVector%set(3,5.0_SRK)
!    CALL yVector%set(1,2.0_SRK)
!    CALL yVector%set(2,4.0_SRK)
!    CALL yVector%set(3,6.0_SRK)
!    CALL BLAS_swap(THISVECTOR=xVector,THATVECTOR=yVector,INCY=1)
!    CALL xVector%get(dummyvec)
!    CALL yVector%get(dummyvec2)
!    bool = (dummyvec(1) .APPROXEQ. 2._SRK) .AND. &
!           (dummyvec(2) .APPROXEQ. 4._SRK) .AND. &
!           (dummyvec(3) .APPROXEQ. 6._SRK) .AND. &
!           (dummyvec2(1) .APPROXEQ. 1._SRK) .AND. &
!           (dummyvec2(2) .APPROXEQ. 3._SRK) .AND. &
!           (dummyvec2(3) .APPROXEQ. 5._SRK)
!    ASSERT(bool, 'BLAS_swap(THISVECTOR=xVector,THATVECTOR=yVector,INCY=1) [Trilinos]')
!    CALL xVector%set(1,1.0_SRK)
!    CALL xVector%set(2,3.0_SRK)
!    CALL xVector%set(3,5.0_SRK)
!    CALL yVector%set(1,2.0_SRK)
!    CALL yVector%set(2,4.0_SRK)
!    CALL yVector%set(3,6.0_SRK)
!    CALL BLAS_swap(THISVECTOR=xVector,THATVECTOR=yVector,N=xVector%n)
!    CALL xVector%get(dummyvec)
!    CALL yVector%get(dummyvec2)
!    bool = (dummyvec(1) .APPROXEQ. 2._SRK) .AND. &
!           (dummyvec(2) .APPROXEQ. 4._SRK) .AND. &
!           (dummyvec(3) .APPROXEQ. 6._SRK) .AND. &
!           (dummyvec2(1) .APPROXEQ. 1._SRK) .AND. &
!           (dummyvec2(2) .APPROXEQ. 3._SRK) .AND. &
!           (dummyvec2(3) .APPROXEQ. 5._SRK)
!    ASSERT(bool, 'BLAS_swap(THISVECTOR=xVector,THATVECTOR=yVector,N=xVector%n) [Trilinos]')
!    CALL xVector%set(1,1.0_SRK)
!    CALL xVector%set(2,3.0_SRK)
!    CALL xVector%set(3,5.0_SRK)
!    CALL yVector%set(1,2.0_SRK)
!    CALL yVector%set(2,4.0_SRK)
!    CALL yVector%set(3,6.0_SRK)
!    CALL BLAS_swap(THISVECTOR=xVector,THATVECTOR=yVector)
!    CALL xVector%get(dummyvec)
!    CALL yVector%get(dummyvec2)
!    bool = (dummyvec(1) .APPROXEQ. 2._SRK) .AND. &
!           (dummyvec(2) .APPROXEQ. 4._SRK) .AND. &
!           (dummyvec(3) .APPROXEQ. 6._SRK) .AND. &
!           (dummyvec2(1) .APPROXEQ. 1._SRK) .AND. &
!           (dummyvec2(2) .APPROXEQ. 3._SRK) .AND. &
!           (dummyvec2(3) .APPROXEQ. 5._SRK)
!    ASSERT(bool, 'BLAS_swap(THISVECTOR=xVector,THATVECTOR=yVector) [Trilinos]')
!    WRITE(*,*) '  Passed: CALL BLAS_swap(...) [Trilinos]'
    CALL xVector%clear()
    CALL yVector%clear()
    CALL aVector%clear()
    DEALLOCATE(xVector)
    DEALLOCATE(yVector)
    DEALLOCATE(aVector)
    CALL pList%clear()
#endif
    ENDSUBROUTINE testBLAS1Interface
!
!-------------------------------------------------------------------------------
    SUBROUTINE testVectorFactory()
      CLASS(VectorType),POINTER :: vec_p
      CLASS(VectorType),POINTER :: other_vec_p
      CLASS(ParamType),ALLOCATABLE :: params
#if defined(FUTILITY_HAVE_PETSC) || defined(FUTILITY_HAVE_ForTrilinos)
      CLASS(DistributedVectorType),POINTER :: dvec_p
#endif

      ALLOCATE(ParamType :: params)

      vec_p => NULL()
      other_vec_p => NULL()

      ! Native
      CALL params%add("VectorType->n",10_SIK)
      CALL params%add("VectorType->engine",VM_NATIVE)
      CALL VectorFactory(vec_p,params)
      ASSERT(ASSOCIATED(vec_p),"Vector not ALLOCATED")
      ASSERT(vec_p%isInit, "Vector not initialized")
      SELECTTYPE(vec_p); TYPE IS(RealVectorType)
        ASSERT(.TRUE.,"Vector TYPE")
      CLASS DEFAULT
        ASSERT(.FALSE.,"Vector TYPE")
      ENDSELECT
      
      CALL VectorResemble(other_vec_p,vec_p,params)
      ASSERT(ASSOCIATED(other_vec_p),"Cloned vector not ALLOCATED")
      ASSERT(other_vec_p%isInit,"Other vector not initialized")
      ASSERT(other_vec_p%n==vec_p%n,"Cloned vector %n")
      SELECTTYPE(other_vec_p); TYPE IS(RealVectorType)
        ASSERT(.TRUE.,"Resemble vector type")
      CLASS DEFAULT
        ASSERT(.FALSE.,"Resemble vector type")
      ENDSELECT
      CALL other_vec_p%clear()
      DEALLOCATE(other_vec_p)
      NULLIFY(other_vec_p)
      
      CALL params%add("VectorType->n",5_SIK)
      CALL VectorResemble(other_vec_p,vec_p,params)
      ASSERT(other_vec_p%n==5_SIK,"Cloned overridden vector %n")
      CALL other_vec_p%clear()
      DEALLOCATE(other_vec_p)
      NULLIFY(other_vec_p)

      CALL params%clear()
      CALL vec_p%clear()
      DEALLOCATE(vec_p)
      NULLIFY(vec_p)

      ! PETSc
#ifdef FUTILITY_HAVE_PETSC
      CALL params%add("VectorType->n",10_SIK)
      CALL params%add("VectorType->nlocal",10_SIK)
      CALL params%add("VectorType->engine",VM_PETSC)
      CALL params%add("VectorType->MPI_Comm_ID",PE_COMM_SELF)
      CALL VectorFactory(vec_p,params)
      ASSERT(ASSOCIATED(vec_p),"PETSc vector associated")
      ASSERT(vec_p%isInit,"PETSc vector initialized")
      SELECTTYPE(vec_p); TYPE IS(PETScVectorType)
        ASSERT(.TRUE.,"PETSc type")
        dvec_p => vec_p
      CLASS DEFAULT
        ASSERT(.FALSE.,"PETSc type")
      ENDSELECT
      CALL params%clear()

      ! Resemble, no param override
      CALL VectorResemble(other_vec_p,vec_p,params)
      ASSERT(ASSOCIATED(other_vec_p),"Resemble vector associated")
      ASSERT(other_vec_p%isInit,"Resemble vector %isInit")
      SELECTTYPE(other_vec_p); TYPE IS(PETScVectorType)
        ASSERT(other_vec_p%n == dvec_p%n,"Resemble vector %n") 
        ASSERT(other_vec_p%nlocal == dvec_p%nlocal,"Resemble vector %nlocal") 
      CLASS DEFAULT
        ASSERT(.FALSE.,"Resemble vector type")
      ENDSELECT
      CALL other_vec_p%clear()
      DEALLOCATE(other_vec_p)
      NULLIFY(other_vec_p)

      ! Resemble, override params
      CALL params%add("VectorType->n",5_SIK)
      CALL params%add("VectorType->nlocal",5_SIK)
      CALL VectorResemble(other_vec_p,vec_p,params)
      ASSERT(ASSOCIATED(other_vec_p),"Resemble vector associated")
      ASSERT(other_vec_p%isInit,"Resemble vector %isInit")
      SELECTTYPE(other_vec_p); TYPE IS(PETScVectorType)
        ASSERT(other_vec_p%n == 5_SIK,"Resemble vector %n") 
        ASSERT(other_vec_p%nlocal == 5_SIK,"Resemble vector %nlocal") 
      CLASS DEFAULT
        ASSERT(.FALSE.,"Resemble vector type")
      ENDSELECT
      CALL other_vec_p%clear()
      DEALLOCATE(other_vec_p)
      NULLIFY(other_vec_p)

      ! Clean up
      NULLIFY(dvec_p)
      CALL vec_p%clear()
      DEALLOCATE(vec_p)
      NULLIFY(vec_p)
#endif

      ! Trilinos
#ifdef FUTILITY_HAVE_ForTrilinos
      CALL params%add("VectorType->n",10_SIK)
      CALL params%add("VectorType->nlocal",10_SIK)
      CALL params%add("VectorType->engine",VM_TRILINOS)
      CALL params%add("VectorType->MPI_Comm_ID",PE_COMM_SELF)
      CALL VectorFactory(vec_p,params)
      ASSERT(ASSOCIATED(vec_p),"Trilinos vector associated")
      ASSERT(vec_p%isInit,"Trilinos vector initialized")
      SELECTTYPE(vec_p); TYPE IS(TrilinosVectorType)
        ASSERT(.TRUE.,"Trilinos type")
        dvec_p => vec_p
      CLASS DEFAULT
        ASSERT(.FALSE.,"Trilinos type")
      ENDSELECT
      CALL params%clear()

      ! Resemble, no param override
      CALL VectorResemble(other_vec_p,vec_p,params)
      ASSERT(ASSOCIATED(other_vec_p),"Resemble vector associated")
      ASSERT(other_vec_p%isInit,"Resemble vector %isInit")
      SELECTTYPE(other_vec_p); TYPE IS(TrilinosVectorType)
        ASSERT(other_vec_p%n == dvec_p%n,"Resemble vector %n") 
        ASSERT(other_vec_p%nlocal == dvec_p%nlocal,"Resemble vector %nlocal") 
      CLASS DEFAULT
        ASSERT(.FALSE.,"Resemble vector type")
      ENDSELECT
      CALL other_vec_p%clear()
      DEALLOCATE(other_vec_p)
      NULLIFY(other_vec_p)

      ! Resemble, override params
      CALL params%add("VectorType->n",5_SIK)
      CALL params%add("VectorType->nlocal",5_SIK)
      CALL VectorResemble(other_vec_p,vec_p,params)
      ASSERT(ASSOCIATED(other_vec_p),"Resemble vector associated")
      ASSERT(other_vec_p%isInit,"Resemble vector %isInit")
      SELECTTYPE(other_vec_p); TYPE IS(TrilinosVectorType)
        ASSERT(other_vec_p%n == 5_SIK,"Resemble vector %n") 
        ASSERT(other_vec_p%nlocal == 5_SIK,"Resemble vector %nlocal") 
      CLASS DEFAULT
        ASSERT(.FALSE.,"Resemble vector type")
      ENDSELECT
      CALL other_vec_p%clear()
      DEALLOCATE(other_vec_p)
      NULLIFY(other_vec_p)

      ! Clean up
      NULLIFY(dvec_p)
      CALL vec_p%clear()
      DEALLOCATE(vec_p)
      NULLIFY(vec_p)
#endif

      CALL params%clear()
      DEALLOCATE(params)

    ENDSUBROUTINE testVectorFactory
!
ENDPROGRAM testVectorTypes
