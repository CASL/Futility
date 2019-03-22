!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
PROGRAM testMatrixTypes
#include "Futility_DBC.h"
#include "UnitTest.h"
  USE ISO_FORTRAN_ENV
  USE ISO_C_BINDING
  USE UnitTest
  USE IntrType
  USE ExceptionHandler
  USE Futility_DBC
  USE trilinos_interfaces
  USE ParameterLists
  USE ParallelEnv
  USE VectorTypes
  USE MatrixTypes

  IMPLICIT NONE

#ifdef FUTILITY_HAVE_PETSC
#include <petscversion.h>
#if ((PETSC_VERSION_MAJOR>=3) && (PETSC_VERSION_MINOR>=6))
#include <petsc/finclude/petsc.h>
#else
#include <finclude/petsc.h>
#endif
#undef IS
  PetscErrorCode  :: ierr
#else
#ifdef HAVE_MPI
#include <mpif.h>
  INTEGER :: ierr
#endif
#endif

  TYPE(ExceptionHandlerType),TARGET :: e
  TYPE(ParamType) :: pList,optListMat,vecPList

  !Configure exception handler for test
  CALL e%setStopOnError(.FALSE.)
  CALL e%setQuietMode(.TRUE.)
  CALL eParams%addSurrogate(e)
  CALL eMatrixType%addSurrogate(e)

#ifdef FUTILITY_HAVE_PETSC
  CALL PetscInitialize(PETSC_NULL_CHARACTER,ierr)
#else
#ifdef HAVE_MPI
  CALL MPI_Init(ierr)
#endif
#endif

  CREATE_TEST('Matvec Timing Results')
  REGISTER_SUBTEST('Petsc',timePetsc)
  REGISTER_SUBTEST('Banded',timeBanded)
  FINALIZE_TEST()

  CALL optListMat%clear()
  CALL vecPList%clear()
  CALL pList%clear()
  CALL MatrixTypes_Clear_ValidParams()
  CALL VectorType_Clear_ValidParams()

#ifdef FUTILITY_HAVE_PETSC
  CALL PetscFinalize(ierr)
#endif

!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------

    SUBROUTINE timeBanded()

      CLASS(BandedMatrixType),ALLOCATABLE :: fdBanded
      REAL(SRK),ALLOCATABLE :: x(:),y(:)
      REAL(SRK) :: timetaken
      INTEGER(SIK) :: i,n,nnz,xCoord,yCoord,gridSize,time1,time2,clock_rate
      TYPE(ParamType) :: bandedPlist

      ! Create finite difference matrix
      ALLOCATE(BandedMatrixType :: fdBanded)
      n = 16384
      gridSize = 128
      nnz = 1
      IF (n > 1) THEN
        nnz = nnz + 11
      END IF
      IF (n > 2) THEN
        nnz = nnz + 5*(gridSize-2)*(gridSize-2) + 16*(gridSize-2)
      END IF
      CALL bandedPlist%add('MatrixType->n',n)
      CALL bandedPlist%add('MatrixType->m',n)
      CALL bandedPlist%add('MatrixType->nnz',nnz)
      CALL bandedPlist%validate(bandedPlist)
      CALL fdBanded%init(bandedPlist)

      DO i=1,n
        yCoord = (i-1)/gridSize
        xCoord = MOD(i-1,gridSize)
        IF (yCoord > 0) THEN
          CALL fdBanded%set(i,i-gridSize,-1.0_SRK)
        END IF
        IF (xCoord > 0) THEN
          CALL fdBanded%set(i, i-1,-1.0_SRK)
        END IF
        CALL fdBanded%set(i,i,4.0_SRK)
        IF (xCoord < gridSize-1) THEN
          CALL fdBanded%set(i, i+1,-1.0_SRK)
        END IF
        IF (yCoord < gridSize-1) THEN
          CALL fdBanded%set(i,i+gridSize,-1.0_SRK)
        END IF
      END DO

      WRITE(*,*) "Beginning Assemble"
      CALL fdBanded%assemble()
      WRITE(*,*) "Completed Assemble"

      ALLOCATE(x(n))
      ALLOCATE(y(n))

      x = 1.0_SRK

      ! Get clock
      CALL SYSTEM_CLOCK(time1)
        ! Loop multiply banded*x = y
      DO i=1,n
        CALL BLAS_matvec(THISMATRIX=fdBanded,X=x,Y=y)
      END DO
      ! Get clock
      CALL SYSTEM_CLOCK(time2,clock_rate)
      ! report total time
      timetaken = (time2*1.0_SRK - time1*1.0_SRK)/(clock_rate*1.0_SRK)
      WRITE(*,*) n,"Multiplications completed in",timetaken,"seconds"

    END SUBROUTINE timeBanded


    SUBROUTINE timePetsc()
      CLASS(MatrixType),ALLOCATABLE :: fdPetsc
      CLASS(VectorType),ALLOCATABLE :: x,y
      TYPE(ParamType) :: petscPlist
      INTEGER(SIK) :: xCoord,yCoord,i,gridsize,n,nnz,time1,time2,clock_rate
      REAL(SRK) :: timeTaken

      ! Create finite difference matrix
      n = 16384
      gridSize = 128
      nnz = 1
      IF (n > 1) THEN
        nnz = nnz + 11
      END IF
      IF (n > 2) THEN
        nnz = nnz + 5*(gridSize-2)*(gridSize-2) + 16*(gridSize-2)
      END IF

      ALLOCATE(PETScMatrixType :: fdPetsc)
      CALL petscPlist%add('MatrixType->n',n)
      CALL petscPlist%add('MatrixType->m',n)
      CALL petscPlist%add('MatrixType->nnz',nnz)
      CALL petscPlist%add('MatrixType->isSym',.FALSE.)
      CALL petscPlist%add('MatrixType->matType',SPARSE)
      CALL petscPlist%add('MatrixType->MPI_Comm_ID',PE_COMM_SELF)
      CALL petscPlist%validate(petscPlist)
      CALL fdPetsc%init(petscPlist)

      CALL vecPList%add('VectorType->n',n)
      CALL vecPList%add('VectorType->MPI_Comm_ID',PE_COMM_SELF)

      ALLOCATE(PETScVectorType :: x)
      ALLOCATE(PETScVectorType :: y)
      CALL x%init(vecPList)
      CALL y%init(vecPList)

      DO i=1,n
        yCoord = (i-1)/gridSize
        xCoord = MOD(i-1,gridSize)
        IF (yCoord > 0) THEN
          CALL fdPetsc%set(i,i-gridSize,-1.0_SRK)
        END IF
        IF (xCoord > 0) THEN
          CALL fdPetsc%set(i, i-1,-1.0_SRK)
        END IF
        CALL fdPetsc%set(i,i,4.0_SRK)
        IF (xCoord < gridSize-1) THEN
          CALL fdPetsc%set(i, i+1,-1.0_SRK)
        END IF
        IF (yCoord < gridSize-1) THEN
          CALL fdPetsc%set(i,i+gridSize,-1.0_SRK)
        END IF
      END DO

      CALL x%setAll_scalar(1.0_SRK)

      ! Get clock
      WRITE(*,*) "Beginning multiplication loop"
      CALL SYSTEM_CLOCK(time1)
        ! Loop multiply banded*x = y
      DO i=1,n
        CALL BLAS_matvec(THISMATRIX=fdPetsc,X=x,Y=y)
      END DO
      ! Get clock
      CALL SYSTEM_CLOCK(time2,clock_rate)
      ! report total time
      timetaken = (time2*1.0_SRK - time1*1.0_SRK)/(clock_rate*1.0_SRK)
      WRITE(*,*) n,"Multiplications completed in",timetaken,"seconds"

      ! report total time

    END SUBROUTINE timePetsc

ENDPROGRAM testMatrixTypes
