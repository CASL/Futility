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
  USE Times

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

  CLASS(DistributedBandedMatrixType),ALLOCATABLE :: cmfdBanded
  CLASS(PETScMatrixType),ALLOCATABLE :: cmfdPetsc
  TYPE(NativeDistributedVectorType),ALLOCATABLE :: x,y
  TYPE(ExceptionHandlerType),TARGET :: e
  CLASS(PETScVectorType),ALLOCATABLE :: xPetsc, yPetsc
  TYPE(ParamType) :: bandedPlist,petscPList,vecPList

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

  CALL setup()
  CREATE_TEST('Verify results')
  !REGISTER_SUBTEST('Products Match',verifyFD)
  REGISTER_SUBTEST('Products Match',verifyCMFD)


  !CREATE_TEST('Matvec FD Timing Results')
  !REGISTER_SUBTEST('Petsc',timePetsc)
  !REGISTER_SUBTEST('Banded',timeBanded)

  CREATE_TEST('Matvec CMFD Timing Results')
  REGISTER_SUBTEST('Petsc',timePetscCMFD)
  REGISTER_SUBTEST('Banded',timeBandedCMFD)
  FINALIZE_TEST()

  CALL vecPList%clear()
  CALL bandedPlist%clear()
  CALL petscPlist%clear()
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
  
    SUBROUTINE setup()
      INTEGER(SIK) :: n,nnz,i,j,rank,mpierr,ios
      REAL(SRK) :: tmpreal
      CHARACTER(200)::tempcharacter,dirname
      TYPE(TimerType) :: bandAssemble,petscAssemble

      ALLOCATE(DistributedBandedMatrixType :: cmfdBanded)
      ALLOCATE(PETScMatrixType :: cmfdPetsc)

      CALL MPI_Comm_rank(PE_COMM_WORLD,rank,mpierr)
      CALL bandAssemble%setTimerName('Banded Matrix Assemble Time')
      CALL petscAssemble%setTimerName('PETSc Matrix Assemble Time')

      n = 148716
      nnz = 6015708

      CALL bandedPList%clear()
      CALL bandedPlist%add('MatrixType->n',n)
      CALL bandedPlist%add('MatrixType->m',n)
      CALL bandedPlist%add('MatrixType->nnz',nnz)
      CALL bandedPList%add('MatrixType->MPI_Comm_ID',PE_COMM_WORLD)

      CALL petscPList%clear()
      CALL petscPlist%add('MatrixType->n',n)
      CALL petscPlist%add('MatrixType->m',n)
      CALL petscPlist%add('MatrixType->nnz',nnz)
      CALL petscPlist%add('MatrixType->isSym',.FALSE.)
      CALL petscPlist%add('MatrixType->matType',SPARSE)
      CALL petscPlist%add('MatrixType->MPI_Comm_ID',PE_COMM_WORLD)

      CALL vecPList%clear()
      CALL vecPList%add('VectorType->n',n)
      CALL vecPList%add('VectorType->MPI_Comm_ID',PE_COMM_WORLD)


      CALL cmfdBanded%init(bandedPList)
      CALL cmfdPetsc%init(petscPList)

      ALLOCATE(NativeDistributedVectorType :: x)
      ALLOCATE(NativeDistributedVectorType :: y)
      ALLOCATE(petscVectorType :: xPetsc)
      ALLOCATE(petscVectorType :: yPetsc)

      CALL xPetsc%init(vecPList)
      CALL yPetsc%init(vecPList)
      CALL x%init(vecPList)
      CALL y%init(vecPList)

      WRITE(*,*) "Reading in data..."
      WRITE(dirname,'(2A)'),'/home/mkbz/Research/genCMFD/it_end.cmfd.txt'
      !RITE(dirname,'(2A)'),'/home/mkbz/git/Futility/unit_tests/testLinearSolver/matrices/mg_matrix.txt'

      OPEN(UNIT=11,FILE=dirname,STATUS='OLD',ACTION='READ',IOSTAT=ios,IOMSG=tempcharacter)
      IF(ios .NE. 0)THEN
          WRITE(*,*)tempcharacter
          WRITE(*,*)'Could not open res.dat'
          STOP
      END IF
      READ(11,*)
      DO
          READ(11,*,IOSTAT=ios)i,j,tmpreal
          IF(ios >0)THEN
              STOP 'File input error'
          ELSE IF(ios<0)THEN
              EXIT
          ELSE
            CALL cmfdBanded%set(i,j,tmpreal)
            CALL cmfdPetsc%set(i,j,tmpreal)
          END IF
      END DO
      CLOSE(11)

      WRITE(dirname,'(2A)'),'/home/mkbz/Research/genCMFD/it_end.src.txt'
      !RITE(dirname,'(2A)'),'/home/mkbz/git/Futility/unit_tests/testLinearSolver/matrices/mg_matrix.txt'

      !OPEN(UNIT=11,FILE=dirname,STATUS='OLD',ACTION='READ',IOSTAT=ios,IOMSG=tempcharacter)
      !IF(ios .NE. 0)THEN
      !    WRITE(*,*)tempcharacter
      !    WRITE(*,*)'Could not open res.dat'
      !    STOP
      !END IF
      !READ(11,*)
      !DO
      !    READ(11,*,IOSTAT=ios)i,tmpreal
      !    IF(ios >0)THEN
      !        STOP 'File input error'
      !    ELSE IF(ios<0)THEN
      !        EXIT
      !    ELSE
      !      trueSrc(i) = tmpreal
      !    END IF
      !END DO
      !CLOSE(11) 

      WRITE(dirname,'(2A)'),'/home/mkbz/Research/genCMFD/it_end.sol.txt'
      !RITE(dirname,'(2A)'),'/home/mkbz/git/Futility/unit_tests/testLinearSolver/matrices/mg_matrix.txt'

      OPEN(UNIT=11,FILE=dirname,STATUS='OLD',ACTION='READ',IOSTAT=ios,IOMSG=tempcharacter)
      IF(ios .NE. 0)THEN
          WRITE(*,*)tempcharacter
          WRITE(*,*)'Could not open res.dat'
          STOP
      END IF
      READ(11,*)
      DO
          READ(11,*,IOSTAT=ios)i,tmpreal
          IF(ios >0)THEN
              STOP 'File input error'
          ELSE IF(ios<0)THEN
              EXIT
          ELSE
            CALL x%set(i,tmpreal)
            IF (i > cmfdBanded%iOffsets(rank+1) .AND. i <= cmfdBanded%iOffsets(rank+2))  CALL xPetsc%set(i,tmpreal)
          END IF
      END DO
      CLOSE(11)
 
      CALL MPI_Barrier(PE_COMM_WORLD,mpierr)
      WRITE(*,*) "Assembling cmfdBanded"
      CALL bandAssemble%tic()
      CALL cmfdBanded%assemble()
      CALL bandAssemble%toc()
      
      CALL MPI_Barrier(PE_COMM_WORLD,mpierr)
      WRITE(*,*) "Assembling cmfdPetsc"
      CALL petscAssemble%tic()
      CALL cmfdPetsc%assemble()
      CALL petscAssemble%toc()
   
      WRITE(*,*) 'Timer '//bandAssemble%getTimerName()//' had time '// &
                 bandAssemble%getTimeChar()
      WRITE(*,*) bandAssemble%getTimeHHMMSS()
      WRITE(*,*) bandAssemble%getTimeReal()
      
      WRITE(*,*) 'Timer '//petscAssemble%getTimerName()//' had time '// &
                 petscAssemble%getTimeChar()
      WRITE(*,*) petscAssemble%getTimeHHMMSS()
      WRITE(*,*) petscAssemble%getTimeReal()
 
    END SUBROUTINE setup

    SUBROUTINE verifyCMFD()
      REAL(SRK) :: tmpreal,tmpreal2
      INTEGER(SIK) :: i,lowIdx,highIdx,rank,mpierr
      LOGICAL(SBK) :: bool
      
      CALL MPI_Comm_rank(PE_COMM_WORLD,rank,mpierr)
      WRITE(*,*) "Executing BLAS_matvec (banded)"
      CALL BLAS_matvec(THISMATRIX=cmfdBanded,X=x,Y=y,alpha=1.0_SRK,beta=0.0_SRK)
      WRITE(*,*) "Executing BLAS_matvec (petsc)"
      CALL BLAS_matvec(THISMATRIX=cmfdPetsc,X=xPetsc,Y=yPetsc,alpha=1.0_SRK,beta=0.0_SRK)

      WRITE(*,*) "Comparing"
      lowIdx = cmfdBanded%iOffsets(rank+1) + 1
      highIdx = cmfdBanded%iOffsets(rank+2)

      DO i=lowIdx,highIdx
        CALL yPetsc%get(i,tmpreal2)
        CALL y%get(i,tmpreal)
        ASSERT(ABS(tmpreal - tmpreal2) < 1.0e-12,'petsc works')
      ENDDO

      CALL vecPList%clear()
      CALL petscPList%clear()
      CALL bandedPlist%clear()

    END SUBROUTINE verifyCMFD



    SUBROUTINE timeBandedCMFD()

      REAL(SRK) :: timetaken,tmpreal
      INTEGER(SIK) :: i,j,n,nnz,xCoord,yCoord,gridSize,time1,time2,clock_rate,ios
      TYPE(TimerType) :: bandMult

      CALL bandMult%setTimerName('Banded Multiplication Time')

      ! Loop multiply banded*x = y
      CALL bandMult%tic()
      DO i=1,64*8
        CALL BLAS_matvec(THISMATRIX=cmfdBanded,X=x,Y=y)
      END DO
      ! Get clock
      CALL bandMult%toc()
      ! report total time
      WRITE(*,*) 'Timer '//bandMult%getTimerName()//' had time '// &
                 bandMult%getTimeChar()
      WRITE(*,*) bandMult%getTimeHHMMSS()
      WRITE(*,*) bandMult%getTimeReal()

    END SUBROUTINE timeBandedCMFD


    SUBROUTINE timePetscCMFD()
      INTEGER(SIK) :: xCoord,yCoord,i,gridsize,n,nnz,time1,time2,clock_rate,ios,j
      REAL(SRK) :: timeTaken,tmpreal
      TYPE(TimerType) :: petscMult

      CALL petscMult%setTimerName('Petsc Multiplication Time')

      ! Get clock
      CALL petscMult%tic()
      ! Loop multiply banded*x = y
      DO i=1,64*8
        CALL BLAS_matvec(THISMATRIX=cmfdPetsc,X=x,Y=y)
      END DO
      ! Get clock
      CALL petscMult%toc()
      ! report total time
      WRITE(*,*) 'Timer '//petscMult%getTimerName()//' had time '// &
                 petscMult%getTimeChar()
      WRITE(*,*) petscMult%getTimeHHMMSS()
      WRITE(*,*) petscMult%getTimeReal()

    END SUBROUTINE timePetscCMFD

ENDPROGRAM testMatrixTypes
