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
  USE FileType_HDF5
  USE Strings

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
  TYPE(DenseSquareMatrixType),ALLOCATABLE :: denseSample(:)
  TYPE(ExceptionHandlerType),TARGET :: e
  CLASS(PETScVectorType),ALLOCATABLE :: xPetsc, yPetsc
  TYPE(ParamType) :: bandedPlist,petscPList,vecPList,denseMatPlist

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
  WRITE(*,*) "Passed setup"
  CREATE_TEST('Verify results')
  !REGISTER_SUBTEST('Products Match',verifyFD)
  REGISTER_SUBTEST('Products Match',verifyCMFD)

  !CREATE_TEST('Matvec CMFD Timing Results')
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
      INTEGER(SIK) :: n,nnz,nnz_banded,i,j,rank,mpierr,ios,lowIdx,highIdx,offset,dmat
      REAL(SRK) :: tmpreal
      CHARACTER(200)::tempcharacter,dirname
      TYPE(TimerType) :: bandAssemble,petscAssemble
      TYPE(StringType),ALLOCATABLE :: contents(:)
      TYPE(HDF5FileType) :: inputData
      INTEGER(SIK),ALLOCATABLE :: matrix_row(:),matrix_col(:),src_idx(:),sol_idx(:)
      REAL(SRK),ALLOCATABLE :: matrix_val(:),src_val(:),sol_val(:)


      ALLOCATE(DistributedBandedMatrixType :: cmfdBanded)
      ALLOCATE(PETScMatrixType :: cmfdPetsc)
      ALLOCATE(denseSample(162))

      CALL MPI_Comm_rank(PE_COMM_WORLD,rank,mpierr)
      CALL bandAssemble%setTimerName('Banded Matrix Assemble Time')
      CALL petscAssemble%setTimerName('PETSc Matrix Assemble Time')

      n = 148716
      nnz = 6015708
      nnz_banded = 583848

      CALL bandedPList%clear()
      CALL bandedPlist%add('MatrixType->n',n)
      CALL bandedPlist%add('MatrixType->m',n)
      CALL bandedPlist%add('MatrixType->nnz',nnz_banded)
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

      CALL denseMatPlist%clear()
      CALL denseMatPlist%add('MatrixType->n',51)
      CALL denseMatPlist%add('MatrixType->isSym',.FALSE.)

      CALL cmfdBanded%init(bandedPList)
      CALL cmfdPetsc%init(petscPList)
      DO i=1,SIZE(denseSample)
        CALL denseSample(i)%init(denseMatPlist)
      ENDDO

      ALLOCATE(NativeDistributedVectorType :: x)
      ALLOCATE(NativeDistributedVectorType :: y)
      ALLOCATE(petscVectorType :: xPetsc)
      ALLOCATE(petscVectorType :: yPetsc)

      CALL xPetsc%init(vecPList)
      CALL yPetsc%init(vecPList)
      CALL x%init(vecPList)
      CALL y%init(vecPList)

      WRITE(dirname,'(2A)'),'/home/mkbz/Research/bandMatResults/largeCMFD/it_end.hdf5'
      CALL inputData%init(dirname,'READ')
      CALL inputData%fopen()
     
      CALL inputData%ls('',contents)
      WRITE(*,*) TRIM(contents(1))
      WRITE(*,*) TRIM(contents(2))
      WRITE(*,*) TRIM(contents(3))

      WRITE(*,*) "Reading cmfdmatrix" 
      CALL inputData%fread('cmfd_matrix/rows',matrix_row)
      CALL inputData%fread('cmfd_matrix/cols',matrix_col)
      CALL inputData%fread('cmfd_matrix/vals',matrix_val)

      WRITE(*,*) "Reading sol" 
      CALL inputData%fread('sol_vec/idx',sol_idx)
      CALL inputData%fread('sol_vec/val',sol_val)
      
      WRITE(*,*) "Reading src" 
      CALL inputData%fread('src_vec/idx',src_idx)
      CALL inputData%fread('src_vec/val',src_val)
     
      WRITE(*,*) "Setting banded elements" 
      DO i=1,SIZE(matrix_row)
        IF ((matrix_row(i)-1)/51 /= (matrix_col(i)-1)/51) THEN
          CALL cmfdBanded%set(matrix_row(i),matrix_col(i),matrix_val(i))
        ELSE IF ((matrix_row(i)-1)/51 >= rank*162 .AND. (matrix_row(i)-1)/51 < (rank+1)*162) THEN
          dmat = (matrix_row(i)-1)/51+1 - rank*162
          offset = 51*(dmat-1+rank*162) 
          CALL denseSample(dmat)%set(matrix_row(i)-offset,matrix_col(i)-offset,matrix_val(i))
        END IF
      END DO
      
      WRITE(*,*) "Setting petsc elements"
      highIdx = 0
      DO WHILE (highIdx < SIZE(matrix_row))
        lowIdx = highIdx + 1
        highIdx = lowIdx
        IF (rank==0 .AND. MOD(matrix_row(highIdx),100) == 0) &
          WRITE(*,*) "On row",matrix_row(highIdx),"/",matrix_row(SIZE(matrix_row))
        DO WHILE (matrix_row(highIdx) == matrix_row(lowIdx))
          highIdx = highIdx + 1
          IF (highIdx == SIZE(matrix_row)) EXIT
        END DO
        IF (highIdx == SIZE(matrix_row)) THEN
          CALL cmfdPetsc%setRow(matrix_row(lowIdx),matrix_col(lowIdx:highIdx),matrix_val(lowIdx:highIdx))
          EXIT
        ENDIF
        highIdx = highIdx - 1
        CALL cmfdPetsc%setRow(matrix_row(lowIdx),matrix_col(lowIdx:highIdx),matrix_val(lowIdx:highIdx))
      END DO

      !DO i=1,SIZE(src_idx)
      !  trueSrc(src_idx(i)) = src_val(i)
      !END DO

      DO i=1,SIZE(sol_idx)
        CALL x%set(sol_idx(i),sol_val(i))
        IF (i > cmfdBanded%iOffsets(rank+1) .AND. i <= cmfdBanded%iOffsets(rank+2)) &
          CALL xPetsc%set(sol_idx(i),sol_val(i))
      END DO
      CALL inputData%clear()
 
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
      INTEGER(SIK) :: i,j,lowIdx,highIdx,rank,mpierr
      LOGICAL(SBK) :: bool
      REAL(SRK),ALLOCATABLE :: dummyvec(:)
      
      CALL MPI_Comm_rank(PE_COMM_WORLD,rank,mpierr)
      WRITE(*,*) "Executing BLAS_matvec (banded)"
      CALL BLAS_matvec(THISMATRIX=cmfdBanded,X=x,Y=y,alpha=1.0_SRK,beta=0.0_SRK)
      DO j=1,SIZE(denseSample)
        lowIdx = 1 + (j-1)*51
        highIdx = j*51
        CALL BLAS_matvec(THISMATRIX=denseSample(j),X=x%b(lowIdx:highIdx),Y=y%b(lowIdx:highIdx),ALPHA=1.0_SRK,BETA=1.0_SRK)
      END DO
      
      
      WRITE(*,*) "Executing BLAS_matvec (petsc)"
      CALL BLAS_matvec(THISMATRIX=cmfdPetsc,X=xPetsc,Y=yPetsc,alpha=1.0_SRK,beta=0.0_SRK)

      WRITE(*,*) "Comparing"
      lowIdx = cmfdBanded%iOffsets(rank+1) + 1
      highIdx = cmfdBanded%iOffsets(rank+2)

      ALLOCATE(dummyvec(highIdx - lowIdx+1))

      DO i=lowIdx,highIdx
        CALL yPetsc%get(i,dummyvec(i-lowIdx+1))
      ENDDO

      ASSERT(ALL(ABS(dummyvec - y%b) < 1.0e-12),'products match')

      CALL vecPList%clear()
      CALL petscPList%clear()
      CALL bandedPlist%clear()

    END SUBROUTINE verifyCMFD



    SUBROUTINE timeBandedCMFD()

      REAL(SRK) :: timetaken,tmpreal
      INTEGER(SIK) :: i,j,n,nnz,xCoord,yCoord,gridSize,lowIdx,highIdx,ios,mpierr
      TYPE(TimerType) :: bandMult

      CALL bandMult%setTimerName('Banded Multiplication Time')

      ! Loop multiply banded*x = y
      CALL bandMult%tic()
      DO i=1,64*8
        CALL BLAS_matvec(THISMATRIX=cmfdBanded,X=x,Y=y,BETA=0.0_SRK)
        DO j=1,SIZE(denseSample)
          lowIdx = 1 + (j-1)*51
          highIdx = j*51
          CALL BLAS_matvec(THISMATRIX=denseSample(j),X=x%b(lowIdx:highIdx),Y=y%b(lowIdx:highIdx),ALPHA=1.0_SRK,BETA=1.0_SRK)
        END DO
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
      INTEGER(SIK) :: xCoord,yCoord,i,gridsize,n,nnz,time1,time2,clock_rate,ios,j,mpierr
      REAL(SRK) :: timeTaken,tmpreal
      TYPE(TimerType) :: petscMult

      CALL petscMult%setTimerName('Petsc Multiplication Time')

      ! Get clock
      CALL petscMult%tic()
      ! Loop multiply banded*x = y
      DO i=1,64*8
        CALL BLAS_matvec(THISMATRIX=cmfdPetsc,X=xPetsc,Y=yPetsc,BETA=0.0_SRK)
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
