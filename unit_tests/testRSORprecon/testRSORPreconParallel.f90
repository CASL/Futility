!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
! test with ./unit_tests/testRSORprecon/Futility_testRSORprecon.exe
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
PROGRAM testRSORPreconParallel
#include "UnitTest.h"

USE ISO_FORTRAN_ENV
USE UnitTest
USE IntrType
USE ExceptionHandler
USE BLAS
USE ParameterLists
USE ParallelEnv
USE VectorTypes
USE MatrixTypes
USE PreconditionerTypes
IMPLICIT NONE

#ifdef HAVE_MPI
INCLUDE 'mpif.h'
#endif
#ifdef HAVE_MPI
TYPE(ExceptionHandlerType),TARGET :: e
TYPE(ParamType) :: PListMat,PListVec,PListRSOR
CLASS(MatrixType),ALLOCATABLE :: testBandedMatrix,testBlockBandedMatrix
TYPE(NativeDistributedVectorType) :: testVector,workVector,refVector
CLASS(VectorType),ALLOCATABLE :: testVec_1g,testVec_mg

INTEGER :: mpierr
CALL MPI_Init(mpierr)

!Configure exception handler for test
CALL e%setStopOnError(.FALSE.)
CALL e%setQuietMode(.TRUE.)
CALL eParams%addSurrogate(e)
CALL ePreCondType%addSurrogate(e)

CREATE_TEST('Test Parallel RSOR Preconditioner')

CALL setupRSORTest()
REGISTER_SUBTEST('Test Parallel RSOR Preconditioner Type',testRSOR_PreCondType)
CALL clearTest()

FINALIZE_TEST()
CALL MPI_Finalize(mpierr)
!
!===============================================================================
CONTAINS
!
!-------------------------------------------------------------------------------
SUBROUTINE setupRSORTest()
  INTEGER(SIK)::i,j,numnonzero,rank,nproc
  REAL(SRK)::tmpreal1(9*9),tmpreal2(9)

  CALL PListRSOR%add('PreCondType->omega',1.0_SRK)
  CALL PListRSOR%add('PreCondType->comm',PE_COMM_WORLD)
  CALL PListVec%add('VectorType->n',9)
  CALL PListVec%add('VectorType->chunkSize',3)
  CALL PListVec%add('VectorType->MPI_Comm_ID',PE_COMM_WORLD)
  CALL PListVec%add('VectorType->nlocal',-1)
  CALL testVector%init(PListVec)
  CALL workVector%init(PListVec)
  CALL refVector%init(PListVec)
  CALL PListMat%add('MatrixType->n',9)
  CALL PListMat%add('MatrixType->m',9)
  CALL PListMat%add('MatrixType->blockSize',3)
  CALL PListMat%add('MatrixType->MPI_Comm_ID',PE_COMM_WORLD)
  CALL PListVec%add('MatrixType->nlocal',-1)

  CALL MPI_Comm_rank(PE_COMM_WORLD,rank,mpierr)
  CALL MPI_Comm_size(PE_COMM_WORLD,nproc,mpierr)

  ASSERT(nproc==2, 'nproc valid')

  tmpreal1=(/-64_SRK, 16_SRK,  0_SRK, 16_SRK,  0_SRK,  0_SRK,  0_SRK,  0_SRK,  0_SRK, &
              16_SRK,-64_SRK, 16_SRK,  0_SRK, 16_SRK,  0_SRK,  0_SRK,  0_SRK,  0_SRK, &
               0_SRK, 16_SRK,-64_SRK,  0_SRK,  0_SRK, 16_SRK,  0_SRK,  0_SRK,  0_SRK, &
              16_SRK,  0_SRK,  0_SRK,-64_SRK, 16_SRK,  0_SRK, 16_SRK,  0_SRK,  0_SRK, &
               0_SRK, 16_SRK,  0_SRK, 16_SRK,-64_SRK, 16_SRK,  0_SRK, 16_SRK,  0_SRK, &
               0_SRK,  0_SRK, 16_SRK,  0_SRK, 16_SRK,-64_SRK,  0_SRK,  0_SRK, 16_SRK, &
               0_SRK,  0_SRK,  0_SRK, 16_SRK,  0_SRK,  0_SRK,-64_SRK, 16_SRK,  0_SRK, &
               0_SRK,  0_SRK,  0_SRK,  0_SRK, 16_SRK,  0_SRK, 16_SRK,-64_SRK, 16_SRK, &
               0_SRK,  0_SRK,  0_SRK,  0_SRK,  0_SRK, 16_SRK,  0_SRK, 16_SRK,-64_SRK/)

  !setup the dense matrixk and count the number of nonzeroentries
  numnonzero=0
  DO i=1,9
    DO j=1,9
      IF(tmpreal1((i-1)*9+j) /= 0_SRK) numnonzero=numnonzero+1
    ENDDO
  ENDDO

  !setup the sparse version of the matrix
  CALL PListMat%add('MatrixType->nnz',INT(numnonzero,kind=8))

  !setup the banded version of the matrix
  ALLOCATE(DistributedBandedMatrixType :: testBandedMatrix)
  ALLOCATE(DistributedBlockBandedMatrixType :: testBlockBandedMatrix)
  CALL testBandedMatrix%init(PListMat)
  CALL testBlockBandedMatrix%init(PListMat)
  SELECT TYPE(testBandedMatrix); TYPE IS(DistributedBandedMatrixType)
    SELECT TYPE(testBlockBandedMatrix); TYPE IS(DistributedBlockBandedMatrixType)
      DO i=1,9
        IF (rank==0) THEN
          DO j=1,6
            IF(tmpreal1((i-1)*9+j) /= 0_SRK) THEN
              CALL testBandedMatrix%set(i,j,tmpreal1((i-1)*9+j))
              CALL testBlockBandedMatrix%set(i,j,tmpreal1((i-1)*9+j))
            ENDIF
          ENDDO
         ELSE
          DO j=7,9
            IF(tmpreal1((i-1)*9+j) /= 0_SRK)THEN
              CALL testBandedMatrix%set(i,j,tmpreal1((i-1)*9+j))
              CALL testBlockBandedMatrix%set(i,j,tmpreal1((i-1)*9+j))
            ENDIF
          ENDDO
        ENDIF
      ENDDO
      CALL testBandedMatrix%assemble()
      CALL testBlockBandedMatrix%assemble()
    ENDSELECT
  ENDSELECT

  tmpreal2=(/-16.0_SRK,                  &
              -1.95943487863577E-15_SRK, &
              16.0_SRK,                  &
              25.1327412287183_SRK,      &
               3.07787310995486E-15_SRK, &
             -25.1327412287183_SRK,      &
             -16.0000000000000_SRK,      &
              -1.95943487863576E-15_SRK, &
              16.0000000000000_SRK/)

  IF (rank == 0) testVector%b = tmpreal2(1:6)
  IF (rank == 1) testVector%b = tmpreal2(7:9)

  tmpreal2=(/0.15182522957531913_SRK,     &
             7.4014868308343768E-018_SRK, &
            -0.15182522957531916_SRK,     &
            -0.26769908169872342_SRK,     &
            -2.9605947323337507E-017_SRK, &
             0.26769908169872342_SRK,     &
             0.15182522957531913_SRK,     &
             7.4014868308343768E-018_SRK, &
            -0.15182522957531916_SRK/)

  IF (rank == 0) refVector%b = tmpreal2(1:6)
  IF (rank == 1) refVector%b = tmpreal2(7:9)

ENDSUBROUTINE setupRSORTest
!
!-------------------------------------------------------------------------------
SUBROUTINE testRSOR_PreCondType()
  CLASS(DistributedSOR_PreCondType),ALLOCATABLE :: testSORP
  INTEGER(SIK)::i,j,k
  REAL(SRK)::trv1(9*9),trv2(3*3*3)
  REAL(SRK)::refLpU(9,9),refLU(3,3,3)

  ALLOCATE(DistributedRSOR_PreCondType :: testSORP)
  !data for checking that the setup is correct
  trv1=(/0_SRK,  0_SRK,  0_SRK, 16_SRK,  0_SRK,  0_SRK,  0_SRK,  0_SRK,  0_SRK,&
         0_SRK,  0_SRK,  0_SRK,  0_SRK, 16_SRK,  0_SRK,  0_SRK,  0_SRK,  0_SRK,&
         0_SRK,  0_SRK,  0_SRK,  0_SRK,  0_SRK, 16_SRK,  0_SRK,  0_SRK,  0_SRK,&
        16_SRK,  0_SRK,  0_SRK,  0_SRK,  0_SRK,  0_SRK, 16_SRK,  0_SRK,  0_SRK,&
         0_SRK, 16_SRK,  0_SRK,  0_SRK,  0_SRK,  0_SRK,  0_SRK, 16_SRK,  0_SRK,&
         0_SRK,  0_SRK, 16_SRK,  0_SRK,  0_SRK,  0_SRK,  0_SRK,  0_SRK, 16_SRK,&
         0_SRK,  0_SRK,  0_SRK, 16_SRK,  0_SRK,  0_SRK,  0_SRK,  0_SRK,  0_SRK,&
         0_SRK,  0_SRK,  0_SRK,  0_SRK, 16_SRK,  0_SRK,  0_SRK,  0_SRK,  0_SRK,&
         0_SRK,  0_SRK,  0_SRK,  0_SRK,  0_SRK, 16_SRK,  0_SRK,  0_SRK,  0_SRK/)

  DO i=1,9
    DO j=1,9
      refLpU(i,j)=trv1((i-1)*9+j)
    ENDDO
  ENDDO

  trv2=(/-64.0_SRK, -0.25_SRK, -0.0_SRK,                 &
          16.0_SRK,-60.00_SRK, -0.26666666666666666_SRK, &
           0.0_SRK, 16.00_SRK,-59.733333333333334_SRK,   &
         -64.0_SRK, -0.25_SRK, -0.0_SRK,                 &
          16.0_SRK,-60.00_SRK, -0.26666666666666666_SRK, &
           0.0_SRK, 16.00_SRK,-59.733333333333334_SRK,   &
         -64.0_SRK, -0.25_SRK, -0.0_SRK,                 &
          16.0_SRK,-60.00_SRK, -0.26666666666666666_SRK, &
           0.0_SRK, 16.00_SRK,-59.733333333333334_SRK/)

  DO k=1,3
    DO i=1,3
      DO j=1,3
        refLU(j,i,k)=trv2((i-1)*3+j+(k-1)*9)
      ENDDO
    ENDDO
  ENDDO

  !check if it works for banded matrices
  COMPONENT_TEST('RSOR_PreCondType, BandedMatrixType')
  IF(testBandedMatrix%isInit .AND. testVector%isInit) THEN
    ! Check %init
    CALL testSORP%init(testBandedMatrix,PListRSOR)
    ASSERT(testSORP%isInit,'BandedMatrixType RSOR%isInit')
    ASSERT(ASSOCIATED(testSORP%A),'BandedMatrixType ASSOCIATED(RSOR%LU%A)')
    ASSERT(testSORP%LpU%isInit,'BandedMatrixType RSOR%LpU%isInit')

    ! Check %setup
    CALL testSORP%setup()
    DO k=testSORP%blockOffset+1,testSORP%blockOffset+testSORP%nLocalBlocks
      SELECT TYPE(LU => testSORP%LU(k-testSORP%blockOffset))
      TYPE IS(DenseSquareMatrixType)
        DO i=1,testSORP%blockSize
          DO j=1,testSORP%blockSize
            ASSERT(LU%a(i,j) .APPROXEQA. refLU(i,j,k),'RSOR%LU(k)%a Correct')
          ENDDO
        ENDDO
        FINFO() 'Result:',LU%a,'Solution:',refLU(:,:,k)
      ENDSELECT
    ENDDO

    ! Check %apply
    workVector%b = testVector%b
    CALL testSORP%apply(workVector)
    ASSERT(ALL(workVector%b .APPROXEQR. refVector%b),'BandedMatrixType RSOR%apply(vector)')
    FINFO() 'Result:',workVector%b,'Solution:',refVector%b

    ! Check %clear
    CALL testSORP%clear()
    ASSERT(.NOT.(testSORP%isInit),'BandedMatrixType .NOT.(RSOR%SOR%isInit)')
    ASSERT(.NOT.(ASSOCIATED(testSORP%A)),'BandedMatrixType .NOT.(ASSOCIATED(RSOR%SOR%A))')
    ASSERT(.NOT.(ASSOCIATED(testSORP%LpU)),'BandedMatrixType .NOT.(ASSOCIATED(RSOR%SOR%LpU))')
  ELSE
    ASSERT(testBandedMatrix%isInit,'TestBandedMatrix Initialization')
    ASSERT(testVector%isInit,'TestVector Initialization')
  ENDIF

  COMPONENT_TEST('RSOR_PreCondType, BlockBandedMatrixType')
  IF(testBlockBandedMatrix%isInit .AND. testVector%isInit) THEN
    ! Check %init
    CALL testSORP%init(testBlockBandedMatrix,PListRSOR)
    ASSERT(testSORP%isInit,'BlockBandedMatrixType RSOR%isInit')
    ASSERT(ASSOCIATED(testSORP%A),'BlockBandedMatrixType ASSOCIATED(RSOR%LU%A)')
    ASSERT(testSORP%LpU%isInit,'BlockBandedMatrixType RSOR%LpU%isInit')

    ! Check %setup
    CALL testSORP%setup()
    DO k=testSORP%blockOffset+1,testSORP%blockOffset+testSORP%nLocalBlocks
      SELECT TYPE(LU => testSORP%LU(k-testSORP%blockOffset))
      TYPE IS(DenseSquareMatrixType)
        DO i=1,LU%n
          DO j=1,LU%n
            ASSERT(LU%a(i,j) .APPROXEQA. refLU(i,j,k),'RSOR%LU(k)%a Correct')
          ENDDO
        ENDDO
        FINFO() 'Result:',LU%a,'Solution:',refLU(:,:,k)
      ENDSELECT
    ENDDO

    ! Check %apply
    workVector%b = testVector%b
    CALL testSORP%apply(workVector)
    ASSERT(ALL(workVector%b .APPROXEQR. refVector%b),'BlockBandedMatrixType RSOR%apply(vector)')
    FINFO() 'Result:',workVector%b,'Solution:',refVector%b

    ! Check %clear
    CALL testSORP%clear()
    ASSERT(.NOT.(testSORP%isInit),'BlockBandedMatrixType .NOT.(RSOR%SOR%isInit)')
    ASSERT(.NOT.(ASSOCIATED(testSORP%A)),'BlockBandedMatrixType .NOT.(ASSOCIATED(RSOR%SOR%A))')
    ASSERT(.NOT.(ASSOCIATED(testSORP%LpU)),'BlockBandedMatrixType .NOT.(ASSOCIATED(RSOR%SOR%LpU))')
  ELSE
    ASSERT(testBandedMatrix%isInit,'TestBlockBandedMatrix Initialization')
    ASSERT(testVector%isInit,'TestVector Initialization')
  ENDIF

  DEALLOCATE(testSORP)

ENDSUBROUTINE testRSOR_PreCondtype
!
!-------------------------------------------------------------------------------
SUBROUTINE clearTest()

  CALL PListMat%clear()
  CALL PListVec%clear()
  CALL MatrixTypes_Clear_ValidParams()
  IF(ALLOCATED(testVec_1g)) CALL testVec_1g%clear()
  IF(ALLOCATED(testVec_mg)) CALL testVec_mg%clear()
  IF(ALLOCATED(testBandedMatrix)) CALL testBandedMatrix%clear()
  IF(ALLOCATED(testBlockBandedMatrix)) CALL testBlockBandedMatrix%clear()
  CALL testVector%clear()

  IF(ALLOCATED(testVec_1g)) DEALLOCATE(testVec_1g)
  IF(ALLOCATED(testVec_mg)) DEALLOCATE(testVec_mg)
  IF(ALLOCATED(testBandedMatrix)) DEALLOCATE(testBandedMatrix)
  IF(ALLOCATED(testBlockBandedMatrix)) DEALLOCATE(testBlockBandedMatrix)

ENDSUBROUTINE clearTest
#endif
ENDPROGRAM testRSORPreconParallel
