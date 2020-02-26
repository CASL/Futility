!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
! test with ./unit_tests/testRSORprecon/Futility_testRSORprecon.exe
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
PROGRAM testRSORPrecon
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

TYPE(ExceptionHandlerType),TARGET :: e
TYPE(ParamType) :: PListMat,PListVec,PListRSOR
CLASS(MatrixType),ALLOCATABLE :: testSparseMatrix,testDenseMatrix,testMatrix
CLASS(MatrixType),ALLOCATABLE :: testBandedMatrix
CLASS(VectorType),ALLOCATABLE :: testVector,testDummy,refVector
CLASS(VectorType),ALLOCATABLE :: testVec_1g,testVec_mg

#ifdef HAVE_MPI
INTEGER :: mpierr
CALL MPI_Init(mpierr)
#endif
!Configure exception handler for test
CALL e%setStopOnError(.FALSE.)
CALL e%setQuietMode(.TRUE.)
CALL eParams%addSurrogate(e)
CALL ePreCondType%addSurrogate(e)

CREATE_TEST('Test RSOR Preconditioner')

CALL setupRSORTest()
REGISTER_SUBTEST('Test RSOR Preconditioner Type',testRSOR_PreCondType)
CALL clearTest()

FINALIZE_TEST()

#ifdef HAVE_MPI
CALL MPI_Finalize(mpierr)
#endif
!
!===============================================================================
CONTAINS
!
!-------------------------------------------------------------------------------
SUBROUTINE setupRSORTest()
  INTEGER(SIK) :: i,j,numnonzero
  REAL(SRK) :: tmpreal1(9*9),tmpreal2(9)

  CALL PListRSOR%add('PCType->numblocks',3)
  CALL PListRSOR%add('PCType->omega',1.0_SRK)
  CALL PListVec%add('VectorType->n',9)
  ALLOCATE(RealVectorType :: testVector)
  ALLOCATE(RealVectorType :: refVector)
  CALL testVector%init(PListVec)
  CALL refVector%init(PListVec)
  CALL PListMat%add('MatrixType->n',9)
  CALL PListMat%add('MatrixType->m',9)
  CALL PListMat%add('MatrixType->isSym',.FALSE.)
  ALLOCATE(DenseSquareMatrixType :: testDenseMatrix)
  CALL testDenseMatrix%init(PListMat)

  tmpreal1=(/-64_SRK, 16_SRK,  0_SRK, 16_SRK,  0_SRK,  0_SRK,  0_SRK,  0_SRK,  0_SRK,&
              16_SRK,-64_SRK, 16_SRK,  0_SRK, 16_SRK,  0_SRK,  0_SRK,  0_SRK,  0_SRK,&
               0_SRK, 16_SRK,-64_SRK,  0_SRK,  0_SRK, 16_SRK,  0_SRK,  0_SRK,  0_SRK,&
              16_SRK,  0_SRK,  0_SRK,-64_SRK, 16_SRK,  0_SRK, 16_SRK,  0_SRK,  0_SRK,&
               0_SRK, 16_SRK,  0_SRK, 16_SRK,-64_SRK, 16_SRK,  0_SRK, 16_SRK,  0_SRK,&
               0_SRK,  0_SRK, 16_SRK,  0_SRK, 16_SRK,-64_SRK,  0_SRK,  0_SRK, 16_SRK,&
               0_SRK,  0_SRK,  0_SRK, 16_SRK,  0_SRK,  0_SRK,-64_SRK, 16_SRK,  0_SRK,&
               0_SRK,  0_SRK,  0_SRK,  0_SRK, 16_SRK,  0_SRK, 16_SRK,-64_SRK, 16_SRK,&
               0_SRK,  0_SRK,  0_SRK,  0_SRK,  0_SRK, 16_SRK,  0_SRK, 16_SRK,-64_SRK/)

  !setup the dense matrixk and count the number of nonzeroentries
  numnonzero=0
  DO i=1,9
    DO j=1,9
      CALL testDenseMatrix%set(i,j,tmpreal1((i-1)*9+j))
      IF(tmpreal1((i-1)*9+j) /= 0_SRK) numnonzero=numnonzero+1
    ENDDO
  ENDDO

  !setup the sparse version of the matrix
  CALL PListMat%add('MatrixType->nnz',numnonzero)
  ALLOCATE(SparseMatrixType :: testSparseMatrix)
  CALL testSparseMatrix%init(PListMat)
  SELECT TYPE(testSparseMatrix); TYPE IS(SparseMatrixType)
    DO i=1,9
      DO j=1,9
        IF(tmpreal1((i-1)*9+j) /= 0_SRK)THEN
          CALL testSparseMatrix%setShape(i,j,tmpreal1((i-1)*9+j))
        ENDIF
      ENDDO
    ENDDO
  ENDSELECT

  !setup the banded version of the matrix
  ALLOCATE(BandedMatrixType :: testBandedMatrix)
  CALL testBandedMatrix%init(PListMat)
  SELECT TYPE(testBandedMatrix); TYPE IS(BandedMatrixType)
    DO i=1,9
      DO j=1,9
        IF(tmpreal1((i-1)*9+j) /= 0_SRK) THEN
          CALL testBandedMatrix%set(i,j,tmpreal1((i-1)*9+j))
        ENDIF
      ENDDO
    ENDDO
    CALL testBandedMatrix%assemble()
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
  SELECT TYPE(testVector); TYPE IS(RealVectorType)
    testVector%b(:)=tmpreal2(:)
  ENDSELECT

  tmpreal2=(/0.15182522957531913_SRK,     &
             7.4014868308343768E-018_SRK, &
            -0.15182522957531916_SRK,     &
            -0.26769908169872342_SRK,     &
            -2.9605947323337507E-017_SRK, &
             0.26769908169872342_SRK,     &
             0.15182522957531913_SRK,     &
             7.4014868308343768E-018_SRK, &
            -0.15182522957531916_SRK/)
  SELECT TYPE(refVector); TYPE IS(RealVectorType)
    refVector%b(:) = tmpreal2(:)
  ENDSELECT

ENDSUBROUTINE setupRSORTest
!
!-------------------------------------------------------------------------------
SUBROUTINE testRSOR_PreCondType()
  CLASS(SOR_PreCondType),ALLOCATABLE :: testSOR
  INTEGER(SIK) :: i,j,k
  REAL(SRK) :: trv1(9*9),trv2(3*3*3)
  REAL(SRK) :: refLpU(9,9),refLU(3,3,3)
  REAL(SRK) :: vecsave(9)

  ALLOCATE(RSOR_PreCondType :: testSOR)
  !data for checking that the setup is correct
  trv1=(/ 0_SRK,  0_SRK,  0_SRK, 16_SRK,  0_SRK,  0_SRK,  0_SRK,  0_SRK,  0_SRK,&
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

  trv2=(/-64.0_SRK, -0.25_SRK, -0.00_SRK,                &
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

  SELECT TYPE(tv => testVector); TYPE IS(RealVectorType)
    vecsave=tv%b
  ENDSELECT

  !check if it works for dense matrices
  COMPONENT_TEST('RSOR_PreCondType, DenseMatrixType')
  IF(testDenseMatrix%isInit .AND. testVector%isInit) THEN
    ! Check %init
    CALL testSOR%init(testDenseMatrix,PListRSOR)

    ASSERT(testSOR%isInit,'DenseSquareMatrixType RSOR%isInit')
    ASSERT(ASSOCIATED(testSOR%A),'DenseSquareMatrixType ASSOCIATED(RSOR%LU%A)')
    ASSERT(testSOR%LpU%isInit,'DenseSquareMatrixType RSOR%LpU%isInit')
    SELECT TYPE(LpU => testSOR%LpU); TYPE IS(DenseSquareMatrixType)
      SELECT TYPE(A => testSOR%A); TYPE IS(DenseSquareMatrixType)
        ASSERT(ALL(LpU%a .APPROXEQA. A%a),'DenseSquareMatrixType RSOR%LpU%a')
      ENDSELECT
    CLASS DEFAULT
      ASSERT(.FALSE.,'DenseSquareMatrixType RSOR%LpU TYPE IS(DenseSquareMatrixType)')
    ENDSELECT

    ! Check %setup
    CALL testSOR%setup()
    SELECT TYPE(LpU => testSOR%LpU); TYPE IS(DenseSquareMatrixType)
      ASSERT(ALL(LpU%a .APPROXEQA. refLpU),'RSOR%LpU%a Correct')
    ENDSELECT
    DO k=1,3
      SELECT TYPE(LU => testSOR%LU(k)); TYPE IS(DenseSquareMatrixType)
        DO i=1,3
          DO j=1,3
            ASSERT(LU%a(i,j) .APPROXEQA. refLU(i,j,k),'RSOR%LU(k)%a Correct')
          ENDDO
        ENDDO
        FINFO() 'Result:',LU%a,'Solution:',refLU(:,:,k)
      ENDSELECT
    ENDDO

    ! Check %apply
    CALL testSOR%apply(testVector)
    SELECT TYPE(tv => testVector); TYPE IS(RealVectorType)
      SELECT TYPE(rv => refVector); TYPE IS(RealVectorType)
        ASSERT(ALL(tv%b .APPROXEQA. rv%b),'DenseSquareMatrixType RSOR%apply(vector)')
        FINFO() 'Result:',tv%b,'Solution:',rv%b
      ENDSELECT
    ENDSELECT

    ! Check %clear
    CALL testSOR%clear()
    ASSERT(.NOT.(testSOR%isInit),'DenseSquareMatrixType .NOT.(RSOR%SOR%isInit)')
    ASSERT(.NOT.(ASSOCIATED(testSOR%A)),'DenseSquareMatrixType .NOT.(ASSOCIATED(RSOR%SOR%A))')
    ASSERT(.NOT.(ALLOCATED(testSOR%LpU)),'DenseSquareMatrixType .NOT.(ASSOCIATED(RSOR%SOR%LpU))')
  ELSE
    ASSERT(testDenseMatrix%isInit,'TestDenseMatrix Initialization')
    ASSERT(testVector%isInit,'TestVector Initialization')
  ENDIF

  SELECT TYPE(tv => testVector); TYPE IS(RealVectorType)
    tv%b=vecsave
  ENDSELECT

  !check if it works for sparse matrices
  COMPONENT_TEST('RSOR_PreCondType, SparseMatrixType')
  IF(testSparseMatrix%isInit .AND. testVector%isInit) THEN
    ! Check %init
    CALL testSOR%init(testSparseMatrix,PListRSOR)
    ASSERT(testSOR%isInit,'SparseMatrixType RSOR%isInit')
    ASSERT(ASSOCIATED(testSOR%A),'SparseMatrixType ASSOCIATED(RSOR%LU%A)')
    ASSERT(testSOR%LpU%isInit,'SparseMatrixType RSOR%LpU%isInit')
    SELECT TYPE(LpU => testSOR%LpU); TYPE IS(SparseMatrixType)
      SELECT TYPE(A => testSOR%A); TYPE IS(SparseMatrixType)
        ASSERT(ALL(LpU%a .APPROXEQA. A%a),'SparseMatrixType RSOR%LpU%a')
      ENDSELECT
    CLASS DEFAULT
      ASSERT(.FALSE.,'SparseMatrixType RSOR%LpU TYPE IS(SparseMatrixType)')
    ENDSELECT

    ! Check %setup
    CALL testSOR%setup()
    !SELECT TYPE(LpU => testSOR%LpU); TYPE IS(SparseMatrixType)
    !  ASSERT(ALL(LpU%a .APPROXEQA. refLpU),'RSOR%LpU%a Correct')
    !ENDSELECT
    DO k=1,3
      SELECT TYPE(LU => testSOR%LU(k)); TYPE IS(DenseSquareMatrixType)
        DO i=1,3
          DO j=1,3
            ASSERT(LU%a(i,j) .APPROXEQA. refLU(i,j,k),'RSOR%LU(k)%a Correct')
          ENDDO
        ENDDO
        FINFO() 'Result:',LU%a,'Solution:',refLU(:,:,k)
      ENDSELECT
    ENDDO


    ! Check %apply
    CALL testSOR%apply(testVector)
    SELECT TYPE(tv => testVector); TYPE IS(RealVectorType)
      SELECT TYPE(rv => refVector); TYPE IS(RealVectorType)
        ASSERT(ALL(tv%b .APPROXEQA. rv%b),'SparseMatrixType RSOR%apply(vector)')
        FINFO() 'Result:',tv%b,'Solution:',rv%b
      ENDSELECT
    ENDSELECT

    ! Check %clear
    CALL testSOR%clear()
    ASSERT(.NOT.(testSOR%isInit),'SparseMatrixType .NOT.(RSOR%SOR%isInit)')
    ASSERT(.NOT.(ASSOCIATED(testSOR%A)),'SparseMatrixType .NOT.(ASSOCIATED(RSOR%SOR%A))')
    ASSERT(.NOT.(ALLOCATED(testSOR%LpU)),'SparseMatrixType .NOT.(ASSOCIATED(RSOR%SOR%LpU))')
  ELSE
    ASSERT(testDenseMatrix%isInit,'TestSparseMatrix Initialization')
    ASSERT(testVector%isInit,'TestVector Initialization')
  ENDIF

  SELECT TYPE(tv => testVector); TYPE IS(RealVectorType)
    tv%b=vecsave
  ENDSELECT

  !check if it works for banded matrices
  COMPONENT_TEST('RSOR_PreCondType, BandedMatrixType')
  IF(testBandedMatrix%isInit .AND. testVector%isInit) THEN
    ! Check %init
    CALL testSOR%init(testBandedMatrix,PListRSOR)
    ASSERT(testSOR%isInit,'BandedMatrixType RSOR%isInit')
    ASSERT(ASSOCIATED(testSOR%A),'BandedMatrixType ASSOCIATED(RSOR%LU%A)')
    ASSERT(testSOR%LpU%isInit,'BandedMatrixType RSOR%LpU%isInit')

    ! Check %setup
    CALL testSOR%setup()
    DO k=1,3
      SELECT TYPE(LU => testSOR%LU(k)); TYPE IS(DenseSquareMatrixType)
        DO i=1,3
          DO j=1,3
            ASSERT(LU%a(i,j) .APPROXEQA. refLU(i,j,k),'RSOR%LU(k)%a Correct')
          ENDDO
        ENDDO
        FINFO() 'Result:',LU%a,'Solution:',refLU(:,:,k)
      ENDSELECT
    ENDDO

    ! Check %apply
    CALL testSOR%apply(testVector)
    SELECT TYPE(tv => testVector); TYPE IS(RealVectorType)
      SELECT TYPE(rv => refVector); TYPE IS(RealVectorType)
        ASSERT(ALL(tv%b .APPROXEQA. rv%b),'BandedMatrixType RSOR%apply(vector)')
        FINFO() 'Result:',tv%b,'Solution:',rv%b
      ENDSELECT
    ENDSELECT

    ! Check %clear
    CALL testSOR%clear()
    ASSERT(.NOT.(testSOR%isInit),'BandedMatrixType .NOT.(RSOR%SOR%isInit)')
    ASSERT(.NOT.(ASSOCIATED(testSOR%A)),'BandedMatrixType .NOT.(ASSOCIATED(RSOR%SOR%A))')
    ASSERT(.NOT.(ALLOCATED(testSOR%LpU)),'BandedMatrixType .NOT.(ASSOCIATED(RSOR%SOR%LpU))')
  ELSE
    ASSERT(testDenseMatrix%isInit,'TestBandedMatrix Initialization')
    ASSERT(testVector%isInit,'TestVector Initialization')
  ENDIF

  DEALLOCATE(testSOR)
ENDSUBROUTINE testRSOR_PreCondtype
!
!-------------------------------------------------------------------------------
SUBROUTINE clearTest()

  CALL PListMat%clear()
  CALL PListVec%clear()
  CALL MatrixTypes_Clear_ValidParams()
  IF(ALLOCATED(testSparseMatrix)) CALL testSparseMatrix%clear()
  IF(ALLOCATED(testVec_1g)) CALL testVec_1g%clear()
  IF(ALLOCATED(testVec_mg)) CALL testVec_mg%clear()
  IF(ALLOCATED(testDenseMatrix)) CALL testDenseMatrix%clear()
  IF(ALLOCATED(testMatrix)) CALL testMatrix%clear()
  IF(ALLOCATED(testVector)) CALL testVector%clear()
  IF(ALLOCATED(testDummy)) CALL testDummy%clear()

  IF(ALLOCATED(testSparseMatrix)) DEALLOCATE(testSparseMatrix)
  IF(ALLOCATED(testVec_1g)) DEALLOCATE(testVec_1g)
  IF(ALLOCATED(testVec_mg)) DEALLOCATE(testVec_mg)
  IF(ALLOCATED(testDenseMatrix)) DEALLOCATE(testDenseMatrix)
  IF(ALLOCATED(testMatrix)) DEALLOCATE(testMatrix)
  IF(ALLOCATED(testVector)) DEALLOCATE(testVector)
  IF(ALLOCATED(testDummy)) DEALLOCATE(testDummy)
ENDSUBROUTINE clearTest
!
ENDPROGRAM testRSORPrecon
