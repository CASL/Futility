!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
! test with ./unit_tests/testRSORprecon/Futility_testRSORprecon.exe
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
PROGRAM testRSORprecon
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
  CLASS(VectorType),ALLOCATABLE :: testVector,testDummy,refVector
  CLASS(VectorType),ALLOCATABLE :: testVec_1g,testVec_mg
  INTEGER(SIK) :: nerrors1,nerrors2

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
        INTEGER(SIK)::ioerr,i,j
        REAL(SRK)::tmpreal1(9*9),tmpreal2(9)
        
        CALL PListRSOR%add('PCType->numblocks',3_SIK)
        CALL PListVec%add('VectorType->n',9_SIK)
        ALLOCATE(RealVectorType :: testVector)
        ALLOCATE(RealVectorType :: refVector)
        CALL testVector%init(PListVec)
        CALL refVector%init(PListVec)
        CALL PListMat%add('MatrixType->n',9_SIK)
        CALL PListMat%add('MatrixType->isSym',.FALSE.)
        ALLOCATE(DenseSquareMatrixType :: testDenseMatrix)
        CALL testDenseMatrix%init(PListMat)
        
        tmpreal1=(/-64,    16,    0,    16,    0,    0,    0,    0,    0,&
                    16,    -64,    16,    0,    16,    0,    0,    0,    0,&
                    0,    16,    -64,    0,    0,    16,    0,    0,    0,&
                    16,    0,    0,    -64,    16,    0,    16,    0,    0,&
                    0,    16,    0,    16,    -64,    16,    0,    16,    0,&
                    0,    0,    16,    0,    16,    -64,    0,    0,    16,&
                    0,    0,    0,    16,    0,    0,    -64, 16,    0,&
                    0,    0,    0,    0,    16,    0,    16,    -64, 16,&
                    0,    0,    0,    0,    0,    16,    0,    16,    -64/)
        
        DO i=1,9
            DO j=1,9
                CALL testDenseMatrix%set(i,j,tmpreal1((i-1)*9+j))
            END DO
        END DO
        
        tmpreal2=(/-16.0_SRK,&
                -1.95943487863577E-15_SRK,&
                16.0_SRK,&
                25.1327412287183_SRK,&
                3.07787310995486E-15_SRK,&
                -25.1327412287183_SRK,&
                -16.0000000000000_SRK,&
                -1.95943487863576E-15_SRK,&
                16.0000000000000_SRK/)
        
        DO i=1,9
            CALL testVector%set(i,tmpreal2(i))
        END DO
        
        tmpreal2=(/0.134981101936174,&
                    1.48029736616688e-17,&
                    -0.134981101936174,&
                    -0.246252472291998,&
                    -3.70074341541719e-17,&
                    0.246252472291998,&
                    0.134981101936174,&
                    7.40148683083438e-18,&
                    -0.134981101936174/)
        
        DO i=1,9
            CALL refVector%set(i,tmpreal2(i))
        END DO

    ENDSUBROUTINE setupRSORTest
!
!-------------------------------------------------------------------------------
    SUBROUTINE testRSOR_PreCondType()
        CLASS(SOR_PreCondType),ALLOCATABLE :: testSOR
        INTEGER(SIK)::i,j,k
        REAL(SRK)::tmpreal


        ALLOCATE(RSOR_PreCondType :: testSOR)
        
        !check to make sure that bad/redundant info is noticed
        CALL testSOR%init(testDenseMatrix,PListRSOR)
        nerrors1=e%getCounter(EXCEPTION_ERROR)
        CALL testSOR%init(testDenseMatrix,PListRSOR)
        nerrors2=e%getCounter(EXCEPTION_ERROR)
        ASSERT(nerrors2 == nerrors1+1,'init_SOR_Preconditioner PC%isInit check')
        FINFO() 'Result:',nerrors2,'Solution:',nerrors1+1
        CALL testSOR%clear()

        nerrors1=e%getCounter(EXCEPTION_ERROR)
        CALL testSOR%init(testMatrix,PListRSOR)
        nerrors2=e%getCounter(EXCEPTION_ERROR)
        ASSERT(nerrors2 == nerrors1+1,'init_SOR_Preconditioner ALLOCATED(PC%A) check')
        FINFO() 'Result:',nerrors2,'Solution:',nerrors1+1

        nerrors1=e%getCounter(EXCEPTION_ERROR)
        ALLOCATE(DenseSquareMatrixType :: testMatrix) !Just a dummy matrix for error check tests
        CALL testSOR%init(testMatrix,PListRSOR)
        nerrors2=e%getCounter(EXCEPTION_ERROR)
        ASSERT(nerrors2 == nerrors1+1,'init_SOR_Preconditioner PC%A%isInit check')
        FINFO() 'Result:',nerrors2,'Solution:',nerrors1+1

        CALL testSOR%clear()
        nerrors1=e%getCounter(EXCEPTION_ERROR)
        CALL testSOR%setup()
        nerrors2=e%getCounter(EXCEPTION_ERROR)
        ASSERT(nerrors2 == nerrors1+1,'setup_SOR_Preconditioner PC%isInit check')
        FINFO() 'Result:',nerrors2,'Solution:',nerrors1+1

        CALL testSOR%init(testDenseMatrix,PListRSOR)
        SELECTTYPE(LpU => testSOR%LpU); TYPE IS(DenseSquareMatrixType)
            CALL LpU%clear()
            nerrors1=e%getCounter(EXCEPTION_ERROR)
            CALL testSOR%setup()
            nerrors2=e%getCounter(EXCEPTION_ERROR)
            ASSERT(nerrors2 == nerrors1+1,'setup_SOR_Preconditioner PC%LpU%isInit check')
            FINFO() 'Result:',nerrors2,'Solution:',nerrors1+1
        ENDSELECT

        CALL testSOR%clear()

        ALLOCATE(RealVectorType :: testDummy)
        nerrors1=e%getCounter(EXCEPTION_ERROR)
        CALL testSOR%apply(testDummy)
        nerrors2=e%getCounter(EXCEPTION_ERROR)
        ASSERT(nerrors2 == nerrors1+1,'apply_SOR_Preconditioner v%isInit check')
        FINFO() 'Result:',nerrors2,'Solution:',nerrors1+1
        
        !check if it works
        COMPONENT_TEST('RSOR_PreCondType, DenseMatrixType')
        IF(testDenseMatrix%isInit .AND. testVector%isInit) THEN
        
            ! Check %init
            CALL testSOR%init(testDenseMatrix,PListRSOR)
            ASSERT(testSOR%isInit,'DenseSquareMatrixType RSOR%isInit')
            ASSERT(ASSOCIATED(testSOR%A),'DenseSquareMatrixType ASSOCIATED(RSOR%LU%A)')
            ASSERT(testSOR%LpU%isInit,'DenseSquareMatrixType RSOR%LpU%isInit')
            SELECTTYPE(LpU => testSOR%LpU); TYPE IS(DenseSquareMatrixType)
                SELECTTYPE(A => testSOR%A); TYPE IS(DenseSquareMatrixType)
                    ASSERT(ALL(LpU%a .APPROXEQA. A%a),'DenseSquareMatrixType RSOR%LpU%a')
                ENDSELECT
            CLASS DEFAULT
                ASSERT(.FALSE.,'DenseSquareMatrixType RSOR%LpU TYPE IS(DenseSquareMatrixType)')
            ENDSELECT


            ! Check %setup
            CALL testSOR%setup()
            
            ! Check %apply
            CALL testSOR%apply(testVector)
            
        ELSE
            ASSERT(testDenseMatrix%isInit,'TestDenseMatrix Initialization')
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
ENDPROGRAM testRSORprecon
