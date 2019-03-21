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

  TYPE(ExceptionHandlerType),TARGET :: e
  TYPE(ParamType) :: PListMat,PListVec,PListRSOR
  CLASS(MatrixType),ALLOCATABLE :: testBandedMatrix
  CLASS(VectorType),ALLOCATABLE :: testVector,testDummy,refVector
  CLASS(VectorType),ALLOCATABLE :: testVec_1g,testVec_mg
  INTEGER(SIK) :: nerrors1,nerrors2

#ifdef HAVE_MPI
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
#endif
!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
    SUBROUTINE setupRSORTest()
        INTEGER(SIK)::ioerr,i,j,numnonzero,rank,nproc
        REAL(SRK)::tmpreal1(9*9),tmpreal2(9),tempreal
#ifdef HAVE_MPI

        CALL PListRSOR%add('PCType->numblocks',3_SIK)
        CALL PListRSOR%add('PCType->omega',1.0_SRK)
        CALL PListRSOR%add('PCType->comm',MPI_COMM_WORLD)
        CALL PListVec%add('VectorType->n',9_SIK)
        ALLOCATE(RealVectorType :: testVector)
        ALLOCATE(RealVectorType :: refVector)
        CALL testVector%init(PListVec)
        CALL refVector%init(PListVec)
        CALL PListMat%add('MatrixType->n',9_SIK)
        CALL PListMat%add('MatrixType->m',9_SIK)
        CALL PListMat%add('MatrixType->isSym',.FALSE.)
        CALL PListMat%add('MatrixType->comm',MPI_COMM_WORLD)


        CALL MPI_Comm_rank(MPI_COMM_WORLD,rank,mpierr)
        CALL MPI_Comm_size(MPI_COMM_WORLD,nproc,mpierr)

        ASSERT(nproc==2, 'nproc valid')

        tmpreal1=(/-64_SRK, 16_SRK, 0_SRK, 16_SRK, 0_SRK, 0_SRK, 0_SRK, 0_SRK, 0_SRK,&
                16_SRK, -64_SRK, 16_SRK, 0_SRK, 16_SRK, 0_SRK, 0_SRK, 0_SRK, 0_SRK,&
                0_SRK, 16_SRK, -64_SRK, 0_SRK, 0_SRK, 16_SRK, 0_SRK, 0_SRK, 0_SRK,&
                16_SRK, 0_SRK, 0_SRK, -64_SRK, 16_SRK, 0_SRK, 16_SRK, 0_SRK, 0_SRK,&
                0_SRK, 16_SRK, 0_SRK, 16_SRK, -64_SRK, 16_SRK, 0_SRK, 16_SRK, 0_SRK,&
                0_SRK, 0_SRK, 16_SRK, 0_SRK, 16_SRK, -64_SRK, 0_SRK, 0_SRK, 16_SRK,&
                0_SRK, 0_SRK, 0_SRK, 16_SRK, 0_SRK, 0_SRK, -64_SRK, 16_SRK, 0_SRK,&
                0_SRK, 0_SRK, 0_SRK, 0_SRK, 16_SRK, 0_SRK, 16_SRK, -64_SRK, 16_SRK,&
                0_SRK, 0_SRK, 0_SRK, 0_SRK, 0_SRK, 16_SRK, 0_SRK, 16_SRK, -64_SRK/)

        !setup the dense matrixk and count the number of nonzeroentries
        numnonzero=0
        DO i=1,9
            DO j=1,9
                IF(tmpreal1((i-1)*9+j) .NE. 0_SRK)numnonzero=numnonzero+1
            END DO
        END DO

        !setup the sparse version of the matrix
        CALL PListMat%add('MatrixType->nnz',numnonzero)

        !setup the banded version of the matrix
        ALLOCATE(DistributedBandedMatrixType :: testBandedMatrix)
        CALL testBandedMatrix%init(PListMat)
        SELECTTYPE(testBandedMatrix); TYPE IS(DistributedBandedMatrixType)
            DO i=1,9
                DO j=1,9
                    IF(tmpreal1((i-1)*9+j) .NE. 0_SRK)THEN
                        CALL testBandedMatrix%set(i,j,tmpreal1((i-1)*9+j))
                    END IF
                END DO
            END DO
            CALL testBandedMatrix%assemble()
        ENDSELECT

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

        tmpreal2=(/0.15182522957531913_SRK,&
                7.4014868308343768E-018_SRK,&
                -0.15182522957531916_SRK,&
                -0.26769908169872342_SRK,&
                -2.9605947323337507E-017_SRK,&
                0.26769908169872342_SRK,&
                0.15182522957531913_SRK,&
                7.4014868308343768E-018_SRK,&
                -0.15182522957531916_SRK/)

        DO i=1,9
            CALL refVector%set(i,tmpreal2(i))
        END DO
#endif

    ENDSUBROUTINE setupRSORTest
!
!-------------------------------------------------------------------------------
    SUBROUTINE testRSOR_PreCondType()
        CLASS(DistributedSOR_PreCondType),ALLOCATABLE :: testSORP
        INTEGER(SIK)::i,j,k
        REAL(SRK)::tmpreal,trv1(9*9),trv2(3*3*3)
        REAL(SRK)::refLpU(9,9),refLU(3,3,3)
        REAL(SRK)::vecsave(9)
#ifdef HAVE_MPI

        ALLOCATE(DistributedRSOR_PreCondType :: testSORP)

        !data for checking that the setup is correct
        trv1=(/0_SRK,    0_SRK,    0_SRK,    16_SRK,    0_SRK,    0_SRK,    0_SRK,    0_SRK,    0_SRK,&
                0_SRK,    0_SRK,    0_SRK,    0_SRK,    16_SRK,    0_SRK,    0_SRK,    0_SRK,    0_SRK,&
                0_SRK,    0_SRK,    0_SRK,    0_SRK,    0_SRK,    16_SRK,    0_SRK,    0_SRK,    0_SRK,&
                16_SRK,    0_SRK,    0_SRK,    0_SRK,    0_SRK,    0_SRK,    16_SRK,    0_SRK,    0_SRK,&
                0_SRK,    16_SRK,    0_SRK,    0_SRK,    0_SRK,    0_SRK,    0_SRK,    16_SRK,    0_SRK,&
                0_SRK,    0_SRK,    16_SRK,    0_SRK,    0_SRK,    0_SRK,    0_SRK,    0_SRK,    16_SRK,&
                0_SRK,    0_SRK,    0_SRK,    16_SRK,    0_SRK,    0_SRK,    0_SRK,    0_SRK,    0_SRK,&
                0_SRK,    0_SRK,    0_SRK,    0_SRK,    16_SRK,    0_SRK,    0_SRK,    0_SRK,    0_SRK,&
                0_SRK,    0_SRK,    0_SRK,    0_SRK,    0_SRK,    16_SRK,    0_SRK,    0_SRK,    0_SRK/)

        DO i=1,9
            DO j=1,9
                refLpU(i,j)=trv1((i-1)*9+j)
            END DO
        END DO

        trv2=(/-64.0_SRK, -0.250_SRK,  -0.00_SRK,&
            16.0_SRK,  -60.0_SRK, -0.26666666666666666_SRK,&
            0.00_SRK,   16.0_SRK,  -59.733333333333334_SRK,&
            -64.0_SRK, -0.250_SRK,  -0.00_SRK,&
            16.0_SRK,  -60.0_SRK, -0.26666666666666666_SRK,&
            0.00_SRK,   16.0_SRK,  -59.733333333333334_SRK,&
            -64.0_SRK, -0.250_SRK,  -0.00_SRK,&
            16.0_SRK,  -60.0_SRK, -0.26666666666666666_SRK,&
            0.00_SRK,   16.0_SRK,  -59.733333333333334_SRK/)

        DO k=1,3
            DO i=1,3
                DO j=1,3
                    refLU(j,i,k)=trv2((i-1)*3+j+(k-1)*9)
                END DO
            END DO
        END DO

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
            DO k=testSORP%myFirstBlock,testSORP%myFirstBlock+testSORP%myNumBlocks-1
                SELECTTYPE(LU => testSORP%LU(k-testSORP%myFirstBlock+1)); TYPE IS(DenseSquareMatrixType)
                    ASSERT(ALL(LU%a .APPROXEQA. refLU(:,:,k)),'RSOR%LU(k)%a Correct')
                    FINFO() 'Result:',LU%a,'Solution:',refLU(:,:,k)
                ENDSELECT
            END DO

            ! Check %apply
            CALL testSORP%apply(testVector)
            SELECTTYPE(tv => testVector); TYPE IS(RealVectorType)
                SELECTTYPE(rv => refVector); TYPE IS(RealVectorType)
                    ASSERT(ALL(tv%b .APPROXEQA. rv%b),'BandedMatrixType RSOR%apply(vector)')
                    FINFO() 'Result:',tv%b,'Solution:',rv%b
                ENDSELECT
            ENDSELECT

            ! Check %clear
            CALL testSORP%clear()
            ASSERT(.NOT.(testSORP%isInit),'BandedMatrixType .NOT.(RSOR%SOR%isInit)')
            ASSERT(.NOT.(ASSOCIATED(testSORP%A)),'BandedMatrixType .NOT.(ASSOCIATED(RSOR%SOR%A))')
            ASSERT(.NOT.(ALLOCATED(testSORP%LpU)),'BandedMatrixType .NOT.(ASSOCIATED(RSOR%SOR%LpU))')

        ELSE
            ASSERT(testBandedMatrix%isInit,'TestBandedMatrix Initialization')
            ASSERT(testVector%isInit,'TestVector Initialization')
        ENDIF

        DEALLOCATE(testSORP)
#endif

    ENDSUBROUTINE testRSOR_PreCondtype
!
!-------------------------------------------------------------------------------
    SUBROUTINE clearTest()
#ifdef HAVE_MPI

      CALL PListMat%clear()
      CALL PListVec%clear()
      CALL MatrixTypes_Clear_ValidParams()
      IF(ALLOCATED(testVec_1g)) CALL testVec_1g%clear()
      IF(ALLOCATED(testVec_mg)) CALL testVec_mg%clear()
      IF(ALLOCATED(testVector)) CALL testVector%clear()
      IF(ALLOCATED(testDummy)) CALL testDummy%clear()

      IF(ALLOCATED(testVec_1g)) DEALLOCATE(testVec_1g)
      IF(ALLOCATED(testVec_mg)) DEALLOCATE(testVec_mg)
      IF(ALLOCATED(testVector)) DEALLOCATE(testVector)
      IF(ALLOCATED(testDummy)) DEALLOCATE(testDummy)
#endif

    ENDSUBROUTINE clearTest
!
ENDPROGRAM testRSORPreconParallel
