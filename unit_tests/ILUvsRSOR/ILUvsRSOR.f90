!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
! test with ./unit_tests/testRSORprecon/Futility_testRSORprecon.exe
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
PROGRAM ILUvsRSOR
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
  USE LinearSolverTypes
  IMPLICIT NONE

#ifdef HAVE_MPI
  INCLUDE 'mpif.h'
#endif

  TYPE(ExceptionHandlerType),TARGET :: e
  TYPE(ParamType) :: PListMat,PListVec,PListRSOR,pList,optListLS
  CLASS(MatrixType),ALLOCATABLE :: testSparseMatrix,testDenseMatrix,testMatrix
  CLASS(VectorType),ALLOCATABLE :: rhsVector,testDummy
  CLASS(VectorType),ALLOCATABLE :: testVec_1g,testVec_mg
  TYPE(MPI_EnvType) :: mpiTestEnv
  INTEGER(SIK) :: nerrors1,nerrors2

#ifdef HAVE_MPI
  INTEGER :: mpierr
  CALL MPI_Init(mpierr)
#endif

  !> set up default parameter list
  CALL optListLS%clear()
  CALL optListLS%add('LinearSolverType->TPLType',NATIVE)
  CALL optListLS%add('LinearSolverType->solverMethod',1_SNK) ! GE or BICGSTAB
  CALL optListLS%add('LinearSolverType->MPI_Comm_ID',PE_COMM_SELF)
  CALL optListLS%add('LinearSolverType->numberOMP',1_SNK)
  CALL optListLS%add('LinearSolverType->timerName','LinearSolver Timer')
  CALL optListLS%add('LinearSolverType->matType',SPARSE)
  ! set parameters for matrices
  CALL optListLS%add('LinearSolverType->A->MatrixType->n',-1_SNK)
  CALL optListLS%add('LinearSolverType->A->MatrixType->nnz',-1_SNK)
  CALL optListLS%add('LinearSolverType->A->MatrixType->isSym',.FALSE.)
  ! set parameters for vectors
  CALL optListLS%add('LinearSolverType->x->VectorType->n',-1_SNK)
  CALL optListLS%add('LinearSolverType->b->VectorType->n',-1_SNK)
  
  !Configure exception handler for test
  CALL e%setStopOnError(.FALSE.)
  CALL e%setQuietMode(.TRUE.)
  CALL eParams%addSurrogate(e)
  CALL ePreCondType%addSurrogate(e)
  CALL eLinearSolverType%addSurrogate(e)
  CALL mpiTestEnv%init(PE_COMM_SELF)

  CREATE_TEST('Test RSOR Preconditioner')

  CALL setupILUvsRSOR()
  REGISTER_SUBTEST('Test RSOR Preconditioner Type',testILUvsRSOR)
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
    SUBROUTINE setupILUvsRSOR()
        INTEGER(SIK)::i,j,ios
        REAL(SRK)::tmpreal
        CHARACTER(100)::tempcharacter
        
        CALL PListRSOR%add('PCType->numblocks',27_SIK)
        CALL PListRSOR%add('PCType->omega',1.0_SRK)
        CALL PListVec%add('VectorType->n',1512_SIK)
        ALLOCATE(RealVectorType :: rhsVector)
        CALL rhsVector%init(PListVec)
        CALL PListMat%add('MatrixType->n',1512_SIK)
        CALL PListMat%add('MatrixType->isSym',.FALSE.)
        ALLOCATE(DenseSquareMatrixType :: testDenseMatrix)
        CALL testDenseMatrix%init(PListMat)
        
        

        OPEN(UNIT=11,FILE='/home/nfherrin/research/MPACT/Futility/unit_tests/ILUvsRSOR/matrices/mg_matrix.txt',&
            STATUS='OLD',ACTION='READ',IOSTAT=ios,IOMSG=tempcharacter)
        IF(ios .NE. 0)THEN
            WRITE(*,*)tempcharacter
            WRITE(*,*)'Could not open res.dat'
            STOP
        END IF
        DO i=1,1512
            DO j=1,1512
                CALL testDenseMatrix%set(i,j,0.0_SRK)
            END DO
        END DO
        READ(11,*)
        DO
            READ(11,*,IOSTAT=ios)i,j,tmpreal
            IF(ios >0)THEN
                STOP 'File input error'
            ELSE IF(ios<0)THEN
                EXIT
            ELSE
                CALL testDenseMatrix%set(i,j,tmpreal)
            END IF
        END DO
        
        CLOSE(11)
        
        DO i=1,1512
            CALL RANDOM_NUMBER(tmpreal)
            CALL rhsVector%set(i,tmpreal)
        END DO

    ENDSUBROUTINE setupILUvsRSOR
!
!-------------------------------------------------------------------------------
    SUBROUTINE testILUvsRSOR()
        CLASS(SOR_PreCondType),ALLOCATABLE :: testSOR
        CLASS(LU_PreCondType),ALLOCATABLE :: testLU
        CLASS(LinearSolverType_Base),ALLOCATABLE :: thisGMRES
        REAL(SRK),POINTER :: thisX(:)
        INTEGER(SIK)::i,j,k
        REAL(SRK)::tmpreal
        
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        !No preconditioner
        
        !ALLOCATE(LinearSolverType_Iterative :: thisGMRES)
        !
        !! initialize linear system
        !CALL pList%clear()
        !CALL pList%add('LinearSolverType->TPLType',NATIVE)
        !CALL pList%add('LinearSolverType->solverMethod',GMRES)
        !CALL pList%add('LinearSolverType->MPI_Comm_ID',PE_COMM_SELF)
        !CALL pList%add('LinearSolverType->numberOMP',1_SNK)
        !CALL pList%add('LinearSolverType->timerName','testTimer')
        !CALL pList%add('LinearSolverType->matType',DENSESQUARE)
        !CALL pList%add('LinearSolverType->A->MatrixType->n',1512_SNK)
        !CALL pList%add('LinearSolverType->A->MatrixType->isSym',.FALSE.)
        !CALL pList%add('LinearSolverType->x->VectorType->n',1512_SNK)
        !CALL pList%add('LinearSolverType->b->VectorType->n',1512_SNK)
        !CALL pList%validate(pList,optListLS)
        !CALL thisGMRES%init(pList)
        !
        !! build X0 and set it to 1.0s
        !ALLOCATE(thisX(1512))
        !thisX=1.0_SRK
        !
        !SELECTTYPE(thisGMRES); TYPE IS(LinearSolverType_Iterative)
        !    CALL thisGMRES%setX0(thisX)
        !ENDSELECT
        !
        !SELECTTYPE(A => thisGMRES%A); TYPE IS(DenseSquareMatrixType)
        !    DO i=1,1512
        !        DO j=1,1512
        !            CALL testDenseMatrix%get(i,j,tmpreal)
        !            CALL A%set(i,j,tmpreal)
        !        END DO
        !    END DO
        !ENDSELECT
        !
        !SELECTTYPE(b => thisGMRES%b); TYPE IS(RealVectorType)
        !    DO i=1,1512
        !            CALL rhsVector%get(i,tmpreal)
        !            CALL b%set(i,tmpreal)
        !    END DO
        !ENDSELECT
        !
        !!set iterations and convergence information and build/set M
        !SELECTTYPE(thisGMRES); TYPE IS(LinearSolverType_Iterative)
        !    CALL thisGMRES%setConv(2_SIK,1.0E-9_SRK,1.0E-9_SRK,1000_SIK,1512_SIK)
        !ENDSELECT
        !
        !CALL thisGMRES%solve()
        !SELECTTYPE(thisGMRES); TYPE IS(LinearSolverType_Iterative)
        !    WRITE(*,'(A,I0)')'No Precondition Iterations ',thisGMRES%iters
        !    WRITE(*,'(A,ES28.18)')'No Precondition Residual ',thisGMRES%residual
        !ENDSELECT
        !DEALLOCATE(thisGMRES)
        !
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        !!ILU preconditioner
        !ALLOCATE(LinearSolverType_Iterative :: thisGMRES)
        !
        !! initialize linear system
        !CALL pList%clear()
        !CALL pList%add('LinearSolverType->TPLType',NATIVE)
        !CALL pList%add('LinearSolverType->solverMethod',GMRES)
        !CALL pList%add('LinearSolverType->MPI_Comm_ID',PE_COMM_SELF)
        !CALL pList%add('LinearSolverType->numberOMP',1_SNK)
        !CALL pList%add('LinearSolverType->timerName','testTimer')
        !CALL pList%add('LinearSolverType->matType',DENSESQUARE)
        !CALL pList%add('LinearSolverType->A->MatrixType->n',1512_SNK)
        !CALL pList%add('LinearSolverType->A->MatrixType->isSym',.FALSE.)
        !CALL pList%add('LinearSolverType->x->VectorType->n',1512_SNK)
        !CALL pList%add('LinearSolverType->b->VectorType->n',1512_SNK)
        !CALL pList%validate(pList,optListLS)
        !CALL thisGMRES%init(pList)
        !
        !! build X0 and set it to 1.0s
        !ALLOCATE(thisX(1512))
        !thisX=1.0_SRK
        !
        !SELECTTYPE(thisGMRES); TYPE IS(LinearSolverType_Iterative)
        !    CALL thisGMRES%setX0(thisX)
        !ENDSELECT
        !
        !SELECTTYPE(A => thisGMRES%A); TYPE IS(DenseSquareMatrixType)
        !    DO i=1,1512
        !        DO j=1,1512
        !            CALL testDenseMatrix%get(i,j,tmpreal)
        !            CALL A%set(i,j,tmpreal)
        !        END DO
        !    END DO
        !ENDSELECT
        !
        !SELECTTYPE(b => thisGMRES%b); TYPE IS(RealVectorType)
        !    DO i=1,1512
        !            CALL rhsVector%get(i,tmpreal)
        !            CALL b%set(i,tmpreal)
        !    END DO
        !ENDSELECT
        !
        !SELECTTYPE(thisGMRES); TYPE IS(LinearSolverType_Iterative)
        !    ALLOCATE(ILU_PreCondType :: thisGMRES%PreCondType)
        !    CALL thisGMRES%setupPC()
        !    thisGMRES%pciters=1
        !ENDSELECT
        !
        !!set iterations and convergence information and build/set M
        !SELECTTYPE(thisGMRES); TYPE IS(LinearSolverType_Iterative)
        !    CALL thisGMRES%setConv(2_SIK,1.0E-9_SRK,1.0E-9_SRK,1000_SIK,1512_SIK)
        !ENDSELECT
        !
        !CALL thisGMRES%solve()
        !SELECTTYPE(thisGMRES); TYPE IS(LinearSolverType_Iterative)
        !    WRITE(*,'(A,I0)')'ILU Iterations ',thisGMRES%iters
        !    WRITE(*,'(A,ES28.18)')'ILU Residual ',thisGMRES%residual
        !ENDSELECT
        !DEALLOCATE(thisGMRES)
        
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        !RSOR preconditioner
        ALLOCATE(LinearSolverType_Iterative :: thisGMRES)

        ! initialize linear system
        CALL pList%clear()
        CALL pList%add('LinearSolverType->TPLType',NATIVE)
        CALL pList%add('LinearSolverType->solverMethod',GMRES)
        CALL pList%add('LinearSolverType->MPI_Comm_ID',PE_COMM_SELF)
        CALL pList%add('LinearSolverType->numberOMP',1_SNK)
        CALL pList%add('LinearSolverType->timerName','testTimer')
        CALL pList%add('LinearSolverType->matType',DENSESQUARE)
        CALL pList%add('LinearSolverType->A->MatrixType->n',1512_SNK)
        CALL pList%add('LinearSolverType->A->MatrixType->isSym',.FALSE.)
        CALL pList%add('LinearSolverType->x->VectorType->n',1512_SNK)
        CALL pList%add('LinearSolverType->b->VectorType->n',1512_SNK)
        CALL pList%validate(pList,optListLS)
        CALL thisGMRES%init(pList)

        ! build X0 and set it to 1.0s
        ALLOCATE(thisX(1512))
        thisX=1.0_SRK

        SELECTTYPE(thisGMRES); TYPE IS(LinearSolverType_Iterative)
            CALL thisGMRES%setX0(thisX)
        ENDSELECT
        
        SELECTTYPE(A => thisGMRES%A); TYPE IS(DenseSquareMatrixType)
            DO i=1,1512
                DO j=1,1512
                    CALL testDenseMatrix%get(i,j,tmpreal)
                    CALL A%set(i,j,tmpreal)
                END DO
            END DO
        ENDSELECT
        
        SELECTTYPE(b => thisGMRES%b); TYPE IS(RealVectorType)
            DO i=1,1512
                    CALL rhsVector%get(i,tmpreal)
                    CALL b%set(i,tmpreal)
            END DO
        ENDSELECT
        
        SELECTTYPE(thisGMRES); TYPE IS(LinearSolverType_Iterative)
            ALLOCATE(RSOR_PreCondType :: thisGMRES%PreCondType)
            CALL thisGMRES%setupPC(PListRSOR)
            thisGMRES%pciters=1
            SELECTTYPE(precon => thisGMRES%PreCondType); TYPE IS(RSOR_PreCondType)
                WRITE(*,'(A,F8.4)')'Omega ',precon%omega
            ENDSELECT
        ENDSELECT

        !set iterations and convergence information and build/set M
        SELECTTYPE(thisGMRES); TYPE IS(LinearSolverType_Iterative)
            CALL thisGMRES%setConv(2_SIK,1.0E-9_SRK,1.0E-9_SRK,1000_SIK,1512_SIK)
        ENDSELECT
        
        CALL thisGMRES%solve()
        SELECTTYPE(thisGMRES); TYPE IS(LinearSolverType_Iterative)
            WRITE(*,'(A,I0)')'RSOR Iterations ',thisGMRES%iters
            WRITE(*,'(A,ES28.18)')'RSOR Residual ',thisGMRES%residual
        ENDSELECT
        
        !troubleshooting
        !SELECTTYPE(thisGMRES); TYPE IS(LinearSolverType_Iterative)
        !    !WRITE(*,*)'AMatrix'
        !    !DO i=1,1512
        !    !    DO j=1,1512
        !    !        CALL thisGMRES%A%get(i,j,tmpreal)
        !    !        WRITE(*,'(ES28.18)',ADVANCE='NO')tmpreal
        !    !    END DO
        !    !    WRITE(*,*)
        !    !END DO
        !    SELECTTYPE(precon => thisGMRES%PreCondType); TYPE IS(RSOR_PreCondType)
        !        !WRITE(*,*)'LpUMatrix'
        !        !DO i=1,1512
        !        !    DO j=1,1512
        !        !        CALL precon%LpU%get(i,j,tmpreal)
        !        !        WRITE(*,'(ES28.18)',ADVANCE='NO')tmpreal
        !        !    END DO
        !        !    WRITE(*,*)
        !        !END DO
        !        WRITE(*,*)'LUofBlocks'
        !        DO k=1,precon%numblocks
        !            !WRITE(*,'(A,I0)')'Block',k
        !            DO i=1,precon%blocksize
        !                DO j=1,precon%blocksize
        !                    CALL precon%LU(k)%get(i,j,tmpreal)
        !                    WRITE(*,'(ES28.18)',ADVANCE='NO')tmpreal
        !                END DO
        !            WRITE(*,*)
        !            END DO
        !        END DO
        !    ENDSELECT
        !    !WRITE(*,*)'RHSVector'
        !    !DO i=1,1512
        !    !    CALL thisGMRES%b%get(i,tmpreal)
        !    !    WRITE(*,'(ES28.18)')tmpreal
        !    !END DO
        !    WRITE(*,*)'AppliedToRHS'
        !    CALL thisGMRES%PreCondType%apply(thisGMRES%b)
        !    DO i=1,1512
        !        CALL thisGMRES%b%get(i,tmpreal)
        !        WRITE(*,'(ES28.18)')tmpreal
        !    END DO
        !ENDSELECT
        
        DEALLOCATE(thisGMRES)

    ENDSUBROUTINE testILUvsRSOR
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
      IF(ALLOCATED(rhsVector)) CALL rhsVector%clear()
      IF(ALLOCATED(testDummy)) CALL testDummy%clear()

      IF(ALLOCATED(testSparseMatrix)) DEALLOCATE(testSparseMatrix)
      IF(ALLOCATED(testVec_1g)) DEALLOCATE(testVec_1g)
      IF(ALLOCATED(testVec_mg)) DEALLOCATE(testVec_mg)
      IF(ALLOCATED(testDenseMatrix)) DEALLOCATE(testDenseMatrix)
      IF(ALLOCATED(testMatrix)) DEALLOCATE(testMatrix)
      IF(ALLOCATED(rhsVector)) DEALLOCATE(rhsVector)
      IF(ALLOCATED(testDummy)) DEALLOCATE(testDummy)

    ENDSUBROUTINE clearTest
!
ENDPROGRAM ILUvsRSOR
