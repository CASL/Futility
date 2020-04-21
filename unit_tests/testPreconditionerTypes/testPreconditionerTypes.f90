!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE dummyPCShell
USE IntrType
USE VectorTypes
USE MatrixTypes
USE PreconditionerTypes
USE ParameterLists

#ifdef FUTILITY_HAVE_PETSC
#include <petscversion.h>
#if ((PETSC_VERSION_MAJOR>=3) && (PETSC_VERSION_MINOR>6))
USE PETSCPC
#endif
#endif

#ifdef FUTILITY_HAVE_PETSC
#include <petscversion.h>
#if ((PETSC_VERSION_MAJOR>=3) && (PETSC_VERSION_MINOR>=6))
#include <petsc/finclude/petsc.h>
#else
#include <finclude/petsc.h>
#endif
#undef IS
#endif
TYPE,EXTENDS(PreconditionerType) :: dummyPCType
#ifdef FUTILITY_HAVE_PETSC
  Mat :: M
#endif
  CONTAINS
    PROCEDURE,PASS :: init => init_dummyPC
    PROCEDURE,PASS :: clear => clear_dummyPC
    PROCEDURE,PASS :: setup => setup_dummyPC
    PROCEDURE,PASS :: apply => apply_dummyPC
ENDTYPE dummyPCType

TYPE,EXTENDS(dummyPCType) :: smartPCType
  CONTAINS
    PROCEDURE,PASS :: setup => setup_smartPC
    PROCEDURE,PASS :: apply => apply_smartPC
ENDTYPE smartPCType

CONTAINS
!
SUBROUTINE init_dummyPC(thisPC,A,params)
  CLASS(dummyPCType),INTENT(INOUT) :: thisPC
  CLASS(MatrixType),TARGET,INTENT(IN) :: A
  TYPE(ParamType),INTENT(IN),OPTIONAL :: params
#ifdef FUTILITY_HAVE_PETSC
  PetscErrorCode  :: ierr
  CALL MatCreate(MPI_COMM_WORLD,thisPC%M,ierr)
#endif
  thisPC%isInit=.TRUE.
ENDSUBROUTINE init_dummyPC
!
SUBROUTINE clear_dummyPC(thisPC)
  CLASS(dummyPCType),INTENT(INOUT) :: thisPC
#ifdef FUTILITY_HAVE_PETSC
  PetscErrorCode  :: ierr
  CALL MatDestroy(thisPC%M,ierr)
#endif
  thisPC%isInit=.FALSE.
ENDSUBROUTINE clear_dummyPC
!
SUBROUTINE setup_dummyPC(thisPC)
  CLASS(dummyPCType),INTENT(INOUT) :: thisPC
ENDSUBROUTINE setup_dummyPC
!
SUBROUTINE apply_dummyPC(thisPC,v)
  CLASS(dummyPCType),INTENT(INOUT) :: thisPC
  CLASS(VectorType),INTENT(INOUT) :: v
ENDSUBROUTINE apply_dummyPC
!
SUBROUTINE setup_smartPC(thisPC)
  CLASS(smartPCType),INTENT(INOUT) :: thisPC
#ifdef FUTILITY_HAVE_PETSC
  PetscErrorCode  :: ierr
  CALL MatSetSizes(thisPC%M,3,3,3,3,ierr)
  CALL MatSetType(thisPC%M,MATMPIAIJ,ierr)
  CALL MatSetUp(thisPC%M,ierr)
  CALL MatSetValues(thisPC%M,1,0,1,0,0.75_SRK,INSERT_VALUES,ierr)
  CALL MatSetValues(thisPC%M,1,0,1,1,0.50_SRK,INSERT_VALUES,ierr)
  CALL MatSetValues(thisPC%M,1,0,1,2,0.25_SRK,INSERT_VALUES,ierr)
  CALL MatSetValues(thisPC%M,1,1,1,0,0.50_SRK,INSERT_VALUES,ierr)
  CALL MatSetValues(thisPC%M,1,1,1,1,1.00_SRK,INSERT_VALUES,ierr)
  CALL MatSetValues(thisPC%M,1,1,1,2,0.50_SRK,INSERT_VALUES,ierr)
  CALL MatSetValues(thisPC%M,1,2,1,0,0.25_SRK,INSERT_VALUES,ierr)
  CALL MatSetValues(thisPC%M,1,2,1,1,0.50_SRK,INSERT_VALUES,ierr)
  CALL MatSetValues(thisPC%M,1,2,1,2,0.75_SRK,INSERT_VALUES,ierr)
  CALL MatAssemblyBegin(thisPC%M,MAT_FINAL_ASSEMBLY,ierr)
  CALL MatAssemblyEnd(thisPC%M,MAT_FINAL_ASSEMBLY,ierr)
#endif
ENDSUBROUTINE setup_smartPC
!
SUBROUTINE apply_smartPC(thisPC,v)
  CLASS(smartPCType),INTENT(INOUT) :: thisPC
  CLASS(VectorType),INTENT(INOUT) :: v
#ifdef FUTILITY_HAVE_PETSC
  Vec :: tmp
  PetscErrorCode  :: ierr
  SELECT TYPE(v); TYPE IS(PETScVectorType)
    CALL VecDuplicate(v%b,tmp,ierr)
    CALL VecCopy(v%b,tmp,ierr)
    CALL MatMult(thisPC%M,tmp,v%b,ierr)
  ENDSELECT
#endif
ENDSUBROUTINE apply_smartPC
ENDMODULE

PROGRAM testPreconditionerTypes
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
USE dummyPCShell
IMPLICIT NONE

#ifdef FUTILITY_HAVE_PETSC
PetscErrorCode  :: ierr
#else
#ifdef HAVE_MPI
INCLUDE 'mpif.h'
#endif
#endif

TYPE(ExceptionHandlerType),TARGET :: e
TYPE(ParamType) :: PListMat,PListVec
CLASS(MatrixType),ALLOCATABLE :: testSparseMatrix,testDenseMatrix,testMatrix
CLASS(VectorType),ALLOCATABLE :: testVector,testDummy
CLASS(VectorType),ALLOCATABLE :: testVec_1g,testVec_mg
INTEGER(SIK) :: nerrors1,nerrors2

#ifdef HAVE_MPI
INTEGER :: mpierr
CALL MPI_Init(mpierr)
#elif defined FUTILITY_HAVE_PETSC
INTEGER :: MPI_COMM_WORLD=0
#endif
!Configure exception handler for test
CALL e%setStopOnError(.FALSE.)
CALL e%setQuietMode(.TRUE.)
CALL eParams%addSurrogate(e)
CALL ePreCondType%addSurrogate(e)

#ifdef FUTILITY_HAVE_PETSC
CALL PetscInitialize(PETSC_NULL_CHARACTER,ierr)
#endif

CREATE_TEST('Test Preconditioner Types')

CALL setupILUTest()
REGISTER_SUBTEST('Test ILU Preconditioner Type',testILU_PreCondType)
CALL clearTest()

REGISTER_SUBTEST('Test PCShell',testPCShell)

FINALIZE_TEST()

#ifdef FUTILITY_HAVE_PETSC
CALL PetscFinalize(ierr)
#endif
#ifdef HAVE_MPI
CALL MPI_Finalize(mpierr)
#endif
!
!===============================================================================
CONTAINS
!
!-------------------------------------------------------------------------------
SUBROUTINE setupILUTest()
  INTEGER(SIK) :: i
  REAL(SRK) :: tmpreal

  !Set up Vector
  CALL PListVec%add('VectorType->n',10_SIK)
  ALLOCATE(RealVectorType :: testVector)
  CALL testVector%init(PListVec)

  !Set up Matrices
  CALL PListMat%add('MatrixType->nnz',40) ! Will be a 10x10, 5-stripe matrix
  CALL PListMat%add('MatrixType->n',10)
  CALL PListMat%add('MatrixType->isSym',.FALSE.)
  ALLOCATE(SparseMatrixType :: testSparseMatrix)
  ALLOCATE(DenseSquareMatrixType :: testDenseMatrix)
  CALL testSparseMatrix%init(PListMat)
  CALL testDenseMatrix%init(PListMat)

  ! Fill Matrix
  SELECT TYPE(testSparseMatrix)
  TYPE IS(SparseMatrixType)
    tmpreal=0.0_SRK
    DO i=1,10
      IF(i >= 5) THEN
        tmpreal=tmpreal+1.0_SRK
        CALL testSparseMatrix%setShape(i,i-4,tmpreal)
        CALL testDenseMatrix%set(i,i-4,tmpreal)
      ENDIF
      IF(i >= 2) THEN
        tmpreal=tmpreal+1.0_SRK
        CALL testSparseMatrix%setShape(i,i-1,tmpreal)
        CALL testDenseMatrix%set(i,i-1,tmpreal)
      ENDIF
      tmpreal=tmpreal+1.0_SRK
      CALL testSparseMatrix%setShape(i,i,tmpreal)
      CALL testDenseMatrix%set(i,i,tmpreal)
      IF(i <= 9) THEN
        tmpreal=tmpreal+1.0_SRK
        CALL testSparseMatrix%setShape(i,i+1,tmpreal)
        CALL testDenseMatrix%set(i,i+1,tmpreal)
      ENDIF
      IF(i <= 6) THEN
        tmpreal=tmpreal+1.0_SRK
        CALL testSparseMatrix%setShape(i,i+4,tmpreal)
        CALL testDenseMatrix%set(i,i+4,tmpreal)
      ENDIF
      CALL testVector%set(i,REAL(i*1.0_SRK,SRK))
    ENDDO
  CLASS DEFAULT
    ASSERT(.FALSE.,'ALLOCATE(SparseMatrixType :: testSparseMatrix)')
  ENDSELECT

ENDSUBROUTINE setupILUTest
!
!-------------------------------------------------------------------------------
SUBROUTINE testILU_PreCondType()
  CLASS(LU_PreCondType),ALLOCATABLE :: testLU
  TYPE(RealVectorType) :: tempVector
  LOGICAL(SBK) :: bool
  REAL(SRK) :: tmpreal(40),tmpreal2(10)
  INTEGER(SIK) :: i,j,k


  ALLOCATE(ILU_PreCondType :: testLU)
  !Test error checking
  CALL testLU%init(testDenseMatrix)
  nerrors1=e%getCounter(EXCEPTION_ERROR)
  CALL testLU%init(testDenseMatrix)
  nerrors2=e%getCounter(EXCEPTION_ERROR)
  ASSERT(nerrors2 == nerrors1+1,'init_LU_Preconditioner PC%isInit check')
  FINFO() 'Result:',nerrors2,'Solution:',nerrors1+1
  CALL testLU%clear()

  nerrors1=e%getCounter(EXCEPTION_ERROR)
  CALL testLU%init(testMatrix)
  nerrors2=e%getCounter(EXCEPTION_ERROR)
  ASSERT(nerrors2 == nerrors1+1,'init_LU_Preconditioner ALLOCATED(PC%A) check')
  FINFO() 'Result:',nerrors2,'Solution:',nerrors1+1

  nerrors1=e%getCounter(EXCEPTION_ERROR)
  ALLOCATE(DenseSquareMatrixType :: testMatrix) !Just a dummy matrix for error check tests
  CALL testLU%init(testMatrix)
  nerrors2=e%getCounter(EXCEPTION_ERROR)
  ASSERT(nerrors2 == nerrors1+1,'init_LU_Preconditioner PC%A%isInit check')
  FINFO() 'Result:',nerrors2,'Solution:',nerrors1+1

  CALL testLU%clear()
  nerrors1=e%getCounter(EXCEPTION_ERROR)
  CALL testLU%setup()
  nerrors2=e%getCounter(EXCEPTION_ERROR)
  ASSERT(nerrors2 == nerrors1+1,'setup_LU_Preconditioner PC%isInit check')
  FINFO() 'Result:',nerrors2,'Solution:',nerrors1+1

  CALL testLU%init(testDenseMatrix)
  SELECT TYPE(LU => testLU%LU)
  TYPE IS(DenseSquareMatrixType)
    CALL LU%clear()
    nerrors1=e%getCounter(EXCEPTION_ERROR)
    CALL testLU%setup()
    nerrors2=e%getCounter(EXCEPTION_ERROR)
    ASSERT(nerrors2 == nerrors1+1,'setup_LU_Preconditioner PC%LU%isInit check')
    FINFO() 'Result:',nerrors2,'Solution:',nerrors1+1
! This test is commented out for the time being
! A workaround had to be implemented in gnu, and a petsc build still fails
! There's no way this should ever happen, so it shouldn't be a problem.
!        DEALLOCATE(LU)
!        nerrors1=e%getCounter(EXCEPTION_ERROR)
!        CALL testLU%setup()
!        nerrors2=e%getCounter(EXCEPTION_ERROR)
!        ASSERT(nerrors2 == nerrors1+1,'setup_LU_Preconditioner ALLOCATED(PC%LU) check')
!        FINFO() 'Result:',nerrors2,'Solution:',nerrors1+1
!        ALLOCATE(LU) ! This has to be done or memory corruption occurs in testLU%clear()
!                     ! I think this is a gnu bug based on some documentation.  This doesn't
!                     ! hurt anything though
  ENDSELECT

  CALL testLU%clear()

  ALLOCATE(RealVectorType :: testDummy)
  nerrors1=e%getCounter(EXCEPTION_ERROR)
  CALL testLU%apply(testDummy)
  nerrors2=e%getCounter(EXCEPTION_ERROR)
  ASSERT(nerrors2 == nerrors1+1,'apply_LU_Preconditioner v%isInit check')
  FINFO() 'Result:',nerrors2,'Solution:',nerrors1+1

  !This is used to check %apply() routines
  tmpreal=(/1.0000000000000000_SRK,2.0000000000000000_SRK,3.0000000000000000_SRK, &
      4.0000000000000000_SRK,-3.0000000000000000_SRK,6.0000000000000000_SRK, &
      7.0000000000000000_SRK,-2.6666666666666665_SRK,25.000000000000000_SRK, &
      10.000000000000000_SRK,11.000000000000000_SRK,0.47999999999999998_SRK, &
      8.1999999999999993_SRK,14.000000000000000_SRK,15.000000000000000_SRK, &
      16.000000000000000_SRK,2.0731707317073171_SRK,-59.024390243902438_SRK, &
      19.000000000000000_SRK,20.000000000000000_SRK,-7.0000000000000000_SRK, &
      -0.37272727272727274_SRK,79.081818181818178_SRK,24.000000000000000_SRK, &
      25.000000000000000_SRK,1.0400000000000000_SRK,0.34141855385676517_SRK, &
      8.3659547074376341_SRK,29.000000000000000_SRK,3.6585365853658538_SRK, &
      3.7054946009258081_SRK,-130.33739220733625_SRK,33.000000000000000_SRK,&
      -0.57603305785123970_SRK,-0.26853383673906256_SRK,56.382277769413854_SRK, &
      37.000000000000000_SRK,0.48051500172433614_SRK,0.69170671251519822_SRK,&
      2.3939765938292652_SRK/)
  tmpreal2=(/81.828106457936357_SRK,-32.910224240167040_SRK,-8.6840659884980056_SRK,12.521242974252598_SRK, &
      -5.0025526592007603_SRK,-6.9466109699304406_SRK,8.1414442396900757_SRK,-1.8345636773373761_SRK, &
      -9.2450823045000341_SRK,13.515984012597492_SRK/)

  COMPONENT_TEST('ILU_PreCondType, DenseMatrixType')
  IF(testDenseMatrix%isInit .AND. testVector%isInit) THEN

    ! Check %init
    CALL testLU%init(testDenseMatrix)
    ASSERT(testLU%isInit,'DenseSquareMatrixType ILU%isInit')
    ASSERT(ASSOCIATED(testLU%A),'DenseSquareMatrixType ASSOCIATED(ILU%LU%A)')
    ASSERT(testLU%LU%isInit,'DenseSquareMatrixType ILU%LU%isInit')
    SELECT TYPE(LU => testLU%LU)
    TYPE IS(DenseSquareMatrixType)
      SELECT TYPE(A => testLU%A); TYPE IS(DenseSquareMatrixType)
        ASSERT(ALL(LU%a .APPROXEQR. A%a),'DenseSquareMatrixType ILU%LU%a')
      ENDSELECT
    CLASS DEFAULT
      ASSERT(.FALSE.,'DenseSquareMatrixType ILU%LU TYPE IS(DenseSquareMatrixType)')
    ENDSELECT


    ! Check %setup
    CALL testLU%setup()
    SELECT TYPE(LU => testLU%LU); TYPE IS(DenseSquareMatrixType)
      k=0
      DO j=1,LU%n
        DO i=1,LU%n
          IF(LU%a(j,i) .APPROXEQR. 0.0_SRK) THEN
            CYCLE
          ELSE
            k=k+1
            ASSERT(LU%a(j,i) .APPROXEQR. tmpreal(k),'testLU%LU%a')
            FINFO() 'row:',j,'column',i,'result:',LU%a(j,i),'solution:',tmpreal(k)
          ENDIF
        ENDDO
      ENDDO
    ENDSELECT

    ! Check %apply
    SELECT TYPE(testVector); TYPE IS(RealVectorType)
      tempVector=testVector
    ENDSELECT
    CALL testLU%apply(tempVector)
    ASSERT(ALL(tempVector%b .APPROXEQR. tmpreal2),'DenseSquareMatrixType ILU%apply(vector)')
    FINFO() 'Result:',tempVector%b,'Solution:',tmpreal2


    ! Check %clear
    CALL testLU%clear()
    ASSERT(.NOT.(testLU%isInit),'SparseMatrixType .NOT.(ILU%LU%isInit)')
    ASSERT(.NOT.(ASSOCIATED(testLU%A)),'SparseMatrixType .NOT.(ASSOCIATED(ILU%LU%A))')
    ASSERT(.NOT.(ALLOCATED(testLU%LU)),'SparseMatrixType .NOT.(ASSOCIATED(ILU%LU%L))')
  ELSE
    ASSERT(testSparseMatrix%isInit,'TestDenseMatrix Initialization')
    ASSERT(testVector%isInit,'TestVector Initialization')
  ENDIF

  COMPONENT_TEST('ILU_PreCondType, SparseMatrixType')
  ! Check %init
  CALL testLU%init(testSparseMatrix)
  IF(testSparseMatrix%isInit .AND. testVector%isInit) THEN
    ASSERT(testLU%isInit,'SparseMatrixType ILU%isInit')
    ASSERT(ASSOCIATED(testLU%A),'SparseMatrixType ASSOCIATED(ILU%LU%A)')
    ASSERT(testLU%LU%isInit,'SparseMatrixType ILU%LU%isInit')
    SELECT TYPE(LU => testLU%LU)
    TYPE IS(SparseMatrixType)
      bool=ALL(LU%ia == (/1,4,8,12,16,21,26,30,34,38,41/))
      ASSERT(bool,'SparseMatrixType ILU%LU%ia')
      FINFO() 'Result:',LU%ia,'Solution:',(/1,4,8,12,16,21,26,30,34,38,41/)
      bool=ALL(LU%ja == (/1,2,5,1,2,3,6,2,3,4,7,3,4,5,8,1,4,5,6,9,2,5,6,7,10,3,6,7,8, &
      4,7,8,9,5,8,9,10,6,9,10/))
      ASSERT(bool,'SparseMatrixType ILU%LU%ja')
    CLASS DEFAULT
      ASSERT(.FALSE.,'SparseMatrixType ILU%LU TYPE IS(SparseMatrixType)')
    ENDSELECT

!        ! Check %setup
    CALL testLU%setup()
    SELECT TYPE(LU => testLU%LU); TYPE IS(SparseMatrixType)
      ASSERT(ALL(LU%a .APPROXEQR. tmpreal),'SparseMatrixtype ILU%L%a')
      FINFO() 'Result:',LU%a,'Solution:',tmpreal
    ENDSELECT

    ! Check %apply
    SELECT TYPE(testVector); TYPE IS(RealVectorType)
      tempVector=testVector
    ENDSELECT
    CALL testLU%apply(tempVector)
    ASSERT(ALL(tempVector%b .APPROXEQR. tmpreal2),'SparseMatrixType ILU%apply(vector)')
    FINFO() 'Result:',tempVector%b,'Solution:',tmpreal2

    ! Check %clear
    CALL testLU%clear()
    ASSERT(.NOT.(testLU%isInit),'SparseMatrixType .NOT.(ILU%LU%isInit)')
    ASSERT(.NOT.(ASSOCIATED(testLU%A)),'SparseMatrixType .NOT.(ASSOCIATED(ILU%LU%A))')
    ASSERT(.NOT.(ALLOCATED(testLU%LU)),'SparseMatrixType .NOT.(ASSOCIATED(ILU%LU%L))')
  ELSE
    ASSERT(testSparseMatrix%isInit,'TestSparseMatrix Initialization')
    ASSERT(testVector%isInit,'TestVector Initialization')
  ENDIF

  DEALLOCATE(testLU)

ENDSUBROUTINE testILU_PreCondtype
!
!-------------------------------------------------------------------------------
SUBROUTINE testPCShell()
#ifdef FUTILITY_HAVE_PETSC
  KSP :: ksp
  PC  :: pc
  Mat :: A
  Vec :: x
  Vec :: b
  PetscErrorCode :: ierr

  INTEGER(SIK) :: i,niters
  REAL(SRK) :: val(1),resid
  TYPE(PETScMatrixType) :: matrix
  CLASS(PreconditionerType),POINTER :: shellPC

  ALLOCATE(dummyPCType :: shellPC)
  CALL shellPC%init(matrix)

  PETSC_PCSHELL_PC=>shellPC

  ASSERT(ASSOCIATED(PETSC_PCSHELL_PC),'preconditioner associated')
  ASSERT(PETSC_PCSHELL_PC%isInit,'preconditioner isInit')

  CALL KSPCreate(MPI_COMM_WORLD,ksp,ierr)
  CALL KSPSetType(ksp,KSPGMRES,ierr)
  CALL KSPGetPC(ksp,pc,ierr)
  CALL PCSetType(pc,PCSHELL,ierr)
  CALL PCShellSetApply(pc,PETSC_PCSHELL_apply_extern,ierr)
  CALL PCShellSetSetup(pc,PETSC_PCSHELL_setup_extern,ierr)

  CALL MatCreate(MPI_COMM_WORLD,A,ierr)
  CALL MatSetSizes(A,3,3,3,3,ierr)
  CALL MatSetType(A,MATMPIAIJ,ierr)
  CALL MatSetUp(A,ierr)
  CALL MatSetValues(A,1,0,1,0,2.0_SRK,INSERT_VALUES,ierr)
  CALL MatSetValues(A,1,1,1,1,2.0_SRK,INSERT_VALUES,ierr)
  CALL MatSetValues(A,1,2,1,2,2.0_SRK,INSERT_VALUES,ierr)
  CALL MatSetValues(A,1,0,1,1,-1.0_SRK,INSERT_VALUES,ierr)
  CALL MatSetValues(A,1,1,1,0,-1.0_SRK,INSERT_VALUES,ierr)
  CALL MatSetValues(A,1,1,1,2,-1.0_SRK,INSERT_VALUES,ierr)
  CALL MatSetValues(A,1,2,1,1,-1.0_SRK,INSERT_VALUES,ierr)
  CALL MatAssemblyBegin(A,MAT_FINAL_ASSEMBLY,ierr)
  CALL MatAssemblyEnd(A,MAT_FINAL_ASSEMBLY,ierr)

  !setup b
  CALL VecCreate(MPI_COMM_WORLD,b,ierr)
  CALL VecSetSizes(b,3,3,ierr)
  CALL VecSetType(b,VECMPI,ierr)
  CALL VecSetValue(b,0,1.0_SRK,INSERT_VALUES,ierr)
  CALL VecSetValue(b,1,1.0_SRK,INSERT_VALUES,ierr)
  CALL VecSetValue(b,2,1.0_SRK,INSERT_VALUES,ierr)
  CALL VecAssemblyBegin(b,ierr)
  CALL VecAssemblyEnd(b,ierr)

  !set initial x
  CALL VecCreate(MPI_COMM_WORLD,x,ierr)
  CALL VecSetSizes(x,3,3,ierr)
  CALL VecSetType(x,VECMPI,ierr)
  CALL VecSetValue(x,0,0.0_SRK,INSERT_VALUES,ierr)
  CALL VecSetValue(x,1,0.0_SRK,INSERT_VALUES,ierr)
  CALL VecSetValue(x,2,0.0_SRK,INSERT_VALUES,ierr)
  CALL VecAssemblyBegin(x,ierr)
  CALL VecAssemblyEnd(x,ierr)

  !set up ksp operators
#if ((PETSC_VERSION_MAJOR>=3) && (PETSC_VERSION_MINOR>=5))
  CALL KSPSetOperators(ksp,A,A,ierr)
#else
  CALL KSPSetOperators(ksp,A,A,DIFFERENT_NONZERO_PATTERN,ierr)
#endif
  !solve ksp, get number of iterations
  CALL KSPSolve(ksp,b,x,ierr)
  CALL KSPGetIterationNumber(ksp,niters,ierr)
  CALL KSPGetResidualNorm(ksp,resid,ierr)
  ASSERT(niters==2,'ksp niters')
  FINFO() niters,resid
  val=-1.0
  DO i=1,3
    CALL VecGetValues(x,1,(/i-1/),val,ierr)
    IF(i==2) THEN
      ASSERT(val(1).APPROXEQ.2.0_SRK,'ksp sol')
    ELSE
      ASSERT(val(1).APPROXEQ.1.5_SRK,'ksp sol')
    ENDIF
    FINFO() i, val(1)
  ENDDO
  CALL KSPDestroy(ksp,ierr)

  CALL shellPC%clear()
  DEALLOCATE(shellPC)
  NULLIFY(PETSC_PCSHELL_PC)


  ALLOCATE(smartPCType :: shellPC)
  CALL shellPC%init(matrix)
  !modify pc to be exact inverse
  PETSC_PCSHELL_PC=>shellPC

  ASSERT(ASSOCIATED(PETSC_PCSHELL_PC),'preconditioner associated')
  ASSERT(PETSC_PCSHELL_PC%isInit,'preconditioner isInit')
  !setup ksp again
  CALL KSPCreate(MPI_COMM_WORLD,ksp,ierr)
  CALL KSPSetType(ksp,KSPGMRES,ierr)
  CALL KSPGetPC(ksp,pc,ierr)
  CALL PCSetType(pc,PCSHELL,ierr)
  CALL PCShellSetApply(pc,PETSC_PCSHELL_apply_extern,ierr)
  CALL PCShellSetSetup(pc,PETSC_PCSHELL_setup_extern,ierr)
  !set up ksp operators
#if ((PETSC_VERSION_MAJOR>=3) && (PETSC_VERSION_MINOR>=5))
  CALL KSPSetOperators(ksp,A,A,ierr)
#else
  CALL KSPSetOperators(ksp,A,A,DIFFERENT_NONZERO_PATTERN,ierr)
#endif

  !reset x0
  CALL VecSetValue(x,0,0.0_SRK,INSERT_VALUES,ierr)
  CALL VecSetValue(x,1,0.0_SRK,INSERT_VALUES,ierr)
  CALL VecSetValue(x,2,0.0_SRK,INSERT_VALUES,ierr)
  CALL VecAssemblyBegin(x,ierr)
  CALL VecAssemblyEnd(x,ierr)

  !solve and ensure it converges in 1 iteration
  CALL KSPSolve(ksp,b,x,ierr)
  CALL KSPGetIterationNumber(ksp,niters,ierr)
  CALL KSPGetResidualNorm(ksp,resid,ierr)
  ASSERT(niters==1,'ksp niters')
  FINFO() niters,resid
  val=-1.0
  DO i=1,3
    CALL VecGetValues(x,1,(/i-1/),val,ierr)
    IF(i==2) THEN
      ASSERT(val(1).APPROXEQ.2.0_SRK,'ksp sol')
    ELSE
      ASSERT(val(1).APPROXEQ.1.5_SRK,'ksp sol')
    ENDIF
    FINFO() i, val(1)
  ENDDO

  CALL MatDestroy(A,ierr)
  CALL VecDestroy(x,ierr)
  CALL VecDestroy(b,ierr)
  CALL KSPDestroy(ksp,ierr)
  CALL shellPC%clear()
  DEALLOCATE(shellPC)
  NULLIFY(PETSC_PCSHELL_PC)
#endif
ENDSUBROUTINE testPCShell
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
ENDPROGRAM testPreconditionerTypes
