!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
PROGRAM testEigenvalueSolver
#include "UnitTest.h"
USE UnitTest
USE IntrType
USE ExceptionHandler
USE trilinos_interfaces
USE ParameterLists
USE ParallelEnv
USE VectorTypes
USE MatrixTypes
USE EigenvalueSolverTypes

#ifdef FUTILITY_HAVE_PETSC
#include <petscversion.h>
#if ((PETSC_VERSION_MAJOR>=3) && (PETSC_VERSION_MINOR>6))
USE PETSCSYS
#endif
#endif

IMPLICIT NONE

TYPE(ExceptionHandlerType),TARGET :: e
TYPE(MPI_EnvType) :: mpiTestEnv
TYPE(ParamType) :: pList, optList
CLASS(EigenvalueSolverType_Base),POINTER :: testEVS
CLASS(DistributedMatrixType),POINTER :: A => NULL()
CLASS(DistributedMatrixType),POINTER :: B => NULL()

#ifdef FUTILITY_HAVE_PETSC
#include <petscversion.h>
#if ((PETSC_VERSION_MAJOR>=3) && (PETSC_VERSION_MINOR>=6))
#include <petsc/finclude/petsc.h>
#else
#include <finclude/petsc.h>
#endif
#undef IS
PetscErrorCode  :: ierr
#ifdef FUTILITY_HAVE_SLEPC
CALL SlepcInitialize(PETSC_NULL_CHARACTER,ierr)
#else
CALL PETScInitialize(PETSC_NULL_CHARACTER,ierr)
#endif
#endif
!> set up default parameter list
CALL optList%clear()
CALL optList%add('EigenvalueSolverType->n',2_SIK)
CALL optList%add('EigenvalueSolverType->nlocal',2_SIK)
CALL optList%add('EigenvalueSolverType->solver',ARNOLDI)
CALL optList%add('EigenvalueSolverType->preconditioner','none')
CALL optList%add('EigenvalueSolverType->tolerance',1.0e-8_SRK)
CALL optList%add('EigenvalueSolverType->max_iterations',2_SIK)

!Configure exception handler for test
CALL e%setStopOnError(.FALSE.)
CALL e%setQuietMode(.TRUE.)
CALL eParams%addSurrogate(e)
CALL eEigenvalueSolverType%addSurrogate(e)
CALL mpiTestEnv%init(PE_COMM_SELF)

CREATE_TEST('Test Eigenvalue Solvers')

ALLOCATE(EigenvalueSolverType_SLEPC :: testEVS)
#ifdef FUTILITY_HAVE_SLEPC
REGISTER_SUBTEST('testInitSLEPc',testInitSLEPc)
REGISTER_SUBTEST('testSetMatSLEPc',testSetMatSLEPc)
REGISTER_SUBTEST('testSetX0SLEPc',testSetX0SLEPc)
REGISTER_SUBTEST('testSetConvSLEPc',testSetConvSLEPc)
REGISTER_SUBTEST('testSolveSLEPc',testSolveSLEPc)
REGISTER_SUBTEST('testGetResidSLEPc',testGetResidSLEPc)
REGISTER_SUBTEST('testClearSLEPc',testClearSLEPc)
#endif
IF(ASSOCIATED(A)) THEN
  IF(A%isInit) THEN
    CALL A%clear()
  ENDIF
  DEALLOCATE(A)
ENDIF
IF(ASSOCIATED(B)) THEN
  IF(B%isInit) THEN
    CALL B%clear()
  ENDIF
  DEALLOCATE(B)
ENDIF
DEALLOCATE(testEVS)
ALLOCATE(EigenvalueSolverType_Anasazi :: testEVS)
CALL optList%set('EigenvalueSolverType->solver',GD)
CALL optList%set('EigenvalueSolverType->n',30_SIK)
CALL optList%set('EigenvalueSolverType->nlocal',30_SIK)
#ifdef FUTILITY_HAVE_Trilinos
REGISTER_SUBTEST('testInitAnasazi',testInitAnasazi)
REGISTER_SUBTEST('testSetMatAnasazi',testSetMatAnasazi)
REGISTER_SUBTEST('testSetX0Anasazi',testSetX0Anasazi)
REGISTER_SUBTEST('testSetConvAnasazi',testSetConvAnasazi)
REGISTER_SUBTEST('testSolveAnasazi',testSolveAnasazi)
!REGISTER_SUBTEST('testGetResidAnasazi',testGetResidAnasazi)
REGISTER_SUBTEST('testClearAnasazi',testClearAnasazi)
#endif
FINALIZE_TEST()

CALL pList%clear()
CALL optList%clear()

IF(ASSOCIATED(A)) THEN
  IF(A%isInit) THEN
    CALL A%clear()
  ENDIF
  DEALLOCATE(A)
ENDIF
IF(ASSOCIATED(B)) THEN
  IF(B%isInit) THEN
    CALL B%clear()
  ENDIF
  DEALLOCATE(B)
ENDIF
DEALLOCATE(testEVS)

#ifdef FUTILITY_HAVE_PETSC
CALL PetscFinalize(ierr)
#else
CALL mpiTestEnv%finalize()
#endif
!
!===============================================================================
CONTAINS
!
!-------------------------------------------------------------------------------
SUBROUTINE testInitSLEPc()
  CALL testEVS%init(mpiTestEnv,optList)
  ASSERT(testEVS%isInit,'%isInit')
  ASSERT(testEVS%n==2,'%n')
  ASSERT(testEVS%maxit==2,'%maxit')
  ASSERT(testEVS%tol==1.0E-8_SRK,'%tol')
  ASSERT(testEVS%SolverMethod==ARNOLDI,'%SolverMethod')
  ASSERT(testEVS%TPLType==SLEPC,'%SolverMethod')
  ASSERT(ASSOCIATED(testEVS%MPIparallelEnv),'%MPIenv')
  ASSERT(testEVS%X%isInit ,'%x')
  SELECTTYPE(testEVS); TYPE IS(EigenvalueSolverType_SLEPC)
#ifdef FUTILITY_HAVE_SLEPC
    ASSERT(testEVS%xi%isInit,'%xi')
#endif
  ENDSELECT
ENDSUBROUTINE testInitSLEPc
!
!-------------------------------------------------------------------------------
SUBROUTINE testClearSLEPc()
  CALL testEVS%clear()
  ASSERT(testEVS%solverMethod==-1,'%solverMethod')
  ASSERT(testEVS%TPLType==-1,'%TPLType')
  ASSERT(.NOT. ASSOCIATED(testEVS%MPIparallelEnv),'%MPIenv')
  ASSERT(.NOT. testEVS%OMPparallelEnv%isInit(),'%OMPenv')
  ASSERT(testEVS%n==-1,'%n')
  ASSERT(testEVS%maxit==-1,'%maxit')
  ASSERT(testEVS%tol==0.0_SRK,'%tol')
  ASSERT(testEVS%k==0.0_SRK,'%k')
  ASSERT(.NOT. ASSOCIATED(testEVS%A),'%A')
  ASSERT(.NOT. ASSOCIATED(testEVS%B),'%B')
  ASSERT(.NOT. ASSOCIATED(testEVS%X),'%x')
  SELECTTYPE(testEVS); TYPE IS(EigenvalueSolverType_SLEPC)
#ifdef FUTILITY_HAVE_SLEPC
    ASSERT(.NOT. (testEVS%xi%isInit),'%xi')
#endif
  ENDSELECT
ENDSUBROUTINE testClearSLEPc
!
!-------------------------------------------------------------------------------
SUBROUTINE testSetMatSLEPc()
  CALL plist%clear()
  CALL plist%add('MatrixType->n',2_SIK)
  CALL plist%add('MatrixType->nlocal',2_SIK)
  CALL plist%add('MatrixType->isSym',.FALSE.)
  CALL plist%add('MatrixType->matType',0_SIK)
  CALL plist%add('MatrixType->engine',VM_PETSC)
  CALL plist%add('MatrixType->MPI_COMM_ID',testEVS%MPIparallelEnv%comm)

  CALL DistributedMatrixFactory(A, plist)
  ASSERT(ASSOCIATED(A), "A matrix not ASSOCIATED")
  CALL DistributedMatrixFactory(B, plist)
  ASSERT(ASSOCIATED(B), "B matrix not ASSOCIATED")

  CALL B%set(1,1,0.1208_SRK)
  CALL B%set(2,1,-0.117_SRK)
  CALL B%set(2,2,0.184_SRK)
  CALL A%set(1,1,0.0015_SRK)
  CALL A%set(1,2,0.325_SRK)

  CALL testEVS%setMat(A,B)

  ASSERT(ASSOCIATED(testEVS%A),'%setMat A')
  ASSERT(ASSOCIATED(testEVS%B),'%setMat B')
  ASSERT(testEVS%A%isInit,'A%isInit')
  ASSERT(testEVS%B%isInit,'B%isInit')

ENDSUBROUTINE testSetMatSLEPc
!
!-------------------------------------------------------------------------------
SUBROUTINE testSolveSLEPc()
  REAL(SRK) :: kerr
  REAL(SRK) :: flux(2)

  CALL testEVS%solve()
  kerr=testEVS%k - (0.0015_SRK+0.117_SRK*0.325_SRK/0.184_SRK)/0.1208_SRK
  ASSERT(ABS(kerr)<=1.0E-8_SRK,'%solve k')
  FINFO() testEVS%k, kerr

  CALL testEVS%x%get(flux)
  ASSERT(ABS(flux(1)/flux(2)-0.184_SRK/0.117_SRK)<1.0E-8_SRK,'%solve flux')
  FINFO() flux

ENDSUBROUTINE testSolveSLEPc
!
!-------------------------------------------------------------------------------
SUBROUTINE testSetX0SLEPc()
#ifdef FUTILITY_HAVE_PETSC
  TYPE(PETScVectorType) :: x
  REAL(SRK) :: tmp(2)
  CALL plist%clear()
  CALL plist%add('VectorType->n',2_SIK)
  CALL plist%add('VectorType->nlocal',2_SIK)
  CALL plist%add('VectorType->MPI_COMM_ID',testEVS%MPIparallelEnv%comm)
  CALL x%init(plist)
  CALL x%set(1.5_SRK)
  CALL x%assemble()

  CALL testEVS%setX0(x)

  CALL testEVS%x%get(1,tmp(1))
  CALL testEVS%x%get(2,tmp(2))
  ASSERT(tmp(1)==1.5_SRK,'%x(1)')
  ASSERT(tmp(2)==1.5_SRK,'%x(2)')

  CALL x%clear()
#endif
ENDSUBROUTINE testSetX0SLEPc
!
!-------------------------------------------------------------------------------
SUBROUTINE testSetConvSLEPc()
  CALL testEVS%SetConv(1.0E-10_SRK,100)

  ASSERT(testEVS%tol==1.0E-10_SRK,'%tol')
  ASSERT(testEVS%maxit==100,'%maxit')

ENDSUBROUTINE testSetConvSLEPc
!
!-------------------------------------------------------------------------------
SUBROUTINE testGetResidSLEPc()
  INTEGER(SIK) :: it=0
  REAL(SRK) :: r=0.0_SRK

  CALL testEVS%getResidual(r,it)

  ASSERT(it>0,'iteration')
  ASSERT(ABS(r)<1.0E-10_SRK,'residual')

ENDSUBROUTINE testGetResidSLEPc
!
!-------------------------------------------------------------------------------
!
SUBROUTINE testInitAnasazi()
  CALL testEVS%init(mpiTestEnv,optList)
  ASSERT(testEVS%isInit,'%isInit')
  ASSERT(testEVS%n==30,'%n')
  ASSERT(testEVS%maxit==2,'%maxit')
  ASSERT(testEVS%tol==1.0E-8_SRK,'%tol')
  ASSERT(testEVS%SolverMethod==GD,'%SolverMethod')
  ASSERT(testEVS%TPLType==Anasazi,'%SolverMethod')
  ASSERT(ASSOCIATED(testEVS%MPIparallelEnv),'%MPIenv')
  ASSERT(testEVS%X%isInit ,'%x')
  SELECTTYPE(testEVS); TYPE IS(EigenvalueSolverType_Anasazi)
#ifdef FUTILITY_HAVE_Trilinos
    ASSERT(testEVS%x_scale%isInit,'%x_scale')
#endif
  ENDSELECT
ENDSUBROUTINE testInitAnasazi
!
!-------------------------------------------------------------------------------
SUBROUTINE testClearAnasazi()
  CALL testEVS%clear()
  ASSERT(testEVS%solverMethod==-1,'%solverMethod')
  ASSERT(testEVS%TPLType==-1,'%TPLType')
  ASSERT(.NOT. ASSOCIATED(testEVS%MPIparallelEnv),'%MPIenv')
  ASSERT(.NOT. testEVS%OMPparallelEnv%isInit(),'%OMPenv')
  ASSERT(testEVS%n==-1,'%n')
  ASSERT(testEVS%maxit==-1,'%maxit')
  ASSERT(testEVS%tol==0.0_SRK,'%tol')
  ASSERT(testEVS%k==0.0_SRK,'%k')
  ASSERT(.NOT. ASSOCIATED(testEVS%A),'%A')
  ASSERT(.NOT. ASSOCIATED(testEVS%B),'%B')
  ASSERT(.NOT. (testEVS%X%isInit) ,'%x')
  SELECTTYPE(testEVS); TYPE IS(EigenvalueSolverType_Anasazi)
#ifdef FUTILITY_HAVE_Trilinos
    ASSERT(.NOT. testEVS%x_scale%isInit,'%x_scale')
#endif
  ENDSELECT
ENDSUBROUTINE testClearAnasazi
!
!-------------------------------------------------------------------------------
SUBROUTINE testSetMatAnasazi()
  INTEGER(SIK) :: i
  CALL plist%clear()
  CALL plist%add('MatrixType->n',30_SIK)
  CALL plist%add('MatrixType->nlocal',30_SIK)
  CALL plist%add('MatrixType->isSym',.FALSE.)
  CALL plist%add('MatrixType->matType',0_SIK)
  CALL plist%add('MatrixType->engine',VM_TRILINOS)
  CALL plist%add('MatrixType->MPI_COMM_ID',testEVS%MPIparallelEnv%comm)

  CALL DistributedMatrixFactory(A,plist)
  CALL DistributedMatrixFactory(B,plist)

  CALL B%set(1,1,0.1208_SRK)
  CALL B%set(2,1,-0.117_SRK)
  CALL B%set(2,2,0.184_SRK)
  CALL A%set(1,1,0.0015_SRK)
  CALL A%set(1,2,0.325_SRK)
  !Adding rows 3-30 because minimum subspace is set to 25 and Anasazi doesn't
  ! like having a subspace bigger than problem size.  Adding diagonal entries
  ! into B but since A is all 0 for rows 3-30, there is no source so the flux
  ! is 0 thus having no impact on the solution
  DO i=3,30
    CALL B%set(i,i,0.1_SRK)
  ENDDO

  CALL testEVS%setMat(A,B)

  ASSERT(ASSOCIATED(testEVS%A),'%setMat A')
  ASSERT(ASSOCIATED(testEVS%B),'%setMat B')
  ASSERT(testEVS%A%isInit,'A%isInit')
  ASSERT(testEVS%B%isInit,'B%isInit')

ENDSUBROUTINE testSetMatAnasazi
!
!-------------------------------------------------------------------------------
SUBROUTINE testSolveAnasazi()
  INTEGER(SIK) :: i
  REAL(SRK) :: kerr
  REAL(SRK) :: flux(30)

  CALL testEVS%solve()
  kerr=testEVS%k - (0.0015_SRK+0.117_SRK*0.325_SRK/0.184_SRK)/0.1208_SRK
  ASSERT(ABS(kerr)<=1.0E-8_SRK,'%solve k')
  FINFO() testEVS%k, kerr

  DO i=1,30
    CALL testEVS%x%get(i,flux(i))
  ENDDO
  ASSERT(ABS(flux(1)/flux(2)-0.184_SRK/0.117_SRK)<1.0E-8_SRK,'%solve flux')
  FINFO() flux(1), flux(2)
  DO i=3,30
    ASSERT(ABS(flux(i))<=1.0E-10_SRK,'%solve flux')
    FINFO() i, flux(i)
  ENDDO

ENDSUBROUTINE testSolveAnasazi
!
!-------------------------------------------------------------------------------
SUBROUTINE testSetX0Anasazi()
#ifdef FUTILITY_HAVE_Trilinos
  TYPE(TrilinosVectorType) :: x
  REAL(SRK) :: tmp(30)
  INTEGER(SIK) :: i
  CALL plist%clear()
  CALL plist%add('VectorType->n',30_SIK)
  CALL plist%add('VectorType->nlocal',30_SIK)
  CALL plist%add('VectorType->MPI_COMM_ID',testEVS%MPIparallelEnv%comm)
  CALL x%init(plist)
  CALL x%set(1.5_SRK)
  CALL x%assemble()

  CALL testEVS%setX0(x)

  DO i=1,30
    CALL testEVS%x%get(i,tmp(i))
    ASSERT(tmp(i)==1.5_SRK,'%x(i)')
    FINFO() i,tmp(i)
  ENDDO

  CALL x%clear()
#endif
ENDSUBROUTINE testSetX0Anasazi
!
!-------------------------------------------------------------------------------
SUBROUTINE testSetConvAnasazi()
  CALL testEVS%SetConv(1.0E-10_SRK,100)

  ASSERT(testEVS%tol==1.0E-10_SRK,'%tol')
  ASSERT(testEVS%maxit==100,'%maxit')

ENDSUBROUTINE testSetConvAnasazi
!
!-------------------------------------------------------------------------------
SUBROUTINE testGetResidAnasazi()
  INTEGER(SIK) :: it=0
  REAL(SRK) :: r=0.0_SRK

  CALL testEVS%getResidual(r,it)

  ASSERT(it>0,'iteration')
  ASSERT(ABS(r)<1.0E-10_SRK,'residual')

ENDSUBROUTINE testGetResidAnasazi
!
!-------------------------------------------------------------------------------
!
ENDPROGRAM testEigenvalueSolver
