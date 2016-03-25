!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!  This manuscript has been authored by UT-Battelle, LLC, under Contract       !
!  No. DE-AC0500OR22725 with the U.S. Department of Energy. The United States  !
!  Government retains and the publisher, by accepting the article for          !
!  publication, acknowledges that the United States Government retains a       !
!  non-exclusive, paid-up, irrevocable, world-wide license to publish or       !
!  reproduce the published form of this manuscript, or allow others to do so,  !
!  for the United States Government purposes.                                  !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
PROGRAM testEigenvalueSolver
#include "UnitTest.h"
  USE UnitTest
  USE IntrType
  USE ExceptionHandler
  USE ParameterLists
  USE ParallelEnv
  USE VectorTypes
  USE MatrixTypes
  USE EigenvalueSolverTypes

  IMPLICIT NONE

  TYPE(ExceptionHandlerType),TARGET :: e
  TYPE(MPI_EnvType) :: mpiTestEnv
  TYPE(ParamType) :: pList, optList
  TYPE(EigenvalueSolverType_SLEPc) :: testEVS
  TYPE(PETScMatrixType) :: A, B

#ifdef MPACT_HAVE_PETSC
#include <finclude/petsc.h>
#undef IS
  PetscErrorCode  :: ierr
#ifdef MPACT_HAVE_SLEPC
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

#ifdef MPACT_HAVE_PETSC
  REGISTER_SUBTEST('testInit',testInit)
  REGISTER_SUBTEST('testSetMat',testSetMat)
  REGISTER_SUBTEST('testSetX0',testSetX0)
  REGISTER_SUBTEST('testSetConv',testSetConv)
#ifdef MPACT_HAVE_SLEPC
  REGISTER_SUBTEST('testSolve',testSolve)
  REGISTER_SUBTEST('testGetResid',testGetResid)
#endif
  REGISTER_SUBTEST('testClear',testClear)
#endif
  FINALIZE_TEST()

  CALL pList%clear()
  CALL optList%clear()
  CALL A%clear()
  CALL B%clear()

#ifdef MPACT_HAVE_PETSC
  CALL PetscFinalize(ierr)
#else
  CALL mpiTestEnv%finalize()
#endif
!
!===============================================================================
CONTAINS
!
!-------------------------------------------------------------------------------
    SUBROUTINE testInit()
      CALL testEVS%init(mpiTestEnv,optList)
      ASSERT(testEVS%isInit,'%isInit')
      ASSERT(testEVS%n==2,'%n')
      ASSERT(testEVS%maxit==2,'%maxit')
      ASSERT(testEVS%tol==1.0E-8_SRK,'%tol')
      ASSERT(testEVS%SolverMethod==ARNOLDI,'%SolverMethod')
      ASSERT(testEVS%TPLType==SLEPC,'%SolverMethod')
      ASSERT(ASSOCIATED(testEVS%MPIparallelEnv),'%MPIenv')
      ASSERT(testEVS%X%isInit ,'%x')
      ASSERT(testEVS%xi%isInit,'%xi')
    ENDSUBROUTINE testInit
!
!-------------------------------------------------------------------------------
    SUBROUTINE testClear()
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
      ASSERT(.NOT. (testEVS%xi%isInit),'%xi')
    ENDSUBROUTINE testClear
!
!-------------------------------------------------------------------------------
    SUBROUTINE testSetMat()
      CALL plist%clear()
      CALL plist%add('MatrixType->n',2_SIK)
      CALL plist%add('MatrixType->nlocal',2_SIK)
      CALL plist%add('MatrixType->isSym',.FALSE.)
      CALL plist%add('MatrixType->matType',0_SIK)
      CALL plist%add('MatrixType->MPI_COMM_ID',testEVS%MPIparallelEnv%comm)

      CALL A%init(plist)
      CALL B%init(plist)

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

    ENDSUBROUTINE testSetMat
!
!-------------------------------------------------------------------------------
    SUBROUTINE testSolve()
      REAL(SRK) :: kerr
      REAL(SRK) :: flux(2)

      CALL testEVS%solve()
      kerr=testEVS%k - (0.0015_SRK+0.117_SRK*0.325_SRK/0.184_SRK)/0.1208_SRK
      ASSERT(ABS(kerr)<=1.0E-8_SRK,'%solve k')
      FINFO() testEVS%k, kerr

      CALL testEVS%x%get(flux)
      ASSERT(ABS(flux(1)/flux(2)-0.184_SRK/0.117_SRK)<1.0E-8_SRK,'%solve flux')
      FINFO() flux

    ENDSUBROUTINE testSolve
!
!-------------------------------------------------------------------------------
    SUBROUTINE testSetX0()
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

    ENDSUBROUTINE testSetX0
!
!-------------------------------------------------------------------------------
    SUBROUTINE testSetConv()
      CALL testEVS%SetConv(1.0E-10_SRK,100)

      ASSERT(testEVS%tol==1.0E-10_SRK,'%tol')
      ASSERT(testEVS%maxit==100,'%maxit')

    ENDSUBROUTINE testSetConv
!
!-------------------------------------------------------------------------------
    SUBROUTINE testGetResid()
      INTEGER(SIK) :: it=0
      REAL(SRK) :: r=0.0_SRK

      CALL testEVS%getResidual(r,it)

      ASSERT(it>0,'iteration')
      ASSERT(ABS(r)<1.0E-10_SRK,'residual')

    ENDSUBROUTINE testGetResid
!
!-------------------------------------------------------------------------------
!
ENDPROGRAM testEigenvalueSolver
