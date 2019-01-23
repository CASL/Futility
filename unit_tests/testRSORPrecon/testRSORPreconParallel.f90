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
  CLASS(MatrixType),ALLOCATABLE :: testSparseMatrix,testDenseMatrix,testMatrix
  CLASS(MatrixType),ALLOCATABLE :: testBandedMatrix
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

    ENDSUBROUTINE setupRSORTest
!
!-------------------------------------------------------------------------------
    SUBROUTINE testRSOR_PreCondType()

    ENDSUBROUTINE testRSOR_PreCondtype
!
!-------------------------------------------------------------------------------
    SUBROUTINE clearTest()
    
    ENDSUBROUTINE clearTest
!
ENDPROGRAM testRSORPreconParallel
