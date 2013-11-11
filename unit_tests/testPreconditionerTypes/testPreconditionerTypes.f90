!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                              Copyright (C) 2012                              !
!                   The Regents of the University of Michigan                  !
!              MPACT Development Group and Prof. Thomas J. Downar              !
!                             All rights reserved.                             !
!                                                                              !
! Copyright is reserved to the University of Michigan for purposes of          !
! controlled dissemination, commercialization through formal licensing, or     !
! other disposition. The University of Michigan nor any of their employees,    !
! makes any warranty, express or implied, or assumes any liability or          !
! responsibility for the accuracy, completeness, or usefulness of any          !
! information, apparatus, product, or process disclosed, or represents that    !
! its use would not infringe privately owned rights. Reference herein to any   !
! specific commercial products, process, or service by trade name, trademark,  !
! manufacturer, or otherwise, does not necessarily constitute or imply its     !
! endorsement, recommendation, or favoring by the University of Michigan.      !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
PROGRAM testMatrixTypes
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
  
#ifdef MPACT_HAVE_PETSC
#include <finclude/petsc.h>
#undef IS
  PetscErrorCode  :: ierr
#endif
  
  TYPE(ExceptionHandlerType),TARGET :: e
  TYPE(ParamType) :: PListMat,PListVec
  CLASS(MatrixType),ALLOCATABLE :: testMatrix
  CLASS(VectorType),ALLOCATABLE :: testVector

#ifdef HAVE_MPI
  INTEGER :: mpierr
  CALL MPI_Init(mpierr)
#else
  INTEGER :: MPI_COMM_WORLD=0
#endif
  !Configure exception handler for test
  CALL e%setStopOnError(.TRUE.)
  CALL e%setQuietMode(.FALSE.)
  eParams => e
  ePreCondType => e
  
  
#ifdef MPACT_HAVE_PETSC    
  CALL PetscInitialize(PETSC_NULL_CHARACTER,ierr)
#endif

  CREATE_TEST('Test Preconditioner Types')

  CALL setupILUTest()
  REGISTER_SUBTEST('Test ILU Preconditioner Type',testILU_PreCondType)
  CALL clearTest()
  
  CALL setupBILUTest()
  REGISTER_SUBTEST('Test BILU Preconditioner Type',testBILU_PreCondType)
  CALL clearTest()  
  
  FINALIZE_TEST()
  
#ifdef MPACT_HAVE_PETSC    
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

      !Set up Matrix
      CALL PListMat%add('MatrixType->nnz',40) ! Will be a 10x10, 5-stripe matrix
      CALL PListMat%add('MatrixType->n',10)
      ALLOCATE(SparseMatrixType :: testMatrix)
      CALL testMatrix%init(PListMat)

      ! Fill Matrix
      SELECTTYPE(testMatrix); TYPE IS(SparseMatrixType)
        tmpreal=0.0_SRK
        DO i=1,10
          IF(i >= 5) THEN
            tmpreal=tmpreal+1.0_SRK
            CALL testMatrix%setShape(i,i-4,tmpreal)
          ENDIF
          IF(i >= 2) THEN
            tmpreal=tmpreal+1.0_SRK
            CALL testMatrix%setShape(i,i-1,tmpreal)
          ENDIF
          tmpreal=tmpreal+1.0_SRK
          CALL testMatrix%setShape(i,i,tmpreal)
          IF(i <= 9) THEN
            tmpreal=tmpreal+1.0_SRK
            CALL testMatrix%setShape(i,i+1,tmpreal)
          ENDIF
          IF(i <= 6) THEN
            tmpreal=tmpreal+1.0_SRK
            CALL testMatrix%setShape(i,i+4,tmpreal)
          ENDIF
          CALL testVector%set(i,REAL(i*1.0_SRK,SRK))
        ENDDO
      CLASS DEFAULT
        ASSERT(.FALSE.,'ALLOCATE(SparseMatrixType :: testMatrix)')
      ENDSELECT

    ENDSUBROUTINE setupILUTest
!
!-------------------------------------------------------------------------------
    SUBROUTINE testILU_PreCondType()
      CLASS(LU_PreCondType),ALLOCATABLE :: testLU

      IF(testMatrix%isInit .AND. testVector%isInit) THEN
        COMPONENT_TEST('ILU Preconditioner Type')
        ALLOCATE(ILU_PreCondType :: testLU)

        ! Check %init
        CALL testLU%init(testMatrix)
        ASSERT(testLU%isInit,'ILU Preconditioner %isInit')
        ASSERT(ASSOCIATED(testLU%A),'ILU Preconditioner ASSOCIATED(LU%A)')

        ! Check %setup
        CALL testLU%setup()
        ASSERT(testLU%L%isInit,'ILU Preconditioner %L%isInit')
        ASSERT(testLU%U%isInit,'ILU Preconditioner %U%isInit')
        ! Check L
        ! Check U
        
        ! Check %apply
        CALL testLU%apply(testVector)

        ! Check %clear
        CALL testLU%clear()
        ASSERT(.NOT.(testLU%isInit),'ILU Preconditioner .NOT.(lu%isInit)')
        ASSERT(.NOT.(ASSOCIATED(testLU%A)),'ILU Preconditioner .NOT.(ASSOCIATED(LU%A))')
        ASSERT(.NOT.(ASSOCIATED(testLU%L)),'ILU Preconditioner .NOT.(ASSOCIATED(LU%L))')
        ASSERT(.NOT.(ASSOCIATED(testLU%U)),'ILU Preconditioner .NOT.(ASSOCIATED(LU%U))')
      ELSE
        ASSERT(testMatrix%isInit,'TestMatrix Initialization')
        ASSERT(testVector%isInit,'TestVector Initialization')
      ENDIF

      DEALLOCATE(testLU)

    ENDSUBROUTINE testILU_PreCondtype
!
!-------------------------------------------------------------------------------
    SUBROUTINE setupBILUTest()
      INTEGER(SIK) :: i
      REAL(SRK) :: tmpreal

      !Set up Vector
      CALL PListVec%add('VectorType->n',10_SIK)
      ALLOCATE(RealVectorType :: testVector)
      CALL testVector%init(PListVec)      

      !Set up Matrix
      CALL PListMat%add('MatrixType->nnz',40) ! Will be a 10x10, 5-stripe matrix
      CALL PListMat%add('MatrixType->n',10)
      ALLOCATE(SparseMatrixType :: testMatrix)
      CALL testMatrix%init(PListMat)

      ! Fill Matrix
      SELECTTYPE(testMatrix); TYPE IS(SparseMatrixType)
        tmpreal=0.0_SRK
        DO i=1,10
          IF(i >= 5) THEN
            tmpreal=tmpreal+1.0_SRK
            CALL testMatrix%setShape(i,i-4,tmpreal)
          ENDIF
          IF(i >= 2) THEN
            tmpreal=tmpreal+1.0_SRK
            CALL testMatrix%setShape(i,i-1,tmpreal)
          ENDIF
          tmpreal=tmpreal+1.0_SRK
          CALL testMatrix%setShape(i,i,tmpreal)
          IF(i <= 9) THEN
            tmpreal=tmpreal+1.0_SRK
            CALL testMatrix%setShape(i,i+1,tmpreal)
          ENDIF
          IF(i <= 6) THEN
            tmpreal=tmpreal+1.0_SRK
            CALL testMatrix%setShape(i,i+4,tmpreal)
          ENDIF
          CALL testVector%set(i,REAL(i*1.0_SRK,SRK))
        ENDDO
      CLASS DEFAULT
        ASSERT(.FALSE.,'ALLOCATE(SparseMatrixType :: testMatrix)')
      ENDSELECT

    ENDSUBROUTINE setupBILUTest
!
!-------------------------------------------------------------------------------
    SUBROUTINE testBILU_PreCondType()
      CLASS(LU_PreCondType),ALLOCATABLE :: testLU

      IF(testMatrix%isInit .AND. testVector%isInit) THEN
        COMPONENT_TEST('BILU Preconditioner Type')
        ALLOCATE(BILU_PreCondType :: testLU)

        ! Check %init
        CALL testLU%init(testMatrix)
        ASSERT(testLU%isInit,'BILU Preconditioner %isInit')
        ASSERT(ASSOCIATED(testLU%A),'BILU Preconditioner ASSOCIATED(LU%A)')

        ! Check %setup
        CALL testLU%setup()
        ASSERT(testLU%L%isInit,'BILU Preconditioner %L%isInit')
        ASSERT(testLU%U%isInit,'BILU Preconditioner %U%isInit')
        ! Check L
        ! Check U
        
        ! Check %apply
        CALL testLU%apply(testVector)

        ! Check %clear
        CALL testLU%clear()
        ASSERT(.NOT.(testLU%isInit),'BILU Preconditioner .NOT.(lu%isInit)')
        ASSERT(.NOT.(ASSOCIATED(testLU%A)),'BILU Preconditioner .NOT.(ASSOCIATED(LU%A))')
        ASSERT(.NOT.(ASSOCIATED(testLU%L)),'BILU Preconditioner .NOT.(ASSOCIATED(LU%L))')
        ASSERT(.NOT.(ASSOCIATED(testLU%U)),'BILU Preconditioner .NOT.(ASSOCIATED(LU%U))')
      ELSE
        ASSERT(testMatrix%isInit,'TestMatrix Initialization')
        ASSERT(testVector%isInit,'TestVector Initialization')
      ENDIF

      DEALLOCATE(testLU)

    ENDSUBROUTINE testBILU_PreCondtype
!
!-------------------------------------------------------------------------------
    SUBROUTINE clearTest()

      CALL PListMat%clear()
      CALL PListVec%clear()
      CALL MatrixTypes_Clear_ValidParams()
      CALL testMatrix%clear()
      CALL testVector%clear()
      IF(ALLOCATED(testMatrix)) DEALLOCATE(testMatrix)
      IF(ALLOCATED(testVector)) DEALLOCATE(testVector)

    ENDSUBROUTINE clearTest
ENDPROGRAM testMatrixTypes
