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
  
  TYPE(ExceptionHandlerType),TARGET :: e
  TYPE(ParamType) :: pList,optListMat,vecPList

#ifdef HAVE_MPI
  INCLUDE 'mpif.h'
  INTEGER :: mpierr
  CALL MPI_Init(mpierr)
#else
  INTEGER :: MPI_COMM_WORLD=0
#endif
#ifdef MPACT_HAVE_PETSC
#include <finclude/petsc.h>
#undef IS
  PetscErrorCode  :: ierr
#endif
  
  !Configure exception handler for test
  CALL e%setStopOnError(.FALSE.)
  CALL e%setQuietMode(.TRUE.)
  eParams => e
  ePreCondType => e
  
  !Set up optional PL
  CALL optListMat%add('MatrixType->nnz',-1_SNK)
  CALL optListMat%add('MatrixType->isSym',.FALSE.)
  CALL optListMat%add('MatrixType->matType',SPARSE)
  CALL optListMat%add('MatrixType->MPI_Comm_ID',PE_COMM_SELF)
  
  !Set up vector PL
  CALL vecPList%add('VectorType -> n',1)
  CALL vecPList%add('VectorType -> MPI_Comm_ID',PE_COMM_SELF)
  
!#ifdef MPACT_HAVE_PETSC    
!  CALL PetscInitialize(PETSC_NULL_CHARACTER,ierr)
!#endif

  CREATE_TEST('Test Preconditioner Types')

  CALL setupTest()

  FINALIZE_TEST()

  CALL clearTest()  
  
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
    SUBROUTINE setupTest()



    ENDSUBROUTINE setupTest
!
!-------------------------------------------------------------------------------
    SUBROUTINE clearTest()

      CALL optListMat%clear()
      CALL vecPList%clear()
      CALL pList%clear()
      CALL MatrixTypes_Clear_ValidParams()
      CALL VectorType_Clear_ValidParams()

    ENDSUBROUTINE clearTest
ENDPROGRAM testMatrixTypes
