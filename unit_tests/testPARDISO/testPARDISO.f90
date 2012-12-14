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
include '/opt/intel-12/mkl/include/mkl_pardiso.f90'
PROGRAM testPARDISO
  
  USE ExceptionHandler
  USE IntrType
  USE MatrixTypes
  USE VectorTypes
  USE MKL_PARDISO
  IMPLICIT NONE

  
  TYPE(ExceptionHandlerType),TARGET :: e
  
  !Configure exception handler for test
  CALL e%setStopOnError(.FALSE.)
  CALL e%setQuietMode(.TRUE.)

  WRITE(*,*) '==================================================='
  WRITE(*,*) 'TESTING PARDISO...'
  WRITE(*,*) '==================================================='

#ifdef HAVE_PARDISO
  CALL testPARDISOInterface()
#endif
  
  WRITE(*,*) '==================================================='
  WRITE(*,*) 'TESTING PARDISO PASSED!'
  WRITE(*,*) '==================================================='

!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
    SUBROUTINE testPARDISOInterface()
    TYPE(SparseMatrixType) :: matrix
    TYPE(RealVectorType) :: x,b
    
    INTEGER(SIK) :: pt(64)
    INTEGER(SIK) :: maxfct,solver,mnum,mtype,phase,n,nrhs,error
    INTEGER(SIK) :: perm(64),iparm(64)
    REAL(SRK) :: dparm(64)
    
!    CALL PARDISO_INIT(pt,mtype,solver,iparm,dparm,error)
!    WRITE(*,*) error
      
      !CALL pardiso()

    ENDSUBROUTINE testPARDISOInterface

ENDPROGRAM
