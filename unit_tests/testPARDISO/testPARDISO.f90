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
include 'mkl_pardiso.f90'
PROGRAM testPARDISO
  
  USE ExceptionHandler
  USE IntrType
  USE MatrixTypes
  USE VectorTypes
  USE ParameterLists
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
#else
  WRITE(*,*) "PARDISO NOT ENABLED!"
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
#ifdef HAVE_PARDISO
    TYPE(SparseMatrixType) :: thisMatrix
    TYPE(RealVectorType) :: thisx,thisb
    TYPE(ParamType) :: pList
    INTEGER(SIK) :: i
    
    TYPE(MKL_PARDISO_HANDLE):: pt(64)
    INTEGER(SIK) :: maxfct,solver,mnum,mtype,phase,n,nrhs,msglvl,error
    INTEGER(SIK) :: iparm(64)
    INTEGER(SIK),ALLOCATABLE :: perm(:)
    
    DO i = 1, 64
      iparm(i) = 0
      pt(i)%dummy=0
    ENDDO 
    iparm(3)=1
  
    maxfct=1
    mnum=1
    mtype=11
    phase=13
    nrhs=1
    msglvl=0
    error=0
    
    CALL PARDISOINIT(pt,mtype,iparm)
    
!    ! build matrix
!    CALL pList%clear()
!    CALL pList%add('MatrixType->n',2_SNK)
!    CALL pList%add('MatrixType->nnz',2_SNK)
!    CALL thisMatrix%clear()
!    CALL thisMatrix%init(pList)
!    CALL thisMatrix%setShape(1,1,1.0_SRK)
!    CALL thisMatrix%setShape(2,2,1.0_SRK)
!    WRITE(*,*) thisMatrix%ia
!    WRITE(*,*) thisMatrix%ja
!    WRITE(*,*) thisMatrix%a
!    
!    CALL pList%clear()
!    CALL pList%add('VectorType->n',2)
!    CALL thisb%init(pList)
!    WRITE(*,*) thisb%b
!    
!    CALL pList%clear()
!    CALL pList%add('VectorType->n',2)
!    CALL thisx%init(pList)
!    thisx%b=1
!    WRITE(*,*) thisx%b
!    
!    n=thisb%n
!    ALLOCATE(perm(n))
!    perm=1
!
!    WRITE(*,*)
!    WRITE(*,*) maxfct
!    WRITE(*,*) mnum
!    WRITE(*,*) mtype
!    WRITE(*,*) phase
!    WRITE(*,*) n
!    WRITE(*,*) perm
!    WRITE(*,*) nrhs
!    WRITE(*,*) iparm
!    WRITE(*,*) msglvl
!    WRITE(*,*)
!     
!    !CALL PARDISO(pt,maxfct,mnum,mtype,phase,n,thisMatrix%a,thisMatrix%ia, &
!    !  thisMatrix%ja,perm,nrhs,iparm,msglvl,thisb%b,thisx%b,error)
!      
!    WRITE(*,*) thisx%b
!
#endif
    ENDSUBROUTINE testPARDISOInterface

ENDPROGRAM
