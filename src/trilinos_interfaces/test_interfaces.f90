!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
PROGRAM test_interfaces
  USE ISO_C_BINDING
  IMPLICIT NONE

#ifdef HAVE_MPI
#ifdef MPACT_HAVE_TRILINOS
  INCLUDE 'mpif.h'
#include "trilinos_f_interfaces.h"

  INTEGER :: mpierr
  INTEGER :: nproc
  INTEGER :: rank
  INTEGER :: n=16
  INTEGER :: nlocal
  INTEGER :: xid, i
  INTEGER,ALLOCATABLE :: gids(:)
  REAL(8) :: tmp

  CALL MPI_Init(mpierr)
  CALL MPI_Comm_size(MPI_COMM_WORLD,nproc,mpierr)
  CALL MPI_Comm_rank(MPI_COMM_WORLD,rank,mpierr)

  nlocal=n/nproc

  CALL ForPetra_VecInit(xid,n,nlocal,MPI_COMM_WORLD)

  IF(rank>0 .AND. rank <nproc-1) THEN
    ALLOCATE(gids(nlocal+2))
    DO i=0,nlocal+1
      gids(i+1)=rank*nlocal+i
    ENDDO
  ELSEIF(rank==0 .AND. nproc>1) THEN
    ALLOCATE(gids(nlocal+1))
    DO i=1,nlocal+1
      gids(i)=i
    ENDDO
  ELSEIF(rank==nproc-1 .AND. nproc>1) THEN
    ALLOCATE(gids(nlocal+1))
    DO i=0,nlocal
      gids(i+1)=rank*nlocal+i
    ENDDO
  ELSE
    ALLOCATE(gids(nlocal))
    DO i=1,nlocal
      gids(i)=i
    ENDDO
  ENDIF
  CALL ForPETRA_VecSetImportMap(xid,size(gids),gids)


  DO i=rank*nlocal+1,(rank+1)*nlocal
    CALL ForPetra_VecSet(xid,i,REAL(rank+1,C_DOUBLE))
  ENDDO

  CALL ForPETRA_VecTransfer(xid)

  DO i=1,n
    tmp=0.0
    CALL ForPetra_VecGet(xid,i,tmp)
    WRITE(*,*) rank, i, tmp
  ENDDO

  CALL ForPetra_VecDestroy(xid)

  CALL MPI_Finalize(mpierr)
#endif
#endif
ENDPROGRAM test_interfaces
