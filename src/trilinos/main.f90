PROGRAM ben_thing
  USE ISO_C_BINDING

  implicit none

  include 'mpif.h'
  include 'store_interface.h'

  INTEGER(C_INT) :: rank, size, n, nlocal, nval, idX, idR, idL, eid, pcid, ierr, i, p, j(3)
  REAL(C_DOUBLE) :: x, a(3)

  n=128
  call MPI_INIT(ierr)
  call MPI_COMM_SIZE(MPI_COMM_WORLD, size, ierr)
  call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)
  CALL MPACT_Trilinos_Init()

  nlocal=n/size

  CALL ForPETRA_VecInit(idX,n,nlocal,MPI_COMM_WORLD)

  do i=rank*nlocal+1,(rank+1)*nlocal
    x=real(i)+0.01
    CALL ForPETRA_VecSet(idX,i,x)
  ENDDO
  CALL ForPETRA_VecEdit(idX)

  DO p=0,size-1
    if(rank==p)THEN
      WRITE(*,*) "Proc: ", p
      DO i=rank*nlocal+1,(rank+1)*nlocal
        CALL ForPETRA_VecGet(idX,i,x)
        WRITE(*,*) i, x
      ENDDO
    ENDIF
    CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)
  ENDDO

  CALL ForPETRA_MatInit(idL,n,nlocal,3,MPI_COMM_WORLD)
  CALL ForPETRA_MatInit(idR,n,nlocal,3,MPI_COMM_WORLD)

  do i=rank*nlocal+1,(rank+1)*nlocal
    if(i==1) THEN
      j(1)=1; j(2)=2
      a(1)=2.0; a(2)=-1.0
      nval=2
    elseif(i==n) THEN
      a(1)=-1.0; a(2)=2.0
      j(1)=n-1; j(2)=n
      nval=2
    else
      j(1)=i-1; j(2)=i; j(3)=i+1
      a(1)=-1.0; a(2)=2.0; a(3)=-1.0
      nval=3
    endif
    CALL ForPETRA_MatSet(idR,i,nval,j,a)
  ENDDO
  CALL ForPETRA_MatAssemble(idR)
  CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)
  a=1.
  DO i=rank*nlocal+1,(rank+1)*nlocal
    j=i
    CALL ForPETRA_MatSet(idL,i,1,j,a)
  ENDDO
  CALL ForPETRA_MatAssemble(idL)
  CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)
  CALL ForPETRA_MatNormF(idL,x)
  IF(rank==0) WRITE(*,*) x, sqrt(REAL(n))
  CALL ForPETRA_MatNormF(idR,x)
  IF(rank==0) WRITE(*,*) x, sqrt(REAL(4*n+2*(n-1)))
  CALL ForPETRA_MatEdit(idL)
!  CALL ForPETRA_MatEdit(idR)
  IF(rank==0) WRITE(*,*) "-------------------------------------------------"
  IF(rank==0) WRITE(*,*)
  CALL Anasazi_Init(eid)
  CALL Preconditioner_Init(pcid)
  CALL Preconditioner_Setup(pcid,idR)
  CALL Anasazi_SetMat(eid,idR,idL)
  CALL Anasazi_SetX(eid,idX)
  CALL Anasazi_SetPC(eid,pcid)

  CALL Anasazi_Solve(eid)

  DO p=0,size-1
    if(rank==p)THEN
      WRITE(*,*) "Proc: ", p
      DO i=rank*nlocal+1,(rank+1)*nlocal
        CALL ForPETRA_VecGet(idX,i,x)
        WRITE(*,*) i, x
      ENDDO
    ENDIF
    CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)
  ENDDO

  CALL MPACT_Trilinos_Finalize()
ENDPROGRAM ben_thing
