PROGRAM ben_thing
  USE ISO_C_BINDING
  USE IntrType
  USE Times

  implicit none

  include 'mpif.h'
  include 'store_interface.h'

  INTEGER(C_INT) :: rank, size, n, nlocal, nval, idX, idR, idL, eid, pcid, ierr, i, p, j(3), pctype
  REAL(C_DOUBLE) :: x, a(3)
  TYPE(TimerType) :: Timer

  n=32
  pctype=2
  call MPI_INIT(ierr)
  call MPI_COMM_SIZE(MPI_COMM_WORLD, size, ierr)
  call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)
  CALL MPACT_Trilinos_Init()

  nlocal=n/size

  CALL ForPETRA_VecInit(idX,n,nlocal,MPI_COMM_WORLD)
!!!!!!!!!!!!!!!!!! new code
  CALL ForPETRA_VecSetAll(idX,-1.0_SRK)
  CALL ForPETRA_VECMax(idx,x)
  WRITE(*,*) x
  CALL MPACT_Trilinos_Finalize()
STOP 0
!!!!!!!!!!!!!!!!!!!!!!!
  do i=rank*nlocal+1,(rank+1)*nlocal
    x=real(i)+0.01
    CALL ForPETRA_VecSet(idX,i,x)
  ENDDO
  CALL ForPETRA_VecEdit(idX,"x.vec"//C_NULL_CHAR)

  DO p=0,size-1
    if(rank==p)THEN
      WRITE(*,*) "Proc: ", p
      DO i=rank*nlocal+1,(rank+1)*nlocal
        CALL ForPETRA_VecGet(idX,i,x)
        !WRITE(*,*) i, x
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

  CALL ForPETRA_MatEdit(idL,"LHS.mat"//C_NULL_CHAR)
  CALL ForPETRA_MatEdit(idR,"RHS.mat"//C_NULL_CHAR)
  IF(rank==0) WRITE(*,*) "-------------------------------------------------"
  IF(rank==0) WRITE(*,*)
  CALL Anasazi_Init(eid)
  CALL Preconditioner_Init(pcid,pctype)
  CALL Preconditioner_Setup(pcid,idR)
  CALL Anasazi_SetMat(eid,idR,idL)
  CALL Anasazi_SetX(eid,idX)
  CALL Anasazi_SetPC(eid,pcid)

  CALL Timer%tic()
  CALL Anasazi_Solve(eid)
  CALL Timer%toc()
  IF(rank==0) WRITE(*,*) Timer%elapsedtime

  DO p=0,size-1
    if(rank==p)THEN
      WRITE(*,*) "Proc: ", p
      DO i=rank*nlocal+1,(rank+1)*nlocal
        CALL ForPETRA_VecGet(idX,i,x)
        !WRITE(*,*) i, x
      ENDDO
    ENDIF
    CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)
  ENDDO

  CALL MPACT_Trilinos_Finalize()
ENDPROGRAM ben_thing
