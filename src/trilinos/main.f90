PROGRAM ben_thing
  USE ISO_C_BINDING

  implicit none

  include 'mpif.h'
  include 'store_interface.h'

  INTEGER(C_INT) :: id, ierr, i
  REAL(C_DOUBLE) :: x

  call MPI_INIT(ierr)
  CALL MPACT_Trilinos_Init()

  id=5

  CALL ForPETRA_VecInit(id,10,10,MPI_COMM_WORLD)
  WRITE(*,*)id

  CALL ForPETRA_VecInit(id,15,15,MPI_COMM_WORLD)
  WRITE(*,*)id

  CALL ForPETRA_VecInit(id,20,20,MPI_COMM_WORLD)
  WRITE(*,*)id

  x=1.01
  CALL ForPETRA_VecSet(0,1,x)
  x=3.01
  CALL ForPETRA_VecSet(0,3,x)
  x=5.01
  CALL ForPETRA_VecSet(0,5,x)
  x=7.01
  CALL ForPETRA_VecSet(0,7,x)
  x=9.01
  CALL ForPETRA_VecSet(0,9,x)
  !CALL ForPETRA_VecEdit(0)
  x=0.0
  DO i=1,10
    CALL ForPETRA_VecGet(0,i,x)
    WRITE(*,*) i, x
  ENDDO

  CALL ForPETRA_MatInit(id,10,10,3,MPI_COMM_WORLD)
  WRITE(*,*)id

  CALL ForPETRA_MatInit(id,15,15,3,MPI_COMM_WORLD)
  WRITE(*,*)id

  CALL ForPETRA_MatInit(id,20,20,3,MPI_COMM_WORLD)
  WRITE(*,*)id


  CALL MPACT_Trilinos_Finalize()
ENDPROGRAM ben_thing
