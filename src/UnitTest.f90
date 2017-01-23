!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!> @brief description
!>
!>
!> @par
!> @code
!> @endcode
!>
!>
!> @author Benjamin Collins
!>    @date 11/17/2012
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE UnitTest
#include "UnitTest.h"
  IMPLICIT NONE

#ifdef HAVE_MPI
  INCLUDE "mpif.h"
#endif

  PRIVATE

!
! List of Public items
  PUBLIC :: UTest_Start
  PUBLIC :: UTest_Finalize
  PUBLIC :: UTest_Start_SubTest
  PUBLIC :: UTest_End_SubTest
  PUBLIC :: UTest_Start_Component
  PUBLIC :: UTest_End_Component
  PUBLIC :: UTest_Assert
  PUBLIC :: UTest_Stay
  PUBLIC :: UTest_setpfx
  PUBLIC :: utest_prefix
  PUBLIC :: utest_lastfail
  PUBLIC :: utest_interactive
  PUBLIC :: utest_verbose
  PUBLIC :: utest_nfail
  PUBLIC :: utest_inmain
  PUBLIC :: utest_master
!
! List of global types
  TYPE :: UTestElement
    CHARACTER(LEN=60) :: subtestname
    INTEGER :: nfail=0
    INTEGER :: npass=0
    TYPE(UTestElement),POINTER :: next => NULL()
  ENDTYPE UTestElement
!
! List of global variables
  CHARACTER(LEN=60) :: utest_testname
  CHARACTER(LEN=60) :: utest_subtestname
  CHARACTER(LEN=60) :: utest_componentname
  CHARACTER(LEN=60) :: utest_prefix
  LOGICAL :: utest_master=.TRUE.
  LOGICAL :: utest_component=.FALSE.
  LOGICAL :: utest_compfail=.FALSE.
  LOGICAL :: utest_interactive=.FALSE.
  LOGICAL :: utest_lastfail=.FALSE.
  LOGICAL :: utest_inmain=.TRUE.
  INTEGER :: utest_nfail=0
  INTEGER :: utest_verbose
  TYPE(UTestElement),POINTER :: utest_firsttest => NULL()
  TYPE(UTestElement),POINTER :: utest_curtest => NULL()
!
! List of local variables
  CHARACTER(LEN=79),PARAMETER :: utest_hline='============================='// &
    '=================================================='
  CHARACTER(LEN=79),PARAMETER :: utest_pad='                               '// &
    '                                                '
  CHARACTER(LEN=79),PARAMETER :: utest_dot='  .............................'// &
    '................................................'

#ifdef COLOR_LINUX
  CHARACTER(LEN=7),PARAMETER :: c_red=ACHAR(27)//'[31;1m'
  CHARACTER(LEN=7),PARAMETER :: c_grn=ACHAR(27)//'[32;1m'
  CHARACTER(LEN=7),PARAMETER :: c_yel=ACHAR(27)//'[33;1m'
  CHARACTER(LEN=7),PARAMETER :: c_blu=ACHAR(27)//'[34;1m'
  CHARACTER(LEN=4),PARAMETER :: c_nrm=ACHAR(27)//'[0m'
#else
  CHARACTER(LEN=1),PARAMETER :: c_red=ACHAR(0)
  CHARACTER(LEN=1),PARAMETER :: c_grn=ACHAR(0)
  CHARACTER(LEN=1),PARAMETER :: c_yel=ACHAR(0)
  CHARACTER(LEN=1),PARAMETER :: c_blu=ACHAR(0)
  CHARACTER(LEN=1),PARAMETER :: c_nrm=ACHAR(0)
#endif

  !> Variables for MPI tests
#ifdef HAVE_MPI
  INTEGER :: rank,nproc,mpierr,i
  INTEGER :: mpistatus(MPI_STATUS_SIZE)
  LOGICAL :: mpiinit
#endif

  CHARACTER(LEN=80) :: line
  INTEGER :: utest_npfx=0
  INTEGER :: utest_lvl=0
!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief Increase unit test output indentation level
!>
!> Self-explanatory.
!>
    SUBROUTINE UTest_incl()
      utest_lvl=utest_lvl+1
    ENDSUBROUTINE UTest_incl
!
!-------------------------------------------------------------------------------
!> @brief Decrease unit test output indentation level
!>
!> Self-explanatory.
!>
    SUBROUTINE UTest_decl()
      utest_lvl=utest_lvl-1
    ENDSUBROUTINE UTest_decl
!
!-------------------------------------------------------------------------------
!> @brief description
!> @param parameter    description
!>
!> description
!>
    SUBROUTINE UTest_Start(testname)
      CHARACTER(LEN=*),INTENT(IN) :: testname
      TYPE(UTestElement),POINTER :: tmp

!Determine the global MPI parameters
#ifdef HAVE_MPI
      CALL MPI_initialized(mpiinit,mpierr)
      IF(mpierr /= 0) THEN
        WRITE(*,*) 'MPI ERROR: UTest_Start :: MPI_Comm_initialized FAILED'
        STOP
      ENDIF
      IF(mpiinit) THEN
        CALL MPI_Comm_size(MPI_COMM_WORLD,nproc,mpierr)
        IF(mpierr /= 0) THEN
          WRITE(*,*) 'MPI ERROR: UTest_Start :: MPI_Comm_size FAILED'
          STOP
        ENDIF
        CALL MPI_Comm_rank(MPI_COMM_WORLD,rank,mpierr)
        IF(mpierr /= 0) THEN
          WRITE(*,*) 'MPI ERROR: UTest_Start :: MPI_Comm_rank FAILED'
          STOP
        ENDIF
        IF(rank /= 0) utest_master=.FALSE.
      !MPI is not actually being used
      ELSE
        nproc=1
        rank=0
      ENDIF
#endif

      utest_testname=testname
      IF(utest_master) THEN
        WRITE(*,'(A)') utest_hline
        WRITE(*,*) 'STARTING TEST: '//TRIM(utest_testname)//'...'
        WRITE(*,'(A)') utest_hline
      ENDIF

      utest_inmain=.TRUE.
      ALLOCATE(tmp)
      tmp%subtestname='main'
      utest_firsttest => tmp
      utest_curtest => tmp

      utest_interactive=.FALSE.
      utest_verbose=0
    ENDSUBROUTINE UTest_Start
!
!-------------------------------------------------------------------------------
!> @brief description
!> @param parameter    description
!>
!> description
!>
    SUBROUTINE UTest_Finalize()
      CHARACTER(LEN=17) :: passfail
      INTEGER :: npass=0
      INTEGER :: nfail=0
      INTEGER :: sendbuf(2),recvbuf(2)
      CHARACTER(LEN=79) :: sendcharbuf,recvcharbuf,subtest_stats
      TYPE(UTestElement),POINTER :: tmp, tmp1

!If this is a parallel test, the test fails if tests on ANY processor failed
#ifdef HAVE_MPI
      !If MPI is not actually being used, then no reduction is necessary
      IF(mpiinit) THEN
        sendbuf(1)=utest_nfail
        CALL MPI_Allreduce(sendbuf(1),recvbuf(1),1,MPI_INTEGER,MPI_SUM,MPI_COMM_WORLD,mpierr)
        IF(mpierr /= 0) THEN
          WRITE(*,*) 'MPI ERROR: UTest_FINALIZE :: MPI_Allreduce FAILED'
          STOP
        ENDIF
        utest_nfail=recvbuf(1)
      ENDIF
#endif

      IF(utest_master) THEN
        IF(utest_nfail > 0) THEN
          passfail=c_red//'FAILED'//c_nrm
        ELSE
          passfail=c_grn//'PASSED'//c_nrm
        ENDIF
      ENDIF

      IF(utest_master) THEN
        WRITE(*,'(A)') utest_hline
        WRITE(*,'(A72,A)')  ' TEST '//utest_testname//utest_pad,passfail
        WRITE(*,'(A)') utest_hline

        WRITE(*,'(A)') utest_hline
        WRITE(*,'(A,31x,A,31x,A)') '|','TEST STATISTICS','|'
        WRITE(*,'(A)') '|--------------------------------------------------'// &
          '---------------------------|'
        WRITE(*,'(A,24x,a)') '|  SUBTEST NAME','|    PASS    |    FAIL    |'// &
          '    TOTAL   |'
        WRITE(*,'(A)') '|--------------------------------------------------'// &
          '---------------------------|'
      ENDIF
      tmp => utest_firsttest

      DO
        IF(tmp%npass+tmp%nfail>0) THEN
#ifdef HAVE_MPI
          !The master prints its stats first
          IF(utest_master) THEN
            WRITE(*,"('| ',A37,'| ',I10,' | ',I10,' | ',I10,' |')") &
              adjustl(tmp%subtestname//"                            "),&
              tmp%npass,tmp%nfail,tmp%npass+tmp%nfail
          ENDIF

          !The other processes send there stats to the master
          !The master then prints them in order
          !If mpi is unitialized or if only 1 process exists, this loop is never
          !executed
          DO i=1,nproc-1
            IF(rank == 0) THEN
              CALL MPI_Recv(recvcharbuf,79,MPI_CHARACTER,i,1,MPI_COMM_WORLD,mpistatus,mpierr)
              IF(mpierr /= 0) THEN
                WRITE(*,*) 'MPI ERROR: UTest_FINALIZE :: MPI_Recv FAILED'
                STOP
              ENDIF
              subtest_stats=recvcharbuf
            ENDIF
            IF(i == rank) THEN
              WRITE(sendcharbuf,"('| ',A37,'| ',I10,' | ',I10,' | ',I10,' |')") &
                adjustl(tmp%subtestname//"                            "),&
                tmp%npass,tmp%nfail,tmp%npass+tmp%nfail
              CALL MPI_Send(sendcharbuf,79,MPI_CHARACTER,0,1,MPI_COMM_WORLD,mpierr)
              IF(mpierr /= 0) THEN
                WRITE(*,*) 'MPI ERROR: UTest_FINALIZE :: MPI_Send FAILED'
                STOP
              ENDIF
            ENDIF
            IF(utest_master) THEN
              WRITE(*,'(A79)') subtest_stats
            ENDIF
          ENDDO
#else
          WRITE(*,"('| ',A37,'| ',I10,' | ',I10,' | ',I10,' |')") &
            adjustl(tmp%subtestname//"                            "),&
            tmp%npass,tmp%nfail,tmp%npass+tmp%nfail
#endif
          npass=npass+tmp%npass
          nfail=nfail+tmp%nfail
        ENDIF
        tmp1 => tmp%next
        DEALLOCATE(tmp)
        IF(.NOT.ASSOCIATED(tmp1)) EXIT
        tmp => tmp1
      ENDDO

!If this is a parallel test, we need the statistics across all processors
#ifdef HAVE_MPI
      !If MPI is not actually being used, then no reduction is necessary
      IF(mpiinit) THEN
        sendbuf(1)=npass
        sendbuf(2)=nfail
        CALL MPI_Reduce(sendbuf,recvbuf,2,MPI_INTEGER,MPI_SUM,0,MPI_COMM_WORLD,mpierr)
        IF(mpierr /= 0) THEN
          WRITE(*,*) 'MPI ERROR: UTest_FINALIZE :: MPI_Reduce FAILED'
          STOP
        ENDIF
        npass=recvbuf(1)
        nfail=recvbuf(2)
      ENDIF
#endif

      IF(utest_master) THEN
        WRITE(*,'(A)') '|--------------------------------------------------'// &
          '---------------------------|'
        WRITE(*,"('| ',A37,'| ',I10,' | ',I10,' | ',I10,' |')") 'Total                ', &
          npass,nfail,npass+nfail
        WRITE(*,'(A)') utest_hline
      ENDIF

      IF(utest_nfail > 0) THEN
        !This statement is not standard and may not be portable.
        !CALL EXIT(utest_nfail)
        STOP UTEST_FAIL_CODE
      ENDIF

      !Possibly call MPI_Finalize here if this is the last thing called in the
      !unit test

    ENDSUBROUTINE UTest_Finalize
!
!-------------------------------------------------------------------------------
!> @brief description
!> @param parameter    description
!>
!> description
!>
    SUBROUTINE UTest_Start_SubTest(subtestname)
      CHARACTER(LEN=*),INTENT(IN) :: subtestname
      TYPE(UTestElement),POINTER :: tmp

      ALLOCATE(tmp)
!If we're using MPI, each processor should have a unique subtest name
      tmp%subtestname=subtestname
#ifdef HAVE_MPI
      IF(nproc == 1) THEN
        tmp%subtestname=subtestname
      ELSE
        WRITE(tmp%subtestname,'(A40,A4,I5)') TRIM(ADJUSTL(subtestname))," PID",rank
      ENDIF
#else
      tmp%subtestname=subtestname
#endif
      utest_curtest%next=>tmp
      utest_curtest=>tmp

      WRITE(*,*)
      WRITE(*,'(A)') utest_pad(1:utest_lvl*2)//'BEGIN SUBTEST '//TRIM(ADJUSTL(tmp%subtestname))

      CALL UTest_incl()
      utest_inmain=.FALSE.
    ENDSUBROUTINE UTest_Start_SubTest
!
!-------------------------------------------------------------------------------
!> @brief description
!> @param parameter    description
!>
!> description
!>
    SUBROUTINE UTest_End_SubTest()
      CHARACTER(LEN=19) :: pfstr

      IF(utest_component) CALL UTest_End_Component()

      CALL UTest_decl()

      IF(utest_curtest%nfail > 0) THEN
        pfstr=c_red//' FAILED'//c_nrm
      ELSE
        pfstr=c_grn//' PASSED'//c_nrm
      ENDIF

      WRITE(*,'(A71,A)') utest_pad(1:utest_lvl*2)//'SUBTEST '// &
        TRIM(ADJUSTL(utest_curtest%subtestname))//utest_dot,pfstr
      WRITE(*,*)
      utest_inmain=.TRUE.

#ifdef HAVE_MPI
      !If MPI is not initialized, we don't need a barrier
      IF(mpiinit) THEN
        CALL MPI_Barrier(MPI_COMM_WORLD,mpierr)
        IF(mpierr /= 0) THEN
          WRITE(*,*) 'MPI ERROR: UTest_End_SubTest :: MPI_Barrier FAILED'
          STOP
        ENDIF
      ENDIF
#endif

    ENDSUBROUTINE UTest_End_SubTest
!
!-------------------------------------------------------------------------------
!> @brief description
!> @param parameter    description
!>
!> description
!>
    SUBROUTINE UTest_Start_Component(componentname)
      CHARACTER(LEN=*),INTENT(IN) :: componentname

      IF(utest_component) CALL UTest_End_Component()

      utest_component=.TRUE.
      utest_compfail=.FALSE.
!If we're using MPI, each processor should have a unique subtest name
#ifdef HAVE_MPI
      IF(nproc == 1) THEN
        utest_componentname=componentname
      ELSE
        WRITE(utest_componentname,'(A40,A4,I5)') TRIM(ADJUSTL(componentname))," PID",rank
      ENDIF
#else
      utest_componentname=componentname
#endif
      utest_prefix=utest_componentname//" -"
      utest_npfx=MIN(LEN(utest_componentname)+3,20)

      WRITE(*,*)
      WRITE(*,'(A)') utest_pad(1:utest_lvl*2)//'BEGIN COMPONENT '// &
        TRIM(ADJUSTL(utest_componentname))

      CALL UTest_incl()

    ENDSUBROUTINE UTest_Start_Component
!
!-------------------------------------------------------------------------------
!> @brief description
!> @param parameter    description
!>
!> description
!>
    SUBROUTINE UTest_End_Component()
      CHARACTER(LEN=19) :: pfstr

      CALL UTest_decl()

      IF(utest_compfail) THEN
        pfstr=c_red//' FAILED'//c_nrm
      ELSE
        pfstr=c_grn//' PASSED'//c_nrm
      ENDIF

      WRITE(*,'(A71,A)') utest_pad(1:utest_lvl*2)//'COMPONENT '// &
        TRIM(ADJUSTL(utest_componentname))//utest_dot,pfstr

      utest_component=.FALSE.
      utest_prefix=''
      utest_npfx=0
    ENDSUBROUTINE UTest_End_Component
!
!-------------------------------------------------------------------------------
!> @brief description
!> @param parameter    description
!>
!> description
!>
    SUBROUTINE UTest_Assert(bool,line,msg)
      LOGICAL,INTENT(IN) :: bool
      INTEGER,INTENT(IN) :: line
      CHARACTER(LEN=*),INTENT(IN) :: msg

      IF(bool) THEN
        utest_lastfail=.FALSE.
        IF(utest_inmain) THEN
          utest_firsttest%npass=utest_firsttest%npass+1
        ELSE
          utest_curtest%npass=utest_curtest%npass+1
        ENDIF
      ELSE
        utest_nfail=utest_nfail+1
        utest_lastfail=.TRUE.
        IF(utest_inmain) THEN
          utest_firsttest%nfail=utest_firsttest%nfail+1
        ELSE
          utest_curtest%nfail=utest_curtest%nfail+1
        ENDIF
        utest_compfail=.TRUE.
        WRITE(*,'(A,I0,A,A)') utest_pad(1:utest_lvl*2)//c_red// &
          'ASSERTION FAILED'//c_nrm//' on line ',line,':'
        WRITE(*,'(A)') utest_pad(1:utest_lvl*2)//utest_prefix(1:utest_npfx)// &
          TRIM(ADJUSTL(msg))
        WRITE(*,*)
      ENDIF
    ENDSUBROUTINE UTest_Assert
!
!-------------------------------------------------------------------------------
!> @brief description
!> @param parameter    description
!>
!> description
!>
    SUBROUTINE UTest_Stay()
      WRITE(*,*) 'Press Return to Continue:'
      READ(*,*)
    ENDSUBROUTINE UTest_Stay
!
!-------------------------------------------------------------------------------
!> @brief description
!> @param parameter    description
!>
!> description
!>
    FUNCTION trim_path(file) RESULT(name)
      CHARACTER(LEN=*),INTENT(IN) :: file
      CHARACTER(LEN=LEN(file)) :: name
      INTEGER :: i

      DO i=LEN(file),1,-1
        IF(file(i:i) == ACHAR(92)) EXIT
      ENDDO
      name=file(i+1:LEN(file))
    ENDFUNCTION
!
!-------------------------------------------------------------------------------
    SUBROUTINE UTest_setpfx(pfx)
      CHARACTER(LEN=*),INTENT(IN) :: pfx

      IF(pfx == '') THEN
        utest_prefix=''
        utest_npfx=0
      ELSE
        utest_prefix=pfx//' - '
        utest_npfx=MIN(LEN(pfx)+3,20)
      ENDIF
    ENDSUBROUTINE UTest_setpfx
!
ENDMODULE UnitTest
