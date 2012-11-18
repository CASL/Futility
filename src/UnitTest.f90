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
!> @brief description
!>
!>
!> @par
!> @code
!> @endcode
!>
!> @par Module Dependencies
!>   - @ref IntrType "IntrType": @copybrief IntrType
 
!>
!> @author Benjamin Collins
!>    @date 11/17/2012
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE UnitTest
  IMPLICIT NONE
  PRIVATE
!
! List of Public items
  PUBLIC :: UTest_Start
  PUBLIC :: UTest_Finalize
  PUBLIC :: UTest_Register
  PUBLIC :: UTest_Assert
  PUBLIC :: utest_prefix
  PUBLIC :: utest_lastfail
  PUBLIC :: utest_interactive
  PUBLIC :: utest_verbose
  PUBLIC :: utest_inmain
!
! List of global types
  TYPE :: UTestElement
    CHARACTER(LEN=20) :: subtestname
    INTEGER :: nfail=0
    INTEGER :: npass=0
    TYPE(UTestElement),POINTER :: next=>NULL()
  ENDTYPE UTestElement
!
! List of global variables
  CHARACTER(LEN=20) :: utest_testname
  CHARACTER(LEN=20) :: utest_subtestname
  CHARACTER(LEN=20) :: utest_prefix
  LOGICAL :: utest_interactive
  LOGICAL :: utest_lastfail
  LOGICAL :: utest_inmain
  INTEGER :: utest_verbose
  TYPE(UTestElement),POINTER :: utest_firsttest=>NULL()
  TYPE(UTestElement),POINTER :: utest_curtest=>NULL()
!
!===============================================================================
  CONTAINS
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
      
      utest_testname=testname
      WRITE(*,*) '==================================================='
      WRITE(*,*) 'STARTING TEST: '//TRIM(utest_testname)//'...'
      WRITE(*,*) '==================================================='
      
      utest_inmain=.TRUE.
      ALLOCATE(tmp)
      tmp%subtestname="main"
      utest_firsttest=>tmp
      utest_curtest=>tmp
      
      utest_interactive=.FALSE.
      utest_verbose=0
      
      ! some initialization stuff
    ENDSUBROUTINE UTest_Start
!
!-------------------------------------------------------------------------------
!> @brief description
!> @param parameter    description
!>
!> description
!>
    SUBROUTINE UTest_Finalize()
      CHARACTER(LEN=6) :: passfail
      INTEGER :: npass=0
      INTEGER :: nfail=0
      TYPE(UTestElement),POINTER :: tmp, tmp1
      
      WRITE(*,"('===================================================')")
      WRITE(*,"('|                 TEST STATISTICS                 |')")
      WRITE(*,"('|-------------------------------------------------|')")
      WRITE(*,"('|  SUBTEST NAME       |  PASS  |  FAIL  |  TOTAL  |')")
      WRITE(*,"('|-------------------------------------------------|')")
      tmp=>utest_firsttest
      DO 
        IF (tmp%npass+tmp%nfail>0) THEN
          WRITE(*,"('| ',A20,'| ',I06,' | ',I06,' | ',I07,' |')") tmp%subtestname,tmp%npass,tmp%nfail,tmp%npass+tmp%nfail
          npass=npass+tmp%npass
          nfail=nfail+tmp%nfail
        ENDIF
        tmp1=>tmp%next
        DEALLOCATE(tmp)
        IF (.NOT. ASSOCIATED(tmp1)) EXIT
        tmp=>tmp1
      ENDDO
      WRITE(*,"('|-------------------------------------------------|')")
      WRITE(*,"('| ',A20,'| ',I06,' | ',I06,' | ',I07,' |')") '       Total        ',npass,nfail,npass+nfail
      WRITE(*,"('===================================================')")
      IF (nfail==0) THEN
        passfail="PASSED"
      ELSE
        passfail="FAILED"
      ENDIF
      
      WRITE(*,"('===================================================')")
      WRITE(*,*)  ' TEST '//utest_testname//' '//passfail
      WRITE(*,"('===================================================')")
      
    ENDSUBROUTINE UTest_Finalize
!
!-------------------------------------------------------------------------------
!> @brief description
!> @param parameter    description
!>
!> description
!>
    SUBROUTINE UTest_Register(subtestname)
      CHARACTER(LEN=*),INTENT(IN) :: subtestname
      TYPE(UTestElement),POINTER :: tmp
      
      ALLOCATE(tmp)
      tmp%subtestname=subtestname
      utest_curtest%next=>tmp
      utest_curtest=>tmp
      
      WRITE(*,*) '  BEGIN TEST '//subtestname
      utest_inmain=.FALSE.
      
    ENDSUBROUTINE UTest_Register
!
!-------------------------------------------------------------------------------
!> @brief description
!> @param parameter    description
!>
!> description
!>
    SUBROUTINE UTest_Assert(bool,file,line,msg)
      LOGICAL,INTENT(IN) :: bool
      CHARACTER(LEN=*),INTENT(IN) :: file
      INTEGER,INTENT(IN) :: line
      CHARACTER(LEN=*),INTENT(IN) :: msg
      
      CHARACTER(LEN=LEN(file)) :: name
      name=trim_path(file)
      
      IF(bool) THEN
        utest_lastfail=.FALSE.
        IF(utest_inmain) THEN
          utest_firsttest%npass=utest_firsttest%npass+1
        ELSE
          utest_curtest%npass=utest_curtest%npass+1
        ENDIF
      ELSE
        utest_lastfail=.TRUE.
        IF(utest_inmain) THEN
          utest_firsttest%nfail=utest_firsttest%nfail+1
        ELSE
          utest_curtest%nfail=utest_curtest%nfail+1
        ENDIF
        WRITE(*,'(A,A,I0,A,A,A)') TRIM(name),'|Line:',line,' - ', TRIM(utest_prefix)//" "//TRIM(msg), '  FAILED'
      ENDIF
      
    ENDSUBROUTINE UTest_Assert
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
        IF(file(i:i)=="\") EXIT
      ENDDO
      name=file(i+1:LEN(file))
    ENDFUNCTION
!
ENDMODULE UnitTest
