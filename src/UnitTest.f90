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
  PUBLIC :: UTest_Assert
  PUBLIC :: utest_lastfail
  PUBLIC :: utest_interactive
!
! List of global variables
  CHARACTER(LEN=20) :: utest_testname
  CHARACTER(LEN=20) :: utest_subtestname
  LOGICAL :: utest_interactive
  LOGICAL :: utest_lastfail
  LOGICAL :: utest_failure
  INTEGER :: utest_verbose
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
      
      utest_testname=testname
      WRITE(*,*) '==================================================='
      WRITE(*,*) 'STARTING TEST: '//TRIM(utest_testname)//'...'
      WRITE(*,*) '==================================================='
      
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
      
      IF (.NOT. utest_failure) THEN
        passfail="PASSED"
      ELSE
        passfail="FAILED"
      ENDIF
      
      WRITE(*,*) '==================================================='
      WRITE(*,*) 'TEST '//utest_testname//' '//passfail
      WRITE(*,*) '==================================================='
      WRITE(*,*) '|                 TEST STATISTICS                 |'
      WRITE(*,*) '|-------------------------------------------------|'
      WRITE(*,*) '|  SUBTEST NAME      |  PASS   |  FAIL  |  TOTAL  |'
      WRITE(*,*) '|-------------------------------------------------|'
      WRITE(*,*) '|                    |         |        |         |'
      WRITE(*,*) '|                    |         |        |         |'
      WRITE(*,*) '==================================================='
      
    ENDSUBROUTINE UTest_Finalize
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
      ELSE
        utest_lastfail=.TRUE.
        WRITE(*,'(A,A,I0,A,A,A)') TRIM(name),'|Line:',line,' - ', msg, '  FAILED'
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
