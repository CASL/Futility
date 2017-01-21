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

#define UTEST_FAIL_CODE 666

#define CREATE_TEST(name)  CALL UTest_Start(name)

#define FINALIZE_TEST()  CALL UTest_Finalize()

#define REGISTER_SUBTEST(name,subname)  CALL UTest_Start_SubTest(name); CALL subname; CALL UTest_End_SubTest()

#define COMPONENT_TEST(name)  CALL UTest_Start_Component(name)

#define SET_PREFIX(pfx)  CALL UTest_setpfx(pfx)

#define SET_INTERACTIVE()  utest_interactive=.TRUE.

#define SET_VERBOSE(verb)  utest_verbose=verb

! Removed __FILE__ from ASSERT because path is too long
#define ASSERT(bool,msg)  CALL UTest_Assert(bool,__LINE__,msg)

#define ASSERTFAIL(bool,msg)  ASSERT(bool,msg); IF(utest_lastfail) STOP UTEST_FAIL_CODE

#define STAY()  IF(utest_interactive) CALL UTest_Stay()

#define FINFO()  IF(utest_lastfail) WRITE(*,*)

#define INFO(verb)  IF(verb<=utest_verbose) WRITE(*,*) 
