!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
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

! ASSERT_EQ used when values should be exactly equal
#define ASSERT_EQ(val1,val2,msg) ASSERT(val1==val2,msg);FINFO() val1, " Should be ", val2

! ASSERT_APPROXEQ used when values are equal within specified precision
#define ASSERT_APPROXEQ(val1,val2,msg) ASSERT(val1.APPROXEQ.val2,msg); FINFO()val1,val2

! ASSERT_APPROXEQF used to determine bitwise equivalence of reals
#define ASSERT_APPROXEQF(val1,val2,msg) ASSERT(val1.APPROXEQF.val2,msg); FINFO() val1, val2

! ASSERT_APPROXEQR used to determine equivalence of very large or very small numbers
#define ASSERT_APPROXEQR(val1,val2,msg) ASSERT(val1.APPROXEQR.val2,msg); FINFO() val1,val2

! ASSERT_APPROXEQA used for reals where equality is a comparison to defined epsilon 
#define ASSERT_APPROXEQA(val1,val2,msg) ASSERT(val1.APPROXEQA.val2,msg); FINFO()val1,val2

! ASSERT_SOFTEQ used for comparing reals within user defiend tolerance
#define ASSERT_SOFTEQ(val1,val2,tol,msg) ASSERT(SOFTEQ(val1,val2,tol),msg); FINFO()val1,val2,tol

#define ASSERTFAIL(bool,msg)  ASSERT(bool,msg); IF(utest_lastfail) STOP UTEST_FAIL_CODE

#define STAY()  IF(utest_interactive) CALL UTest_Stay()

#define FINFO()  IF(utest_lastfail) WRITE(*,*)

#define INFO(verb)  IF(verb<=utest_verbose) WRITE(*,*)
