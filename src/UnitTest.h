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
#define ASSERT_EQ(b,a,msg) ASSERT(b==a,msg);FINFO() b, "Not =", a

! ASSERT_LE used when b <= a
#define ASSERT_LE(b,a,msg) ASSERT(b.LE.a,msg);FINFO() b, "Not <= ", a

! ASSERT_LT used when b < a
#define ASSERT_LT(b,a,msg) ASSERT(b.LT.a,msg);FINFO() b, "Not < ", a

! ASSERT_GE used when b >= a
#define ASSERT_GE(b,a,msg) ASSERT(b.GE.a,msg);FINFO() b, "Not >= ", a

! ASSERT_GT used when b > a
#define ASSERT_GT(b,a,msg) ASSERT(b.GT.a,msg);FINFO() b, "Not >", a

! ASSERT_APPROXEQ used when values are equal within specified precision
#define ASSERT_APPROXEQ(b,a,msg) ASSERT(b.APPROXEQ.a,msg); FINFO()b,"/=",a

! ASSERT_APPROXLE used when values are equal within specified precision
#define ASSERT_APPROXLE(b,a,msg) ASSERT(b.APPROXLE.a,msg); FINFO()b,"Not <=",a

! ASSERT_APPROXGE used when values are equal within specified precision
#define ASSERT_APPROXGE(b,a,msg) ASSERT(b.APPROXGE.a,msg); FINFO()b,"Not >=",a

! ASSERT_APPROXEQF used to determine bitwise equivalence of reals
#define ASSERT_APPROXEQF(b,a,msg) ASSERT(b.APPROXEQF.a,msg); FINFO() b,"/=", a

! ASSERT_APPROXEQR used to determine equivalence of very large or very small numbers
#define ASSERT_APPROXEQR(b,a,msg) ASSERT(b.APPROXEQR.a,msg); FINFO() b,"/=",a

! ASSERT_APPROXEQA used for reals where equality is a comparison to defined epsilon 
#define ASSERT_APPROXEQA(b,a,msg) ASSERT(b.APPROXEQA.a,msg); FINFO()b,"/=",a

! ASSERT_SOFTEQ  and ASSERT_SOFTEQR used for comparing reals within user defiend tolerance
#define ASSERT_SOFTEQ(b,a,c,msg) ASSERT(SOFTEQ(b,a,c),msg); FINFO()b,"/=",a,c
#define ASSERT_SOFTEQR(b,a,c,msg) ASSERT(SOFTEQR(b,a,c),msg); FINFO()b,"/=",a,c

#define ASSERTFAIL(bool,msg)  ASSERT(bool,msg); IF(utest_lastfail) STOP UTEST_FAIL_CODE

#define STAY()  IF(utest_interactive) CALL UTest_Stay()

#define FINFO()  IF(utest_lastfail) WRITE(*,*)

#define INFO(verb)  IF(verb<=utest_verbose) WRITE(*,*)
