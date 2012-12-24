#define CREATE_TEST(name)  CALL UTest_Start(name)

#define FINALIZE_TEST()  CALL UTest_Finalize()

#define REGISTER_SUBTEST(name,subname)  CALL UTest_Start_SubTest(name); CALL subname(); CALL UTest_End_SubTest()

#define COMPONENT_TEST(name)  CALL UTest_Start_Component(name)

#define SET_PREFIX(pfx)  utest_prefix=pfx//" -"

#define SET_INTERACTIVE()  utest_interactive=.TRUE.

#define SET_VERBOSE(verb)  utest_verbose=verb

! Removed __FILE__ from ASSERT because path is too long
#define ASSERT(bool,msg)  CALL UTest_Assert(bool,__LINE__,msg)

#define ASSERTFAIL(bool,msg)  ASSERT(bool,msg); IF(utest_lastfail) STOP __LINE__

#define STAY()  IF(utest_interactive) READ(*,*)

#define FINFO()  IF(utest_lastfail) WRITE(*,*)

#define INFO(verb)  IF(verb<=utest_verbose) WRITE(*,*) 
