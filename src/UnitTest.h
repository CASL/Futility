#define CREATE_TEST(name)  CALL UTest_Start(name)

#define FINALIZE_TEST()  CALL UTest_Finalize()

#define ASSERT(bool,msg)  CALL UTest_Assert(bool,__FILE__,__LINE__,msg)

#define ASSERTFAIL(bool,msg)  ASSERT(bool,msg); IF(utest_lastfail) STOP __LINE__

#define STAY()  IF(utest_interactive) READ(*,*)

