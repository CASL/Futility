!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!

#ifdef FUTILITY_DBC
#define DBC_REQUIRE(test)  IF(.NOT. (test)) CALL DBC_FAIL("test",modName,__LINE__)
#define DBC_ENSURE(test)   IF(.NOT. (test)) CALL DBC_FAIL("test",modName,__LINE__)
#define DBC_REQUIRE_MSG(test,msg)  IF(.NOT. (test)) CALL DBC_FAIL("test",msg,__LINE__)
#define DBC_ENSURE_MSG(test,msg)   IF(.NOT. (test)) CALL DBC_FAIL("test",msg,__LINE__)
#else
#define DBC_REQUIRE(test)  ! DBC REQUIRE - test
#define DBC_ENSURE(test)   ! DBC ENSURE  - test
#define DBC_REQUIRE_MSG(test,msg)  ! DBC REQUIRE - test, msg
#define DBC_ENSURE_MSG(test,msg)   ! DBC ENSURE  - test, msg
#endif
