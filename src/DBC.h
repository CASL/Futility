!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!

#ifdef FUTILITY_DBC
#define REQUIRE(test)  IF(.NOT. (test)) CALL DBC_FAIL("test",__FILE__,__LINE__)
#define ENSURE(test)   IF(.NOT. (test)) CALL DBC_FAIL("test",__FILE__,__LINE__)
  USE DBC
#else
#define REQUIRE(test)  ! DBC REQUIRE - test
#define ENSURE(test)   ! DBC ENSURE  - test
#endif
