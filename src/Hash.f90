!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!> @brief Provides hashing algorithms
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE HashModule
USE IntrType

IMPLICIT NONE
PRIVATE

PUBLIC :: stringHash

CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief Hashes a string to an integer value
!> @param string the string to hash
!> @param p the rolling polynomial base
!> @param m the modulus
!>
!> This algorithm is based on https://cp-algorithms.com/string/string-hashing.html
!>
!> It allows for ASCII values 32 and up.  Recommended value for p is 97 (a prime
!> number close to 95, which is the number of "normal" ASCII values expected in the
!> 32-126 range).  Recommended value for m is a large prime number such as 10**9 + 9.
!> However, other arguments may be valid for certain applications.
!>
PURE FUNCTION stringHash(string,p,m) RESULT(hash)
  CHARACTER(LEN=*),INTENT(IN) :: string
  INTEGER(SLK),INTENT(IN) :: p
  INTEGER(SLK),INTENT(IN) :: m
  INTEGER(SLK) :: hash
  !
  INTEGER(SIK) :: i
  INTEGER(SLK) :: p_pow

  hash = 0
  p_pow = 1
  DO i=1,LEN(string)
    !1-31 are special characters that we disallow
    hash = MOD(hash + (IACHAR(string(i:i))-31_SIK)*p_pow,m)
    p_pow = MOD(p_pow * p,m)
  ENDDO !i

ENDFUNCTION stringHash
!
ENDMODULE HashModule