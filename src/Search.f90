!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!> @brief A Fortran 2003 module implementing some basic search algorithms.
!>
!> @par Module Dependencies
!>  - @ref IntrType "IntrType": @copybrief IntrType
!>
!> @author Mitchell Young
!>    @date 3/30/2017
!>
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE Search
  USE IntrType
  USE Strings
  IMPLICIT NONE

  PRIVATE

  PUBLIC :: upper_bound
  PUBLIC :: lower_bound

  !> @brief Generic interface to upper bound
  INTERFACE upper_bound
    !> @copybrief Search::binary_search_1Dint
    !> @copydetails Search::binary_search_1Dint
    MODULE PROCEDURE upper_bound_1Dint
  ENDINTERFACE upper_bound

  !> @brief Generic interface to lower bound
  INTERFACE lower_bound
    !> @copybrief Search::binary_search_1Dint
    !> @copydetails Search::binary_search_1Dint
    MODULE PROCEDURE lower_bound_1Dint
  ENDINTERFACE lower_bound

!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief 
!> @param list the list to earch
!> @param val the value to find
!> @returns the index to the first element of the list that is greater than the
!> passed search value
!>
    PURE FUNCTION upper_bound_1Dint(list,val) RESULT (i)
      INTEGER(SIK),INTENT(IN) :: list(:)
      INTEGER(SIK),INTENT(IN) :: val
      INTEGER(SIK) :: j
      !
      INTEGER(SIK) :: n, step, i

      j=1
      n=SIZE(list)

      DO WHILE(n>0)
        i=j
        step=n/2
        i=i+step
        IF(.NOT. (val < list(i))) THEN
          i=i+1
          j=i
          n=n-step-1
        ELSE
          n=step
        ENDIF
      ENDDO

    ENDFUNCTION upper_bound_1Dint
!
!-------------------------------------------------------------------------------
!> @brief 
!> @param list the list to earch
!> @param val the value to find
!> @returns the index to the first element of the list that is greater than the
!> passed search value
!>
    PURE FUNCTION lower_bound_1Dint(list,val) RESULT(j)
      INTEGER(SIK),INTENT(IN) :: list(:)
      INTEGER(SIK),INTENT(IN) :: val
      INTEGER(SIK) :: j
      !
      INTEGER(SIK) :: n, step, i

      j=1
      n=SIZE(list)

      DO WHILE(n>0)
        i=j
        step = n/2
        i=i+step
        IF(list(i) < val) THEN
          i=i+1
          j=i
          n=n-step-1
        ELSE
          n=step
        ENDIF
      ENDDO

    ENDFUNCTION lower_bound_1Dint

      
ENDMODULE Search
