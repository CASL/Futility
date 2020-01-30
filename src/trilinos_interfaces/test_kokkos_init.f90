!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
PROGRAM test_kokkos_init
  USE ISO_C_BINDING
  USE trilinos_interfaces
  IMPLICIT NONE

  !Initializing Kokkos.
  CALL InitializeKokkos(4,-1)
  !Finalizing Kokkos.
  CALL FinalizeKokkos()

ENDPROGRAM test_kokkos_init
