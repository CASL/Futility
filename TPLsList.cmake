#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
#                          Futility Development Group                          !
#                             All rights reserved.                             !
#                                                                              !
# Futility is a jointly-maintained, open-source project between the University !
# of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
# can be found in LICENSE.txt in the head directory of this repository.        !
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!

SET(Futility_TPLS_FINDMODS_CLASSIFICATIONS
       MPI      "${${PROJECT_NAME}_TRIBITS_DIR}/core/std_tpls/" SS
       BLAS     "${Futility_SOURCE_DIR}/cmake/tpl/"             SS
       PARDISO  "${Futility_SOURCE_DIR}/cmake/tpl/"             SS
       HDF5     "${Futility_SOURCE_DIR}/cmake/tpl/"             SS
       HYPRE    "${Futility_SOURCE_DIR}/cmake/tpl/"             SS
       PETSC    "${Futility_SOURCE_DIR}/cmake/tpl/"             SS
       PAPI     "${Futility_SOURCE_DIR}/cmake/tpl/"             SS
       SLEPC    "${Futility_SOURCE_DIR}/cmake/tpl/"             SS
       SUNDIALS "${Futility_SOURCE_DIR}/cmake/tpl/"             SS
  )
