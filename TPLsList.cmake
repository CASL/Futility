#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
#                          Futility Development Group                          !
#                             All rights reserved.                             !
#                                                                              !
# Futility is a jointly-maintained, open-source project between the University !
# of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
# can be found in LICENSE.txt in the head directory of this repository.        !
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!

IF (${PROJECT_NAME}_TRILINOS_TPL)
  SET(TRILINOS_PACKAGE_TPLS_LIST
    Tpetra        TRIBITS_PKG   SS
    MueLu         TRIBITS_PKG   SS
    Belos         TRIBITS_PKG   SS
    Anasazi       TRIBITS_PKG   SS
    Ifpack2       TRIBITS_PKG   SS
    #NOX           TRIBITS_PKG   SS
    #Rythmos       TRIBITS_PKG   SS
    CTeuchos      TRIBITS_PKG   SS
    ForTeuchos    TRIBITS_PKG   SS
    )
ELSE()
  SET(TRILINOS_PACKAGE_TPLS_LIST)
ENDIF()

TRIBITS_REPOSITORY_DEFINE_TPLS(
  MPI      "${${PROJECT_NAME}_TRIBITS_DIR}/core/std_tpls/" SS
  BLAS     "${Futility_SOURCE_DIR}/cmake/tpl/"             SS
  PARDISO  "${Futility_SOURCE_DIR}/cmake/tpl/"             SS
  HDF5     "${Futility_SOURCE_DIR}/cmake/tpl/"             SS
  HYPRE    "${Futility_SOURCE_DIR}/cmake/tpl/"             SS
  PETSC    "${Futility_SOURCE_DIR}/cmake/tpl/"             SS
  PAPI     "${Futility_SOURCE_DIR}/cmake/tpl/"             SS
  SLEPC    "${Futility_SOURCE_DIR}/cmake/tpl/"             SS
  SUNDIALS "${Futility_SOURCE_DIR}/cmake/tpl/"             SS
  ${TRILINOS_PACKAGE_TPLS_LIST}
  )
