#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
#                          Futility Development Group                          !
#                             All rights reserved.                             !
#                                                                              !
# Futility is a jointly-maintained, open-source project between the University !
# of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
# can be found in LICENSE.txt in the head directory of this repository.        !
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!

SET(LIB_OPTIONAL_DEP_PACKAGES_local
  Tpetra
  #NOX
  #Rythmos
  MueLu
  Ifpack2
  Anasazi
  Belos
  CTeuchos
  ForTeuchos
  )

SET(LIB_OPTIONAL_DEP_TPLS_local
 BLAS LAPACK MPI HYPRE PETSC PARDISO HDF5 PAPI SLEPC SUNDIALS
 )

IF (${PROJECT_NAME}_ENABLE_STANDALONE)

  TRIBITS_PACKAGE_DEFINE_DEPENDENCIES(
    LIB_OPTIONAL_TPLS
      ${LIB_OPTIONAL_DEP_PACKAGES_local}
      ${LIB_OPTIONAL_DEP_TPLS_local}
    )

ELSE()

  TRIBITS_PACKAGE_DEFINE_DEPENDENCIES(
    LIB_OPTIONAL_PACKAGES
      ${LIB_OPTIONAL_DEP_PACKAGES_local}
    LIB_OPTIONAL_TPLS
      ${LIB_OPTIONAL_DEP_TPLS_local}
  )

  TRIBITS_ALLOW_MISSING_EXTERNAL_PACKAGES(${LIB_OPTIONAL_DEP_PACKAGES})

ENDIF()


SET(HDF5_REQUIRE_FORTRAN TRUE)
