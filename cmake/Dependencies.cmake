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

TRIBITS_PACKAGE_DEFINE_DEPENDENCIES(
  LIB_OPTIONAL_PACKAGES
    ${LIB_OPTIONAL_DEP_PACKAGES_local}
  LIB_OPTIONAL_TPLS
    BLAS LAPACK MPI HYPRE PETSC PARDISO HDF5 PAPI SLEPC SUNDIALS
  TEST_OPTIONAL_TPLS
    MPI
  )

SET(HDF5_REQUIRE_FORTRAN TRUE)

TRIBITS_ALLOW_MISSING_EXTERNAL_PACKAGES(${LIB_OPTIONAL_DEP_PACKAGES})
