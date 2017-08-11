#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
#                          Futility Development Group                          !
#                             All rights reserved.                             !
#                                                                              !
# Futility is a jointly-maintained, open-source project between the University !
# of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
# can be found in LICENSE.txt in the head directory of this repository.        !
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!

SET(LIB_REQUIRED_DEP_PACKAGES)
SET(LIB_OPTIONAL_DEP_PACKAGES Tpetra Amesos2 Thyra Rythmos MueLu Ifpack2 Epetra Ifpack ML Anasazi Belos NOX CTeuchos ForTeuchos)
SET(TEST_REQUIRED_DEP_PACKAGES)
SET(TEST_OPTIONAL_DEP_PACKAGES)
SET(LIB_REQUIRED_DEP_TPLS)
SET(LIB_OPTIONAL_DEP_TPLS BLAS LAPACK MPI HYPRE PETSC PARDISO HDF5 PAPI SLEPC SUNDIALS)
SET(TEST_REQUIRED_DEP_TPLS)
SET(TEST_OPTIONAL_DEP_TPLS MPI)

SET(HDF5_REQUIRE_FORTRAN TRUE)

TRIBITS_ALLOW_MISSING_EXTERNAL_PACKAGES(Epetra Ifpack ML Anasazi Belos NOX CTeuchos ForTeuchos)
