#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
#                          Futility Development Group                          !
#                             All rights reserved.                             !
#                                                                              !
# Futility is a jointly-maintained, open-source project between the University !
# of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
# can be found in LICENSE.txt in the head directory of this repository.        !
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!

# Preprocessor symbols for TPLs.
IF(${PACKAGE_NAME}_ENABLE_PAPI)
    SET(${PACKAGE_NAME}_DEFINES ${${PACKAGE_NAME}_DEFINES} HAVE_PAPI)
    MESSAGE(STATUS "Enabling PAPI Instrumentation")
ENDIF()
IF(${PACKAGE_NAME}_ENABLE_HDF5)
   SET(${PACKAGE_NAME}_DEFINES ${${PACKAGE_NAME}_DEFINES} FUTILITY_HAVE_HDF5)
   MESSAGE(STATUS "Enabling HDF5")
ENDIF()
IF(${PACKAGE_NAME}_ENABLE_BLAS)
    SET(${PACKAGE_NAME}_DEFINES ${${PACKAGE_NAME}_DEFINES} HAVE_BLAS)
    MESSAGE(STATUS "Enabling BLAS Routines")
ENDIF()
IF(${PACKAGE_NAME}_ENABLE_MKL)
    SET(${PACKAGE_NAME}_DEFINES ${${PACKAGE_NAME}_DEFINES} HAVE_MKL)
    MESSAGE(STATUS "Enabling MKL Routines")
ENDIF()
IF(${PACKAGE_NAME}_ENABLE_MPI)
    SET(${PACKAGE_NAME}_DEFINES ${${PACKAGE_NAME}_DEFINES} HAVE_MPI)
    MESSAGE(STATUS "Enabling MPI")
ENDIF()
IF(${PACKAGE_NAME}_ENABLE_PETSC)
    SET(${PACKAGE_NAME}_DEFINES ${${PACKAGE_NAME}_DEFINES} FUTILITY_HAVE_PETSC)
    MESSAGE(STATUS "Enabling PETSC Solvers")
ENDIF()
IF(${PACKAGE_NAME}_ENABLE_PETSC AND ${PACKAGE_NAME}_ENABLE_SLEPC)
    SET(${PACKAGE_NAME}_DEFINES ${${PACKAGE_NAME}_DEFINES} FUTILITY_HAVE_SLEPC)
    MESSAGE(STATUS "Enabling SLEPC Solvers")
ENDIF()

IF(${PACKAGE_NAME}_ENABLE_MPI AND ${PROJECT_NAME}_ENABLE_Epetra AND
        ${PROJECT_NAME}_ENABLE_Ifpack AND ${PROJECT_NAME}_ENABLE_ML AND
        ${PROJECT_NAME}_ENABLE_Belos AND ${PROJECT_NAME}_ENABLE_Anasazi AND
        ${PROJECT_NAME}_ENABLE_NOX AND ${PROJECT_NAME}_ENABLE_ForTeuchos)
    SET(${PACKAGE_NAME}_DEFINES ${${PACKAGE_NAME}_DEFINES} FUTILITY_HAVE_Trilinos)
    MESSAGE(STATUS "Enabling Trilinos Solvers")
ENDIF()
IF(${PACKAGE_NAME}_ENABLE_PARDISO)
    SET(${PACKAGE_NAME}_DEFINES ${${PACKAGE_NAME}_DEFINES} HAVE_PARDISO)
    MESSAGE(STATUS "Enabling Pardiso solver")
ENDIF()
IF(${PACKAGE_NAME}_ENABLE_SUNDIALS)
    SET(${PACKAGE_NAME}_DEFINES ${${PACKAGE_NAME}_DEFINES} FUTILITY_HAVE_SUNDIALS)
    MESSAGE(STATUS "Enabling Sundials ODE solver")
ENDIF()

#Other package preprocessor symbols
IF(${PROJECT_NAME}_ENABLE_DEBUG_MSG)
    SET(${PACKAGE_NAME}_DEFINES ${${PACKAGE_NAME}_DEFINES} FUTILITY_DEBUG_MSG)
    MESSAGE(STATUS "Enabling Debug Messages")
ENDIF()

#Process the default Futility project configurations
INCLUDE(Futility_Configurations)
