SET_DEFAULT_AND_FROM_ENV(Futility_URL_REPO_BASE
  $ENV{USER}@ners-arc-05.engin.umich.edu:/git-root/MPACT/)

#
# Define the extra repos
#
IF(NOT ${PROJECT_NAME}_TRILINOS_TPL)
  SET(TRILINOS_PACKAGE_EXTRA_REPO
    Trilinos           ""  GIT  ${Futility_URL_REPO_BASE}Trilinos            PRE         Continuous
    TeuchosWrappersExt ""  GIT  ${Futility_URL_REPO_BASE}TeuchosWrappersExt  PRE         Continuous
  )
ELSE()
  SET(TRILINOS_PACKAGE_EXTRA_REPO )
ENDIF()

TRIBITS_PROJECT_DEFINE_EXTRA_REPOSITORIES(
  ${TRILINOS_PACKAGE_EXTRA_REPO}
  )