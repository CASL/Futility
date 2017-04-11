SET_DEFAULT_AND_FROM_ENV(Futility_URL_REPO_BASE
  $ENV{USER}@ners-arc-05.engin.umich.edu:/git-root/MPACT/)
  
#
# Define the extra repos
#
TRIBITS_PROJECT_DEFINE_EXTRA_REPOSITORIES(
  Trilinos           ""  GIT  ${Futility_URL_REPO_BASE}Trilinos            PRE         Continuous
  TeuchosWrappersExt ""  GIT  ${Futility_URL_REPO_BASE}TeuchosWrappersExt  PRE         Continuous
)
