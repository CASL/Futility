#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
#                          Futility Development Group                          !
#                             All rights reserved.                             !
#                                                                              !
# Futility is a jointly-maintained, open-source project between the University !
# of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
# can be found in LICENSE.txt in the head directory of this repository.        !
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!

SET(${PROJECT_NAME}_TEST_CATEGORIES_DEFAULT "BASIC")
SET(MPI_EXEC_MAX_NUMPROCS_DEFAULT 60)

# Automatically read in Continuous cmake/ExtraRepositories.cmake file repos!
SET(${PROJECT_NAME}_ENABLE_KNOWN_EXTERNAL_REPOS_TYPE Continuous
  CACHE STRING
  "Set by default in ProjectName.cmake")
SET(${PROJECT_NAME}_IGNORE_MISSING_EXTRA_REPOSITORIES TRUE
  CACHE BOOL
  "Set by default in ProjectName.cmake")

# Elevating ST packages to PT packages so that the checkin-test.py script will
# allow these in --default-builds.
SET(${PROJECT_NAME}_ELEVATE_ST_TO_PT ON CACHE BOOL
  "Set by default in ProjectName.cmake")

# Turn on Secondary Stable code for now until you can fix elevating SS to PS
# for subpackages!
SET(${PROJECT_NAME}_ENABLE_SECONDARY_TESTED_CODE  ON  CACHE BOOL  "")

# Exclude a bunch of packages from primary meta-project packages
SET(Trilinos_NO_PRIMARY_META_PROJECT_PACKAGES TRUE)

# Enable all Primary Meta-Project Packages by default
SET(${PROJECT_NAME}_ENABLE_ALL_PACKAGES  ON
  CACHE  BOOL  "Set by default in ProjectName.cmake")

# Turn on C++11 by default
SET(${PROJECT_NAME}_ENABLE_CXX11_DEFAULT  ON)

# These are all packages that are not needed.  This list is the only
# list that need to be maintained to exclude and disable Trilinos packages.
# This list is used in a variety of places.
IF (NOT ${PROJECT_NAME}_EXCLUDE_PACKAGES)
  SET(${PROJECT_NAME}_EXCLUDE_PACKAGES
    # Trilinos disables
    ML
    Gtest
    Sacado
    Zoltan
    Shards
    Intrepid
    Pike
    ThreadPool
    GlobiPack
    OptiPack
    Pliris
    Claps
    Galeri
    Pamgen
    Komplex
    RBGen
    Phdmesh
    Moertel
    TrilinosCouplings
    MOOCHO
    Stokhos
    Piro
    Panzer
    Sundance
    CTrilinos
    ForTrilinos
    PyTrilinos
    Didasko
    Optika
    Mesquite
    FEApp
    Zoltan2
    Gtest
    SEACAS
    FEI
    STK
    Phalanx
    KokkosTPL
    ShyLU
    ShyLU_Node
    )
ENDIF()

# Disable a bunch of other stuff we don't want/need
SET(EpetraExt_ENABLE_PETSC  OFF)
SET(ML_ENABLE_PETSC  OFF)
SET(NOX_ENABLE_PETSC  OFF)

#####################################################
###                                               ###
###   Put in hard disables for excluded packages  ###
###                                               ###
#####################################################

SET(${PROJECT_NAME}_FORCE_EXCLUDED_PACKAGE_DISABLES  ON
  CACHE  BOOL
  "If ON, all excluded packages are forced off even if user tries to enable")
MARK_AS_ADVANCED(${PROJECT_NAME}_FORCE_EXCLUDED_PACKAGE_DISABLES)

IF (${PROJECT_NAME}_FORCE_EXCLUDED_PACKAGE_DISABLES)
   SET(FORCE_EXCLUDED_PACKAGE_DISABLE FORCE)
ELSE()
   SET(FORCE_EXCLUDED_PACKAGE_DISABLE)
ENDIF()
