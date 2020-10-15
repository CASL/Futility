#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
#                          Futility Development Group                          !
#                             All rights reserved.                             !
#                                                                              !
# Futility is a jointly-maintained, open-source project between the University !
# of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
# can be found in LICENSE.txt in the head directory of this repository.        !
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!

SET(PROJECT_NAME Futility)

SET(FUTILITY_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR})

INCLUDE(cmake/Project_Common.cmake)

# Put in hard disables for excluded packages
FOREACH(EXCLUDED_PACKAGE ${${PROJECT_NAME}_EXCLUDE_PACKAGES})
  SET(${PROJECT_NAME}_ENABLE_${EXCLUDED_PACKAGE} OFF CACHE BOOL
    "Disabled in Futility ProjectName.cmake"
    ${FORCE_EXCLUDED_PACKAGE_DISABLE}
    )
ENDFOREACH()

