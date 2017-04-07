#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
#                          Futility Development Group                          !
#                             All rights reserved.                             !
#                                                                              !
# Futility is a jointly-maintained, open-source project between the University !
# of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
# can be found in LICENSE.txt in the head directory of this repository.        !
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!

SET(Futility_VERSION_MAJOR 2)
SET(Futility_VERSION_MINOR 1)
SET(Futility_VERSION_PATCH 0)
SET(Futility_VERSION ${${PROJECT_NAME}_VERSION_MAJOR}.${${PROJECT_NAME}_VERSION_MINOR}.${${PROJECT_NAME}_VERSION_PATCH})
SET(Futility_VERSION_STRING "${Futility_VERSION} (Dev)")
SET(Futility_MAJOR_VERSION ${Futility_VERSION_MAJOR})

# Used by testing scripts and should not be used elsewhere
SET(Futility_REPOSITORY_BRANCH "master" CACHE INTERNAL "")
SET(Futility_TESTING_TRACK "" CACHE INTERNAL "")
