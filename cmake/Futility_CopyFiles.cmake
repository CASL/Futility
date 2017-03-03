#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
#                          Futility Development Group                          !
#                             All rights reserved.                             !
#                                                                              !
# Futility is a jointly-maintained, open-source project between the University !
# of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
# can be found in LICENSE.txt in the head directory of this repository.        !
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
FUNCTION(Futility_CopyFiles FILES)

    FOREACH(file ${${FILES}})
        CONFIGURE_FILE(${CMAKE_CURRENT_SOURCE_DIR}/${file}
            ${CMAKE_CURRENT_BINARY_DIR}/${file} COPYONLY)
    ENDFOREACH()
    
    UNSET(file)
    UNSET(FILES)
ENDFUNCTION()
