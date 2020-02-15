# Set Kokkos host spaces
IF(${PROJECT_NAME}_ENABLE_Kokkos)
  SET(KOKKOS_ENABLE_SERIAL TRUE)
  IF(${PROJECT_NAME}_ENABLE_OpenMP)
    SET(KOKKOS_ENABLE_OPENMP TRUE)
  ENDIF()
ENDIF()

if(Trilinos_SOURCE_DIR)
  include(${Trilinos_SOURCE_DIR}/cmake/ProjectCompilerPostConfig.cmake)
endif()
