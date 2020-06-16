# Set Kokkos host spaces
IF(${PROJECT_NAME}_ENABLE_Kokkos)
  SET(KOKKOS_ENABLE_SERIAL TRUE)
  IF(DEFINED ${PROJECT_NAME}_ENABLE_OpenMP)
    SET(KOKKOS_ENABLE_OPENMP ${${PROJECT_NAME}_ENABLE_OpenMP})
  ENDIF()
ENDIF()

IF(Trilinos_SOURCE_DIR)
  include(${Trilinos_SOURCE_DIR}/cmake/ProjectCompilerPostConfig.cmake)
ENDIF()
