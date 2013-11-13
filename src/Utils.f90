!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                              Copyright (C) 2012                              !
!                   The Regents of the University of Michigan                  !
!              MPACT Development Group and Prof. Thomas J. Downar              !
!                             All rights reserved.                             !
!                                                                              !
! Copyright is reserved to the University of Michigan for purposes of          !
! controlled dissemination, commercialization through formal licensing, or     !
! other disposition. The University of Michigan nor any of their employees,    !
! makes any warranty, express or implied, or assumes any liability or          !
! responsibility for the accuracy, completeness, or usefulness of any          !
! information, apparatus, product, or process disclosed, or represents that    !
! its use would not infringe privately owned rights. Reference herein to any   !
! specific commercial products, process, or service by trade name, trademark,  !
! manufacturer, or otherwise, does not necessarily constitute or imply its     !
! endorsement, recommendation, or favoring by the University of Michigan.      !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE Utils
  USE IntrType
  USE Strings
  USE Times
  USE BLAS1
  USE BLAS2
  USE BLAS3
  USE BinaryTrees
  USE Sorting
  USE MortonOrdering
  USE Geom_Points
  USE Geom_Line
  USE Geom_Plane
  USE Geom_CircCyl
  USE Geom_Box
  USE ExceptionHandler
  USE Allocs
  USE ParallelEnv
  USE IO_Strings
  USE FileType_Base
  USE FileType_Fortran
  USE FileType_Input
  USE FileType_Log
  USE FileType_HDF5
  USE IOutil
  USE VTKFiles
  USE ParameterLists
  USE CommandLineProcessor
  USE ExpTables
  USE VectorTypes
  USE MatrixTypes
  USE LinearSolverTypes
  USE StochasticSampling
  
  IMPLICIT NONE
  
  INCLUDE 'getSysProcInfo_F.h'
  
ENDMODULE Utils

