!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!  This manuscript has been authored by UT-Battelle, LLC, under Contract       !
!  No. DE-AC0500OR22725 with the U.S. Department of Energy. The United States  !
!  Government retains and the publisher, by accepting the article for          !
!  publication, acknowledges that the United States Government retains a       !
!  non-exclusive, paid-up, irrevocable, world-wide license to publish or       !
!  reproduce the published form of this manuscript, or allow others to do so,  !
!  for the United States Government purposes.                                  !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE Futility

  USE ISO_FORTRAN_ENV
  USE ISO_C_BINDING
  USE Futility_DBC
  USE IntrType
  USE Strings
  USE ExceptionHandler
  USE Times
  USE Allocs
  USE ParallelEnv
  USE MemProf
  USE IO_Strings
  USE IOutil
  USE BLAS
  USE FileType_Fortran
  USE FileTYpe_Input
  USE FileType_Log
  USE ExpTables
  IMPLICIT NONE
!
ENDMODULE Futility
