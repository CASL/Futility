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
PROGRAM testFileType_Input
#include "UnitTest.h"
  USE ISO_FORTRAN_ENV
  USE UnitTest
  USE ExceptionHandler
  USE FileType_Fortran
  USE FileType_Input
  
  IMPLICIT NONE
  
  CHARACTER(LEN=MAX_INPUT_FILE_LINE_LEN) :: string
  TYPE(ExceptionHandlerType),TARGET :: e
  TYPE(FortranFileType) :: testFile
  TYPE(InputFileType) :: testInpFile
      
  CREATE_TEST('FILETYPE_INPUT')
  
  CALL e%setStopOnError(.FALSE.)
  CALL e%setQuietMode(.TRUE.)
  
  REGISTER_SUBTEST('PARAMETERS',testParameters)
  REGISTER_SUBTEST('InputFileType',testInputFileType)
  
  FINALIZE_TEST()
!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
    SUBROUTINE testParameters()
      WRITE(*,*) '  Passed:  MAX_INPUT_FILE_LINE_LEN = ',MAX_INPUT_FILE_LINE_LEN
    ENDSUBROUTINE testParameters
!
!-------------------------------------------------------------------------------
    SUBROUTINE testInputFileType()
  
      CALL testInpFile%e%addSurrogate(e)
      ASSERT(.NOT.(testInpFile%getEchoStat()),'%getEchoStat()')
      ASSERT(testInpFile%getEchoUnit() == -1,'%getEchoUnit()')
      CALL testInpFile%setEchoStat(.TRUE.)
      ASSERT(testInpFile%getEchoStat(),'%setEchoStat(...)')
      CALL testInpFile%setEchoUnit(0)
      CALL testInpFile%setEchoUnit(25)
      ASSERT(testInpFile%getEchoUnit() == 25,'%setEchoUnit(...)')
      CALL testFile%initialize(UNIT=66,FILE='./test.inp',STATUS='REPLACE', &
        ACTION='WRITE')
      CALL testFile%fopen()
      WRITE(testFile%getUnitNo(),'(a,i2)') 'sample oneline',1
      CALL testFile%clear()
      CALL testFile%initialize(UNIT=25,FILE='./test.out',STATUS='REPLACE', &
        ACCESS='DIRECT',RECL=100,ACTION='WRITE')
      CALL testFile%fopen()
      CALL testInpFile%initialize(UNIT=46,FILE='./test.inp')
      CALL testInpFile%fopen()
      string=testInpFile%fgetl()
      ASSERT(TRIM(string) == 'sample oneline 1','%fgetl()')
      ASSERT(testInpFile%getProbe() == 's','%getProbe()')
      CALL testInpFile%frewind()
      ASSERT(LEN_TRIM(testInpFile%getProbe()) == 0,'%frewind()')
      string=testInpFile%fgetl()
      CALL testInpFile%fbackspace()
      ASSERT(LEN_TRIM(testInpFile%getProbe()) == 0,'%fbackspace()')
      CALL testFile%clear(.TRUE.)
      CALL testInpFile%setEchoStat(.FALSE.)
      string=testInpFile%fgetl()
      CALL testInpFile%clear(.TRUE.)
      ASSERT(testInpFile%getEchoUnit() == -1,'%clear() (echo unit)')
      ASSERT(.NOT.(testInpFile%getEchostat()),'%clear() (echo stat)')
    ENDSUBROUTINE testInputFileType
!
ENDPROGRAM testFileType_Input
