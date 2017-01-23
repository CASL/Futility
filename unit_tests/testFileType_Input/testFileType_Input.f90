!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
PROGRAM testFileType_Input
#include "UnitTest.h"
  USE ISO_FORTRAN_ENV
  USE UnitTest
  USE Strings
  USE ExceptionHandler
  USE FileType_Fortran
  USE FileType_Input

  IMPLICIT NONE

  TYPE(StringType) :: string
  TYPE(ExceptionHandlerType),TARGET :: e
  TYPE(FortranFileType) :: testFile
  TYPE(InputFileType) :: testInpFile

  CREATE_TEST('FILETYPE_INPUT')

  CALL e%setStopOnError(.FALSE.)
  CALL e%setQuietMode(.TRUE.)

  REGISTER_SUBTEST('InputFileType',testInputFileType)

  FINALIZE_TEST()
!
!===============================================================================
  CONTAINS
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
      CALL testInpFile%fgetl(string)
      ASSERT(TRIM(string) == 'sample oneline 1','%fgetl()')
      ASSERT(testInpFile%getProbe() == 's','%getProbe()')
      CALL testInpFile%frewind()
      ASSERT(LEN_TRIM(testInpFile%getProbe()) == 0,'%frewind()')
      CALL testInpFile%fgetl(string)
      CALL testInpFile%fbackspace()
      ASSERT(LEN_TRIM(testInpFile%getProbe()) == 0,'%fbackspace()')
      CALL testFile%clear(.TRUE.)
      CALL testInpFile%setEchoStat(.FALSE.)
      CALL testInpFile%fgetl(string)
      CALL testInpFile%clear(.TRUE.)
      ASSERT(testInpFile%getEchoUnit() == -1,'%clear() (echo unit)')
      ASSERT(.NOT.(testInpFile%getEchostat()),'%clear() (echo stat)')
    ENDSUBROUTINE testInputFileType
!
ENDPROGRAM testFileType_Input
