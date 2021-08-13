!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
PROGRAM testFileType_Fortran
#include "UnitTest.h"
USE ISO_FORTRAN_ENV
USE UnitTest
USE IntrType
USE Strings
USE ExceptionHandler
USE FileType_Fortran

IMPLICIT NONE

LOGICAL(SBK) :: lexist
TYPE(ExceptionHandlerType),TARGET :: e
TYPE(FortranFileType) :: testFile,testFile2

CREATE_TEST('FILETYPE_FORTRAN')

CALL e%setStopOnError(.FALSE.)
CALL e%setQuietMode(.TRUE.)

REGISTER_SUBTEST('FortranFileType',testFortranFileType)

FINALIZE_TEST()
!
!===============================================================================
CONTAINS
!
!-------------------------------------------------------------------------------
SUBROUTINE testFortranFileType()
  CHARACTER(LEN=256) :: buffer
  INTEGER(SIK) :: i,ioerr
  TYPE(StringType),ALLOCATABLE :: lines(:)

  COMPONENT_TEST('%clear()')
  CALL testFile%clear()
  !Configure exception handler for testing
  CALL testFile%e%addSurrogate(e)
  CALL testFile2%e%addSurrogate(e)
  ASSERT(testFile%getUnitNo() == -1,'%getUnitNo()')
  ASSERT(.NOT.(testFile%isFormatted()),'%isFormatted()')
  ASSERT(.NOT.(testFile%isDirect()),'%isDirect()')
  ASSERT(testFile%getRecLen() == -1,'%getRecLen()')
  ASSERT(.NOT.(testFile%isPadded()),'%isPadded()')
  ASSERT(.NOT.(testFile%isNew()),'%isNew()')
  ASSERT(.NOT.(testFile%isOverwrite()),'%isOverwrite()')

  !Called for coverage/error checking
  COMPONENT_TEST('%initialize()')
  CALL testFile%initialize(UNIT=OUTPUT_UNIT,FILE='./testFile.txt',STATUS='OOPS',&
      ACCESS='OOPS',FORM='OOPS',POSITION='OOPS',ACTION='OOPS',PAD='OOPS',RECL=-1)
  CALL testFile%initialize(UNIT=ERROR_UNIT,FILE='./testFile.txt',STATUS='OLD',&
      ACCESS='SEQUENTIAL',FORM='FORMATTED',POSITION='ASIS',ACTION='WRITE',PAD='NO')
  CALL testFile%initialize(UNIT=INPUT_UNIT,FILE='./testFile.txt',STATUS='NEW',&
      ACCESS='DIRECT',FORM='UNFORMATTED',POSITION='APPEND',ACTION='READ', &
      PAD='YES')
  CALL testFile%initialize(UNIT=OUTPUT_UNIT,FILE='oops.txt',STATUS='REPLACE', &
      ACCESS='DIRECT',POSITION='REWIND',ACTION='READWRITE',RECL=100)
  CALL testFile%initialize(UNIT=OUTPUT_UNIT,FILE='oops.txt',STATUS='SCRATCH')
  CALL testFile%initialize(UNIT=OUTPUT_UNIT,FILE='oops.txt',STATUS='UNKNOWN')

  CALL testFile%initialize(UNIT=12,FILE='./testFile.txt',PAD='NO')
  ASSERT(testFile%isInit(),'%isInit(...)')
  ASSERT(TRIM(testFile%getFileName()) == 'testFile','%getFileName()')

  CALL testFile%setStatus('Garbage')
  CALL testFile%setStatus('OLD')
  ASSERT(.NOT.testFile%isNew(),'%isNew() OLD')
  ASSERT(.NOT.testFile%isOverwrite(),'%isOverwrite() OLD')
  CALL testFile%setStatus('NEW')
  ASSERT(testFile%isNew(),'%isNew() NEW')
  ASSERT(.NOT.testFile%isOverwrite(),'%isOverwrite() NEW')
  CALL testFile%setStatus('REPLACE')
  ASSERT(testFile%isNew(),'%isNew() REPLACE')
  ASSERT(testFile%isOverwrite(),'%isOverwrite() REPLACE')
  CALL testFile%setStatus('SCRATCH')
  ASSERT(testFile%isNew(),'%isNew() SCRATCH')
  ASSERT(testFile%isOverwrite(),'%isOverwrite() SCRATCH')
  CALL testFile%setStatus('UNKNOWN')
  ASSERT(testFile%isNew(),'%isNew() UNKNOWN')
  ASSERT(testFile%isOverwrite(),'%isOverwrite() UNKNOWN')

  COMPONENT_TEST('%fopen()')
  CALL testFile%fopen()
  ASSERT(testFile%isOpen(),'testFile%fopen()')

  !Coverage/Error checking
  CALL testFile2%initialize(UNIT=12,FILE='./testFile.txt')

  CALL testFile%fclose()
  ASSERT(.NOT.(testFile%isOpen()),'%fclose()')
  CALL testFile%fdelete()
  INQUIRE(FILE='./testFile.txt',EXIST=lexist)
  ASSERT(.NOT.lexist,'%fdelete()')
  CALL testFile%clear()

  !Coverage/Error checking
  CALL testFile2%initialize(UNIT=13,FILE='./testFile2',ACCESS='DIRECT', &
      STATUS='NEW',FORM='UNFORMATTED',RECL=100*NUMERIC_STORAGE_SIZE, &
      ACTION='WRITE')
  CALL testFile2%fopen()
  CALL testFile2%fdelete()
  CALL testFile%e%addSurrogate(e)
  CALL testFile%initialize(UNIT=12,FILE='./testFile.txt',STATUS='OLD', &
      ACCESS='DIRECT',ACTION='READ',RECL=100,FORM='FORMATTED')
  CALL testFile%fopen()
  CALL testFile%fdelete()
  CALL testFile%clear()
  CALL testFile%e%addSurrogate(e)
  CALL testFile%initialize(UNIT=12,FILE='./testFile.txt',STATUS='OLD', &
      ACTION='READ',FORM='UNFORMATTED')
  CALL testFile%fopen()
  CALL testFile%fdelete()
  CALL testFile%clear()

  COMPONENT_TEST('%fwrite()')
  CALL testFile%initialize(UNIT=12,FILE='./testFile.txt',STATUS='NEW', &
      POSITION='REWIND')
  CALL testFile%fopen()
  ALLOCATE(lines(5))
  lines(1)='+-------+-----+'
  lines(2)='| Test1 | 1.0 |'
  lines(3)='+-------+-----+'
  lines(4)='| Test2 | 10  |'
  lines(5)='+-------+-----+'
  CALL testFile%fwrite(lines)

  ioerr=0
  !Be kind, rewind since we wrote the table as well.
  REWIND(testFile%getUnitNo())
  READ(UNIT=testfile%getUnitNo(),FMT='(a)',ADVANCE='NO',IOSTAT=ioerr) buffer
  ASSERT_EQ(TRIM(buffer),'   '//CHAR(lines(1)),"lines line 1")
  READ(UNIT=testfile%getUnitNo(),FMT='(a)',ADVANCE='NO',IOSTAT=ioerr) buffer
  ASSERT_EQ(TRIM(buffer),'   '//CHAR(lines(2)),"lines line 2")
  READ(UNIT=testfile%getUnitNo(),FMT='(a)',ADVANCE='NO',IOSTAT=ioerr) buffer
  ASSERT_EQ(TRIM(buffer),'   '//CHAR(lines(3)),"lines line 3")
  READ(UNIT=testfile%getUnitNo(),FMT='(a)',ADVANCE='NO',IOSTAT=ioerr) buffer
  ASSERT_EQ(TRIM(buffer),'   '//CHAR(lines(4)),"lines line 4")
  READ(UNIT=testfile%getUnitNo(),FMT='(a)',ADVANCE='NO',IOSTAT=ioerr) buffer
  ASSERT_EQ(TRIM(buffer),'   '//CHAR(lines(5)),"lines line 5")
  DEALLOCATE(lines)

  !Another table
  ALLOCATE(lines(9))
  lines(1)='+------------------+-------------------+-----------------+'
  lines(2)='| "Some Quotes"    | semi-             | more            |'
  lines(3)='|                  | colon             | than            |'
  lines(4)='|                  |                   | one semi colon  |'
  lines(5)='+------------------+-------------------+-----------------+'
  lines(6)='| True             | loooooooooooooong | -               |'
  lines(7)='| Mostly False-ish | 1.000E+23         | 100,000,000,000 |'
  lines(8)='| or True          |                   |                 |'
  lines(9)='+------------------+-------------------+-----------------+'
  CALL testFile%fwrite(lines)

  ioerr=0
  !Be kind, rewind since we wrote the other table as well.
  REWIND(testFile%getUnitNo())
  DO i=1,5
    READ(UNIT=testfile%getUnitNo(),FMT='(a)',ADVANCE='NO',IOSTAT=ioerr) buffer
  ENDDO
  READ(UNIT=testfile%getUnitNo(),FMT='(a)',ADVANCE='NO',IOSTAT=ioerr) buffer
  ASSERT_EQ(TRIM(buffer),'   '//CHAR(lines(1)),"lines line 6")
  READ(UNIT=testfile%getUnitNo(),FMT='(a)',ADVANCE='NO',IOSTAT=ioerr) buffer
  ASSERT_EQ(TRIM(buffer),'   '//CHAR(lines(2)),"lines line 7")
  READ(UNIT=testfile%getUnitNo(),FMT='(a)',ADVANCE='NO',IOSTAT=ioerr) buffer
  ASSERT_EQ(TRIM(buffer),'   '//CHAR(lines(3)),"lines line 8")
  READ(UNIT=testfile%getUnitNo(),FMT='(a)',ADVANCE='NO',IOSTAT=ioerr) buffer
  ASSERT_EQ(TRIM(buffer),'   '//CHAR(lines(4)),"lines line 9")
  READ(UNIT=testfile%getUnitNo(),FMT='(a)',ADVANCE='NO',IOSTAT=ioerr) buffer
  ASSERT_EQ(TRIM(buffer),'   '//CHAR(lines(5)),"table line 10")
  READ(UNIT=testfile%getUnitNo(),FMT='(a)',ADVANCE='NO',IOSTAT=ioerr) buffer
  ASSERT_EQ(TRIM(buffer),'   '//CHAR(lines(6)),"lines line 11")
  READ(UNIT=testfile%getUnitNo(),FMT='(a)',ADVANCE='NO',IOSTAT=ioerr) buffer
  ASSERT_EQ(TRIM(buffer),'   '//CHAR(lines(7)),"lines line 12")
  READ(UNIT=testfile%getUnitNo(),FMT='(a)',ADVANCE='NO',IOSTAT=ioerr) buffer
  ASSERT_EQ(TRIM(buffer),'   '//CHAR(lines(8)),"lines line 13")
  READ(UNIT=testfile%getUnitNo(),FMT='(a)',ADVANCE='NO',IOSTAT=ioerr) buffer
  ASSERT_EQ(TRIM(buffer),'   '//CHAR(lines(9)),"lines line 14")
  DEALLOCATE(lines)
  CALL testFile%fclose()
  CALL testFile%clear()

  ! Ensure unit number is as expected
  ASSERT(testFile%getUnitNo()==-1, "")
  call testFile%initialize(UNIT=12,FILE='.testFile.txt')
  ASSERT(testFile%getUnitNo()==12, "")
  call testFile%clear(ldel=.TRUE.)
  ASSERT(testFile%getUnitNo()==-1, "")

ENDSUBROUTINE testFortranFileType
!
ENDPROGRAM testFileType_Fortran
