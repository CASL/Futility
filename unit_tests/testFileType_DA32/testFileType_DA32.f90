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
PROGRAM testFileType_DA32
#include "UnitTest.h"
  USE ISO_FORTRAN_ENV
  USE UnitTest
  USE IntrType
  USE ExceptionHandler
  USE FileType_DA32
  
  IMPLICIT NONE
  
  TYPE(ExceptionHandlerType),TARGET,SAVE :: e
  TYPE(DA32FileType) :: testDA32File
  
  CREATE_TEST('DA32 FILETYPE')
  
  CALL e%setStopOnError(.FALSE.)
  CALL e%setQuietMode(.TRUE.)
  
  CALL setupTest()
  REGISTER_SUBTEST('Parameters',testParams)
  REGISTER_SUBTEST('Read',testRead)
  REGISTER_SUBTEST('Write',testWrite)
  REGISTER_SUBTEST('%getPad2NextBlk()',testPad)
  REGISTER_SUBTEST('%writeEmptyBlock()',testWMTBlock)
  
  FINALIZE_TEST()
!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
    SUBROUTINE setupTest()
      INTEGER(SNK) :: i,singleint,tmpData(2),intarry(100),intarry2(258)
      INTEGER(SLK) :: doubleint,longarry(100)
      REAL(SSK) :: singlereal,singlearray(100)
      REAL(SDK) :: doublereal,doublearray(100)
      
      singleint=HUGE(singleint)
      doubleint=HUGE(doubleint)
      singlereal=EPSILON(singlereal)
      doublereal=EPSILON(doublereal)
      
      OPEN(UNIT=7,FILE='testReadfile.bin',ACCESS='DIRECT',FORM='UNFORMATTED', &
        RECL=RECL32,ACTION='READWRITE')
            
      tmpData(1)=TRANSFER(.TRUE.,tmpData(1))
      WRITE(7,REC=1) tmpData(1)
      WRITE(7,REC=2) singleint
      tmpData=TRANSFER(doubleint,tmpData)
      WRITE(7,REC=3) tmpData(1)
      WRITE(7,REC=4) tmpData(2)
      
      WRITE(7,REC=5) singlereal
      tmpData=TRANSFER(doublereal,tmpData)
      WRITE(7,REC=6) tmpData(1)
      WRITE(7,REC=7) tmpData(2)
      
      WRITE(7,REC=8) '1234'
      WRITE(7,REC=9) '5'
      intarry=0
      intarry(1)=singleint
      intarry(99)=singleint
      DO i=1,100
        WRITE(7,REC=9+i) intarry(i)
      ENDDO
      
      longarry=0
      longarry(1)=doubleint
      longarry(99)=doubleint
      DO i=1,100
        tmpData=TRANSFER(longarry(i),tmpData)
        WRITE(7,REC=109+2*i-1) tmpData(1)
        WRITE(7,REC=109+2*i  ) tmpData(2)
      ENDDO
      DO i=1,258
        WRITE(7,REC=511+i) i        
      ENDDO
      
      singlearray=0.0_SSK
      singlearray(1)=TINY(singlereal)
      singlearray(99)=HUGE(singlereal)
      DO i=1,100
        WRITE(7,REC=770+i) singlearray(i)
      ENDDO
      
      doublearray=0.0_SSK
      doublearray(1)=TINY(doublereal)
      doublearray(99)=HUGE(doublereal)
      DO i=1,100
        tmpData=TRANSFER(doublearray(i),tmpData)
        WRITE(7,REC=870+2*i-1) tmpData(1)
        WRITE(7,REC=870+2*i  ) tmpData(2)
      ENDDO
      CLOSE(7)
    ENDSUBROUTINE setupTest
!
!-------------------------------------------------------------------------------
    SUBROUTINE testParams()
      INFO(0) "RECL32=",RECL32
      INFO(0) "RECLSZ=",RECLSZ
      INFO(0) "RECL_UNIT="//'"'//RECL_UNIT//'"'
      INFO(0) "MAX REC VALUE=",testDA32File%maxRecVal()
    ENDSUBROUTINE testParams
!
!-------------------------------------------------------------------------------
    SUBROUTINE testRead()
      LOGICAL(SBK) :: testBool
      INTEGER(SNK) :: testSNK,testSNK1(100),testSNK2(10,10),test2SNK(258),i,j
      INTEGER(SLK) :: testSLK,testSLK1(100),testSLK2(10,10)
      REAL(SSK) :: testSSK,testSSK1(100),testSSK2(10,10)
      REAL(SDK) :: testSDK,testSDK1(100),testSDK2(10,10)
      INTEGER(SIK) :: ioerr
      INTEGER(SLK) :: n
      CHARACTER(LEN=6) :: testCHAR
      
      CALL testDA32File%e%addSurrogate(e)
      CALL testDA32File%initialize(FILE='testReadfile.bin',STATUS='OLD', &
        ACTION='READ')
      CALL testDA32File%fopen()
      
      COMPONENT_TEST('read_sbk')
      testBool=.FALSE.; n=0; ioerr=-1
      CALL testDA32File%readdat(REC=1_SLK,DAT=testBool,IOSTAT=ioerr,NREC=n)
      ASSERT(testBool,'testBool value 1')
      ASSERT(n == 1,'n')
      ASSERT(ioerr == 0,'ioerr')
      testBool=.FALSE.; n=0; ioerr=-1
      CALL testDA32File%readdat(REC=1_SLK,DAT=testBool,IOSTAT=ioerr)
      ASSERT(testBool,'testBool value 2')
      ASSERT(ioerr == 0,'ioerr')
      testBool=.FALSE.; n=0; ioerr=-1
      CALL testDA32File%readdat(REC=1_SLK,DAT=testBool,NREC=n)
      ASSERT(testBool,'testBool value 3')
      ASSERT(n == 1,'n')
      testBool=.FALSE.; n=0; ioerr=-1
      CALL testDA32File%readdat(REC=1_SLK,DAT=testBool)
      ASSERT(testBool,'testBool value 4')
      
      COMPONENT_TEST('read_snk')
      testSNK=0; n=0; ioerr=-1
      CALL testDA32File%readdat(REC=2_SLK,DAT=testSNK,IOSTAT=ioerr,NREC=n)
      ASSERT(testSNK == HUGE(testSNK),'testSNK value 1')
      ASSERT(n == 1,'n')
      ASSERT(ioerr == 0,'ioerr')
      testSNK=0; n=0; ioerr=-1
      CALL testDA32File%readdat(REC=2_SLK,DAT=testSNK,IOSTAT=ioerr)
      ASSERT(testSNK == HUGE(testSNK),'testSNK value 2')
      ASSERT(ioerr == 0,'ioerr')
      testSNK=0; n=0; ioerr=-1
      CALL testDA32File%readdat(REC=2_SLK,DAT=testSNK,NREC=n)
      ASSERT(testSNK == HUGE(testSNK),'testSNK value 3')
      ASSERT(n == 1,'n')
      testSNK=0; n=0; ioerr=-1
      CALL testDA32File%readdat(REC=2_SLK,DAT=testSNK)
      ASSERT(testSNK == HUGE(testSNK),'testSNK value 4')
      CALL testDA32File%readdat(REC=10_SLK,DAT=testSNK1,IOSTAT=ioerr,NREC=n)
      ASSERT(ioerr == 0,'testSNK1 ioerr')
      ASSERT(n == 100,'testSNK1 n')
      ASSERT(testSNK1(1) == HUGE(testSNK),'testSNK1(1) value')
      ASSERT(testSNK1(99) == HUGE(testSNK),'testSNK1(99) value')
      ASSERT(testSNK1(100) == 0,'testSNK1(100) value')
      DO i=2,98
        ASSERT(testSNK1(i) == 0,'testSNK1(i) value')
        FINFO() i
      ENDDO
      CALL testDA32File%readdat(REC=10_SLK,DAT=testSNK2,IOSTAT=ioerr,NREC=n)
      ASSERT(ioerr == 0,'testSNK2 ioerr')
      ASSERT(n == 100,'testSNK2 n')
      DO j=1,10
        DO i=1,10
          IF((i == 1 .AND. j == 1) .OR. (i == 9 .AND. j == 10)) THEN
            ASSERT(testSNK2(i,j) == HUGE(testSNK),'testSNK2(i,j) value')
          ELSE
            ASSERT(testSNK2(i,j) == 0,'testSNK2(i,j) value')
          ENDIF
          FINFO() i,j,testSNK2(i,j)
        ENDDO
      ENDDO
      
      COMPONENT_TEST('read_ssk')
      testSSK=0.0; n=0; ioerr=-1
      CALL testDA32File%readdat(REC=5_SLK,DAT=testSSK,IOSTAT=ioerr,NREC=n)
      ASSERT(testSSK == EPSILON(testSSK),'testSSK value 1')
      ASSERT(n == 1,'n')
      ASSERT(ioerr == 0,'ioerr')
      testSSK=0.0; n=0; ioerr=-1
      CALL testDA32File%readdat(REC=5_SLK,DAT=testSSK,IOSTAT=ioerr)
      ASSERT(testSSK == EPSILON(testSSK),'testSSK value 2')
      ASSERT(ioerr == 0,'ioerr')
      testSSK=0.0; n=0; ioerr=-1
      CALL testDA32File%readdat(REC=5_SLK,DAT=testSSK,NREC=n)
      ASSERT(testSSK == EPSILON(testSSK),'testSSK value 3')
      ASSERT(n == 1,'n')
      testSSK=0.0; n=0; ioerr=-1
      CALL testDA32File%readdat(REC=5_SLK,DAT=testSSK)
      ASSERT(testSSK == EPSILON(testSSK),'testSSK value 4')
      CALL testDA32File%readdat(REC=771_SLK,DAT=testSSK1,IOSTAT=ioerr,NREC=n)
      ASSERT(ioerr == 0,'testSSK1 ioerr')
      ASSERT(n == 100,'testSSK1 n')
      ASSERT(testSSK1(1) == TINY(testSSK),'testSSK1(1) value')
      ASSERT(testSSK1(99) == HUGE(testSSK),'testSSK1(99) value')
      ASSERT(testSSK1(100) == 0,'testSSK1(100) value')
      DO i=2,98
        ASSERT(testSSK1(i) == 0,'testSSK1(i) value')
        FINFO() i
      ENDDO
      CALL testDA32File%readdat(REC=771_SLK,DAT=testSSK2,IOSTAT=ioerr,NREC=n)
      ASSERT(ioerr == 0,'testSSK2 ioerr')
      ASSERT(n == 100,'testSSK2 n')
      DO j=1,10
        DO i=1,10
          IF(i == 1 .AND. j == 1) THEN
            ASSERT(testSSK2(i,j) == TINY(testSSK),'testSSK2(i,j) value')
          ELSEIF(i == 9 .AND. j == 10) THEN
            ASSERT(testSSK2(i,j) == HUGE(testSSK),'testSSK2(i,j) value')
          ELSE
            ASSERT(testSSK2(i,j) == 0,'testSSK2(i,j) value')
          ENDIF
          FINFO() i,j,testSSK2(i,j)
        ENDDO
      ENDDO
      
      COMPONENT_TEST('read_slk')
      testSLK=0; n=0; ioerr=-1
      CALL testDA32File%readdat(REC=3_SLK,DAT=testSLK,IOSTAT=ioerr,NREC=n)
      ASSERT(testSLK == HUGE(testSLK),'testSLK value 1')
      ASSERT(n == 2,'n')
      ASSERT(ioerr == 0,'ioerr')
      testSLK=0; n=0; ioerr=-1
      CALL testDA32File%readdat(REC=3_SLK,DAT=testSLK,IOSTAT=ioerr)
      ASSERT(testSLK == HUGE(testSLK),'testSLK value 2')
      ASSERT(ioerr == 0,'ioerr')
      testSLK=0; n=0; ioerr=-1
      CALL testDA32File%readdat(REC=3_SLK,DAT=testSLK,NREC=n)
      ASSERT(testSLK == HUGE(testSLK),'testSLK value 3')
      ASSERT(n == 2,'n')
      testSLK=0; n=0; ioerr=-1
      CALL testDA32File%readdat(REC=3_SLK,DAT=testSLK)
      ASSERT(testSLK == HUGE(testSLK),'testSLK value 4')
      CALL testDA32File%readdat(REC=110_SLK,DAT=testSLK1,IOSTAT=ioerr,NREC=n)
      ASSERT(ioerr == 0,'testSLK1 ioerr')
      ASSERT(n == 200,'testSLK1 n')
      ASSERT(testSLK1(1) == HUGE(testSLK),'testSLK1(1) value')
      ASSERT(testSLK1(99) == HUGE(testSLK),'testSLK1(99) value')
      ASSERT(testSLK1(100) == 0,'testSLK1(100) value')
      DO i=2,98
        ASSERT(testSLK1(i) == 0,'testSLK1(i) value')
        FINFO() i
      ENDDO
      CALL testDA32File%readdat(REC=110_SLK,DAT=testSLK2,IOSTAT=ioerr,NREC=n)
      ASSERT(ioerr == 0,'testSLK2 ioerr')
      ASSERT(n == 200,'testSLK2 n')
      DO j=1,10
        DO i=1,10
          IF((i == 1 .AND. j == 1) .OR. (i == 9 .AND. j == 10)) THEN
            ASSERT(testSLK2(i,j) == HUGE(testSLK),'testSLK2(i,j) value')
          ELSE
            ASSERT(testSLK2(i,j) == 0,'testSLK2(i,j) value')
          ENDIF
          FINFO() i,j,testSLK2(i,j)
        ENDDO
      ENDDO
      
      COMPONENT_TEST('read_sdk')
      testSDK=0.0_SDK; n=0; ioerr=-1
      CALL testDA32File%readdat(REC=6_SLK,DAT=testSDK,IOSTAT=ioerr,NREC=n)
      ASSERT(testSDK == EPSILON(testSDK),'testSDK value 1')
      ASSERT(n == 2,'n')
      ASSERT(ioerr == 0,'ioerr')
      testSDK=0.0_SDK; n=0; ioerr=-1
      CALL testDA32File%readdat(REC=6_SLK,DAT=testSDK,IOSTAT=ioerr)
      ASSERT(testSDK == EPSILON(testSDK),'testSDK value 2')
      ASSERT(ioerr == 0,'ioerr')
      testSDK=0.0_SDK; n=0; ioerr=-1
      CALL testDA32File%readdat(REC=6_SLK,DAT=testSDK,NREC=n)
      ASSERT(testSDK == EPSILON(testSDK),'testSDK value 3')
      ASSERT(n == 2,'n')
      testSDK=0.0_SDK; n=0; ioerr=-1
      CALL testDA32File%readdat(REC=6_SLK,DAT=testSDK)
      ASSERT(testSDK == EPSILON(testSDK),'testSDK value 4')
      CALL testDA32File%readdat(REC=871_SLK,DAT=testSDK1,IOSTAT=ioerr,NREC=n)
      ASSERT(ioerr == 0,'testSDK1 ioerr')
      ASSERT(n == 200,'testSDK1 n')
      ASSERT(testSDK1(1) == TINY(testSDK),'testSDK1(1) value')
      ASSERT(testSDK1(99) == HUGE(testSDK),'testSDK1(99) value')
      ASSERT(testSDK1(100) == 0,'testSDK1(100) value')
      DO i=2,98
        ASSERT(testSDK1(i) == 0,'testSDK1(i) value')
        FINFO() i
      ENDDO
      CALL testDA32File%readdat(REC=871_SLK,DAT=testSDK2,IOSTAT=ioerr,NREC=n)
      ASSERT(ioerr == 0,'testSDK2 ioerr')
      ASSERT(n == 200,'testSDK2 n')
      DO j=1,10
        DO i=1,10
          IF(i == 1 .AND. j == 1) THEN
            ASSERT(testSDK2(i,j) == TINY(testSDK),'testSDK2(i,j) value')
          ELSEIF(i == 9 .AND. j == 10) THEN
            ASSERT(testSDK2(i,j) == HUGE(testSDK),'testSDK2(i,j) value')
          ELSE
            ASSERT(testSDK2(i,j) == 0,'testSDK2(i,j) value')
          ENDIF
          FINFO() i,j,testSDK2(i,j)
        ENDDO
      ENDDO
      
      
      COMPONENT_TEST('read_char')
      testCHAR=''; n=0; ioerr=-1
      CALL testDA32File%readdat(REC=8_SLK,DAT=testCHAR,IOSTAT=ioerr,NREC=n)
      ASSERT(testCHAR == '12345 ','testCHAR value 1')
      FINFO() '"'//testCHAR//'"'
      ASSERT(n == 2,'n')
      ASSERT(ioerr == 0,'ioerr')
      testCHAR=''; n=0; ioerr=-1
      CALL testDA32File%readdat(REC=8_SLK,DAT=testCHAR,IOSTAT=ioerr)
      ASSERT(TRIM(testCHAR) == '12345','testCHAR value 2')
      FINFO() '"'//testCHAR//'"'
      ASSERT(ioerr == 0,'ioerr')
      testCHAR=''; n=0; ioerr=-1
      CALL testDA32File%readdat(REC=8_SLK,DAT=testCHAR,NREC=n)
      ASSERT(TRIM(testCHAR) == '12345','testCHAR value 3')
      FINFO() '"'//testCHAR//'"'
      ASSERT(n == 2,'n')
      testCHAR=''; n=0; ioerr=-1
      CALL testDA32File%readdat(REC=8_SLK,DAT=testCHAR)
      ASSERT(TRIM(testCHAR) == '12345','testCHAR value 4')
      FINFO() '"'//testCHAR//'"'
      
      COMPONENT_TEST('read big')
      CALL testDA32File%readdat(REC=512_SLK,DAT=test2SNK,IOSTAT=ioerr,NREC=n)
      ASSERT(ioerr == 0,'test2SNK ioerr')
      ASSERT(n == 258,'test2SNK n')
      DO i=1,258
        ASSERT(test2SNK(i) == i,'test2SNK value i')
        FINFO() i,test2SNK(i)
      ENDDO
      CALL testDA32File%fclose()
      CALL testDA32File%clear()
    ENDSUBROUTINE testRead
!
!-------------------------------------------------------------------------------
    SUBROUTINE testWrite()
      LOGICAL(SBK) :: testBool
      INTEGER(SNK) :: testSNK,testSNK1(100),test2SNK(258)
      INTEGER(SLK) :: testSLK,testSLK1(100)
      REAL(SSK) :: testSSK,testSSK1(100)
      REAL(SDK) :: testSDK,testSDK1(100)
      CHARACTER(LEN=5) :: testCHAR
      INTEGER(SIK) :: ioerr,i
      INTEGER(SLK) :: n
      
      CALL testDA32File%e%addSurrogate(e)
      CALL testDA32File%initialize(FILE='testReadfile.bin',STATUS='REPLACE', &
        ACTION='WRITE')
      CALL testDA32File%fopen()
      
      COMPONENT_TEST('write_sbk')
      testBool=.TRUE.; n=0; ioerr=-1
      CALL testDA32File%writedat(REC=1_SLK,DAT=testBool,IOSTAT=ioerr,NREC=n)
      ASSERT(n == 1,'n')
      ASSERT(ioerr == 0,'ioerr')
      testBool=.TRUE.; n=0; ioerr=-1
      CALL testDA32File%writedat(REC=1_SLK,DAT=testBool,IOSTAT=ioerr)
      ASSERT(ioerr == 0,'ioerr')
      testBool=.TRUE.; n=0; ioerr=-1
      CALL testDA32File%writedat(REC=1_SLK,DAT=testBool,NREC=n)
      ASSERT(n == 1,'n')
      testBool=.TRUE.; n=0; ioerr=-1
      CALL testDA32File%writedat(REC=1_SLK,DAT=testBool)

      FLUSH(testDA32File%getUnitNo())
      
      COMPONENT_TEST('write_snk')
      testSNK=HUGE(testSNK); n=0; ioerr=-1
      CALL testDA32File%writedat(REC=2_SLK,DAT=testSNK,IOSTAT=ioerr,NREC=n)
      ASSERT(n == 1,'n')
      ASSERT(ioerr == 0,'ioerr')
      testSNK=HUGE(testSNK); n=0; ioerr=-1
      CALL testDA32File%writedat(REC=2_SLK,DAT=testSNK,IOSTAT=ioerr)
      ASSERT(ioerr == 0,'ioerr')
      testSNK=HUGE(testSNK); n=0; ioerr=-1
      CALL testDA32File%writedat(REC=2_SLK,DAT=testSNK,NREC=n)
      ASSERT(n == 1,'n')
      testSNK=HUGE(testSNK); n=0; ioerr=-1
      CALL testDA32File%writedat(REC=2_SLK,DAT=testSNK)
      testSNK1=0
      testSNK1(1)=HUGE(testSNK)
      testSNK1(99)=HUGE(testSNK)
      CALL testDA32File%writedat(REC=10_SLK,DAT=testSNK1,IOSTAT=ioerr,NREC=n)
      ASSERT(ioerr == 0,'testSNK1 ioerr')
      ASSERT(n == 100,'testSNK1 n')
      
      COMPONENT_TEST('write_ssk')
      testSSK=EPSILON(testSSK); n=0; ioerr=-1
      CALL testDA32File%writedat(REC=5_SLK,DAT=testSSK,IOSTAT=ioerr,NREC=n)
      ASSERT(n == 1,'n')
      ASSERT(ioerr == 0,'ioerr')
      testSSK=EPSILON(testSSK); n=0; ioerr=-1
      CALL testDA32File%writedat(REC=5_SLK,DAT=testSSK,IOSTAT=ioerr)
      ASSERT(ioerr == 0,'ioerr')
      testSSK=EPSILON(testSSK); n=0; ioerr=-1
      CALL testDA32File%writedat(REC=5_SLK,DAT=testSSK,NREC=n)
      ASSERT(n == 1,'n')
      testSSK=EPSILON(testSSK); n=0; ioerr=-1
      CALL testDA32File%writedat(REC=5_SLK,DAT=testSSK)
      testSSK1=0.0_SSK
      testSSK1(1)=TINY(testSSK)
      testSSK1(99)=HUGE(testSSK)
      CALL testDA32File%writedat(REC=771_SLK,DAT=testSSK1,IOSTAT=ioerr,NREC=n)
      ASSERT(ioerr == 0,'testSSK1 ioerr')
      ASSERT(n == 100,'testSSK1 n')
      
      COMPONENT_TEST('write_slk')
      testSLK=HUGE(testSLK); n=0; ioerr=-1
      CALL testDA32File%writedat(REC=3_SLK,DAT=testSLK,IOSTAT=ioerr,NREC=n)
      ASSERT(n == 2,'n')
      ASSERT(ioerr == 0,'ioerr')
      testSLK=HUGE(testSLK); n=0; ioerr=-1
      CALL testDA32File%writedat(REC=3_SLK,DAT=testSLK,IOSTAT=ioerr)
      ASSERT(ioerr == 0,'ioerr')
      testSLK=HUGE(testSLK); n=0; ioerr=-1
      CALL testDA32File%writedat(REC=3_SLK,DAT=testSLK,NREC=n)
      ASSERT(n == 2,'n')
      testSLK=HUGE(testSLK); n=0; ioerr=-1
      CALL testDA32File%writedat(REC=3_SLK,DAT=testSLK)
      testSLK1=0
      testSLK1(1)=HUGE(testSLK)
      testSLK1(99)=HUGE(testSLK)
      CALL testDA32File%writedat(REC=110_SLK,DAT=testSLK1,IOSTAT=ioerr,NREC=n)
      ASSERT(ioerr == 0,'testSLK1 ioerr')
      ASSERT(n == 200,'testSLK1 n')
      
      COMPONENT_TEST('write_sdk')
      testSDK=EPSILON(testSDK); n=0; ioerr=-1
      CALL testDA32File%writedat(REC=6_SLK,DAT=testSDK,IOSTAT=ioerr,NREC=n)
      ASSERT(n == 2,'n')
      ASSERT(ioerr == 0,'ioerr')
      testSDK=EPSILON(testSDK); n=0; ioerr=-1
      CALL testDA32File%writedat(REC=6_SLK,DAT=testSDK,IOSTAT=ioerr)
      ASSERT(ioerr == 0,'ioerr')
      testSDK=EPSILON(testSDK); n=0; ioerr=-1
      CALL testDA32File%writedat(REC=6_SLK,DAT=testSDK,NREC=n)
      ASSERT(n == 2,'n')
      testSDK=EPSILON(testSDK); n=0; ioerr=-1
      CALL testDA32File%writedat(REC=6_SLK,DAT=testSDK)
      testSDK1=0.0_SDK
      testSDK1(1)=TINY(testSDK)
      testSDK1(99)=HUGE(testSDK)
      CALL testDA32File%writedat(REC=871_SLK,DAT=testSDK1,IOSTAT=ioerr,NREC=n)
      ASSERT(ioerr == 0,'testSDK1 ioerr')
      ASSERT(n == 200,'testSDK1 n')
      
      COMPONENT_TEST('write_char')
      testCHAR='12345'; n=0; ioerr=-1
      CALL testDA32File%writedat(REC=8_SLK,DAT=testCHAR,IOSTAT=ioerr,NREC=n)
      ASSERT(n == 2,'n')
      ASSERT(ioerr == 0,'ioerr')
      testCHAR='12345'; n=0; ioerr=-1
      CALL testDA32File%writedat(REC=8_SLK,DAT=testCHAR,IOSTAT=ioerr)
      ASSERT(ioerr == 0,'ioerr')
      testCHAR='12345'; n=0; ioerr=-1
      CALL testDA32File%writedat(REC=8_SLK,DAT=testCHAR,NREC=n)
      ASSERT(n == 2,'n')
      testCHAR='12345'; n=0; ioerr=-1
      CALL testDA32File%writedat(REC=8_SLK,DAT=testCHAR)
      
      COMPONENT_TEST('write big')
      DO i=1,258
        test2SNK(i)=i
      ENDDO
      CALL testDA32File%writedat(REC=512_SLK,DAT=test2SNK,IOSTAT=ioerr,NREC=n)
      ASSERT(ioerr == 0,'test2SNK ioerr')
      ASSERT(n == 258,'test2SNK n')
      CALL testDA32File%fclose()
      CALL testDA32File%clear()
      CALL testRead()
    ENDSUBROUTINE testWrite
!
!-------------------------------------------------------------------------------
    SUBROUTINE testPad()
      INTEGER(SIK) :: i,WORDSREC
      
      WORDSREC=RECLSZ/RECL32
      ASSERT(testDA32File%getPad2NextBlk(1_SLK) == 0,'i=1')
      DO i=2,WORDSREC
        ASSERT(testDA32File%getPad2NextBlk(INT(i,SLK)) == WORDSREC-i+1,'i')
        FINFO() i,testDA32File%getPad2NextBlk(INT(i,SLK))
      ENDDO
    ENDSUBROUTINE testPad
!
!-------------------------------------------------------------------------------
    SUBROUTINE testWMTBlock()
      INTEGER(SIK) :: i,idum,ioerr
      ioerr=0
      CALL testDA32File%writeEmptyBlock(REC=1_SLK,IOSTAT=ioerr)
      ASSERT(ioerr == IOSTAT_END,'Error check uninit')
      CALL testDA32File%initialize(FILE='testReadfile.bin',STATUS='REPLACE', &
        ACTION='READWRITE')
      ioerr=0
      CALL testDA32File%writeEmptyBlock(REC=1_SLK,IOSTAT=ioerr)
      ASSERT(ioerr == IOSTAT_END,'Error check not open')
      ioerr=0
      CALL testDA32File%fopen()
      CALL testDA32File%writeEmptyBlock(REC=1_SLK,IOSTAT=ioerr)
      DO i=1,RECLSZ/RECL32
        CALL testDA32File%readdat(REC=INT(i,SLK),DAT=idum,IOSTAT=ioerr)
        ASSERT(idum == 0,'idum read-check')
        ASSERT(ioerr == 0,'ioerr read-check')
      ENDDO
    ENDSUBROUTINE testWMTBlock
!
ENDPROGRAM testFileType_DA32
