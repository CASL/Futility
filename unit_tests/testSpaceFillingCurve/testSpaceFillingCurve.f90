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
PROGRAM testSpaceFillingCurve
      
  USE IntrType
  USE MortonOrdering
  IMPLICIT NONE
  
  INTEGER(SIK) :: i,j,k,iout,jout,kout,istt,istp,rx(2),ry(2),rz(2)
  
  TYPE(ZTreeNodeType) :: testZTree
  TYPE(ZTreeNodeType) :: tmpZTreeNode
  TYPE(ZTreeNodeType),POINTER :: tmpZTreeLevel(:)

  WRITE(*,*) '==================================================='
  WRITE(*,*) 'TESTING SPACE FILLING CURVES...'
  WRITE(*,*) '==================================================='
  WRITE(*,*) 'TESTING MORTON ORDERING'
!
!Test MortonIndex
  IF(MortonIndex(0,0,0) /= 0) THEN
    WRITE(*,*) 'MortonIndex(0,0,0) FAILED!'
    STOP 666
  ENDIF
  IF(MortonIndex(1,0,0) /= 1) THEN
    WRITE(*,*) 'MortonIndex(1,0,0) FAILED!'
    STOP 666
  ENDIF
  IF(MortonIndex(0,1,0) /= 2) THEN
    WRITE(*,*) 'MortonIndex(0,1,0) FAILED!'
    STOP 666
  ENDIF
  IF(MortonIndex(1,1,0) /= 3) THEN
    WRITE(*,*) 'MortonIndex(1,1,0) FAILED!'
    STOP 666
  ENDIF
  IF(MortonIndex(0,0,1) /= 4) THEN
    WRITE(*,*) 'MortonIndex(0,0,1) FAILED!'
    STOP 666
  ENDIF
  IF(MortonIndex(1,0,1) /= 5) THEN
    WRITE(*,*) 'MortonIndex(1,0,1) FAILED!'
    STOP 666
  ENDIF
  IF(MortonIndex(0,1,1) /= 6) THEN
    WRITE(*,*) 'MortonIndex(0,1,1) FAILED!'
    STOP 666
  ENDIF
  IF(MortonIndex(1,4,5) /= 389) THEN
    WRITE(*,*) 'MortonIndex(1,4,5) FAILED!'
    STOP 666
  ENDIF
  IF(MortonIndex(1,1,1) /= 7) THEN
    WRITE(*,*) 'MortonIndex(1,1,1) FAILED!'
    STOP 666
  ENDIF
  IF(MortonIndex(0,0) /= 0) THEN
    WRITE(*,*) 'MortonIndex(0,0) FAILED!'
    STOP 666
  ENDIF
  IF(MortonIndex(1,0) /= 1) THEN
    WRITE(*,*) 'MortonIndex(1,0) FAILED!'
    STOP 666
  ENDIF
  IF(MortonIndex(0,1) /= 2) THEN
    WRITE(*,*) 'MortonIndex(0,1) FAILED!'
    STOP 666
  ENDIF
  IF(MortonIndex(1,1) /= 3) THEN
    WRITE(*,*) 'MortonIndex(1,1) FAILED!'
    STOP 666
  ENDIF
  IF(MortonIndex(5,6) /= 57) THEN
    WRITE(*,*) 'MortonIndex(5,6) FAILED!'
    STOP 666
  ENDIF
  WRITE(*,*) '  Passed: MortonIndex(...)'
!
!Test Z-Tree methods for uninitialized state
  CALL testZTree%clear()
  IF(testZTree%istpMax() /= -1) THEN
    WRITE(*,*) 'testZTree%istp() uninitialized FAILED!'
    STOP 666
  ENDIF
  IF(testZTree%ijk2oneD(1,1,1) /= -1) THEN
    WRITE(*,*) 'testZTree%ijk2oneD(1,1,1) uninitialized FAILED!'
    STOP 666
  ENDIF
  IF(testZTree%ijk2oneD(0,0,0) /= -1) THEN
    WRITE(*,*) 'testZTree%ijk2oneD(0,0,0) uninitialized FAILED!'
    STOP 666
  ENDIF
  CALL testZTree%oneD2ijk(0,iout,jout,kout)
  IF(iout /= -1 .OR. jout /= -1 .OR. kout /= -1) THEN
    WRITE(*,*) 'CALL testZTree%oneD2ijk(0,iout,jout,kout) uninitialized FAILED!'
    STOP 666
  ENDIF
  CALL testZTree%oneD2ijk(-1,iout,jout,kout)
  IF(iout /= -1 .OR. jout /= -1 .OR. kout /= -1) THEN
    WRITE(*,*) 'CALL testZTree%oneD2ijk(-1,iout,jout,kout) uninitialized FAILED!'
    STOP 666
  ENDIF
!
!Test %init
  !Degenerate cases
  CALL testZTree%init(4,1,1,4,1,4,1)
  IF(ANY(testZTree%x /= 0) .OR. ANY(testZTree%y /= 0) .OR. &
    ANY(testZTree%z /= 0) .OR. testZTree%istt /= -1 .OR.  &
      testZTree%nsubdomains /= 0 .OR. ASSOCIATED(testZTree%subdomains)) THEN
    WRITE(*,*) 'CALL testZTree%init(4,1,1,4,1,4,1) FAILED!'
    STOP 666
  ENDIF
  CALL testZTree%init(1,4,4,1,1,4,1)
  IF(ANY(testZTree%x /= 0) .OR. ANY(testZTree%y /= 0) .OR. &
    ANY(testZTree%z /= 0) .OR. testZTree%istt /= -1 .OR.  &
      testZTree%nsubdomains /= 0 .OR. ASSOCIATED(testZTree%subdomains)) THEN
    WRITE(*,*) 'CALL testZTree%init(1,4,4,1,1,4,1) FAILED!'
    STOP 666
  ENDIF
  CALL testZTree%init(1,4,1,4,4,1,1)
  IF(ANY(testZTree%x /= 0) .OR. ANY(testZTree%y /= 0) .OR. &
    ANY(testZTree%z /= 0) .OR. testZTree%istt /= -1 .OR.  &
      testZTree%nsubdomains /= 0 .OR. ASSOCIATED(testZTree%subdomains)) THEN
    WRITE(*,*) 'CALL testZTree%init(1,4,1,4,4,1,1) FAILED!'
    STOP 666
  ENDIF
  CALL testZTree%init(1,4,1,4,1,4,-5)
  IF(ANY(testZTree%x /= 0) .OR. ANY(testZTree%y /= 0) .OR. &
    ANY(testZTree%z /= 0) .OR. testZTree%istt /= -1 .OR.  &
      testZTree%nsubdomains /= 0 .OR. ASSOCIATED(testZTree%subdomains)) THEN
    WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,-5) FAILED!'
    STOP 666
  ENDIF
  CALL testZTree%init(-1,4,1,4,1,4,1)
  IF(ANY(testZTree%x /= 0) .OR. ANY(testZTree%y /= 0) .OR. &
    ANY(testZTree%z /= 0) .OR. testZTree%istt /= -1 .OR.  &
      testZTree%nsubdomains /= 0 .OR. ASSOCIATED(testZTree%subdomains)) THEN
    WRITE(*,*) 'CALL testZTree%init(-1,4,1,4,1,4,1) FAILED!'
    STOP 666
  ENDIF
  CALL testZTree%init(-1,4,-1,4,1,4,1)
  IF(ANY(testZTree%x /= 0) .OR. ANY(testZTree%y /= 0) .OR. &
    ANY(testZTree%z /= 0) .OR. testZTree%istt /= -1 .OR.  &
      testZTree%nsubdomains /= 0 .OR. ASSOCIATED(testZTree%subdomains)) THEN
    WRITE(*,*) 'CALL testZTree%init(-1,4,-1,4,1,4,1) FAILED!'
    STOP 666
  ENDIF
  CALL testZTree%init(1,4,1,4,-1,4,1)
  IF(ANY(testZTree%x /= 0) .OR. ANY(testZTree%y /= 0) .OR. &
    ANY(testZTree%z /= 0) .OR. testZTree%istt /= -1 .OR.  &
      testZTree%nsubdomains /= 0 .OR. ASSOCIATED(testZTree%subdomains)) THEN
    WRITE(*,*) 'CALL testZTree%init(1,4,1,4,-1,4,1) FAILED!'
    STOP 666
  ENDIF
  !Correct case    
  CALL testZTree%init(1,4,1,4,1,4,1)
  IF(ANY(testZTree%x /= (/1,4/))) THEN
    WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %x FAILED!'
    STOP 666
  ENDIF
  IF(ANY(testZTree%y /= (/1,4/))) THEN
    WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %y FAILED!'
    STOP 666
  ENDIF
  IF(ANY(testZTree%z /= (/1,4/))) THEN
    WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %z FAILED!'
    STOP 666
  ENDIF
  IF(testZTree%istt /= 1) THEN
    WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %istt FAILED!'
    STOP 666
  ENDIF
  IF(testZTree%istp /= 64) THEN
    WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %istt FAILED!'
    STOP 666
  ENDIF
  IF(testZTree%nsubdomains /= 8 .OR. SIZE(testZTree%subdomains,DIM=1) /= 8) THEN
    WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %nsubdomains FAILED!'
    STOP 666
  ENDIF
  CALL checkLevel1() !Check level 1 domains
  CALL checkLevel2d1() !Check level 2 domain 1
  CALL checkLevel2d2() !Check level 2 domain 2
  CALL checkLevel2d3() !Check level 2 domain 3
  CALL checkLevel2d4() !Check level 2 domain 4
  CALL checkLevel2d5() !Check level 2 domain 5
  CALL checkLevel2d6() !Check level 2 domain 6
  CALL checkLevel2d7() !Check level 2 domain 7
  CALL checkLevel2d8() !Check level 2 domain 8
  CALL testZTree%init(1,4,1,4,1,4,1) !Error check
  
  CALL testZTree%clear()
  CALL testZTree%init(1,3,1,3,1,6,1)
  IF(testZTree%nsubdomains /= 2) THEN
    WRITE(*,*) 'testZTree%init(...) aspect ratio == 2 FAILED!'
    STOP 666
  ENDIF
  CALL testZTree%clear()
  CALL testZTree%init(1,4,1,4,1,4,1)
  WRITE(*,*) '  Passed: CALL testZTree%init(...)'
  
  !Test %istp()
  IF(testZTree%istpMax() /= 64) THEN
    WRITE(*,*) 'testZTree%istpMax() FAILED!'
    STOP 666
  ENDIF
  DO i=1,8
    IF(testZTree%subdomains(i)%istpMax() /= 8*i) THEN
      WRITE(*,*) 'testZTree%subdomains(i)%istpMax() FAILED!',i
      STOP 666
    ENDIF
    DO j=1,8
      IF(testZTree%subdomains(i)%subdomains(j)%istpMax() /= (i-1)*8+j) THEN
        WRITE(*,*) 'testZTree%subdomains(i)%subdomains(j)%istpMax() FAILED!',i,j
        STOP 666
      ENDIF
    ENDDO
  ENDDO
  WRITE(*,*) '  Passed: testZTree%istpMax()'
!
!Test %ijk2oneD
  !Degenerate cases
  IF(testZTree%ijk2oneD(0,1,1) /= -1) THEN
    WRITE(*,*) 'testZTree%ijk2oneD(0,1,1) FAILED!'
    STOP 666
  ENDIF
  IF(testZTree%ijk2oneD(1,0,1) /= -1) THEN
    WRITE(*,*) 'testZTree%ijk2oneD(1,0,1) FAILED!'
    STOP 666
  ENDIF
  IF(testZTree%ijk2oneD(1,1,0) /= -1) THEN
    WRITE(*,*) 'testZTree%ijk2oneD(1,1,0) FAILED!'
    STOP 666
  ENDIF
  IF(testZTree%ijk2oneD(5,1,1) /= -1) THEN
    WRITE(*,*) 'testZTree%ijk2oneD(5,1,1) FAILED!'
    STOP 666
  ENDIF
  IF(testZTree%ijk2oneD(1,5,1) /= -1) THEN
    WRITE(*,*) 'testZTree%ijk2oneD(1,5,1) FAILED!'
    STOP 666
  ENDIF
  IF(testZTree%ijk2oneD(1,1,5) /= -1) THEN
    WRITE(*,*) 'testZTree%ijk2oneD(1,1,5) FAILED!'
    STOP 666
  ENDIF
  !Correctness
  DO k=1,4
    DO j=1,4
      DO i=1,4
        IF(testZTree%ijk2oneD(i,j,k) /= MortonIndex(i-1,j-1,k-1)+1) THEN
          WRITE(*,*) 'testZTree%ijk2oneD(i,j,k) FAILED!',i,j,k
          STOP 666
        ENDIF
      ENDDO
    ENDDO
  ENDDO
  WRITE(*,*) '  Passed: testZTree%ijk2oneD(...)'
!
!Test %oneD2ijk
  !Degenerate cases
  CALL testZTree%oneD2ijk(-1,iout,jout,kout)
  IF(iout /= -1 .OR. jout /= -1 .OR. kout /= -1) THEN
    WRITE(*,*) 'CALL testZTree%oneD2ijk(-1,iout,jout,kout) FAILED!'
    STOP 666
  ENDIF
  CALL testZTree%oneD2ijk(65,iout,jout,kout)
  IF(iout /= -1 .OR. jout /= -1 .OR. kout /= -1) THEN
    WRITE(*,*) 'CALL testZTree%oneD2ijk(65,iout,jout,kout) FAILED!'
    STOP 666
  ENDIF
  !Correctness
  DO k=1,4
    DO j=1,4
      DO i=1,4
        CALL testZTree%oneD2ijk(MortonIndex(i-1,j-1,k-1)+1,iout,jout,kout)
        IF(i /= iout .OR. j /= jout .OR. k /= kout) THEN
          WRITE(*,*) 'CALL testZTree%oneD2ijk(...) FAILED!',i,j,k
          STOP 666
        ENDIF
      ENDDO
    ENDDO
  ENDDO
  WRITE(*,*) '  Passed: CALL testZTree%oneD2ijk(...)'
!
!Test %renumber 
  !Degenerate cases
  CALL testZTree%renumber(-1)
  DO k=1,4
    DO j=1,4
      DO i=1,4
        IF(testZTree%ijk2oneD(i,j,k) /= MortonIndex(i-1,j-1,k-1)+1) THEN
          WRITE(*,*) 'CALL testZTree%renumber(-1) FAILED!',i,j,k
          STOP 666
        ENDIF
      ENDDO
    ENDDO
  ENDDO
  testZTree%istt=-1
  CALL testZTree%renumber(1)
  DO k=1,4
    DO j=1,4
      DO i=1,4
        IF(testZTree%ijk2oneD(i,j,k) /= MortonIndex(i-1,j-1,k-1)+1) THEN
          WRITE(*,*) 'CALL testZTree%renumber(1) FAILED!',i,j,k
          STOP 666
        ENDIF
      ENDDO
    ENDDO
  ENDDO
  !Correctness
  testZTree%istt=1
  CALL testZTree%renumber(10)
  DO k=1,4
    DO j=1,4
      DO i=1,4
        IF(testZTree%ijk2oneD(i,j,k) /= MortonIndex(i-1,j-1,k-1)+10) THEN
          WRITE(*,*) 'CALL testZTree%renumber(10) FAILED!',i,j,k
          STOP 666
        ENDIF
      ENDDO
    ENDDO
  ENDDO

  CALL testZTree%clear()
  IF(ANY(testZTree%x /= 0)) THEN
    WRITE(*,*) 'testZTree%clear() %x FAILED!'
    STOP 666
  ENDIF
  IF(ANY(testZTree%y /= 0)) THEN
    WRITE(*,*) 'testZTree%clear() %y FAILED!'
    STOP 666
  ENDIF
  IF(ANY(testZTree%z /= 0)) THEN
    WRITE(*,*) 'testZTree%clear() %z FAILED!'
    STOP 666
  ENDIF
  IF(testZTree%istt /= -1) THEN
    WRITE(*,*) 'testZTree%clear() %istt FAILED!'
    STOP 666
  ENDIF
  IF(testZTree%istp /= -1) THEN
    WRITE(*,*) 'testZTree%clear() %istp FAILED!'
    STOP 666
  ENDIF
  IF(testZTree%nsubdomains /= 0) THEN
    WRITE(*,*) 'testZTree%clear() %nsubdomains FAILED!'
    STOP 666
  ENDIF
  IF(ASSOCIATED(testZTree%subdomains)) THEN
    WRITE(*,*) 'testZTree%clear() %subdomains FAILED!'
    STOP 666
  ENDIF
  WRITE(*,*) '  Passed: testZTree%clear()'
!
!Check a few other grids to test for preferential splitting and uneven splitting
  CALL testZTree%init(1,3,1,3,1,1,1)
  IF(testZTree%nsubdomains /= 4 .OR. testZTree%subdomains(1)%istt /= 1 .OR. &
    testZTree%subdomains(1)%nsubdomains /= 0 .OR. testZTree%subdomains(2)%istt /= 2 .OR. &
      testZTree%subdomains(2)%nsubdomains /= 2 .OR. testZTree%subdomains(3)%istt /= 4 .OR. &
        testZTree%subdomains(3)%nsubdomains /= 2 .OR. testZTree%subdomains(4)%istt /= 6 .OR. &
          testZTree%subdomains(4)%nsubdomains /= 4) THEN
    WRITE(*,*) 'CALL testZTree%init(1,3,1,3,1,1,1) FAILED!'
    STOP 666
  ENDIF
  CALL testZTree%clear()
  CALL testZTree%init(1,1,1,1,1,3,1)
  IF(testZTree%nsubdomains /= 2 .OR. testZTree%subdomains(1)%istt /= 1 .OR. &
      testZTree%subdomains(1)%nsubdomains /= 0 .OR. testZTree%subdomains(2)%istt /= 2 .OR. &
        testZTree%subdomains(2)%nsubdomains /= 2) THEN
    WRITE(*,*) 'CALL testZTree%init(1,1,1,1,1,3,1) FAILED!'
    STOP 666
  ENDIF
  
  CALL testZTree%clear()
  CALL testZTree%init(1,4,1,4,1,1,1)
  rx=1; ry=1; rz=1
  CALL testZTree%shave(rx,ry,rz)
  IF(testZTree%istp /= 15 .OR. testZTree%subdomains(1)%nsubdomains /= 3 .OR. &
    testZTree%subdomains(4)%istt /= 12 .OR. testZTree%ijk2oneD(1,1,1) /= -1) THEN
    WRITE(*,*) 'CALL testZTree%shave((/1,1/),(/1,1/),(/1,1/)) FAILED!'
    STOP 666
  ENDIF
  rx=4
  CALL testZTree%shave(rx,ry,rz)
  IF(testZTree%istp /= 14 .OR. testZTree%subdomains(2)%nsubdomains /= 3 .OR. &
    testZTree%subdomains(4)%istt /= 11 .OR. testZTree%ijk2oneD(4,1,1) /= -1) THEN
    WRITE(*,*) 'CALL testZTree%shave((/4,4/),(/1,1/),(/1,1/)) FAILED!'
    STOP 666
  ENDIF
  rx=1; ry=4
  CALL testZTree%shave(rx,ry,rz)
  IF(testZTree%istp /= 13 .OR. testZTree%subdomains(3)%nsubdomains /= 3 .OR. &
    testZTree%subdomains(4)%istt /= 10 .OR. testZTree%ijk2oneD(1,4,1) /= -1) THEN
    WRITE(*,*) 'CALL testZTree%shave((/1,1/),(/4,4/),(/1,1/)) FAILED!'
    STOP 666
  ENDIF
  rx=4
  CALL testZTree%shave(rx,ry,rz)
  IF(testZTree%istp /= 12 .OR. testZTree%subdomains(4)%nsubdomains /= 3 .OR. &
    testZTree%subdomains(4)%istt /= 10 .OR. testZTree%ijk2oneD(4,4,1) /= -1) THEN
    WRITE(*,*) 'CALL testZTree%shave((/4,4/),(/4,4/),(/1,1/)) FAILED!'
    STOP 666
  ENDIF
  rx=(/0,5/); ry=(/0,5/); rz=(/0,5/)
  CALL testZTree%shave(rx,ry,rz)
  IF(testZTree%istt /= -1) THEN
    WRITE(*,*) 'CALL testZTree%shave((/0,5/),(/0,5/),(/0,5/)) FAILED!'
    STOP 666
  ENDIF
  CALL testZTree%init(1,4,1,4,1,1,1)
  rx=2; ry=2; rz=1
  CALL testZTree%shave(rx,ry,rz)
  IF(testZTree%istp /= 15 .OR. testZTree%subdomains(1)%nsubdomains /= 3 .OR. &
    testZTree%subdomains(4)%istt /= 12 .OR. testZTree%ijk2oneD(2,2,1) /= -1) THEN
    WRITE(*,*) 'CALL testZTree%shave((/2,2/),(/2,2/),(/1,1/)) FAILED!'
    STOP 666
  ENDIF
  rx=(/3,4/); ry=(/3,4/)
  CALL testZTree%shave(rx,ry,rz)
  IF(testZTree%istp /= 11 .OR. testZTree%nsubdomains /= 3 .OR. &
    testZTree%subdomains(3)%istt /= 8 .OR. testZTree%ijk2oneD(2,2,1) /= -1) THEN
    WRITE(*,*) 'CALL testZTree%shave((/2,2/),(/2,2/),(/1,1/)) FAILED!'
    STOP 666
  ENDIF
  DO j=3,4
    DO i=3,4
      IF(testZTree%ijk2oneD(i,j,1) /= -1) THEN
        WRITE(*,*) 'CALL testZTree%shave((/2,2/),(/2,2/),(/1,1/)) FAILED!',i,j
        STOP 666
      ENDIF
    ENDDO
  ENDDO
  CALL testZTree%clear()
  CALL testZTree%init(1,4,1,4,1,1,1)
  rx=4; ry=4; rz=1
  CALL testZTree%shave(rx,ry,rz)
  rx=(/3,4/); ry=(/1,2/)
  CALL testZTree%shave(rx,ry,rz)
  IF(testZTree%istp /= 11 .OR. testZTree%nsubdomains /= 3 .OR. &
    testZTree%subdomains(3)%istt /= 9 .OR. testZTree%ijk2oneD(4,4,1) /= -1) THEN
    WRITE(*,*) 'CALL testZTree%shave((/3,4/),(/1,2/),(/1,1/)) FAILED!'
    STOP 666
  ENDIF
  DO j=1,2
    DO i=3,4
      IF(testZTree%ijk2oneD(i,j,1) /= -1) THEN
        WRITE(*,*) 'CALL testZTree%shave((/3,4/),(/1,2/),(/1,1/)) FAILED!',i,j
        STOP 666
      ENDIF
    ENDDO
  ENDDO
  CALL testZTree%clear()
  CALL testZTree%init(1,4,1,4,1,4,1)
  rx=(/2,3/); ry=(/2,3/); rz=(/2,3/)
  CALL testZTree%shave(rx,ry,rz)
  IF(testZTree%istp /= 56 .OR. testZTree%nsubdomains /= 8 .OR. &
    testZTree%subdomains(8)%istt /= 50) THEN
    WRITE(*,*) 'CALL testZTree%shave((/2,3/),(/2,3/),(/2,3/)) FAILED!'
    STOP 666
  ENDIF
  DO k=2,3
    DO j=2,3
      DO i=2,3
        IF(testZTree%ijk2oneD(i,j,k) /= -1) THEN
          WRITE(*,*) 'CALL testZTree%shave((/2,3/),(/2,3/),(/2,3/)) FAILED!',i,j,k
          STOP 666
        ENDIF
      ENDDO
    ENDDO
  ENDDO
  rx=2; ry=2; rz=2
  CALL testZTree%shave(rx,ry,rz)
  IF(testZTree%istp /= 56 .OR. testZTree%nsubdomains /= 8 .OR. &
    testZTree%subdomains(8)%istt /= 50) THEN
    WRITE(*,*) 'CALL testZTree%shave(...) (already shaved) FAILED!'
    STOP 666
  ENDIF
  DO k=2,3
    DO j=2,3
      DO i=2,3
        IF(testZTree%ijk2oneD(i,j,k) /= -1) THEN
          WRITE(*,*) 'CALL testZTree%shave(...) (already shaved) FAILED!',i,j,k
          STOP 666
        ENDIF
      ENDDO
    ENDDO
  ENDDO
  rx=25; ry=25; rz=25
  CALL testZTree%shave(rx,ry,rz)
  IF(testZTree%istp /= 56 .OR. testZTree%nsubdomains /= 8 .OR. &
    testZTree%subdomains(8)%istt /= 50) THEN
    WRITE(*,*) 'CALL testZTree%shave(...) (out of bounds) FAILED!'
    STOP 666
  ENDIF
  DO k=2,3
    DO j=2,3
      DO i=2,3
        IF(testZTree%ijk2oneD(i,j,k) /= -1) THEN
          WRITE(*,*) 'CALL testZTree%shave(...) (out of bounds) FAILED!',i,j,k
          STOP 666
        ENDIF
      ENDDO
    ENDDO
  ENDDO
  WRITE(*,*) '  Passed: testZTree%shave(...)'
!
!Test %getMaxLevels
  CALL testZTree%clear()
  IF(testZTree%getMaxLevels(0) /= -1) THEN
    WRITE(*,*) 'testZTree%getMaxLevels(0) (uninit) FAILED!'
    STOP 666
  ENDIF
  CALL testZTree%init(1,4,1,4,1,1,1)
  IF(testZTree%getMaxLevels(-1) /= -1) THEN
    WRITE(*,*) 'testZTree%getMaxLevels(-1) FAILED!'
    STOP 666
  ENDIF
  IF(testZTree%getMaxLevels(0) /= 2) THEN
    WRITE(*,*) 'testZTree%getMaxLevels(0) 4x4x1 FAILED!'
    STOP 666
  ENDIF
  CALL testZTree%clear()
  CALL testZTree%init(1,1,1,1,1,3,1)
  IF(testZTree%getMaxLevels(0) /= 2) THEN
    WRITE(*,*) 'testZTree%getMaxLevels(0) 1x1x3 FAILED!'
    STOP 666
  ENDIF
  CALL testZTree%clear()
  CALL testZTree%init(1,8,1,8,1,35,1)
  IF(testZTree%getMaxLevels(0) /= 6) THEN
    WRITE(*,*) 'testZTree%getMaxLevels(0) big FAILED!'
    STOP 666
  ENDIF
  CALL testZTree%clear()
  CALL testZTree%init(1,2,1,1,1,1,1)
  IF(testZTree%getMaxLevels(0) /= 1) THEN
    WRITE(*,*) 'testZTree%getMaxLevels(0) 2x1x1 FAILED!'
    STOP 666
  ENDIF
  CALL testZTree%clear()
  CALL testZTree%init(1,1,1,1,1,1,1)
  IF(testZTree%getMaxLevels(0) /= 0) THEN
    WRITE(*,*) 'testZTree%getMaxLevels(0) 1x1x1 FAILED!'
    STOP 666
  ENDIF
  WRITE(*,*) '  Passed: testZTree%getMaxLevels(0)'
  CALL testZTree%clear()
!
!Test %getNDomains
  IF(testZTree%getNDomains(4) /= 0) THEN
    WRITE(*,*) 'testZTree%getNDomains(4) (uninit) FAILED!'
    STOP 666
  ENDIF
  IF(testZTree%getNDomains(0) /= 0) THEN
    WRITE(*,*) 'testZTree%getNDomains(0) (uninit) FAILED!'
    STOP 666
  ENDIF
  CALL testZTree%init(1,4,1,4,1,1,1)
  IF(testZTree%getNDomains(0) /= 1) THEN
    WRITE(*,*) 'testZTree%getNDomains(0) 4x4x1 FAILED!'
    STOP 666
  ENDIF
  IF(testZTree%getNDomains(1) /= 4) THEN
    WRITE(*,*) 'testZTree%getNDomains(1) 4x4x1 FAILED!'
    STOP 666
  ENDIF
  IF(testZTree%getNDomains(2) /= 16) THEN
    WRITE(*,*) 'testZTree%getNDomains(2) 4x4x1 FAILED!'
    STOP 666
  ENDIF
  IF(testZTree%getNDomains(3) /= 0) THEN
    WRITE(*,*) 'testZTree%getNDomains(3) 4x4x1 FAILED!'
    STOP 666
  ENDIF
  CALL testZTree%clear()
  CALL testZTree%init(1,1,1,1,1,3,1)
  IF(testZTree%getNDomains(1) /= 2 .OR. testZTree%getNDomains(2) /= 2) THEN
    WRITE(*,*) 'testZTree%getNDomains(...) 1x1x3 FAILED!'
    STOP 666
  ENDIF
  CALL testZTree%clear()
  CALL testZTree%init(1,8,1,8,1,35,1)
  IF(testZTree%getNDomains(1) /= 2 .OR. testZTree%getNDomains(2) /= 4 .OR. &
    testZTree%getNDomains(3) /= 32 .OR. testZTree%getNDomains(4) /= 256 .OR. &
      testZTree%getNDomains(5) /= 2048 .OR. testZTree%getNDomains(6) /= 384) THEN
    WRITE(*,*) 'testZTree%getNDomains(...) 8x8x35 FAILED!'
    STOP 666
  ENDIF
  WRITE(*,*) '  Passed: testZTree%getNDomains(...)'
  CALL testZTree%clear()
!
!Test %getNodeBounds
  CALL testZTree%getSubNodeBounds(1,3,istt,istp)
  IF(istt /= -1 .OR. istp /= -1) THEN
    WRITE(*,*) 'CALL testZTree%getSubNodeBounds(1,3,istt,istp) (uninit) FAILED!'
    STOP 666
  ENDIF
  CALL testZTree%getSubNodeBounds(0,1,istt,istp)
  IF(istt /= -1 .OR. istp /= -1) THEN
    WRITE(*,*) 'CALL testZTree%getSubNodeBounds(0,1,istt,istp) (uninit) FAILED!'
    STOP 666
  ENDIF
  CALL testZTree%init(1,4,1,4,1,1,1)
  CALL testZTree%getSubNodeBounds(0,1,istt,istp)
  IF(istt /= 1 .OR. istp /= 16) THEN
    WRITE(*,*) 'CALL testZTree%getSubNodeBounds(0,1,istt,istp) 4x4x1 FAILED!'
    STOP 666
  ENDIF
  CALL testZTree%getSubNodeBounds(1,3,istt,istp)
  IF(istt /= 9 .OR. istp /= 12) THEN
    WRITE(*,*) 'CALL testZTree%getSubNodeBounds(1,3,istt,istp) 4x4x1 FAILED!'
    STOP 666
  ENDIF
  CALL testZTree%getSubNodeBounds(2,3,istt,istp)
  IF(istt /= 3 .OR. istp /= 3) THEN
    WRITE(*,*) 'CALL testZTree%getSubNodeBounds(2,3,istt,istp) 4x4x1 FAILED!'
    STOP 666
  ENDIF
  CALL testZTree%getSubNodeBounds(2,13,istt,istp)
  IF(istt /= 13 .OR. istp /= 13) THEN
    WRITE(*,*) 'CALL testZTree%getSubNodeBounds(2,13,istt,istp) 4x4x1 FAILED!'
    STOP 666
  ENDIF
  CALL testZTree%getSubNodeBounds(3,13,istt,istp)
  IF(istt /= -1 .OR. istp /= -1) THEN
    WRITE(*,*) 'CALL testZTree%getSubNodeBounds(3,13,istt,istp) 4x4x1 FAILED!'
    STOP 666
  ENDIF
  CALL testZTree%clear()
  CALL testZTree%init(1,8,1,8,1,35,1)
  CALL testZTree%getSubNodeBounds(6,13,istt,istp)
  IF(istt /= 821 .OR. istp /= 821) THEN
    WRITE(*,*) 'CALL testZTree%getSubNodeBounds(6,13,istt,istp) 8x8x35 FAILED!'
    STOP 666
  ENDIF
  CALL testZTree%getSubNodeBounds(5,400,istt,istp)
  IF(istt /= 400 .OR. istp /= 400) THEN
    WRITE(*,*) 'CALL testZTree%getSubNodeBounds(5,400,istt,istp) 8x8x35 FAILED!'
    STOP 666
  ENDIF
  CALL testZTree%getSubNodeBounds(6,400,istt,istp)
  IF(istt /= -1 .OR. istp /= -1) THEN
    WRITE(*,*) 'CALL testZTree%getSubNodeBounds(6,400,istt,istp) 8x8x35 FAILED!'
    STOP 666
  ENDIF
  WRITE(*,*) '  Passed: CALL testZTree%getSubNodeBounds(...)'
  CALL testZTree%clear()
!
!Test %partition
  CALL testZTree%partition(4,0,istt,istp)
  IF(istt /= -1 .OR. istp /= -1) THEN
    WRITE(*,*) 'CALL testZTree%partition(4,0,istt,istp) (uninit) FAILED!'
    STOP 666
  ENDIF
  CALL testZTree%init(1,4,1,4,1,1,1)
  CALL testZTree%partition(4,-1,istt,istp)
  IF(istt /= -1 .OR. istp /= -1) THEN
    WRITE(*,*) 'CALL testZTree%partition(4,-1,istt,istp) FAILED!'
    STOP 666
  ENDIF
  CALL testZTree%partition(4,4,istt,istp)
  IF(istt /= -1 .OR. istp /= -1) THEN
    WRITE(*,*) 'CALL testZTree%partition(4,4,istt,istp) FAILED!'
    STOP 666
  ENDIF
  CALL testZTree%partition(4,0,istt,istp)
  IF(istt /= 1 .OR. istp /= 4) THEN
    WRITE(*,*) 'CALL testZTree%partition(4,0,istt,istp) FAILED!'
    STOP 666
  ENDIF
  CALL testZTree%partition(16,2,istt,istp)
  IF(istt /= 3 .OR. istp /= 3) THEN
    WRITE(*,*) 'CALL testZTree%partition(16,2,istt,istp) FAILED!'
    STOP 666
  ENDIF
  CALL testZTree%partition(3,0,istt,istp)
  IF(istt /= 1 .OR. istp /= 8) THEN
    WRITE(*,*) 'CALL testZTree%partition(3,0,istt,istp) FAILED!'
    STOP 666
  ENDIF
  CALL testZTree%partition(3,1,istt,istp)
  IF(istt /= 9 .OR. istp /= 12) THEN
    WRITE(*,*) 'CALL testZTree%partition(3,1,istt,istp) FAILED!'
    STOP 666
  ENDIF
  CALL testZTree%partition(3,2,istt,istp)
  IF(istt /= 13 .OR. istp /= 16) THEN
    WRITE(*,*) 'CALL testZTree%partition(3,2,istt,istp) FAILED!'
    STOP 666
  ENDIF
  CALL testZTree%partition(5,0,istt,istp)
  IF(istt /= 1 .OR. istp /= 4) THEN
    WRITE(*,*) 'CALL testZTree%partition(5,0,istt,istp) FAILED!'
    STOP 666
  ENDIF
  CALL testZTree%partition(5,1,istt,istp)
  IF(istt /= 5 .OR. istp /= 7) THEN
    WRITE(*,*) 'CALL testZTree%partition(5,1,istt,istp) FAILED!'
    STOP 666
  ENDIF
  CALL testZTree%partition(5,2,istt,istp)
  IF(istt /= 8 .OR. istp /= 10) THEN
    WRITE(*,*) 'CALL testZTree%partition(5,2,istt,istp) FAILED!'
    STOP 666
  ENDIF
  CALL testZTree%partition(5,3,istt,istp)
  IF(istt /= 11 .OR. istp /= 13) THEN
    WRITE(*,*) 'CALL testZTree%partition(5,3,istt,istp) FAILED!'
    STOP 666
  ENDIF
  CALL testZTree%partition(5,4,istt,istp)
  IF(istt /= 14 .OR. istp /= 16) THEN
    WRITE(*,*) 'CALL testZTree%partition(5,4,istt,istp) FAILED!'
    STOP 666
  ENDIF
  CALL testZTree%clear()
  WRITE(*,*) '---------------------------------------------------'
  WRITE(*,*) '==================================================='
  WRITE(*,*) 'TESTING SPACE FILLING CURVES PASSED!'
  WRITE(*,*) '==================================================='
!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
    SUBROUTINE checkLevel1()
      tmpZTreeLevel => testZTree%subdomains
      
      tmpZTreeNode=tmpZTreeLevel(1) !Domain 1
      IF(ANY(tmpZTreeNode%x /= (/1,2/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(1)%x FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%y /= (/1,2/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(1)%y FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%z /= (/1,2/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(1)%z FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istt /= 1) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(1)%istt FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istp /= 8) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(1)%istp FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%nsubdomains /= 8 .OR. SIZE(tmpZTreeNode%subdomains,DIM=1) /= 8) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(1)%nsubdomains FAILED!'
        STOP 666
      ENDIF
      
      tmpZTreeNode=tmpZTreeLevel(2) !Domain 2
      IF(ANY(tmpZTreeNode%x /= (/3,4/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(2)%x FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%y /= (/1,2/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(2)%y FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%z /= (/1,2/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(2)%z FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istt /= 9) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(2)%istt FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istp /= 16) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(2)%istp FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%nsubdomains /= 8 .OR. SIZE(tmpZTreeNode%subdomains,DIM=1) /= 8) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(2)%nsubdomains FAILED!'
        STOP 666
      ENDIF
      
      tmpZTreeNode=tmpZTreeLevel(3) !Domain 3
      IF(ANY(tmpZTreeNode%x /= (/1,2/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(3)%x FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%y /= (/3,4/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(3)%y FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%z /= (/1,2/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(3)%z FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istt /= 17) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(3)%istt FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istp /= 24) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(3)%istp FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%nsubdomains /= 8 .OR. SIZE(tmpZTreeNode%subdomains,DIM=1) /= 8) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(3)%nsubdomains FAILED!'
        STOP 666
      ENDIF
      
      tmpZTreeNode=tmpZTreeLevel(4) !Domain 4
      IF(ANY(tmpZTreeNode%x /= (/3,4/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(4)%x FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%y /= (/3,4/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(4)%y FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%z /= (/1,2/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(4)%z FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istt /= 25) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(4)%istt FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istp /= 32) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(4)%istp FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%nsubdomains /= 8 .OR. SIZE(tmpZTreeNode%subdomains,DIM=1) /= 8) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(4)%nsubdomains FAILED!'
        STOP 666
      ENDIF
      
      tmpZTreeNode=tmpZTreeLevel(5) !Domain 5
      IF(ANY(tmpZTreeNode%x /= (/1,2/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(5)%x FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%y /= (/1,2/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(5)%y FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%z /= (/3,4/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(5)%z FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istt /= 33) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(5)%istt FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istp /= 40) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(5)%istp FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%nsubdomains /= 8 .OR. SIZE(tmpZTreeNode%subdomains,DIM=1) /= 8) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(5)%nsubdomains FAILED!'
        STOP 666
      ENDIF
      
      tmpZTreeNode=tmpZTreeLevel(6) !Domain 6
      IF(ANY(tmpZTreeNode%x /= (/3,4/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(6)%x FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%y /= (/1,2/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(6)%y FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%z /= (/3,4/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(6)%z FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istt /= 41) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(6)%istt FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istp /= 48) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(6)%istp FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%nsubdomains /= 8 .OR. SIZE(tmpZTreeNode%subdomains,DIM=1) /= 8) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(6)%nsubdomains FAILED!'
        STOP 666
      ENDIF
      
      tmpZTreeNode=tmpZTreeLevel(7) !Domain 7
      IF(ANY(tmpZTreeNode%x /= (/1,2/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(7)%x FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%y /= (/3,4/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(7)%y FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%z /= (/3,4/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(7)%z FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istt /= 49) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(7)%istt FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istp /= 56) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(7)%istp FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%nsubdomains /= 8 .OR. SIZE(tmpZTreeNode%subdomains,DIM=1) /= 8) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(7)%nsubdomains FAILED!'
        STOP 666
      ENDIF
      
      tmpZTreeNode=tmpZTreeLevel(8) !Domain 8
      IF(ANY(tmpZTreeNode%x /= (/3,4/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(8)%x FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%y /= (/3,4/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(8)%y FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%z /= (/3,4/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(8)%z FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istt /= 57) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(8)%istt FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istp /= 64) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(8)%istp FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%nsubdomains /= 8 .OR. SIZE(tmpZTreeNode%subdomains,DIM=1) /= 8) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(8)%nsubdomains FAILED!'
        STOP 666
      ENDIF
    ENDSUBROUTINE checkLevel1
!
!-------------------------------------------------------------------------------
    SUBROUTINE checkLevel2d1()
      tmpZTreeLevel => testZTree%subdomains(1)%subdomains
      
      tmpZTreeNode=tmpZTreeLevel(1) !Domain 1
      IF(ANY(tmpZTreeNode%x /= (/1,1/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(1)%subdomains(1)%x FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%y /= (/1,1/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(1)%subdomains(1)%y FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%z /= (/1,1/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(1)%subdomains(1)%z FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istt /= 1) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(1)%subdomains(1)%istt FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istp /= tmpZTreeNode%istt) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(1)%subdomains(1)%istp FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%nsubdomains /= 0 .OR. ASSOCIATED(tmpZTreeNode%subdomains)) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(1)%subdomains(1)%nsubdomains FAILED!'
        STOP 666
      ENDIF
      
      tmpZTreeNode=tmpZTreeLevel(2) !Domain 2
      IF(ANY(tmpZTreeNode%x /= (/2,2/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(1)%subdomains(2)%x FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%y /= (/1,1/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(1)%subdomains(2)%y FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%z /= (/1,1/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(1)%subdomains(2)%z FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istt /= 2) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(1)%subdomains(2)%istt FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istp /= tmpZTreeNode%istt) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(1)%subdomains(2)%istp FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%nsubdomains /= 0 .OR. ASSOCIATED(tmpZTreeNode%subdomains)) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(1)%subdomains(2)%nsubdomains FAILED!'
        STOP 666
      ENDIF
      
      tmpZTreeNode=tmpZTreeLevel(3) !Domain 3
      IF(ANY(tmpZTreeNode%x /= (/1,1/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(1)%subdomains(3)%x FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%y /= (/2,2/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(1)%subdomains(3)%y FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%z /= (/1,1/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(1)%subdomains(3)%z FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istt /= 3) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(1)%subdomains(3)%istt FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istp /= tmpZTreeNode%istt) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(1)%subdomains(3)%istp FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%nsubdomains /= 0 .OR. ASSOCIATED(tmpZTreeNode%subdomains)) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(1)%subdomains(3)%nsubdomains FAILED!'
        STOP 666
      ENDIF
      
      tmpZTreeNode=tmpZTreeLevel(4) !Domain 4
      IF(ANY(tmpZTreeNode%x /= (/2,2/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(1)%subdomains(4)%x FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%y /= (/2,2/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(1)%subdomains(4)%y FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%z /= (/1,1/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(1)%subdomains(4)%z FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istt /= 4) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(1)%subdomains(4)%istt FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istp /= tmpZTreeNode%istt) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(1)%subdomains(4)%istp FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%nsubdomains /= 0 .OR. ASSOCIATED(tmpZTreeNode%subdomains)) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(1)%subdomains(4)%nsubdomains FAILED!'
        STOP 666
      ENDIF
      
      tmpZTreeNode=tmpZTreeLevel(5) !Domain 5
      IF(ANY(tmpZTreeNode%x /= (/1,1/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(1)%subdomains(1)%subdomains(5)%x FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%y /= (/1,1/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(1)%subdomains(1)%subdomains(5)%y FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%z /= (/2,2/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(1)%subdomains(5)%z FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istt /= 5) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(1)%subdomains(5)%istt FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istp /= tmpZTreeNode%istt) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(1)%subdomains(5)%istp FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%nsubdomains /= 0 .OR. ASSOCIATED(tmpZTreeNode%subdomains)) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(1)%subdomains(5)%nsubdomains FAILED!'
        STOP 666
      ENDIF
      
      tmpZTreeNode=tmpZTreeLevel(6) !Domain 6
      IF(ANY(tmpZTreeNode%x /= (/2,2/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(1)%subdomains(6)%x FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%y /= (/1,1/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(1)%subdomains(6)%y FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%z /= (/2,2/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(1)%subdomains(6)%z FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istt /= 6) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(1)%subdomains(6)%istt FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istp /= tmpZTreeNode%istt) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(1)%subdomains(6)%istp FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%nsubdomains /= 0 .OR. ASSOCIATED(tmpZTreeNode%subdomains)) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(1)%subdomains(6)%nsubdomains FAILED!'
        STOP 666
      ENDIF
      
      tmpZTreeNode=tmpZTreeLevel(7) !Domain 7
      IF(ANY(tmpZTreeNode%x /= (/1,1/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(1)%subdomains(7)%x FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%y /= (/2,2/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(1)%subdomains(7)%y FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%z /= (/2,2/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(1)%subdomains(7)%z FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istt /= 7) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(1)%subdomains(7)%istt FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istp /= tmpZTreeNode%istt) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(1)%subdomains(7)%istp FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%nsubdomains /= 0 .OR. ASSOCIATED(tmpZTreeNode%subdomains)) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(1)%subdomains(7)%nsubdomains FAILED!'
        STOP 666
      ENDIF
      
      tmpZTreeNode=tmpZTreeLevel(8) !Domain 8
      IF(ANY(tmpZTreeNode%x /= (/2,2/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(1)%subdomains(8)%x FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%y /= (/2,2/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(1)%subdomains(8)%y FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%z /= (/2,2/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(1)%subdomains(8)%z FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istt /= 8) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(1)%subdomains(8)%istt FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istp /= tmpZTreeNode%istt) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(1)%subdomains(8)%istp FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%nsubdomains /= 0 .OR. ASSOCIATED(tmpZTreeNode%subdomains)) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(1)%subdomains(8)%nsubdomains FAILED!'
        STOP 666
      ENDIF
    ENDSUBROUTINE checkLevel2d1
!
!-------------------------------------------------------------------------------
    SUBROUTINE checkLevel2d2()
      tmpZTreeLevel => testZTree%subdomains(2)%subdomains
      
      tmpZTreeNode=tmpZTreeLevel(1) !Domain 1
      IF(ANY(tmpZTreeNode%x /= (/3,3/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(2)%subdomains(1)%x FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%y /= (/1,1/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(2)%subdomains(1)%y FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%z /= (/1,1/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(2)%subdomains(1)%z FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istt /= 9) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(2)%subdomains(1)%istt FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istp /= tmpZTreeNode%istt) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(2)%subdomains(1)%istp FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%nsubdomains /= 0 .OR. ASSOCIATED(tmpZTreeNode%subdomains)) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(2)%subdomains(1)%nsubdomains FAILED!'
        STOP 666
      ENDIF
      
      tmpZTreeNode=tmpZTreeLevel(2) !Domain 2
      IF(ANY(tmpZTreeNode%x /= (/4,4/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(2)%subdomains(2)%x FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%y /= (/1,1/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(2)%subdomains(2)%y FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%z /= (/1,1/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(2)%subdomains(2)%z FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istt /= 10) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(2)%subdomains(2)%istt FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istp /= tmpZTreeNode%istt) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(2)%subdomains(2)%istp FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%nsubdomains /= 0 .OR. ASSOCIATED(tmpZTreeNode%subdomains)) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(2)%subdomains(2)%nsubdomains FAILED!'
        STOP 666
      ENDIF
      
      tmpZTreeNode=tmpZTreeLevel(3) !Domain 3
      IF(ANY(tmpZTreeNode%x /= (/3,3/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(2)%subdomains(3)%x FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%y /= (/2,2/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(2)%subdomains(3)%y FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%z /= (/1,1/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(2)%subdomains(3)%z FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istt /= 11) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(2)%subdomains(3)%istt FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istp /= tmpZTreeNode%istt) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(2)%subdomains(3)%istp FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%nsubdomains /= 0 .OR. ASSOCIATED(tmpZTreeNode%subdomains)) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(2)%subdomains(3)%nsubdomains FAILED!'
        STOP 666
      ENDIF
      
      tmpZTreeNode=tmpZTreeLevel(4) !Domain 4
      IF(ANY(tmpZTreeNode%x /= (/4,4/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(2)%subdomains(4)%x FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%y /= (/2,2/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(2)%subdomains(4)%y FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%z /= (/1,1/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(2)%subdomains(4)%z FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istt /= 12) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(2)%subdomains(4)%istt FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istp /= tmpZTreeNode%istt) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(2)%subdomains(4)%istp FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%nsubdomains /= 0 .OR. ASSOCIATED(tmpZTreeNode%subdomains)) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(2)%subdomains(4)%nsubdomains FAILED!'
        STOP 666
      ENDIF
      
      tmpZTreeNode=tmpZTreeLevel(5) !Domain 5
      IF(ANY(tmpZTreeNode%x /= (/3,3/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(2)%subdomains(1)%subdomains(5)%x FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%y /= (/1,1/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(2)%subdomains(1)%subdomains(5)%y FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%z /= (/2,2/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(2)%subdomains(5)%z FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istt /= 13) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(2)%subdomains(5)%istt FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istp /= tmpZTreeNode%istt) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(2)%subdomains(5)%istp FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%nsubdomains /= 0 .OR. ASSOCIATED(tmpZTreeNode%subdomains)) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(2)%subdomains(5)%nsubdomains FAILED!'
        STOP 666
      ENDIF
      
      tmpZTreeNode=tmpZTreeLevel(6) !Domain 6
      IF(ANY(tmpZTreeNode%x /= (/4,4/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(2)%subdomains(6)%x FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%y /= (/1,1/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(2)%subdomains(6)%y FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%z /= (/2,2/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(2)%subdomains(6)%z FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istt /= 14) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(2)%subdomains(6)%istt FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istp /= tmpZTreeNode%istt) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(2)%subdomains(6)%istp FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%nsubdomains /= 0 .OR. ASSOCIATED(tmpZTreeNode%subdomains)) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(2)%subdomains(6)%nsubdomains FAILED!'
        STOP 666
      ENDIF
      
      tmpZTreeNode=tmpZTreeLevel(7) !Domain 7
      IF(ANY(tmpZTreeNode%x /= (/3,3/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(2)%subdomains(7)%x FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%y /= (/2,2/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(2)%subdomains(7)%y FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%z /= (/2,2/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(2)%subdomains(7)%z FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istt /= 15) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(2)%subdomains(7)%istt FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istp /= tmpZTreeNode%istt) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(2)%subdomains(7)%istp FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%nsubdomains /= 0 .OR. ASSOCIATED(tmpZTreeNode%subdomains)) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(2)%subdomains(7)%nsubdomains FAILED!'
        STOP 666
      ENDIF
      
      tmpZTreeNode=tmpZTreeLevel(8) !Domain 8
      IF(ANY(tmpZTreeNode%x /= (/4,4/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(2)%subdomains(8)%x FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%y /= (/2,2/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(2)%subdomains(8)%y FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%z /= (/2,2/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(2)%subdomains(8)%z FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istt /= 16) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(2)%subdomains(8)%istt FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istp /= tmpZTreeNode%istt) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(2)%subdomains(8)%istp FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%nsubdomains /= 0 .OR. ASSOCIATED(tmpZTreeNode%subdomains)) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(2)%subdomains(8)%nsubdomains FAILED!'
        STOP 666
      ENDIF
    ENDSUBROUTINE checkLevel2d2
!
!-------------------------------------------------------------------------------
    SUBROUTINE checkLevel2d3()
      tmpZTreeLevel => testZTree%subdomains(3)%subdomains
      
      tmpZTreeNode=tmpZTreeLevel(1) !Domain 1
      IF(ANY(tmpZTreeNode%x /= (/1,1/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(3)%subdomains(1)%x FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%y /= (/3,3/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(3)%subdomains(1)%y FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%z /= (/1,1/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(3)%subdomains(1)%z FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istt /= 17) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(3)%subdomains(1)%istt FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istp /= tmpZTreeNode%istt) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(3)%subdomains(1)%istp FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%nsubdomains /= 0 .OR. ASSOCIATED(tmpZTreeNode%subdomains)) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(3)%subdomains(1)%nsubdomains FAILED!'
        STOP 666
      ENDIF
      
      tmpZTreeNode=tmpZTreeLevel(2) !Domain 2
      IF(ANY(tmpZTreeNode%x /= (/2,2/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(3)%subdomains(2)%x FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%y /= (/3,3/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(3)%subdomains(2)%y FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%z /= (/1,1/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(3)%subdomains(2)%z FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istt /= 18) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(3)%subdomains(2)%istt FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istp /= tmpZTreeNode%istt) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(3)%subdomains(2)%istp FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%nsubdomains /= 0 .OR. ASSOCIATED(tmpZTreeNode%subdomains)) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(3)%subdomains(2)%nsubdomains FAILED!'
        STOP 666
      ENDIF
      
      tmpZTreeNode=tmpZTreeLevel(3) !Domain 3
      IF(ANY(tmpZTreeNode%x /= (/1,1/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(3)%subdomains(3)%x FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%y /= (/4,4/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(3)%subdomains(3)%y FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%z /= (/1,1/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(3)%subdomains(3)%z FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istt /= 19) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(3)%subdomains(3)%istt FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istp /= tmpZTreeNode%istt) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(3)%subdomains(3)%istp FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%nsubdomains /= 0 .OR. ASSOCIATED(tmpZTreeNode%subdomains)) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(3)%subdomains(3)%nsubdomains FAILED!'
        STOP 666
      ENDIF
      
      tmpZTreeNode=tmpZTreeLevel(4) !Domain 4
      IF(ANY(tmpZTreeNode%x /= (/2,2/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(3)%subdomains(4)%x FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%y /= (/4,4/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(3)%subdomains(4)%y FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%z /= (/1,1/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(3)%subdomains(4)%z FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istt /= 20) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(3)%subdomains(4)%istt FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istp /= tmpZTreeNode%istt) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(3)%subdomains(4)%istp FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%nsubdomains /= 0 .OR. ASSOCIATED(tmpZTreeNode%subdomains)) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(3)%subdomains(4)%nsubdomains FAILED!'
        STOP 666
      ENDIF
      
      tmpZTreeNode=tmpZTreeLevel(5) !Domain 5
      IF(ANY(tmpZTreeNode%x /= (/1,1/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(3)%subdomains(1)%subdomains(5)%x FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%y /= (/3,3/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(3)%subdomains(1)%subdomains(5)%y FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%z /= (/2,2/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(3)%subdomains(5)%z FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istt /= 21) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(3)%subdomains(5)%istt FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istp /= tmpZTreeNode%istt) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(3)%subdomains(5)%istp FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%nsubdomains /= 0 .OR. ASSOCIATED(tmpZTreeNode%subdomains)) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(3)%subdomains(5)%nsubdomains FAILED!'
        STOP 666
      ENDIF
      
      tmpZTreeNode=tmpZTreeLevel(6) !Domain 6
      IF(ANY(tmpZTreeNode%x /= (/2,2/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(3)%subdomains(6)%x FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%y /= (/3,3/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(3)%subdomains(6)%y FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%z /= (/2,2/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(3)%subdomains(6)%z FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istt /= 22) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(3)%subdomains(6)%istt FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istp /= tmpZTreeNode%istt) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(3)%subdomains(6)%istp FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%nsubdomains /= 0 .OR. ASSOCIATED(tmpZTreeNode%subdomains)) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(3)%subdomains(6)%nsubdomains FAILED!'
        STOP 666
      ENDIF
      
      tmpZTreeNode=tmpZTreeLevel(7) !Domain 7
      IF(ANY(tmpZTreeNode%x /= (/1,1/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(3)%subdomains(7)%x FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%y /= (/4,4/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(3)%subdomains(7)%y FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%z /= (/2,2/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(3)%subdomains(7)%z FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istt /= 23) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(3)%subdomains(7)%istt FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istp /= tmpZTreeNode%istt) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(3)%subdomains(7)%istp FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%nsubdomains /= 0 .OR. ASSOCIATED(tmpZTreeNode%subdomains)) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(3)%subdomains(7)%nsubdomains FAILED!'
        STOP 666
      ENDIF
      
      tmpZTreeNode=tmpZTreeLevel(8) !Domain 8
      IF(ANY(tmpZTreeNode%x /= (/2,2/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(3)%subdomains(8)%x FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%y /= (/4,4/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(3)%subdomains(8)%y FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%z /= (/2,2/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(3)%subdomains(8)%z FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istt /= 24) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(3)%subdomains(8)%istt FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istp /= tmpZTreeNode%istt) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(3)%subdomains(8)%istp FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%nsubdomains /= 0 .OR. ASSOCIATED(tmpZTreeNode%subdomains)) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(3)%subdomains(8)%nsubdomains FAILED!'
        STOP 666
      ENDIF
    ENDSUBROUTINE checkLevel2d3
!
!-------------------------------------------------------------------------------
    SUBROUTINE checkLevel2d4()
      tmpZTreeLevel => testZTree%subdomains(4)%subdomains
      
      tmpZTreeNode=tmpZTreeLevel(1) !Domain 1
      IF(ANY(tmpZTreeNode%x /= (/3,3/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(4)%subdomains(1)%x FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%y /= (/3,3/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(4)%subdomains(1)%y FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%z /= (/1,1/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(4)%subdomains(1)%z FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istt /= 25) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(4)%subdomains(1)%istt FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istp /= tmpZTreeNode%istt) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(4)%subdomains(1)%istp FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%nsubdomains /= 0 .OR. ASSOCIATED(tmpZTreeNode%subdomains)) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(4)%subdomains(1)%nsubdomains FAILED!'
        STOP 666
      ENDIF
      
      tmpZTreeNode=tmpZTreeLevel(2) !Domain 2
      IF(ANY(tmpZTreeNode%x /= (/4,4/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(4)%subdomains(2)%x FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%y /= (/3,3/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(4)%subdomains(2)%y FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%z /= (/1,1/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(4)%subdomains(2)%z FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istt /= 26) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(4)%subdomains(2)%istt FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istp /= tmpZTreeNode%istt) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(4)%subdomains(2)%istp FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%nsubdomains /= 0 .OR. ASSOCIATED(tmpZTreeNode%subdomains)) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(4)%subdomains(2)%nsubdomains FAILED!'
        STOP 666
      ENDIF
      
      tmpZTreeNode=tmpZTreeLevel(3) !Domain 3
      IF(ANY(tmpZTreeNode%x /= (/3,3/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(4)%subdomains(3)%x FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%y /= (/4,4/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(4)%subdomains(3)%y FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%z /= (/1,1/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(4)%subdomains(3)%z FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istt /= 27) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(4)%subdomains(3)%istt FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istp /= tmpZTreeNode%istt) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(4)%subdomains(3)%istp FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%nsubdomains /= 0 .OR. ASSOCIATED(tmpZTreeNode%subdomains)) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(4)%subdomains(3)%nsubdomains FAILED!'
        STOP 666
      ENDIF
      
      tmpZTreeNode=tmpZTreeLevel(4) !Domain 4
      IF(ANY(tmpZTreeNode%x /= (/4,4/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(4)%subdomains(4)%x FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%y /= (/4,4/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(4)%subdomains(4)%y FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%z /= (/1,1/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(4)%subdomains(4)%z FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istt /= 28) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(4)%subdomains(4)%istt FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istp /= tmpZTreeNode%istt) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(4)%subdomains(4)%istp FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%nsubdomains /= 0 .OR. ASSOCIATED(tmpZTreeNode%subdomains)) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(4)%subdomains(4)%nsubdomains FAILED!'
        STOP 666
      ENDIF
      
      tmpZTreeNode=tmpZTreeLevel(5) !Domain 5
      IF(ANY(tmpZTreeNode%x /= (/3,3/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(4)%subdomains(1)%subdomains(5)%x FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%y /= (/3,3/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(4)%subdomains(1)%subdomains(5)%y FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%z /= (/2,2/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(4)%subdomains(5)%z FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istt /= 29) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(4)%subdomains(5)%istt FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istp /= tmpZTreeNode%istt) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(4)%subdomains(5)%istp FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%nsubdomains /= 0 .OR. ASSOCIATED(tmpZTreeNode%subdomains)) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(4)%subdomains(5)%nsubdomains FAILED!'
        STOP 666
      ENDIF
      
      tmpZTreeNode=tmpZTreeLevel(6) !Domain 6
      IF(ANY(tmpZTreeNode%x /= (/4,4/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(4)%subdomains(6)%x FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%y /= (/3,3/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(4)%subdomains(6)%y FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%z /= (/2,2/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(4)%subdomains(6)%z FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istt /= 30) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(4)%subdomains(6)%istt FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istp /= tmpZTreeNode%istt) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(4)%subdomains(6)%istp FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%nsubdomains /= 0 .OR. ASSOCIATED(tmpZTreeNode%subdomains)) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(4)%subdomains(6)%nsubdomains FAILED!'
        STOP 666
      ENDIF
      
      tmpZTreeNode=tmpZTreeLevel(7) !Domain 7
      IF(ANY(tmpZTreeNode%x /= (/3,3/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(4)%subdomains(7)%x FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%y /= (/4,4/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(4)%subdomains(7)%y FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%z /= (/2,2/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(4)%subdomains(7)%z FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istt /= 31) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(4)%subdomains(7)%istt FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istp /= tmpZTreeNode%istt) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(4)%subdomains(7)%istp FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%nsubdomains /= 0 .OR. ASSOCIATED(tmpZTreeNode%subdomains)) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(4)%subdomains(7)%nsubdomains FAILED!'
        STOP 666
      ENDIF
      
      tmpZTreeNode=tmpZTreeLevel(8) !Domain 8
      IF(ANY(tmpZTreeNode%x /= (/4,4/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(4)%subdomains(8)%x FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%y /= (/4,4/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(4)%subdomains(8)%y FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%z /= (/2,2/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(4)%subdomains(8)%z FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istt /= 32) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(4)%subdomains(8)%istt FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istp /= tmpZTreeNode%istt) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(4)%subdomains(8)%istp FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%nsubdomains /= 0 .OR. ASSOCIATED(tmpZTreeNode%subdomains)) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(4)%subdomains(8)%nsubdomains FAILED!'
        STOP 666
      ENDIF
    ENDSUBROUTINE checkLevel2d4
!
!-------------------------------------------------------------------------------
    SUBROUTINE checkLevel2d5()
      tmpZTreeLevel => testZTree%subdomains(5)%subdomains
      
      tmpZTreeNode=tmpZTreeLevel(1) !Domain 1
      IF(ANY(tmpZTreeNode%x /= (/1,1/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(5)%subdomains(1)%x FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%y /= (/1,1/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(5)%subdomains(1)%y FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%z /= (/3,3/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(5)%subdomains(1)%z FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istt /= 33) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(5)%subdomains(1)%istt FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istp /= tmpZTreeNode%istt) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(5)%subdomains(1)%istp FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%nsubdomains /= 0 .OR. ASSOCIATED(tmpZTreeNode%subdomains)) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(5)%subdomains(1)%nsubdomains FAILED!'
        STOP 666
      ENDIF
      
      tmpZTreeNode=tmpZTreeLevel(2) !Domain 2
      IF(ANY(tmpZTreeNode%x /= (/2,2/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(5)%subdomains(2)%x FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%y /= (/1,1/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(5)%subdomains(2)%y FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%z /= (/3,3/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(5)%subdomains(2)%z FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istt /= 34) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(5)%subdomains(2)%istt FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istp /= tmpZTreeNode%istt) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(5)%subdomains(2)%istp FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%nsubdomains /= 0 .OR. ASSOCIATED(tmpZTreeNode%subdomains)) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(5)%subdomains(2)%nsubdomains FAILED!'
        STOP 666
      ENDIF
      
      tmpZTreeNode=tmpZTreeLevel(3) !Domain 3
      IF(ANY(tmpZTreeNode%x /= (/1,1/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(5)%subdomains(3)%x FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%y /= (/2,2/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(5)%subdomains(3)%y FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%z /= (/3,3/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(5)%subdomains(3)%z FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istt /= 35) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(5)%subdomains(3)%istt FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istp /= tmpZTreeNode%istt) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(5)%subdomains(3)%istp FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%nsubdomains /= 0 .OR. ASSOCIATED(tmpZTreeNode%subdomains)) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(5)%subdomains(3)%nsubdomains FAILED!'
        STOP 666
      ENDIF
      
      tmpZTreeNode=tmpZTreeLevel(4) !Domain 4
      IF(ANY(tmpZTreeNode%x /= (/2,2/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(5)%subdomains(4)%x FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%y /= (/2,2/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(5)%subdomains(4)%y FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%z /= (/3,3/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(5)%subdomains(4)%z FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istt /= 36) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(5)%subdomains(4)%istt FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istp /= tmpZTreeNode%istt) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(5)%subdomains(4)%istp FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%nsubdomains /= 0 .OR. ASSOCIATED(tmpZTreeNode%subdomains)) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(5)%subdomains(4)%nsubdomains FAILED!'
        STOP 666
      ENDIF
      
      tmpZTreeNode=tmpZTreeLevel(5) !Domain 5
      IF(ANY(tmpZTreeNode%x /= (/1,1/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(5)%subdomains(1)%subdomains(5)%x FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%y /= (/1,1/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(5)%subdomains(1)%subdomains(5)%y FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%z /= (/4,4/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(5)%subdomains(5)%z FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istt /= 37) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(5)%subdomains(5)%istt FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istp /= tmpZTreeNode%istt) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(5)%subdomains(5)%istp FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%nsubdomains /= 0 .OR. ASSOCIATED(tmpZTreeNode%subdomains)) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(5)%subdomains(5)%nsubdomains FAILED!'
        STOP 666
      ENDIF
      
      tmpZTreeNode=tmpZTreeLevel(6) !Domain 6
      IF(ANY(tmpZTreeNode%x /= (/2,2/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(5)%subdomains(6)%x FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%y /= (/1,1/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(5)%subdomains(6)%y FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%z /= (/4,4/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(5)%subdomains(6)%z FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istt /= 38) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(5)%subdomains(6)%istt FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istp /= tmpZTreeNode%istt) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(5)%subdomains(6)%istp FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%nsubdomains /= 0 .OR. ASSOCIATED(tmpZTreeNode%subdomains)) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(5)%subdomains(6)%nsubdomains FAILED!'
        STOP 666
      ENDIF
      
      tmpZTreeNode=tmpZTreeLevel(7) !Domain 7
      IF(ANY(tmpZTreeNode%x /= (/1,1/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(5)%subdomains(7)%x FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%y /= (/2,2/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(5)%subdomains(7)%y FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%z /= (/4,4/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(5)%subdomains(7)%z FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istt /= 39) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(5)%subdomains(7)%istt FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istp /= tmpZTreeNode%istt) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(5)%subdomains(7)%istp FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%nsubdomains /= 0 .OR. ASSOCIATED(tmpZTreeNode%subdomains)) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(5)%subdomains(7)%nsubdomains FAILED!'
        STOP 666
      ENDIF
      
      tmpZTreeNode=tmpZTreeLevel(8) !Domain 8
      IF(ANY(tmpZTreeNode%x /= (/2,2/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(5)%subdomains(8)%x FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%y /= (/2,2/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(5)%subdomains(8)%y FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%z /= (/4,4/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(5)%subdomains(8)%z FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istt /= 40) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(5)%subdomains(8)%istt FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istp /= tmpZTreeNode%istt) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(5)%subdomains(8)%istp FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%nsubdomains /= 0 .OR. ASSOCIATED(tmpZTreeNode%subdomains)) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(5)%subdomains(8)%nsubdomains FAILED!'
        STOP 666
      ENDIF
    ENDSUBROUTINE checkLevel2d5
!
!-------------------------------------------------------------------------------
    SUBROUTINE checkLevel2d6()
      tmpZTreeLevel => testZTree%subdomains(6)%subdomains
      
      tmpZTreeNode=tmpZTreeLevel(1) !Domain 1
      IF(ANY(tmpZTreeNode%x /= (/3,3/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(6)%subdomains(1)%x FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%y /= (/1,1/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(6)%subdomains(1)%y FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%z /= (/3,3/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(6)%subdomains(1)%z FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istt /= 41) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(6)%subdomains(1)%istt FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istp /= tmpZTreeNode%istt) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(6)%subdomains(1)%istp FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%nsubdomains /= 0 .OR. ASSOCIATED(tmpZTreeNode%subdomains)) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(6)%subdomains(1)%nsubdomains FAILED!'
        STOP 666
      ENDIF
      
      tmpZTreeNode=tmpZTreeLevel(2) !Domain 2
      IF(ANY(tmpZTreeNode%x /= (/4,4/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(6)%subdomains(2)%x FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%y /= (/1,1/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(6)%subdomains(2)%y FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%z /= (/3,3/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(6)%subdomains(2)%z FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istt /= 42) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(6)%subdomains(2)%istt FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istp /= tmpZTreeNode%istt) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(6)%subdomains(2)%istp FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%nsubdomains /= 0 .OR. ASSOCIATED(tmpZTreeNode%subdomains)) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(6)%subdomains(2)%nsubdomains FAILED!'
        STOP 666
      ENDIF
      
      tmpZTreeNode=tmpZTreeLevel(3) !Domain 3
      IF(ANY(tmpZTreeNode%x /= (/3,3/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(6)%subdomains(3)%x FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%y /= (/2,2/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(6)%subdomains(3)%y FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%z /= (/3,3/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(6)%subdomains(3)%z FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istt /= 43) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(6)%subdomains(3)%istt FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istp /= tmpZTreeNode%istt) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(6)%subdomains(3)%istp FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%nsubdomains /= 0 .OR. ASSOCIATED(tmpZTreeNode%subdomains)) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(6)%subdomains(3)%nsubdomains FAILED!'
        STOP 666
      ENDIF
      
      tmpZTreeNode=tmpZTreeLevel(4) !Domain 4
      IF(ANY(tmpZTreeNode%x /= (/4,4/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(6)%subdomains(4)%x FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%y /= (/2,2/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(6)%subdomains(4)%y FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%z /= (/3,3/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(6)%subdomains(4)%z FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istt /= 44) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(6)%subdomains(4)%istt FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istp /= tmpZTreeNode%istt) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(6)%subdomains(4)%istp FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%nsubdomains /= 0 .OR. ASSOCIATED(tmpZTreeNode%subdomains)) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(6)%subdomains(4)%nsubdomains FAILED!'
        STOP 666
      ENDIF
      
      tmpZTreeNode=tmpZTreeLevel(5) !Domain 5
      IF(ANY(tmpZTreeNode%x /= (/3,3/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(6)%subdomains(1)%subdomains(5)%x FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%y /= (/1,1/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(6)%subdomains(1)%subdomains(5)%y FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%z /= (/4,4/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(6)%subdomains(5)%z FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istt /= 45) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(6)%subdomains(5)%istt FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istp /= tmpZTreeNode%istt) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(6)%subdomains(5)%istp FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%nsubdomains /= 0 .OR. ASSOCIATED(tmpZTreeNode%subdomains)) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(6)%subdomains(5)%nsubdomains FAILED!'
        STOP 666
      ENDIF
      
      tmpZTreeNode=tmpZTreeLevel(6) !Domain 6
      IF(ANY(tmpZTreeNode%x /= (/4,4/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(6)%subdomains(6)%x FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%y /= (/1,1/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(6)%subdomains(6)%y FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%z /= (/4,4/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(6)%subdomains(6)%z FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istt /= 46) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(6)%subdomains(6)%istt FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istp /= tmpZTreeNode%istt) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(6)%subdomains(6)%istp FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%nsubdomains /= 0 .OR. ASSOCIATED(tmpZTreeNode%subdomains)) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(6)%subdomains(6)%nsubdomains FAILED!'
        STOP 666
      ENDIF
      
      tmpZTreeNode=tmpZTreeLevel(7) !Domain 7
      IF(ANY(tmpZTreeNode%x /= (/3,3/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(6)%subdomains(7)%x FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%y /= (/2,2/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(6)%subdomains(7)%y FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%z /= (/4,4/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(6)%subdomains(7)%z FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istt /= 47) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(6)%subdomains(7)%istt FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istp /= tmpZTreeNode%istt) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(6)%subdomains(7)%istp FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%nsubdomains /= 0 .OR. ASSOCIATED(tmpZTreeNode%subdomains)) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(6)%subdomains(7)%nsubdomains FAILED!'
        STOP 666
      ENDIF
      
      tmpZTreeNode=tmpZTreeLevel(8) !Domain 8
      IF(ANY(tmpZTreeNode%x /= (/4,4/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(6)%subdomains(8)%x FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%y /= (/2,2/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(6)%subdomains(8)%y FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%z /= (/4,4/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(6)%subdomains(8)%z FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istt /= 48) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(6)%subdomains(8)%istt FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istp /= tmpZTreeNode%istt) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(6)%subdomains(8)%istp FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%nsubdomains /= 0 .OR. ASSOCIATED(tmpZTreeNode%subdomains)) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(6)%subdomains(8)%nsubdomains FAILED!'
        STOP 666
      ENDIF
    ENDSUBROUTINE checkLevel2d6
!
!-------------------------------------------------------------------------------
    SUBROUTINE checkLevel2d7()
      tmpZTreeLevel => testZTree%subdomains(7)%subdomains
      
      tmpZTreeNode=tmpZTreeLevel(1) !Domain 1
      IF(ANY(tmpZTreeNode%x /= (/1,1/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(7)%subdomains(1)%x FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%y /= (/3,3/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(7)%subdomains(1)%y FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%z /= (/3,3/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(7)%subdomains(1)%z FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istt /= 49) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(7)%subdomains(1)%istt FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istp /= tmpZTreeNode%istt) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(7)%subdomains(1)%istp FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%nsubdomains /= 0 .OR. ASSOCIATED(tmpZTreeNode%subdomains)) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(7)%subdomains(1)%nsubdomains FAILED!'
        STOP 666
      ENDIF
      
      tmpZTreeNode=tmpZTreeLevel(2) !Domain 2
      IF(ANY(tmpZTreeNode%x /= (/2,2/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(7)%subdomains(2)%x FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%y /= (/3,3/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(7)%subdomains(2)%y FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%z /= (/3,3/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(7)%subdomains(2)%z FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istt /= 50) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(7)%subdomains(2)%istt FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istp /= tmpZTreeNode%istt) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(7)%subdomains(2)%istp FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%nsubdomains /= 0 .OR. ASSOCIATED(tmpZTreeNode%subdomains)) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(7)%subdomains(2)%nsubdomains FAILED!'
        STOP 666
      ENDIF
      
      tmpZTreeNode=tmpZTreeLevel(3) !Domain 3
      IF(ANY(tmpZTreeNode%x /= (/1,1/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(7)%subdomains(3)%x FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%y /= (/4,4/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(7)%subdomains(3)%y FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%z /= (/3,3/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(7)%subdomains(3)%z FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istt /= 51) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(7)%subdomains(3)%istt FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istp /= tmpZTreeNode%istt) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(7)%subdomains(3)%istp FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%nsubdomains /= 0 .OR. ASSOCIATED(tmpZTreeNode%subdomains)) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(7)%subdomains(3)%nsubdomains FAILED!'
        STOP 666
      ENDIF
      
      tmpZTreeNode=tmpZTreeLevel(4) !Domain 4
      IF(ANY(tmpZTreeNode%x /= (/2,2/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(7)%subdomains(4)%x FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%y /= (/4,4/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(7)%subdomains(4)%y FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%z /= (/3,3/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(7)%subdomains(4)%z FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istt /= 52) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(7)%subdomains(4)%istt FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istp /= tmpZTreeNode%istt) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(7)%subdomains(4)%istp FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%nsubdomains /= 0 .OR. ASSOCIATED(tmpZTreeNode%subdomains)) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(7)%subdomains(4)%nsubdomains FAILED!'
        STOP 666
      ENDIF
      
      tmpZTreeNode=tmpZTreeLevel(5) !Domain 5
      IF(ANY(tmpZTreeNode%x /= (/1,1/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(7)%subdomains(1)%subdomains(5)%x FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%y /= (/3,3/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(7)%subdomains(1)%subdomains(5)%y FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%z /= (/4,4/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(7)%subdomains(5)%z FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istt /= 53) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(7)%subdomains(5)%istt FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istp /= tmpZTreeNode%istt) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(7)%subdomains(5)%istp FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%nsubdomains /= 0 .OR. ASSOCIATED(tmpZTreeNode%subdomains)) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(7)%subdomains(5)%nsubdomains FAILED!'
        STOP 666
      ENDIF
      
      tmpZTreeNode=tmpZTreeLevel(6) !Domain 6
      IF(ANY(tmpZTreeNode%x /= (/2,2/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(7)%subdomains(6)%x FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%y /= (/3,3/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(7)%subdomains(6)%y FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%z /= (/4,4/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(7)%subdomains(6)%z FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istt /= 54) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(7)%subdomains(6)%istt FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istp /= tmpZTreeNode%istt) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(7)%subdomains(6)%istp FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%nsubdomains /= 0 .OR. ASSOCIATED(tmpZTreeNode%subdomains)) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(7)%subdomains(6)%nsubdomains FAILED!'
        STOP 666
      ENDIF
      
      tmpZTreeNode=tmpZTreeLevel(7) !Domain 7
      IF(ANY(tmpZTreeNode%x /= (/1,1/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(7)%subdomains(7)%x FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%y /= (/4,4/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(7)%subdomains(7)%y FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%z /= (/4,4/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(7)%subdomains(7)%z FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istt /= 55) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(7)%subdomains(7)%istt FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istp /= tmpZTreeNode%istt) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(7)%subdomains(7)%istp FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%nsubdomains /= 0 .OR. ASSOCIATED(tmpZTreeNode%subdomains)) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(7)%subdomains(7)%nsubdomains FAILED!'
        STOP 666
      ENDIF
      
      tmpZTreeNode=tmpZTreeLevel(8) !Domain 8
      IF(ANY(tmpZTreeNode%x /= (/2,2/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(7)%subdomains(8)%x FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%y /= (/4,4/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(7)%subdomains(8)%y FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%z /= (/4,4/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(7)%subdomains(8)%z FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istt /= 56) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(7)%subdomains(8)%istt FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istp /= tmpZTreeNode%istt) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(7)%subdomains(8)%istp FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%nsubdomains /= 0 .OR. ASSOCIATED(tmpZTreeNode%subdomains)) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(7)%subdomains(8)%nsubdomains FAILED!'
        STOP 666
      ENDIF
    ENDSUBROUTINE checkLevel2d7
!
!-------------------------------------------------------------------------------
    SUBROUTINE checkLevel2d8()
      tmpZTreeLevel => testZTree%subdomains(8)%subdomains
      
      tmpZTreeNode=tmpZTreeLevel(1) !Domain 1
      IF(ANY(tmpZTreeNode%x /= (/3,3/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(8)%subdomains(1)%x FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%y /= (/3,3/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(8)%subdomains(1)%y FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%z /= (/3,3/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(8)%subdomains(1)%z FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istt /= 57) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(8)%subdomains(1)%istt FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istp /= tmpZTreeNode%istt) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(8)%subdomains(1)%istp FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%nsubdomains /= 0 .OR. ASSOCIATED(tmpZTreeNode%subdomains)) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(8)%subdomains(1)%nsubdomains FAILED!'
        STOP 666
      ENDIF
      
      tmpZTreeNode=tmpZTreeLevel(2) !Domain 2
      IF(ANY(tmpZTreeNode%x /= (/4,4/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(8)%subdomains(2)%x FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%y /= (/3,3/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(8)%subdomains(2)%y FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%z /= (/3,3/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(8)%subdomains(2)%z FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istt /= 58) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(8)%subdomains(2)%istt FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istp /= tmpZTreeNode%istt) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(8)%subdomains(2)%istp FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%nsubdomains /= 0 .OR. ASSOCIATED(tmpZTreeNode%subdomains)) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(8)%subdomains(2)%nsubdomains FAILED!'
        STOP 666
      ENDIF
      
      tmpZTreeNode=tmpZTreeLevel(3) !Domain 3
      IF(ANY(tmpZTreeNode%x /= (/3,3/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(8)%subdomains(3)%x FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%y /= (/4,4/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(8)%subdomains(3)%y FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%z /= (/3,3/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(8)%subdomains(3)%z FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istt /= 59) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(8)%subdomains(3)%istt FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istp /= tmpZTreeNode%istt) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(8)%subdomains(3)%istp FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%nsubdomains /= 0 .OR. ASSOCIATED(tmpZTreeNode%subdomains)) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(8)%subdomains(3)%nsubdomains FAILED!'
        STOP 666
      ENDIF
      
      tmpZTreeNode=tmpZTreeLevel(4) !Domain 4
      IF(ANY(tmpZTreeNode%x /= (/4,4/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(8)%subdomains(4)%x FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%y /= (/4,4/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(8)%subdomains(4)%y FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%z /= (/3,3/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(8)%subdomains(4)%z FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istt /= 60) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(8)%subdomains(4)%istt FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istp /= tmpZTreeNode%istt) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(8)%subdomains(4)%istp FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%nsubdomains /= 0 .OR. ASSOCIATED(tmpZTreeNode%subdomains)) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(8)%subdomains(4)%nsubdomains FAILED!'
        STOP 666
      ENDIF
      
      tmpZTreeNode=tmpZTreeLevel(5) !Domain 5
      IF(ANY(tmpZTreeNode%x /= (/3,3/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(8)%subdomains(1)%subdomains(5)%x FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%y /= (/3,3/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(8)%subdomains(1)%subdomains(5)%y FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%z /= (/4,4/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(8)%subdomains(5)%z FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istt /= 61) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(8)%subdomains(5)%istt FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istp /= tmpZTreeNode%istt) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(8)%subdomains(5)%istp FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%nsubdomains /= 0 .OR. ASSOCIATED(tmpZTreeNode%subdomains)) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(8)%subdomains(5)%nsubdomains FAILED!'
        STOP 666
      ENDIF
      
      tmpZTreeNode=tmpZTreeLevel(6) !Domain 6
      IF(ANY(tmpZTreeNode%x /= (/4,4/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(8)%subdomains(6)%x FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%y /= (/3,3/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(8)%subdomains(6)%y FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%z /= (/4,4/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(8)%subdomains(6)%z FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istt /= 62) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(8)%subdomains(6)%istt FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istp /= tmpZTreeNode%istt) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(8)%subdomains(6)%istp FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%nsubdomains /= 0 .OR. ASSOCIATED(tmpZTreeNode%subdomains)) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(8)%subdomains(6)%nsubdomains FAILED!'
        STOP 666
      ENDIF
      
      tmpZTreeNode=tmpZTreeLevel(7) !Domain 7
      IF(ANY(tmpZTreeNode%x /= (/3,3/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(8)%subdomains(7)%x FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%y /= (/4,4/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(8)%subdomains(7)%y FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%z /= (/4,4/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(8)%subdomains(7)%z FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istt /= 63) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(8)%subdomains(7)%istt FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istp /= tmpZTreeNode%istt) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(8)%subdomains(7)%istp FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%nsubdomains /= 0 .OR. ASSOCIATED(tmpZTreeNode%subdomains)) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(8)%subdomains(7)%nsubdomains FAILED!'
        STOP 666
      ENDIF
      
      tmpZTreeNode=tmpZTreeLevel(8) !Domain 8
      IF(ANY(tmpZTreeNode%x /= (/4,4/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(8)%subdomains(8)%x FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%y /= (/4,4/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(8)%subdomains(8)%y FAILED!'
        STOP 666
      ENDIF
      IF(ANY(tmpZTreeNode%z /= (/4,4/))) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(8)%subdomains(8)%z FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istt /= 64) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(8)%subdomains(8)%istt FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%istp /= tmpZTreeNode%istt) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(8)%subdomains(8)%istp FAILED!'
        STOP 666
      ENDIF
      IF(tmpZTreeNode%nsubdomains /= 0 .OR. ASSOCIATED(tmpZTreeNode%subdomains)) THEN
        WRITE(*,*) 'CALL testZTree%init(1,4,1,4,1,4,1) %subdomains(8)%subdomains(8)%nsubdomains FAILED!'
        STOP 666
      ENDIF
    ENDSUBROUTINE checkLevel2d8
!
ENDPROGRAM testSpaceFillingCurve
