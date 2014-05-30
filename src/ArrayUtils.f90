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
!> @brief A Fortran 2003 module implementing some mesh/array utility functions.
!>        Examples include searching for an index in a 1-D real array given a 
!>        position, converting an index from one 1-D real array to another 1-D
!>        real array, along with several others.
!> 
!> @par Module Dependencies
!>  - @ref IntrType "IntrType": @copybrief IntrType
!>
!> @author Dan Jabaay
!>    @date 04/29/2014
!>
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE ArrayUtils
  
  USE IntrType
  USE Sorting
  IMPLICIT NONE
  
  PUBLIC :: getAbsolute
  PUBLIC :: getDelta
  PUBLIC :: getUnique
  PUBLIC :: findNUnique
  PUBLIC :: getUnion
  PUBLIC :: findIndex
  PUBLIC :: findLowBound
  PUBLIC :: findUpBound
  PUBLIC :: findEleHtAbove
  PUBLIC :: findEleHtBelow
  !PUBLIC :: findIntersection
  !Need a routine in here that compares a 1-D array to a 2-D array for a given dimension
  !to see if the 1-D array exists in the 2-D array...
  
  !> @brief Generic interface to the getAbsolute
  !>
  INTERFACE getAbsolute
    !> @copybrief ArrayUtils::getAbsolute_1DReal
    !> @copydetails ArrayUtils::getAbsolute_1DReal
    MODULE PROCEDURE getAbsolute_1DReal
    !> @copybrief ArrayUtils::getAbsolute_1DInt
    !> @copydetails ArrayUtils::getAbsolute_1DInt
    MODULE PROCEDURE getAbsolute_1DInt
  ENDINTERFACE getAbsolute
  
  !> @brief Generic interface to ...
  !>
  INTERFACE getDelta
    !> @copybrief ArrayUtils::getDelta_1DReal
    !> @copydetails ArrayUtils::getDelta_1DReal
    MODULE PROCEDURE getDelta_1DReal
  ENDINTERFACE getDelta
  
  !> @brief Generic interface to ...
  !>
  INTERFACE getUnique
    !> @copybrief ArrayUtils::getUnique_1DReal
    !> @copydetails ArrayUtils::getUnique_1DReal
    MODULE PROCEDURE getUnique_1DReal
    !> @copybrief ArrayUtils::getUnique_1DInt
    !> @copydetails ArrayUtils::getUnique_1DInt
    MODULE PROCEDURE getUnique_1DInt
    !> @copybrief ArrayUtils::getUnique_2DInt
    !> @copydetails ArrayUtils::getUnique_2DInt
    MODULE PROCEDURE getUnique_2DInt
  ENDINTERFACE getUnique
  
  !> @brief Generic interface to ...
  !>
  INTERFACE findNUnique
    !> @copybrief ArrayUtils::findNUnique_1DReal
    !> @copydetails ArrayUtils::findNUnique_1DReal
    MODULE PROCEDURE findNUnique_1DReal
    !> @copybrief ArrayUtils::findNUnique_1DInt
    !> @copydetails ArrayUtils::findNUnique_1DInt
    MODULE PROCEDURE findNUnique_1DInt
    !> @copybrief ArrayUtils::findNUnique_2DInt
    !> @copydetails ArrayUtils::findNUnique_2DInt
    MODULE PROCEDURE findNUnique_2DInt
  ENDINTERFACE findNUnique
  
  !> @brief Generic interface to ...
  !>
  INTERFACE getUnion
    !> @copybrief ArrayUtils::getUnion_1DReal
    !> @copydetails ArrayUtils::getUnion_1DReal
    MODULE PROCEDURE getUnion_1DReal
  ENDINTERFACE getUnion
  
  !> @brief Generic interface to find the index in an array.
  !>
  INTERFACE findIndex
    !> @copybrief ArrayUtils::findIndex_1DReal
    !> @copydetails ArrayUtils::findIndex_1DReal
    MODULE PROCEDURE findIndex_1DReal
    !> @copybrief ArrayUtils::findIndex_1DInt
    !> @copydetails ArrayUtils::findIndex_1DInt
    MODULE PROCEDURE findIndex_1DInt
  ENDINTERFACE findIndex

  !> @brief Generic interface to ...
  !>
  INTERFACE findLowBound
    !> @copybrief ArrayUtils::findLowBound_1DReal
    !> @copydetails ArrayUtils::findLowBound_1DReal
    MODULE PROCEDURE findLowBound_1DReal
  ENDINTERFACE findLowBound
  
  !> @brief Generic interface to ...
  !>
  INTERFACE findUpBound
    !> @copybrief ArrayUtils::findUpBound_1DReal
    !> @copydetails ArrayUtils::findUpBound_1DReal
    MODULE PROCEDURE findUpBound_1DReal
  ENDINTERFACE findUpBound
  
  !> @brief Generic interface to ...
  !>
  INTERFACE findEleHtAbove
    !> @copybrief ArrayUtils::findEleHtAbove_1DReal
    !> @copydetails ArrayUtils::findEleHtAbove_1DReal
    MODULE PROCEDURE findEleHtAbove_1DReal
  ENDINTERFACE findEleHtAbove
  
  !> @brief Generic interface to ...
  !>
  INTERFACE findEleHtBelow
    !> @copybrief ArrayUtils::findEleHtBelow_1DReal
    !> @copydetails ArrayUtils::findEleHtBelow_1DReal
    MODULE PROCEDURE findEleHtBelow_1DReal
  ENDINTERFACE findEleHtBelow
!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief This routine takes an array of fractions, or deltas, and sums them
!>        to get an absolute reference frame.  The bottom value can be adjusted
!>        using the optional xi input.  The default is to use the first value in
!>        r as the starting point for the absolute array.
!> @param r The array of delta values
!> @param rout The output array of absolute values
!> @param xi The optional input for the bottom starting position
!>
    PURE SUBROUTINE getAbsolute_1DReal(r,rout,xi)
      REAL(SRK),INTENT(IN) :: r(:)
      REAL(SRK),ALLOCATABLE,INTENT(OUT) :: rout(:)
      REAL(SRK),INTENT(IN),OPTIONAL :: xi
      INTEGER(SIK) :: i,n
      
      n=SIZE(r,DIM=1)
      IF(PRESENT(xi)) THEN
        n=n+1
        ALLOCATE(rout(n))
        rout(1)=xi
        DO i=2,n
          rout(i)=rout(i-1)+r(i-1)
        ENDDO
      ELSE
        ALLOCATE(rout(n))
        rout(1)=r(1)
        DO i=2,n
          rout(i)=rout(i-1)+r(i)
        ENDDO
      ENDIF
    ENDSUBROUTINE getAbsolute_1DReal
!
!-------------------------------------------------------------------------------
!> @brief This routine takes a 1-D array of fractions, or deltas, and sums them
!>        to get an absolute reference frame.  The bottom value can be adjusted
!>        using the optional xi input.  The default is to use the first value in
!>        r as the starting point for the absolute array.
!> @param r The array of delta values
!> @param rout The output array of absolute values
!> @param xi The optional input for the bottom starting position
!>
    PURE SUBROUTINE getAbsolute_1DInt(r,rout,xi)
      INTEGER(SIK),INTENT(IN) :: r(:)
      INTEGER(SIK),ALLOCATABLE,INTENT(OUT) :: rout(:)
      INTEGER(SIK),INTENT(IN),OPTIONAL :: xi
      INTEGER(SIK) :: i,n
      
      n=SIZE(r,DIM=1)
      IF(PRESENT(xi)) THEN
        n=n+1
        ALLOCATE(rout(n))
        rout(1)=xi
        DO i=2,n
          rout(i)=rout(i-1)+r(i-1)
        ENDDO
      ELSE
        ALLOCATE(rout(n))
        rout(1)=r(1)
        DO i=2,n
          rout(i)=rout(i-1)+r(i)
        ENDDO
      ENDIF
    ENDSUBROUTINE getAbsolute_1DInt
!
!-------------------------------------------------------------------------------
!> @brief This routine takes a 2-D array of fractions, or deltas, and sums them
!>        to get an absolute reference frame.  The bottom value can be adjusted
!>        using the optional xi input.  The default is to use the first value in
!>        r as the starting point for the absolute array.
!> @param r The array of delta values
!> @param rout The output array of absolute values
!> @param xi The optional input for the bottom starting position
!>
    PURE SUBROUTINE getAbsolute_2DInt(r,rout,xi)
      INTEGER(SIK),INTENT(IN) :: r(:,:)
      INTEGER(SIK),ALLOCATABLE,INTENT(OUT) :: rout(:,:)
      INTEGER(SIK),INTENT(IN),OPTIONAL :: xi
      INTEGER(SIK) :: i,n
      
      !n=SIZE(r,DIM=1)
      !IF(PRESENT(xi)) THEN
      !  n=n+1
      !  ALLOCATE(rout(n))
      !  rout(1)=xi
      !  DO i=2,n
      !    rout(i)=rout(i-1)+r(i-1)
      !  ENDDO
      !ELSE
      !  ALLOCATE(rout(n))
      !  rout(1)=r(1)
      !  DO i=2,n
      !    rout(i)=rout(i-1)+r(i)
      !  ENDDO
      !ENDIF
    ENDSUBROUTINE getAbsolute_2DInt
!
!-------------------------------------------------------------------------------
!> @brief This routine takes an array of absolute values that are assumed to be
!>        in increasing order and finds the difference between successive (i.e. 
!>        delta) values.  The bottom value can be adjusted using the optional xi
!>        input.  The default is to use the first value in r as the starting 
!>        point for the delta array.
!> @param r The array of absolute values
!> @param rout The output array of absolute values
!> @param xi The optional input for the bottom starting position
!>
    PURE SUBROUTINE getDelta_1DReal(r,rout,xi)
      REAL(SRK),INTENT(IN) :: r(:)
      REAL(SRK),ALLOCATABLE,INTENT(OUT) :: rout(:)
      REAL(SRK),INTENT(IN),OPTIONAL :: xi
      INTEGER(SIK) :: i,n
      
      n=SIZE(r,DIM=1)
      IF(PRESENT(xi)) THEN
        ALLOCATE(rout(n))
        DO i=n,2,-1
          rout(i)=r(i)-r(i-1)
        ENDDO
        rout(1)=rout(1)-xi
      ELSE
        n=n-1
        ALLOCATE(rout(n))
        DO i=n,1,-1
          rout(i)=r(i+1)-r(i)
        ENDDO
      ENDIF
    ENDSUBROUTINE getDelta_1DReal
!
!-------------------------------------------------------------------------------
!> @brief This routine takes an array of reals and returns the number of unique
!>        entries within a given tolerance for the equivalence of reals.  The
!>        optional delta input is whether the array is composed of incremental 
!>        values (deltas) or absolute values.  The optional tol input can 
!>        specifically decide the tolerance for whether two reals are unique.
!> @param r The input array of reals
!> @param delta The optional input for whether the array is incremental or not
!> @param tol The tolerance for comparing two real values
!> @param sout The number of unique entries in the array r.
!>
    PURE FUNCTION findNUnique_1DReal(r,delta,tol) RESULT(sout)
      REAL(SRK),INTENT(IN) :: r(:)
      LOGICAL(SBK),INTENT(IN),OPTIONAL :: delta
      REAL(SRK),INTENT(IN),OPTIONAL :: tol
      INTEGER(SIK) :: sout
      
      INTEGER(SIK) :: i,n
      REAL(SRK) :: loctol
      REAL(SRK),ALLOCATABLE :: tmpr(:)
      
      n=SIZE(r,DIM=1)
      IF(PRESENT(delta)) THEN
        IF(delta) THEN
          CALL getAbsolute_1DReal(r,tmpr)
          n=SIZE(tmpr,DIM=1)
        ENDIF
      ENDIF
      IF(.NOT.ALLOCATED(tmpr)) THEN
        ALLOCATE(tmpr(n))
        tmpr=r
      ENDIF
      loctol=EPSREAL*100.0_SRK
      IF(PRESENT(tol)) THEN
        IF((0.0_SRK < tol) .AND. (tol <= EPSREAL*1000.0_SRK)) loctol=tol
      ENDIF
      
      !Find the number of unique entries
      CALL sort(tmpr)
      sout=1
      DO i=2,n
        IF(.NOT. SOFTEQ(tmpr(i-1),tmpr(i),loctol)) sout=sout+1
      ENDDO
      !Deallocate
      DEALLOCATE(tmpr)
    ENDFUNCTION findNUnique_1DReal
!
!-------------------------------------------------------------------------------
!> @brief This routine takes a 1-D array of integers and returns the number of 
!>        unique entries.  The optional delta input is whether the array is 
!>        composed of incremental values (deltas) or absolute values.  
!> @param r The input array of integers
!> @param delta The optional input for whether the array is incremental or not
!> @param sout The number of unique entries in the array r.
!>
    PURE FUNCTION findNUnique_1DInt(r,delta) RESULT(sout)
      INTEGER(SIK),INTENT(IN) :: r(:)
      LOGICAL(SBK),INTENT(IN),OPTIONAL :: delta
      INTEGER(SIK) :: sout
      
      INTEGER(SIK) :: i,n
      INTEGER(SIK),ALLOCATABLE :: tmpr(:)
      
      n=SIZE(r,DIM=1)
      IF(PRESENT(delta)) THEN
        IF(delta) THEN
          CALL getAbsolute_1DInt(r,tmpr)
          n=SIZE(tmpr,DIM=1)
        ENDIF
      ENDIF
      IF(.NOT.ALLOCATED(tmpr)) THEN
        ALLOCATE(tmpr(n))
        tmpr=r
      ENDIF
      
      !Find the number of unique entries
      CALL sort(tmpr)
      sout=1
      DO i=2,n
        IF(tmpr(i-1) /= tmpr(i)) sout=sout+1
      ENDDO
      !Deallocate
      DEALLOCATE(tmpr)
    ENDFUNCTION findNUnique_1DInt
!
!-------------------------------------------------------------------------------
!> @brief This routine takes a 2-D array of integers and returns the number of 
!>        unique entries.  The optional delta input is whether the array is 
!>        composed of incremental values (deltas) or absolute values.  
!> @param r The input array of integers
!> @param delta The optional input for whether the array is incremental or not
!> @param sout The number of unique entries in the array r.
!>
    PURE FUNCTION findNUnique_2DInt(r,delta) RESULT(sout)
      INTEGER(SIK),INTENT(IN) :: r(:,:)
      LOGICAL(SBK),INTENT(IN),OPTIONAL :: delta
      INTEGER(SIK) :: sout
      
      INTEGER(SIK) :: i,n
      INTEGER(SIK),ALLOCATABLE :: tmpr(:,:)
      
      !n=SIZE(r,DIM=1)
      !IF(PRESENT(delta)) THEN
      !  IF(delta) THEN
      !    CALL getAbsolute_2DInt(r,tmpr)
      !    n=SIZE(tmpr,DIM=1)
      !  ENDIF
      !ENDIF
      !IF(.NOT.ALLOCATED(tmpr)) THEN
      !  ALLOCATE(tmpr(n))
      !  tmpr=r
      !ENDIF
      !
      !!Find the number of unique entries
      !CALL sort(tmpr)
      !sout=1
      !DO i=2,n
      !  IF(tmpr(i-1) /= tmpr(i)) sout=sout+1
      !ENDDO
      !!Deallocate
      !DEALLOCATE(tmpr)
    ENDFUNCTION findNUnique_2DInt
!
!-------------------------------------------------------------------------------
!> @brief This routine takes an array of reals and returns the unique
!>        entries within a given tolerance for the equivalence of reals.  The
!>        optional delta input is whether the array is composed of incremental 
!>        values (deltas) or absolute values.  The optional tol input can 
!>        specifically decide the tolerance for whether two reals are unique.
!> @param r The input array of reals
!> @param rout The 1-D array of unique entries in the array r.
!> @param delta The optional input for whether the array is incremental or not
!> @param tol The tolerance for comparing two real values
!>
    PURE SUBROUTINE getUnique_1DReal(r,rout,delta,tol)
      REAL(SRK),INTENT(IN) :: r(:)
      REAL(SRK),ALLOCATABLE,INTENT(OUT) :: rout(:)
      LOGICAL(SBK),INTENT(IN),OPTIONAL :: delta
      REAL(SRK),INTENT(IN),OPTIONAL :: tol
      
      INTEGER(SIK) :: i,n,sout
      REAL(SRK) :: loctol
      REAL(SRK),ALLOCATABLE :: tmpr(:)
      
      n=SIZE(r,DIM=1)
      IF(PRESENT(delta)) THEN
        IF(delta) THEN
          CALL getAbsolute_1DReal(r,tmpr)
          n=SIZE(tmpr,DIM=1)
        ENDIF
      ENDIF
      IF(.NOT.ALLOCATED(tmpr)) THEN
        ALLOCATE(tmpr(n))
        tmpr=r
      ENDIF
      loctol=EPSREAL*100.0_SRK
      IF(PRESENT(tol)) THEN
        IF((0.0_SRK < tol) .AND. (tol <= EPSREAL*1000.0_SRK)) loctol=tol
      ENDIF
      
      !Find the number of unique entries
      CALL sort(tmpr)
      sout=1
      DO i=2,n
        IF(.NOT. SOFTEQ(tmpr(i-1),tmpr(i),loctol)) sout=sout+1
      ENDDO
      ALLOCATE(rout(sout))
      rout=0.0_SRK
      rout(1)=tmpr(1)
      DO i=2,n
        IF(.NOT. SOFTEQ(tmpr(i-1),tmpr(i),loctol)) rout(i)=tmpr(i)
      ENDDO
      
      !Deallocate
      DEALLOCATE(tmpr)
    ENDSUBROUTINE getUnique_1DReal
!
!-------------------------------------------------------------------------------
!> @brief This routine takes an array of integers and returns the unique
!>        entries.  The optional delta input is whether the array is composed of
!>         incremental values (deltas) or absolute values.  
!> @param r The input array of integers
!> @param rout The 1-D array of unique entries in the array r.
!> @param delta The optional input for whether the array is incremental or not
!>
    PURE SUBROUTINE getUnique_1DInt(r,rout,delta)
      INTEGER(SIK),INTENT(IN) :: r(:)
      INTEGER(SIK),ALLOCATABLE,INTENT(OUT) :: rout(:)
      LOGICAL(SBK),INTENT(IN),OPTIONAL :: delta
      
      INTEGER(SIK) :: i,n,sout
      INTEGER(SIK),ALLOCATABLE :: tmpr(:)
      
      n=SIZE(r,DIM=1)
      IF(PRESENT(delta)) THEN
        IF(delta) THEN
          CALL getAbsolute_1DInt(r,tmpr)
          n=SIZE(tmpr,DIM=1)
        ENDIF
      ENDIF
      IF(.NOT.ALLOCATED(tmpr)) THEN
        ALLOCATE(tmpr(n))
        tmpr=r
      ENDIF
      
      !Find the number of unique entries
      CALL sort(tmpr)
      sout=1
      DO i=2,n
        IF(tmpr(i-1) /= tmpr(i)) sout=sout+1
      ENDDO
      ALLOCATE(rout(sout))
      rout=0
      rout(1)=tmpr(1)
      DO i=2,n
        IF(tmpr(i-1) /= tmpr(i)) rout(i)=tmpr(i)
      ENDDO
      !Deallocate
      DEALLOCATE(tmpr)
    ENDSUBROUTINE getUnique_1DInt
!
!-------------------------------------------------------------------------------
!> @brief This routine takes a 2-D array of integers and returns the unique
!>        entries.  The optional delta input is whether the array is composed of
!>         incremental values (deltas) or absolute values.  
!> @param r The input array of integers
!> @param rout The 2-D array of unique entries in the array r.
!> @param delta The optional input for whether the array is incremental or not
!>
    PURE SUBROUTINE getUnique_2DInt(r,rout,delta)
      INTEGER(SIK),INTENT(IN) :: r(:,:)
      INTEGER(SIK),ALLOCATABLE,INTENT(OUT) :: rout(:)
      LOGICAL(SBK),INTENT(IN),OPTIONAL :: delta
      
      INTEGER(SIK) :: i,n,sout
      INTEGER(SIK),ALLOCATABLE :: tmpr(:,:)
      
      n=SIZE(r,DIM=1)
      IF(PRESENT(delta)) THEN
        IF(delta) THEN
          CALL getAbsolute_2DInt(r,tmpr)
          n=SIZE(tmpr,DIM=1)
        ENDIF
      ENDIF
      IF(.NOT.ALLOCATED(tmpr)) THEN
        ALLOCATE(tmpr(n,n))
        tmpr=r
      ENDIF
      
      !Find the number of unique entries
      CALL sort(tmpr)
      sout=1
      DO i=2,n
        IF(tmpr(i-1,i-1) /= tmpr(i,i)) sout=sout+1
      ENDDO
      ALLOCATE(rout(sout))
      rout=0
      rout(1)=tmpr(1,1)
      DO i=2,n
        IF(tmpr(i-1,i-1) /= tmpr(i,i)) rout(i)=tmpr(i,i)
      ENDDO
      !Deallocate
      DEALLOCATE(tmpr)
    ENDSUBROUTINE getUnique_2DInt
!
!-------------------------------------------------------------------------------
!> @brief This routine takes two real arrays and finds all the unique absolute
!>        values between them (i.e. the union operation).  It returns an array
!>        consisting of those values.  There are optional values for determining
!>        the starting position of either array via xi, whether either array
!>        is an incremental array, and whether the returned array should be
!>        incremental or not.
!> @param r1 The first array of reals
!> @param r2 The second array of reals
!> @param rout The output of the union of r1 and r2
!> @param xi1 The optional offset for the first array 
!> @param xi2 The optional offset for the second array 
!> @param delta1 The optional flag if the first array is an incremental array
!> @param delta2 The optional flag if the second array is an incremental array
!> @param deltaout The optional flag if the output array is an incremental array
!>
    SUBROUTINE getUnion_1DReal(r1,r2,rout,xi1,xi2,delta1,delta2,deltaout)
      REAL(SRK),INTENT(IN) :: r1(:)
      REAL(SRK),INTENT(IN) :: r2(:)
      REAL(SRK),ALLOCATABLE,INTENT(OUT) :: rout(:)
      REAL(SRK),INTENT(IN),OPTIONAL :: xi1
      REAL(SRK),INTENT(IN),OPTIONAL :: xi2
      LOGICAL(SBK),INTENT(IN),OPTIONAL :: delta1
      LOGICAL(SBK),INTENT(IN),OPTIONAL :: delta2
      LOGICAL(SBK),INTENT(IN),OPTIONAL :: deltaout
      LOGICAL(SBK) :: bool1,bool2
      INTEGER(SIK) :: i,j,sr1,sr2,sout,tmpsout
      REAL(SRK),ALLOCATABLE :: tmpout(:),tmp1(:),tmp2(:)
      
      !Process the first array if it is a delta
      bool1=.FALSE.
      IF(PRESENT(delta1)) THEN
        IF(delta1) THEN
          IF(PRESENT(xi1)) THEN
            CALL getAbsolute_1DReal(r1,tmp1,XI=xi1)
          ELSE
            CALL getAbsolute_1DReal(r1,tmp1)
          ENDIF
          bool1=.TRUE.
          sr1=SIZE(tmp1,DIM=1)
        ELSE
          sr1=SIZE(r1,DIM=1)
        ENDIF
      ELSE
        sr1=SIZE(r1,DIM=1)
      ENDIF
      !Process the second array if it is a delta
      bool2=.FALSE.
      IF(PRESENT(delta2)) THEN
        IF(delta2) THEN
          IF(PRESENT(xi2)) THEN
            CALL getAbsolute_1DReal(r2,tmp2,XI=xi2)
          ELSE
            CALL getAbsolute_1DReal(r2,tmp2)
          ENDIF
          bool2=.TRUE.
          sr2=SIZE(tmp2,DIM=1)
        ELSE
          sr2=SIZE(r2,DIM=1)
        ENDIF
      ELSE
        sr2=SIZE(r2,DIM=1)
      ENDIF
      !Allocate the tmp array to the full size of the 2 arrays
      tmpsout=sr1+sr2
      ALLOCATE(tmpout(tmpsout))
      IF(bool1) THEN
        tmpout(1:sr1)=tmp1
      ELSE
        tmpout(1:sr1)=r1
      ENDIF
      IF(bool2) THEN
        tmpout(sr1+1:sr2)=tmp2
      ELSE
        tmpout(sr1+1:sr2)=r2
      ENDIF
      !Sort the array components to make finding repeated values easy.
      CALL sort(tmpout)
      sout=tmpsout
      DO i=2,tmpsout
        IF(tmpout(i-1) .APPROXEQ. tmpout(i)) sout=sout-1
      ENDDO
      ALLOCATE(rout(sout))
      rout=0.0_SRK
      rout(1)=tmpout(1)
      j=2
      DO i=2,tmpsout
        IF(.NOT.(tmpout(i-1) .APPROXEQ. tmpout(i))) THEN
          rout(j)=tmpout(i)
          j=j+1
        ENDIF
      ENDDO
      IF(PRESENT(deltaout)) THEN
        IF(deltaout) THEN
          CALL getDelta_1DReal(rout,rout)
        ENDIF
      ENDIF
    ENDSUBROUTINE getUnion_1DReal
!
!-------------------------------------------------------------------------------
!> @brief This routine finds the index in array r in which the position pos 
!>        falls.  It has the optional for specifying the lower array bounds xi,
!>        and whether the array is incremental or not.  It returns the array 
!>        index if it is found.  A -1 is returned for a position value lower
!>        than the bottom array value and a -2 for a position value higher than
!>        the top array value.  Behavior for a point that falls on a boundary
!>        isn't currently defined.  It is proposed to return a -3.  TBD.
!>
!>        Example:
!>    r=  0.0_SRK   10.0_SRK   15.0_SRK   25.0_SRK  (xi=0.0_SRK, r is size 3)
!>  index= |    1     |     2    |     3    |
!>
!>        findIndex(r,5.0_SRK,xi=0.0_SRK) = 1
!>        findIndex(r,12.0_SRK,xi=0.0_SRK) = 2
!>        findIndex(r,20.0_SRK,xi=0.0_SRK) = 3
!>        !Cases without xi
!>        findIndex(r,5.0_SRK,delta=.FALSE.) = -1
!>        findIndex(r,12.0_SRK,delta=.FALSE.) = 1
!>        findIndex(r,20.0_SRK,delta=.FALSE.) = 2
!>        !Cases with delta
!>        findIndex(r,5.0_SRK,delta=.TRUE.) = -1
!>        findIndex(r,12.0_SRK,delta=.TRUE.) = 1
!>        findIndex(r,20.0_SRK,delta=.TRUE.) = 1
!>        findIndex(r,45.0_SRK,delta=.TRUE.) = 2
!>        findIndex(r,55.0_SRK,delta=.TRUE.) = -2
!>        !out of bounds cases
!>        findIndex(r,-5.0_SRK,xi=0.0_SRK,delta=.FALSE.) = -1
!>        findIndex(r,35.0_SRK,xi=0.0_SRK,delta=.FALSE.) = -2
!> @param r The array in which to find an index
!> @param pos The position for which to find an index
!> @param xi The optional input for the bottom starting position
!> @param delta The optional input for whether the array is incremental or not
!> @param ind The array index where pos is located in r.
!>
    PURE FUNCTION findIndex_1DReal(r,pos,xi,delta,incl,tol) RESULT(ind)
      REAL(SRK),INTENT(IN) :: r(:)
      REAL(SRK),INTENT(IN) :: pos
      REAL(SRK),INTENT(IN),OPTIONAL :: xi
      LOGICAL(SBK),INTENT(IN),OPTIONAL :: delta
      INTEGER(SIK),INTENT(IN),OPTIONAL :: incl
      REAL(SRK),INTENT(IN),OPTIONAL :: tol
      INTEGER(SIK) :: ind
      INTEGER(SIK) :: i,n,l_incl
      REAL(SRK) :: tmp(SIZE(r,DIM=1)+1),l_tol
      
      !Initialize the tmp array and adjust for any offset xi
      n=SIZE(r,DIM=1)
      tmp=0.0_SRK
      IF(PRESENT(xi)) THEN
        tmp(1)=xi
        tmp(2:n+1)=r
        n=n+1
      ELSE
        tmp(1:n)=r
      ENDIF
      
      !If the array is in increments/deltas and not full values
      IF(PRESENT(delta)) THEN
        IF(delta) THEN
          DO i=2,SIZE(tmp,DIM=1)
            tmp(i)=tmp(i-1)+tmp(i)
          ENDDO
        ENDIF
      ENDIF
      
      !If the logic should be inclusive or exclusive
      l_incl=0
      IF(PRESENT(incl)) THEN
        IF((0 <= incl) .AND. (incl <= 2)) l_incl=incl
      ENDIF
      
      !If there is a tolerance specified, assign it
      l_tol=EPSREAL
      IF(PRESENT(tol)) THEN
        !Give tolerance a range of say 1000*EPSREAL
        IF((0.0_SRK < tol) .AND. (tol <= EPSREAL*1000.0_SRK)) l_tol=tol
      ENDIF
      
      !Below the array
      IF(pos < tmp(1)) THEN
        ind=-1
      !Above the array
      ELSEIF(tmp(n) < pos) THEN
        ind=-2
      !Inbetween, error if on mesh
      ELSEIF(l_incl == 0) THEN
        IF(ANY(tmp .APPROXEQ. pos)) THEN
          ind=-3
        ELSEIF((tmp(1) < pos) .AND. (pos < tmp(n))) THEN
          ind=1
          DO WHILE(pos > tmp(ind))
            ind=ind+1
          ENDDO
          ind=ind-1
        ENDIF
      !Inbetween, don't error on mesh
      ELSEIF(l_incl > 0) THEN
        IF((tmp(1) .APPROXLE. pos) .AND. (pos .APPROXLE. tmp(n))) THEN
          ind=1
          DO WHILE(SOFTGE(pos,tmp(ind),l_tol))
          !DO WHILE(pos .APPROXGE. tmp(ind))
            ind=ind+1
            IF(ind > n) EXIT
          ENDDO
          ind=ind-1
          IF((l_incl == 1) .AND. SOFTEQ(pos,tmp(ind),l_tol)) ind=ind-1
          IF((l_incl == 1) .AND. (ind == 0)) ind=-1
          IF((l_incl == 2) .AND. (ind == n)) ind=-2
          IF(ind == n) ind=ind-1
        ENDIF
      ENDIF
    ENDFUNCTION findIndex_1DReal
!
!-------------------------------------------------------------------------------
!> @brief 
!> @param r
!>
    PURE FUNCTION findIndex_1DInt(r,pos,xi,delta) RESULT(ind)
      INTEGER(SIK),INTENT(IN) :: r(:)
      INTEGER(SIK),INTENT(IN) :: pos
      INTEGER(SIK),INTENT(IN),OPTIONAL :: xi
      LOGICAL(SBK),INTENT(IN),OPTIONAL :: delta
      INTEGER(SIK) :: ind
      REAL(SRK) :: tmp
      ind=-1
      tmp=1.0_SRK
      
    ENDFUNCTION findIndex_1DInt
!
!-------------------------------------------------------------------------------
!> @brief This routine returns the nearest value of array r that is less than
!>        position pos.  It has the optional for specifying the lower array 
!>        bounds xi, and whether the array is incremental or not.  If the pos 
!>        value is not within array r, the return argument val will be 0.0_SRK.
!> @param r The array in which to find the nearest lesser value
!> @param pos The position for which to find the nearest lesser value
!> @param xi The optional input for the bottom starting position
!> @param delta The optional input for whether the array is incremental or not
!> @param val The nearest lesser value
!>
    PURE FUNCTION findLowBound_1DReal(r,pos,xi,delta) RESULT(val)
      REAL(SRK),INTENT(IN) :: r(:)
      REAL(SRK),INTENT(IN) :: pos
      REAL(SRK),INTENT(IN),OPTIONAL :: xi
      LOGICAL(SBK),INTENT(IN),OPTIONAL :: delta
      REAL(SRK) :: val
      INTEGER(SIK) :: ind
      INTEGER(SIK) :: i,n
      REAL(SRK) :: tmp(SIZE(r,DIM=1)+1)
      
      !Initialize the tmp array and adjust for any offset xi
      val=0.0_SRK
      n=SIZE(r,DIM=1)
      tmp=0.0_SRK
      IF(PRESENT(xi)) THEN
        tmp(1)=xi
        tmp(2:n+1)=r
        n=n+1
      ELSE
        tmp(1:n)=r
      ENDIF
      
      !If the array is in increments/deltas and not full values
      IF(PRESENT(delta)) THEN
        IF(delta) THEN
          DO i=2,SIZE(tmp,DIM=1)
            tmp(i)=tmp(i-1)+tmp(i)
          ENDDO
        ENDIF
      ENDIF
      
      IF((tmp(1) .APPROXLE. pos) .AND. (pos .APPROXLE. tmp(n))) THEN
        ind=1
        DO WHILE(pos .APPROXGE. tmp(ind))
          ind=ind+1
        ENDDO
        IF(ind > 1) val=tmp(ind-1)
      ENDIF
    ENDFUNCTION findLowBound_1DReal
!
!-------------------------------------------------------------------------------
!> @brief This routine returns the nearest value of array r that is greater than
!>        position pos.  It has the optional for specifying the lower array 
!>        bounds xi, and whether the array is incremental or not.  If the pos 
!>        value is not within array r, the return argument val will be 0.0_SRK.
!> @param r The array in which to find the nearest greater value
!> @param pos The position for which to find the nearest greater value
!> @param xi The optional input for the bottom starting position
!> @param delta The optional input for whether the array is incremental or not
!> @param val The nearest greater value
!>
    PURE FUNCTION findUpBound_1DReal(r,pos,xi,delta) RESULT(val)
      REAL(SRK),INTENT(IN) :: r(:)
      REAL(SRK),INTENT(IN) :: pos
      REAL(SRK),INTENT(IN),OPTIONAL :: xi
      LOGICAL(SBK),INTENT(IN),OPTIONAL :: delta
      REAL(SRK) :: val
      INTEGER(SIK) :: ind
      INTEGER(SIK) :: i,n
      REAL(SRK) :: tmp(SIZE(r,DIM=1)+1)
      
      !Initialize the tmp array and adjust for any offset xi
      val=0.0_SRK
      n=SIZE(r,DIM=1)
      tmp=0.0_SRK
      IF(PRESENT(xi)) THEN
        tmp(1)=xi
        tmp(2:n+1)=r
        n=n+1
      ELSE
        tmp(1:n)=r
      ENDIF
      
      !If the array is in increments/deltas and not full values
      IF(PRESENT(delta)) THEN
        IF(delta) THEN
          DO i=2,SIZE(tmp,DIM=1)
            tmp(i)=tmp(i-1)+tmp(i)
          ENDDO
        ENDIF
      ENDIF
      
      IF((tmp(1) .APPROXLE. pos) .AND. (pos .APPROXLE. tmp(n))) THEN
        ind=1
        DO WHILE(pos .APPROXGE. tmp(ind))
          ind=ind+1
        ENDDO
        val=tmp(ind)
      ENDIF
    ENDFUNCTION findUpBound_1DReal
!
!-------------------------------------------------------------------------------
!> @brief This routine returns the difference of the nearest value of array r 
!>        that is greater than position pos and position pos.  It has the 
!>        optional for specifying the lower array bounds xi, and whether the 
!>        array is incremental or not.  If the pos value is not within array r, 
!>        the return argument val will be -pos.
!> @param r The array in which to find the difference
!> @param pos The position for which to find the difference
!> @param xi The optional input for the bottom starting position
!> @param delta The optional input for whether the array is incremental or not
!> @param val The difference of the nearest greater value and pos
!>
    PURE FUNCTION findEleHtAbove_1DReal(r,pos,xi,delta) RESULT(val)
      REAL(SRK),INTENT(IN) :: r(:)
      REAL(SRK),INTENT(IN) :: pos
      REAL(SRK),INTENT(IN),OPTIONAL :: xi
      LOGICAL(SBK),INTENT(IN),OPTIONAL :: delta
      REAL(SRK) :: val
      REAL(SRK) :: tmp
      
      IF(PRESENT(xi) .AND. PRESENT(delta)) THEN
        tmp=findUpBound(r,pos,XI=xi,delta=DELTA)
      ELSEIF(PRESENT(delta)) THEN
        tmp=findUpBound(r,pos,DELTA=delta)
      ELSEIF(PRESENT(xi)) THEN
        tmp=findUpBound(r,pos,XI=xi)
      ELSE
        tmp=findUpBound(r,pos)
      ENDIF
      
      val=tmp-pos
      
    ENDFUNCTION findEleHtAbove_1DReal
!
!-------------------------------------------------------------------------------
!> @brief This routine returns the difference of position pos and the nearest 
!>        value of array r that is lesser than position pos.  It has the 
!>        optional for specifying the lower array bounds xi, and whether the 
!>        array is incremental or not.  If the pos value is not within array r, 
!>        the return argument val will be -pos.
!> @param r The array in which to find the difference
!> @param pos The position for which to find the difference
!> @param xi The optional input for the bottom starting position
!> @param delta The optional input for whether the array is incremental or not
!> @param val The difference of pos and the nearest lesser value
!>
    PURE FUNCTION findEleHtBelow_1DReal(r,pos,xi,delta) RESULT(val)
      REAL(SRK),INTENT(IN) :: r(:)
      REAL(SRK),INTENT(IN) :: pos
      REAL(SRK),INTENT(IN),OPTIONAL :: xi
      LOGICAL(SBK),INTENT(IN),OPTIONAL :: delta
      REAL(SRK) :: val
      REAL(SRK) :: tmp
      
      IF(PRESENT(xi) .AND. PRESENT(delta)) THEN
        tmp=findLowBound(r,pos,XI=xi,DELTA=delta)
      ELSEIF(PRESENT(delta)) THEN
        tmp=findLowBound(r,pos,DELTA=delta)
      ELSEIF(PRESENT(xi)) THEN
        tmp=findLowBound(r,pos,XI=xi)
      ELSE
        tmp=findLowBound(r,pos)
      ENDIF
      
      val=pos-tmp
      
    ENDFUNCTION findEleHtBelow_1DReal
!
ENDMODULE ArrayUtils
