!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!> @brief A Fortran 2003 module implementing some mesh/array utility functions.
!>        Examples include searching for an index in a 1-D real array given a
!>        position, converting an index from one 1-D real array to another 1-D
!>        real array, along with several others.
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE ArrayUtils
#include "Futility_DBC.h"
USE ISO_FORTRAN_ENV
USE Futility_DBC
USE IntrType
USE Sorting
USE Strings

IMPLICIT NONE
PRIVATE

PUBLIC :: getAbsolute
PUBLIC :: getDelta
PUBLIC :: getUnique
PUBLIC :: findNUnique
PUBLIC :: getUnion
PUBLIC :: findIndex
PUBLIC :: isStrictlyIncDec
PUBLIC :: isStrictlyIncreasing
PUBLIC :: isStrictlyDecreasing
PUBLIC :: isMonotonic
PUBLIC :: isIncreasing
PUBLIC :: isDecreasing
PUBLIC :: hasAnyRemainder
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
  !> @copybrief ArrayUtils::getUnique_1DString
  !> @copydetails ArrayUtils::getUnique_1DString
  MODULE PROCEDURE getUnique_1DString
  !> @copybrief ArrayUtils::getUnique_2DString
  !> @copydetails ArrayUtils::getUnique_2DString
  MODULE PROCEDURE getUnique_2DString
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
  !> @copybrief ArrayUtils::findNUnique_1DString
  !> @copydetails ArrayUtils::findNUnique_1DString
  MODULE PROCEDURE findNUnique_1DString
  !> @copybrief ArrayUtils::findNUnique_2DString
  !> @copydetails ArrayUtils::findNUnique_2DString
  MODULE PROCEDURE findNUnique_2DString
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

!> @brief Generic interface to check if an array is strictly decreasing or increasing
!>
INTERFACE isStrictlyIncDec
  !> @copybrief ArrayUtils::isStrictlyIncDec_1DInt
  !> @copydetails ArrayUtils::isStrictlyIncDec_1DInt
  MODULE PROCEDURE isStrictlyIncDec_1DInt
  !> @copybrief ArrayUtils::isStrictlyIncDec_1DReal
  !> @copydetails ArrayUtils::isStrictlyIncDec_1DReal
  MODULE PROCEDURE isStrictlyIncDec_1DReal
ENDINTERFACE

!> @brief Generic interface to check if an array is monotonically decreasing or increasing
!>
INTERFACE isMonotonic
  !> @copybrief ArrayUtils::isMonotonic_1DInt
  !> @copydetails ArrayUtils::isMonotonic_1DInt
  MODULE PROCEDURE isMonotonic_1DInt
  !> @copybrief ArrayUtils::Monotonic_1DReal
  !> @copydetails ArrayUtils::Monotonic_1DReal
  MODULE PROCEDURE isMonotonic_1DReal
ENDINTERFACE

!> @brief Generic interface to check if an array is strictly decreasing
!>
INTERFACE isStrictlyDecreasing
  !> @copybrief ArrayUtils::isStrictlyDecreasing_1DInt
  !> @copydetails ArrayUtils::isStrictlyDecreasing_1DInt
  MODULE PROCEDURE isStrictlyDecreasing_1DInt
  !> @copybrief ArrayUtils::isStrictlyDecreasing_1DReal
  !> @copydetails ArrayUtils::isStrictlyDecreasing_1DReal
  MODULE PROCEDURE isStrictlyDecreasing_1DReal
ENDINTERFACE

!> @brief Generic interface to check if an array is monotonically decreasing
!>
INTERFACE isDecreasing
  !> @copybrief ArrayUtils::isDecreasing_1DInt
  !> @copydetails ArrayUtils::isDecreasing_1DInt
  MODULE PROCEDURE isDecreasing_1DInt
  !> @copybrief ArrayUtils::isDecreasing_1DReal
  !> @copydetails ArrayUtils::isDecreasing_1DReal
  MODULE PROCEDURE isDecreasing_1DReal
ENDINTERFACE

!> @brief Generic interface to check if an array is strictly increasing
!>
INTERFACE isStrictlyIncreasing
  !> @copybrief ArrayUtils::isStrictlyIncreasing_1DInt
  !> @copydetails ArrayUtils::isStrictlyIncreasing_1DInt
  MODULE PROCEDURE isStrictlyIncreasing_1DInt
  !> @copybrief ArrayUtils::isStrictlyIncreasing_1DReal
  !> @copydetails ArrayUtils::isStrictlyIncreasing_1DReal
  MODULE PROCEDURE isStrictlyIncreasing_1DReal
ENDINTERFACE

!> @brief Generic interface to check if an array is monotonically increasing
!>
INTERFACE isIncreasing
  !> @copybrief ArrayUtils::isIncreasing_1DInt
  !> @copydetails ArrayUtils::isIncreasing_1DInt
  MODULE PROCEDURE isIncreasing_1DInt
  !> @copybrief ArrayUtils::isIncreasing_1DReal
  !> @copydetails ArrayUtils::isIncreasing_1DReal
  MODULE PROCEDURE isIncreasing_1DReal
ENDINTERFACE
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
  REAL(SRK),ALLOCATABLE,INTENT(INOUT) :: rout(:)
  REAL(SRK),INTENT(IN),OPTIONAL :: xi
  INTEGER(SIK) :: i,n

  n=SIZE(r,DIM=1)
  IF(ALLOCATED(rout)) DEALLOCATE(rout)
  ALLOCATE(rout(n+1))
  IF(PRESENT(xi)) THEN
    rout(1)=xi
  ELSE
    rout(1)=0.0_SRK
  ENDIF
  DO i=1,n
    rout(i+1)=rout(i)+r(i)
  ENDDO
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
  INTEGER(SIK),ALLOCATABLE,INTENT(INOUT) :: rout(:)
  INTEGER(SIK),INTENT(IN),OPTIONAL :: xi
  INTEGER(SIK) :: i,n

  n=SIZE(r,DIM=1)
  IF(ALLOCATED(rout)) DEALLOCATE(rout)
  ALLOCATE(rout(n+1))
  IF(PRESENT(xi)) THEN
    rout(1)=xi
  ELSE
    rout(1)=0
  ENDIF
  DO i=1,n
    rout(i+1)=rout(i)+r(i)
  ENDDO
ENDSUBROUTINE getAbsolute_1DInt
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
  REAL(SRK),ALLOCATABLE,INTENT(INOUT) :: rout(:)
  REAL(SRK),INTENT(IN),OPTIONAL :: xi
  INTEGER(SIK) :: i,n

  n=SIZE(r,DIM=1)
  IF(ALLOCATED(rout)) DEALLOCATE(rout)
  IF(PRESENT(xi)) THEN
    ALLOCATE(rout(n))
    DO i=n,2,-1
      rout(i)=r(i)-r(i-1)
    ENDDO
    rout(1)=r(1)-xi
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
!> @returns sout The number of unique entries in the array r.
!>
FUNCTION findNUnique_1DReal(r,delta,tol) RESULT(sout)
  REAL(SRK),INTENT(IN) :: r(:)
  LOGICAL(SBK),INTENT(IN),OPTIONAL :: delta
  REAL(SRK),INTENT(IN),OPTIONAL :: tol
  INTEGER(SIK) :: sout

  INTEGER(SIK) :: i,n
  REAL(SRK) :: loctol
  REAL(SRK),ALLOCATABLE :: tmpr(:)

  n=SIZE(r,DIM=1)
  sout=0
  IF(n > 0) THEN
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
      IF((0.0_SRK < tol)) loctol=tol
    ENDIF

    !Find the number of unique entries
    CALL sort(tmpr)
    sout=1
    DO i=2,n
      IF(.NOT. SOFTEQ(tmpr(i-1),tmpr(i),loctol)) sout=sout+1
    ENDDO
    !Deallocate
    DEALLOCATE(tmpr)
  ENDIF
ENDFUNCTION findNUnique_1DReal
!
!-------------------------------------------------------------------------------
!> @brief This routine takes a 1-D array of integers and returns the number of
!>        unique entries.  The optional delta input is whether the array is
!>        composed of incremental values (deltas) or absolute values.
!> @param r The input array of integers
!> @param delta The optional input for whether the array is incremental or not
!> @returns sout The number of unique entries in the array r.
!>
FUNCTION findNUnique_1DInt(r,delta) RESULT(sout)
  INTEGER(SIK),INTENT(IN) :: r(:)
  LOGICAL(SBK),INTENT(IN),OPTIONAL :: delta
  INTEGER(SIK) :: sout

  INTEGER(SIK) :: i,n
  INTEGER(SIK),ALLOCATABLE :: tmpr(:)

  n=SIZE(r,DIM=1)
  sout=0
  IF(n > 0) THEN
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
  ENDIF
ENDFUNCTION findNUnique_1DInt
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
!> @returns sout The number of unique entries in the array r.
!>
FUNCTION findNUnique_1DString(r,delta,tol) RESULT(sout)
  TYPE(StringType),INTENT(IN) :: r(:)
  LOGICAL(SBK),INTENT(IN),OPTIONAL :: delta
  REAL(SRK),INTENT(IN),OPTIONAL :: tol
  INTEGER(SIK) :: sout

  INTEGER(SIK) :: i,j,n
  TYPE(StringType),ALLOCATABLE :: tmpr(:)

  n=SIZE(r,DIM=1)
  sout=0
  IF(n > 0) THEN
    ALLOCATE(tmpr(n))
    tmpr=r

    !Find the number of unique entries
    DO i=1,n
      DO j=i+1,n
        IF((TRIM(tmpr(i)) == TRIM(tmpr(j))) .AND. (LEN_TRIM(tmpr(j)) > 0)) tmpr(j)=''
      ENDDO
    ENDDO
    DO i=1,n
      IF(TRIM(tmpr(i)) /= '') sout=sout+1
    ENDDO
    !Deallocate
    DEALLOCATE(tmpr)
  ENDIF
ENDFUNCTION findNUnique_1DString
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
!> @returns sout The number of unique entries in the array r.
!>
FUNCTION findNUnique_2DString(r,delta,tol) RESULT(sout)
  TYPE(StringType),INTENT(IN) :: r(:,:)
  LOGICAL(SBK),INTENT(IN),OPTIONAL :: delta
  REAL(SRK),INTENT(IN),OPTIONAL :: tol
  INTEGER(SIK) :: sout

  INTEGER(SIK) :: i,j,n,m,x1,y1,x2,y2
  TYPE(StringType),ALLOCATABLE :: tmpr(:,:)

  n=SIZE(r,DIM=1)
  m=SIZE(r,DIM=2)

  sout=0
  IF(n > 0 .AND. m > 0) THEN
    ALLOCATE(tmpr(n,m))
    tmpr=r

    !Find the number of unique entries
    DO i=1,n*m
      y1=(i-1)/n+1
      x1=i-(y1-1)*n
      DO j=i+1,n*m
        y2=(j-1)/n+1
        x2=j-(y2-1)*n
        IF((TRIM(tmpr(x1,y1)) == TRIM(tmpr(x2,y2))) .AND. (LEN_TRIM(tmpr(x2,y2)) > 0)) tmpr(x2,y2)=''
      ENDDO
    ENDDO
    sout=0
    DO j=1,m
      DO i=1,n
        IF(TRIM(tmpr(i,j)) /= '') sout=sout+1
      ENDDO
    ENDDO
    !Deallocate
    DEALLOCATE(tmpr)
  ENDIF
ENDFUNCTION findNUnique_2DString
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
SUBROUTINE getUnique_1DReal(r,rout,delta,tol)
  REAL(SRK),INTENT(IN) :: r(:)
  REAL(SRK),ALLOCATABLE,INTENT(INOUT) :: rout(:)
  LOGICAL(SBK),INTENT(IN),OPTIONAL :: delta
  REAL(SRK),INTENT(IN),OPTIONAL :: tol

  INTEGER(SIK) :: i,n,sout
  REAL(SRK) :: loctol
  REAL(SRK),ALLOCATABLE :: tmpr(:)

  n=SIZE(r,DIM=1)
  IF(ALLOCATED(rout)) DEALLOCATE(rout)
  IF(n > 0) THEN
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
      IF((0.0_SRK < tol)) loctol=tol
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
    sout=2
    DO i=2,n
      IF(.NOT. SOFTEQ(tmpr(i-1),tmpr(i),loctol)) THEN
        rout(sout)=tmpr(i)
        sout=sout+1
      ENDIF
    ENDDO

    !Deallocate
    DEALLOCATE(tmpr)
  ENDIF
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
SUBROUTINE getUnique_1DInt(r,rout,delta)
  INTEGER(SIK),INTENT(IN) :: r(:)
  INTEGER(SIK),ALLOCATABLE,INTENT(INOUT) :: rout(:)
  LOGICAL(SBK),INTENT(IN),OPTIONAL :: delta

  INTEGER(SIK) :: i,n,sout
  INTEGER(SIK),ALLOCATABLE :: tmpr(:)

  n=SIZE(r,DIM=1)
  IF(ALLOCATED(rout)) DEALLOCATE(rout)
  IF(n > 0) THEN
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
    sout=2
    DO i=2,n
      IF(tmpr(i-1) /= tmpr(i)) THEN
        rout(sout)=tmpr(i)
        sout=sout+1
      ENDIF
    ENDDO
    !Deallocate
    DEALLOCATE(tmpr)
  ENDIF
ENDSUBROUTINE getUnique_1DInt
!
!-------------------------------------------------------------------------------
!> @brief This routine takes an array of integers and returns the unique
!>        entries.  The optional delta input is whether the array is composed of
!>         incremental values (deltas) or absolute values.
!> @param r The input array of integers
!> @param rout The 1-D array of unique entries in the array r.
!> @param delta The optional input for whether the array is incremental or not
!>
SUBROUTINE getUnique_1DString(r,rout,delta)
  TYPE(StringType),INTENT(IN) :: r(:)
  TYPE(StringType),ALLOCATABLE,INTENT(INOUT) :: rout(:)
  LOGICAL(SBK),INTENT(IN),OPTIONAL :: delta

  INTEGER(SIK) :: i,j,n,sout
  TYPE(StringType),ALLOCATABLE :: tmpr(:)

  n=SIZE(r,DIM=1)
  ALLOCATE(tmpr(n))
  tmpr=r

  !Find the number of unique entries
  sout=findNUnique_1DString(r)
  IF(sout > 0) THEN
    IF(ALLOCATED(rout)) DEALLOCATE(rout)
    ALLOCATE(rout(sout))
    rout=''
    !remove duplicate entries
    DO i=1,n
      DO j=i+1,n
        IF(TRIM(tmpr(i)) == TRIM(tmpr(j)) .AND. (LEN_TRIM(tmpr(j)) > 0)) tmpr(j)=''
      ENDDO
    ENDDO

    sout=1
    DO i=1,n
      IF(TRIM(tmpr(i)) /= '') THEN
        rout(sout)=tmpr(i)
        sout=sout+1
      ENDIF
    ENDDO
  ENDIF

  !Deallocate
  DEALLOCATE(tmpr)
ENDSUBROUTINE getUnique_1DString
!
!-------------------------------------------------------------------------------
!> @brief This routine takes an array of integers and returns the unique
!>        entries.  The optional delta input is whether the array is composed of
!>         incremental values (deltas) or absolute values.
!> @param r The input array of integers
!> @param rout The 1-D array of unique entries in the array r.
!> @param delta The optional input for whether the array is incremental or not
!>
SUBROUTINE getUnique_2DString(r,rout,delta)
  TYPE(StringType),INTENT(IN) :: r(:,:)
  TYPE(StringType),ALLOCATABLE,INTENT(INOUT) :: rout(:)
  LOGICAL(SBK),INTENT(IN),OPTIONAL :: delta

  INTEGER(SIK) :: i,j,n,m,x1,y1,x2,y2,sout
  TYPE(StringType),ALLOCATABLE :: tmpr(:,:)
  n=SIZE(r,DIM=1)
  m=SIZE(r,DIM=2)
  ALLOCATE(tmpr(n,m))
  tmpr=r

  !Find the number of unique entries
  sout=findNUnique_2DString(r)
  IF(sout > 0) THEN
    IF(ALLOCATED(rout)) DEALLOCATE(rout)
    ALLOCATE(rout(sout))
    rout=''
    !Remove the duplicate entries
    DO i=1,n*m
      y1=(i-1)/n+1
      x1=i-(y1-1)*n
      DO j=i+1,n*m
        y2=(j-1)/n+1
        x2=j-(y2-1)*n
        IF((TRIM(tmpr(x1,y1)) == TRIM(tmpr(x2,y2))) .AND. (LEN_TRIM(tmpr(x2,y2)) > 0)) tmpr(x2,y2)=''
      ENDDO
    ENDDO
    !Assign unique entries
    sout=1
    DO j=1,m
      DO i=1,n
        IF(TRIM(tmpr(i,j)) /= '') THEN
          rout(sout)=tmpr(i,j)
          sout=sout+1
        ENDIF
      ENDDO
    ENDDO
  ENDIF
  !Deallocate
  DEALLOCATE(tmpr)
ENDSUBROUTINE getUnique_2DString
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
!> @param tol The optional value used to perform the SOFTEQ check
!>
SUBROUTINE getUnion_1DReal(r1,r2,rout,xi1,xi2,delta1,delta2,deltaout,tol)
  REAL(SRK),INTENT(IN) :: r1(:)
  REAL(SRK),INTENT(IN) :: r2(:)
  REAL(SRK),ALLOCATABLE,INTENT(INOUT) :: rout(:)
  REAL(SRK),INTENT(IN),OPTIONAL :: xi1
  REAL(SRK),INTENT(IN),OPTIONAL :: xi2
  LOGICAL(SBK),INTENT(IN),OPTIONAL :: delta1
  LOGICAL(SBK),INTENT(IN),OPTIONAL :: delta2
  LOGICAL(SBK),INTENT(IN),OPTIONAL :: deltaout
  REAL(SRK),INTENT(IN),OPTIONAL :: tol
  LOGICAL(SBK) :: bool1,bool2
  INTEGER(SIK) :: i,j,sr1,sr2,sout,tmpsout
  REAL(SRK) :: localtol
  REAL(SRK),ALLOCATABLE :: tmpout(:),tmpout2(:),tmp1(:),tmp2(:)

  localtol=EPSREAL
  IF(PRESENT(tol)) localtol=tol
  IF(ALLOCATED(rout)) DEALLOCATE(rout)
  !Process the first array if it is a delta
  IF(SIZE(r1) > 0 .AND. SIZE(r2) == 0) THEN
    ALLOCATE(rout(SIZE(r1)))
    rout=r1
    !Need checks for delta in, xi1, and delta out
  ELSEIF(SIZE(r1) == 0 .AND. SIZE(r2) > 0) THEN
    ALLOCATE(rout(SIZE(r2)))
    rout=r2
    !Need checks for delta in, xi2, and delta out
  ELSEIF(SIZE(r1) > 0 .AND. SIZE(r2) > 0) THEN
    bool1=.FALSE.
    sr1=SIZE(r1,DIM=1)
    IF(PRESENT(delta1)) THEN
      IF(delta1) THEN
        IF(PRESENT(xi1)) THEN
          CALL getAbsolute_1DReal(r1,tmp1,XI=xi1)
        ELSE
          CALL getAbsolute_1DReal(r1,tmp1)
        ENDIF
        bool1=.TRUE.
        sr1=SIZE(tmp1,DIM=1)
      ENDIF
    ENDIF
    !Process the second array if it is a delta
    bool2=.FALSE.
    sr2=SIZE(r2,DIM=1)
    IF(PRESENT(delta2)) THEN
      IF(delta2) THEN
        IF(PRESENT(xi2)) THEN
          CALL getAbsolute_1DReal(r2,tmp2,XI=xi2)
        ELSE
          CALL getAbsolute_1DReal(r2,tmp2)
        ENDIF
        bool2=.TRUE.
        sr2=SIZE(tmp2,DIM=1)
      ENDIF
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
      tmpout(sr1+1:tmpsout)=tmp2
    ELSE
      tmpout(sr1+1:tmpsout)=r2
    ENDIF
    !Sort the array components to make finding repeated values easy.
    CALL sort(tmpout)
    sout=tmpsout
    !Find the number of repeated values
    DO i=2,tmpsout
      IF(SOFTEQ(tmpout(i-1),tmpout(i),localtol)) sout=sout-1
    ENDDO
    ALLOCATE(tmpout2(sout))
    tmpout2=0.0_SRK
    tmpout2(1)=tmpout(1)
    j=2
    !Assign the non-repeated values
    DO i=2,tmpsout
      IF(.NOT.SOFTEQ(tmpout(i-1),tmpout(i),localtol)) THEN
        tmpout2(j)=tmpout(i)
        j=j+1
      ENDIF
    ENDDO
    !If the output requires a delta array, convert it.
    IF(PRESENT(deltaout)) THEN
      IF(deltaout) THEN
        CALL getDelta_1DReal(tmpout2,rout)
      ELSE
        ALLOCATE(rout(sout))
        rout=tmpout2
      ENDIF
    ELSE
      ALLOCATE(rout(sout))
      rout=tmpout2
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
!> @returns ind The array index where pos is located in r.
!>
PURE FUNCTION findIndex_1DReal(r,pos,delta,incl,tol) RESULT(ind)
  REAL(SRK),INTENT(IN) :: r(:)
  REAL(SRK),INTENT(IN) :: pos
  LOGICAL(SBK),INTENT(IN) :: delta
  INTEGER(SIK),INTENT(IN),OPTIONAL :: incl
  REAL(SRK),INTENT(IN),OPTIONAL :: tol
  INTEGER(SIK) :: ind
  INTEGER(SIK) :: i,n,l_incl
  REAL(SRK) :: tmp(SIZE(r,DIM=1)+1),l_tol

  !Initialize the tmp array and adjust for any offset xi
  n=SIZE(r,DIM=1)
  tmp=0.0_SRK
  tmp(1:n)=r

  !If the array is in increments/deltas and not full values
  IF(delta) THEN
    DO i=2,SIZE(tmp,DIM=1)
      tmp(i)=tmp(i-1)+tmp(i)
    ENDDO
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
    IF((0.0_SRK < tol)) l_tol=tol
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
PURE FUNCTION findIndex_1DInt(r,pos,delta,incl) RESULT(ind)
  INTEGER(SIK),INTENT(IN) :: r(:)
  INTEGER(SIK),INTENT(IN) :: pos
  LOGICAL(SBK),INTENT(IN) :: delta
  INTEGER(SIK),INTENT(IN),OPTIONAL :: incl
  INTEGER(SIK) :: ind
  INTEGER(SIK) :: n,i,l_incl
  INTEGER(SIK) :: tmp(SIZE(r,DIM=1)+1)

  !Initialize the tmp array and adjust for any offset xi
  n=SIZE(r,DIM=1)
  tmp=0
  tmp(1:n)=r

  !If the array is in increments/deltas and not full values
  IF(delta) THEN
    DO i=2,SIZE(tmp,DIM=1)
      tmp(i)=tmp(i-1)+tmp(i)
    ENDDO
  ENDIF

  !If the logic should be inclusive or exclusive
  l_incl=0
  IF(PRESENT(incl)) THEN
    IF((0 <= incl) .AND. (incl <= 2)) l_incl=incl
  ENDIF

  !Below the array
  IF(pos < tmp(1)) THEN
    ind=-1
  !Above the array
  ELSEIF(tmp(n) < pos) THEN
    ind=-2
  !Inbetween, error if on mesh
  ELSEIF(l_incl == 0) THEN
    IF(ANY(tmp == pos)) THEN
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
    IF((tmp(1) <= pos) .AND. (pos <= tmp(n))) THEN
      ind=1
      DO WHILE(pos > tmp(ind))
        ind=ind+1
        IF(ind > n) EXIT
      ENDDO
      ind=ind-1
      IF((l_incl == 1) .AND. (pos == tmp(ind))) ind=ind-1
      IF((l_incl == 1) .AND. (ind == 0)) ind=-1
      IF((l_incl == 2) .AND. (ind == n)) ind=-2
      IF(ind == n) ind=ind-1
    ENDIF
  ENDIF
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
!> @returns val The nearest lesser value
!>
PURE FUNCTION findLowBound_1DReal(r,pos,delta,incl,tol) RESULT(val)
  REAL(SRK),INTENT(IN) :: r(:)
  REAL(SRK),INTENT(IN) :: pos
  LOGICAL(SBK),INTENT(IN) :: delta
  INTEGER(SIK),INTENT(IN),OPTIONAL :: incl
  REAL(SRK),INTENT(IN),OPTIONAL :: tol
  REAL(SRK) :: val,l_tol
  INTEGER(SIK) :: ind
  INTEGER(SIK) :: i,n,l_incl
  REAL(SRK) :: tmp(SIZE(r,DIM=1)+1)

  !Initialize the tmp array and adjust for any offset xi
  val=0.0_SRK
  n=SIZE(r,DIM=1)
  tmp=0.0_SRK
  tmp(1:n)=r

  !If the array is in increments/deltas and not full values
  IF(delta) THEN
    DO i=2,SIZE(tmp,DIM=1)
      tmp(i)=tmp(i-1)+tmp(i)
    ENDDO
  ENDIF

  !If incl is present
  l_incl=0
  IF(PRESENT(incl)) THEN
    IF((0 <= incl) .AND. (incl <= 2)) l_incl=incl
  ENDIF

  !If tol is present
  l_tol=EPSREAL
  IF(PRESENT(tol)) THEN
    IF((0.0_SRK < tol)) l_tol=tol
  ENDIF

  IF((tmp(1) .APPROXLE. pos) .AND. (pos .APPROXLE. tmp(n))) THEN
    ind=1
    DO WHILE(SOFTGE(pos,tmp(ind),l_tol))
      IF(ind == n) EXIT
      IF(l_incl == 1) THEN
        IF(SOFTEQ(pos,tmp(ind),l_tol)) EXIT
      ENDIF
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
!> @returns val The nearest greater value
!>
PURE FUNCTION findUpBound_1DReal(r,pos,delta,incl,tol) RESULT(val)
  REAL(SRK),INTENT(IN) :: r(:)
  REAL(SRK),INTENT(IN) :: pos
  LOGICAL(SBK),INTENT(IN) :: delta
  INTEGER(SIK),INTENT(IN),OPTIONAL :: incl
  REAL(SRK),INTENT(IN),OPTIONAL :: tol
  REAL(SRK) :: val,l_tol
  INTEGER(SIK) :: ind
  INTEGER(SIK) :: i,n,l_incl
  REAL(SRK) :: tmp(SIZE(r,DIM=1)+1)

  !Initialize the tmp array and adjust for any offset xi
  val=0.0_SRK
  n=SIZE(r,DIM=1)
  tmp=0.0_SRK
  tmp(1:n)=r

  !If the array is in increments/deltas and not full values
  IF(delta) THEN
    DO i=2,SIZE(tmp,DIM=1)
      tmp(i)=tmp(i-1)+tmp(i)
    ENDDO
  ENDIF

  !If incl is present
  l_incl=0
  IF(PRESENT(incl)) THEN
    IF((0 <= incl) .AND. (incl <= 2)) l_incl=incl
  ENDIF

  !If tol is present
  l_tol=EPSREAL
  IF(PRESENT(tol)) THEN
    IF((0.0_SRK < tol)) l_tol=tol
  ENDIF

  IF((tmp(1) .APPROXLE. pos) .AND. (pos .APPROXLE. tmp(n))) THEN
    ind=1
    DO WHILE(SOFTGE(pos,tmp(ind),l_tol))
      IF(ind == n) EXIT
      IF(l_incl == 1) THEN
        IF(SOFTEQ(pos,tmp(ind),l_tol)) EXIT
      ENDIF
      ind=ind+1
    ENDDO
    val=tmp(ind)
  ENDIF
ENDFUNCTION findUpBound_1DReal
!
!-------------------------------------------------------------------------------
!> @brief Routine that checks that the entries of r either strictly increase or decrease.
!> @param r input array to check
!> @returns good boolean indicating whether the check has passed or not
!>
PURE FUNCTION isStrictlyIncDec_1DInt(r) RESULT(good)
  INTEGER(SIK),INTENT(IN) :: r(:)
  LOGICAL(SBK) :: good
  good=.FALSE.

  IF(SIZE(r) > 1) THEN
    IF(r(1) < r(2)) THEN
      good=isStrictlyIncreasing_1DInt(r)
    ELSE
      good=isStrictlyDecreasing_1DInt(r)
    ENDIF
  ENDIF

ENDFUNCTION isStrictlyIncDec_1DInt
!
!-------------------------------------------------------------------------------
!> @brief Routine that checks that the entries of r strictly increase.
!> @param r input array to check
!> @returns good boolean indicating whether the check has passed or not
!>
PURE FUNCTION isStrictlyIncreasing_1DInt(r) RESULT(good)
  INTEGER(SIK),INTENT(IN) :: r(:)
  INTEGER(SIK) :: i
  LOGICAL(SBK) :: good
  good=.TRUE.

  IF(SIZE(r) > 1) THEN
    DO i=2,SIZE(r)
      IF(r(i) <= r(i-1)) good=.FALSE.
    ENDDO
  ELSE
    good=.FALSE.
  ENDIF

ENDFUNCTION isStrictlyIncreasing_1DInt
!
!-------------------------------------------------------------------------------
!> @brief Routine that checks that the entries of r strictly decrease.
!> @param r input array to check
!> @returns good boolean indicating whether the check has passed or not
!>
PURE FUNCTION isStrictlyDecreasing_1DInt(r) RESULT(good)
  INTEGER(SIK),INTENT(IN) :: r(:)
  INTEGER(SIK) :: i
  LOGICAL(SBK) :: good
  good=.TRUE.

  IF(SIZE(r) > 1) THEN
    DO i=2,SIZE(r)
      IF(r(i) >= r(i-1)) good=.FALSE.
    ENDDO
  ELSE
    good=.FALSE.
  ENDIF

ENDFUNCTION isStrictlyDecreasing_1DInt
!
!-------------------------------------------------------------------------------
!> @brief Routine that checks that the entries of r either monotonically increase or decrease.
!> @param r input array to check for monotonicity of values
!> @returns good boolean indicating whether the check has passed or not
!>
PURE FUNCTION isMonotonic_1DInt(r) RESULT(good)
  INTEGER(SIK),INTENT(IN) :: r(:)
  LOGICAL(SBK) :: good
  good=.FALSE.

  IF(SIZE(r) > 1) THEN
    IF(r(1) < r(SIZE(r))) THEN
      good=isIncreasing_1DInt(r)
    ELSE
      good=isDecreasing_1DInt(r)
    ENDIF
  ENDIF

ENDFUNCTION isMonotonic_1DInt
!
!-------------------------------------------------------------------------------
!> @brief Routine that checks that the entries of r monotonically increase.
!> @param r input array to check for monotonicity of values
!> @returns good boolean indicating whether the check has passed or not
!>
PURE FUNCTION isIncreasing_1DInt(r) RESULT(good)
  INTEGER(SIK),INTENT(IN) :: r(:)
  INTEGER(SIK) :: i
  LOGICAL(SBK) :: good
  good=.TRUE.

  IF(SIZE(r) > 1) THEN
    DO i=2,SIZE(r)
      IF(r(i) < r(i-1)) good=.FALSE.
    ENDDO
  ELSE
    good=.FALSE.
  ENDIF

ENDFUNCTION isIncreasing_1DInt
!
!-------------------------------------------------------------------------------
!> @brief Routine that checks that the entries of r monotonically decrease.
!> @param r input array to check for monotonicity of values
!> @returns good boolean indicating whether the check has passed or not
!>
PURE FUNCTION isDecreasing_1DInt(r) RESULT(good)
  INTEGER(SIK),INTENT(IN) :: r(:)
  INTEGER(SIK) :: i
  LOGICAL(SBK) :: good
  good=.TRUE.

  IF(SIZE(r) > 1) THEN
    DO i=2,SIZE(r)
      IF(r(i) > r(i-1)) good=.FALSE.
    ENDDO
  ELSE
    good=.FALSE.
  ENDIF

ENDFUNCTION isDecreasing_1DInt
!
!-------------------------------------------------------------------------------
!> @brief Routine that checks that the entries of r either strictly increase or decrease.
!> @param r input array to check
!> @returns good boolean indicating whether the check has passed or not
!>
PURE FUNCTION isStrictlyIncDec_1DReal(r) RESULT(good)
  REAL(SRK),INTENT(IN) :: r(:)
  LOGICAL(SBK) :: good
  good=.FALSE.

  IF(SIZE(r) > 1) THEN
    IF(SOFTLT(r(1),r(2),EPSREAL)) THEN
      good=isStrictlyIncreasing(r)
    ELSE
      good=isStrictlyDecreasing(r)
    ENDIF
  ENDIF

ENDFUNCTION isStrictlyIncDec_1DReal
!
!-------------------------------------------------------------------------------
!> @brief Routine that checks that the entries of r strictly increase.
!>        Note this is limited to 1 dimensional real arrays.
!> @param r input array to check for monotonicity of values
!> @returns good boolean indicating whether the check has passed or not
!>
PURE FUNCTION isStrictlyIncreasing_1DReal(r) RESULT(good)
  REAL(SRK),INTENT(IN) :: r(:)
  INTEGER(SIK) :: i
  LOGICAL(SBK) :: good
  good=.TRUE.

  IF(SIZE(r) > 1) THEN
    DO i=2,SIZE(r)
      IF(SOFTLE(r(i),r(i-1),EPSREAL)) good=.FALSE.
    ENDDO
  ELSE
    good=.FALSE.
  ENDIF

ENDFUNCTION isStrictlyIncreasing_1DReal
!
!-------------------------------------------------------------------------------
!> @brief Routine that checks that the entries of r strictly decrease.
!>        Note this is limited to 1 dimensional real arrays.
!> @param r input array to check for monotonicity of values
!> @returns good boolean indicating whether the check has passed or not
!>
PURE FUNCTION isStrictlyDecreasing_1DReal(r) RESULT(good)
  REAL(SRK),INTENT(IN) :: r(:)
  INTEGER(SIK) :: i
  LOGICAL(SBK) :: good
  good=.TRUE.

  IF(SIZE(r) > 1) THEN
    DO i=2,SIZE(r)
      IF(SOFTGE(r(i),r(i-1),EPSREAL)) good=.FALSE.
    ENDDO
  ELSE
    good=.FALSE.
  ENDIF

ENDFUNCTION isStrictlyDecreasing_1DReal
!
!-------------------------------------------------------------------------------
!> @brief Routine that checks that the entries of r either monotonically increase or decrease.
!>        Note this is limited to 1 dimensional real arrays.
!> @param r input array to check for monotonicity of values
!> @returns good boolean indicating whether the check has passed or not
!>
PURE FUNCTION isMonotonic_1DReal(r) RESULT(good)
  REAL(SRK),INTENT(IN) :: r(:)
  LOGICAL(SBK) :: good
  good=.FALSE.

  IF(SIZE(r) > 1) THEN
    IF(SOFTLT(r(1),r(SIZE(r)),EPSREAL)) THEN
      good=isIncreasing(r)
    ELSE
      good=isDecreasing(r)
    ENDIF
  ENDIF

ENDFUNCTION isMonotonic_1DReal
!
!-------------------------------------------------------------------------------
!> @brief Routine that checks that the entries of r monotonically increase.
!>        Note this is limited to 1 dimensional real arrays.
!> @param r input array to check for monotonicity of values
!> @returns good boolean indicating whether the check has passed or not
!>
PURE FUNCTION isIncreasing_1DReal(r) RESULT(good)
  REAL(SRK),INTENT(IN) :: r(:)
  INTEGER(SIK) :: i
  LOGICAL(SBK) :: good
  good=.TRUE.

  IF(SIZE(r) > 1) THEN
    DO i=2,SIZE(r)
      IF(SOFTLT(r(i),r(i-1),EPSREAL)) good=.FALSE.
    ENDDO
  ELSE
    good=.FALSE.
  ENDIF

ENDFUNCTION isIncreasing_1DReal
!
!-------------------------------------------------------------------------------
!> @brief Routine that checks that the entries of r monotonically decrease.
!>        Note this is limited to 1 dimensional real arrays.
!> @param r input array to check for monotonicity of values
!> @returns good boolean indicating whether the check has passed or not
!>
PURE FUNCTION isDecreasing_1DReal(r) RESULT(good)
  REAL(SRK),INTENT(IN) :: r(:)
  INTEGER(SIK) :: i
  LOGICAL(SBK) :: good
  good=.TRUE.

  IF(SIZE(r) > 1) THEN
    DO i=2,SIZE(r)
      IF(SOFTGT(r(i),r(i-1),EPSREAL)) good=.FALSE.
    ENDDO
  ELSE
    good=.FALSE.
  ENDIF

ENDFUNCTION isDecreasing_1DReal
!
!-------------------------------------------------------------------------------
!> @brief Queries an array to see if any value is specified with a greater
!>        precision than the input tolerance value.
!> @param r The input real array to query
!> @param tol a number between 1.0E-14 and 1 to query the values on the array.
!>
FUNCTION hasAnyRemainder(r,tol) RESULT(bool)
  REAL(SRK),INTENT(IN) :: r(:)
  REAL(SRK),INTENT(IN) :: tol
  LOGICAL(SBK) :: bool

  REAL(SRK) :: rem(SIZE(r)),tmpr
  CHARACTER(LEN=32) :: dchar,fmtstr,fchar
  INTEGER(SIK) :: i,d,w

  REQUIRE(tol < 1.0_SRK)
  REQUIRE(tol >= 1.0E-13_SRK)

  !Construct format string
  WRITE(dchar,'(es12.5)') tol
  READ(dchar(11:12),*) d
  w=d+10

  !Store the remainder
  rem=r

  !Round floating point numbers
  WRITE(fmtstr,'(a,i0.4,a,i0.4,a)') '(f',w,'.',d,')'
  DO i=1,SIZE(r)
    WRITE(fchar,FMT=fmtstr) r(i)+1.0E-14_SRK
    READ(fchar,*) tmpr
    rem(i)=rem(i)-tmpr
  ENDDO

  !Compute the remainder, zero any floating precision values.
  WHERE(ABS(rem) < 1.0E-14_SRK) rem=0.0_SRK
  bool=ANY(rem > 1.0E-14_SRK)
ENDFUNCTION hasAnyRemainder
!
ENDMODULE ArrayUtils
