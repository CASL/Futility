!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!> @brief A Fortran 2003 module defining "Points" for use in geometry.
!>
!> This module provides derived data types for a "point" in any dimensional
!> space. It also provides methods for constructing and destruction the points
!> and printing the point information. Two other routines are provided to return
!> the distance between two points and to return the midpoint between two points.
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE Geom_Points
USE IntrType
USE Constants_Conversion
USE ExtendedMath

IMPLICIT NONE
PRIVATE !Default contents of module to private
!
! List of Public items
PUBLIC :: MAX_COORD_STR_LEN
PUBLIC :: PointType
PUBLIC :: LinkedListPointType
PUBLIC :: ClearLinkedListPointType
PUBLIC :: Distance
PUBLIC :: DOT_PRODUCT
PUBLIC :: cross
PUBLIC :: midPoint
PUBLIC :: innerAngle
PUBLIC :: outerAngle
PUBLIC :: OPERATOR(+)
PUBLIC :: OPERATOR(-)
PUBLIC :: OPERATOR(==)
PUBLIC :: OPERATOR(/=)
PUBLIC :: OPERATOR(.APPROXEQA.)
PUBLIC :: ASSIGNMENT(=)

INTEGER(SIK),PARAMETER :: MAX_COORD_STR_LEN=128

!> @brief The derived type for a point
TYPE :: PointType
  !> The number of dimensions of space in which this point lives
  INTEGER(SIK) :: dim=0
  !> Coordinate values for each dimension of the point (X=1, Y=2, Z=3)
  REAL(SRK),ALLOCATABLE :: coord(:)
!
!List of type bound procedures
  CONTAINS
    !> @copybrief GeomPoints::init_PointType
    !> @copydetails GeomPoints::init_PointType
    PROCEDURE,PASS :: init => init_PointType
    !> @copybrief GeomPoints::clear_PointType
    !> @copydetails GeomPoints::clear_PointType
    PROCEDURE,PASS :: clear => clear_PointType
    !> @copybrief GeomPoints::RotateQtrClockwise_PointType
    !> @copydetails GeomPoints::RotateQtrClockwise_PointType
    PROCEDURE,PASS :: RotateQtrClockwise => RotateQtrClockwise_PointType
    !> @copybrief GeomPoints::coord2str_PointType
    !> @copydetails GeomPoints::coord2str_PointType
    PROCEDURE,PASS :: getCoordString => coord2str_PointType
ENDTYPE PointType

!> A linked list type of points
!>
!> This is used by the ray trace routines that determine segment information
TYPE :: LinkedListPointType
  !> A sorting value for a comparison operation. This is most likely the
  !> distance to a reference point.
  REAL(SRK) :: sortval=0.0_SRK
  !> The point type for this record in the list
  TYPE(PointType):: p
  !> Is this point a duplicate?
  LOGICAL(SBK) :: isDuplicate=.FALSE.
  !> The next record in the list.
  TYPE(LinkedListPointType),POINTER :: next => NULL()
!
!List of type bound procedures
  CONTAINS
    !> @copybrief GeomPoints::insert_LinkedListPointType
    !> @copydetails GeomPoints::insert_LinkedListPointType
    PROCEDURE,PASS :: insert => insert_LinkedListPointType
ENDTYPE LinkedListPointType

!> @brief Generic interface for computing innerAngle
!>
!> Adds the listed module procedures to the global interface name for innerAngle
INTERFACE innerAngle
  !> @copybrief GeomPoints::innerAngle_3points
  !> @copydetails GeomPoints::innerAngle_3points
  MODULE PROCEDURE innerAngle_3points
ENDINTERFACE innerAngle

!> @brief Generic interface for computing outerAngle
!>
!> Adds the listed module procedures to the global interface name for outerAngle
INTERFACE outerAngle
  !> @copybrief GeomPoints::outerAngle_3points
  !> @copydetails GeomPoints::outerAngle_3points
  MODULE PROCEDURE outerAngle_3points
ENDINTERFACE outerAngle

!> @brief Generic interface for computing midpoint
!>
!> Adds the listed module procedures to the global interface name for midPoint
INTERFACE midPoint
  !> @copybrief GeomPoints::midPoint_2points
  !> @copydetails GeomPoints::midPoint_2points
  MODULE PROCEDURE midPoint_2points
ENDINTERFACE midPoint

!> @brief Generic interface for computing distance
!>
!> Adds the listed module procedures to the global interface name for Distance
INTERFACE Distance
  !> @copybrief GeomPoints::distance_2points
  !> @copydetails GeomPoints::distance_2points
  MODULE PROCEDURE distance_2points
ENDINTERFACE Distance

!> @brief Generic interface for the dot product
INTERFACE DOT_PRODUCT
  !> @copybrief GeomPoints::DOT_PRODUCT_2points
  !> @copydetails GeomPoints::DOT_PRODUCT_2points
  MODULE PROCEDURE DOT_PRODUCT_2points
ENDINTERFACE DOT_PRODUCT

!> @brief Generic interface for computing distance
INTERFACE cross
  !> @copybrief GeomPoints::cross_2points
  !> @copydetails GeomPoints::cross_2points
  MODULE PROCEDURE cross_2points
ENDINTERFACE cross

!> @brief Generic interface for addition operator (+)
!>
!> Adds addition capability for point types
INTERFACE OPERATOR(+)
  !> @copybrief GeomPoints::add_PointType
  !> @copydetails GeomPoints::add_PointType
  MODULE PROCEDURE add_PointType
ENDINTERFACE

!> @brief Generic interface for subtraction operator (-)
!>
!> Adds subtraction capability for point types
INTERFACE OPERATOR(-)
  !> @copybrief GeomPoints::subtract_PointType
  !> @copydetails GeomPoints::subtract_PointType
  MODULE PROCEDURE subtract_PointType
ENDINTERFACE

!> @brief Generic interface for 'is equal to' operator (==)
!>
!> Adds 'is equal to' capability for point types
INTERFACE OPERATOR(==)
  !> @copybrief GeomPoints::isequal_PointType
  !> @copydetails GeomPoints::isequal_PointType
  MODULE PROCEDURE isequal_PointType
ENDINTERFACE

!> @brief Generic interface for 'is not equal to' operator (/=)
!>
!> Adds 'is not equal to' capability for point types
INTERFACE OPERATOR(/=)
  !> @copybrief GeomPoints::notequal_PointType
  !> @copydetails GeomPoints::notequal_PointType
  MODULE PROCEDURE notequal_PointType
ENDINTERFACE

!> @brief Generic interface for 'is approximately equal to' operator
!> (.APPROXEQA.)
!>
!> Adds 'is approximately equal to' capability for point types
INTERFACE OPERATOR(.APPROXEQA.)
  !> @copybrief GeomPoints::approxequal_PointType
  !> @copydetails GeomPoints::approxequal_PointType
  MODULE PROCEDURE approxequal_PointType
ENDINTERFACE

!> @brief Generic interface for assignment operator (=)
!>
!> Adds assignment capability for point types. Done primarily to help
!> eliminate valgrind errors in VERA.
INTERFACE ASSIGNMENT(=)
  !> @copybrief GeomPoints::assign_PointType
  !> @copydetails GeomPoints::assign_PointType
  MODULE PROCEDURE assign_PointType
ENDINTERFACE
!
!===============================================================================
CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief Constructor for a point with a double precision argument
!> @param p the point type variable to initialize
!> @param DIM the number of dimensions of space in which this point lives
!>        (optional).
!> @param X the x-coordinate of the point (OPTIONAL)
!> @param Y the y-coordinate of the point (OPTIONAL)
!> @param Z the z-coordinate of the point (OPTIONAL)
!> @param coord an array with the coordinates of the point (OPTIONAL)
!>
!> There are essentially only a few ways to use this routine. If it is not
!> called in this way then the routine will return the point uninitialized.
!> Below are examples of how to correctly use this routine to create a point at
!> origin in various dimensions.
!>
!> @code
!> TYPE(PointType) :: p
!> CALL p%init(COORD=(/0.0,0.0,0.0,0.0/))
!> @endcode
!>
!> @code
!> TYPE(PointType) :: p
!> CALL p%init(DIM=1,X=0.0)
!> @endcode
!>
!> @code
!> TYPE(PointType) :: p
!> CALL p%init(DIM=2,X=0.0,Y=0.0)
!> @endcode
!>
!> @code
!> TYPE(PointType) :: p
!> CALL p%init(DIM=3,X=0.0,Y=0.0,Z=0.0)
!> @endcode
PURE SUBROUTINE init_PointType(p,DIM,X,Y,Z,coord)
  CLASS(PointType),INTENT(INOUT) :: p
  INTEGER(SIK),INTENT(IN),OPTIONAL :: DIM
  REAL(SRK),INTENT(IN),OPTIONAL :: X,Y,Z
  REAL(SRK),INTENT(IN),OPTIONAL :: coord(:)

  IF(.NOT.ALLOCATED(p%coord)) THEN !initialize only if its not already initialized
    IF(PRESENT(coord)) THEN
      p%dim=SIZE(coord)
      ALLOCATE(p%coord(1:p%dim))
      p%coord=coord
    ELSEIF(PRESENT(DIM)) THEN
      SELECTCASE(DIM)
      CASE(1)
        IF(PRESENT(X)) THEN
          p%dim=1
          ALLOCATE(p%coord(1))
          p%coord(1)=X
        ENDIF
      CASE(2)
        IF(PRESENT(X) .AND. PRESENT(Y)) THEN
          p%dim=2
          ALLOCATE(p%coord(2))
          p%coord(1)=X
          p%coord(2)=Y
        ENDIF
      CASE(3)
        IF(PRESENT(X) .AND. PRESENT(Y) .AND. PRESENT(Z)) THEN
          p%dim=3
          ALLOCATE(p%coord(3))
          p%coord(1)=X
          p%coord(2)=Y
          p%coord(3)=Z
        ENDIF
      ENDSELECT
    ENDIF
  ENDIF
ENDSUBROUTINE init_PointType
!
!-------------------------------------------------------------------------------
!> @brief Routine clears the data in a point type variable
!> @param p the point type variable to clear
ELEMENTAL SUBROUTINE clear_PointType(p)
  CLASS(PointType),INTENT(INOUT) :: p
  p%dim=0
  IF(ALLOCATED(p%coord)) DEALLOCATE(p%coord)
ENDSUBROUTINE clear_PointType
!
!-------------------------------------------------------------------------------
!> @brief Routine rotates the x-y coordinates of a point type variable @nrot number
!>        of clockwise quarter rotations.
!> @param p the point type variable to rotate
!> @param nrot the number of clockwise quarter rotations.
!>
!> @note: Values from [-3,-1] will be handled as well, even though they
!>        represent counter clockwise rotations.  This will only work for 2-D or
!>        greater point dimensions.
ELEMENTAL SUBROUTINE RotateQtrClockwise_PointType(p,nrot)
  CLASS(PointType),INTENT(INOUT) :: p
  INTEGER(SIK),INTENT(IN) :: nrot 
  
  IF(ALLOCATED(p%coord) .AND. p%dim >= 2) &
      CALL RotateQtrClockwise(p%coord(1),p%coord(2),nrot)
ENDSUBROUTINE RotateQtrClockwise_PointType
!
!-------------------------------------------------------------------------------
!> @brief Defines the addition operation between two points e.g. @c p=p0+p1
!> @param p0 the first point
!> @param p1 the second point
!> @returns @c p the return point value
!>
!> Function is elemental so it can be used on an array of points.
ELEMENTAL FUNCTION add_PointType(p0,p1) RESULT(p)
  TYPE(PointType),INTENT(IN) :: p0,p1
  TYPE(PointType) :: p
  CALL p%clear()
  IF(p0%dim == p1%dim .AND. p0%dim > 0) THEN
    p%dim=p0%dim
    ALLOCATE(p%coord(p%dim))
    SELECTCASE(p%dim) !Explicit unrolling, this may improve performance
    CASE(1)
      p%coord(1)=p0%coord(1)+p1%coord(1)
    CASE(2)
      p%coord(1)=p0%coord(1)+p1%coord(1)
      p%coord(2)=p0%coord(2)+p1%coord(2)
    CASE(3)
      p%coord(1)=p0%coord(1)+p1%coord(1)
      p%coord(2)=p0%coord(2)+p1%coord(2)
      p%coord(3)=p0%coord(3)+p1%coord(3)
    CASE DEFAULT
      p%coord=p0%coord+p1%coord
    ENDSELECT
  ENDIF
ENDFUNCTION add_PointType
!
!-------------------------------------------------------------------------------
!> @brief Defines the subtraction operation between two points e.g. @c p=p0-p1
!> @param p0 the first point
!> @param p1 the second point
!> @returns @c p the return point value
!>
!> Function is elemental so it can be used on an array of points.
ELEMENTAL FUNCTION subtract_PointType(p0,p1) RESULT(p)
  TYPE(PointType),INTENT(IN) :: p0,p1
  TYPE(PointType) :: p
  CALL p%clear()
  IF(p0%dim == p1%dim .AND. p0%dim > 0) THEN
    p%dim=p0%dim
    ALLOCATE(p%coord(p%dim))
    SELECTCASE(p%dim) !Explicit unrolling, this may improve performance
    CASE(1)
      p%coord(1)=p0%coord(1)-p1%coord(1)
    CASE(2)
      p%coord(1)=p0%coord(1)-p1%coord(1)
      p%coord(2)=p0%coord(2)-p1%coord(2)
    CASE(3)
      p%coord(1)=p0%coord(1)-p1%coord(1)
      p%coord(2)=p0%coord(2)-p1%coord(2)
      p%coord(3)=p0%coord(3)-p1%coord(3)
    CASE DEFAULT
      p%coord=p0%coord-p1%coord
    ENDSELECT
  ENDIF
ENDFUNCTION subtract_PointType
!
!-------------------------------------------------------------------------------
!> @brief Defines the 'is equal to' operation between two points e.g. @c p0==p1
!> @param p0 the first point
!> @param p1 the second point
!> @returns @c bool the boolean result of the operation
!>
!> Function is elemental so it can be used on an array of points.
ELEMENTAL FUNCTION isequal_PointType(p0,p1) RESULT(bool)
  TYPE(PointType),INTENT(IN) :: p0,p1
  LOGICAL(SBK) :: bool
  bool=.FALSE.
  IF(p0%dim == p1%dim .AND. p0%dim > 0) &
      bool=ALL(p0%coord == p1%coord)
ENDFUNCTION isequal_PointType
!
!-------------------------------------------------------------------------------
!> @brief Defines the 'is not equal to' operation between two points e.g. @c p0/=p1
!> @param p0 the first point
!> @param p1 the second point
!> @returns @c bool the boolean result of the operation
!>
!> Function is elemental so it can be used on an array of points.
ELEMENTAL FUNCTION notequal_PointType(p0,p1) RESULT(bool)
  TYPE(PointType),INTENT(IN) :: p0,p1
  LOGICAL(SBK) :: bool
  bool=.TRUE.
  IF(p0%dim == p1%dim .AND. p0%dim > 0) &
      bool=ANY(p0%coord /= p1%coord)
ENDFUNCTION notequal_PointType
!
!-------------------------------------------------------------------------------
!> @brief Defines the 'approximately equal to' operation between two points
!> e.g. p0 .APPROXEQA. p1
!> @param p0 the first point
!> @param p1 the second point
!> @returns @c bool the boolean result of the operation
!>
!> Function is elemental so it can be used on an array of points.
ELEMENTAL FUNCTION approxequal_PointType(p0,p1) RESULT(bool)
  TYPE(PointType),INTENT(IN) :: p0,p1
  LOGICAL(SBK) :: bool
  bool=.FALSE.
  IF(p0%dim == p1%dim .AND. p0%dim > 0) &
      bool=ALL(p0%coord .APPROXEQA. p1%coord)
ENDFUNCTION approxequal_PointType
!
!-------------------------------------------------------------------------------
!> @brief Defines the assignment operation between two points
!> e.g. p0 = p1
!> @param p0 the first point to be assigned to
!> @param p1 the second point to be assigned from
!>
!> Function is elemental so it can be used on an array of points.
ELEMENTAL SUBROUTINE assign_PointType(p0,p1)
  TYPE(PointType),INTENT(INOUT) :: p0
  TYPE(PointType),INTENT(IN) :: p1
  p0%dim=p1%dim
  IF(ALLOCATED(p0%coord)) DEALLOCATE(p0%coord)
  IF(ALLOCATED(p1%coord)) THEN
    ALLOCATE(p0%coord(SIZE(p1%coord)))
    p0%coord=p1%coord
  ENDIF
ENDSUBROUTINE assign_PointType
!
!-------------------------------------------------------------------------------
!> @brief Return the coordinates as a string
!> @param p the point type whose coordinates are being returned as a string
!> @param fmt_string a string giving the format statement for the write
!> statement (OPTIONAL)
!> @returns @c cstring the returned string
!>
!> The default format is '(a,100(es23.12,a))' this means the maximum dimension
!> for the point is 100 for this routine. Should be changed to the value of
!> p%dim
ELEMENTAL FUNCTION coord2str_PointType(p,fmt_string) RESULT(cstring)
  CLASS(PointType),INTENT(IN) :: p
  CHARACTER(LEN=*),OPTIONAL,INTENT(IN) :: fmt_string
  CHARACTER(LEN=MAX_COORD_STR_LEN) :: cstring
  INTEGER(SIK) :: i,ioerr
  cstring=''
  IF(ALLOCATED(p%coord)) THEN
    IF(PRESENT(fmt_string)) THEN
      WRITE(cstring,FMT='(a,100('//fmt_string//',a))',IOSTAT=ioerr) &
          '(',(p%coord(i),',',i=1,p%dim-1),p%coord(p%dim),')'
    ELSE
      WRITE(cstring,'(a,100(es21.14,a))',IOSTAT=ioerr) &
          '(',(p%coord(i),',',i=1,p%dim-1),p%coord(p%dim),')'
    ENDIF
    IF(ioerr /= 0) cstring=''
  ENDIF
ENDFUNCTION coord2str_PointType
!
!-------------------------------------------------------------------------------
!> @brief Computes the distance between two points
!> @param p0 the first point
!> @param p1 the second point
!> @returns @c d the distance between @c p0 and @c p1
!> @note @c p0 and @c p1 must be defined in the same dimension or the
!>   result is 0.0.
!> The routine implicitly works for points in any dimension.
!>
!> Function is elemental so it can be used on an array of points.
ELEMENTAL FUNCTION distance_2points(p0,p1) RESULT(d)
  CLASS(PointType),INTENT(IN) :: p0,p1
  INTEGER(SIK) :: i
  REAL(SRK) :: d
  d=0.0_SRK
  IF(p0%dim == p1%dim) THEN
    !Explicit unrolling for lower order expected dimensions
    SELECTCASE(p0%dim)
    CASE(1)
      d=ABS((p1%coord(1)-p0%coord(1)))
    CASE(2)
      d=SQRT((p1%coord(1)-p0%coord(1))*(p1%coord(1)-p0%coord(1))+ &
          (p1%coord(2)-p0%coord(2))*(p1%coord(2)-p0%coord(2)))
    CASE(3)
      d=SQRT((p1%coord(1)-p0%coord(1))*(p1%coord(1)-p0%coord(1))+ &
          (p1%coord(2)-p0%coord(2))*(p1%coord(2)-p0%coord(2))+ &
          (p1%coord(3)-p0%coord(3))*(p1%coord(3)-p0%coord(3)))
    CASE DEFAULT
      DO i=1,p0%dim
        d=d+(p1%coord(i)-p0%coord(i))*(p1%coord(i)-p0%coord(i))
      ENDDO
      d=SQRT(d)
    ENDSELECT
  ENDIF
ENDFUNCTION distance_2points
!
!-------------------------------------------------------------------------------
!> @brief Computes the dot product of two points
!> @param p1 the first point
!> @param p2 the second point
!> @returns @c v the dot product of @c p1 and @c p2
!>
!> Function is elemental so it can be used on an array of points.
ELEMENTAL FUNCTION DOT_PRODUCT_2points(p1,p2) RESULT(v)
  CLASS(PointType),INTENT(IN) :: p1,p2
  REAL(SRK) :: v
  INTEGER(SIK) :: i
  IF(p1%dim == p2%dim) THEN
    SELECTCASE(p1%dim)
    CASE(1)
      v = p1%coord(1)*p2%coord(1)
    CASE(2)
      v = p1%coord(1)*p2%coord(1) + p1%coord(2)*p2%coord(2)
    CASE(3)
      v = p1%coord(1)*p2%coord(1) + p1%coord(2)*p1%coord(2) + p1%coord(3)*p1%coord(3)
    ENDSELECT
  ENDIF
ENDFUNCTION DOT_PRODUCT_2points
!
!-------------------------------------------------------------------------------
!> @brief Computes the cross product of two points
!> @param p1 the first point
!> @param p2 the second point
!> @returns @c p the cross product of @c p1 and @c p2
!>
!> Function is elemental so it can be used on an array of points.
ELEMENTAL FUNCTION cross_2points(p1,p2) RESULT(p)
  CLASS(PointType),INTENT(IN) :: p1,p2
  TYPE(PointType) :: p
  INTEGER(SIK) :: i
  IF(p1%dim == p2%dim) THEN
    SELECTCASE(p1%dim)
    CASE(2)
      CALL p%init(DIM=3, X=0.0_SRK, &
                         Y=0.0_SRK, & 
                         Z=p1%coord(1)*p2%coord(2) - p2%coord(1)*p1%coord(2))
    CASE(3)
      CALL p%init(DIM=3, X=p1%coord(2)*p2%coord(3) - p2%coord(2)*p1%coord(3), &
                         Y=p1%coord(3)*p2%coord(1) - p2%coord(3)*p1%coord(1), & 
                         Z=p1%coord(1)*p2%coord(2) - p2%coord(1)*p1%coord(2))
    ENDSELECT
  ENDIF
ENDFUNCTION cross_2points
!
!-------------------------------------------------------------------------------
!> @brief Computes the midpoint between two points in any dimension
!> @param p0 the first point
!> @param p1 the second point
!> @returns @c p the midpoint between @c p0 and @c p1
!>
!> Function is elemental so it can be used on an array of points.
ELEMENTAL FUNCTION midPoint_2points(p0,p1) RESULT(p)
  TYPE(PointType),INTENT(IN) :: p0,p1
  TYPE(PointType) :: p
  CALL p%clear()
  IF(p0%dim == p1%dim .AND. p0%dim > 0) THEN
    p%dim=p0%dim
    ALLOCATE(p%coord(p%dim))
    !Explicit unrolling for lower order expected dimensions
    SELECTCASE(p0%dim)
    CASE(1)
      p%coord(1)=(p1%coord(1)+p0%coord(1))*0.5_SRK
    CASE(2)
      p%coord(1)=(p1%coord(1)+p0%coord(1))*0.5_SRK
      p%coord(2)=(p1%coord(2)+p0%coord(2))*0.5_SRK
    CASE(3)
      p%coord(1)=(p1%coord(1)+p0%coord(1))*0.5_SRK
      p%coord(2)=(p1%coord(2)+p0%coord(2))*0.5_SRK
      p%coord(3)=(p1%coord(3)+p0%coord(3))*0.5_SRK
    CASE DEFAULT
      p%coord=(p1%coord+p0%coord)*0.5_SRK
    ENDSELECT
  ENDIF
ENDFUNCTION midPoint_2points
!
!-------------------------------------------------------------------------------
!> @brief Computes the interior angle between three points in up to three
!>        dimensions, where the points exist like:
!>   ^   * p0
!>    \   \
!>     v1  \
!>          \     v2->
!>        p1 *----------* p2
!> @param p0 the first point
!> @param p1 the second point
!> @param p2 the third point
!> @returns @c angle the angle between @c p0, @c p1, and @c p2
!>
!>  Equation implemented:
!> theta=ACOS(DOT(v1,v2)/(|v1|*|v2|))
!>
!> Function is elemental so it can be used on an array of points.
ELEMENTAL FUNCTION innerAngle_3points(p0,p1,p2) RESULT(angle)
  TYPE(PointType),INTENT(IN) :: p0,p1,p2
  REAL(SRK) :: vec1(3),vec2(3),angle,d1,d2
  IF((p0%dim > 1) .AND. (p0%dim == p1%dim) .AND. (p1%dim == p2%dim)) THEN
    !Explicit unrolling for lower order expected dimensions
    vec1=0.0_SRK; vec2=0.0_SRK
    d1=distance(p0,p1)
    d2=distance(p2,p1)
    SELECTCASE(p0%dim)
    CASE(2)
      vec1(1)=p0%coord(1)-p1%coord(1)
      vec1(2)=p0%coord(2)-p1%coord(2)
      vec2(1)=p2%coord(1)-p1%coord(1)
      vec2(2)=p2%coord(2)-p1%coord(2)
      angle=ACOS((vec1(1)*vec2(1)+vec1(2)*vec2(2))/(d1*d2))
    CASE(3)
      vec1(1)=p0%coord(1)-p1%coord(1)
      vec1(2)=p0%coord(2)-p1%coord(2)
      vec1(3)=p0%coord(3)-p1%coord(3)
      vec2(1)=p2%coord(1)-p1%coord(1)
      vec2(2)=p2%coord(2)-p1%coord(2)
      vec2(3)=p2%coord(3)-p1%coord(3)
      angle=ACOS((vec1(1)*vec2(1)+vec1(2)*vec2(2)+vec1(3)*vec2(3))/ &
          (d1*d2))
    ENDSELECT
  ENDIF
ENDFUNCTION innerAngle_3points
!
!-------------------------------------------------------------------------------
!> @brief Computes the exterior angle between three points in up to three
!>        dimensions, where the points exist like:
!>   ^   * p0
!>    \   \
!>     v1  \
!>          \     v2->
!>        p1 *----------* p2
!> @param p0 the first point
!> @param p1 the second point
!> @param p2 the third point
!> @returns @c angle the angle between @c p0, @c p1, and @c p2
!>
!> Function is elemental so it can be used on an array of points.
ELEMENTAL FUNCTION outerAngle_3points(p0,p1,p2) RESULT(angle)
  TYPE(PointType),INTENT(IN) :: p0,p1,p2
  REAL(SRK) :: vec1(3),vec2(3),angle,d1,d2
  IF((p0%dim > 1) .AND. (p0%dim == p1%dim) .AND. (p1%dim == p2%dim)) THEN
    !Explicit unrolling for lower order expected dimensions
    vec1=0.0_SRK; vec2=0.0_SRK
    d1=distance(p0,p1)
    d2=distance(p2,p1)
    SELECTCASE(p0%dim)
    CASE(2)
      vec1(1)=p0%coord(1)-p1%coord(1)
      vec1(2)=p0%coord(2)-p1%coord(2)
      vec2(1)=p2%coord(1)-p1%coord(1)
      vec2(2)=p2%coord(2)-p1%coord(2)
      angle=TWOPI-ACOS((vec1(1)*vec2(1)+vec1(2)*vec2(2))/(d1*d2))
    CASE(3)
      vec1(1)=p0%coord(1)-p1%coord(1)
      vec1(2)=p0%coord(2)-p1%coord(2)
      vec1(3)=p0%coord(3)-p1%coord(3)
      vec2(1)=p2%coord(1)-p1%coord(1)
      vec2(2)=p2%coord(2)-p1%coord(2)
      vec2(3)=p2%coord(3)-p1%coord(3)
      angle=TWOPI-ACOS((vec1(1)*vec2(1)+vec1(2)*vec2(2)+vec1(3)*vec2(3))/ &
          (d1*d2))
    ENDSELECT
  ENDIF
ENDFUNCTION outerAngle_3points
!
!-------------------------------------------------------------------------------
!> @brief Inserts a point into a linked list of points.
!> @param firstPoint the first point in the linked list
!> @param thisPoint the point to insert into the list
!>
!> This routine assumes that the point to be inserted will come after
!> firstPoint and before the last point in the list. If thisPoint
!> returns to the calling routine associated then it was NOT inserted
!> into the list successfully.
SUBROUTINE insert_LinkedListPointType(firstPoint,thisPoint,markDuplicates)
  CLASS(LinkedListPointType),TARGET,INTENT(INOUT) :: firstPoint
  TYPE(LinkedListPointType),POINTER,INTENT(INOUT) :: thisPoint
  LOGICAL(SBK),INTENT(IN),OPTIONAL :: markDuplicates

  LOGICAL(SBK) :: mark=.FALSE.
  LOGICAL(SBK) :: linsert
  REAL(SRK) :: d
  TYPE(LinkedListPointType),POINTER :: searchPoint1,searchPoint2

  IF(PRESENT(markDuplicates))THEN
    mark=markDuplicates
  ELSE
    mark=.FALSE.
  ENDIF

  IF(ASSOCIATED(thisPoint)) THEN
    SELECTTYPE(firstPoint); TYPE IS(LinkedListPointType)
      searchPoint1 => firstPoint
      d=thisPoint%sortval
      IF(ASSOCIATED(firstPoint%next)) THEN
        linsert=.TRUE.
        DO WHILE(linsert)
          searchPoint2 => searchPoint1%next

          !Do not allow for points that are approximately equal to
          !existing points in the list.
          !
          !Because the sort value is distance, "approximately equal to"
          !should be use SQRT(dim)*EPSREAL instead of EPSREAL for the
          !tolerance. We use a value of 2 since this caputes SQRT(3) and
          !we don't really expect higher dimensions.
          !
          !However, in practice this was determined to be too small of a
          !tolerance. It appears there are some cases where the approxeq
          !is satisfied for 2 of the 3 dimensions and because the surface
          !and line segment are nearly parallel the floating point error
          !in the 3rd dimension will be significantly larger.
          !
          !A new factor of 10 was arbitrarily chosen and may need to be
          !updated again if other degenerate cases are still encountered
          !to not be be correctly identified.
          IF(ABS(d-searchPoint1%sortval) <= 100._SRK*EPSREAL) THEN
            IF(mark) THEN
              searchPoint1%isDuplicate=.TRUE.
            ENDIF
            linsert=.FALSE.
          ELSEIF(ABS(d-searchPoint2%sortval) <= 100._SRK*EPSREAL) THEN
            IF(mark) THEN
              searchPoint2%isDuplicate=.TRUE.
            ENDIF
            linsert=.FALSE.
          ELSEIF(searchPoint1%sortval < d .AND. d < searchPoint2%sortval) THEN
            thisPoint%next => searchPoint2
            searchPoint1%next => thisPoint
            NULLIFY(thisPoint)
            linsert=.FALSE.
          ENDIF
          searchPoint1 => searchPoint2
          IF(.NOT.ASSOCIATED(searchPoint2%next)) linsert=.FALSE.
        ENDDO
      ENDIF
      NULLIFY(searchPoint1)
      NULLIFY(searchPoint2)
    ENDSELECT
  ENDIF
ENDSUBROUTINE insert_LinkedListPointType
!
!-------------------------------------------------------------------------------
!> @brief Clears a linked list of PointTypes.
!> @param firstPoint the first element in the linked list
!>
SUBROUTINE ClearLinkedListPointType(firstPoint)
  TYPE(LinkedListPointType),POINTER :: firstPoint
  TYPE(LinkedListPointType),POINTER :: thisPoint
  DO WHILE(ASSOCIATED(firstPoint))
    thisPoint => firstPoint
    CALL thisPoint%p%clear()
    firstPoint => thisPoint%next
    DEALLOCATE(thisPoint)
  ENDDO
ENDSUBROUTINE ClearLinkedListPointType
!
ENDMODULE Geom_Points
