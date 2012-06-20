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
!> @brief A Fortran 2003 module defining "Points" for use in geometry.
!>
!> This module provides derived data types for a "point" in any dimensional
!> space. It also provides methods for constructing and destruction the points
!> and printing the point information. Two other routines are provided to return
!> the distance between two points and to return the midpoint between two points.
!>
!> @par Module Dependencies
!>  - @ref IntrType "IntrType": @copybrief IntrType
!>
!> @author Brendan Kochunas
!>    @date 5/26/2011
!>
!> @par Revisions:
!> (10/25/2011) - Brendan Kochunas
!>   - Changed the point derived type to use arrays instead of records. This
!>     allows for extension past 3-D to N-D. It also eliminates the need to use
!>     polymorphic types, so it requires less Fortran 2003 features.
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE Geom_Points

  USE IntrType
  IMPLICIT NONE
  PRIVATE !Default contents of module to private
!
! List of Public items
  PUBLIC :: MAX_COORD_STR_LEN
  PUBLIC :: PointType
  PUBLIC :: LinkedListPointType
  PUBLIC :: ClearLinkedListPointType
  PUBLIC :: Distance
  PUBLIC :: midPoint
  PUBLIC :: OPERATOR(+)
  PUBLIC :: OPERATOR(-)
  PUBLIC :: OPERATOR(==)
  PUBLIC :: OPERATOR(/=)
  PUBLIC :: OPERATOR(.APPROXEQ.)
  
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
    REAL(SRK) :: sortval
    !> The point type for this record in the list
    TYPE(PointType):: p
    !> The next record in the list.
    TYPE(LinkedListPointType),POINTER :: next => NULL()
!
!List of type bound procedures
    CONTAINS
      !> @copybrief GeomPoints::insert_LinkedListPointType
      !> @copydetails GeomPoints::insert_LinkedListPointType
      PROCEDURE,PASS :: insert => insert_LinkedListPointType
  ENDTYPE LinkedListPointType
  
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
  !> (.APPROXEQ.)
  !>
  !> Adds 'is approximately equal to' capability for point types
  INTERFACE OPERATOR(.APPROXEQ.)
    !> @copybrief GeomPoints::approxequal_PointType
    !> @copydetails GeomPoints::approxequal_PointType
    MODULE PROCEDURE approxequal_PointType
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
!> CALL p%initialize(COORD=(/0.0,0.0,0.0,0.0/))
!> @endcode
!>
!> @code
!> TYPE(PointType) :: p
!> CALL p%initialize(DIM=1,X=0.0)
!> @endcode
!>
!> @code
!> TYPE(PointType) :: p
!> CALL p%initialize(DIM=2,X=0.0,Y=0.0)
!> @endcode
!>
!> @code
!> TYPE(PointType) :: p
!> CALL p%initialize(DIM=3,X=0.0,Y=0.0,Z=0.0)
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
!> e.g. p0.APPROXEQ.p1
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
        bool=ALL(p0%coord .APPROXEQ. p1%coord)
    ENDFUNCTION approxequal_PointType
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
!> @brief Inserts a point into a linked list of points.
!> @param firstPoint the first point in the linked list
!> @param thisPoint the point to insert into the list
!>
!> This routine assumes that the point to be inserted will come after
!> firstPoint and before the last point in the list. If thisPoint
!> returns to the calling routine associated then it was NOT inserted
!> into the list successfully.
    SUBROUTINE insert_LinkedListPointType(firstPoint,thisPoint)
      CLASS(LinkedListPointType),TARGET,INTENT(INOUT) :: firstPoint
      TYPE(LinkedListPointType),POINTER,INTENT(INOUT) :: thisPoint
      
      LOGICAL(SBK) :: linsert
      REAL(SRK) :: d
      TYPE(LinkedListPointType),POINTER :: searchPoint1,searchPoint2
      
      IF(ASSOCIATED(thisPoint)) THEN
        SELECTTYPE(firstPoint); TYPE IS(LinkedListPointType)
          searchPoint1 => firstPoint
          d=thisPoint%sortval
          IF(ASSOCIATED(firstPoint%next)) THEN
            linsert=.TRUE.
            DO WHILE(linsert)
              searchPoint2 => searchPoint1%next
              
              !IF((d .APPROXEQ. searchPoint1%sortval) .OR. &
              !  (d .APPROXEQ. searchPoint2%sortval)) THEN
              IF((ABS(d-searchPoint1%sortval) <= 2._SRK*EPSREAL) .OR. &
                 (ABS(d-searchPoint2%sortval) <= 2._SRK*EPSREAL)) THEN
                !Do not allow for points that are approximately equal to existing
                !points in the list.
                !
                !Because the sort value is distance, "approximately equal to"
                !should be use SQRT(dim)*EPSREAL instead of EPSREAL for the
                !tolerance. We use a value of 2 since this caputes SQRT(3) and
                !we don't really expect higher dimensions.
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
