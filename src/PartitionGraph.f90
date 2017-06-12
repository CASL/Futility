!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!> @brief Module defines the type and routines to partition a graph.
!>
!> @par Module Dependencies
!>  - @ref IntrType "IntrType": @copybrief IntrType
!>  - @ref ExceptionHandler : "ExceptionHandler" @copybrief ExceptionHandler
!>  - @ref ParameterLists : "ParameterLists" @copybrief ParameterLists
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE PartitionGraph
  USE IntrType
  USE Allocs
  USE Strings
  USE IO_Strings
  USE ExceptionHandler
  USE ParameterLists

  IMPLICIT NONE
  PRIVATE

  PUBLIC :: PartitionGraphType
  PUBLIC :: PartitionGraphType_Clear_Params
#ifdef UNIT_TEST
  PUBLIC :: ePartitionGraph
#endif

  !> Pointer to a partitioning algorithm. Type used to create arrays of
  !> procedures
  TYPE :: PartitionPtrArry
    PROCEDURE(partition_absintfc),POINTER,NOPASS :: p => NULL()
  ENDTYPE

  !> @brief Derived type to spatially decompose a reactor
  !>
  !> Graph structure to decompose a reactor spatially. Retains spatial
  !> coordinate information. Each vertex, and edge may have an associated
  !> weight, which will be accounted for in the partitioning.
  !>
  !> Assumptions:
  !> 1. Structured Graph/Core - connectivity is reasonably low
  !>
  TYPE :: PartitionGraphType
    !> The number of vertices in the graph (core)
    INTEGER(SIK) :: nvert=0
    !> Dimensions (2 or 3)
    INTEGER(SIK) :: dim=0
    !> The maximum number of neighbors
    INTEGER(SIK) :: maxneigh=0
    !> Number of groups to decompose into
    INTEGER(SIK) :: nGroups=0
    !> Number of partitioning methods
    INTEGER(SIK) :: nPart=0
    !> Starting index of each group. Size [nGroup+1]
    INTEGER(SIK),ALLOCATABLE :: groupIdx(:)
    !> Vertex indices belonging to each group.
    !> Group separation given by groupIdx. Size [nvert]
    INTEGER(SIK),ALLOCATABLE :: groupList(:)
    !> The computational weight associated with each vertex. Size [nvert]
    INTEGER(SIK),ALLOCATABLE :: wts(:)
    !> Max. size for each partitioning algorithm to be used. Size [nPart-1]
    !> The first algorithm will have no size condition
    INTEGER(SIK),ALLOCATABLE :: cond(:)
    !> Neighbor matrix. Size [maxneigh, nvert]
    !> Vertex index if neighbor exists, otherwise 0
    INTEGER(SIK),ALLOCATABLE :: neigh(:,:)
    !> Communication weights. Size [maxneigh, nvert]
    INTEGER(SIK),ALLOCATABLE :: neighwts(:,:)
    !> Coordinates. Ordered x,y,z(if it exists). Size [dim, nvert]
    REAL(SRK),ALLOCATABLE :: coord(:,:)
    !> Array containing pointers to partitioning routines
    TYPE(PartitionPtrArry),ALLOCATABLE :: partitionAlgArry(:)
  CONTAINS
    !> @copybrief PartitionGraph::init_PartitionGraph
    !> @copydetails PartitionGraph::init_PartitionGraph
    PROCEDURE,PASS :: initialize => init_PartitionGraph
    !> @copybrief PartitionGraph::clear_PartitionGraph
    !> @copydetails PartitionGraph::clear_PartitionGraph
    PROCEDURE,PASS :: clear => clear_PartitionGraph
  ENDTYPE PartitionGraphType

  !> Abstract interface for pointer to partitioning routine
  ABSTRACT INTERFACE
    SUBROUTINE partition_absintfc(thisGraph)
      IMPORT :: PartitionGraphType,SRK,SIK
      CLASS(PartitionGraphType),INTENT(INOUT) :: thisGraph
    ENDSUBROUTINE partition_absintfc
  ENDINTERFACE

  !> Name of the module
  CHARACTER(LEN=*),PARAMETER :: modName='PartitionGraph'

  !> The module exception handler
  TYPE(ExceptionHandlerType),SAVE :: ePartitionGraph

  !> Logical flag to check whether the required and optional parameter lists
  !> have been created yet for the PartitionGraphType
  LOGICAL(SBK),SAVE :: PartitionGraphType_flagParams=.FALSE.

  !> The parameter list to use when validating a parameter list for
  !> initialization of the PartitionGraphType
  TYPE(ParamType),SAVE :: PartitionGraphType_reqParams
!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief Intialization routine for a PartitionGraphType
!>
   SUBROUTINE init_PartitionGraph(thisGraph,params)
      CHARACTER(LEN=*),PARAMETER :: myName='init_PartitionGraph'
      CLASS(PartitionGraphType),INTENT(INOUT) :: thisGraph
      TYPE(ParamType),INTENT(IN) :: params
      INTEGER(SIK) :: nerror,nvert,maxneigh,nGroups,nPart,dim
      INTEGER(SIK) :: ipart,pcond
      TYPE(ParamType) :: validParams
      TYPE(StringType) :: algName
      INTEGER(SIK),ALLOCATABLE :: wts(:),neigh(:,:),neighwts(:,:), cond(:)
      REAL(SRK),ALLOCATABLE :: coord(:,:)
      TYPE(StringType),ALLOCATABLE :: partAlgs(:)

      !Clear the type in case it has already been intialized
      CALL thisGraph%clear()

      !Check to set up required and optional param lists
      IF(.NOT. PartitionGraphType_flagParams) &
        CALL PartitionGraphType_Declare_Params()

      validParams=params
      CALL validParams%validate(PartitionGraphType_reqParams)

      !Error checking for subroutine input
      nerror=ePartitionGraph%getCounter(EXCEPTION_ERROR)

      !Pull Required data from parameter list
      CALL validParams%get('PartitionGraph -> nvert', nvert)
      CALL validParams%get('PartitionGraph -> nGroups', nGroups)
      CALL validParams%get('PartitionGraph -> neigh', neigh)
      CALL validParams%get('PartitionGraph -> coord', coord)
      CALL validParams%get('PartitionGraph -> Algorithms', partAlgs)
      CALL validParams%clear()

      !Check input number of groups to partition graph into
      IF(nGroups < 1) CALL ePartitionGraph%raiseError(modName//'::'//myName// &
        ' - invalid number of partitioning groups!')

      !Check input number of vertices
      IF(nvert >= 1) THEN
        !Check input coordinate matrix
        dim=SIZE(coord,DIM=1)
        IF(SIZE(coord,DIM=2) /= nvert) CALL ePartitionGraph%raiseError(modName// &
          '::'//myName//' - coordinate matrix is incorrect size!')

        !Check neighbor matrix
        maxneigh=SIZE(neigh,DIM=1)
        IF(SIZE(neigh,DIM=2) == nvert) THEN
          IF(ANY(neigh < 0) .OR. ANY(neigh > nvert)) THEN
            CALL ePartitionGraph%raiseError(modName//'::'//myName// &
              ' - invalid neighbor matrix!')
          ENDIF

          !Check edge weights
          IF(params%has('PartitionGraph -> neighwts')) THEN
            CALL params%get('PartitionGraph -> neighwts',neighwts)

            !Check it is the correct size
            IF((SIZE(neighwts, DIM=1) == maxneigh) .AND. &
               (SIZE(neighwts,DIM=2) == nvert)) THEN
              !Check weights are valid (>0)
              IF(ANY(neighwts < 0)) THEN
                CALL ePartitionGraph%raiseError(modName//'::'//myName// &
                  ' - edge weights must be > 0!')
              ENDIF
            ELSE
              CALL ePartitionGraph%raiseError(modName//'::'//myName// &
                ' - input edge weights matrix is incorrect size!')
            ENDIF
          ELSE
            !If no communication weights are provided, all are assumed to be 1
            ALLOCATE(neighwts(maxneigh,nvert))
            neighwts=1
          ENDIF
        ELSE
          CALL ePartitionGraph%raiseError(modName//'::'//myName// &
            ' - neighbor matrix is incorrect size!')
        ENDIF

        !Check vertex weights
        IF(params%has('PartitionGraph -> wts')) THEN
          CALL params%get('PartitionGraph -> wts',wts)

          !Check it is the correct size
          IF(SIZE(wts) == nvert) THEN
            !Check weights are valid (>0)
            IF(ANY(wts < 0)) THEN
              CALL ePartitionGraph%raiseError(modName//'::'//myName// &
                ' - vertex weights must be > 0!')
            ENDIF
          ELSE
            CALL ePartitionGraph%raiseError(modName//'::'//myName// &
              ' - input vertex weights array is incorrect size!')
          ENDIF
        ELSE
          !If no weights provided, all are assumed to be 1
          ALLOCATE(wts(nvert))
          wts=1
        ENDIF
      ELSE
        CALL ePartitionGraph%raiseError(modName//'::'//myName// &
        ' - invalid number of vertices!')
      ENDIF

      !Check partitioning algorithms/conditions
      nPart=SIZE(partAlgs)
      IF(nPart > 1) THEN
        !Check conditions
        IF(params%has('PartitionGraph->Conditions')) THEN
          CALL params%get('PartitionGraph->Conditions',cond)

          IF(SIZE(cond) == nPart-1) THEN
            !Check all conditions are valid (positive integers)
            IF(ALL(cond > 0)) THEN
              !Check that the conditions do not conflict
              DO ipart=2,nPart-1
                pcond=cond(ipart-1)
                IF(pcond <= cond(ipart)) THEN
                  CALL ePartitionGraph%raiseError(modName//'::'//myName// &
                    ' - partitioning algorithm size conditions are invalid!')
                ENDIF
              ENDDO !ipart
            ELSE
              CALL ePartitionGraph%raiseError(modName//'::'//myName// &
                ' - partitioning algorithm size conditions are invalid (< 0)!')
            ENDIF
          ELSE
            CALL ePartitionGraph%raiseError(modName//'::'//myName// &
              ' - Wrong number of conditions specified!')
          ENDIF
        ELSE
          CALL ePartitionGraph%raiseError(modName//'::'//myName// &
            ' - Conditions must be specified if more than 1 algorithm is to be used!')
        ENDIF
      ENDIF

      !If no errors, then initialize the type
      IF(nerror == ePartitionGraph%getCounter(EXCEPTION_ERROR)) THEN
        !Set scalars
        thisGraph%nvert=nvert
        thisGraph%dim=dim
        thisGraph%maxneigh=maxneigh
        thisGraph%nGroups=nGroups
        thisGraph%nPart=nPart

        !Move allocated data onto the type
        CALL MOVE_ALLOC(neigh,thisGraph%neigh)
        CALL MOVE_ALLOC(coord,thisGraph%coord)
        CALL MOVE_ALLOC(wts,thisGraph%wts)
        CALL MOVE_ALLOC(neighwts,thisGraph%neighwts)
        IF(nPart > 1) THEN
          CALL MOVE_ALLOC(cond,thisGraph%cond)
        ENDIF

        !Assign procedure pointers
        ALLOCATE(thisGraph%partitionAlgArry(nPart))
        DO ipart=1,SIZE(partAlgs)
          thisGraph%partitionAlgArry(ipart)%p => NULL()
          algName=partAlgs(ipart)
          CALL toUPPER(algName)
          SELECTCASE(TRIM(algName))
            CASE('Recursive Spectral Bisection')
            CASE('Recursive Expansion Bisection')
            CASE DEFAULT
              CALL ePartitionGraph%raiseError(modName//'::'//myName// &
                ' - Partitioning algorithm "'//TRIM(algName)//'" not recognized!')
          ENDSELECT
        ENDDO !ipart
      ENDIF
    ENDSUBROUTINE init_PartitionGraph
!
!-------------------------------------------------------------------------------
!> @brief Clear routine for a PartitionGraphType
!>
    SUBROUTINE clear_PartitionGraph(thisGraph)
      CLASS(PartitionGraphType),INTENT(INOUT) :: thisGraph
      INTEGER(SIK) :: ipart

      thisGraph%nvert=0
      thisGraph%dim=0
      thisGraph%maxneigh=0
      thisGraph%nGroups=0
      thisGraph%nPart=0
      IF(ALLOCATED(thisGraph%groupIdx)) DEALLOCATE(thisGraph%groupIdx)
      IF(ALLOCATED(thisGraph%groupList)) DEALLOCATE(thisGraph%groupList)
      IF(ALLOCATED(thisGraph%wts)) DEALLOCATE(thisGraph%wts)
      IF(ALLOCATED(thisGraph%cond)) DEALLOCATE(thisGraph%cond)
      IF(ALLOCATED(thisGraph%neigh)) DEALLOCATE(thisGraph%neigh)
      IF(ALLOCATED(thisGraph%neighwts)) DEALLOCATE(thisGraph%neighwts)
      IF(ALLOCATED(thisGraph%coord)) DEALLOCATE(thisGraph%coord)

      !Nullify each procedure pointer
      IF(ALLOCATED(thisGraph%partitionAlgArry)) THEN
        DO ipart=1,SIZE(thisGraph%partitionAlgArry)
          thisGraph%partitionAlgArry(ipart)%p => NULL()
        ENDDO !ipart
        DEALLOCATE(thisGraph%partitionAlgArry)
      ENDIF
    ENDSUBROUTINE clear_PartitionGraph
!
!-------------------------------------------------------------------------------
!> @brief Declares the required and optional parameters for initialization of
!> the PartitionGraphType
!>
    SUBROUTINE PartitionGraphType_Declare_Params()
      INTEGER(SIK) :: neigh(1,1)
      REAL(SRK) :: coord(1,1)
      TYPE(StringType) :: algNames(1)

      !Required parameters
      neigh=0
      coord=0.0_SRK
      algNames(1)=''
      CALL PartitionGraphType_reqParams%add('PartitionGraph -> nvert',0)
      CALL PartitionGraphType_reqParams%add('PartitionGraph -> nGroups',0)
      CALL PartitionGraphType_reqParams%add('PartitionGraph -> coord',coord)
      CALL PartitionGraphType_reqParams%add('PartitionGraph -> neigh',neigh)
      CALL PartitionGraphType_reqParams%add('PartitionGraph -> Algorithms', &
        algNames)

      !Set flag to true since the parameters have been set for this type.
      PartitionGraphType_flagParams=.TRUE.
    ENDSUBROUTINE PartitionGraphType_Declare_Params
!
!-------------------------------------------------------------------------------
!> @brief Clears the required parameter lists for the PartitionGraphType
!>
    SUBROUTINE PartitionGraphType_Clear_Params()
      !Reset flag to false
      PartitionGraphType_flagParams=.FALSE.
      !Clear parameter list
      CALL PartitionGraphType_reqParams%clear()
    ENDSUBROUTINE PartitionGraphType_Clear_Params
!
ENDMODULE PartitionGraph
