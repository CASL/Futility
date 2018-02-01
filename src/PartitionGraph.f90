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
!> Provides a type with interfaces to partition a graph. The graph may be have
!> coordinates, if it is associated with a mesh. Each vertex and edge may be
!> weighted, which will be considered during partitioning and refinement.
!>
!> The PartitionGraphType is initialized using a parameter list. The partition
!> groups may be determined using either:
!>  - PartitionGraphType%partition() !Automated
!>  - PartitionGraphType%setGroups() !Manual
!> Some metrics for determining partition quality may be found using:
!>  - PartitionGraphType%metrics()
!>
!> @par Module Dependencies
!>  - @ref IntrType "IntrType": @copybrief IntrType
!>  - @ref Allocs "Allocs": @copybrief Allocs
!>  - @ref Strings "Strings": @copybrief Strings
!>  - @ref IO_Strings "IO_Strings": @copybrief IO_Strings
!>  - @ref ExceptionHandler "ExceptionHandler": @copybrief ExceptionHandler
!>  - @ref ParameterLists "ParameterLists": @copybrief ParameterLists
!>  - @ref ParallelEnv "ParallelEnv": @copybrief ParallelEnv
!>  - @ref VectorTypes "VectorTypes": @copybrief VectorTypes
!>  - @ref MatrixTypes "MatrixTypes": @copybrief MatrixTypes
!>  - @ref Sorting "Sorting": @copybrief Sorting
!>
!> @author Andrew Fitzgerald
!>   @date 08/08/2017
!>
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE PartitionGraph
  USE IntrType
  USE Allocs
  USE Strings
  USE IO_Strings
  USE ExceptionHandler
  USE ParameterLists
  USE ParallelEnv
  USE VectorTypes
  USE MatrixTypes
  USE Sorting

  IMPLICIT NONE

#ifdef FUTILITY_HAVE_PETSC
#include <petscversion.h>
#if ((PETSC_VERSION_MAJOR>=3) && (PETSC_VERSION_MINOR>=6))
#include <petsc/finclude/petsc.h>
#else
#include <finclude/petsc.h>
#endif
!petscisdef.h defines the keyword IS, and it needs to be reset
#ifdef FUTILITY_HAVE_SLEPC
#include <finclude/slepcsys.h>
#include <finclude/slepceps.h>
#endif
#undef IS
#endif

  PRIVATE

  PUBLIC :: PartitionGraphType
#ifdef UNIT_TEST
  PUBLIC :: ePartitionGraph
#endif

  !> Pointer to a partitioning algorithm. Type used to create arrays of
  !> procedures
  TYPE :: PartitionPtrArry
    PROCEDURE(partition_absintfc),POINTER,NOPASS :: p => NULL()
  ENDTYPE

  !> Pointer to a refinement algorithm. Type used to create arrays of
  !> procedures
  TYPE :: RefinePtrArry
    PROCEDURE(refine_absintfc),POINTER,NOPASS :: r => NULL()
  ENDTYPE

  !> @brief Derived type to partition a graph
  !>
  !> Graph structure used to partition a graph. The graph may have associated
  !> spatial coordinates. The vertices and edges may also be weighted, which is
  !> considered during partitioning.
  !>
  TYPE :: PartitionGraphType
    !> Logical if the type is already initialized
    LOGICAL(SBK) :: isInit=.FALSE.
    !> The number of vertices in the graph
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
    !> Max. size for each partitioning algorithm to be used. Size [nPart-1]
    !> The first algorithm will have no size condition
    INTEGER(SIK),ALLOCATABLE :: cond(:)
    !> Degree array. Number of edges from each vertex. Size [nvert]
    INTEGER(SIK),ALLOCATABLE :: d(:)
    !> The computational weight associated with each vertex. Size [nvert]
    REAL(SRK),ALLOCATABLE :: wts(:)
    !> Neighbor matrix. Size [maxneigh, nvert]
    !> Vertex index if neighbor exists, otherwise 0
    INTEGER(SIK),ALLOCATABLE :: neigh(:,:)
    !> Communication weights. Size [maxneigh, nvert]
    REAL(SRK),ALLOCATABLE :: neighwts(:,:)
    !> Coordinates. Ordered x,y,z(if it exists). Size [dim, nvert]
    REAL(SRK),ALLOCATABLE :: coord(:,:)
    !> Array containing pointers to partitioning routines
    TYPE(PartitionPtrArry),ALLOCATABLE :: partitionAlgArry(:)
    !> Array containing pointers to refinement routines
    TYPE(RefinePtrArry),ALLOCATABLE :: refineAlgArry(:)
  CONTAINS
    !> @copybrief PartitionGraph::init_PartitionGraph
    !> @copydetails PartitionGraph::init_PartitionGraph
    PROCEDURE,PASS :: initialize => init_PartitionGraph
    !> @copybrief PartitionGraph::clear_PartitionGraph
    !> @copydetails PartitionGraph::clear_PartitionGraph
    PROCEDURE,PASS :: clear => clear_PartitionGraph
    !> @copybrief PartitionGraph::setGroups_PartitionGraph
    !> @copydetails PartitionGraph::setGroups_PartitionGraph
    PROCEDURE,PASS :: setGroups => setGroups_PartitionGraph
    !> @copybrief PartitionGraph::GenerateSubgraph
    !> @copydetails PartitionGraph::GenerateSubgraph
    PROCEDURE,PASS :: subgraph => GenerateSubgraph_PartitionGraph
    !> @copybrief PartitionGraph::partition_PartitionGraph
    !> @copydetails PartitionGraph::partition_PartitionGraph
    PROCEDURE,PASS :: partition => partition_PartitionGraph
    !> @copybrief PartitionGraph::refine_PartitionGraph
    !> @copydetails PartitionGraph::refine_PartitionGraph
    PROCEDURE,PASS :: refine => refine_PartitionGraph
    !> @copybrief PartitionGraph::calcDecompMetrics_PartitionGraph
    !> @copydetails PartitionGraph::calcDecompMetrics_PartitionGraph
    PROCEDURE,PASS :: metrics => calcDecompMetrics_PartitionGraph
  ENDTYPE PartitionGraphType

  !> Abstract interface for pointer to partitioning routine
  ABSTRACT INTERFACE
    SUBROUTINE partition_absintfc(thisGraph)
      IMPORT :: PartitionGraphType
      CLASS(PartitionGraphType),INTENT(INOUT) :: thisGraph
    ENDSUBROUTINE partition_absintfc
  ENDINTERFACE

  !> Abstract interface for pointer to refinement routine
  ABSTRACT INTERFACE
    SUBROUTINE refine_absintfc(thisGraph,L1,L2)
      IMPORT :: PartitionGraphType,SIK
      CLASS(PartitionGraphType),INTENT(IN) :: thisGraph
      INTEGER(SIK),INTENT(INOUT) :: L1(:),L2(:)
    ENDSUBROUTINE refine_absintfc
  ENDINTERFACE

  !> Name of the module
  CHARACTER(LEN=*),PARAMETER :: modName='PartitionGraph'

  !> The module exception handler
  TYPE(ExceptionHandlerType),SAVE :: ePartitionGraph
!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief Intialization routine for a PartitionGraphType
!> @param thisGraph the graph to be initialized
!> @param params the parameter list describing the graph
!>
!> The graph is defined from a parameter list containing 'PartitionGraph->':
!>  Required:
!>    - nvert      (INTEGER)
!>        Number of vertices in the graph
!>    - nGroups    (INTEGER)
!>        Number of groups to partition the graph into
!>    - neigh      (2D INTEGER)[max neigh, nvert]
!>        Neighbor matrix. Each neigh(:,ivert) contains indices of vertices
!>          which have edges with the vertex 'ivert'
!>    - Algorithms (1D StringType)
!>        List of partitioning algorithms to use. Options are:
!>          a. 'Recursive Spectral Bisection'
!>          b. 'Recursive Inertial Bisection'
!>          c. 'Recursive Expansion Bisection'
!>          d. 'None'   - Should only be used when %setGroups is used
!>  Optional:
!>    - Coord      (2D REAL)[dim, nvert]
!>        Spatial coordinates for each vertex
!>    - Refinement (1D StringType)
!>        Refinement algorithms to use during partitioning. Options are:
!>          a. 'Kernighan-Lin'
!>          b. 'Spatial Kernighan-Lin'
!>          c. 'None' / ''
!>        If only 1 is specified it will be used for all (sub)graph sizes
!>    - Conditions (1D INTEGER)
!>        List of minimum size(nvert) for each partitioning algorithm.
!>    - wts        (1D REAL)
!>        Weight of each vertex.
!>    - neighwts   (2D REAL)
!>        Weight of each edge. Should be same shape as neigh
!>
   SUBROUTINE init_PartitionGraph(thisGraph,params)
      CHARACTER(LEN=*),PARAMETER :: myName='init_PartitionGraph'
      CLASS(PartitionGraphType),INTENT(INOUT) :: thisGraph
      TYPE(ParamType),INTENT(IN) :: params
      INTEGER(SIK) :: nerror,nvert,maxneigh,nGroups,nPart,dim
      INTEGER(SIK) :: ipart,pcond,iv
      TYPE(StringType) :: algName
      INTEGER(SIK),ALLOCATABLE :: neigh(:,:), cond(:)
      REAL(SRK),ALLOCATABLE :: wts(:),neighwts(:,:),coord(:,:)
      TYPE(StringType),ALLOCATABLE :: partAlgs(:),refAlgNames(:)

      !Error checking for initialization input
      nerror=ePartitionGraph%getCounter(EXCEPTION_ERROR)
      IF(.NOT. thisGraph%isInit) THEN
        !Check number of vertices
        IF(params%has('PartitionGraph -> nvert')) THEN
          CALL params%get('PartitionGraph -> nvert', nvert)
          !Must have atleast 1 vertex
          IF(nvert < 1) THEN
            CALL ePartitionGraph%raiseError(modName//'::'//myName// &
            ' - invalid number of vertices!')
            RETURN
          ENDIF
        ELSE
          CALL ePartitionGraph%raiseError(modName//'::'//myName// &
            ' - nvert is required for PartitionGraph initialization!')
          RETURN
        ENDIF

        !Check number of groups to partition graph into
        IF(params%has('PartitionGraph -> nGroups')) THEN
          CALL params%get('PartitionGraph -> nGroups', nGroups)
          !Must partition into at least 1 group
          IF(nGroups < 1 .OR. nGroups > nvert) &
            CALL ePartitionGraph%raiseError(modName//'::'//myName// &
              ' - invalid number of partitioning groups!')
        ELSE
          CALL ePartitionGraph%raiseError(modName//'::'//myName// &
            ' - nGroups is required for PartitionGraph initialization!')
        ENDIF

        !Check neighbor matrix
        IF(params%has('PartitionGraph -> neigh')) THEN
          CALL params%get('PartitionGraph -> neigh', neigh)
        ELSE
          CALL ePartitionGraph%raiseError(modName//'::'//myName// &
            ' - neigh is required for PartitionGraph initialization!')
        ENDIF

        !Check partitioning algorithm list
        nPart=0
        IF(params%has('PartitionGraph -> Algorithms')) THEN
          CALL params%get('PartitionGraph -> Algorithms', partAlgs)
          nPart=SIZE(partAlgs)
        ELSE
          CALL ePartitionGraph%raiseError(modName//'::'//myName// &
            ' - Algorithms is required for PartitionGraph initialization!')
        ENDIF

        !If more than 1 algorithm there must be an input condition list
        IF(nPart > 1) THEN
          IF(params%has('PartitionGraph -> Conditions')) THEN
            CALL params%get('PartitionGraph -> Conditions', cond)
          ELSE
            CALL ePartitionGraph%raiseError(modName//'::'//myName// &
              ' - Conditions must be specified if more than 1 algorithm is'// &
              ' to be used!')
          ENDIF
        ENDIF
      ELSE
        CALL ePartitionGraph%raiseError(modName//'::'//myName// &
          ' - partition graph is already initialized!')
      ENDIF

      !If no errors then check validity of input and optional inputs
      IF(nerror == ePartitionGraph%getCounter(EXCEPTION_ERROR)) THEN
        !Check neighbor matrix
        maxneigh=SIZE(neigh,DIM=1)
        IF(SIZE(neigh,DIM=2) == nvert) THEN
          IF(ANY(neigh < 0) .OR. ANY(neigh > nvert)) THEN
            CALL ePartitionGraph%raiseError(modName//'::'//myName// &
              ' - invalid neighbor matrix!')
          ENDIF
        ELSE
          CALL ePartitionGraph%raiseError(modName//'::'//myName// &
            ' - neighbor matrix is incorrect size!')
        ENDIF

        !Check the condition list
        IF(nPart > 1) THEN
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
        ENDIF

        !Check refinement algorithm
        IF(params%has('PartitionGraph -> Refinement')) THEN
          CALL params%get('PartitionGraph -> Refinement', refAlgNames)
          IF((SIZE(refAlgNames) /= 1) .AND. (SIZE(refAlgNames) /= nPart)) THEN
            CALL ePartitionGraph%raiseError(modName//'::'//myName// &
              ' - invalid number of refinement algorithms specified!')
          ENDIF
        ENDIF

        !Check vertex weights (optional)
        IF(params%has('PartitionGraph -> wts')) THEN
          CALL params%get('PartitionGraph -> wts', wts)
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
          ALLOCATE(wts(nvert))
          wts=1.0_SRK
        ENDIF

        !Check neighbor weights
        IF(params%has('PartitionGraph -> neighwts')) THEN
          CALL params%get('PartitionGraph -> neighwts', neighwts)
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
          ALLOCATE(neighwts(maxneigh, nvert))
          neighwts=0.0_SRK
          WHERE(neigh /= 0.0_SRK) neighwts=1.0_SRK
        ENDIF

        !Check coordinates (optional)
        dim=0
        IF(params%has('PartitionGraph -> coord')) THEN
          CALL params%get('PartitionGraph -> coord', coord)
          !Check input coordinate matrix
          dim=SIZE(coord,DIM=1)
          IF(SIZE(coord,DIM=2) /= nvert) THEN
            CALL ePartitionGraph%raiseError(modName//'::'//myName// &
              ' - coordinate matrix is incorrect size!')
          ENDIF
        ENDIF
      ENDIF

      !If no errors then initialize the type
      IF(nerror == ePartitionGraph%getCounter(EXCEPTION_ERROR)) THEN
        !Set scalars
        thisGraph%isInit=.TRUE.
        thisGraph%nvert=nvert
        thisGraph%dim=dim
        thisGraph%maxneigh=maxneigh
        thisGraph%nGroups=nGroups
        thisGraph%nPart=nPart

        !Move allocated data onto the type
        CALL MOVE_ALLOC(neigh,thisGraph%neigh)
        CALL MOVE_ALLOC(wts,thisGraph%wts)
        CALL MOVE_ALLOC(neighwts,thisGraph%neighwts)
        IF(nPart > 1) THEN
          CALL MOVE_ALLOC(cond,thisGraph%cond)
        ENDIF
        IF(ALLOCATED(coord)) CALL MOVE_ALLOC(coord,thisGraph%coord)

        !Assign procedure pointers for partitioning
        ALLOCATE(thisGraph%partitionAlgArry(nPart))
        DO ipart=1,SIZE(partAlgs)
          thisGraph%partitionAlgArry(ipart)%p => NULL()
          algName=partAlgs(ipart)
          CALL toUPPER(algName)
          SELECTCASE(TRIM(algName))
            CASE('NONE')
            CASE('RECURSIVE EXPANSION BISECTION')
              thisGraph%partitionAlgArry(ipart)%p => RecursiveExpansionBisection
#ifdef FUTILITY_HAVE_SLEPC
            CASE('RECURSIVE SPECTRAL BISECTION')
              thisGraph%partitionAlgArry(ipart)%p => RecursiveSpectralBisection
            CASE('RECURSIVE INERTIAL BISECTION')
              thisGraph%partitionAlgArry(ipart)%p => RecursiveInertialBisection
#else
            CASE('RECURSIVE SPECTRAL BISECTION')
              CALL ePartitionGraph%raiseError(modName//'::'//myName// &
                ' - must recompile with SLEPc to use '//TRIM(ADJUSTL(algName)))
            CASE('RECURSIVE INERTIAL BISECTION')
              CALL ePartitionGraph%raiseError(modName//'::'//myName// &
                ' - must recompile with SLEPc to use '//TRIM(ADJUSTL(algName)))
#endif
            CASE DEFAULT
              CALL ePartitionGraph%raiseError(modName//'::'//myName// &
                ' - Partitioning algorithm "'//TRIM(algName)//'" not recognized!')
          ENDSELECT
        ENDDO !ipart

        !Assign procedure pointers for refinement
        ALLOCATE(thisGraph%refineAlgArry(nPart))
        ipart=1
        algName=''
        DO WHILE(ipart <= npart)
          thisGraph%refineAlgArry(ipart)%r => NULL()
          !Use previous procedure if no new algorithm is specified
          IF(ALLOCATED(refAlgNames)) THEN
            IF(ipart <= SIZE(refAlgNames)) THEN
              algName=refAlgNames(ipart)
            ENDIF
          ENDIF
          CALL toUPPER(algName)
          SELECTCASE(TRIM(algName))
            CASE('')     !Do nothing if nothing specified
            CASE('None') !Do nothing if nothing specified
            CASE('KERNIGHAN-LIN')
              thisGraph%refineAlgArry(ipart)%r => KernighanLin_PartitionGraph
            CASE('SPATIAL KERNIGHAN-LIN')
              thisGraph%refineAlgArry(ipart)%r => SpatialKernighanLin_PartitionGraph
            CASE DEFAULT
              CALL ePartitionGraph%raiseError(modName//'::'//myName// &
                ' - Refinement algorithm "'//TRIM(algName)//'" not recognized!')
          ENDSELECT
          ipart=ipart+1
        ENDDO !ipart

        !Calculate degree of each vertex
        ALLOCATE(thisGraph%d(nvert))
        thisGraph%d=0
        DO iv=1,nvert
          thisGraph%d(iv)=COUNT(thisGraph%neigh(1:maxneigh,iv) /= 0)
        ENDDO !iv
      ENDIF
    ENDSUBROUTINE init_PartitionGraph
!
!-------------------------------------------------------------------------------
!> @brief Clear routine for a PartitionGraphType
!> @param thisGraph the graph to be cleared
!>
    SUBROUTINE clear_PartitionGraph(thisGraph)
      CLASS(PartitionGraphType),INTENT(INOUT) :: thisGraph
      INTEGER(SIK) :: ipart

      thisGraph%isInit=.FALSE.
      thisGraph%nvert=0
      thisGraph%dim=0
      thisGraph%maxneigh=0
      thisGraph%nGroups=0
      thisGraph%nPart=0
      IF(ALLOCATED(thisGraph%groupIdx)) DEALLOCATE(thisGraph%groupIdx)
      IF(ALLOCATED(thisGraph%groupList)) DEALLOCATE(thisGraph%groupList)
      IF(ALLOCATED(thisGraph%wts)) DEALLOCATE(thisGraph%wts)
      IF(ALLOCATED(thisGraph%cond)) DEALLOCATE(thisGraph%cond)
      IF(ALLOCATED(thisGraph%d)) DEALLOCATE(thisGraph%d)
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

      !Nullify each procedure pointer for refinement
      IF(ALLOCATED(thisGraph%refineAlgArry)) THEN
        DO ipart=1,SIZE(thisGraph%refineAlgArry)
          thisGraph%refineAlgArry(ipart)%r => NULL()
        ENDDO !ipart
        DEALLOCATE(thisGraph%refineAlgArry)
      ENDIF
    ENDSUBROUTINE clear_PartitionGraph
!
!-------------------------------------------------------------------------------
!> @brief Generate an independent "subgraph" containing the vertices in the
!>        given list. This graph is independent from the parent graph, meaning
!>        vertices in the subgraph are only considered to have edges to other
!>        vertices in the subgraph.
!> @param thisGraph the parent graph to create a subgraph from
!> @param L the list of vertex indices to create the subgraph
!> @param subgraph the generated subgraph
!>
    SUBROUTINE GenerateSubgraph_PartitionGraph(thisGraph,L,ng,subgraph)
      CLASS(PartitionGraphType),INTENT(IN) :: thisGraph
      INTEGER(SIK),INTENT(IN) :: ng,L(:)
      TYPE(PartitionGraphType),INTENT(OUT) :: subgraph
      LOGICAL(SBK) :: lcoord
      LOGICAL(SBK),ALLOCATABLE :: lInL(:)
      INTEGER(SIK) :: iv,cv,snv,in,cv2,il2
      INTEGER(SIK) :: maxneigh,dim

      !Size of new subgraph
      snv=SIZE(L)
      ALLOCATE(lInL(thisGraph%nvert))

      !Determine which verties are in the subgraph
      lInL=.FALSE.
      DO iv=1,snv
        lInL(L(iv))=.TRUE.
      ENDDO !iv

      !Construct the subgraph manually
      maxneigh=thisGraph%maxneigh
      dim=thisGraph%dim

      !Set subgraph scalars
      subgraph%nvert=snv
      subgraph%dim=dim
      subgraph%maxneigh=maxneigh
      subgraph%nPart=thisGraph%nPart
      subgraph%nGroups=ng

      !Allocate memory for arrays
      ALLOCATE(subgraph%wts(snv))
      ALLOCATE(subgraph%d(snv))
      ALLOCATE(subgraph%neigh(maxneigh,snv))
      ALLOCATE(subgraph%neighwts(thisGraph%maxneigh,snv))
      ALLOCATE(subgraph%partitionAlgArry(thisGraph%nPart))
      ALLOCATE(subgraph%refineAlgArry(thisGraph%nPart))
      IF(ALLOCATED(thisGraph%cond)) THEN
        ALLOCATE(subgraph%cond(thisGraph%nPart-1))
        subgraph%cond=thisGraph%cond
      ENDIF
      lcoord=.FALSE.
      IF(ALLOCATED(thisGraph%coord)) THEN
        lcoord=.TRUE.
        ALLOCATE(subgraph%coord(dim,snv))
      ENDIF

      !Populate arrays
      subgraph%partitionAlgArry=thisGraph%partitionAlgArry
      subgraph%refineAlgArry=thisGraph%refineAlgArry
      DO cv=1,snv
        iv=L(cv)
        subgraph%wts(cv)=thisGraph%wts(iv)
        subgraph%d(cv)=thisGraph%d(iv)
        IF(lcoord) subgraph%coord(1:dim,cv)=thisGraph%coord(1:dim,iv)
        subgraph%neigh(1:maxneigh,cv)=thisGraph%neigh(1:maxneigh,iv)
        subgraph%neighwts(1:maxneigh,cv)=thisGraph%neighwts(1:maxneigh,iv)
        !Fix indexing/existence of neighbor matrices and update d accordingly
        DO in=1,maxneigh
          cv2=subgraph%neigh(in,cv)
          IF(cv2 <= 0) CYCLE
          !Vertex is not present in the graph
          IF(.NOT. lInL(cv2)) THEN
            subgraph%neigh(in,cv)=0
            subgraph%neighwts(in,cv)=0
            subgraph%d(cv)=subgraph%d(cv)-1
          ELSE !it is present and needs index change
            DO il2=1,snv
              IF(L(il2) == cv2) THEN
                subgraph%neigh(in,cv)=il2
                EXIT
              ENDIF
            ENDDO !il2
          ENDIF
        ENDDO !in
      ENDDO !cv
      DEALLOCATE(lInL)
    ENDSUBROUTINE GenerateSubgraph_PartitionGraph
!
!-------------------------------------------------------------------------------
!> @brief Manually set the groups of the graph
!> @param thisGraph the graph to set the groups within
!> @param groupIdx the starting index for each group(extra index is nvert+1)
!> @param groupList the list of vertices within each group
!>
    SUBROUTINE setGroups_PartitionGraph(thisGraph,groupIdx,groupList)
      CLASS(PartitionGraphType),INTENT(INOUT) :: thisGraph
      INTEGER(SIK),INTENT(IN) :: groupIdx(:),groupList(:)

      !If already partitioned then deallocate previous partition description
      IF(ALLOCATED(thisGraph%groupIdx)) THEN
        DEALLOCATE(thisGraph%groupIdx)
        DEALLOCATE(thisGraph%groupList)
      ENDIF

      !Allocate new memory and copy lists
      ALLOCATE(thisGraph%groupIdx(thisGraph%nGroups+1))
      ALLOCATE(thisGraph%groupList(thisGraph%nvert))
      thisGraph%groupIdx=groupIdx
      thisGraph%groupList=groupList
    ENDSUBROUTINE setGroups_PartitionGraph
!
!-------------------------------------------------------------------------------
!> @brief Wrapper routine to call the procedure to partition the graph
!> @param thisGraph the graph to be partitioned
!>
    RECURSIVE SUBROUTINE partition_PartitionGraph(thisGraph)
      CLASS(PartitionGraphType),INTENT(INOUT) :: thisGraph
      INTEGER(SIK) :: iv,corAlg,ialg

      IF(thisGraph%nGroups > 1) THEN
        !Determine which partitioning algorithm to use based on graph size
        corAlg=1

        IF(ALLOCATED(thisGraph%cond)) THEN
          DO ialg=1,thisGraph%nPart-1
            IF(thisGraph%nvert > thisGraph%cond(ialg)) THEN
              EXIT
            ENDIF
          ENDDO !ialg
          corAlg=ialg
        ENDIF

        !Call the correct partitioning algorithm
        IF(ASSOCIATED(thisGraph%partitionAlgArry(corAlg)%p)) &
          CALL thisGraph%partitionAlgArry(corAlg)%p(thisGraph)
      ELSE
        !Single group graph...it will contain all the vertices
        ALLOCATE(thisGraph%groupIdx(2))
        ALLOCATE(thisGraph%groupList(thisGraph%nvert))

        !Group indices
        thisGraph%groupIdx(1)=1
        thisGraph%groupIdx(2)=thisGraph%nvert+1

        !List of 1 to nvert
        DO iv=1,thisGraph%nvert
          thisGraph%groupList(iv)=iv
        ENDDO !iv
      ENDIF
    ENDSUBROUTINE partition_PartitionGraph
!
!-------------------------------------------------------------------------------
!> @brief Wrapper routine to call the procedure to refine the graph
!> @param thisGraph the graph containing groups L1, L2
!> @param L1 list of vertices in group 1
!> @param L2 list of vertices in group 2
!>
    SUBROUTINE refine_PartitionGraph(thisGraph,L1,L2)
      CLASS(PartitionGraphType),INTENT(IN) :: thisGraph
      INTEGER(SIK),INTENT(INOUT) :: L1(:),L2(:)
      INTEGER(SIK) :: ialg,corAlg

      !Determine which refinement algorithm to use based on graph size
      corAlg=1
      IF(ALLOCATED(thisGraph%cond)) THEN
        DO ialg=1,thisGraph%nPart-1
          IF(thisGraph%nvert > thisGraph%cond(ialg)) THEN
            EXIT
          ENDIF
        ENDDO !ialg
        corAlg=ialg
      ENDIF

      !Call the correct refminement algorithm
      IF(ASSOCIATED(thisGraph%refineAlgArry(corAlg)%r)) &
         CALL thisGraph%refineAlgArry(corAlg)%r(thisGraph,L1,L2)
    ENDSUBROUTINE refine_PartitionGraph
!
!-------------------------------------------------------------------------------
!> @brief Recursively bisect the graph by starting at a specific vertex, and
!> expanding to a vertex with five prioritized properties:
!> 1. Highest Connectivity to the group
!> 2. Lowest Connectivity to outside the group
!> 3. Largest within group Sphere of Influence (SoI)
!> 4. Largest outside group SoI
!> 5. Smallest distance from reference vertex
!>
!> @param thisGraph the graph to use the REB metod on
!>
    RECURSIVE SUBROUTINE RecursiveExpansionBisection(thisGraph)
      CHARACTER(LEN=*),PARAMETER :: myName='RecursiveExpansionBisection'
      CLASS(PartitionGraphType),INTENT(INOUT) :: thisGraph

      !local scalars
      LOGICAL(SBK) :: lfillL1
      INTEGER(SIK) :: nvert,nv1,iv,jv,kv,in,jn,kn,maxd,maxd2
      INTEGER(SIK) :: soiv,count,bvert
      INTEGER(SIK) :: curSI,curSE,maxSI,maxSE
      REAL(SRK) :: cw1,wg1,wtSum,cE,cS,lE,dS,soid,curdif,wt
      REAL(SRK) :: curInt,curExt,maxInt,minExt,prevExt
      !local arrays
      REAL(SRK) :: wc(thisGraph%dim),soic(thisGraph%dim),gsc(thisGraph%dim)
      !local allocatables
      LOGICAL(SBK),ALLOCATABLE :: SCalc(:),linL1(:),lL1neigh(:)
      INTEGER(SIK),ALLOCATABLE :: S(:,:)
      !local pointers
      INTEGER(SIK),POINTER :: Order(:)

      IF(.NOT. ALLOCATED(thisGraph%coord)) THEN
        CALL ePartitionGraph%raiseError(modName//'::'//myName// &
          ' - cannot use REB method to partition without coordinates!')
      ELSE
        !Initial guess for group sizes
        nvert=thisGraph%nvert
        CALL detGroupSize(thisGraph,wg1,wtSum)
        ALLOCATE(Order(nvert))
        Order=-1

        !Calculate the weighted "centroid" of the graph
        !(i.e. weighted average position)
        wc=0.0_SRK
        DO iv=1,nvert
          wc=wc+thisGraph%coord(1:thisGraph%dim,iv)*thisGraph%wts(iv)
        ENDDO !iv
        wc=wc/wtSum

        !Determine the maximum distance for a "Sphere of Influence" (SoI) vertex
        !Sphere of Influence contains
        ! 1. Directly neighboring vertices
        ! 2. Vertices which neighbor more than 1 direct neighbor
        !    or those which would neighbor more than 1 if a neighbor is missing
        !    (edge cases)
        !The SoI will be of maximum size: maxneigh*dim
        !Find a max degree vertex with max degree neighbor
        maxd=MAXVAL(thisGraph%d)
        maxd2=0
        soiv=-1
        DO iv=1,nvert
          !Check if this vertex is max degree
          IF(thisGraph%d(iv) == maxd) THEN
            !Check if it has atleast 1 max degree neighbor
            DO in=1,thisGraph%maxneigh
              jv=thisGraph%neigh(in,iv)
              IF(jv /= 0) THEN
                IF(thisGraph%d(jv) > maxd2) THEN
                  maxd2=thisGraph%d(jv)
                  soiv=iv
                ENDIF
              ENDIF
            ENDDO !in
          ENDIF
        ENDDO !iv
        soic=thisGraph%coord(1:thisGraph%dim,soiv)

        !Calculate maximum distance in SoI
        soid=0.0_SRK
        DO in=1,thisGraph%maxneigh
          iv=thisGraph%neigh(in,soiv) !neighbor vertex
          IF(iv /= 0) THEN
            !Calculate distance to neighbor
            cS=distance(soic,thisGraph%coord(1:thisGraph%dim,iv))
            IF(cS > soid) soid=cS

            !Check for SoI vertices
            !Loop over 2nd-degree neighbors
            DO jn=1,thisGraph%maxneigh
              count=0
              jv=thisGraph%neigh(jn,iv)
              !Don't check the original vertex or neighbors.
              IF((jv /= 0) .AND. .NOT. ((jv == soiv) .OR. &
                  ANY(jv == thisGraph%neigh(1:thisGraph%maxneigh,soiv)))) THEN
                DO kn=1,thisGraph%maxneigh
                  kv=thisGraph%neigh(kn,jv)
                  IF((kv /= 0) .AND. &
                    ANY(kv == thisGraph%neigh(1:thisGraph%maxneigh,soiv))) &
                    count=count+1
                ENDDO !kn
              ENDIF
              IF(count >= 2) THEN
                cS=distance(soic,thisGraph%coord(1:thisGraph%dim,jv))
                IF(cS > soid) soid=cS
              ENDIF
            ENDDO !jn
          ENDIF
        ENDDO !in

        !Determine the starting vertex using the following prioritized rules:e
        ! 1. Outer rim vertex (on outside edge of graph)
        ! 2. Lowest weighted edges with neighbors(direct)
        ! 3. Furthest from weighted centroid
        lE=HUGE(1.0_SRK)
        dS=0.0_SRK
        DO iv=1,nvert
          !Check if the vertex is a vertex on the outer rim of graph
          IF(ANY(thisGraph%neigh(1:thisGraph%maxneigh,iv) == 0)) THEN
            !Calculate the vertex's weighted edge sum
            cE=SUM(thisGraph%neighwts(1:thisGraph%maxneigh,iv))
            IF(cE < lE) THEN
              lE=cE
              !Calculate distance from graph's weighted centroid
              dS=distance(wc,thisGraph%coord(1:thisGraph%dim,iv))
              Order(1)=iv !Starting vertex
            ELSEIF(cE == lE) THEN
              !Calculate distance from graph's weighted centroid
              cS=distance(wc,thisGraph%coord(1:thisGraph%dim,iv))
              IF(cS > dS+EPSREAL) THEN
                dS=cS
                Order(1)=iv !Starting vertex
              ENDIF
            ENDIF
          ENDIF
        ENDDO !iv
        gsc=thisGraph%coord(1:thisGraph%dim,Order(1)) !Starting vertex coordinates

        !Allocate memory for SoI list for each vertex...only calculate the ones
        !which are needed, in the interior loops
        !Maximum size expected is dim*maxneigh for each vertex
        ALLOCATE(SCalc(nvert))
        ALLOCATE(S(thisGraph%dim*thisGraph%maxneigh,nvert))
        ALLOCATE(lInL1(nvert))
        ALLOCATE(lL1neigh(nvert))
        Scalc=.FALSE.
        lInL1=.FALSE.
        lL1neigh=.FALSE.
        S=0
        lInL1(Order(1))=.TRUE.

        !Calculate (weighted) sum of external edges
        iv=Order(1)
        prevExt=0.0_SRK
        DO jn=1,thisGraph%maxneigh
          jv=thisGraph%neigh(jn,iv)
          IF(jv > 0) THEN
              prevExt=prevExt+thisGraph%neighwts(jn,iv)
          ENDIF
        ENDDO !jn

        !Expand group using the following rules:
        ! 1. Highest I
        ! 2. Lowest E
        ! 3. Highest SI
        ! 4. Highest SE
        ! 5. Smallest distance from reference vertex
        lfillL1=.TRUE.
        cw1=thisGraph%wts(Order(1))
        nv1=1
        curdif=wg1-cw1
        DO kv=2,nvert
          !Update list of possible neighbors
          DO jn=1,thisGraph%maxneigh
            jv=thisGraph%neigh(jn,Order(kv-1))
            IF(jv > 0) lL1neigh(jv)=.TRUE.
          ENDDO !jn

          !Reset targets
          maxInt=0
          minExt=HUGE(1)
          maxSI=0
          maxSE=0
          dS=HUGE(1.0_SRK)
          bvert=-1
          !Loop over all vertices
          DO iv=1,nvert
            !Only consider those not already within the group
            IF(.NOT. lInL1(iv) .AND. lL1neigh(iv)) THEN
              wt=thisGraph%wts(iv)

              !Calculate (weighted) internal and external edge sums
              curInt=0
              curExt=0
              DO jn=1,thisGraph%maxneigh
                jv=thisGraph%neigh(jn,iv)
                IF(jv /= 0) THEN
                  IF(lInL1(jv)) THEN
                    curInt=curInt+thisGraph%neighwts(jn,iv)
                  ELSE
                    curExt=curExt+thisGraph%neighwts(jn,iv)
                  ENDIF
                ENDIF
              ENDDO !jn

              IF(curInt > maxInt) THEN
                !Set new min/max and store index for new "best"
                maxInt=curInt
                minExt=curExt
                CALL detSoI(thisGraph,iv,linL1,soid,Scalc,S,maxSI,maxSE)
                dS=distance(gsc,thisGraph%coord(1:thisGraph%dim,iv))
                bvert=iv
              ELSEIF(curInt .APPROXEQA. maxInt) THEN
                IF(curExt < minExt) THEN
                  minExt=curExt
                  CALL detSoI(thisGraph,iv,linL1,soid,Scalc,S,maxSI,maxSE)
                  dS=distance(gsc,thisGraph%coord(1:thisGraph%dim,iv))
                  bvert=iv
                ELSEIF(curExt .APPROXEQA. minExt) THEN
                  !Determine sphere of influence weighted internal/external edges
                  CALL detSoI(thisGraph,iv,linL1,soid,Scalc,S,curSI,curSE)
                  IF(curSI > maxSI) THEN
                    maxSI=curSI
                    maxSE=curSE
                    dS=distance(gsc,thisGraph%coord(1:thisGraph%dim,iv))
                    bvert=iv
                  ELSEIF(curSI == maxSI) THEN
                    IF(curSE > maxSE) THEN
                      maxSE=curSE
                      dS=distance(gsc,thisGraph%coord(1:thisGraph%dim,iv))
                      bvert=iv
                    ELSEIF(curSE == maxSE) THEN
                      cS=distance(gsc,thisGraph%coord(1:thisGraph%dim,iv))
                      IF(cS < dS - EPSREAL) THEN
                        dS=cS
                        bvert=iv
                      ENDIF
                    ENDIF
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
          ENDDO !iv

          !Add "best" vertex to the list and update weighted sum
          IF(bvert == -1) EXIT
          IF(ABS(curdif-thisGraph%wts(bvert)) > ABS(curdif)) lfillL1=.FALSE.
          IF(lfillL1) THEN
            nv1=nv1+1
            cw1=cw1+thisGraph%wts(bvert)
            curdif=curdif-thisGraph%wts(bvert)
          ENDIF
          lInL1(bvert)=.TRUE.
          Order(kv)=bvert

          !Check if reference (distance) point needs to be changed
          IF(minExt < prevExt) THEN
            gsc=thisGraph%coord(1:thisGraph%dim,bvert)
          ENDIF
          prevExt = minExt
        ENDDO !kv

        ! If the domain we decomposed was discontinuous (rarely occurs)
        ! Fill the rest into order
        IF(bvert == -1) THEN
          DO iv=1,nvert
            IF(.NOT.linL1(iv)) THEN
              Order(kv)=iv
              kv=kv+1
            ENDIF
          ENDDO !iv
        ENDIF

        !Deallocate sphere memory
        DEALLOCATE(S)
        DEALLOCATE(Scalc)
        DEALLOCATE(lInL1)
        DEALLOCATE(lL1neigh)

        !Recursively partition the graph
        CALL recursivePartitioning(thisGraph,Order,nv1,cw1,wtSum)

        !Deallocate lists
        DEALLOCATE(Order)
      ENDIF
    ENDSUBROUTINE RecursiveExpansionBisection
!
!-------------------------------------------------------------------------------
!> @brief Recursively bisects the graph using a spectral(matrix) method
!> @param thisGraph the graph to be partitioned
!>
    RECURSIVE SUBROUTINE RecursiveSpectralBisection(thisGraph)
      CHARACTER(LEN=*),PARAMETER :: myName='RecursiveSpectralBisection'
      CLASS(PartitionGraphType),INTENT(INOUT) :: thisGraph
#ifdef FUTILITY_HAVE_SLEPC
      INTEGER(SIK) :: nvert,iv,jv,in,nlocal,nneigh
      INTEGER(SIK) :: nv1
      REAL(SRK) :: wg1,cw1,curdif,wt,wtSum,wneigh
      TYPE(ParamType) :: matParams
      CLASS(VectorType),ALLOCATABLE :: evecs(:)
      TYPE(PETScMatrixType) :: Lmat
      INTEGER(SIK),POINTER :: Order(:)

      nvert=thisGraph%nvert
      nneigh=thisGraph%maxneigh

      !Generate edge-weighted Laplacian
      !All local for now
      nlocal=nvert

      !Construct the matrix
      CALL matParams%add('MatrixType->n',nvert)
      CALL matParams%add('MatrixType->isSym',.TRUE.)
      CALL matParams%add('MatrixType->matType',SPARSE)
      CALL matParams%add('MatrixType->MPI_COMM_ID',PE_COMM_SELF)
      CALL matParams%add('MatrixType->nlocal',nlocal)
      CALL matParams%add('MatrixType->dnnz',(/0/))
      CALL matParams%add('MatrixType->onnz',(/0/))

      !Initialize matrix
      CALL Lmat%init(matParams)

      !Clear parameter list
      CALL matParams%clear()
      !Set the matrix
      DO iv=1,nvert
        wneigh=SUM(thisGraph%neighwts(1:nneigh,iv))
        CALL Lmat%set(iv,iv,wneigh)
        DO in=1,nneigh
          jv=thisGraph%neigh(in,iv)
          IF(jv /= 0) THEN
            wneigh=-thisGraph%neighwts(in,iv)
            CALL Lmat%set(iv,jv,wneigh)
          ENDIF
        ENDDO !in
      ENDDO !iv

      !Find Fiedler vector (Algebraic connectivity) - 2nd smallest eigenvector
      CALL getEigenVecs(Lmat,.TRUE.,4,evecs)
      CALL Lmat%clear()

      !Get the Order
      ALLOCATE(Order(nvert))
      DO iv=1,nvert
        Order(iv)=iv
      ENDDO !iv
      SELECTTYPE(evecs(1)); TYPEIS(PETScVectorType)
        CALL recursiveEigenOrder(evecs(2:), Order)
      ENDSELECT
      DO iv=1,SIZE(evecs)
        CALL evecs(iv)%clear()
      ENDDO !iv
      DEALLOCATE(evecs)

      !Initial guess for group sizes
      CALL detGroupSize(thisGraph,wg1,wtSum)
      cw1=0.0_SRK
      curdif=wg1
      !Expand until closest to desired size
      nv1=0
      DO jv=1,nvert
        iv=Order(jv)
        wt=thisGraph%wts(iv)
        IF(ABS(curdif-wt) < ABS(curdif)) THEN
          nv1=nv1+1
          curdif=curdif-wt
          cw1=cw1+wt
        ELSE
          EXIT
        ENDIF
      ENDDO !jv

      !Recursively partition the graph
      CALL recursivePartitioning(thisGraph,Order,nv1,cw1,wtSum)

      !Deallocate lists
      DEALLOCATE(Order)
#else
      CALL ePartitionGraph%raiseError(modName//'::'//myName// &
        ' - must recompile with PETSc and SLEPc to use RSB!')
#endif
    ENDSUBROUTINE RecursiveSpectralBisection
!
!-------------------------------------------------------------------------------
!> @brief Recursively bisects the graph using inertial moments
!> @param thisGraph the graph to be partitioned
!>
    RECURSIVE SUBROUTINE RecursiveInertialBisection(thisGraph)
      CHARACTER(LEN=*),PARAMETER :: myName='RecursiveInertialBisection'
      CLASS(PartitionGraphType),INTENT(INOUT) :: thisGraph
#ifdef FUTILITY_HAVE_SLEPC
      INTEGER(SIK) :: dim,iv,jv
      INTEGER(SIK) :: nv1,idim,idim2
      REAL(SRK) :: wtSum,wg1,wt,cw1,curdif,ccoord,cprod
      REAL(SRK),ALLOCATABLE :: I(:,:),coord(:,:),cent(:)
      TYPE(ParamType) :: mparams
      CLASS(VectorType),ALLOCATABLE :: evecs(:)
      TYPE(PETScMatrixType) :: Imat
      INTEGER(SIK),POINTER :: Order(:)

      IF(.NOT. ALLOCATED(thisGraph%coord)) THEN
        CALL ePartitionGraph%raiseError(modName//'::'//myName// &
          ' - cannot use REB method to partition without coordinates!')
      ELSE
        !Transform the graph coordinates so the origin is at weighted centroid
        dim=thisGraph%dim
        ALLOCATE(cent(dim))
        ALLOCATE(coord(dim,thisGraph%nvert))
        cent=0.0_SRK
        wtSum=0.0_SRK
        DO iv=1,thisGraph%nvert
          wt=thisGraph%wts(iv)
          wtSum=wtSum+wt
          cent(1:dim)=cent(1:dim)+wt*thisGraph%coord(1:dim,iv)
        ENDDO !iv
        cent=cent/wtSum
        coord=thisGraph%coord
        DO iv=1,thisGraph%nvert
          coord(1:dim,iv)=coord(1:dim,iv)-cent
        ENDDO !iv

        !Calculate the inertial matrix
        ALLOCATE(I(dim,dim))
        I=0.0_SRK
        DO iv=1,thisGraph%nvert
          DO idim=1,dim
            ccoord=coord(idim,iv)
            I(idim,idim)=I(idim,idim)+ccoord*ccoord
            DO idim2=idim+1,dim
              cprod=ccoord*coord(idim2,iv)
              I(idim,idim2)=I(idim,idim2)+cprod
              I(idim2,idim)=I(idim2,idim)+cprod
            ENDDO !idim2
          ENDDO !idim
        ENDDO !iv


        !Now construct the matrix type from I
        CALL mparams%add('MatrixType->n', dim)
        CALL mparams%add('MatrixType->isSym', .TRUE.)
        CALL mparams%add('MatrixType->matType', DENSESQUARE)
        CALL mparams%add('MatrixType->MPI_COMM_ID', PE_COMM_SELF)
        CALL mparams%add('MatrixType->nlocal', dim)
        CALL mparams%add('MatrixType->dnnz', (/0/))
        CALL mparams%add('MatrixType->onnz', (/0/))
        CALL Imat%init(mparams)
        CALL mparams%clear()

        DO iv=1,dim
          DO jv=1,iv
            CALL Imat%set(iv,jv,I(jv,iv))
            CALL Imat%set(jv,iv,I(jv,iv))
          ENDDO !jv
        ENDDO !iv

        !Solve the eigenvalue problem
        CALL getEigenVecs(Imat, .FALSE., dim, evecs)
        CALL Imat%clear()

        !Determine the order based on distance from the inertial vectors
        ALLOCATE(Order(thisGraph%nvert))
        DO iv=1,thisGraph%nvert
          Order(iv)=iv
        ENDDO !iv
        SELECTTYPE(evecs); TYPEIS(PETScVectorType)
          CALL recursiveEigenOrder(evecs, Order, dot=coord)
        ENDSELECT
        DEALLOCATE(coord)
        DO iv=1,SIZE(evecs)
          CALL evecs(iv)%clear()
        ENDDO !iv
        DEALLOCATE(evecs)

        !Initial guess for group sizes
        CALL detGroupSize(thisGraph,wg1,wtSum)
        cw1=0.0_SRK
        curdif=wg1

        !Expand until closest to desired size
        nv1=0
        DO jv=1,thisGraph%nvert
          iv=Order(jv)
          wt=thisGraph%wts(iv)
          IF(ABS(curdif-wt) < ABS(curdif)) THEN
            nv1=nv1+1
            curdif=curdif-wt
            cw1=cw1+wt
          ELSE
            EXIT
          ENDIF
        ENDDO !jv

        !Recursively partition the graph
        CALL recursivePartitioning(thisGraph,Order,nv1,cw1,wtSum)

        !Deallocate lists
        DEALLOCATE(Order)
      ENDIF
#else
      CALL ePartitionGraph%raiseError(modName//'::'//myName// &
        ' - must recompile with PETSc and SLEPc to use RIB!')
#endif
    ENDSUBROUTINE RecursiveInertialBisection
!
!-------------------------------------------------------------------------------
!> @brief Calculates distance between two points
!> @param c1 coordinate/point 1
!> @param c2 coordinate/point 2
!> @param d distance between c1 and c2
!>
    PURE FUNCTION distance(c1,c2) RESULT(d)
      REAL(SRK),INTENT(IN) :: c1(:), c2(:)
      REAL(SRK) :: d,dif
      INTEGER(SIK) :: id

      d=0.0_SRK
      DO id=1,SIZE(c1)
        dif=c1(id)-c2(id)
        d=d+dif*dif
      ENDDO !id
      d=SQRT(d)
    ENDFUNCTION distance
!
!-------------------------------------------------------------------------------
!> @brief Determines the Sphere of Influence of a vertex, and the weighted
!>        internal and external edge sums relative to the given group
!> @param thisGraph the graph type the SoI is within
!> @param iv the vertex index for finding SoI
!> @param linL1 logical array for if vertex is within the expanding domain
!> @param r radius of SoI
!> @param Scalc logical on whether or not the SoI has already been determined
!> @param S SoI vertex indices
!> @param SI SoI intenal edges sum
!> @param SE SoI external edges sum
!>
    SUBROUTINE detSoI(thisGraph,iv,linL1,r,Scalc,S,SI,SE)
      CLASS(PartitionGraphType),INTENT(IN) :: thisGraph
      INTEGER(SIK),INTENT(IN) :: iv
      LOGICAL(SBK),INTENT(IN) :: linL1(:)
      REAL(SRK),INTENT(IN) :: r
      LOGICAL(SBK),INTENT(INOUT) :: Scalc(thisGraph%nvert)
      INTEGER(SIK),INTENT(INOUT) :: S(thisGraph%dim*thisGraph%maxneigh,thisGraph%nvert)
      INTEGER(SIK),INTENT(OUT) :: SI,SE
      INTEGER(SIK) :: maxEl
      INTEGER(SIK) :: jv,kv,jn,kn,csi
      REAL(SRK) :: cr

      !Maximum size of the SoI
      maxEl=thisGraph%dim*thisGraph%maxneigh
      !Check if SoI has already been determined for this vertex
      IF(.NOT. Scalc(iv)) THEN
        !Determine Sphere of Influence
        S(1:thisGraph%maxneigh*thisGraph%dim,iv)=0
        csi=0
        DO jn=1,thisGraph%maxneigh
          jv=thisGraph%neigh(jn,iv)
          IF(jv /= 0) THEN
            !Add existing neighbors to the sphere list
            IF(.NOT. ANY(jv == S(1:maxEl,iv))) THEN
              csi=csi+1
              S(csi,iv)=jv
            ENDIF

            !Check 2nd degree neighbors via radius
            DO kn=1,thisGraph%maxneigh
              kv=thisGraph%neigh(kn,jv)
              !Check 2nd-degree neighbor exists, and is not a 1st degree neighbor
              IF((kv /= 0) .AND. (kv /= iv) .AND. &
                 .NOT. ANY(kv == S(1:maxEl,iv))) THEN
                !Calculate distance to this vertex
                cr=distance(thisGraph%coord(1:thisGraph%dim,iv), &
                            thisGraph%coord(1:thisGraph%dim,kv))
                IF(cr .APPROXLE. r) THEN
                  csi=csi+1
                  S(csi,iv)=kv
                ENDIF
              ENDIF
            ENDDO !kn
          ENDIF
        ENDDO !jn
        Scalc(iv)=.TRUE.
      ENDIF

      !Calculate internal and external edge sums
      SI=0
      SE=0
      DO jn=1,maxEl
        jv=S(jn,iv)
        IF(jv > 0) THEN
          IF(linL1(jv)) THEN
            SI=SI+1
          ELSE
            SE=SE+1
          ENDIF
        ENDIF
      ENDDO !jn
    ENDSUBROUTINE detSoI
!
!-------------------------------------------------------------------------------
!> @brief Algorithm to minimize communication between groups in a PartitionGraph
!> @param thisGraph the graph containing vertices in L1, L2
!> @param L1 the list of vertices in group 1
!> @param L2 the list of vertices in group 2
!>
    SUBROUTINE KernighanLin_PartitionGraph(thisGraph,L1,L2)
      CLASS(PartitionGraphType),INTENT(IN) :: thisGraph
      INTEGER(SIK),INTENT(INOUT) :: L1(:),L2(:)
      INTEGER(SIK) :: ia,ib,iam,ibm,cva,cvb,in,ineigh
      INTEGER(SIK) :: N1,N2,nneigh,ng,cg,iv,ig,k
      LOGICAL(SBK),ALLOCATABLE :: lInL1(:),locked(:)
      INTEGER(SIK),ALLOCATABLE :: av(:),bv(:)
      REAL(SRK),ALLOCATABLE :: D(:),gv(:)
      REAL(SRK) :: wg1,wg2,wdiff,wta,wtb,g,gmax
      REAL(SRK) :: wo1,wo2,woc

      !Allocate memory
      nneigh=thisGraph%maxneigh
      N1=SIZE(L1)
      N2=SIZE(L2)
      ng=MIN(N1,N2)
      ALLOCATE(D(thisGraph%nvert))
      ALLOCATE(lInL1(thisGraph%nvert))
      ALLOCATE(locked(thisGraph%nvert))
      ALLOCATE(av(ng))
      ALLOCATE(bv(ng))
      ALLOCATE(gv(ng))

      !Determine which vertices are in L1
      lInL1=.FALSE.
      DO iv=1,N1
        lInL1(L1(iv))=.TRUE.
      ENDDO !iv

      !Determine optimal weights for domains (1 or 2 doesn't matter)
      wg1=SUM(thisGraph%wts)
      wo1=wg1*(REAL(thisGraph%nGroups/2,SRK)/REAL(thisGraph%nGroups,SRK))
      wo2=wg1-wo1
      woc=wo1 ! Current optimal target for domain 1

      gmax=1.0_SRK
      DO WHILE(gmax > 0)
        !Calculate D=E-I
        D=0
        DO ia=1,N1
          cva=L1(ia)
          DO in=1,nneigh
            ineigh=thisGraph%neigh(in,cva)
            IF(ineigh == 0) CYCLE
            IF(lInL1(ineigh)) THEN
              D(cva)=D(cva)-thisGraph%neighwts(in,cva)
            ELSE
              D(cva)=D(cva)+thisGraph%neighwts(in,cva)
            ENDIF
          ENDDO !in
        ENDDO !ia
        DO ib=1,N2
          cvb=L2(ib)
          DO in=1,nneigh
            ineigh=thisGraph%neigh(in,cvb)
            IF(ineigh == 0) CYCLE
            IF(.NOT. lInL1(ineigh)) THEN
              D(cvb)=D(cvb)-thisGraph%neighwts(in,cvb)
            ELSE
              D(cvb)=D(cvb)+thisGraph%neighwts(in,cvb)
            ENDIF
          ENDDO !in
        ENDDO !ib

        !Begin with empty set
        av=-1
        bv=-1
        gv=-1.0_SRK
        cg=0
        locked=.FALSE.

        !Current size of each group
        wg1=0
        wg2=0
        DO iv=1,N1
          wg1=wg1+thisGraph%wts(L1(iv))
        ENDDO !iv
        DO iv=1,N2
          wg2=wg2+thisGraph%wts(L2(iv))
        ENDDO !iv

        DO ig=1,ng
          !Current weight difference between groups
          wdiff=wg2-wg1

          !Find maximum value of g
          iam=-1
          ibm=-1
          gmax=-HUGE(1.0_SRK)
          DO ia=1,N1
            cva=L1(ia)
            IF(.NOT. locked(cva)) THEN
              DO ib=1,N2
                cvb=L2(ib)
                IF(.NOT. locked(cvb)) THEN
                  g=D(cva)+D(cvb)
                  DO in=1,nneigh
                    ineigh=thisGraph%neigh(in,cva)
                    IF(ineigh == cvb) THEN
                      g=g-2.0_SRK*thisGraph%neighwts(in,cva)
                      EXIT
                    ENDIF
                  ENDDO !in

                  !Check for new maximal value
                  IF(g > gmax) THEN
                    iam=ia
                    ibm=ib
                    gmax=g
                  ENDIF
                ENDIF
              ENDDO !ib
            ENDIF
          ENDDO !ia

          !Check if found vertices to swap
          IF(iam == -1) THEN
            EXIT
          ELSE
            cg=cg+1
          ENDIF

          !Check if balance is conserved or improved. If not, stop search.
          wta=thisGraph%wts(iam)
          wtb=thisGraph%wts(ibm)
          wdiff=wtb-wta
          IF(ABS(wg1+wdiff-wo1) <= ABS(wg1-woc)) THEN
            woc=wo1
          ELSEIF(ABS(wg1+wdiff-wo2) <= ABS(wg1-woc)) THEN
            woc=wo2
          ELSE
            cg=cg-1
            EXIT
          ENDIF

          !Update sets
          av(ig)=L1(iam)
          bv(ig)=L2(ibm)
          gv(ig)=gmax
          locked(av(ig))=.TRUE.
          locked(bv(ig))=.TRUE.

          !Update weights of groups as if these have been swapped
          wg1=wg1+wdiff
          wg2=wg2-wdiff

          !Update D values as if these have been swapped
          DO in=1,nneigh
            ineigh=thisGraph%neigh(in,av(ig))
            IF(ineigh <= 0) CYCLE
            IF(lInL1(ineigh)) THEN
              D(ineigh)=D(ineigh)+2.0_SRK*thisGraph%neighwts(in,av(ig))
            ELSE
              D(ineigh)=D(ineigh)-2.0_SRK*thisGraph%neighwts(in,av(ig))
            ENDIF
          ENDDO !in
          DO in=1,nneigh
            ineigh=thisGraph%neigh(in,bv(ig))
            IF(ineigh <= 0) CYCLE
            IF(lInL1(ineigh)) THEN
              D(ineigh)=D(ineigh)-2.0_SRK*thisGraph%neighwts(in,bv(ig))
            ELSE
              D(ineigh)=D(ineigh)+2.0_SRK*thisGraph%neighwts(in,bv(ig))
            ENDIF
          ENDDO !in
        ENDDO !ig

        k=-1
        gmax=-HUGE(1.0_SRK)
        g=0.0_SRK
        !Find k which maximizes sum gv(1:k)
        DO ig=1,cg
          g=g+gv(ig)
          IF(g > gmax) THEN
            gmax=g
            k=ig
          ENDIF
        ENDDO !ig

        !Exchange between groups
        IF(gmax > 0) THEN
          DO ig=1,k
            lInL1(av(ig))=.FALSE.
            lInL1(bv(ig))=.TRUE.
            DO ia=1,N1
              IF(L1(ia) == av(ig)) THEN
                L1(ia)=bv(ig)
                EXIT
              ENDIF
            ENDDO !ia
            DO ib=1,N2
              IF(L2(ib) == bv(ig)) THEN
                L2(ib)=av(ig)
                EXIT
              ENDIF
            ENDDO !ib
          ENDDO !ig
        ENDIF
      ENDDO !gmax > 0

      !Free up memory
      DEALLOCATE(D)
      DEALLOCATE(av)
      DEALLOCATE(bv)
      DEALLOCATE(gv)
      DEALLOCATE(locked)
      DEALLOCATE(lInL1)
    ENDSUBROUTINE KernighanLin_PartitionGraph
!
!-------------------------------------------------------------------------------
!> @brief Algorithm to minimize communication between groups in a PartitionGraph
!> @param thisGraph the graph containing vertices in L1, L2
!> @param L1 the list of vertices in group 1
!> @param L2 the list of vertices in group 2
!>
    SUBROUTINE SpatialKernighanLin_PartitionGraph(thisGraph,L1,L2)
      CHARACTER(LEN=*),PARAMETER :: myName='SpatialKernighanLin_PartitionGraph'
      CLASS(PartitionGraphType),INTENT(IN) :: thisGraph
      INTEGER(SIK),INTENT(INOUT) :: L1(:),L2(:)
      INTEGER(SIK) :: ia,ib,iam,ibm,cva,cvb,in,ineigh
      INTEGER(SIK) :: N1,N2,nneigh,ng,cg,iv,ig,k
      LOGICAL(SBK),ALLOCATABLE :: lInL1(:),locked(:)
      INTEGER(SIK),ALLOCATABLE :: av(:),bv(:)
      REAL(SRK),ALLOCATABLE :: D(:),gv(:),wd(:)
      REAL(SRK) :: wg1,wg2,wdiff,wta,wtb,g,gmax,sd,dmax
      REAL(SRK) :: wo1,wo2,woc

      IF(.NOT. ALLOCATED(thisGraph%coord)) THEN
        CALL ePartitionGraph%raiseError(modName//'::'//myName// &
          ' - cannot use SKL method to refine without coordinates!')
      ELSE
        !Allocate memory
        nneigh=thisGraph%maxneigh
        N1=SIZE(L1)
        N2=SIZE(L2)
        ng=MIN(N1,N2)
        ALLOCATE(D(thisGraph%nvert))
        ALLOCATE(wd(thisGraph%nvert))
        ALLOCATE(lInL1(thisGraph%nvert))
        ALLOCATE(locked(thisGraph%nvert))
        ALLOCATE(av(ng))
        ALLOCATE(bv(ng))
        ALLOCATE(gv(ng))

        !Determine which vertices are in L1
        lInL1=.FALSE.
        DO iv=1,N1
          lInL1(L1(iv))=.TRUE.
        ENDDO !iv

        !Calculate weighted sum of edges out of each vertex (weighted degree)
        DO iv=1,thisGraph%nvert
          wd(iv)=SUM(thisGraph%neighwts(:,iv))
        ENDDO !iv

        !Determine optimal weights for domains (1 or 2 doesn't matter)
        wg1=SUM(thisGraph%wts)
        wo1=wg1*(REAL(thisGraph%nGroups/2,SRK)/REAL(thisGraph%nGroups,SRK))
        wo2=wg1-wo1
        woc=wo1 ! Current optimal target for domain 1

        gmax=1.0_SRK
        DO WHILE(gmax > 0)
          !Calculate D=E-I
          D=0
          DO ia=1,N1
            cva=L1(ia)
            DO in=1,nneigh
              ineigh=thisGraph%neigh(in,cva)
              IF(ineigh == 0) CYCLE
              IF(lInL1(ineigh)) THEN
                D(cva)=D(cva)-thisGraph%neighwts(in,cva)
              ELSE
                D(cva)=D(cva)+thisGraph%neighwts(in,cva)
              ENDIF
            ENDDO !in
          ENDDO !ia
          DO ib=1,N2
            cvb=L2(ib)
            DO in=1,nneigh
              ineigh=thisGraph%neigh(in,cvb)
              IF(ineigh == 0) CYCLE
              IF(.NOT. lInL1(ineigh)) THEN
                D(cvb)=D(cvb)-thisGraph%neighwts(in,cvb)
              ELSE
                D(cvb)=D(cvb)+thisGraph%neighwts(in,cvb)
              ENDIF
            ENDDO !in
          ENDDO !ib

          !Begin with empty set
          av=-1
          bv=-1
          gv=-1.0_SRK
          cg=0
          locked=.FALSE.

          !Current size of each group
          wg1=0.0_SRK
          wg2=0.0_SRK
          DO iv=1,N1
            wg1=wg1+thisGraph%wts(L1(iv))
          ENDDO !iv
          DO iv=1,N2
            wg2=wg2+thisGraph%wts(L2(iv))
          ENDDO !iv

          DO ig=1,ng
            !Find maximum value of g
            iam=-1
            ibm=-1
            dmax=0.0_SRK
            gmax=-HUGE(1.0_SRK)
            DO ia=1,N1
              cva=L1(ia)
              IF(D(cva) > -wd(cva) .AND. .NOT. locked(cva)) THEN
                DO ib=1,N2
                  cvb=L2(ib)
                  IF(D(cvb) > -wd(cvb) .AND. .NOT. locked(cvb)) THEN
                    !Calculate gain from swap
                    g=D(cva)+D(cvb)
                    DO in=1,nneigh
                      ineigh=thisGraph%neigh(in,cva)
                      IF(ineigh == cvb) THEN
                        g=g-2.0_SRK*thisGraph%neighwts(in,cva)
                        EXIT
                      ENDIF
                    ENDDO !in

                    !Check for maximal gain
                    IF(g >= gmax) THEN
                      IF(g > gmax) THEN
                        gmax=g
                        dmax=0.0_SRK
                      ENDIF

                      sd=distance(thisGraph%coord(:,cva),thisGraph%coord(:,cvb))
                      IF(sd > dmax) THEN
                        dmax=sd
                        iam=ia
                        ibm=ib
                      ENDIF
                    ENDIF
                  ENDIF
                ENDDO !ib
              ENDIF
            ENDDO !ia

            !If no swap found under the conditions then search using KL
            IF(iam == -1) THEN
              DO ia=1,N1
                cva=L1(ia)
                IF(.NOT. locked(cva)) THEN
                  DO ib=1,N2
                    cvb=L2(ib)
                    IF(.NOT. locked(cvb)) THEN
                      g=D(cva)+D(cvb)
                      DO in=1,nneigh
                        ineigh=thisGraph%neigh(in,cva)
                        IF(ineigh == cvb) THEN
                          g=g-2.0_SRK*thisGraph%neighwts(in,cva)
                          EXIT
                        ENDIF
                      ENDDO !in

                      IF(g > gmax) THEN
                        gmax=g
                        iam=ia
                        ibm=ib
                      ENDIF
                    ENDIF
                  ENDDO !ib
                ENDIF
              ENDDO !ia

              !Exit if a swap is still not found
              IF(iam == -1) THEN
                EXIT
              ELSE
                cg=cg+1
              ENDIF
            ELSE
              cg=cg+1
            ENDIF

            !Check if balance is conserved or improved. If not, stop search.
            wta=thisGraph%wts(iam)
            wtb=thisGraph%wts(ibm)
            wdiff=wtb-wta
            IF(ABS(wg1+wdiff-wo1) <= ABS(wg1-woc)) THEN
              woc=wo1
            ELSEIF(ABS(wg1+wdiff-wo2) <= ABS(wg1-woc)) THEN
              woc=wo2
            ELSE
              cg=cg-1
              EXIT
            ENDIF

            !Update sets
            av(ig)=L1(iam)
            bv(ig)=L2(ibm)
            gv(ig)=gmax
            locked(av(ig))=.TRUE.
            locked(bv(ig))=.TRUE.
            !Update weights of groups as if these have been swapped
            wg1=wg1+wdiff
            wg2=wg2-wdiff

            !Update D values as if these have been swapped
            DO in=1,nneigh
              ineigh=thisGraph%neigh(in,av(ig))
              IF(ineigh <= 0) CYCLE
              IF(lInL1(ineigh)) THEN
                D(ineigh)=D(ineigh)+2.0_SRK*thisGraph%neighwts(in,av(ig))
              ELSE
                D(ineigh)=D(ineigh)-2.0_SRK*thisGraph%neighwts(in,av(ig))
              ENDIF
            ENDDO !in
            DO in=1,nneigh
              ineigh=thisGraph%neigh(in,bv(ig))
              IF(ineigh <= 0) CYCLE
              IF(lInL1(ineigh)) THEN
                D(ineigh)=D(ineigh)-2.0_SRK*thisGraph%neighwts(in,bv(ig))
              ELSE
                D(ineigh)=D(ineigh)+2.0_SRK*thisGraph%neighwts(in,bv(ig))
              ENDIF
            ENDDO !in
          ENDDO !ig

          k=-1
          gmax=-HUGE(1.0_SRK)
          g=0.0_SRK
          !Find k which maximizes sum gv(1:k)
          DO ig=1,cg
            g=g+gv(ig)
            IF(g > gmax) THEN
              gmax=g
              k=ig
            ENDIF
          ENDDO !ig

          !Exchange between groups
          IF(gmax > 0) THEN
            DO ig=1,k
              lInL1(av(ig))=.FALSE.
              lInL1(bv(ig))=.TRUE.
              DO ia=1,N1
                IF(L1(ia) == av(ig)) THEN
                  L1(ia)=bv(ig)
                  EXIT
                ENDIF
              ENDDO !ia
              DO ib=1,N2
                IF(L2(ib) == bv(ig)) THEN
                  L2(ib)=av(ig)
                  EXIT
                ENDIF
              ENDDO !ib
            ENDDO !ig
          ENDIF
        ENDDO !gmax > 0

        !Free up memory
        DEALLOCATE(wd)
        DEALLOCATE(D)
        DEALLOCATE(av)
        DEALLOCATE(bv)
        DEALLOCATE(gv)
        DEALLOCATE(locked)
        DEALLOCATE(lInL1)
      ENDIF
    ENDSUBROUTINE SpatialKernighanLin_PartitionGraph
!
!-------------------------------------------------------------------------------
!> @brief Calculate metrics relevant to the quality of partition
!> @param thisGraph the partitioned graph to check metrics of
!> @param mmr the ratio of maximum sized(weighted) group to minimum
!> @param srms the rms of the group size(weighted) difference from optimal
!> @param ecut the total weight of edges cut
!> @param comm the total weight of communication between groups
!>
    SUBROUTINE calcDecompMetrics_PartitionGraph(thisGraph,mmr,srms,ecut,comm)
      CHARACTER(LEN=*),PARAMETER :: myName='calcDecompMetrics'
      CLASS(PartitionGraphType),INTENT(IN) :: thisGraph
      REAL(SRK),INTENT(OUT) :: mmr,srms,ecut,comm
      INTEGER(SIK) :: ig,igstt,igstp,in,ineigh,iv,ivert,neighGrp
      REAL(SRK) :: wtSum,wtGrp,lgroup,sgroup,optSize,wtDif
      INTEGER(SIK),ALLOCATABLE :: grpMap(:),uniqueGrps(:)

      mmr=0.0_SRK
      srms=0.0_SRK
      ecut=0.0_SRK
      comm=0.0_SRK
      IF(ALLOCATED(thisGraph%groupIdx)) THEN
        !Compute the min/max ratio and rms difference from optimal
        lgroup=0.0_SRK
        sgroup=HUGE(1.0_SRK)
        wtSum=SUM(thisGraph%wts)
        optSize=wtSum/REAL(thisGraph%nGroups,SRK)
        DO ig=1,thisGraph%nGroups

          !Determine group size(weighted)
          wtGrp=0.0_SRK
          DO iv=thisGraph%groupIdx(ig),thisGraph%groupIdx(ig+1)-1
            wtGrp=wtGrp+thisGraph%wts(thisGraph%groupList(iv))
          ENDDO !iv
          !Determine min/max weights
          IF(wtGrp > lgroup) lgroup=wtGrp
          IF(wtGrp < sgroup) sgroup=wtGrp

          !Accumulate rms errors
          wtDif=optSize-wtGrp
          wtDif=wtDif*wtDif
          srms=srms+wtDif
        ENDDO !ig
        mmr=lgroup/sgroup
        srms=100.0_SRK*SQRT(srms)/optSize

        !Determine the total weight of cut edges, and the communication between
        !groups
        ALLOCATE(uniqueGrps(thisGraph%maxneigh))
        ALLOCATE(grpMap(thisGraph%nvert))
        !Generate a map to the groups
        DO ig=1,thisGraph%nGroups
          igstt=thisGraph%groupIdx(ig)
          igstp=thisGraph%groupIdx(ig+1)-1
          DO iv=igstt,igstp
            ivert=thisGraph%groupList(iv)
            grpMap(ivert)=ig
          ENDDO !iv
        ENDDO !ig
        !Determine cut edges and communication
        DO ig=1,thisGraph%nGroups
          igstt=thisGraph%groupIdx(ig)
          igstp=thisGraph%groupIdx(ig+1)-1
          DO iv=igstt,igstp
            ivert=thisGraph%groupList(iv)
            uniqueGrps=0
            DO in=1,thisGraph%maxneigh
              ineigh=thisGraph%neigh(in,ivert)
              IF(ineigh /= 0) THEN
                !Accumulate edges cut
                IF(.NOT. ANY(ineigh == thisGraph%groupList(igstt:igstp))) THEN
                  ecut=ecut+thisGraph%neighwts(in,ivert)
                ENDIF
                !Accumulate communication between groups
                neighGrp=grpMap(ineigh)
                IF((neighGrp /= ig) .AND. &
                   (.NOT. ANY(neighGrp == uniqueGrps))) THEN
                  uniqueGrps(in)=neighGrp
                  comm=comm+thisGraph%neighwts(in,ivert)
                ENDIF
              ENDIF
            ENDDO !in
          ENDDO !iv
        ENDDO !ig
        ecut=0.5_SRK*ecut
      ELSE
        CALL ePartitionGraph%raiseError(modName//'::'//myName// &
          ' - graph is not partitioned!')
      ENDIF
    ENDSUBROUTINE calcDecompMetrics_PartitionGraph
!
!-------------------------------------------------------------------------------
!> @brief Gets the indexed right eigenvectors of matrix A
!> @param A matrix to find eigenvectors of
!> @param lsmall true if searching for n smallest eigenvectors (false for largest)
!> @param numvecs number of eigenvectors to calculate
!> @param V array containing desired eigenvectors
!>
!> Uses SLEPc solver to find the eigenvectors of a matrix. Currently this will
!> calculate all of the eigenvectors, and return the desired ones...some effort
!> may be spent here to speed this up.
!>
    SUBROUTINE getEigenVecs(A,lsmall,numvecs,V)
      CHARACTER(LEN=*),PARAMETER :: myName='getEigenVecs'
      CLASS(MatrixType),INTENT(IN) :: A
      LOGICAL(SBK),INTENT(IN) :: lsmall
      INTEGER(SIK),INTENT(IN) :: numvecs
      CLASS(VectorType),ALLOCATABLE,INTENT(OUT) :: V(:)
#ifdef FUTILITY_HAVE_SLEPC
      INTEGER(SIK) :: n,iv
      INTEGER(SIK) :: nev,ncv,mpd
      INTEGER(SIK) :: ierr
      CLASS(VectorType),ALLOCATABLE :: Vi(:)
      TYPE(ParamType) :: vecParams

      EPS :: eps !SLEPC eigenvalue problem solver type

      !Matrix size
      n=A%n
      !For now only pass in self-communicating MPI ENV
      CALL EPSCreate(PE_COMM_SELF,eps,ierr)
      CALL EPSSetProblemType(eps,EPS_HEP,ierr)
      SELECTTYPE(A); TYPE IS(PETScMatrixType)
        !Assemble the matrix
        CALL A%assemble(ierr)
        !Set the operators (Only matrix A)
        CALL EPSSetOperators(eps, A%A,PETSC_NULL_OBJECT,ierr)
      ENDSELECT

      !Get the eigenvectors
      ALLOCATE(PETScVectorType :: V(numvecs))
      ALLOCATE(PETScVectorType :: Vi(numvecs))
      CALL vecParams%add('VectorType->n',n)
      CALL vecParams%add('VectorType->MPI_Comm_ID',PE_COMM_SELF)
      CALL vecParams%add('VectorType->nlocal',n)

      IF(lsmall) THEN !Calculate from smallest eigenvalue
        CALL EPSSetWhichEigenpairs(eps,EPS_SMALLEST_MAGNITUDE,ierr)
      ELSE !Calculate from largest eigenvalue
        CALL EPSSetWhichEigenpairs(eps,EPS_LARGEST_MAGNITUDE,ierr)
      ENDIF

      !Set problem dimensions (Solve for desired eigen pairs, may get a few extra)
      nev=numvecs
      ncv=MIN(4*nev,n)
      mpd=ncv

      CALL EPSSetDimensions(eps,nev,ncv,mpd,ierr)
      CALL EPSSolve(eps,ierr)
      DO iv=1,numvecs
      SELECTTYPE(v1 => V(iv)); TYPE IS(PETScVectorType)
          SELECTTYPE(v2 => Vi(iv)); TYPE IS(PETScVectorType)
            CALL v1%init(vecParams)
            CALL v2%init(vecParams)
            CALL v1%assemble(ierr)
            CALL v2%assemble(ierr)
            CALL EPSGetEigenVector(eps,iv-1,v1%b,v2%b,ierr)
            CALL v2%clear()
          ENDSELECT
        ENDSELECT
      ENDDO !iv
      CALL vecParams%clear()
      DEALLOCATE(Vi)

      !Destroy
      CALL EPSDestroy(eps,ierr)
#else
      CALL ePartitionGraph%raiseError(modName//'::'//myName// &
        ' - eigenvector solves only available through SLEPC!')
#endif
    ENDSUBROUTINE getEigenVecs
!
!-------------------------------------------------------------------------------
!> @brief Given the eigenvectors of the inertial matrix (RIB method), determine
!>        the order
!> @param coord the coordinates to determine distance
!> @param evecs the eigenvectors of the inertial matrix
!> @param Order the order of the vertices
!>
#ifdef FUTILITY_HAVE_SLEPC
    RECURSIVE SUBROUTINE recursiveEigenOrder(evecs, Order, lpos, dot)
      Type(PETScVectorType),INTENT(INOUT) :: evecs(:)
      INTEGER(SIK),INTENT(INOUT) :: Order(:)
      REAL(SRK),INTENT(INOUT),OPTIONAL :: dot(:,:)
      LOGICAL(SBK),INTENT(IN),OPTIONAL :: lpos
      LOGICAL(SBK) :: lposc
      INTEGER(SIK) :: dim,numvecs,nvert,numeq,iv,jv,idim,ierr
      INTEGER(SIK),ALLOCATABLE :: indOrder(:),oc(:)
      REAL(SRK),ALLOCATABLE :: d(:),dc(:),ec(:)
      TYPE(PETScVectorType),ALLOCATABLE :: evc(:)
      TYPE(ParamType) :: mparams

      lposc=.TRUE.
      IF(PRESENT(lpos)) lposc=lpos
      numvecs=SIZE(evecs,DIM=1)
      nvert = SIZE(Order)

      !If dot is present use this to construct order
      ALLOCATE(d(nvert))
      IF(PRESENT(dot)) THEN
        dim=SIZE(dot,DIM=1)

        ALLOCATE(ec(dim))
        CALL evecs(1)%getall(ec,ierr)
        d=0.0_SRK
        DO iv=1,nvert
          d(iv)=SUM(ec(1:dim)*dot(1:dim,iv))
        ENDDO !iv
        DEALLOCATE(ec)
      ELSE
        !Use the eigenvector itself for sorting
        dim=0
        CALL evecs(1)%getall(d,ierr)
      ENDIF

      !Consistent sorting
      IF(.NOT. lposc) d=-d

      !Sort by value of d
      ALLOCATE(indOrder(nvert))
      ALLOCATE(oc(nvert))
      ALLOCATE(dc(nvert))
      oc=Order
      DO iv=1,nvert
        indOrder(iv)=iv
      ENDDO !iv
      CALL sort(d,indOrder,.TRUE.)
      DO iv=1,nvert
        Order(iv)=oc(indOrder(iv))
      ENDDO !iv
      IF(PRESENT(dot)) THEN
        !Reorder each element for the dot-item
        DO idim=1,dim
          dc=dot(idim,1:nvert)
          DO iv=1,nvert
            dot(idim,iv)=dc(indOrder(iv))
          ENDDO !iv
        ENDDO !idim
      ELSE
        !Reorder each subsequent eigenvector
        ALLOCATE(ec(nvert))
        DO iv=2,numvecs
          CALL evecs(iv)%getall(ec,ierr)
          DO jv=1,nvert
            dc(jv)=ec(indOrder(jv))
          ENDDO !jv
          CALL evecs(iv)%setall_array(dc,ierr)
        ENDDO !iv
        DEALLOCATE(ec)
      ENDIF
      DEALLOCATE(dc)
      DEALLOCATE(oc)
      DEALLOCATE(indOrder)

      !If more than 1 vector, then check for ties
      IF(numvecs > 1) THEN
        iv=0
        IF(.NOT. PRESENT(dot)) THEN
          !Need to send only a slice of the reordered eigenvectors
          ALLOCATE(evc(numvecs-1))
          ALLOCATE(ec(nvert))
          CALl mparams%add('VectorType -> n', 1)
          CALL mparams%add('VectorType -> MPI_COMM_ID', PE_COMM_SELF)
          CALL mparams%add('VectorType -> nlocal',1)
        ENDIF
        DO WHILE(iv < nvert)
          iv=iv+1
          numeq=0
          DO jv=iv+1,nvert
            IF(d(jv) .APPROXEQA. d(iv)) THEN
              numeq=numeq+1
            ELSE
              EXIT
            ENDIF
          ENDDO !jv

          !Recursive sort if ties
          IF(numeq >= 1) THEN
            IF(PRESENT(dot)) THEN
              !Use dot-product approach on eigen-vectors
              IF(d(iv) .APPROXGE. 0.0_SRK) THEN
                CALL recursiveEigenOrder(evecs(2:),Order(iv:iv+numeq),.TRUE., &
                  dot(1:dim, iv:iv+numeq))
              ELSE
                CALL recursiveEigenOrder(evecs(2:),Order(iv:iv+numeq),.FALSE., &
                  dot(1:dim, iv:iv+numeq))
              ENDIF
            ELSE
              !Get only the range of the eigenvectors which is needed
              !for the next section
              CALL mparams%set('VectorType -> n', numeq+1)
              CALL mparams%set('VectorType -> nlocal', numeq+1)
              DO jv=2,numvecs
                CALL evecs(jv)%getRange(iv,iv+numeq,ec(iv:iv+numeq),ierr)
                CALL evc(jv-1)%init(mparams)
                CALL evc(jv-1)%setall_array(ec(iv:iv+numeq))
              ENDDO !iv
              IF(d(iv) .APPROXGE. 0.0_SRK) THEN
                CALL recursiveEigenOrder(evc,Order(iv:iv+numeq),.TRUE.)
              ELSE
                CALL recursiveEigenOrder(evc,Order(iv:iv+numeq),.FALSE.)
              ENDIF
              DO jv=1,numvecs-1
                CALL evc(jv)%clear()
              ENDDO !iv
            ENDIF
            iv=iv+numeq
          ENDIF
        ENDDO !iv < nvert

        IF(.NOT. PRESENT(dot)) THEN
          DEALLOCATE(evc)
          DEALLOCATE(ec)
          CALL mparams%clear()
        ENDIF
      ENDIF
      DEALLOCATE(d)
    ENDSUBROUTINE recursiveEigenOrder
#endif
!
!-------------------------------------------------------------------------------
!> @brief Determine the desired number of groups for each bisection in the
!>        recursive bisection methods.
!> @param thisGraph the graph which is being bisected
!> @param wg1 the desired weight of group 1
!> @param wtSum the sum of weights in the graph
!>
    SUBROUTINE detGroupSize(thisGraph,wg1,wtSum)
      CLASS(PartitionGraphType),INTENT(IN) :: thisGraph
      REAL(SRK),INTENT(OUT) :: wg1,wtSum
      INTEGER(SIK) :: ng,ng1

      !Determine desired number of subgroups for each group
      ng=thisGraph%nGroups
      ng1=ng/2    !Group 1

      !Determine desired weight of each group
      wtSum=SUM(thisGraph%wts)
      wg1=wtSum*REAL(ng1,SRK)/REAL(ng,SRK)  !Group 1
    ENDSUBROUTINE detGroupSize
!
!-------------------------------------------------------------------------------
!> @brief Partitioning process for the recusive class of partitioning methods
!>        Routine added to reduce code replication in all these routines
!> @param thisGraph The graph which is being partitioned
!> @param Order "Ordering" assigned to graph by recusive method
!> @param nv1 The number of vertices in L1
!> @param cw1 The weight of group 1
!> @param wtSum The total weight of the graph
!>
    RECURSIVE SUBROUTINE recursivePartitioning(thisGraph,Order,nv1,cw1,wtSum)
      CLASS(PartitionGraphType),INTENT(INOUT) :: thisGraph
      INTEGER(SIK),POINTER,INTENT(INOUT) :: Order(:)
      INTEGER(SIK),INTENT(INOUT) :: nv1
      REAL(SRK),INTENT(INOUT) :: cw1
      REAL(SRK),INTENT(IN) :: wtSum
      INTEGER(SIK) :: iv,ng,ng1,ng2
      TYPE(PartitionGraphType) :: sg1,sg2
      INTEGER(SIK),POINTER :: L1(:)
      INTEGER(SIK),POINTER :: L2(:)

      !Assign L1,L2
      L1 => Order(1:nv1)
      L2 => Order(nv1+1:thisGraph%nvert)

      !Refine the 2 groups
      CALL thisGraph%refine(L1,L2)

      !Redistribute the number of groups for each subgraph
      ng=thisGraph%nGroups
      ng1=MAX(1,NINT(ng*cw1/wtSum))
      ng2=ng-ng1

      IF(ng1 > 1) THEN
        !Generate subgraph if further decomposition is needed
        CALL thisGraph%subgraph(L1,ng1,sg1)
        !Recursively partition
        CALL sg1%partition()
      ENDIF

      IF(ng2 > 1) THEN
        !Generate subgraph if further decomposition is needed
        CALL thisGraph%subgraph(L2,ng2,sg2)
        !Recursively partition
        CALL sg2%partition()
      ENDIF

      !Allocate group lists on parent type
      ALLOCATE(thisGraph%groupIdx(ng+1))
      ALLOCATE(thisGraph%groupList(thisGraph%nvert))

      IF(ng1 > 1) THEN
        !Pull groups from first subgraph
        thisGraph%groupIdx(1:ng1+1)=sg1%groupIdx(1:ng1+1)
        DO iv=1,sg1%nvert
          thisGraph%groupList(iv)=L1(sg1%groupList(iv))
        ENDDO !iv
        CALL sg1%clear()
      ELSE
        thisGraph%groupIdx(1)=1
        thisGraph%groupIdx(2)=nv1+1
        DO iv=1,nv1
          thisGraph%groupList(iv)=L1(iv)
        ENDDO !iv
      ENDIF
      IF(ng2 > 1) THEN
        !Pull groups from second subgraph
        thisGraph%groupIdx(ng1+1:ng1+ng2+1)=nv1+sg2%groupIdx(1:ng2+1)
        DO iv=1,sg2%nvert
          thisGraph%groupList(iv+nv1)=L2(sg2%groupList(iv))
        ENDDO !iv
        CALL sg2%clear()
      ELSE
        thisGraph%groupIdx(ng1+1)=nv1+1
        thisGraph%groupIdx(ng1+2)=thisGraph%nvert+1
        DO iv=1,thisGraph%nvert-nv1
          thisGraph%groupList(iv+nv1)=L2(iv)
        ENDDO !iv
      ENDIF
      NULLIFY(L1)
      NULLIFY(L2)
    ENDSUBROUTINE recursivePartitioning
!
ENDMODULE PartitionGraph
