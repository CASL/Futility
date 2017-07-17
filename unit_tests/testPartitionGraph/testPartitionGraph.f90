!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
PROGRAM testPartitionGraph
#include "UnitTest.h"
  USE UnitTest
  USE IntrType
  USE Strings
  USE ExceptionHandler
  USE ParameterLists
  USE VectorTypes
  USE MatrixTypes
  USE PartitionGraph

  IMPLICIT NONE

  TYPE(PartitionGraphType) :: testPG,testSG
  TYPE(ParamType) :: params,tparams
  TYPE(ParamType) :: refInitParams,refG1Params,refG2Params,refG3Params
  TYPE(StringType),ALLOCATABLE :: strList(:)
  TYPE(ExceptionHandlerType),POINTER :: e

#ifdef FUTILITY_HAVE_SLEPC
#include <finclude/petsc.h>
#include <finclude/slepc.h>
#undef IS
  PetscErrorCode  :: ierr
#endif

  CREATE_TEST('PartitionGraph')

#ifdef FUTILITY_HAVE_SLEPC
  CALL PETScInitialize(PETSC_NULL_CHARACTER,ierr)
  CALL SlepcInitialize(PETSC_NULL_CHARACTER,ierr)
#endif
  CALL setupTest()

  ALLOCATE(e)
  CALL e%setStopOnError(.FALSE.)
  CALL e%setQuietMode(.TRUE.)
  CALL ePartitionGraph%addSurrogate(e)

  REGISTER_SUBTEST('Initialization',testInit)
  REGISTER_SUBTEST('Clear',testClear)
  REGISTER_SUBTEST('Subgraph',testSubgraph)
  REGISTER_SUBTEST('Recursive Expansion Bisection',testREB)
#ifdef FUTILITY_HAVE_SLEPC
  REGISTER_SUBTEST('Recursive Spectral Bisection',testRSB)
  REGISTER_SUBTEST('Multi-method',testMulti)
#endif
  REGISTER_SUBTEST('Kernighan-Lin',testKL)
  REGISTER_SUBTEST('Metrics Calculation',testMetrics)

#ifdef FUTILITY_HAVE_PETSC
  CALL SlepcFinalize(ierr)
  CALL PetscFinalize(ierr)
#endif
  CALL clearTest()

  FINALIZE_TEST()
!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
    !Test initialization process
    SUBROUTINE testInit()
      CHARACTER(LEN=EXCEPTION_MAX_MESG_LENGTH) :: msg,refmsg
      LOGICAL(SBK) :: bool
      INTEGER(SIK) :: refcond(1)
      INTEGER(SIK),ALLOCATABLE :: refwts(:),refneigh(:,:),refnwts(:,:),refunwt(:,:)
      INTEGER(SIK),ALLOCATABLE :: refd(:)
      REAL(SRK),ALLOCATABLE :: refCoord(:,:)
      TYPE(StringType) :: refAlgNames(2),tmpAlgNames(3)

      !Copy parameter list and get reference data
      tparams=refInitParams !Copy good parameter list
      CALL tparams%get('PartitionGraph -> wts', refwts)
      CALL tparams%get('PartitionGraph -> neigh', refneigh)
      CALL tparams%get('PartitionGraph -> neighwts', refnwts)
      CALL tparams%get('PartitionGraph -> coord', refCoord)

      !Test invalid number of groups
      CALL tparams%set('PartitionGraph -> nGroups', 0)
      CALL testPG%initialize(tparams)

      msg=e%getLastMessage()
      refmsg='#### EXCEPTION_ERROR #### - PartitionGraph::'// &
        'init_PartitionGraph - invalid number of partitioning groups!'
      ASSERT(msg == refmsg, '%init(...) invalid nGroups')
      FINFO() 'Reference: ',refmsg
      FINFO() 'Test:      ',msg
      CALL tparams%set('PartitionGraph -> nGroups',3)

      !Test invalid number of vertices
      CALL tparams%set('PartitionGraph -> nvert',0)
      CALL testPG%initialize(tparams)
      msg=e%getLastMessage()
      refmsg='#### EXCEPTION_ERROR #### - PartitionGraph::'// &
        'init_PartitionGraph - invalid number of vertices!'
      ASSERT(msg == refmsg, '%init(...) invalid nvert')
      FINFO() 'Reference: ',refmsg
      FINFO() 'Test:      ',msg
      CALL tparams%set('PartitionGraph -> nvert',6)

      !Test incorrectly sized coordinate matrix
      CALL tparams%set('PartitionGraph -> coord', RESHAPE((/1.0_SRK,1.0_SRK/),(/2,1/)))
      CALL testPG%initialize(tparams)
      msg=e%getLastMessage()
      refmsg='#### EXCEPTION_ERROR #### - PartitionGraph::'// &
        'init_PartitionGraph - coordinate matrix is incorrect size!'
      ASSERT(msg == refmsg, '%init(...) wrong coord size')
      FINFO() refmsg
      FINFO() msg
      CALL tparams%set('PartitionGraph -> coord',refCoord)

      !Test incorrectly sized neighbor matrix
      CALL tparams%set('PartitionGraph -> neigh',RESHAPE((/1,1/),(/2,1/)))
      CALL tparams%set('PartitionGraph -> neighwts',refnwts(1:2,:))
      CALL testPG%initialize(tparams)
      msg=e%getLastMessage()
      refmsg='#### EXCEPTION_ERROR #### - PartitionGraph::'// &
        'init_PartitionGraph - neighbor matrix is incorrect size!'
      ASSERT(msg == refmsg, '%init(...) wrong neigh size')
      FINFO() refmsg
      FINFO() msg

      !Test invalid neighbor matrix
      CALL tparams%set('PartitionGraph -> neigh', &
        RESHAPE((/-1,1,1,1,-1,1,1,1,-1,1,1,1,-1,1,1,1,-1,1,1,1,-1,1,1,1/), &
          (/4,6/)))
      CALL tparams%set('PartitionGraph -> neighwts',refnwts)
      CALL testPG%initialize(tparams)
      msg=e%getLastMessage()
      refmsg='#### EXCEPTION_ERROR #### - PartitionGraph::'// &
        'init_PartitionGraph - invalid neighbor matrix!'
      ASSERT(msg == refmsg, '%init(...) invalid neigh')
      FINFO() refmsg
      FINFO() msg
      CALL tparams%set('PartitionGraph -> neigh',refneigh)

      !Test incorrectly sized edge weight matrix
      CALL tparams%set('PartitionGraph -> neighwts',RESHAPE((/3,1/),(/2,1/)))
      CALL testPG%initialize(tparams)
      msg=e%getLastMessage()
      refmsg='#### EXCEPTION_ERROR #### - PartitionGraph::'// &
        'init_PartitionGraph - input edge weights matrix is incorrect size!'
      ASSERT(msg == refmsg, '%init(...) wrong neighwts size')
      FINFO() refmsg
      FINFO() msg

      !Test invalid edge weight matrix
      CALL tparams%set('PartitionGraph -> neighwts', &
        RESHAPE((/-1,1,1,1,-1,1,1,1,-1,1,1,1,-1,1,1,1,-1,1,1,1,-1,1,1,1/), &
          (/4,6/)))
      CALL testPG%initialize(tparams)
      msg=e%getLastMessage()
      refmsg='#### EXCEPTION_ERROR #### - PartitionGraph::'// &
        'init_PartitionGraph - edge weights must be > 0!'
      ASSERT(msg == refmsg, '%init(...) invalid neighwts')
      FINFO() refmsg
      FINFO() msg
      CALL tparams%set('PartitionGraph -> neighwts',refnwts)

      !Test incorrectly sized weight array
      CALL tparams%set('PartitionGraph -> wts', (/1/))
      CALL testPG%initialize(tparams)
      msg=e%getLastMessage()
      refmsg='#### EXCEPTION_ERROR #### - PartitionGraph::'// &
        'init_PartitionGraph - input vertex weights array is incorrect size!'
      ASSERT(msg == refmsg, '%init(...) wrong wts size')
      FINFO() refmsg
      FINFO() msg

      !Test invalid wts array
      CALL tparams%set('PartitionGraph -> wts',(/-1,1,1,1,1,1/))
      CALL testPG%initialize(tparams)
      msg=e%getLastMessage()
      refmsg='#### EXCEPTION_ERROR #### - PartitionGraph::'// &
        'init_PartitionGraph - vertex weights must be > 0!'
      ASSERT(msg == refmsg, '%init(...) invalid wts')
      FINFO() refmsg
      FINFO() msg
      CALL tparams%set('PartitionGraph -> wts',refwts)

      !Test invalid(mssing) Conditions
      CALL tparams%remove('PartitionGraph -> Conditions')
      CALL testPG%initialize(tparams)
      msg=e%getLastMessage()
      refmsg='#### EXCEPTION_ERROR #### - PartitionGraph::'// &
        'init_PartitionGraph - Conditions must be specified if more than 1'// &
        ' algorithm is to be used!'
      ASSERT(msg == refmsg, '%init(...) missing cond')
      FINFO() refmsg
      FINFO() msg

      !Test wrong sized Conditions
      CALL tparams%add('PartitionGraph -> Conditions', (/1,2/))
      CALL testPG%initialize(tparams)
      msg=e%getLastMessage()
      refmsg='#### EXCEPTION_ERROR #### - PartitionGraph::'// &
        'init_PartitionGraph - Wrong number of conditions specified!'
      ASSERT(msg == refmsg, '%init(...) wrong # cond')
      FINFO() refmsg
      FINFO() msg

      !Invalid conditions (1)
      CALL tparams%set('PartitionGraph -> Conditions', (/-1/))
      CALL testPG%initialize(tparams)
      msg=e%getLastMessage()
      refmsg='#### EXCEPTION_ERROR #### - PartitionGraph::'// &
        'init_PartitionGraph - partitioning algorithm size conditions are '// &
        'invalid (< 0)!'
      ASSERT(msg == refmsg, '%init(...) invalid cond')
      FINFO() refmsg
      FINFO() msg

      !Invalid conditions (2)
      tmpAlgNames(1:2)=refAlgNames
      tmpAlgNames(3)='Recursive Spectral Bisection'
      CALL tparams%set('PartitionGraph -> Algorithms', tmpAlgNames)
      CALL tparams%set('PartitionGraph -> Conditions', (/1000,10000/))
      CALL testPG%initialize(tparams)
      msg=e%getLastMessage()
      refmsg='#### EXCEPTION_ERROR #### - PartitionGraph::'// &
        'init_PartitionGraph - partitioning algorithm size conditions are '// &
        'invalid!'
      ASSERT(msg == refmsg, '%init(...) invalid cond')
      FINFO() refmsg
      FINFO() msg
      CALL tparams%set('PartitionGraph -> Algorithms',refAlgNames)
      CALL tparams%set('PartitionGraph -> Conditions', (/3/))

      !Test bad function name
      CALL tparams%remove('PartitionGraph -> wts')
      CALL tparams%remove('PartitionGraph -> neighwts')
      CALL tparams%remove('PartitionGraph -> Conditions')
      tmpAlgNames(1)='BADFUNCNAME'
      CALL tparams%set('PartitionGraph -> Algorithms',tmpAlgNames(1:1))
      CALL testPG%initialize(tparams)
      msg=e%getLastMessage()
      refmsg='#### EXCEPTION_ERROR #### - PartitionGraph::'// &
        'init_PartitionGraph - Partitioning algorithm "BADFUNCNAME" not recognized!'
      ASSERT(msg == refmsg, '%init(...) invalid cond')
      FINFO() refmsg
      FINFO() msg

      !Test (unweighted, single algorithm) initialization
      CALL tparams%set('PartitionGraph -> Algorithms',refAlgNames(1:1))
      CALL testPG%initialize(tparams)

      ALLOCATE(refd(6))
      ALLOCATE(refunwt(4,6))
      refd=(/1, 3, 2, 2, 3, 1/)
      refunwt=RESHAPE((/1, 0, 0, 0, &
                        1, 1, 1, 0, &
                        1, 1, 0, 0, &
                        1, 1, 0, 0, &
                        1, 1, 1, 0, &
                        1, 0, 0, 0/),(/4,6/))
      ASSERT(testPG%isInit, '%init(...)%isInit')
      ASSERT(testPG%nvert == 6, '%init(...)%nvert')
      ASSERT(testPG%dim == 2, '%init(...)%dim')
      ASSERT(testPG%nGroups == 3, '%init(...)%nGroups')
      ASSERT(testPG%maxneigh == 4, '%init(...)%maxneigh')
      ASSERT(testPG%nPart == 1, '%init(...)%nPart')
      bool=(SIZE(testPG%wts) == 6)
      ASSERT(bool,'%init(...)%SIZE(wts)')
      bool=ALL(testPG%wts == 1)
      ASSERT(bool,'%init(...)%wts')
      bool=((SIZE(testPG%neigh,DIM=1) == 4) .AND. &
            (SIZE(testPG%neigh,DIM=2) == 6))
      ASSERT(bool,'%init(...)%SIZE(neigh)')
      bool=ALL(testPG%neigh == refneigh)
      ASSERT(bool,'%init(...)%neigh')
      bool=((SIZE(testPG%neighwts,DIM=1) == 4) .AND. &
            (SIZE(testPG%neighwts,DIM=2) == 6))
      ASSERT(bool,'%init(...)%SIZE(neighwts)')
      bool=ALL(testPG%neighwts == refunwt)
      ASSERT(bool,'%init(...)%neighwts')
      bool=((SIZE(testPG%coord,DIM=1) == 2) .AND. &
            (SIZE(testPG%coord,DIM=2) == 6))
      ASSERT(bool,'%init(...)%SIZE(coord)')
      bool=ALL(testPG%coord == refCoord)
      ASSERT(bool,'%init(...)%coord')
      bool=(SIZE(testPG%partitionAlgArry) == 1)
      ASSERT(bool,'%init(...)%SIZE(partitionAlgArry)')
      bool=ALL(testPG%d == refd)
      ASSERT(bool,'%init(...)%d')
      CALL testPG%clear()

      !Test full initializaton (weighted, multiple algorithms)
      tparams=refInitParams
      CALL testPG%initialize(tparams)
      ASSERT(testPG%isInit, '%init(...)%isInit')
      ASSERT(testPG%nvert == 6, '%init(...)%nvert')
      ASSERT(testPG%dim == 2, '%init(...)%dim')
      ASSERT(testPG%nGroups == 3, '%init(...)%nGroups')
      ASSERT(testPG%maxneigh == 4, '%init(...)%maxneigh')
      ASSERT(testPG%nPart == 2, '%init(...)%nPart')
      FINFO() testPG%nPart
      bool=(SIZE(testPG%wts) == 6)
      ASSERT(bool,'%init(...)%SIZE(wts)')
      bool=ALL(testPG%wts == refwts)
      ASSERT(bool,'%init(...)%wts')
      bool=((SIZE(testPG%neigh,DIM=1) == 4) .AND. &
            (SIZE(testPG%neigh,DIM=2) == 6))
      ASSERT(bool,'%init(...)%SIZE(neigh)')
      bool=ALL(testPG%neigh == refneigh)
      ASSERT(bool,'%init(...)%neigh')
      bool=((SIZE(testPG%neighwts,DIM=1) == 4) .AND. &
            (SIZE(testPG%neighwts,DIM=2) == 6))
      ASSERT(bool,'%init(...)%SIZE(neighwts)')
      bool=ALL(testPG%neighwts == refnwts)
      ASSERT(bool,'%init(...)%neighwts')
      bool=((SIZE(testPG%coord,DIM=1) == 2) .AND. &
            (SIZE(testPG%coord,DIM=2) == 6))
      ASSERT(bool,'%init(...)%SIZE(coord)')
      bool=ALL(testPG%coord == refCoord)
      ASSERT(bool,'%init(...)%coord')
      bool=(SIZE(testPG%partitionAlgArry) == 2)
      ASSERT(bool,'%init(...)%SIZE(partitionAlgArry)')
      bool=ALL(testPG%d == refd)
      ASSERT(bool,'%init(...)%d')

      !Clear and deallocate
      CALL testPG%clear()
      CALL tparams%clear()
      DEALLOCATE(refwts)
      DEALLOCATE(refneigh)
      DEALLOCATE(refnwts)
      DEALLOCATE(refunwt)
      DEALLOCATE(refd)
      DEALLOCATE(refCoord)
    ENDSUBROUTINE testInit
!
!-------------------------------------------------------------------------------
    SUBROUTINE testClear()
      LOGICAL(SBK) :: bool
      INTEGER(SIK) :: refwts(6),refneigh(4,6),refnwts(4,6),refcond(1)
      REAL(SRK) :: refCoord(2,6)
      TYPE(StringType) :: refAlgNames(2),tmpAlgNames(3)

      !Generate parameter list
      CALL params%add('PartitionGraph -> nvert', 6)
      CALL params%add('PartitionGraph -> nGroups', 3)
      refneigh=RESHAPE((/2, 0, 0, 0, &
                         1, 3, 4, 0, &
                         2, 5, 0, 0, &
                         2, 5, 0, 0, &
                         3, 4, 6, 0, &
                         5, 0, 0, 0/),(/4,6/))
      CALL params%add('PartitionGraph -> neigh', refneigh)
      refwts=(/1,2,3,2,1,1/)
      CALL params%add('PartitionGraph -> wts', refwts)
      refnwts=RESHAPE((/1, 0, 0, 0, &
                        1, 2, 3, 0, &
                        2, 1, 0, 0, &
                        3, 1, 0, 0, &
                        1, 1, 1, 0, &
                        1, 0, 0, 0/), (/4,6/))
      CALL params%add('PartitionGraph -> neighwts', refnwts)
      refAlgNames(1)='Recursive Expansion Bisection'
      refAlgNames(2)='Recursive Spectral Bisection'
      CALL params%add('PartitionGraph -> Algorithms', refAlgNames)
      refcond=(/3/)
      CALL params%add('PartitionGraph -> Conditions', refcond)
      refCoord=RESHAPE((/-1.0_SRK, -1.0_SRK, &
                         -1.0_SRK,  0.0_SRK, &
                          0.0_SRK,  0.0_SRK, &
                         -1.0_SRK,  1.0_SRK, &
                          0.0_SRK,  1.0_SRK, &
                          1.0_SRK,  1.0_SRK/),(/2,6/))
      CALL params%add('PartitionGraph -> coord',refCoord)

      CALL testPG%initialize(params)
      CALL testPG%clear()
      CALL params%clear()
      ASSERT(testPG%nvert == 0,'%clear(...)%nvert')
      ASSERT(testPG%dim == 0,'%clear(...)%nvert')
      ASSERT(testPG%maxneigh == 0,'%clear(...)%nvert')
      ASSERT(testPG%nGroups == 0,'%clear(...)%nvert')
      ASSERT(testPG%nPart == 0,'%clear(...)%nvert')
      bool=(.NOT. ALLOCATED(testPG%groupIdx))
      ASSERT(bool,'%clear(...)%groupIdx')

      bool=(.NOT. ALLOCATED(testPG%groupList))
      ASSERT(bool,'%clear(...)%groupList')

      bool=(.NOT. ALLOCATED(testPG%wts))
      ASSERT(bool,'%clear(...)%wts')

      bool=(.NOT. ALLOCATED(testPG%cond))
      ASSERT(bool,'%clear(...)%cond')

      bool=(.NOT. ALLOCATED(testPG%neigh))
      ASSERT(bool,'%clear(...)%neigh')

      bool=(.NOT. ALLOCATED(testPG%neighwts))
      ASSERT(bool,'%clear(...)%neighwts')

      bool=(.NOT. ALLOCATED(testPG%coord))
      ASSERT(bool,'%clear(...)%coord')

      bool=(.NOT. ALLOCATED(testPG%partitionAlgArry))
      ASSERT(bool,'%clear(...)%partitionAlgArry')
    ENDSUBROUTINE testClear
!
!-------------------------------------------------------------------------------
    SUBROUTINE testSubgraph()
      LOGICAL(SBK) :: bool
      INTEGER(SIK) :: refneigh(4,6),refList(4)
      REAL(SRK) :: refCoord(2,6)
      TYPE(StringType) :: refAlgNames(1)

      !Generate parameter list
      CALL params%add('PartitionGraph -> nvert', 6)
      CALL params%add('PartitionGraph -> nGroups', 3)
      refneigh=RESHAPE((/2, 0, 0, 0, &
                         1, 3, 4, 0, &
                         2, 5, 0, 0, &
                         2, 5, 0, 0, &
                         3, 4, 6, 0, &
                         5, 0, 0, 0/),(/4,6/))
      CALL params%add('PartitionGraph -> neigh', refneigh)
      refAlgNames(1)='Recursive Expansion Bisection'
      CALL params%add('PartitionGraph -> Algorithms', refAlgNames)
      refCoord=RESHAPE((/-1.0_SRK, -1.0_SRK, &
                         -1.0_SRK,  0.0_SRK, &
                          0.0_SRK,  0.0_SRK, &
                         -1.0_SRK,  1.0_SRK, &
                          0.0_SRK,  1.0_SRK, &
                          1.0_SRK,  1.0_SRK/),(/2,6/))
      CALL params%add('PartitionGraph -> coord',refCoord)
      CALL testPG%initialize(params)

      !Out of order list with 0s (idealy these should only come at the end,
      !but the setup routine should handle this 'worst' case)
      refList=(/4, 3, 6, 5/)
      CALL testPG%subgraph(refList,1,testSG)

      !Test
      ASSERT(testSG%nvert == 4, '%subgraph(...)%nvert')
      ASSERT(testSG%dim == 2, '%subgraph(...)%dim')
      ASSERT(testSG%maxneigh == 4, '%subgraph(...)%maxneigh')
      ASSERT(testSG%nGroups == 1 , '%subgraph(...)%nGroups')
      ASSERT(ALL(testSG%wts == 1) , '%subgraph(...)%wts')
      bool=ALL(testSG%d == (/1, 1, 1, 3/))
      ASSERT(bool,'%subgraph(...)%d')
      bool=ALL(testSG%neigh == RESHAPE((/0, 4, 0, 0, &
                                         0, 4, 0, 0, &
                                         4, 0, 0, 0, &
                                         2, 1, 3, 0/),(/4,4/)))
      ASSERT(bool, '%subgraph(...)%neigh')
      bool=ALL(testSG%neighwts == RESHAPE((/0, 1, 0, 0, &
                                            0, 1, 0, 0, &
                                            1, 0, 0, 0, &
                                            1, 1, 1, 0/),(/4,4/)))
      ASSERT(bool, '%subgraph(...)%neighwts')
      bool=ALL(testSG%coord == RESHAPE((/-1.0_SRK, 1.0_SRK, &
                                          0.0_SRK, 0.0_SRK, &
                                          1.0_SRK, 1.0_SRK, &
                                          0.0_SRK, 1.0_SRK/),(/2,4/)))
      ASSERT(bool, '%subgraph(...)%coord')

      !Clear
      CALL params%clear()
      CALL testPG%clear()
      CALL testSG%clear()
    ENDSUBROUTINE testSubgraph
!
!-------------------------------------------------------------------------------
    SUBROUTINE testREB()
      LOGICAL(SBK) :: bool
      INTEGER(SIK) :: ig,iv
      INTEGER(SIK) :: refneigh(4,6),refGrpIdx(3),refGrpList(6)
      REAL(SRK) :: refCoord(2,6)
      TYPE(StringType) :: AlgName,refAlgNames(1)
      INTEGER(SIK),ALLOCATABLE :: grpIdx(:),grpList(:)

      !Test very simple 2-group, 2D, uniform-weighted graph
      !Decomposition should look like:
      ! 2 2 2
      ! 1 1
      ! 1
      CALL testPG%initialize(refG1Params)

      !Call partition algorithm
      CALL testPG%partition()

      !Reference lists
      refGrpIdx=(/1,4,7/)
      refGrpList=(/1,2,3,4,5,6/)

      DO ig=1,3
        bool=(testPG%groupIdx(ig) == refGrpIdx(ig))
        ASSERT(bool,'%partition(REB)%groupIdx')
      ENDDO !ig

      DO iv=1,6
        bool=(testPG%groupList(iv) == refGrpList(iv))
        ASSERT(bool,'%partition(REB)%groupList')
      ENDDO !iv

      CALL testPG%clear()

      !Test larger 3-group problem (even divisions, unweighted)
      !Decomposition should look like
      ! 3 3 2 2 2 2
      ! 3 3 3 2 2 2
      ! 3 3 3 2
      ! 1 1 1 1
      ! 1 1
      ! 1 1
      !Initialize
      CALL testPG%initialize(refG2Params)
      !Partition
      CALL testPG%partition()

      !Setup references
      ALLOCATE(grpIdx(4))
      ALLOCATE(grpList(24))
      grpIdx=(/1, 9, 17, 25/)
      grpList=(/ 1,  2,  4,  3,  5,  6,  7,  8, &
                24, 18, 17, 23, 22, 16, 12, 21, &
                 9, 10, 11, 13, 14, 15, 19, 20/)
      !Test
      DO ig=1,4
        bool=(testPG%groupIdx(ig) == GrpIdx(ig))
        ASSERT(bool,'%partition(REB)%groupIdx')
      ENDDO !ig

      DO iv=1,24
        bool=(testPG%groupList(iv) == GrpList(iv))
        ASSERT(bool,'%partition(REB)%groupList')
        FINFO() 'Ref :',grpList(iv)
        FINFO() 'Test:',testPG%groupList(iv)
      ENDDO !iv

      DEALLOCATE(grpIdx)
      DEALLOCATE(grpList)

      CALL testPG%clear()

      !Test larger 3-group problem (uneven divisions, weighted)
      !Decomposition should look like
      ! 3 3 3 2 2 2
      ! 3 3 3 2 2 2
      ! 3 3 2 2 2 2
      ! 1 1 1 1
      ! 1 1 1
      ! 1 1 1
      !Initialize
      CALL testPG%initialize(refG3Params)
      !Partition
      CALL testPG%partition()

      !Setup references
      ALLOCATE(grpIdx(4))
      ALLOCATE(grpList(28))
      grpIdx=(/1, 11, 21, 29/)
      grpList=(/ 1,  2,  3,  6,  5,  4,  7,  8,  9, 10, &
                16, 15, 22, 21, 28, 27, 14, 20, 26, 13, &
                11, 12, 17, 18, 19, 23, 24, 25/)

      !Test
      DO ig=1,4
        bool=(testPG%groupIdx(ig) == GrpIdx(ig))
        ASSERT(bool,'%partition(REB)%groupIdx')
      ENDDO !ig

      DO iv=1,28
        bool=(testPG%groupList(iv) == GrpList(iv))
        ASSERT(bool,'%partition(REB)%groupList')
        FINFO() 'Ref :',grpList(iv)
        FINFO() 'Test:',testPG%groupList(iv)
      ENDDO !iv

      DEALLOCATE(grpIdx)
      DEALLOCATE(grpList)

      CALL testPG%clear()
    ENDSUBROUTINE testREB
!
!-------------------------------------------------------------------------------
    SUBROUTINE testRSB()
      LOGICAL(SBK) :: bool
      INTEGER(SIK) :: ig,iv
      INTEGER(SIK),ALLOCATABLE :: grpIdx(:),grpList(:)
      TYPE(StringType) :: algName(1)
      algName(1)='Recursive Spectral Bisection'

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      !Decomposition should look like:
      ! 1 1 1
      ! 2 2
      ! 2
      tparams=refG1Params
      CALL tparams%set('PartitionGraph -> Algorithms',algName)

      !initialize
      CALL testPG%initialize(tparams)
      CALL tparams%clear()

      !Partition
      CALL testPG%partition()

      !Setup reference
      ALLOCATE(grpIdx(3))
      ALLOCATE(grpList(6))
      grpIdx=(/1,4,7/)
      grpList=(/6,5,4,1,2,3/)

      !Test
      DO ig=1,testPG%nGroups+1
        bool=(testPG%groupIdx(ig) == GrpIdx(ig))
        ASSERT(bool,'%partition(RSB)%groupIdx')
        FINFO() 'Index:', ig
        FINFO() 'Ref: ',grpIdx(ig)
        FINFO() 'Test:',testPG%groupIdx(ig)
      ENDDO !ig
      DO iv=1,testPG%nvert
        bool=(testPG%groupList(iv) == GrpList(iv))
        ASSERT(bool,'%partition(RSB)%groupList')
        FINFO() 'Index:',iv
        FINFO() 'Ref: ',grpList(iv)
        FINFO() 'Test:',testPG%groupList(iv)
      ENDDO !iv

      !Clear
      DEALLOCATE(grpIdx)
      DEALLOCATE(grpList)
      CALL testPG%clear()
      CALL tparams%clear()
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      !Decomposition should look like:
      ! 2 2 1 1 1 1
      ! 2 2 1 1 1 1
      ! 3 3 2 2
      ! 3 3 2 2
      ! 3 3
      ! 3 3
      tparams=refG2Params
      CALL tparams%set('PartitionGraph -> Algorithms',algName)

      !initialize
      CALL testPG%initialize(tparams)
      CALL tparams%clear()

      !Partition
      CALL testPG%partition()

      !Setup reference
      ALLOCATE(grpIdx(4))
      ALLOCATE(grpList(24))
      grpIdx=(/1,9,17,25/)
      grpList=(/24,18,23,17,22,16,21,15, &
                 1, 2, 3, 4, 5, 6, 7, 8, &
                 9,10,11,12,13,14,19,20/)

      !Test
      DO ig=1,testPG%nGroups+1
        bool=(testPG%groupIdx(ig) == GrpIdx(ig))
        ASSERT(bool,'%partition(RSB)%groupIdx')
        FINFO() 'Index:',ig
        FINFO() 'Ref: ',grpIdx(ig)
        FINFO() 'Test:',testPG%groupIdx(ig)
      ENDDO !ig
      DO iv=1,testPG%nvert
        bool=(testPG%groupList(iv) == GrpList(iv))
        ASSERT(bool,'%partition(RSB)%groupList')
        FINFO() 'Index:',iv
        FINFO() 'Ref: ',grpList(iv)
        FINFO() 'Test:',testPG%groupList(iv)
      ENDDO !iv

      !Clear
      DEALLOCATE(grpIdx)
      DEALLOCATE(grpList)
      CALL testPG%clear()
      CALL tparams%clear()

      !Test larger 3-group problem (uneven divisions, weighted)
      !Decomposition should look like
      ! 2 2 2 2 1 1
      ! 2 2 2 1 1 1
      ! 3 2 2 1 1 1
      ! 3 3 3 1
      ! 3 3 3
      ! 3 3 3
      !Initialize
      tparams=refG3Params
      CALL tparams%set('PartitionGraph -> Algorithms',algName)
      CALL testPG%initialize(tparams)
      !Partition
      CALL testPG%partition()

      !Setup references
      ALLOCATE(grpIdx(4))
      ALLOCATE(grpList(28))
      grpIdx=(/1, 11, 20, 29/)
      grpList=(/ 28, 22, 16, 27, 21, 15, 26, 20, 25, 14, &
                  3,  2,  1,  6,  5,  4, 10,  9,  8, &
                  7, 11, 12, 13, 17, 18, 19, 23, 24/)

      !Test
      DO ig=1,testPG%nGroups+1
        bool=(testPG%groupIdx(ig) == GrpIdx(ig))
        ASSERT(bool,'%partition(RSB)%groupIdx')
        FINFO() 'Index:',ig
        FINFO() 'Ref: ',grpIdx(ig)
        FINFO() 'Test:',testPG%groupIdx(ig)
      ENDDO !ig
      DO iv=1,testPG%nvert
        bool=(testPG%groupList(iv) == GrpList(iv))
        ASSERT(bool,'%partition(RSB)%groupList')
        FINFO() 'Index:',iv
        FINFO() 'Ref: ',grpList(iv)
        FINFO() 'Test:',testPG%groupList(iv)
      ENDDO !iv

      DEALLOCATE(grpIdx)
      DEALLOCATE(grpList)

      CALL testPG%clear()
      CALL tparams%clear()
    ENDSUBROUTINE testRSB
!
!-------------------------------------------------------------------------------
    SUBROUTINE testMulti()
      LOGICAL(SBK) :: bool
      INTEGER(SIK) :: ig,iv
      INTEGER(SIK),ALLOCATABLE :: grpIdx(:),grpList(:)
      TYPE(StringType) :: algName(2)
      algName(1)='Recursive Expansion Bisection'
      algName(2)='Recursive Spectral Bisection'

      !Test larger 3-group problem (uneven divisions, weighted)
      !Decomposition should look like
      ! 2 2 2 3 3 3
      ! 2 2 2 3 3 3
      ! 2 2 2 3 3 3
      ! 1 1 1 1
      ! 1 1 1
      ! 1 1 1

      !Initialize
      tparams=refG3Params
      CALL tparams%set('PartitionGraph -> Algorithms',algName)
      CALL tparams%add('PartitionGraph -> Conditions',(/20/))
      CALL testPG%initialize(tparams)
      !Partition
      CALL testPG%partition()

      !Setup references
      ALLOCATE(grpIdx(4))
      ALLOCATE(grpList(28))
      grpIdx=(/1, 11, 19, 29/)
      grpList=(/  1,  2,  3,  6,  5,  4,  7,  8,  9, 10, &
                 23, 17, 11, 24, 18, 12, 25, 19, 13, &
                 14, 15, 16, 20, 21, 22, 26, 27, 28/)

      !Test
      DO ig=1,testPG%nGroups+1
        bool=(testPG%groupIdx(ig) == GrpIdx(ig))
        ASSERT(bool,'%partition(REB-RSB)%groupIdx')
        FINFO() 'Index:',ig
        FINFO() 'Ref: ',grpIdx(ig)
        FINFO() 'Test:',testPG%groupIdx(ig)
      ENDDO !ig
      DO iv=1,testPG%nvert
        bool=(testPG%groupList(iv) == GrpList(iv))
        ASSERT(bool,'%partition(REB-RSB)%groupList')
        FINFO() 'Index:',iv
        FINFO() 'Ref: ',grpList(iv)
        FINFO() 'Test:',testPG%groupList(iv)
      ENDDO !iv

      DEALLOCATE(grpIdx)
      DEALLOCATE(grpList)

      CALL testPG%clear()
    ENDSUBROUTINE testMulti
!
!-------------------------------------------------------------------------------
    SUBROUTINE testKL()
      LOGICAL(SBK) :: bool
      INTEGER(SIK) :: iv
      INTEGER(SIK),ALLOCATABLE :: L1(:),L2(:)
      INTEGER(SIK),ALLOCATABLE :: refL1(:),refL2(:)
      TYPE(StringType) :: algName(1),refineAlg(1)
      algName(1)='Recursive Expansion Bisection'
      refineAlg(1)='Kernighan-Lin'

      !Initialize
      tparams=refG2Params
      CALL tparams%set('PartitionGraph -> Algorithms',algName)
      CALL tparams%add('PartitionGraph -> Refinement',refineAlg)
      CALL testPG%initialize(tparams)

      ALLOCATE(L1(11))
      ALLOCATE(L2(13))
      ALLOCATE(refL1(11))
      ALLOCATE(refL2(13))
      L1=(/1,2,3,4,8,9,10,12,14,17,22/)
      L2=(/5,6,7,11,13,15,16,18,19,20,21,23,24/)
      refL1=(/11,15,8,7,22,23,12,18,16,17,24/)
      refL2=(/10,3,4,1,13,2,14,5,19,20,21,9,6/)
      !Refinement should transform:
      ! 2 2 2 1 2 2     2 2 2 1 1 1
      ! 2 1 2 2 1 2     2 2 1 1 1 1
      ! 1 1 2 1     =>  2 2 1 1
      ! 2 2 2 1         2 2 1 1
      ! 1 1             2 2
      ! 1 1             2 2
      CALL testPG%refine(L1,L2)

      !Test
      DO iv=1,SIZE(L1)
        bool=(L1(iv) == refL1(iv))
        ASSERT(bool,'%refine(KL) L1')
        FINFO() 'Index:',iv
        FINFO() 'Ref: ',refL1(iv)
        FINFO() 'Test:',L1(iv)
      ENDDO !iv

      DO iv=1,SIZE(L2)
        bool=(L2(iv) == refL2(iv))
        ASSERT(bool,'%refine(KL) L2')
        FINFO() 'Index:',iv
        FINFO() 'Ref: ',refL2(iv)
        FINFO() 'Test:',L2(iv)
      ENDDO !iv

      !Clear
      CALL tparams%clear()
      CALL testPG%clear()
      DEALLOCATE(L1)
      DEALLOCATE(L2)
      DEALLOCATE(refL1)
      DEALLOCATE(refL2)
    ENDSUBROUTINE testKL
!
!-------------------------------------------------------------------------------
    SUBROUTINE testMetrics()
      LOGICAL(SBK) :: bool
      REAL(SRK) :: mmr,srms,ecut,comm

      !Initialize the graph
      CALL testPG%initialize(refG3Params)
      !Partition the graph manually
      CALL testPG%setGroups( &
          (/1,11,20,29/), & !GroupIdx
          (/  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, & !GroupList Group 1
             11, 12, 13, 14, 15, 17, 18, 23, 24, &
             16, 19, 20, 21, 22, 25, 26, 27, 28/))
      !Calculate the metrics
      CALL testPG%metrics(mmr,srms,ecut,comm)

      !Test values
      bool=(mmr .APPROXEQ. 1.0588235294117647_SRK)
      ASSERT(bool,'max-min ratio')
      FINFO() mmr
      bool=(srms .APPROXEQ. 0.81649658092772603_SRK)
      ASSERT(bool, 'group size rms (from optimal)')
      FINFO() srms
      bool=(ecut .APPROXEQ. 10.0_SRK)
      ASSERT(bool, 'edges cut')
      FINFO() ecut
      bool=(comm .APPROXEQ. 18.0_SRK)
      ASSERT(bool, 'communication')
      FINFO() comm
      !Clear
      CALL testPG%clear()
    ENDSUBROUTINE testMetrics
!
!-------------------------------------------------------------------------------
    SUBROUTINE setupTest()
      TYPE(StringType) :: AlgName
      TYPE(StringType) :: refAlgNames(2)

      !Generate parameter list for the initialization test
      CALL refInitParams%add('PartitionGraph -> nvert', 6)
      CALL refInitParams%add('PartitionGraph -> nGroups', 3)
      CALL refInitParams%add('PartitionGraph -> neigh', &
        RESHAPE((/2, 0, 0, 0, &
                  1, 3, 4, 0, &
                  2, 5, 0, 0, &
                  2, 5, 0, 0, &
                  3, 4, 6, 0, &
                  5, 0, 0, 0/),(/4,6/)))
      CALL refInitParams%add('PartitionGraph -> wts', (/1,2,3,2,1,1/))
      CALL refInitParams%add('PartitionGraph -> neighwts', &
        RESHAPE((/1, 0, 0, 0, &
                  1, 2, 3, 0, &
                  2, 1, 0, 0, &
                  3, 1, 0, 0, &
                  1, 1, 1, 0, &
                  1, 0, 0, 0/), (/4,6/)))
      refAlgNames(1)='Recursive Expansion Bisection'
      refAlgNames(2)='Recursive Spectral Bisection'
      CALL refInitParams%add('PartitionGraph -> Algorithms', refAlgNames)
      CALL refInitParams%add('PartitionGraph -> Conditions', (/3/))
      CALL refInitParams%add('PartitionGraph -> coord', &
        RESHAPE((/-1.0_SRK, -1.0_SRK, &
                  -1.0_SRK,  0.0_SRK, &
                   0.0_SRK,  0.0_SRK, &
                  -1.0_SRK,  1.0_SRK, &
                   0.0_SRK,  1.0_SRK, &
                   1.0_SRK,  1.0_SRK/),(/2,6/)))

      !Generate parameter list for testGraph 1 with shape
      ! 1 1 1
      ! 1 1
      ! 1
      CALL refG1Params%add('PartitionGraph -> nvert', 6)
      CALL refG1Params%add('PartitionGraph -> nGroups', 2)
      CALL refG1Params%add('PartitionGraph -> neigh', &
        RESHAPE((/ 2, 0, 0, 0, &
                   1, 3, 4, 0, &
                   2, 5, 0, 0, &
                   2, 5, 0, 0, &
                   3, 4, 6, 0, &
                   5, 0, 0, 0/),(/4,6/)))
      AlgName='Recursive Expansion Bisection'
      refAlgNames(1)=AlgName
      CALL refG1Params%add('PartitionGraph -> Algorithms', refAlgNames(1:1))

      CALL refG1Params%add('PartitionGraph -> coord' ,&
        RESHAPE((/0.0_SRK, 0.0_SRK, &
                  0.0_SRK, 1.0_SRK, &
                  1.0_SRK, 1.0_SRK, &
                  0.0_SRK, 2.0_SRK, &
                  1.0_SRK, 2.0_SRK, &
                  2.0_SRK, 2.0_SRK/),(/2,6/)))

      !Generate parameter list for testGraph 2 with shape
      ! 1 1 1 1 1 1
      ! 1 1 1 1 1 1
      ! 1 1 1 1
      ! 1 1 1 1
      ! 1 1
      ! 1 1
      CALL refG2Params%add('PartitionGraph -> nvert',24)
      CALL refG2Params%add('PartitionGraph -> nGroups',3)
      CALL refG2Params%add('PartitionGraph -> Algorithms',refAlgNames(1:1))
      CALL refG2Params%add('PartitionGraph -> neigh', &
        RESHAPE((/ 2,  3,  0,  0, &
                   1,  4,  0,  0, &
                   1,  4,  5,  0, &
                   2,  3,  6,  0, &
                   3,  6,  9,  0, &
                   4,  5,  7, 10, &
                   6,  8, 11,  0, &
                   7, 12,  0,  0, &
                   5, 10, 13,  0, &
                   6,  9, 11, 14, &
                   7, 10, 12, 15, &
                   8, 11, 16,  0, &
                   9, 14, 19,  0, &
                  10, 13, 15, 20, &
                  11, 14, 16, 21, &
                  12, 15, 17, 22, &
                  16, 18, 23,  0, &
                  17, 24,  0,  0, &
                  13, 20,  0,  0, &
                  14, 19, 21,  0, &
                  15, 20, 22,  0, &
                  16, 21, 23,  0, &
                  17, 22, 24,  0, &
                  18, 23,  0,  0/),(/4, 24/)))
      CALL refG2Params%add('PartitionGraph -> coord', &
        RESHAPE((/1.0_SRK, 1.0_SRK, &
                  2.0_SRK, 1.0_SRK, &
                  1.0_SRK, 2.0_SRK, &
                  2.0_SRK, 2.0_SRK, &
                  1.0_SRK, 3.0_SRK, &
                  2.0_SRK, 3.0_SRK, &
                  3.0_SRK, 3.0_SRK, &
                  4.0_SRK, 3.0_SRK, &
                  1.0_SRK, 4.0_SRK, &
                  2.0_SRK, 4.0_SRK, &
                  3.0_SRK, 4.0_SRK, &
                  4.0_SRK, 4.0_SRK, &
                  1.0_SRK, 5.0_SRK, &
                  2.0_SRK, 5.0_SRK, &
                  3.0_SRK, 5.0_SRK, &
                  4.0_SRK, 5.0_SRK, &
                  5.0_SRK, 5.0_SRK, &
                  6.0_SRK, 5.0_SRK, &
                  1.0_SRK, 6.0_SRK, &
                  2.0_SRK, 6.0_SRK, &
                  3.0_SRK, 6.0_SRK, &
                  4.0_SRK, 6.0_SRK, &
                  5.0_SRK, 6.0_SRK, &
                  6.0_SRK, 6.0_SRK/),(/2,24/)))

      !Generate parameter list for testGraph 3 with shape (weights shown)
      ! 2 2 2 2 2 2
      ! 2 2 2 2 2 2
      ! 2 2 2 2 1 1
      ! 2 2 2 2
      ! 2 2 1
      ! 2 2 1
      CALL refG3Params%add('PartitionGraph -> nvert',28)
      CALL refG3Params%add('PartitionGraph -> nGroups',3)
      CALL refG3Params%add('PartitionGraph -> Algorithms',refAlgNames(1:1))
      CALL refG3Params%add('PartitionGraph -> wts', &
        (/2,2,1,2,2,1,2,2,2,2,2,2,2,2,1,1,2,2,2,2,2,2,2,2,2,2,2,2/))
      CALL refG3Params%add('PartitionGraph -> neigh', &
        RESHAPE((/  2,  4,  0,  0, &
                    1,  3,  5,  0, &
                    2,  6,  0,  0, &
                    1,  5,  7,  0, &
                    2,  4,  6,  8, &
                    3,  5,  9,  0, &
                    4,  8, 11,  0, &
                    5,  7,  9, 12, &
                    6,  8, 10, 13, &
                    9, 14,  0,  0, &
                    7, 12, 17,  0, &
                    8, 11, 13, 18, &
                    9, 12, 14, 19, &
                   10, 13, 15, 20, &
                   14, 16, 21,  0, &
                   15, 22,  0,  0, &
                   11, 18, 23,  0, &
                   12, 17, 19, 24, &
                   13, 18, 20, 25, &
                   14, 19, 21, 26, &
                   15, 20, 22, 27, &
                   16, 21, 28,  0, &
                   17, 24,  0,  0, &
                   18, 23, 25,  0, &
                   19, 24, 26,  0, &
                   20, 25, 27,  0, &
                   21, 26, 28,  0, &
                   22, 27,  0,  0/),(/4, 28/)))
      CALL refG3Params%add('PartitionGraph -> coord', &
        RESHAPE((/1.0_SRK, 1.0_SRK, &
                  2.0_SRK, 1.0_SRK, &
                  3.0_SRK, 1.0_SRK, &
                  1.0_SRK, 2.0_SRK, &
                  2.0_SRK, 2.0_SRK, &
                  3.0_SRK, 2.0_SRK, &
                  1.0_SRK, 3.0_SRK, &
                  2.0_SRK, 3.0_SRK, &
                  3.0_SRK, 3.0_SRK, &
                  4.0_SRK, 3.0_SRK, &
                  1.0_SRK, 4.0_SRK, &
                  2.0_SRK, 4.0_SRK, &
                  3.0_SRK, 4.0_SRK, &
                  4.0_SRK, 4.0_SRK, &
                  5.0_SRK, 4.0_SRK, &
                  6.0_SRK, 4.0_SRK, &
                  1.0_SRK, 5.0_SRK, &
                  2.0_SRK, 5.0_SRK, &
                  3.0_SRK, 5.0_SRK, &
                  4.0_SRK, 5.0_SRK, &
                  5.0_SRK, 5.0_SRK, &
                  6.0_SRK, 5.0_SRK, &
                  1.0_SRK, 6.0_SRK, &
                  2.0_SRK, 6.0_SRK, &
                  3.0_SRK, 6.0_SRK, &
                  4.0_SRK, 6.0_SRK, &
                  5.0_SRK, 6.0_SRK, &
                  6.0_SRK, 6.0_SRK/),(/2,28/)))
    ENDSUBROUTINE setupTest
!
!-------------------------------------------------------------------------------
    SUBROUTINE clearTest()
      CALL testPG%clear()
      CALL testSG%clear()
      CALL params%clear()
      CALL tparams%clear()
      CALL refInitParams%clear()
      CALL refG1Params%clear()
      CALL refG2Params%clear()
      CALL refG3Params%clear()
      CALL VectorType_Clear_ValidParams()
      CALL MatrixTypes_Clear_ValidParams()
      IF(ALLOCATED(strList)) DEALLOCATE(strList)
      DEALLOCATE(e)
    ENDSUBROUTINE clearTest
!
ENDPROGRAM testPartitionGraph
