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
  TYPE(ParamType) :: params,tparams,refInitParams
  TYPE(ParamType) :: refG1Params,refG2Params,refG3Params,refG4Params
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
  REGISTER_SUBTEST('Recursive Inertial Bisection',testRIB)
  REGISTER_SUBTEST('Multi-method',testMulti)
#endif
  REGISTER_SUBTEST('Kernighan-Lin',testKL)
  REGISTER_SUBTEST('Metrics Calculation',testMetrics)
#ifdef FUTILITY_HAVE_SLEPC
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
    SUBROUTINE testInit()
      CHARACTER(LEN=EXCEPTION_MAX_MESG_LENGTH) :: msg,refmsg
      LOGICAL(SBK) :: bool
      INTEGER(SIK),ALLOCATABLE :: refd(:),refneigh(:,:)
      REAL(SRK),ALLOCATABLE :: refwts(:),refnwts(:,:),refunwt(:,:),refCoord(:,:)
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
      CALL tparams%set('PartitionGraph -> neighwts', &
        RESHAPE((/3.0_SRK,1.0_SRK/),(/2,1/)))
      CALL testPG%initialize(tparams)
      msg=e%getLastMessage()
      refmsg='#### EXCEPTION_ERROR #### - PartitionGraph::'// &
        'init_PartitionGraph - input edge weights matrix is incorrect size!'
      ASSERT(msg == refmsg, '%init(...) wrong neighwts size')
      FINFO() refmsg
      FINFO() msg

      !Test invalid edge weight matrix
      CALL tparams%set('PartitionGraph -> neighwts', &
        RESHAPE((/-1.0_SRK, 1.0_SRK, 1.0_SRK, 1.0_SRK, &
                  -1.0_SRK, 1.0_SRK, 1.0_SRK, 1.0_SRK, &
                  -1.0_SRK, 1.0_SRK, 1.0_SRK, 1.0_SRK, &
                  -1.0_SRK, 1.0_SRK, 1.0_SRK, 1.0_SRK, &
                  -1.0_SRK, 1.0_SRK, 1.0_SRK, 1.0_SRK, &
                  -1.0_SRK ,1.0_SRK, 1.0_SRK, 1.0_SRK/),(/4,6/)))
      CALL testPG%initialize(tparams)
      msg=e%getLastMessage()
      refmsg='#### EXCEPTION_ERROR #### - PartitionGraph::'// &
        'init_PartitionGraph - edge weights must be > 0!'
      ASSERT(msg == refmsg, '%init(...) invalid neighwts')
      FINFO() refmsg
      FINFO() msg
      CALL tparams%set('PartitionGraph -> neighwts',refnwts)

      !Test incorrectly sized weight array
      CALL tparams%set('PartitionGraph -> wts', (/1.0_SRK/))
      CALL testPG%initialize(tparams)
      msg=e%getLastMessage()
      refmsg='#### EXCEPTION_ERROR #### - PartitionGraph::'// &
        'init_PartitionGraph - input vertex weights array is incorrect size!'
      ASSERT(msg == refmsg, '%init(...) wrong wts size')
      FINFO() refmsg
      FINFO() msg

      !Test invalid wts array
      CALL tparams%set('PartitionGraph -> wts',(/-1.0_SRK,1.0_SRK,1.0_SRK, &
        1.0_SRK,1.0_SRK,1.0_SRK/))
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
      refunwt=RESHAPE((/1.0_SRK, 0.0_SRK, 0.0_SRK, 0.0_SRK, &
                        1.0_SRK, 1.0_SRK, 1.0_SRK, 0.0_SRK, &
                        1.0_SRK, 1.0_SRK, 0.0_SRK, 0.0_SRK, &
                        1.0_SRK, 1.0_SRK, 0.0_SRK, 0.0_SRK, &
                        1.0_SRK, 1.0_SRK, 1.0_SRK, 0.0_SRK, &
                        1.0_SRK, 0.0_SRK, 0.0_SRK, 0.0_SRK/),(/4,6/))
      ASSERT(testPG%isInit, '%init(...)%isInit')
      ASSERT(testPG%nvert == 6, '%init(...)%nvert')
      ASSERT(testPG%dim == 2, '%init(...)%dim')
      ASSERT(testPG%nGroups == 3, '%init(...)%nGroups')
      ASSERT(testPG%maxneigh == 4, '%init(...)%maxneigh')
      ASSERT(testPG%nPart == 1, '%init(...)%nPart')
      bool=(SIZE(testPG%wts) == 6)
      ASSERT(bool,'%init(...)%SIZE(wts)')
      bool=ALL(testPG%wts == 1.0_SRK)
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
      INTEGER(SIK) :: refneigh(4,6),refcond(1)
      REAL(SRK) :: refwts(6),refnwts(4,6),refCoord(2,6)
      TYPE(StringType) :: refAlgNames(2)

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
      refwts=(/1.0_SRK,2.0_SRK,3.0_SRK,2.0_SRK,1.0_SRK,1.0_SRK/)
      CALL params%add('PartitionGraph -> wts', refwts)
      refnwts=RESHAPE((/1.0_SRK, 0.0_SRK, 0.0_SRK, 0.0_SRK, &
                        1.0_SRK, 2.0_SRK, 3.0_SRK, 0.0_SRK, &
                        2.0_SRK, 1.0_SRK, 0.0_SRK, 0.0_SRK, &
                        3.0_SRK, 1.0_SRK, 0.0_SRK, 0.0_SRK, &
                        1.0_SRK, 1.0_SRK, 1.0_SRK, 0.0_SRK, &
                        1.0_SRK, 0.0_SRK, 0.0_SRK, 0.0_SRK/), (/4,6/))
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
      ASSERT(ALL(testSG%wts == 1.0_SRK) , '%subgraph(...)%wts')
      bool=ALL(testSG%d == (/1, 1, 1, 3/))
      ASSERT(bool,'%subgraph(...)%d')
      bool=ALL(testSG%neigh == RESHAPE((/0, 4, 0, 0, &
                                         0, 4, 0, 0, &
                                         4, 0, 0, 0, &
                                         2, 1, 3, 0/),(/4,4/)))
      ASSERT(bool, '%subgraph(...)%neigh')
      bool=ALL(testSG%neighwts == RESHAPE((/0.0_SRK, 1.0_SRK, 0.0_SRK, 0.0_SRK, &
                                            0.0_SRK, 1.0_SRK, 0.0_SRK, 0.0_SRK, &
                                            1.0_SRK, 0.0_SRK, 0.0_SRK, 0.0_SRK, &
                                            1.0_SRK, 1.0_SRK, 1.0_SRK, 0.0_SRK/),(/4,4/)))
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
      TYPE(StringType) :: str
      INTEGER(SIK),ALLOCATABLE :: grpIdx(:),grpList(:)

      !String for this partitionTest
      str='REB'

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      !Test very simple 2-group, 2D, uniform-weighted graph
      !Decomposition should look like:
      ! 2 2 2
      ! 1 1
      ! 1
      !Setup test
      tparams=refG1Params
      ALLOCATE(grpIdx(3))
      ALLOCATE(grpList(6))
      grpIdx=(/1,4,7/)
      grpList=(/1,2,3,4,5,6/)
      !Test
      CALL partitionTest(tparams,str,grpIdx,grpList)

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      !Test larger 3-group problem (even divisions, unweighted)
      !Decomposition should look like
      ! 3 3 3 2 2 2
      ! 3 3 3 2 2 2
      ! 3 3 2 2
      ! 1 1 1 1
      ! 1 1
      ! 1 1
      !Setup
      tparams=refG2Params
      ALLOCATE(grpIdx(4))
      ALLOCATE(grpList(24))
      grpIdx=(/1, 9, 17, 25/)
      grpList=(/ 1,  2,  4,  3,  5,  6,  7,  8, &
                24, 18, 17, 23, 22, 16, 12, 11, &
                 9, 10, 13, 14, 15, 19, 20, 21/)
      !Test
      CALL partitionTest(tparams,str,grpIdx,grpList)

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      !Test larger 3-group problem (uneven divisions, weighted)
      !Decomposition should look like
      ! 3 3 3 2 2 2
      ! 3 3 3 2 2 2
      ! 3 3 2 2 2 2
      ! 1 1 1 1
      ! 1 1 1
      ! 1 1 1
      !Setup
      tparams=refG3Params
      ALLOCATE(grpIdx(4))
      ALLOCATE(grpList(28))
      grpIdx=(/1, 11, 21, 29/)
      grpList=(/ 1,  2,  3,  6,  5,  4,  7,  8,  9, 10, &
                16, 15, 22, 21, 28, 27, 26, 20, 14, 13, &
                11, 12, 17, 18, 19, 23, 24, 25/)
      !Test
      CALL partitionTest(tparams,str,grpIdx,grpList)
    ENDSUBROUTINE testREB
!
!-------------------------------------------------------------------------------
    SUBROUTINE testRSB()
      INTEGER(SIK),ALLOCATABLE :: grpIdx(:),grpList(:)
      TYPE(StringType) :: str,algName(1)

      str='RSB'
      algName(1)='Recursive Spectral Bisection'

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      !Decomposition should look like:
      ! 1 1 1
      ! 2 2
      ! 2
      !Setup
      tparams=refG1Params
      CALL tparams%set('PartitionGraph -> Algorithms',algName)
      ALLOCATE(grpIdx(3))
      ALLOCATE(grpList(6))
      grpIdx=(/1,4,7/)
      grpList=(/6,5,4,3,2,1/)
      CALL partitionTest(tparams,str,grpIdx,grpList)

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      !Decomposition should look like:
      ! 2 2 1 1 1 1
      ! 2 2 1 1 1 1
      ! 3 3 2 2
      ! 3 3 2 2
      ! 3 3
      ! 3 3
      !Setup
      tparams=refG2Params
      CALL tparams%set('PartitionGraph -> Algorithms',algName)
      ALLOCATE(grpIdx(4))
      ALLOCATE(grpList(24))
      grpIdx=(/1,9,17,25/)
      grpList=(/24,18,23,17,22,16,21,15, &
                19,20,13,14, 9,10,11,12, &
                 8, 7, 6, 5, 4, 3, 2, 1/)
      CALL partitionTest(tparams,str,grpIdx,grpList)

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      !Test larger 3-group problem (uneven divisions, weighted)
      !Decomposition should look like
      ! 2 2 2 2 1 1
      ! 2 2 2 1 1 1
      ! 3 2 2 1 1 1
      ! 3 3 3 1
      ! 3 3 3
      ! 3 3 3
      !Setup
      tparams=refG3Params
      CALL tparams%set('PartitionGraph -> Algorithms',algName)
      ALLOCATE(grpIdx(4))
      ALLOCATE(grpList(28))
      grpIdx=(/1, 11, 19, 29/)
      grpList=(/28,22,16,27,21,15,26,20,25,14, &
                23,24,17,18,19,11,12,13, &
                 7, 8, 9,10, 4, 5, 6, 1, 2, 3/)
      CALL partitionTest(tparams,str,grpIdx,grpList)
    ENDSUBROUTINE testRSB
!
!-------------------------------------------------------------------------------
    SUBROUTINE testRIB()
      INTEGER(SIK),ALLOCATABLE :: grpIdx(:),grpList(:)
      TYPE(StringType) :: str,algName(1)

      str='RIB'
      algName(1)='Recursive Inertial Bisection'

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      !Decomposition should look like:
      ! 2 1 1
      ! 2 1
      ! 2
      !Setup
      tparams=refG1Params
      CALL tparams%set('PartitionGraph -> Algorithms',algName)
      ALLOCATE(grpIdx(3))
      ALLOCATE(grpList(6))
      grpIdx=(/1,4,7/)
      grpList=(/6,5,4,3,2,1/)
      CALL partitionTest(tparams,str,grpIdx,grpList)
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      !Decomposition should look like:
      ! 2 2 1 1 1 1
      ! 2 2 1 1 1 1
      ! 3 3 2 2
      ! 3 3 2 2
      ! 3 3
      ! 3 3
      !Setup
      tparams=refG2Params
      CALL tparams%set('PartitionGraph -> Algorithms',algName)
      ALLOCATE(grpIdx(4))
      ALLOCATE(grpList(24))
      grpIdx=(/1,9,17,25/)
      grpList=(/24,18,23,17,22,16,21,12, &
                 1, 2, 3, 4, 5, 6, 7, 8, &
                 9,10,11,13,14,15,19,20/)
      CALL partitionTest(tparams,str,grpIdx,grpList)
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      !Test larger 3-group problem (uneven divisions, weighted)
      !Decomposition should look like
      ! 2 2 2 2 1 1
      ! 2 2 2 1 1 1
      ! 3 2 2 1 1 1
      ! 3 3 3 1
      ! 3 3 3
      ! 3 3 3
      !Setup
      tparams=refG3Params
      CALL tparams%set('PartitionGraph -> Algorithms',algName)
      ALLOCATE(grpIdx(4))
      ALLOCATE(grpList(28))
      grpIdx=(/1, 11, 19, 29/)
      grpList=(/28,22,27,16,21,26,15,20,25,14, &
                23,24,17,18,19,11,12,13, &
                 7, 8, 9,10, 4, 5, 6, 1, 2, 3/)
      CALL partitionTest(tparams,str,grpIdx,grpList)
    ENDSUBROUTINE testRIB
!
!-------------------------------------------------------------------------------
    SUBROUTINE testMulti()
      LOGICAL(SBK) :: bool
      INTEGER(SIK) :: ig,iv
      LOGICAL(SBK),ALLOCATABLE :: match(:,:)
      INTEGER(SIK),ALLOCATABLE :: grpIdx(:),grpList(:),grpList2(:)
      TYPE(StringType) :: str,algName(2)

      str='REB-RSB'
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
      !Setup
      tparams=refG3Params
      CALL tparams%set('PartitionGraph -> Algorithms',algName)
      CALL tparams%add('PartitionGraph -> Conditions',(/20/))
      ALLOCATE(grpIdx(4))
      ALLOCATE(grpList(28))
      ALLOCATE(grpList2(28))
      grpIdx=(/1, 11, 19, 29/)
      grpList=(/ 1, 2, 3, 6, 5, 4, 7, 8, 9,10, &
                11,17,23,12,18,24,13,19, &
                25,26,20,14,27,21,15,28,22,16/)
      grpList2=(/ 1, 2, 3, 6, 5, 4, 7, 8, 9,10, &
                 23,17,11,24,18,12,25,19, &
                 13,14,20,26,15,21,27,16,22,28/)
      ALLOCATE(match(2,28))
      !This case leads to a symmetric case with RSB. When getting eigenvectors
      !from SLEPc, different compilations will return different a different order
      !of the 3rd/4th smallest eigenvectors, as their eigenvalues are identical.
      !Thus 2 different orders must be checked.
      !Initialize
      CALL testPG%initialize(tparams)

      !Partition
      CALL testPG%partition()

      !Test
      DO ig=1,testPG%nGroups+1
        bool=(testPG%groupIdx(ig) == grpIdx(ig))
        ASSERT(bool,'%partition%groupIdx')
        FINFO() 'Index:', ig
        FINFO() 'Ref: ',grpIdx(ig)
        FINFO() 'Test:',testPG%groupIdx(ig)
      ENDDO !ig

      !Make sure it follows 1 of the 2 orders
      DO iv=1,testPG%nvert
        match(1,iv)=(testPG%groupList(iv) == grpList(iv))
        match(2,iv)=(testPG%groupList(iv) == grpList2(iv))
        bool=match(1,iv) .OR. match(2,iv)
        ASSERT(bool, '%partition(REB-RSB)%GroupList')
        FINFO() 'Index:',iv
        FINFO() 'Refs: ',grpList(iv),grpList2(iv)
        FINFO() 'Test:',testPG%groupList(iv)
      ENDDO !iv

      !Make sure complete match with 1 of the lists
      bool=ALL(match(1,:)) .OR. ALL(match(2,:))
      ASSERT(bool,'%partition(REB-RSB)%GroupList')
      FINFO() "2-case scenario didn't match either reference!"
      !Clear
      CALL tparams%clear()
      CALL testPG%clear()
      DEALLOCATE(grpIdx)
      DEALLOCATE(grpList)
      DEALLOCATE(grpList2)
      DEALLOCATE(match)
    ENDSUBROUTINE testMulti
!
!-------------------------------------------------------------------------------
    SUBROUTINE testKL()
      INTEGER(SIK),ALLOCATABLE :: L1(:),L2(:)
      INTEGER(SIK),ALLOCATABLE :: refL1(:),refL2(:)
      TYPE(StringType) :: str,algName(1),refineAlg(1)

      COMPONENT_TEST('Kernighan-Lin')
      str='KL'
      algName(1)='None'
      refineAlg(1)='Kernighan-Lin'

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      !Refinement should transform:
      ! 2 2 2 1 2 2     2 2 2 1 1 1
      ! 2 1 2 2 1 2     2 2 1 1 1 1
      ! 1 1 2 1     =>  2 2 1 1
      ! 2 2 2 1         2 2 1 1
      ! 1 1             2 2
      ! 1 1             2 2
      !Setup
      tparams=refG2Params
      CALL tparams%set('PartitionGraph -> Algorithms',algName)
      CALL tparams%add('PartitionGraph -> Refinement',refineAlg)
      ALLOCATE(L1(11))
      ALLOCATE(L2(13))
      ALLOCATE(refL1(11))
      ALLOCATE(refL2(13))
      L1=(/1,2,3,4,8,9,10,12,14,17,22/)
      L2=(/5,6,7,11,13,15,16,18,19,20,21,23,24/)
      refL1=(/11,15,8,7,22,23,12,18,16,17,24/)
      refL2=(/10,3,4,1,13,2,14,5,19,20,21,9,6/)
      CALL refinementTest(tparams,str,L1,L2,refL1,refL2)

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      !Test a small map
      !KL should not optimally refine this (local minima trap)
      !SKL will be able to optimally refine this
      !The initial partition looks like:
      !0 0 0 0 0 2 2 2 2 0 0 0 0 0
      !0 0 0 2 2 2 2 2 2 2 2 0 0 0
      !0 0 2 2 2 2 2 2 2 2 2 2 0 0
      !0 2 2 2 2 2 2 2 2 2 2 2 2 0
      !0 2 2 2 2 2 2 2 2 2 2 2 1 0
      !2 2 2 2 2 2 2 2 2 2 1 1 1 1
      !2 2 2 2 2 2 2 2 1 1 1 1 1 1
      !2 2 2 2 2 2 1 1 1 1 1 1 1 1
      !2 2 2 2 1 1 1 1 1 1 1 1 1 1
      !0 2 1 1 1 1 1 1 1 1 1 1 1 0
      !0 1 1 1 1 1 1 1 1 1 1 1 1 0
      !0 0 1 1 1 1 1 1 1 1 1 1 0 0
      !0 0 0 1 1 1 1 1 1 1 1 0 0 0
      !0 0 0 0 0 1 1 1 1 0 0 0 0 0
      !Setup
      tparams=refG4Params
      ALLOCATE(L1(74))
      ALLOCATE(L2(74))
      L1=(/1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25, &
          26,27,28,29,30,31,32,33,34,36,37,38,39,40,41,42,43,44,45,46,51,52, &
          53,54,55,56,57,58,59,60,67,68,69,70,71,72,73,74,83,84,85,86,87,88, &
          99,100,101,102,114/)
      L2=(/35,47,48,49,50,61,62,63,64,65,66,75,76,77,78,79,80,81,82,89,90,91, &
           92,93,94,95,96,97,98,103,104,105,106,107,108,109,110,111,112,113, &
           115,116,117,118,119,120,121,122,123,124,125,126,127,128,129,130, &
           131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146, &
           147,148/)
      ALLOCATE(refL1(74))
      ALLOCATE(refL2(74))
      refL1=(/1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24, &
             25,26,27,28,29,30,31,32,33,34,36,37,38,39,40,41,42,43,44,45,46, &
             51,52,53,54,55,56,57,58,59,60,67,68,69,70,71,72,73,74,83,84,85, &
             86,87,88,99,100,101,102,35/)
      refL2=(/114,47,48,49,50,61,62,63,64,65,66,75,76,77,78,79,80,81,82,89,90, &
               91,92,93,94,95,96,97,98,103,104,105,106,107,108,109,110,111, &
              112,113,115,116,117,118,119,120,121,122,123,124,125,126,127,128, &
              129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144, &
              145,146,147,148/)
      CALL refinementTest(tparams,str,L1,L2,refL1,refL2)

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      COMPONENT_TEST('Spatial Kernighan-Lin')
      str='SKL'
      refineAlg(1)='Spatial Kernighan-Lin'
      tparams=refG4Params
      CALL tparams%set('PartitionGraph -> Refinement', refineAlg)
      ALLOCATE(L1(74))
      ALLOCATE(L2(74))
      L1=(/1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25, &
          26,27,28,29,30,31,32,33,34,36,37,38,39,40,41,42,43,44,45,46,51,52, &
          53,54,55,56,57,58,59,60,67,68,69,70,71,72,73,74,83,84,85,86,87,88, &
          99,100,101,102,114/)
      L2=(/35,47,48,49,50,61,62,63,64,65,66,75,76,77,78,79,80,81,82,89,90,91, &
           92,93,94,95,96,97,98,103,104,105,106,107,108,109,110,111,112,113, &
           115,116,117,118,119,120,121,122,123,124,125,126,127,128,129,130, &
           131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146, &
           147,148/)
      ALLOCATE(refL1(74))
      ALLOCATE(refL2(74))
      refL1=(/1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24, &
             25,26,27,28,29,30,31,32,33,34,36,37,38,39,40,41,42,43,44,45,46, &
             51,52,53,54,55,56,57,58,59,60,67,68,69,70,71,72,73,74,66,65,64, &
             63,62,61,50,49,48,47,35/)
      refL2=(/114,102,101,100,99,88,87,86,85,84,83,75,76,77,78,79,80,81,82, &
              89,90,91,92,93,94,95,96,97,98,103,104,105,106,107,108,109,110, &
              111,112,113,115,116,117,118,119,120,121,122,123,124,125,126,127, &
              128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143, &
              144,145,146,147,148/)
      CALL refinementTest(tparams,str,L1,L2,refL1,refL2)
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
      bool=(srms .APPROXEQ. 4.7105571976599583_SRK)
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
      REAL(SRK),ALLOCATABLE :: map(:,:)
      TYPE(StringType) :: AlgName
      TYPE(StringType) :: refAlgNames(2),refineAlg(1)

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
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
      CALL refInitParams%add('PartitionGraph -> wts', &
        (/1.0_SRK, 2.0_SRK, 3.0_SRK, 2.0_SRK, 1.0_SRK, 1.0_SRK/))
      CALL refInitParams%add('PartitionGraph -> neighwts', &
        RESHAPE((/1.0_SRK, 0.0_SRK, 0.0_SRK, 0.0_SRK, &
                  1.0_SRK, 2.0_SRK, 3.0_SRK, 0.0_SRK, &
                  2.0_SRK, 1.0_SRK, 0.0_SRK, 0.0_SRK, &
                  3.0_SRK, 1.0_SRK, 0.0_SRK, 0.0_SRK, &
                  1.0_SRK, 1.0_SRK, 1.0_SRK, 0.0_SRK, &
                  1.0_SRK, 0.0_SRK, 0.0_SRK, 0.0_SRK/), (/4,6/)))
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

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      !Generate parameter list for testGraph 1 with shape
      ! 1 1 1
      ! 1 1
      ! 1
      ALLOCATE(map(3,3))
      map=REAL(RESHAPE((/1,1,1, &
                         1,1,0, &
                         1,0,0/),(/3,3/)),SRK)
      CALL map2Graph(map,refG1Params)
      AlgName='Recursive Expansion Bisection'
      refAlgNames(1)=AlgName
      CALL refG1Params%add('PartitionGraph -> nGroups', 2)
      CALL refG1Params%add('PartitionGraph -> Algorithms', refAlgNames(1:1))
      DEALLOCATE(map)

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      !Generate parameter list for testGraph 2 with shape
      ! 1 1 1 1 1 1
      ! 1 1 1 1 1 1
      ! 1 1 1 1
      ! 1 1 1 1
      ! 1 1
      ! 1 1
      ALLOCATE(map(6,6))
      map=REAL(RESHAPE((/1,1,1,1,1,1, &
                         1,1,1,1,1,1, &
                         1,1,1,1,0,0, &
                         1,1,1,1,0,0, &
                         1,1,0,0,0,0, &
                         1,1,0,0,0,0/),(/6,6/)),SRK)
      CALL map2Graph(map,refG2Params)
      CALL refG2Params%add('PartitionGraph -> nGroups',3)
      CALL refG2Params%add('PartitionGraph -> Algorithms',refAlgNames(1:1))
      DEALLOCATE(map)

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      !Generate parameter list for testGraph 3 with shape (weights shown)
      ! 2 2 2 2 2 2
      ! 2 2 2 2 2 2
      ! 2 2 2 2 1 1
      ! 2 2 2 2
      ! 2 2 1
      ! 2 2 1
      ALLOCATE(map(6,6))
      map=REAL(RESHAPE((/2,2,2,2,2,2, &
                         2,2,2,2,2,2, &
                         2,2,2,2,1,1, &
                         2,2,2,2,0,0, &
                         2,2,1,0,0,0, &
                         2,2,1,0,0,0/),(/6,6/)),SRK)
      CALL map2Graph(map,refG3Params)
      CALL refG3Params%add('PartitionGraph -> nGroups',3)
      CALL refG3Params%add('PartitionGraph -> Algorithms',refAlgNames(1:1))
      DEALLOCATE(map)

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      !Generate parameter list for testGraph 4 with shape
      ! 0 0 0 0 0 1 1 1 1 0 0 0 0 0
      ! 0 0 0 1 1 1 1 1 1 1 1 0 0 0
      ! 0 0 1 1 1 1 1 1 1 1 1 1 0 0
      ! 0 1 1 1 1 1 1 1 1 1 1 1 1 0
      ! 0 1 1 1 1 1 1 1 1 1 1 1 1 0
      ! 1 1 1 1 1 1 1 1 1 1 1 1 1 1
      ! 1 1 1 1 1 1 1 1 1 1 1 1 1 1
      ! 1 1 1 1 1 1 1 1 1 1 1 1 1 1
      ! 1 1 1 1 1 1 1 1 1 1 1 1 1 1
      ! 0 1 1 1 1 1 1 1 1 1 1 1 1 0
      ! 0 1 1 1 1 1 1 1 1 1 1 1 1 0
      ! 0 0 1 1 1 1 1 1 1 1 1 1 0 0
      ! 0 0 0 1 1 1 1 1 1 1 1 0 0 0
      ! 0 0 0 0 0 1 1 1 1 0 0 0 0 0
      ALLOCATE(map(14,14))
      map=REAL(RESHAPE((/0,0,0,0,0,1,1,1,1,0,0,0,0,0, &
                         0,0,0,1,1,1,1,1,1,1,1,0,0,0, &
                         0,0,1,1,1,1,1,1,1,1,1,1,0,0, &
                         0,1,1,1,1,1,1,1,1,1,1,1,1,0, &
                         0,1,1,1,1,1,1,1,1,1,1,1,1,0, &
                         1,1,1,1,1,1,1,1,1,1,1,1,1,1, &
                         1,1,1,1,1,1,1,1,1,1,1,1,1,1, &
                         1,1,1,1,1,1,1,1,1,1,1,1,1,1, &
                         1,1,1,1,1,1,1,1,1,1,1,1,1,1, &
                         0,1,1,1,1,1,1,1,1,1,1,1,1,0, &
                         0,1,1,1,1,1,1,1,1,1,1,1,1,0, &
                         0,0,1,1,1,1,1,1,1,1,1,1,0,0, &
                         0,0,0,1,1,1,1,1,1,1,1,0,0,0, &
                         0,0,0,0,0,1,1,1,1,0,0,0,0,0/),(/14,14/)),SRK)
      CALL map2Graph(map,refG4Params)
      DEALLOCATE(map)
      refineAlg(1)='Kernighan-Lin'
      CALL refG4Params%add('PartitionGraph -> Algorithms',refAlgNames(1:1))
      CALL refG4Params%add('PartitionGraph -> Refinement',refineAlg)
      CALL refG4Params%add('PartitionGraph -> nGroups',2)
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
      CALL refG4Params%clear()
      CALL VectorType_Clear_ValidParams()
      CALL MatrixTypes_Clear_ValidParams()
      DEALLOCATE(e)
    ENDSUBROUTINE clearTest
!
!-------------------------------------------------------------------------------
!> @brief help routine to generate a parameter list for a graph from a 2D map
!>        This only adds relevent parameters: nvert, neigh, wts, coord for map
!>        (can do weighted vertices, but not edges)
!>
    SUBROUTINE map2Graph(map,params)
      REAL(SRK),INTENT(IN) :: map(:,:)
      TYPE(ParamType) :: params
      INTEGER(SIK) :: nvert,ix,iy,ij,iv,cv,nx,ny
      INTEGER(SIK),ALLOCATABLE :: indMap(:,:),neigh(:,:)
      REAL(SRK),ALLOCATABLE :: wts(:),coord(:,:)

      nx=SIZE(map,DIM=1)
      ny=SIZE(map,DIM=2)
      nvert=COUNT(map > 0.0_SRK)
      ALLOCATE(wts(nvert))
      ALLOCATE(neigh(4,nvert))
      ALLOCATE(coord(2,nvert))
      ALLOCATE(indMap(nx,ny))
      neigh=0
      indMap=0
      wts=0.0_SRK
      coord=0.0_SRK

      cv=0
      DO iy=ny,1,-1
        DO ix=1,nx
          IF(map(ix,iy) > 0.0_SRK) THEN
            cv=cv+1
            wts(cv)=map(ix,iy)
            ij=ny-iy+1
            coord(1,cv)=REAL(ix,SRK); coord(2,cv)=REAL(ij,SRK)
            indMap(ix,ij)=cv
          ENDIF
        ENDDO !iy
      ENDDO !ix

      !Get neighbors
      DO iv=1,nvert
        ix=INT(coord(1,iv),SIK); iy=INT(coord(2,iv),SIK)
        IF(ix > 1) neigh(1,iv)=indMap(ix-1,iy)
        IF(ix < nx) neigh(2,iv)=indMap(ix+1,iy)
        IF(iy > 1) neigh(3,iv)=indMap(ix,iy-1)
        IF(iy < ny) neigh(4,iv)=indMap(ix,iy+1)
      ENDDO !iv

      DEALLOCATE(indMap)

      !Add to parameter list
      CALL params%add('PartitionGraph -> nvert', nvert)
      CALL params%add('PartitionGraph -> neigh', neigh)
      CALL params%add('PartitionGraph -> wts', wts)
      CALL params%add('PartitionGraph -> coord', coord)

      DEALLOCATE(neigh)
      DEALLOCATE(wts)
      DEALLOCATE(coord)
    ENDSUBROUTINE map2Graph
!
!-------------------------------------------------------------------------------
!> @brief help routine to test each partitioning method given a valid parameter
!>        list to initialize the graph
!> @param params parameter list to initialize graph
!> @param str String to print so user can still find which test is failing
!> @param grpIdx Reference group indices
!> @param grpList Reference group list
!>
!> This routine also clears and deallocates all the arguments
!> (reduces the cleanup code)
!>
  SUBROUTINE partitionTest(params,str,grpIdx,grpList)
    TYPE(ParamType),INTENT(INOUT) :: params
    TYPE(StringType),INTENT(INOUT) :: str
    INTEGER(SIK),ALLOCATABLE,INTENT(INOUT) :: grpIdx(:),grpList(:)
    LOGICAL(SBK) :: bool
    INTEGER(SIK) :: ig,iv
    TYPE(StringType) :: prt

    !Intiialize
    CALL testPG%initialize(params)

    !Call partition algorithm
    CALL testPG%partition()

    prt='%partition('//TRIM(ADJUSTL(str))//')'

    !Test
    DO ig=1,testPG%nGroups+1
      bool=(testPG%groupIdx(ig) == grpIdx(ig))
      ASSERT(bool,prt//'%groupIdx')
      FINFO() 'Index:', ig
      FINFO() 'Ref: ',grpIdx(ig)
      FINFO() 'Test:',testPG%groupIdx(ig)
    ENDDO !ig

    DO iv=1,testPG%nvert
      bool=(testPG%groupList(iv) == grpList(iv))
      ASSERT(bool,prt//'%groupList')
      FINFO() 'Index:',iv
      FINFO() 'Ref: ',grpList(iv)
      FINFO() 'Test:',testPG%groupList(iv)
    ENDDO !iv

    !Clear
    CALL params%clear()
    CALL testPG%clear()
    DEALLOCATE(grpIdx)
    DEALLOCATE(grpList)
  ENDSUBROUTINE partitionTest
!
!-------------------------------------------------------------------------------
!> @brief help routine to test each refinement method given a valid parameter
!>        list to initialize the graph
!> @param params parameter list to initialize graph
!> @param str String to print so user can still find which test is failing
!> @param L1 the first group list
!> @param L2 the second group list
!> @param rL1 the first reference group list to test against
!> @param rL2 the second reference group list to test against
!>
!> This routine also clears and deallocates all the arguments
!> (reduces the cleanup code)
!>
  SUBROUTINE refinementTest(params,str,L1,L2,rL1,rL2)
    TYPE(ParamType),INTENT(INOUT) :: params
    TYPE(StringType),INTENT(INOUT) :: str
    INTEGER(SIK),ALLOCATABLE,INTENT(INOUT) :: L1(:),L2(:),rL1(:),rL2(:)
    LOGICAL(SBK) :: bool
    INTEGER(SIK) :: iv
    TYPE(StringType) :: prt

    !Error String
    prt='%refine('//TRIM(ADJUSTL(str))//')'

    !Initialize
    CALL testPG%initialize(params)
    !Refine
    CALL testPG%refine(L1,L2)

    !Test
    DO iv=1,SIZE(L1)
      bool=(L1(iv) == rL1(iv))
      ASSERT(bool,prt//' L1')
      FINFO() 'Index:',iv
      FINFO() 'Ref: ',rL1(iv)
      FINFO() 'Test:',L1(iv)
    ENDDO !iv

    DO iv=1,SIZE(L2)
      bool=(L2(iv) == rL2(iv))
      ASSERT(bool,prt//' L2')
      FINFO() 'Index:',iv
      FINFO() 'Ref: ',rL2(iv)
      FINFO() 'Test:',L2(iv)
    ENDDO !iv

    !Clear/deallocate
    CALL params%clear()
    CALL testPG%clear()
    DEALLOCATE(L1)
    DEALLOCATE(L2)
    DEALLOCATE(rL1)
    DEALLOCATE(rL2)
  ENDSUBROUTINE refinementTest
!
ENDPROGRAM testPartitionGraph
