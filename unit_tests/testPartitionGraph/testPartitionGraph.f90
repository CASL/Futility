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
  USE PartitionGraph

  IMPLICIT NONE

  TYPE(PartitionGraphType) :: testPG
  TYPE(ParamType) :: params,tparams
  TYPE(StringType),ALLOCATABLE :: strList(:)
  TYPE(ExceptionHandlerType),POINTER :: e

  CREATE_TEST('PartitionGraph')

  ALLOCATE(e)
  CALL e%setStopOnError(.FALSE.)
  CALL e%setQuietMode(.TRUE.)
  CALL ePartitionGraph%addSurrogate(e)

  REGISTER_SUBTEST('Initialization',testInit)
  REGISTER_SUBTEST('Clear',testClear)
  FINALIZE_TEST()

  CALL clearTest()
!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
    !Test initialization process
    SUBROUTINE testInit()
      CHARACTER(LEN=EXCEPTION_MAX_MESG_LENGTH) :: msg,refmsg
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
      tparams=params !Copy good parameter list

      !Test invalid number of groups
      CALL tparams%set('PartitionGraph -> nGroups',0)
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

      ! !Test incorrectly sized coordinate matrix
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
      bool=ALL(testPG%neighwts == 1)
      ASSERT(bool,'%init(...)%neighwts')
      bool=((SIZE(testPG%coord,DIM=1) == 2) .AND. &
            (SIZE(testPG%coord,DIM=2) == 6))
      ASSERT(bool,'%init(...)%SIZE(coord)')
      bool=ALL(testPG%coord == refCoord)
      ASSERT(bool,'%init(...)%coord')
      bool=(SIZE(testPG%partitionAlgArry) == 1)
      ASSERT(bool,'%init(...)%SIZE(partitionAlgArry)')

      !Test full initializaton (weighted, multiple algorithms)
      tparams=params
      CALL testPG%initialize(tparams)

      ASSERT(testPG%nvert == 6, '%init(...)%nvert')
      ASSERT(testPG%dim == 2, '%init(...)%dim')
      ASSERT(testPG%nGroups == 3, '%init(...)%nGroups')
      ASSERT(testPG%maxneigh == 4, '%init(...)%maxneigh')
      ASSERT(testPG%nPart == 2, '%init(...)%nPart')
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

      CALL testPG%clear()
      CALL tparams%clear()
      CALL params%clear()
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
    !Clear data from the unit-test
    SUBROUTINE clearTest()
      CALL testPG%clear()
      CALL params%clear()
      CALL tparams%clear()
      CALL PartitionGraphType_Clear_Params()
      IF(ALLOCATED(strList)) DEALLOCATE(strList)
      DEALLOCATE(e)
    ENDSUBROUTINE clearTest
!
ENDPROGRAM testPartitionGraph
