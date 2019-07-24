!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
PROGRAM testMeshTransfer
#include "UnitTest.h"
  USE UnitTest
  USE IntrType
  USE Strings
  USE Constants_Conversion
  USE ParameterLists
  USE ParallelEnv
  USE VectorTypes
  USE MatrixTypes
  USE LinearSolverTypes
  USE MeshTransfer

  IMPLICIT NONE
!
!Check the timer resolution
  CREATE_TEST('Mesh Transfer')

  REGISTER_SUBTEST('Test LP PointVal',TestLPPointVal)
  REGISTER_SUBTEST('Test LP Integral',TestLPIntegral)
  REGISTER_SUBTEST('Test ZP PointVal',TestZPPointVal)
  REGISTER_SUBTEST('Test ZP Integral',TestZPIntegral)
  REGISTER_SUBTEST('Test 1DBase MeshTransfer',Test1DBase)
  REGISTER_SUBTEST('Test 1D setup',Test1Dsetup)
  REGISTER_SUBTEST('Test 1DCart MeshTransfer',Test1DCart)
  REGISTER_SUBTEST('Test 1DCyl MeshTransfer',Test1DCyl)

  FINALIZE_TEST()
!
!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
!> Using 1DCart but testing only functions on the 1Dbase class.
    SUBROUTINE Test1DBase()
      TYPE(MeshTransfer_1DCart) :: testMT
      TYPE(ParamType) :: testPL
      INTEGER(SIK) :: i
      REAL(SRK) :: mesh(6), tmpin(5),mesh_out(11)
      REAL(SRK),ALLOCATABLE :: tmpout(:)

      DO i=0,5
        mesh(i+1)=SQRT(REAL(i,SRK)/5.0_SRK)
      ENDDO
      DO i=0,10
        mesh_out(i+1)=SQRT(REAL(i,SRK)/10.0_SRK)
      ENDDO

      CALL testPL%add('MeshTransfer->map_in','VOLUME')
      CALL testPL%add('MeshTransfer->map_out','POINT')
      CALL testPL%add('MeshTransfer->volumemesh_in',mesh)
      CALL testPL%add('MeshTransfer->pointmesh_out',mesh_out)
      CALL testPL%add('MeshTransfer->poly_transfer',4)

      COMPONENT_TEST('%init_base')
      CALL testMT%init_base(testPL)

      ASSERT_EQ(testMT%MapType_in,3,'map_in')
      ASSERT_EQ(testMT%MapType_out,2,'map_out')
      ASSERT_EQ(testMT%nmesh_in,5,'nmesh_in')
      ASSERT_EQ(testMT%nmesh_out,11,'nmesh_out')
      ASSERT_EQ(testMT%poly_transfer,4,'poly_transfer')
      ASSERT(ALL(testMT%mesh_in .APPROXEQ. mesh),'mesh_in')
      ASSERT(ALL(testMT%mesh_out .APPROXEQ. mesh_out),'mesh_out')

      COMPONENT_TEST('%clear')
      testMT%isInit=.TRUE.
      ALLOCATE(testMT%transferMatrix(3,4))
      testMT%transferLS%isInit=.TRUE.
      CALL testMT%clear()

      ASSERT_EQ(testMT%MapType_in,0,'map_in')
      ASSERT_EQ(testMT%MapType_out,0,'map_out')
      ASSERT_EQ(testMT%nmesh_in,0,'nmesh_in')
      ASSERT_EQ(testMT%nmesh_out,0,'nmesh_out')
      ASSERT_EQ(testMT%poly_transfer,-1,'poly_transfer')
      ASSERT(.NOT. ALLOCATED(testMT%mesh_in),'mesh_in')
      ASSERT(.NOT. ALLOCATED(testMT%mesh_out),'mesh_out')
      ASSERT(.NOT. ALLOCATED(testMT%transferMatrix),'transferMatrix')
      ASSERT(.NOT. testMT%transferLS%isInit,'transferLS init')

      COMPONENT_TEST('%init_base other settings')
      CALL testPL%clear()
      CALL testPL%add('MeshTransfer->map_in','VOLUME')
      CALL testPL%add('MeshTransfer->map_out','CONTINUOUS')
      CALL testPL%add('MeshTransfer->volumemesh_in',mesh)
      CALL testPL%add('MeshTransfer->moments_out',3)

      CALL testMT%init_base(testPL)

      ASSERT_EQ(testMT%MapType_in,3,'map_in')
      ASSERT_EQ(testMT%MapType_out,1,'map_out')
      ASSERT_EQ(testMT%nmesh_in,5,'nmesh_in')
      ASSERT_EQ(testMT%nmesh_out,3,'nmesh_out')
      ASSERT_EQ(testMT%poly_transfer,-1,'poly_transfer')
      ASSERT(ALL(testMT%mesh_in .APPROXEQ. mesh),'mesh_in')
      ASSERT(.NOT. ALLOCATED(testMT%mesh_out),'mesh_out')
      CALL testMT%clear()

      CALL testPL%clear()
      CALL testPL%add('MeshTransfer->map_in','CONTINUOUS')
      CALL testPL%add('MeshTransfer->map_out','VOLUME')
      CALL testPL%add('MeshTransfer->moments_in',4)
      CALL testPL%add('MeshTransfer->volumemesh_out',mesh_out)

      CALL testMT%init_base(testPL)

      ASSERT_EQ(testMT%MapType_in,1,'map_in')
      ASSERT_EQ(testMT%MapType_out,3,'map_out')
      ASSERT_EQ(testMT%nmesh_in,4,'nmesh_in')
      ASSERT_EQ(testMT%nmesh_out,10,'nmesh_out')
      ASSERT_EQ(testMT%poly_transfer,-1,'poly_transfer')
      ASSERT(.NOT. ALLOCATED(testMT%mesh_in),'mesh_in')
      ASSERT(ALL(testMT%mesh_out .APPROXEQ. mesh_out),'mesh_out')
      CALL testMT%clear()

      COMPONENT_TEST('%transfer')
      CALL testPL%clear()
      CALL testPL%add('MeshTransfer->map_in','VOLUME')
      CALL testPL%add('MeshTransfer->map_out','POINT')
      CALL testPL%add('MeshTransfer->volumemesh_in',mesh)
      CALL testPL%add('MeshTransfer->pointmesh_out',mesh_out)
      CALL testPL%add('MeshTransfer->poly_transfer',4)

      CALL testMT%init_base(testPL)

      CALL testPL%clear()
      CALL testPL%add('LinearSolverType->TPLType',LS_NATIVE)
      CALL testPL%add('LinearSolverType->solverMethod',LU)
      CALL testPL%add('LinearSolverType->MPI_Comm_ID',PE_COMM_SELF)
      CALL testPL%add('LinearSolverType->numberOMP',1_SNK)
      CALL testPL%add('LinearSolverType->timerName','testTimer')
      CALL testPL%add('LinearSolverType->matType',DENSESQUARE)
      CALL testPL%add('LinearSolverType->A->MatrixType->isSym',.FALSE.)
      CALL testPL%add('LinearSolverType->A->MatrixType->n',5)
      CALL testPL%add('LinearSolverType->x->VectorType->n',5)
      CALL testPL%add('LinearSolverType->b->VectorType->n',5)
      CALL testMT%transferLS%init(testPL)
      CALL testPL%clear()

      SELECTTYPE(A => testMT%transferLS%A); TYPE IS(DenseSquareMatrixType)
        CALL A%set(1,1,2.0_SRK)
        CALL A%set(2,2,3.0_SRK)
        CALL A%set(3,3,4.0_SRK)
        CALL A%set(4,4,ONE/5.0_SRK)
        CALL A%set(5,5,ONE)
      ENDSELECT

      tmpin(:)=(/0.8_SRK, 0.9_SRK, 1.0_SRK, 1.1_SRK, 1.2_SRK /)
      CALL testMT%transfer(tmpin,tmpout)

      ASSERT_EQ(SIZE(tmpout),11,'SIZE(tmpout)')
      ASSERT_APPROXEQ(tmpout(1),0.40_SRK,'tmpout(1)')
      ASSERT_APPROXEQ(tmpout(2),0.30_SRK,'tmpout(2)')
      ASSERT_APPROXEQ(tmpout(3),0.25_SRK,'tmpout(3)')
      ASSERT_APPROXEQ(tmpout(4),5.50_SRK,'tmpout(4)')
      ASSERT_APPROXEQ(tmpout(5),1.20_SRK,'tmpout(5)')
      ASSERT(ALL(tmpout(6:11)==ZERO),'tmpout(6:11)')

      DEALLOCATE(tmpout)

      ALLOCATE(testMT%transferMatrix(11,4))
      testMT%transferMatrix(:,:)=ZERO
      testMT%transferMatrix(1,1)=1.5_SRK
      testMT%transferMatrix(1,2)=2.0_SRK
      testMT%transferMatrix(2,3)=ONE
      testMT%transferMatrix(3,4)=ONE
      testMT%transferMatrix(4,1)=ONE
      testMT%transferMatrix(5,2)=ONE
      testMT%transferMatrix(6,3)=ONE
      testMT%transferMatrix(7,4)=ONE
      testMT%transferMatrix(8,3)=ONE
      testMT%transferMatrix(9,2)=ONE
      testMT%transferMatrix(10,1)=ONE

      CALL testMT%transfer(tmpin,tmpout)

      ASSERT_EQ(SIZE(tmpout),11,'SIZE(tmpout)')
      ASSERT_APPROXEQ(tmpout(1),1.20_SRK,'tmpout(1)')
      ASSERT_APPROXEQ(tmpout(2),0.25_SRK,'tmpout(2)')
      ASSERT_APPROXEQ(tmpout(3),5.50_SRK,'tmpout(3)')
      ASSERT_APPROXEQ(tmpout(4),0.40_SRK,'tmpout(4)')
      ASSERT_APPROXEQ(tmpout(5),0.30_SRK,'tmpout(5)')
      ASSERT_APPROXEQ(tmpout(6),0.25_SRK,'tmpout(6)')
      ASSERT_APPROXEQ(tmpout(7),5.50_SRK,'tmpout(7)')
      ASSERT_APPROXEQ(tmpout(8),0.25_SRK,'tmpout(8)')
      ASSERT_APPROXEQ(tmpout(9),0.30_SRK,'tmpout(9)')
      ASSERT_APPROXEQ(tmpout(10),0.40_SRK,'tmpout(10)')
      ASSERT_APPROXEQ(tmpout(11),0.00_SRK,'tmpout(11)')

      CALL testMT%transferLS%clear()
      DEALLOCATE(testMT%transferMatrix)
      ALLOCATE(testMT%transferMatrix(11,5))
      testMT%transferMatrix(:,:)=ZERO
      testMT%transferMatrix(1,1)=1.5_SRK
      testMT%transferMatrix(1,2)=2.0_SRK
      testMT%transferMatrix(1,5)=3.0_SRK
      testMT%transferMatrix(2,3)=ONE
      testMT%transferMatrix(3,4)=ONE
      testMT%transferMatrix(4,5)=ONE
      testMT%transferMatrix(5,4)=ONE
      testMT%transferMatrix(6,3)=ONE
      testMT%transferMatrix(7,2)=ONE
      testMT%transferMatrix(8,1)=ONE
      testMT%transferMatrix(9,2)=ONE
      testMT%transferMatrix(10,3)=ONE

      CALL testMT%transfer(tmpin,tmpout)

      ASSERT_EQ(SIZE(tmpout),11,'SIZE(tmpout)')
      ASSERT_APPROXEQ(tmpout(1),6.6_SRK,'tmpout(1)')
      ASSERT_APPROXEQ(tmpout(2),1.0_SRK,'tmpout(2)')
      ASSERT_APPROXEQ(tmpout(3),1.1_SRK,'tmpout(3)')
      ASSERT_APPROXEQ(tmpout(4),1.2_SRK,'tmpout(4)')
      ASSERT_APPROXEQ(tmpout(5),1.1_SRK,'tmpout(5)')
      ASSERT_APPROXEQ(tmpout(6),1.0_SRK,'tmpout(6)')
      ASSERT_APPROXEQ(tmpout(7),0.9_SRK,'tmpout(7)')
      ASSERT_APPROXEQ(tmpout(8),0.8_SRK,'tmpout(8)')
      ASSERT_APPROXEQ(tmpout(9),0.9_SRK,'tmpout(9)')
      ASSERT_APPROXEQ(tmpout(10),1.0_SRK,'tmpout(10)')
      ASSERT_APPROXEQ(tmpout(11),0.0_SRK,'tmpout(11)')

      CALL testMT%clear()

    ENDSUBROUTINE Test1DBase
!
!-------------------------------------------------------------------------------
    SUBROUTINE Test1Dsetup()
      INTEGER(SIK) :: i,j
      REAL(SRK) :: x
      REAL(SRK),ALLOCATABLE :: mesh_in(:),mesh_out(:),sol(:)
      REAL(SRK),ALLOCATABLE :: TM(:,:)
      TYPE(LinearSolverType_Direct) :: TLS

      COMPONENT_TEST('setupP2P')
      ALLOCATE(mesh_in(4),mesh_out(6),sol(24))
      mesh_in=(/0.0_SRK, 1.5_SRK, 2.0_SRK, 5.0_SRK/)
      mesh_out=(/0.3_SRK, 1.55_SRK, 2.0_SRK, 3.0_SRK,-1.0_SRK,6.0_SRK/)
      sol=(/0.8_SRK,0.2_SRK,ZERO,ZERO,         &
            ZERO,0.9_SRK,0.1_SRK,ZERO,         &
            ZERO,ZERO,ONE,ZERO,                &
            ZERO,ZERO,TWO/3.0_SRK,ONE/3.0_SRK, &
            ONE,ZERO,ZERO,ZERO,                &
            ZERO,ZERO,ZERO,ONE/)
      CALL testsetupP2P(TM,mesh_in,mesh_out)
      ASSERT(ALLOCATED(TM),'TM allocated')
      DO i=1,6
        DO j=1,4
          ASSERT_APPROXEQ(TM(i,j),sol((i-1)*4+j),'TM values')
          FINFO() i,j
        ENDDO
      ENDDO
      DEALLOCATE(mesh_in,mesh_out,sol,TM)

      COMPONENT_TEST('setupV2P')
      ALLOCATE(mesh_in(4),mesh_out(6),sol(18))
      mesh_in=(/0.0_SRK, 1.5_SRK, 2.0_SRK, 5.0_SRK/)
      mesh_out=(/0.0_SRK, 1.55_SRK, 2.0_SRK, 5.0_SRK,-1.0_SRK,6.0_SRK/)
      sol=(/ ONE,ZERO,ZERO, &
            ZERO, ONE,ZERO, &
            ZERO,HALF,HALF, &
            ZERO,ZERO, ONE, &
             ONE,ZERO,ZERO, &
            ZERO,ZERO, ONE /)
      CALL testsetupV2P(TM,mesh_in,mesh_out)
      ASSERT(ALLOCATED(TM),'TM allocated')
      DO i=1,6
        DO j=1,3
          ASSERT_APPROXEQ(TM(i,j),sol((i-1)*3+j),'TM values')
          FINFO() i,j
        ENDDO
      ENDDO
      DEALLOCATE(mesh_in,mesh_out,sol,TM)

      COMPONENT_TEST('setupC2C')
      ALLOCATE(sol(12))
      sol=(/ ONE,ZERO,ZERO, &
            ZERO, ONE,ZERO, &
            ZERO,ZERO, ONE, &
            ZERO,ZERO,ZERO /)
      CALL testsetupC2C(TM,3,4)
      ASSERT(ALLOCATED(TM),'TM allocated')
      DO i=1,4
        DO j=1,3
          ASSERT_APPROXEQ(TM(i,j),sol((i-1)*3+j),'TM values extra moments')
          FINFO() i,j
        ENDDO
      ENDDO
      DEALLOCATE(TM)
      sol=(/ ONE,ZERO,ZERO,ZERO, &
            ZERO, ONE,ZERO,ZERO, &
            ZERO,ZERO, ONE,ZERO /)
      CALL testsetupC2C(TM,4,3)
      ASSERT(ALLOCATED(TM),'TM allocated')
      DO i=1,3
        DO j=1,4
          ASSERT_APPROXEQ(TM(i,j),sol((i-1)*4+j),'TM values less moments')
          FINFO() i,j
        ENDDO
      ENDDO
      DEALLOCATE(sol,TM)

      COMPONENT_TEST('setupP2V_cart')
      ALLOCATE(mesh_in(4),mesh_out(7),sol(24))
      mesh_in=(/0.0_SRK, 1.0_SRK, 2.5_SRK, 4.5_SRK/)
      mesh_out=(/-0.5_SRK, 0.5_SRK, 0.6_SRK, 1.6_SRK, 2.5_SRK, 3.0_SRK, 6.0_SRK/)
      sol=(/ 0.875_SRK, 0.125_SRK,      ZERO,      ZERO, &
              0.45_SRK,  0.55_SRK,      ZERO,      ZERO, &
              0.08_SRK,   0.8_SRK,  0.12_SRK,      ZERO, &
                  ZERO,   0.3_SRK,   0.7_SRK,      ZERO, &
                  ZERO,      ZERO, 0.875_SRK, 0.125_SRK, &
                  ZERO,      ZERO,0.1875_SRK,0.8125_SRK /)
      CALL testsetupP2V_cart(TM,mesh_in,mesh_out)
      ASSERT(ALLOCATED(TM),'TM allocated')
      DO i=1,6
        DO j=1,4
          ASSERT_APPROXEQ(TM(i,j),sol((i-1)*4+j),'TM values extra moments')
          FINFO() i,j
        ENDDO
      ENDDO
      ! More asserts when the routine is written
      DEALLOCATE(mesh_in,mesh_out,sol,TM)

      COMPONENT_TEST('setupP2C_cart')
      ALLOCATE(mesh_in(4))
      mesh_in=(/0.0_SRK, 1.5_SRK, 2.0_SRK, 5.0_SRK/)
      CALL testsetupP2C_cart(TLS,mesh_in,0.0_SRK,6.0_SRK)
      ASSERT(TLS%isInit,'TLS initialized')
      CALL TLS%A%get(1,1,x)
      ASSERT_APPROXEQ(x,LPPointVal((/ ONE,ZERO/),   ZERO,6.0_SRK,3.0_SRK),'TLS values')
      CALL TLS%A%get(1,2,x)
      ASSERT_APPROXEQ(x,LPPointVal((/ZERO, ONE/),   ZERO,6.0_SRK,3.0_SRK),'TLS values')
      CALL TLS%A%get(3,1,x)
      ASSERT_APPROXEQ(x,LPPointVal((/ ONE,ZERO/),    TWO,6.0_SRK,3.0_SRK),'TLS values')
      CALL TLS%A%get(3,2,x)
      ASSERT_APPROXEQ(x,LPPointVal((/ZERO, ONE/),    TWO,6.0_SRK,3.0_SRK),'TLS values')
      CALL TLS%A%get(4,1,x)
      ASSERT_APPROXEQ(x,LPPointVal((/ ONE,ZERO/),5.0_SRK,6.0_SRK,3.0_SRK),'TLS values')
      CALL TLS%A%get(4,2,x)
      ASSERT_APPROXEQ(x,LPPointVal((/ZERO, ONE/),5.0_SRK,6.0_SRK,3.0_SRK),'TLS values')
      DEALLOCATE(mesh_in)
      CALL TLS%clear()

      COMPONENT_TEST('setupV2V_cart')
      ALLOCATE(mesh_in(5),mesh_out(6),sol(20))
      mesh_in=(/0.0_SRK, 1.5_SRK, 2.0_SRK, 3.5_SRK,5.0_SRK/)
      mesh_out=(/0.25_SRK, 0.6_SRK, 1.6_SRK, 1.75_SRK, 2.0_SRK, 4.0_SRK/)
      sol=(/ ONE,ZERO,ZERO,ZERO,   &
        0.9_SRK,0.1_SRK,ZERO,ZERO, &
            ZERO, ONE,ZERO,ZERO,   &
            ZERO, ONE,ZERO,ZERO,   &
            ZERO,ZERO,0.75_SRK,0.25_SRK/)
      CALL testsetupV2V_cart(TM,mesh_in,mesh_out)
      ASSERT(ALLOCATED(TM),'TM allocated')
      DO i=1,5
        DO j=1,4
          ASSERT_APPROXEQ(TM(i,j),sol((i-1)*4+j),'TM values less moments')
          FINFO() i,j
        ENDDO
      ENDDO
      DEALLOCATE(mesh_in,mesh_out,sol,TM)

      ALLOCATE(mesh_in(5),mesh_out(6),sol(20))
      mesh_in=(/0.0_SRK, 1.5_SRK, 2.0_SRK, 3.5_SRK,5.0_SRK/)
      mesh_out=(/-1.0_SRK, 0.6_SRK, 1.6_SRK, 1.75_SRK, 2.0_SRK, 6.0_SRK/)
      sol=(/ ONE,ZERO,ZERO,ZERO,   &
        0.9_SRK,0.1_SRK,ZERO,ZERO, &
            ZERO, ONE,ZERO,ZERO,   &
            ZERO, ONE,ZERO,ZERO,   &
            ZERO,ZERO,0.375_SRK,0.625_SRK/)
      CALL testsetupV2V_cart(TM,mesh_in,mesh_out)
      ASSERT(ALLOCATED(TM),'TM allocated')
      DO i=1,5
        DO j=1,4
          ASSERT_APPROXEQ(TM(i,j),sol((i-1)*4+j),'TM values less moments')
          FINFO() i,j
        ENDDO
      ENDDO
      DEALLOCATE(mesh_in,mesh_out,sol,TM)

      COMPONENT_TEST('setupV2C_cart')
      ALLOCATE(mesh_in(4))
      mesh_in=(/0.0_SRK, 1.5_SRK, 2.0_SRK, 5.0_SRK/)
      CALL testsetupV2C_cart(TLS,mesh_in,0.0_SRK,5.0_SRK)
      ASSERT(TLS%isInit,'TLS initialized')
      CALL TLS%A%get(1,1,x)
      ASSERT_APPROXEQ(x,LPIntegral((/ ONE,ZERO/),   ZERO,1.5_SRK,5.0_SRK,2.5_SRK),'TLS values')
      CALL TLS%A%get(1,2,x)
      ASSERT_APPROXEQ(x,LPIntegral((/ZERO, ONE/),   ZERO,1.5_SRK,5.0_SRK,2.5_SRK),'TLS values')
      CALL TLS%A%get(2,1,x)
      ASSERT_APPROXEQ(x,LPIntegral((/ ONE,ZERO/),1.5_SRK,    TWO,5.0_SRK,2.5_SRK),'TLS values')
      CALL TLS%A%get(2,2,x)
      ASSERT_APPROXEQ(x,LPIntegral((/ZERO, ONE/),1.5_SRK,    TWO,5.0_SRK,2.5_SRK),'TLS values')
      CALL TLS%A%get(3,1,x)
      ASSERT_APPROXEQ(x,LPIntegral((/ ONE,ZERO/),    TWO,5.0_SRK,5.0_SRK,2.5_SRK),'TLS values')
      CALL TLS%A%get(3,2,x)
      ASSERT_APPROXEQ(x,LPIntegral((/ZERO, ONE/),    TWO,5.0_SRK,5.0_SRK,2.5_SRK),'TLS values')
      DEALLOCATE(mesh_in)
      CALL TLS%clear()

      COMPONENT_TEST('setupC2P_cart')
      ALLOCATE(mesh_out(4))
      mesh_out=(/0.3_SRK, 1.55_SRK, 2.0_SRK, 3.0_SRK/)
      CALL testsetupC2P_cart(TM,3,mesh_out,0.0_SRK,5.0_SRK)
      ASSERT(ALLOCATED(TM),'TM allocated')
      ASSERT_APPROXEQ(TM(1,1),LPPointVal((/ ONE,ZERO/),0.3_SRK,5.0_SRK,2.5_SRK),'TM Values')
      ASSERT_APPROXEQ(TM(1,2),LPPointVal((/ZERO, ONE/),0.3_SRK,5.0_SRK,2.5_SRK),'TM Values')
      ASSERT_APPROXEQ(TM(3,1),LPPointVal((/ ONE,ZERO/),2.0_SRK,5.0_SRK,2.5_SRK),'TM Values')
      ASSERT_APPROXEQ(TM(3,2),LPPointVal((/ZERO, ONE/),2.0_SRK,5.0_SRK,2.5_SRK),'TM Values')
      ASSERT_APPROXEQ(TM(4,1),LPPointVal((/ ONE,ZERO/),3.0_SRK,5.0_SRK,2.5_SRK),'TM Values')
      ASSERT_APPROXEQ(TM(4,2),LPPointVal((/ZERO, ONE/),3.0_SRK,5.0_SRK,2.5_SRK),'TM Values')
      DEALLOCATE(mesh_out,TM)

      COMPONENT_TEST('setupC2V_cart')
      ALLOCATE(mesh_out(4))
      mesh_out=(/0.3_SRK, 1.55_SRK, 2.0_SRK, 3.0_SRK/)
      CALL testsetupC2V_cart(TM,3,mesh_out,0.0_SRK,5.0_SRK)
      ASSERT(ALLOCATED(TM),'TM allocated')
      ASSERT_APPROXEQ(TM(1,1),LPIntegral((/ ONE,ZERO/), 0.3_SRK,1.55_SRK,5.0_SRK,2.5_SRK),'TM Values')
      ASSERT_APPROXEQ(TM(1,2),LPIntegral((/ZERO, ONE/), 0.3_SRK,1.55_SRK,5.0_SRK,2.5_SRK),'TM Values')
      ASSERT_APPROXEQ(TM(2,1),LPIntegral((/ ONE,ZERO/),1.55_SRK, 2.0_SRK,5.0_SRK,2.5_SRK),'TM Values')
      ASSERT_APPROXEQ(TM(2,2),LPIntegral((/ZERO, ONE/),1.55_SRK, 2.0_SRK,5.0_SRK,2.5_SRK),'TM Values')
      ASSERT_APPROXEQ(TM(3,1),LPIntegral((/ ONE,ZERO/), 2.0_SRK, 3.0_SRK,5.0_SRK,2.5_SRK),'TM Values')
      ASSERT_APPROXEQ(TM(3,2),LPIntegral((/ZERO, ONE/), 2.0_SRK, 3.0_SRK,5.0_SRK,2.5_SRK),'TM Values')
      DEALLOCATE(mesh_out,TM)

      COMPONENT_TEST('setupP2V_cyl')
      ALLOCATE(mesh_in(2),mesh_out(4),sol(6))
      mesh_in=(/0.0_SRK, 2.0_SRK/)
      mesh_out=(/0.5_SRK, 1.0_SRK, 1.5_SRK, 2.0_SRK/)
      sol=(/ 11.0_SRK/18.0_SRK, 7.0_SRK/18.0_SRK, &
             11.0_SRK/30.0_SRK,19.0_SRK/30.0_SRK, &
              5.0_SRK/42.0_SRK,37.0_SRK/42.0_SRK /)
      CALL testsetupP2V_cyl(TM,mesh_in,mesh_out)
      ASSERT(ALLOCATED(TM),'TM allocated')
      DO i=1,3
        DO j=1,2
          ASSERT_APPROXEQ(TM(i,j),sol((i-1)*2+j),'TM values less moments')
          FINFO() i,j
        ENDDO
      ENDDO
      DEALLOCATE(mesh_in,mesh_out,sol,TM)

      COMPONENT_TEST('setupP2C_cyl')
      ALLOCATE(mesh_in(4))
      mesh_in=(/0.0_SRK, 1.5_SRK, 2.0_SRK, 5.0_SRK/)
      CALL testsetupP2C_cyl(TLS,mesh_in,6.0_SRK)
      ASSERT(TLS%isInit,'TLS initialized')
      CALL TLS%A%get(1,1,x)
      ASSERT_APPROXEQ(x,ZPPointVal((/ ONE,ZERO/),   ZERO,6.0_SRK),'TLS values')
      CALL TLS%A%get(1,2,x)
      ASSERT_APPROXEQ(x,ZPPointVal((/ZERO, ONE/),   ZERO,6.0_SRK),'TLS values')
      CALL TLS%A%get(3,1,x)
      ASSERT_APPROXEQ(x,ZPPointVal((/ ONE,ZERO/),    TWO,6.0_SRK),'TLS values')
      CALL TLS%A%get(3,2,x)
      ASSERT_APPROXEQ(x,ZPPointVal((/ZERO, ONE/),    TWO,6.0_SRK),'TLS values')
      CALL TLS%A%get(4,1,x)
      ASSERT_APPROXEQ(x,ZPPointVal((/ ONE,ZERO/),5.0_SRK,6.0_SRK),'TLS values')
      CALL TLS%A%get(4,2,x)
      ASSERT_APPROXEQ(x,ZPPointVal((/ZERO, ONE/),5.0_SRK,6.0_SRK),'TLS values')
      DEALLOCATE(mesh_in)
      CALL TLS%clear()

      COMPONENT_TEST('setupV2V_cyl')
      ALLOCATE(mesh_in(5),mesh_out(6),sol(20))
      mesh_in=(/0.0_SRK, 1.5_SRK, 2.0_SRK, 3.5_SRK,5.0_SRK/)
      mesh_out=(/0.25_SRK, 0.6_SRK, 1.6_SRK, 1.75_SRK, 2.0_SRK, 4.0_SRK/)
      sol=(/ ONE,ZERO,ZERO,ZERO,   &
        (1.5_SRK**2-0.6_SRK**2)/(1.6_SRK**2-0.6_SRK**2), &
              (1.6_SRK**2-1.5_SRK**2)/(1.6_SRK**2-0.6_SRK**2),ZERO,ZERO, &
            ZERO, ONE,ZERO,ZERO,   &
            ZERO, ONE,ZERO,ZERO,   &
            ZERO,ZERO,0.6875_SRK,0.3125_SRK/)
      CALL testsetupV2V_cyl(TM,mesh_in,mesh_out)
      ASSERT(ALLOCATED(TM),'TM allocated')
      DO i=1,5
        DO j=1,4
          ASSERT_APPROXEQ(TM(i,j),sol((i-1)*4+j),'TM values less moments')
          FINFO() i,j
        ENDDO
      ENDDO
      DEALLOCATE(mesh_in,mesh_out,sol,TM)

      ALLOCATE(mesh_in(5),mesh_out(6),sol(20))
      mesh_in=(/1.0_SRK, 1.5_SRK, 2.0_SRK, 3.5_SRK,5.0_SRK/)
      mesh_out=(/0.0_SRK, 0.6_SRK, 1.6_SRK, 1.75_SRK, 2.0_SRK, 6.0_SRK/)
      sol=(/ ONE,ZERO,ZERO,ZERO,   &
        (1.5_SRK**2-0.6_SRK**2)/(1.6_SRK**2-0.6_SRK**2), &
              (1.6_SRK**2-1.5_SRK**2)/(1.6_SRK**2-0.6_SRK**2),ZERO,ZERO, &
            ZERO, ONE,ZERO,ZERO,   &
            ZERO, ONE,ZERO,ZERO,   &
            ZERO,ZERO,(3.5_SRK**2-2.0_SRK**2)/(6.0_SRK**2-2.0_SRK**2), &
                      (6.0_SRK**2-3.5_SRK**2)/(6.0_SRK**2-2.0_SRK**2)/)
      CALL testsetupV2V_cyl(TM,mesh_in,mesh_out)
      ASSERT(ALLOCATED(TM),'TM allocated')
      DO i=1,5
        DO j=1,4
          ASSERT_APPROXEQ(TM(i,j),sol((i-1)*4+j),'TM values less moments')
          FINFO() i,j
        ENDDO
      ENDDO
      DEALLOCATE(mesh_in,mesh_out,sol,TM)

      COMPONENT_TEST('setupV2C_cyl')
      ALLOCATE(mesh_in(4))
      mesh_in=(/0.0_SRK, 1.5_SRK, 2.0_SRK, 5.0_SRK/)
      CALL testsetupV2C_cyl(TLS,mesh_in,6.0_SRK)
      ASSERT(TLS%isInit,'TLS initialized')
      CALL TLS%A%get(1,1,x)
      ASSERT_APPROXEQ(x,ZPIntegral((/ ONE,ZERO/),   ZERO,1.5_SRK,6.0_SRK),'TLS values')
      CALL TLS%A%get(1,2,x)
      ASSERT_APPROXEQ(x,ZPIntegral((/ZERO, ONE/),   ZERO,1.5_SRK,6.0_SRK),'TLS values')
      CALL TLS%A%get(2,1,x)
      ASSERT_APPROXEQ(x,ZPIntegral((/ ONE,ZERO/),1.5_SRK,    TWO,6.0_SRK),'TLS values')
      CALL TLS%A%get(2,2,x)
      ASSERT_APPROXEQ(x,ZPIntegral((/ZERO, ONE/),1.5_SRK,    TWO,6.0_SRK),'TLS values')
      CALL TLS%A%get(3,1,x)
      ASSERT_APPROXEQ(x,ZPIntegral((/ ONE,ZERO/),    TWO,5.0_SRK,6.0_SRK),'TLS values')
      CALL TLS%A%get(3,2,x)
      ASSERT_APPROXEQ(x,ZPIntegral((/ZERO, ONE/),    TWO,5.0_SRK,6.0_SRK),'TLS values')
      DEALLOCATE(mesh_in)
      CALL TLS%clear()

      COMPONENT_TEST('setupC2P_cyl')
      ALLOCATE(mesh_out(4))
      mesh_out=(/0.3_SRK, 1.55_SRK, 2.0_SRK, 3.0_SRK/)
      CALL testsetupC2P_cyl(TM,3,mesh_out,6.0_SRK)
      ASSERT(ALLOCATED(TM),'TM allocated')
      ASSERT_APPROXEQ(TM(1,1),ZPPointVal((/ ONE,ZERO/),0.3_SRK,6.0_SRK),'TM Values')
      ASSERT_APPROXEQ(TM(1,2),ZPPointVal((/ZERO, ONE/),0.3_SRK,6.0_SRK),'TM Values')
      ASSERT_APPROXEQ(TM(3,1),ZPPointVal((/ ONE,ZERO/),2.0_SRK,6.0_SRK),'TM Values')
      ASSERT_APPROXEQ(TM(3,2),ZPPointVal((/ZERO, ONE/),2.0_SRK,6.0_SRK),'TM Values')
      ASSERT_APPROXEQ(TM(4,1),ZPPointVal((/ ONE,ZERO/),3.0_SRK,6.0_SRK),'TM Values')
      ASSERT_APPROXEQ(TM(4,2),ZPPointVal((/ZERO, ONE/),3.0_SRK,6.0_SRK),'TM Values')
      DEALLOCATE(mesh_out,TM)

      COMPONENT_TEST('setupC2V_cyl')
      ALLOCATE(mesh_out(4))
      mesh_out=(/0.3_SRK, 1.55_SRK, 2.0_SRK, 3.0_SRK/)
      CALL testsetupC2V_cyl(TM,4,mesh_out,6.0_SRK)
      ASSERT(ALLOCATED(TM),'TM allocated')
      ASSERT_APPROXEQ(TM(1,1),ZPIntegral((/ ONE,ZERO/), 0.3_SRK,1.55_SRK,6.0_SRK),'TM Values')
      ASSERT_APPROXEQ(TM(1,2),ZPIntegral((/ZERO, ONE/), 0.3_SRK,1.55_SRK,6.0_SRK),'TM Values')
      ASSERT_APPROXEQ(TM(2,1),ZPIntegral((/ ONE,ZERO/),1.55_SRK, 2.0_SRK,6.0_SRK),'TM Values')
      ASSERT_APPROXEQ(TM(2,2),ZPIntegral((/ZERO, ONE/),1.55_SRK, 2.0_SRK,6.0_SRK),'TM Values')
      ASSERT_APPROXEQ(TM(3,1),ZPIntegral((/ ONE,ZERO/), 2.0_SRK, 3.0_SRK,6.0_SRK),'TM Values')
      ASSERT_APPROXEQ(TM(3,2),ZPIntegral((/ZERO, ONE/), 2.0_SRK, 3.0_SRK,6.0_SRK),'TM Values')
      DEALLOCATE(mesh_out,TM)
    ENDSUBROUTINE Test1Dsetup
!
!-------------------------------------------------------------------------------
    SUBROUTINE Test1DCart()
      TYPE(MeshTransfer_1DCart) :: testMT
      TYPE(ParamType) :: testPL
      INTEGER(SIK) :: i
      REAL(SRK) :: mesh(6),mesh_out(6)

      mesh=(/0.0_SRK,1.0_SRK,3.0_SRK,4.0_SRK,6.5_SRK,10.0_SRK/)
      DO i=0,5
        mesh_out(i+1)=REAL(i,SRK)/2.0_SRK
      ENDDO

      COMPONENT_TEST('Cartesian P2P')
      CALL testPL%add('MeshTransfer->map_in','POINT')
      CALL testPL%add('MeshTransfer->map_out','POINT')
      CALL testPL%add('MeshTransfer->pointmesh_in',mesh)
      CALL testPL%add('MeshTransfer->pointmesh_out',mesh_out)
      CALL testMT%init(testPL)
      ASSERT(ALLOCATED(testMT%TransferMatrix),'Transfer Matrix')
      ASSERT(.NOT. testMT%transferLS%isInit,'Transfer Linear System')
      CALL testMT%clear()

      COMPONENT_TEST('Cartesian P2P Poly Transfer')
      CALL testPL%add('MeshTransfer->poly_transfer',4)
      CALL testMT%init(testPL)
      ASSERT(ALLOCATED(testMT%TransferMatrix),'Transfer Matrix')
      ASSERT(testMT%transferLS%isInit,'Transfer Linear System')
      CALL testMT%clear()
      CALL testPL%clear()

      COMPONENT_TEST('Cartesian P2V')
      CALL testPL%add('MeshTransfer->map_in','POINT')
      CALL testPL%add('MeshTransfer->map_out','VOLUME')
      CALL testPL%add('MeshTransfer->pointmesh_in',mesh)
      CALL testPL%add('MeshTransfer->volumemesh_out',mesh_out)
      CALL testMT%init(testPL)
      ASSERT(ALLOCATED(testMT%TransferMatrix),'Transfer Matrix')
      ASSERT(.NOT. testMT%transferLS%isInit,'Transfer Linear System')
      CALL testMT%clear()

      COMPONENT_TEST('Cartesian P2V Poly Transfer')
      CALL testPL%add('MeshTransfer->poly_transfer',4)
      CALL testMT%init(testPL)
      ASSERT(ALLOCATED(testMT%TransferMatrix),'Transfer Matrix')
      ASSERT(testMT%transferLS%isInit,'Transfer Linear System')
      CALL testMT%clear()
      CALL testPL%clear()

      COMPONENT_TEST('Cartesian V2P')
      CALL testPL%add('MeshTransfer->map_in','VOLUME')
      CALL testPL%add('MeshTransfer->map_out','POINT')
      CALL testPL%add('MeshTransfer->volumemesh_in',mesh)
      CALL testPL%add('MeshTransfer->pointmesh_out',mesh_out)
      CALL testMT%init(testPL)
      ASSERT(ALLOCATED(testMT%TransferMatrix),'Transfer Matrix')
      ASSERT(.NOT. testMT%transferLS%isInit,'Transfer Linear System')
      CALL testMT%clear()

      COMPONENT_TEST('Cartesian V2P Poly Transfer')
      CALL testPL%add('MeshTransfer->poly_transfer',4)
      CALL testMT%init(testPL)
      ASSERT(ALLOCATED(testMT%TransferMatrix),'Transfer Matrix')
      ASSERT(testMT%transferLS%isInit,'Transfer Linear System')
      CALL testMT%clear()
      CALL testPL%clear()

      COMPONENT_TEST('Cartesian V2V')
      CALL testPL%add('MeshTransfer->map_in','VOLUME')
      CALL testPL%add('MeshTransfer->map_out','VOLUME')
      CALL testPL%add('MeshTransfer->volumemesh_in',mesh)
      CALL testPL%add('MeshTransfer->volumemesh_out',mesh_out)
      CALL testMT%init(testPL)
      ASSERT(ALLOCATED(testMT%TransferMatrix),'Transfer Matrix')
      ASSERT(.NOT. testMT%transferLS%isInit,'Transfer Linear System')
      CALL testMT%clear()

      COMPONENT_TEST('Cartesian V2V Poly Transfer')
      CALL testPL%add('MeshTransfer->poly_transfer',4)
      CALL testMT%init(testPL)
      ASSERT(ALLOCATED(testMT%TransferMatrix),'Transfer Matrix')
      ASSERT(testMT%transferLS%isInit,'Transfer Linear System')
      CALL testMT%clear()
      CALL testPL%clear()

      COMPONENT_TEST('Cartesian P2C')
      CALL testPL%add('MeshTransfer->map_in','POINT')
      CALL testPL%add('MeshTransfer->map_out','CONTINUOUS')
      CALL testPL%add('MeshTransfer->pointmesh_in',mesh)
      CALL testPL%add('MeshTransfer->moments_out',3)
      CALL testMT%init(testPL)
      ASSERT(.NOT. ALLOCATED(testMT%TransferMatrix),'Transfer Matrix')
      ASSERT(testMT%transferLS%isInit,'Transfer Linear System')
      CALL testMT%clear()
      CALL testPL%clear()

      COMPONENT_TEST('Cartesian V2C')
      CALL testPL%add('MeshTransfer->map_in','VOLUME')
      CALL testPL%add('MeshTransfer->map_out','CONTINUOUS')
      CALL testPL%add('MeshTransfer->volumemesh_in',mesh)
      CALL testPL%add('MeshTransfer->moments_out',3)
      CALL testMT%init(testPL)
      ASSERT(.NOT. ALLOCATED(testMT%TransferMatrix),'Transfer Matrix')
      ASSERT(testMT%transferLS%isInit,'Transfer Linear System')
      CALL testMT%clear()
      CALL testPL%clear()

      COMPONENT_TEST('Cartesian C2P')
      CALL testPL%add('MeshTransfer->map_in','CONTINUOUS')
      CALL testPL%add('MeshTransfer->map_out','POINT')
      CALL testPL%add('MeshTransfer->moments_in',3)
      CALL testPL%add('MeshTransfer->pointmesh_out',mesh)
      CALL testMT%init(testPL)
      ASSERT(ALLOCATED(testMT%TransferMatrix),'Transfer Matrix')
      ASSERT(.NOT. testMT%transferLS%isInit,'Transfer Linear System')
      CALL testMT%clear()
      CALL testPL%clear()

      COMPONENT_TEST('Cartesian C2V')
      CALL testPL%add('MeshTransfer->map_in','CONTINUOUS')
      CALL testPL%add('MeshTransfer->map_out','VOLUME')
      CALL testPL%add('MeshTransfer->moments_in',3)
      CALL testPL%add('MeshTransfer->volumemesh_out',mesh)
      CALL testPL%add('MeshTransfer->maxRange',12.0_SRK)
      CALL testMT%init(testPL)
      ASSERT(ALLOCATED(testMT%TransferMatrix),'Transfer Matrix')
      ASSERT(.NOT. testMT%transferLS%isInit,'Transfer Linear System')
      CALL testMT%clear()
      CALL testPL%clear()

      COMPONENT_TEST('Cartesian C2C')
      CALL testPL%add('MeshTransfer->map_in','CONTINUOUS')
      CALL testPL%add('MeshTransfer->map_out','CONTINUOUS')
      CALL testPL%add('MeshTransfer->moments_in',3)
      CALL testPL%add('MeshTransfer->moments_out',2)
      CALL testMT%init(testPL)
      ASSERT(ALLOCATED(testMT%TransferMatrix),'Transfer Matrix')
      ASSERT(.NOT. testMT%transferLS%isInit,'Transfer Linear System')
      CALL testMT%clear()
      CALL testPL%clear()
    ENDSUBROUTINE Test1DCart
!
!-------------------------------------------------------------------------------
    SUBROUTINE Test1DCyl()
      TYPE(MeshTransfer_1DCyl) :: testMT
      TYPE(ParamType) :: testPL
      INTEGER(SIK) :: i
      REAL(SRK) :: mesh(6),mesh_out(6)

      mesh=(/0.0_SRK,1.0_SRK,3.0_SRK,4.0_SRK,6.5_SRK,10.0_SRK/)
      DO i=0,5
        mesh_out(i+1)=REAL(i,SRK)/2.0_SRK
      ENDDO

      COMPONENT_TEST('Cylindrical P2P')
      CALL testPL%add('MeshTransfer->map_in','POINT')
      CALL testPL%add('MeshTransfer->map_out','POINT')
      CALL testPL%add('MeshTransfer->pointmesh_in',mesh)
      CALL testPL%add('MeshTransfer->pointmesh_out',mesh_out)
      CALL testMT%init(testPL)
      ASSERT(ALLOCATED(testMT%TransferMatrix),'Transfer Matrix')
      ASSERT(.NOT. testMT%transferLS%isInit,'Transfer Linear System')
      CALL testMT%clear()

      COMPONENT_TEST('Cylindrical P2P Poly Transfer')
      CALL testPL%add('MeshTransfer->poly_transfer',4)
      CALL testMT%init(testPL)
      ASSERT(ALLOCATED(testMT%TransferMatrix),'Transfer Matrix')
      ASSERT(testMT%transferLS%isInit,'Transfer Linear System')
      CALL testMT%clear()
      CALL testPL%clear()

      COMPONENT_TEST('Cylindrical P2V')
      CALL testPL%add('MeshTransfer->map_in','POINT')
      CALL testPL%add('MeshTransfer->map_out','VOLUME')
      CALL testPL%add('MeshTransfer->pointmesh_in',mesh)
      CALL testPL%add('MeshTransfer->volumemesh_out',mesh_out)
      CALL testMT%init(testPL)
      ASSERT(ALLOCATED(testMT%TransferMatrix),'Transfer Matrix')
      ASSERT(.NOT. testMT%transferLS%isInit,'Transfer Linear System')
      CALL testMT%clear()

      COMPONENT_TEST('Cylindrical P2V Poly Transfer')
      CALL testPL%add('MeshTransfer->poly_transfer',4)
      CALL testMT%init(testPL)
      ASSERT(ALLOCATED(testMT%TransferMatrix),'Transfer Matrix')
      ASSERT(testMT%transferLS%isInit,'Transfer Linear System')
      CALL testMT%clear()
      CALL testPL%clear()

      COMPONENT_TEST('Cylindrical V2P')
      CALL testPL%add('MeshTransfer->map_in','VOLUME')
      CALL testPL%add('MeshTransfer->map_out','POINT')
      CALL testPL%add('MeshTransfer->volumemesh_in',mesh)
      CALL testPL%add('MeshTransfer->pointmesh_out',mesh_out)
      CALL testMT%init(testPL)
      ASSERT(ALLOCATED(testMT%TransferMatrix),'Transfer Matrix')
      ASSERT(.NOT. testMT%transferLS%isInit,'Transfer Linear System')
      CALL testMT%clear()

      COMPONENT_TEST('Cylindrical V2P Poly Transfer')
      CALL testPL%add('MeshTransfer->poly_transfer',4)
      CALL testMT%init(testPL)
      ASSERT(ALLOCATED(testMT%TransferMatrix),'Transfer Matrix')
      ASSERT(testMT%transferLS%isInit,'Transfer Linear System')
      CALL testMT%clear()
      CALL testPL%clear()

      COMPONENT_TEST('Cylindrical V2V')
      CALL testPL%add('MeshTransfer->map_in','VOLUME')
      CALL testPL%add('MeshTransfer->map_out','VOLUME')
      CALL testPL%add('MeshTransfer->volumemesh_in',mesh)
      CALL testPL%add('MeshTransfer->volumemesh_out',mesh_out)
      CALL testMT%init(testPL)
      ASSERT(ALLOCATED(testMT%TransferMatrix),'Transfer Matrix')
      ASSERT(.NOT. testMT%transferLS%isInit,'Transfer Linear System')
      CALL testMT%clear()

      COMPONENT_TEST('Cylindrical V2V Poly Transfer')
      CALL testPL%add('MeshTransfer->poly_transfer',4)
      CALL testMT%init(testPL)
      ASSERT(ALLOCATED(testMT%TransferMatrix),'Transfer Matrix')
      ASSERT(testMT%transferLS%isInit,'Transfer Linear System')
      CALL testMT%clear()
      CALL testPL%clear()

      COMPONENT_TEST('Cylindrical P2C')
      CALL testPL%add('MeshTransfer->map_in','POINT')
      CALL testPL%add('MeshTransfer->map_out','CONTINUOUS')
      CALL testPL%add('MeshTransfer->pointmesh_in',mesh)
      CALL testPL%add('MeshTransfer->moments_out',3)
      CALL testMT%init(testPL)
      ASSERT(.NOT. ALLOCATED(testMT%TransferMatrix),'Transfer Matrix')
      ASSERT(testMT%transferLS%isInit,'Transfer Linear System')
      CALL testMT%clear()
      CALL testPL%clear()

      COMPONENT_TEST('Cylindrical V2C')
      CALL testPL%add('MeshTransfer->map_in','VOLUME')
      CALL testPL%add('MeshTransfer->map_out','CONTINUOUS')
      CALL testPL%add('MeshTransfer->volumemesh_in',mesh)
      CALL testPL%add('MeshTransfer->moments_out',3)
      CALL testMT%init(testPL)
      ASSERT(.NOT. ALLOCATED(testMT%TransferMatrix),'Transfer Matrix')
      ASSERT(testMT%transferLS%isInit,'Transfer Linear System')
      CALL testMT%clear()
      CALL testPL%clear()

      COMPONENT_TEST('Cylindrical C2P')
      CALL testPL%add('MeshTransfer->map_in','CONTINUOUS')
      CALL testPL%add('MeshTransfer->map_out','POINT')
      CALL testPL%add('MeshTransfer->moments_in',3)
      CALL testPL%add('MeshTransfer->pointmesh_out',mesh)
      CALL testMT%init(testPL)
      ASSERT(ALLOCATED(testMT%TransferMatrix),'Transfer Matrix')
      ASSERT(.NOT. testMT%transferLS%isInit,'Transfer Linear System')
      CALL testMT%clear()
      CALL testPL%clear()

      COMPONENT_TEST('Cylindrical C2V')
      CALL testPL%add('MeshTransfer->map_in','CONTINUOUS')
      CALL testPL%add('MeshTransfer->map_out','VOLUME')
      CALL testPL%add('MeshTransfer->moments_in',3)
      CALL testPL%add('MeshTransfer->volumemesh_out',mesh)
      CALL testPL%add('MeshTransfer->minRange',-2.0_SRK)
      CALL testPL%add('MeshTransfer->maxRange',12.0_SRK)
      CALL testMT%init(testPL)
      ASSERT(ALLOCATED(testMT%TransferMatrix),'Transfer Matrix')
      ASSERT(.NOT. testMT%transferLS%isInit,'Transfer Linear System')
      CALL testMT%clear()
      CALL testPL%clear()

      COMPONENT_TEST('Cylindrical C2C')
      CALL testPL%add('MeshTransfer->map_in','CONTINUOUS')
      CALL testPL%add('MeshTransfer->map_out','CONTINUOUS')
      CALL testPL%add('MeshTransfer->moments_in',3)
      CALL testPL%add('MeshTransfer->moments_out',2)
      CALL testMT%init(testPL)
      ASSERT(ALLOCATED(testMT%TransferMatrix),'Transfer Matrix')
      ASSERT(.NOT. testMT%transferLS%isInit,'Transfer Linear System')
      CALL testMT%clear()
      CALL testPL%clear()

    ENDSUBROUTINE Test1DCyl
!
!-------------------------------------------------------------------------------
    SUBROUTINE TestLPPointVal()
      REAL(SRK),ALLOCATABLE :: c(:),sol(:),ref(:)
      REAL(SRK) :: x
      INTEGER(SIK) :: i

      ALLOCATE(c(8))
      DO i=1,8
        c(i)=1.0_SRK/REAL(i,SRK)
      ENDDO

      ALLOCATE(sol(11))

      ALLOCATE(ref(11))
      ref(1)=0.6345238095238095238095238095238095238095238095238095238095238095238_SRK
      ref(2)=0.7273055428571428571428571428571428571428571428571428571428571428571_SRK
      ref(3)=0.7314336761904761904761904761904761904761904761904761904761904761905_SRK
      ref(4)=0.7992556095238095238095238095238095238095238095238095238095238095238_SRK
      ref(5)=0.8436587428571428571428571428571428571428571428571428571428571428572_SRK
      ref(6)=0.8636904761904761904761904761904761904761904761904761904761904761905_SRK
      ref(7)=0.9327862095238095238095238095238095238095238095238095238095238095238_SRK
      ref(8)=1.0658213428571428571428571428571428571428571428571428571428571428571_SRK
      ref(9)=1.1812032761904761904761904761904761904761904761904761904761904761905_SRK
      ref(10)=1.3742194095238095238095238095238095238095238095238095238095238095238_SRK
      ref(11)=2.7178571428571428571428571428571428571428571428571428571428571428572_SRK

      DO i=-5,5
        x=REAL(i,SRK)/5.0_SRK
        ASSERT_APPROXEQ(LPPointVal(c,x),ref(i+6),'Legendre Polynomial Point values does not return reference')
      ENDDO
    ENDSUBROUTINE TestLPPointVal
!
!-------------------------------------------------------------------------------
    SUBROUTINE TestLPIntegral()
      REAL(SRK),ALLOCATABLE :: c(:)
      REAL(SRK) :: ref
      INTEGER(SIK) :: i

      ALLOCATE(c(8))
      c(:)=0.0_SRK

      ! Test each moment individually
      c(1)=1.0_SRK
      ref=1.0000_SRK
      ASSERT_APPROXEQ(LPIntegral(c,-0.8_SRK,0.6_SRK), ref,"0th moment Integral")

      c(:)=0.0_SRK
      c(2)=1.0_SRK
      ref=-0.1_SRK
      ASSERT_APPROXEQ(LPIntegral(c,-0.8_SRK,0.6_SRK),ref,"1st moment Integral")

      c(:)=0.0_SRK
      c(3)=1.0_SRK
      ref=-0.24_SRK
      ASSERT_APPROXEQ(LPIntegral(c,-0.8_SRK,0.6_SRK),ref,"2nd moment Integral")

      c(:)=0.0_SRK
      c(4)=1.0_SRK
      ref=0.025_SRK
      ASSERT_APPROXEQ(LPIntegral(c,-0.8_SRK,0.6_SRK),ref,"3rd moment Integral")

      c(:)=0.0_SRK
      c(5)=1.0_SRK
      ref=-0.0216_SRK
      ASSERT_APPROXEQ(LPIntegral(c,-0.8_SRK,0.6_SRK),ref,"4th moment Integral")

      c(:)=0.0_SRK
      c(6)=1.0_SRK
      ref=0.04798_SRK
      ASSERT_APPROXEQ(LPIntegral(c,-0.8_SRK,0.6_SRK),ref,"5th moment Integral")

      c(:)=0.0_SRK
      c(7)=1.0_SRK
      ref=0.034896_SRK
      ASSERT_APPROXEQ(LPIntegral(c,-0.8_SRK,0.6_SRK),ref,"6th moment Integral")

      c(:)=0.0_SRK
      c(8)=1.0_SRK
      ref=-0.0159475_SRK
      ASSERT_APPROXEQ(LPIntegral(c,-0.8_SRK,0.6_SRK),ref,"7th moment Integral")

      ! Test all the moments together
      DO i=1,8
        c(i)=1.0_SRK/REAL(i,SRK)
      ENDDO

      ref=0.88291837202380952380952380952380952380952380952380952380952380952380952380952_SRK
      ASSERT_APPROXEQ(LPIntegral(c,-0.8_SRK,0.6_SRK),ref,"Mixed moments - unit width")
      ASSERT_APPROXEQ(LPIntegral(c,0.6_SRK,-0.8_SRK),ref,"Mixed moments - unit width reversed bounds")
      ! Scale solution up to a 20 cm node using equivalent bounds (ie same reference solution)
      ASSERT_APPROXEQ(LPIntegral(c,-8.0_SRK,6.0_SRK,h=20.0_SRK),ref,"Mixed moments - large width")

      ! Shift previous test over 20 cm
      ASSERT_APPROXEQ(LPIntegral(c,12.0_SRK,26.0_SRK,h=20.0_SRK,nodeX=20.0_SRK),ref,"Mixed moments - large width shifted")

      ref=1.0315685111002619311904027199489064514636993408203125000000000000000000000000_SRK
      ASSERT_APPROXEQ(LPIntegral(c,0.1_SRK,0.6_SRK),ref,"Mixed moments - positive bounds")

      ref=0.75727435035807388885586988180875778198242187500000000000000000000000000000000_SRK
      ASSERT_APPROXEQ(LPIntegral(c,-0.8_SRK,-0.3_SRK),ref,"Mixed moments - negative bounds")

    ENDSUBROUTINE TestLPIntegral
!
!-------------------------------------------------------------------------------
    SUBROUTINE TestZPPointVal()
      REAL(SRK),ALLOCATABLE :: c(:),ref(:)
      REAL(SRK) :: x
      INTEGER(SIK) :: i

      ALLOCATE(c(8))
      DO i=1,8
        c(i)=1.0_SRK/REAL(i,SRK)
      ENDDO

      ALLOCATE(ref(11))
      ref(1)=0.634523809523809523809523809523809523809523809523809523809_SRK
      ref(2)=0.669168466491932857142857142857142857142857142857142857142_SRK
      ref(3)=0.723459283672502857142857142857142857142857142857142857142_SRK
      ref(4)=0.730170025011319523809523809523809523809523809523809523809_SRK
      ref(5)=0.718921933651382857142857142857142857142857142857142857142_SRK
      ref(6)=0.763206263950892857142857142857142857142857142857142857142_SRK
      ref(7)=0.831615464343649523809523809523809523809523809523809523809_SRK
      ref(8)=0.860670908609852857142857142857142857142857142857142857142_SRK
      ref(9)=0.981735365541302857142857142857142857142857142857142857142_SRK
      ref(10)=1.190718355576999523809523809523809523809523809523809523809_SRK
      ref(11)=2.717857142857142857142857142857142857142857142857142857142_SRK

      DO i=0,10
        x=REAL(i,SRK)/10.0_SRK
        ASSERT_APPROXEQ(ZPPointVal(c,x),ref(i+1),'Legendre Polynomial Point values does not return reference')
      ENDDO
    ENDSUBROUTINE TestZPPointVal
!
!-------------------------------------------------------------------------------
    SUBROUTINE TestZPIntegral()
      REAL(SRK),ALLOCATABLE :: c(:)
      REAL(SRK) :: ref
      INTEGER(SIK) :: i

      ALLOCATE(c(8))
      c(:)=0.0_SRK

      ! Test each moment individually
      c(1)=1.0_SRK
      ref=1.0000_SRK
      ASSERT_APPROXEQ(ZPIntegral(c,0.3_SRK,0.6_SRK), ref,"0th moment Integral")

      c(:)=0.0_SRK
      c(2)=1.0_SRK
      ref=-0.55_SRK
      ASSERT_APPROXEQ(ZPIntegral(c,0.3_SRK,0.6_SRK),ref,"1st moment Integral")

      c(:)=0.0_SRK
      c(3)=1.0_SRK
      ref=-0.0098_SRK
      ASSERT_SOFTEQ(ZPIntegral(c,0.3_SRK,0.6_SRK),ref,1.0E-15_SRK,"2nd moment Integral")

      c(:)=0.0_SRK
      c(4)=1.0_SRK
      ref=0.308825_SRK
      ASSERT_APPROXEQ(ZPIntegral(c,0.3_SRK,0.6_SRK),ref,"3rd moment Integral")

      c(:)=0.0_SRK
      c(5)=1.0_SRK
      ref=-0.252552860_SRK
      ASSERT_APPROXEQ(ZPIntegral(c,0.3_SRK,0.6_SRK),ref,"4th moment Integral")

      c(:)=0.0_SRK
      c(6)=1.0_SRK
      ref=0.0376286570_SRK
      ASSERT_APPROXEQ(ZPIntegral(c,0.3_SRK,0.6_SRK),ref,"5th moment Integral")

      c(:)=0.0_SRK
      c(7)=1.0_SRK
      ref=0.0929795207320_SRK
      ASSERT_APPROXEQ(ZPIntegral(c,0.3_SRK,0.6_SRK),ref,"6th moment Integral")

      c(:)=0.0_SRK
      c(8)=1.0_SRK
      ref=-0.072698399532550_SRK
      ASSERT_APPROXEQ(ZPIntegral(c,0.3_SRK,0.6_SRK),ref,"7th moment Integral")

      ! Test all the moments together
      DO i=1,8
        c(i)=1.0_SRK/REAL(i,SRK)
      ENDDO

      ref=0.75889594290109791666666666666666666666666666666667_SRK
      ASSERT_APPROXEQ(ZPIntegral(c,0.3_SRK,0.6_SRK),ref,"Mixed moments - unit width")
      ASSERT_APPROXEQ(ZPIntegral(c,0.6_SRK,0.3_SRK),ref,"Mixed moments - unit width reversed bounds")
      ! Scale solution up to a 20 cm node using equivalent bounds (ie same reference solution)
      ASSERT_APPROXEQ(ZPIntegral(c,6.0_SRK,12.0_SRK,h=20.0_SRK),ref,"Mixed moments - large width")

      ref=0.78953633305305698860168457031250000000000000000000_SRK
      ASSERT_APPROXEQ(ZPIntegral(c,0.1_SRK,0.75_SRK),ref,"Mixed moments - positive bounds")


      ref=1.0000000000000000000_SRK
      ASSERT_APPROXEQ(ZPIntegral(c,0.0_SRK,1.00_SRK),ref,"Mixed moments - full integral")
    ENDSUBROUTINE TestZPIntegral
!
ENDPROGRAM testMeshTransfer
