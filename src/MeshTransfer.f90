!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!> @brief A Fortran 2003 module implementing some basic sorting algorithms.
!>
!> @par Module Dependencies
!>  - @ref IntrType "IntrType": @copybrief IntrType
!>
!> @author Ben Collins
!>    @date 12/22/2017
!>
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE MeshTransfer
#include "DBC.h"
  USE DBC
  USE IntrType
  USE Constants_Conversion
  USE Strings
  USE ParameterLists
  USE ParallelEnv
  USE BLAS
  USE ArrayUtils
  USE VectorTypes
  USE MatrixTypes
  USE LinearSolverTypes
  IMPLICIT NONE

  PUBLIC :: LPPointVal
  PUBLIC :: LPIntegral
  PUBLIC :: ZPPointVal
  PUBLIC :: ZPIntegral
  PUBLIC :: MeshTransfer_base
  PUBLIC :: MeshTransfer_1Dbase
  PUBLIC :: MeshTransfer_1DCart
  PUBLIC :: MeshTransfer_1DCyl
! Test Interfaces
  PUBLIC :: testsetupP2P
  PUBLIC :: testsetupV2P
  PUBLIC :: testsetupC2C
  PUBLIC :: testsetupP2V_cart
  PUBLIC :: testsetupP2C_cart
  PUBLIC :: testsetupV2V_cart
  PUBLIC :: testsetupV2C_cart
  PUBLIC :: testsetupC2P_cart
  PUBLIC :: testsetupC2V_cart
  PUBLIC :: testsetupP2V_cyl
  PUBLIC :: testsetupP2C_cyl
  PUBLIC :: testsetupV2V_cyl
  PUBLIC :: testsetupV2C_cyl
  PUBLIC :: testsetupC2P_cyl
  PUBLIC :: testsetupC2V_cyl

  INTEGER(SIK),PARAMETER :: MapType_CONTINUOUS=1
  INTEGER(SIK),PARAMETER :: MapType_POINT=2
  INTEGER(SIK),PARAMETER :: MapType_VOLUME=3

  TYPE,ABSTRACT :: MeshTransfer_base
    !> Indicates whether or not this object has been initialized.
    LOGICAL(SBK) :: isInit=.FALSE.
!
!List of type-bound procedures
    CONTAINS
      !> Defines interface for deferred initialization method
      PROCEDURE(mt_init_absintfc),DEFERRED,PASS :: init
      !> Defines interface for deferred clear method
      PROCEDURE(mt_clear_absintfc),DEFERRED,PASS :: clear
      !> Defines interface for deferred setup method
      PROCEDURE(mt_trnsfr_absintfc),DEFERRED,PASS :: transfer
  ENDTYPE MeshTransfer_base

  !> Defines the interface for procedures for initializing
  ABSTRACT INTERFACE
    SUBROUTINE mt_init_absintfc(this,pList)
      IMPORT :: MeshTransfer_base,ParamType
      CLASS(MeshTransfer_base),INTENT(INOUT) :: this
      CLASS(ParamType),INTENT(IN) :: pList
    ENDSUBROUTINE mt_init_absintfc
  ENDINTERFACE

  !> Defines the interface for procedures for clearing
  ABSTRACT INTERFACE
    SUBROUTINE mt_clear_absintfc(this)
      IMPORT :: MeshTransfer_base
      CLASS(MeshTransfer_base),INTENT(INOUT) :: this
    ENDSUBROUTINE mt_clear_absintfc
  ENDINTERFACE

  !> Defines the interface for procedures for transfering data
  ABSTRACT INTERFACE
    SUBROUTINE mt_trnsfr_absintfc(this,data_in,data_out)
      IMPORT :: MeshTransfer_base,SRK
      CLASS(MeshTransfer_base),INTENT(INOUT) :: this
      REAL(SRK),INTENT(IN) :: data_in(:)
      REAL(SRK),INTENT(INOUT),ALLOCATABLE :: data_out(:)
    ENDSUBROUTINE mt_trnsfr_absintfc
  ENDINTERFACE

  TYPE,ABSTRACT,EXTENDS(MeshTransfer_base) :: MeshTransfer_1Dbase
    !> Enumeration for incoming map type
    INTEGER(SIK) :: MapType_in=0
    !> Enumeration for outgoing map type
    INTEGER(SIK) :: MapType_out=0
    !> The number of incoming mesh elements (or moments for continuous)
    INTEGER(SIK) :: nmesh_in=0
    !> The number of outcoming mesh elements (or moments for continuous)
    INTEGER(SIK) :: nmesh_out=0
    !>
    INTEGER(SIK) :: poly_transfer=-1
    !>
    REAL(SRK),ALLOCATABLE :: mesh_in(:)
    !>
    REAL(SRK),ALLOCATABLE :: mesh_out(:)
    !>
    REAL(SRK),ALLOCATABLE :: transferMatrix(:,:)
    !>
    TYPE(LinearSolverType_Direct) :: transferLS
!
!List of type-bound procedures
    CONTAINS
      !> @copybrief MeshTransfer::init_1Dbase
      !> @copydetails MeshTransfer::init_1Dbase
      PROCEDURE,PASS :: init_base => init_1Dbase
      !> @copybrief MeshTransfer::clear_1Dbase
      !> @copydetails MeshTransfer::clear_1Dbase
      PROCEDURE,PASS :: clear => clear_1Dbase
      !> @copybrief MeshTransfer::clear_1Dbase
      !> @copydetails MeshTransfer::clear_1Dbase
      PROCEDURE,PASS :: transfer => transfer_1Dbase
  ENDTYPE MeshTransfer_1Dbase

  TYPE,EXTENDS(MeshTransfer_1Dbase) :: MeshTransfer_1DCart
!
!List of type-bound procedures
    CONTAINS
      !> @copybrief MeshTransfer::init_1DCart
      !> @copydetails MeshTransfer::init_1DCart
      PROCEDURE,PASS :: init => init_1DCart
  ENDTYPE MeshTransfer_1DCart

  TYPE,EXTENDS(MeshTransfer_1Dbase) :: MeshTransfer_1DCyl
!
!List of type-bound procedures
    CONTAINS
      !> @copybrief MeshTransfer::init_1DCyl
      !> @copydetails MeshTransfer::init_1DCyl
      PROCEDURE,PASS :: init => init_1DCyl
  ENDTYPE MeshTransfer_1DCyl
!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief
!> @param this  The mesh transfer object
!>
!> This routine
!>
    SUBROUTINE init_1Dbase(this,pList)
      CLASS(MeshTransfer_1Dbase),INTENT(INOUT) :: this
      CLASS(ParamType),INTENT(IN) :: pList

      INTEGER(SIK) :: i
      TYPE(StringType) :: tmpstr

      REQUIRE(pList%has('map_in'))
      REQUIRE(pList%has('map_out'))

      CALL pList%get('map_in',tmpstr)
      SELECTCASE(CHAR(tmpstr))
        CASE('CONTINUOUS')
          this%MapType_in=MapType_CONTINUOUS
        CASE('POINT')
          this%MapType_in=MapType_POINT
        CASE('VOLUME')
          this%MapType_in=MapType_VOLUME
        CASE DEFAULT
          WRITE(*,*) "invalid incomming map type"
          STOP -3
      ENDSELECT
      IF(this%MapType_in == MapType_CONTINUOUS) THEN
        CALL pList%get('moments_in',this%nmesh_in)
      ELSEIF(this%MapType_in == MapType_POINT) THEN
        CALL pList%get('pointmesh_in',this%mesh_in)
        this%nmesh_in=SIZE(this%mesh_in)
      ELSEIF(this%MapType_in == MapType_VOLUME) THEN
        CALL pList%get('volumemesh_in',this%mesh_in)
        this%nmesh_in=SIZE(this%mesh_in)-1
      ENDIF

      CALL pList%get('map_out',tmpstr)
      SELECTCASE(CHAR(tmpstr))
        CASE('CONTINUOUS')
          this%MapType_out=MapType_CONTINUOUS
        CASE('POINT')
          this%MapType_out=MapType_POINT
        CASE('VOLUME')
          this%MapType_out=MapType_VOLUME
        CASE DEFAULT
          WRITE(*,*) "invalid outgoing map type"
          STOP -3
      ENDSELECT
      IF(this%MapType_out == MapType_CONTINUOUS) THEN
        CALL pList%get('moments_out',this%nmesh_out)
      ELSEIF(this%MapType_out == MapType_POINT) THEN
        CALL pList%get('pointmesh_out',this%mesh_out)
        this%nmesh_out=SIZE(this%mesh_out)
      ELSEIF(this%MapType_out == MapType_VOLUME) THEN
        CALL pList%get('volumemesh_out',this%mesh_out)
        this%nmesh_out=SIZE(this%mesh_out)-1
      ENDIF

      IF(pList%has('poly_transfer')) THEN
        CALL pList%get('poly_transfer',this%poly_transfer)
      ENDIF

      IF(this%MapType_in /= MapType_CONTINUOUS) THEN
        DO i=2,SIZE(this%mesh_in)
          ENSURE(this%mesh_in(i)>this%mesh_in(i-1))
        ENDDO
      ENDIF
      IF(this%MapType_out /= MapType_CONTINUOUS) THEN
        DO i=2,SIZE(this%mesh_out)
          ENSURE(this%mesh_out(i)>this%mesh_out(i-1))
        ENDDO
      ENDIF

    ENDSUBROUTINE init_1Dbase
!
!-------------------------------------------------------------------------------
!> @brief
!> @param this  The mesh transfer object
!>
!> This routine
!>
    SUBROUTINE clear_1Dbase(this)
      CLASS(MeshTransfer_1Dbase),INTENT(INOUT) :: this

      this%MapType_in=0
      this%MapType_out=0
      this%nmesh_in=0
      this%nmesh_out=0
      this%poly_transfer=-1
      IF(ALLOCATED(this%mesh_in)) DEALLOCATE(this%mesh_in)
      IF(ALLOCATED(this%mesh_out)) DEALLOCATE(this%mesh_out)
      IF(ALLOCATED(this%transferMatrix)) DEALLOCATE(this%transferMatrix)
      IF(this%transferLS%isInit) CALL this%transferLS%clear()

      this%isInit=.FALSE.
    ENDSUBROUTINE clear_1Dbase
!
!-------------------------------------------------------------------------------
!> @brief
!> @param this  The mesh transfer object
!>
!> This routine
!>
    SUBROUTINE transfer_1Dbase(this,data_in,data_out)
      CLASS(MeshTransfer_1Dbase),INTENT(INOUT) :: this
      REAL(SRK),INTENT(IN) :: data_in(:)
      REAL(SRK),INTENT(INOUT),ALLOCATABLE :: data_out(:)

      INTEGER(SIK) :: i,nmom
      REAL(SRK),ALLOCATABLE :: tmp(:)

      IF(ALLOCATED(data_out)) DEALLOCATE(data_out)
      ALLOCATE(data_out(this%nmesh_out))
      data_out=ZERO

      IF(this%transferLS%isInit) THEN
        IF(ALLOCATED(this%transferMatrix)) THEN
          nmom=this%poly_transfer
        ELSE
          nmom=this%nmesh_out
        ENDIF
        ALLOCATE(tmp(nmom))
        SELECTTYPE(b => this%transferLS%b); TYPE IS(RealVectorType)
          DO i=1,this%nmesh_in
            CALL b%set(i,data_in(i))
          ENDDO
        ENDSELECT
        CALL this%transferLS%solve()

        SELECTTYPE(x => this%transferLS%x); TYPE IS(RealVectorType)
          DO i=1,nmom
            IF(i<=x%n) THEN
              CALL x%get(i,tmp(i))
            ELSE
              tmp(i)=ZERO
            ENDIF
          ENDDO
        ENDSELECT
      ELSE
        ALLOCATE(tmp(this%nmesh_in))
        tmp(:)=data_in(:)
      ENDIF

      IF(ALLOCATED(this%transferMatrix)) THEN
        CALL BLAS_MATVEC(this%transferMatrix,tmp,data_out)
      ELSE
        data_out(:)=tmp(:)
      ENDIF

    ENDSUBROUTINE transfer_1Dbase
!
!-------------------------------------------------------------------------------
!> @brief
!> @param this  The mesh transfer object
!>
!> This routine
!>
    SUBROUTINE init_1DCart(this,pList)
      CLASS(MeshTransfer_1DCart),INTENT(INOUT) :: this
      CLASS(ParamType),INTENT(IN) :: pList

      REAL(SRK) :: minX, maxX
      CALL this%init_base(pList)
      minX=MINVAL(this%mesh_in)
      maxX=MAXVAL(this%mesh_in)
      IF(pList%has('minRange')) CALL pList%get('minRange',minX)
      IF(pList%has('maxRange')) CALL pList%get('maxRange',maxX)

      IF(this%MapType_in == MapType_POINT .AND. this%MapType_out == MapType_POINT) THEN
        IF(this%poly_transfer>=0) THEN
          CALL setupP2C_cart(this%transferLS,this%mesh_in,minX,maxX)
          CALL setupC2P_cart(this%transferMatrix,this%poly_transfer,this%mesh_out,minX,maxX)
        ELSE
          CALL setupP2P(this%transferMatrix,this%mesh_in,this%mesh_out)
        ENDIF
      ELSEIF(this%MapType_in == MapType_POINT .AND. this%MapType_out == MapType_VOLUME) THEN
        IF(this%poly_transfer>=0) THEN
          CALL setupP2C_cart(this%transferLS,this%mesh_in,minX,maxX)
          CALL setupC2V_cart(this%transferMatrix,this%poly_transfer,this%mesh_out,minX,maxX)
        ELSE
          CALL setupP2V_cart(this%transferMatrix,this%mesh_in,this%mesh_out)
        ENDIF
      ELSEIF(this%MapType_in == MapType_POINT .AND. this%MapType_out == MapType_CONTINUOUS) THEN
          CALL setupP2C_cart(this%transferLS,this%mesh_in,minX,maxX)
      ELSEIF(this%MapType_in == MapType_VOLUME .AND. this%MapType_out == MapType_POINT) THEN
        IF(this%poly_transfer>=0) THEN
          CALL setupV2C_cart(this%transferLS,this%mesh_in,minX,maxX)
          CALL setupC2P_cart(this%transferMatrix,this%poly_transfer,this%mesh_out,minX,maxX)
        ELSE
          CALL setupV2P(this%transferMatrix,this%mesh_in,this%mesh_out)
        ENDIF
      ELSEIF(this%MapType_in == MapType_VOLUME .AND. this%MapType_out == MapType_VOLUME) THEN
        IF(this%poly_transfer>=0) THEN
          CALL setupV2C_cart(this%transferLS,this%mesh_in,minX,maxX)
          CALL setupC2V_cart(this%transferMatrix,this%poly_transfer,this%mesh_out,minX,maxX)
        ELSE
          CALL setupV2V_cart(this%transferMatrix,this%mesh_in,this%mesh_out)
        ENDIF
      ELSEIF(this%MapType_in == MapType_VOLUME .AND. this%MapType_out == MapType_CONTINUOUS) THEN
        CALL setupV2C_cart(this%transferLS,this%mesh_in,minX,maxX)
      ELSEIF(this%MapType_in == MapType_CONTINUOUS .AND. this%MapType_out == MapType_POINT) THEN
        CALL setupC2P_cart(this%transferMatrix,this%nmesh_in,this%mesh_out,minX,maxX)
      ELSEIF(this%MapType_in == MapType_CONTINUOUS .AND. this%MapType_out == MapType_VOLUME) THEN
        CALL setupC2V_cart(this%transferMatrix,this%nmesh_in,this%mesh_out,minX,maxX)
      ELSEIF(this%MapType_in == MapType_CONTINUOUS .AND. this%MapType_out == MapType_CONTINUOUS) THEN
        CALL setupC2C(this%transferMatrix,this%nmesh_in,this%nmesh_out)
      ENDIF

      this%isInit=.TRUE.
    ENDSUBROUTINE init_1DCart
!
!-------------------------------------------------------------------------------
!> @brief
!> @param this  The mesh transfer object
!>
!> This routine
!>
    SUBROUTINE init_1DCyl(this,pList)
      CLASS(MeshTransfer_1DCyl),INTENT(INOUT) :: this
      CLASS(ParamType),INTENT(IN) :: pList

      REAL(SRK) :: maxR

      CALL this%init_base(pList)

      maxR=MAXVAL(this%mesh_in)
      IF(pList%has('maxRange')) CALL pList%get('maxRange',maxR)

      IF(this%MapType_in == MapType_POINT .AND. this%MapType_out == MapType_POINT) THEN
        IF(this%poly_transfer>=0) THEN
          CALL setupP2C_cyl(this%transferLS,this%mesh_in,maxR)
          CALL setupC2P_cyl(this%transferMatrix,this%poly_transfer,this%mesh_out,maxR)
        ELSE
          CALL setupP2P(this%transferMatrix,this%mesh_in,this%mesh_out)
        ENDIF
      ELSEIF(this%MapType_in == MapType_POINT .AND. this%MapType_out == MapType_VOLUME) THEN
        IF(this%poly_transfer>=0) THEN
          CALL setupP2C_cyl(this%transferLS,this%mesh_in,maxR)
          CALL setupC2V_cyl(this%transferMatrix,this%poly_transfer,this%mesh_out,maxR)
        ELSE
          CALL setupP2V_cyl(this%transferMatrix,this%mesh_in,this%mesh_out)
        ENDIF
      ELSEIF(this%MapType_in == MapType_POINT .AND. this%MapType_out == MapType_CONTINUOUS) THEN
          CALL setupP2C_cyl(this%transferLS,this%mesh_in,maxR)
      ELSEIF(this%MapType_in == MapType_VOLUME .AND. this%MapType_out == MapType_POINT) THEN
        IF(this%poly_transfer>=0) THEN
          CALL setupV2C_cyl(this%transferLS,this%mesh_in,maxR)
          CALL setupC2P_cyl(this%transferMatrix,this%poly_transfer,this%mesh_out,maxR)
        ELSE
          CALL setupV2P(this%transferMatrix,this%mesh_in,this%mesh_out)
        ENDIF
      ELSEIF(this%MapType_in == MapType_VOLUME .AND. this%MapType_out == MapType_VOLUME) THEN
        IF(this%poly_transfer>=0) THEN
          CALL setupV2C_cyl(this%transferLS,this%mesh_in,maxR)
          CALL setupC2V_cyl(this%transferMatrix,this%poly_transfer,this%mesh_out,maxR)
        ELSE
          CALL setupV2V_cyl(this%transferMatrix,this%mesh_in,this%mesh_out)
        ENDIF
      ELSEIF(this%MapType_in == MapType_VOLUME .AND. this%MapType_out == MapType_CONTINUOUS) THEN
        CALL setupV2C_cyl(this%transferLS,this%mesh_in,maxR)
      ELSEIF(this%MapType_in == MapType_CONTINUOUS .AND. this%MapType_out == MapType_POINT) THEN
        CALL setupC2P_cyl(this%transferMatrix,this%nmesh_in,this%mesh_out,maxR)
      ELSEIF(this%MapType_in == MapType_CONTINUOUS .AND. this%MapType_out == MapType_VOLUME) THEN
        CALL setupC2V_cyl(this%transferMatrix,this%nmesh_in,this%mesh_out,maxR)
      ELSEIF(this%MapType_in == MapType_CONTINUOUS .AND. this%MapType_out == MapType_CONTINUOUS) THEN
        CALL setupC2C(this%transferMatrix,this%nmesh_in,this%nmesh_out)
      ENDIF

      this%isInit=.TRUE.
    ENDSUBROUTINE init_1DCyl
!
!-------------------------------------------------------------------------------
    SUBROUTINE setupP2P(TM,mesh_in,mesh_out)
      REAL(SRK),ALLOCATABLE,INTENT(INOUT) :: TM(:,:)
      REAL(SRK),INTENT(IN) :: mesh_in(:)
      REAL(SRK),INTENT(IN) :: mesh_out(:)

      INTEGER(SIK) :: i,j
      REAL(SRK) :: dx
      REQUIRE(SIZE(mesh_in)>0)
      REQUIRE(SIZE(mesh_out)>0)

      IF(ALLOCATED(TM)) DEALLOCATE(TM)
      ALLOCATE(TM(SIZE(mesh_out),SIZE(mesh_in)))
      TM(:,:)=ZERO

      DO i=1,SIZE(mesh_out)
        j=findIndex(mesh_in,mesh_out(i),.FALSE.,incl=1)
        IF(j>0) THEN
          dx=mesh_in(j+1)-mesh_in(j)
          TM(i,j)=(mesh_in(j+1)-mesh_out(i))/dx
          TM(i,j+1)=(mesh_out(i)-mesh_in(j))/dx
        ELSEIF(j==-1) THEN
          TM(i,1)=ONE
        ELSEIF(j==-2) THEN
          TM(i,SIZE(mesh_in))=ONE
        ENDIF
      ENDDO

      ENSURE(ALLOCATED(TM))
    ENDSUBROUTINE setupP2P
!
!-------------------------------------------------------------------------------
    SUBROUTINE setupV2P(TM,mesh_in,mesh_out)
      REAL(SRK),ALLOCATABLE,INTENT(INOUT) :: TM(:,:)
      REAL(SRK),INTENT(IN) :: mesh_in(:)
      REAL(SRK),INTENT(IN) :: mesh_out(:)

      INTEGER(SIK) :: i,j
      REQUIRE(SIZE(mesh_in)>0)
      REQUIRE(SIZE(mesh_out)>0)

      IF(ALLOCATED(TM)) DEALLOCATE(TM)
      ALLOCATE(TM(SIZE(mesh_out),SIZE(mesh_in)-1))
      TM(:,:)=ZERO

      DO i=1,SIZE(mesh_out)
        j=findIndex(mesh_in,mesh_out(i),.FALSE.)
        IF(j>0) THEN
          TM(i,j)=ONE
        ELSEIF(j==-1) THEN
          TM(i,1)=ONE
        ELSEIF(j==-2) THEN
          TM(i,SIZE(mesh_in)-1)=ONE
        ELSEIF(j==-3) THEN
          j=findIndex(mesh_in,mesh_out(i),.FALSE.,incl=1)
          IF(j==-1) THEN
            TM(i,1)=ONE
          ELSEIF(j==SIZE(mesh_in)-1) THEN
            TM(i,j)=ONE
          ELSE
            TM(i,j)=HALF
            TM(i,j+1)=HALF
          ENDIF
        ENDIF
      ENDDO
      ! TODO: search to find which V that P falls into
      !  look at ArrayUtils, findIndex
      ENSURE(ALLOCATED(TM))
    ENDSUBROUTINE setupV2P
!
!-------------------------------------------------------------------------------
    SUBROUTINE setupC2C(TM,nmesh_in,nmesh_out)
      REAL(SRK),ALLOCATABLE,INTENT(INOUT) :: TM(:,:)
      INTEGER(SIK),INTENT(IN) :: nmesh_in
      INTEGER(SIK),INTENT(IN) :: nmesh_out

      INTEGER(SIK) :: i

      REQUIRE(nmesh_in>0)
      REQUIRE(nmesh_out>0)

      IF(ALLOCATED(TM)) DEALLOCATE(TM)
      ALLOCATE(TM(nmesh_out,nmesh_in))
      TM(:,:)=ZERO

      DO i=1,MIN(nmesh_in,nmesh_out)
        TM(i,i)=ONE
      ENDDO

      ENSURE(ALLOCATED(TM))
    ENDSUBROUTINE setupC2C
!
!-------------------------------------------------------------------------------
!  Cartesian interfaces
!-------------------------------------------------------------------------------
    SUBROUTINE setupP2V_cart(TM,mesh_in,mesh_out)
      REAL(SRK),ALLOCATABLE,INTENT(INOUT) :: TM(:,:)
      REAL(SRK),INTENT(IN) :: mesh_in(:)
      REAL(SRK),INTENT(IN) :: mesh_out(:)

      REQUIRE(SIZE(mesh_in)>0)
      ! Volume requires at least 2 mesh points
      REQUIRE(SIZE(mesh_out)>1)

      IF(ALLOCATED(TM)) DEALLOCATE(TM)
      ALLOCATE(TM(SIZE(mesh_out)-1,SIZE(mesh_in)))
      TM(:,:)=ZERO

      ! TODO: Linear interpolate between P then integrate for V
      !  look at ArrayUtils, findIndex
      ENSURE(ALLOCATED(TM))
    ENDSUBROUTINE setupP2V_cart
!
!-------------------------------------------------------------------------------
    SUBROUTINE setupP2C_cart(TLS,mesh_in,minX,maxX)
      TYPE(LinearSolverType_Direct),INTENT(INOUT) :: TLS
      REAL(SRK),INTENT(IN) :: mesh_in(:)
      REAL(SRK),INTENT(IN) :: minX
      REAL(SRK),INTENT(IN) :: maxX

      INTEGER(SIK) :: i,j,nin
      REAL(SRK) :: h,cent
      REAL(SRK),ALLOCATABLE :: x(:)
      TYPE(ParamType) :: myPL

      REQUIRE(.NOT. TLS%isInit)
      REQUIRE(SIZE(mesh_in)>0)

      nin=SIZE(mesh_in)
      h=maxX-minX
      cent=0.5_SRK*(maxX+minX)

      CALL myPL%add('LinearSolverType->TPLType',NATIVE)
      CALL myPL%add('LinearSolverType->solverMethod',LU)
      CALL myPL%add('LinearSolverType->MPI_Comm_ID',PE_COMM_SELF)
      CALL myPL%add('LinearSolverType->numberOMP',1_SNK)
      CALL myPL%add('LinearSolverType->timerName','testTimer')
      CALL myPL%add('LinearSolverType->matType',DENSESQUARE)
      CALL myPL%add('LinearSolverType->A->MatrixType->isSym',.FALSE.)
      CALL myPL%add('LinearSolverType->A->MatrixType->n',nin)
      CALL myPL%add('LinearSolverType->x->VectorType->n',nin)
      CALL myPL%add('LinearSolverType->b->VectorType->n',nin)
      CALL TLS%init(myPL)
      CALL myPL%clear()

      ALLOCATE(x(nin))

      SELECTTYPE(A => TLS%A); TYPE IS(DenseSquareMatrixType)
        DO i=1,nin
          DO j=1,nin
            x(:)=0.0_SRK
            x(j)=ONE
            CALL A%set(i,j,LPPointVal(x,mesh_in(i),h,cent))
          ENDDO
        ENDDO
      ENDSELECT

      ENSURE(TLS%isInit)
    ENDSUBROUTINE setupP2C_cart
!
!-------------------------------------------------------------------------------
    SUBROUTINE setupV2V_cart(TM,mesh_in,mesh_out)
      REAL(SRK),ALLOCATABLE,INTENT(INOUT) :: TM(:,:)
      REAL(SRK),INTENT(IN) :: mesh_in(:)
      REAL(SRK),INTENT(IN) :: mesh_out(:)

      ! Volume requires at least 2 mesh points
      REQUIRE(SIZE(mesh_in)>1)
      REQUIRE(SIZE(mesh_out)>1)

      IF(ALLOCATED(TM)) DEALLOCATE(TM)
      ALLOCATE(TM(SIZE(mesh_out)-1,SIZE(mesh_in)-1))
      TM(:,:)=ZERO

      ! TODO: Find union mesh, then construct the fractions (sum of each row should be 1)
      !  look at ArrayUtils, getUnion
      ENSURE(ALLOCATED(TM))
    ENDSUBROUTINE setupV2V_cart
!
!-------------------------------------------------------------------------------
    SUBROUTINE setupV2C_cart(TLS,mesh_in,minX,maxX)
      TYPE(LinearSolverType_Direct),INTENT(INOUT) :: TLS
      REAL(SRK),INTENT(IN) :: mesh_in(:)
      REAL(SRK),INTENT(IN) :: minX
      REAL(SRK),INTENT(IN) :: maxX

      INTEGER(SIK) :: i,j,nin
      REAL(SRK) :: h,cent
      REAL(SRK),ALLOCATABLE :: x(:)
      TYPE(ParamType) :: myPL

      REQUIRE(.NOT. TLS%isInit)
      ! Volume mesh requires at least 2 mesh points
      REQUIRE(SIZE(mesh_in)>1)

      nin=SIZE(mesh_in)-1
      h=(maxX-minX)
      cent=0.5_SRK*(maxX+minX)

      CALL myPL%add('LinearSolverType->TPLType',NATIVE)
      CALL myPL%add('LinearSolverType->solverMethod',LU)
      CALL myPL%add('LinearSolverType->MPI_Comm_ID',PE_COMM_SELF)
      CALL myPL%add('LinearSolverType->numberOMP',1_SNK)
      CALL myPL%add('LinearSolverType->timerName','testTimer')
      CALL myPL%add('LinearSolverType->matType',DENSESQUARE)
      CALL myPL%add('LinearSolverType->A->MatrixType->isSym',.FALSE.)
      CALL myPL%add('LinearSolverType->A->MatrixType->n',nin)
      CALL myPL%add('LinearSolverType->x->VectorType->n',nin)
      CALL myPL%add('LinearSolverType->b->VectorType->n',nin)
      CALL TLS%init(myPL)
      CALL myPL%clear()

      ALLOCATE(x(nin))

      SELECTTYPE(A => TLS%A); TYPE IS(DenseSquareMatrixType)
        DO i=1,nin
          DO j=1,nin
            x(:)=0.0_SRK
            x(j)=ONE
            CALL A%set(i,j,LPIntegral(x,mesh_in(i),mesh_in(i+1),h,cent))
          ENDDO
        ENDDO
      ENDSELECT

      ENSURE(TLS%isInit)
    ENDSUBROUTINE setupV2C_cart
!
!-------------------------------------------------------------------------------
    SUBROUTINE setupC2P_cart(TM,nmesh_in,mesh_out,minX,maxX)
      REAL(SRK),ALLOCATABLE,INTENT(INOUT) :: TM(:,:)
      INTEGER(SIK),INTENT(IN) :: nmesh_in
      REAL(SRK),INTENT(IN) :: mesh_out(:)
      REAL(SRK),INTENT(IN) :: minX
      REAL(SRK),INTENT(IN) :: maxX

      INTEGER(SIK) :: i,j,nin,nout
      REAL(SRK) :: h,cent
      REAL(SRK),ALLOCATABLE :: x(:)

      REQUIRE(nmesh_in>0)
      REQUIRE(SIZE(mesh_out)>0)

      nin=nmesh_in
      nout=SIZE(mesh_out)
      h=(maxX-minX)
      cent=0.5_SRK*(maxX+minX)
      ALLOCATE(x(nin))
      IF(ALLOCATED(TM)) DEALLOCATE(TM)
      ALLOCATE(TM(nout,nin))
      TM(:,:)=ZERO

      DO i=1,nout
        DO j=1,nin
          x(:)=ZERO
          x(j)=ONE
          TM(i,j)=LPPointVal(x,mesh_out(i),h,cent)
        ENDDO
      ENDDO

      ENSURE(ALLOCATED(TM))
    ENDSUBROUTINE setupC2P_cart
!
!-------------------------------------------------------------------------------
    SUBROUTINE setupC2V_cart(TM,nmesh_in,mesh_out,minX,maxX)
      REAL(SRK),ALLOCATABLE,INTENT(INOUT) :: TM(:,:)
      INTEGER(SIK),INTENT(IN) :: nmesh_in
      REAL(SRK),INTENT(IN) :: mesh_out(:)
      REAL(SRK),INTENT(IN) :: minX
      REAL(SRK),INTENT(IN) :: maxX

      INTEGER(SIK) :: i,j,nin,nout
      REAL(SRK) :: h,cent
      REAL(SRK),ALLOCATABLE :: x(:)

      REQUIRE(nmesh_in>0)
      ! Volume mesh requires at least 2 points
      REQUIRE(SIZE(mesh_out)>1)

      nin=nmesh_in
      nout=SIZE(mesh_out)-1
      h=(maxX-minX)
      cent=0.5_SRK*(maxX+minX)
      ALLOCATE(x(nin))
      IF(ALLOCATED(TM)) DEALLOCATE(TM)
      ALLOCATE(TM(nout,nin))
      TM(:,:)=ZERO

      DO i=1,nout
        DO j=1,nin
          x(:)=ZERO
          x(j)=ONE
          TM(i,j)=LPIntegral(x,mesh_out(i),mesh_out(i+1),h,cent)
        ENDDO
      ENDDO

      ENSURE(ALLOCATED(TM))
    ENDSUBROUTINE setupC2V_cart
!
!-------------------------------------------------------------------------------
! Cylindrical Interfaces
!-------------------------------------------------------------------------------
    SUBROUTINE setupP2V_cyl(TM,mesh_in,mesh_out)
      REAL(SRK),ALLOCATABLE,INTENT(INOUT) :: TM(:,:)
      REAL(SRK),INTENT(IN) :: mesh_in(:)
      REAL(SRK),INTENT(IN) :: mesh_out(:)

      REQUIRE(SIZE(mesh_in)>0)
      ! Volume requires at least 2 mesh points
      REQUIRE(SIZE(mesh_out)>1)

      IF(ALLOCATED(TM)) DEALLOCATE(TM)
      ALLOCATE(TM(SIZE(mesh_out)-1,SIZE(mesh_in)))
      TM(:,:)=ZERO

      ! TODO: Linear interpolate between P then integrate for
      !  look at ArrayUtils, findIndex
      ENSURE(ALLOCATED(TM))
    ENDSUBROUTINE setupP2V_cyl
!
!-------------------------------------------------------------------------------
    SUBROUTINE setupP2C_cyl(TLS,mesh_in,maxR)
      TYPE(LinearSolverType_Direct),INTENT(INOUT) :: TLS
      REAL(SRK),INTENT(IN) :: mesh_in(:)
      REAL(SRK),INTENT(IN) :: maxR

      INTEGER(SIK) :: i,j,nin
      REAL(SRK),ALLOCATABLE :: x(:)
      TYPE(ParamType) :: myPL

      REQUIRE(.NOT. TLS%isInit)
      REQUIRE(SIZE(mesh_in)>0)

      nin=SIZE(mesh_in)

      CALL myPL%add('LinearSolverType->TPLType',NATIVE)
      CALL myPL%add('LinearSolverType->solverMethod',LU)
      CALL myPL%add('LinearSolverType->MPI_Comm_ID',PE_COMM_SELF)
      CALL myPL%add('LinearSolverType->numberOMP',1_SNK)
      CALL myPL%add('LinearSolverType->timerName','testTimer')
      CALL myPL%add('LinearSolverType->matType',DENSESQUARE)
      CALL myPL%add('LinearSolverType->A->MatrixType->isSym',.FALSE.)
      CALL myPL%add('LinearSolverType->A->MatrixType->n',nin)
      CALL myPL%add('LinearSolverType->x->VectorType->n',nin)
      CALL myPL%add('LinearSolverType->b->VectorType->n',nin)
      CALL TLS%init(myPL)
      CALL myPL%clear()

      ALLOCATE(x(nin))

      SELECTTYPE(A => TLS%A); TYPE IS(DenseSquareMatrixType)
        DO i=1,nin
          DO j=1,nin
            x(:)=0.0_SRK
            x(j)=ONE
            CALL A%set(i,j,ZPPointVal(x,mesh_in(i),maxR))
          ENDDO
        ENDDO
      ENDSELECT

      ENSURE(TLS%isInit)
    ENDSUBROUTINE setupP2C_cyl
!
!-------------------------------------------------------------------------------
    SUBROUTINE setupV2V_cyl(TM,mesh_in,mesh_out)
      REAL(SRK),ALLOCATABLE,INTENT(INOUT) :: TM(:,:)
      REAL(SRK),INTENT(IN) :: mesh_in(:)
      REAL(SRK),INTENT(IN) :: mesh_out(:)

      ! Volume requires at least 2 mesh points
      REQUIRE(SIZE(mesh_in)>1)
      REQUIRE(SIZE(mesh_out)>1)

      IF(ALLOCATED(TM)) DEALLOCATE(TM)
      ALLOCATE(TM(SIZE(mesh_out)-1,SIZE(mesh_in)-1))
      TM(:,:)=ZERO

      ! TODO: Find union mesh, then construct the fractions (sum of each row should be 1)
      !  look at ArrayUtils, getUnion
      ENSURE(ALLOCATED(TM))
    ENDSUBROUTINE setupV2V_cyl
!
!-------------------------------------------------------------------------------
    SUBROUTINE setupV2C_cyl(TLS,mesh_in,maxR)
      TYPE(LinearSolverType_Direct),INTENT(INOUT) :: TLS
      REAL(SRK),INTENT(IN) :: mesh_in(:)
      REAL(SRK),INTENT(IN) :: maxR

      INTEGER(SIK) :: i,j,nin
      REAL(SRK),ALLOCATABLE :: x(:)
      TYPE(ParamType) :: myPL

      REQUIRE(.NOT. TLS%isInit)
      ! Volume mesh requires at least 2 mesh points
      REQUIRE(SIZE(mesh_in)>1)

      nin=SIZE(mesh_in)-1

      CALL myPL%add('LinearSolverType->TPLType',NATIVE)
      CALL myPL%add('LinearSolverType->solverMethod',LU)
      CALL myPL%add('LinearSolverType->MPI_Comm_ID',PE_COMM_SELF)
      CALL myPL%add('LinearSolverType->numberOMP',1_SNK)
      CALL myPL%add('LinearSolverType->timerName','testTimer')
      CALL myPL%add('LinearSolverType->matType',DENSESQUARE)
      CALL myPL%add('LinearSolverType->A->MatrixType->isSym',.FALSE.)
      CALL myPL%add('LinearSolverType->A->MatrixType->n',nin)
      CALL myPL%add('LinearSolverType->x->VectorType->n',nin)
      CALL myPL%add('LinearSolverType->b->VectorType->n',nin)
      CALL TLS%init(myPL)
      CALL myPL%clear()

      ALLOCATE(x(nin))

      SELECTTYPE(A => TLS%A); TYPE IS(DenseSquareMatrixType)
        DO i=1,nin
          DO j=1,nin
            x(:)=ZERO
            x(j)=ONE
            CALL A%set(i,j,ZPIntegral(x,mesh_in(i),mesh_in(i+1),maxR))
          ENDDO
        ENDDO
      ENDSELECT

      ENSURE(TLS%isInit)
    ENDSUBROUTINE setupV2C_cyl
!
!-------------------------------------------------------------------------------
    SUBROUTINE setupC2P_cyl(TM,nmesh_in,mesh_out,maxR)
      REAL(SRK),ALLOCATABLE,INTENT(INOUT) :: TM(:,:)
      INTEGER(SIK),INTENT(IN) :: nmesh_in
      REAL(SRK),INTENT(IN) :: mesh_out(:)
      REAL(SRK),INTENT(IN) :: maxR

      INTEGER(SIK) :: i,j,nin,nout
      REAL(SRK),ALLOCATABLE :: x(:)

      REQUIRE(nmesh_in>0)
      REQUIRE(SIZE(mesh_out)>0)

      nin=nmesh_in
      nout=SIZE(mesh_out)
      ALLOCATE(x(nin))
      IF(ALLOCATED(TM)) DEALLOCATE(TM)
      ALLOCATE(TM(nout,nin))
      TM(:,:)=ZERO

      DO i=1,nout
        DO j=1,nin
          x(:)=ZERO
          x(j)=ONE
          TM(i,j)=ZPPointVal(x,mesh_out(i),maxR)
        ENDDO
      ENDDO

      ENSURE(ALLOCATED(TM))
    ENDSUBROUTINE setupC2P_cyl
!
!-------------------------------------------------------------------------------
    SUBROUTINE setupC2V_cyl(TM,nmesh_in,mesh_out,maxR)
      REAL(SRK),ALLOCATABLE,INTENT(INOUT) :: TM(:,:)
      INTEGER(SIK),INTENT(IN) :: nmesh_in
      REAL(SRK),INTENT(IN) :: mesh_out(:)
      REAL(SRK),INTENT(IN) :: maxR

      INTEGER(SIK) :: i,j,nin,nout
      REAL(SRK),ALLOCATABLE :: x(:)

      REQUIRE(nmesh_in>0)
      ! Volume mesh requires at least 2 points
      REQUIRE(SIZE(mesh_out)>1)

      nin=nmesh_in
      nout=SIZE(mesh_out)-1
      ALLOCATE(x(nin))
      IF(ALLOCATED(TM)) DEALLOCATE(TM)
      ALLOCATE(TM(nout,nin))
      TM(:,:)=ZERO

      DO i=1,nout
        DO j=1,nin
          x(:)=ZERO
          x(j)=ONE
          TM(i,j)=ZPIntegral(x,mesh_out(i),mesh_out(i+1),maxR)
        ENDDO
      ENDDO

      ENSURE(ALLOCATED(TM))
    ENDSUBROUTINE setupC2V_cyl
!
!-------------------------------------------------------------------------------
!> @brief Returns value of an arbitrary order Legendre Polynomial with coefficients coef
!> @param coef  The coefficients of the Legendre expansion
!> @param globX The value of x in the global coordinate
!> @param h     The height of the node, OPTIONAL (if not provided a height of 2 is assumed)
!> @param nodeX The center of the node, OPTIONAL (if not provided a position of 0 is assumed)
!>
!> This routine returns the value of an arbitrary order Legendre expansion at a
!> specific point for NEM and SANM nodal methods.
!>
    FUNCTION LPPointVal(coef,globX,h,nodeX) RESULT(y)
      REAL(SRK),DIMENSION(:),INTENT(IN) :: coef
      REAL(SRK),INTENT(IN) :: globX
      REAL(SRK),INTENT(IN),OPTIONAL :: h
      REAL(SRK),INTENT(IN),OPTIONAL :: nodeX
      REAL(SRK) :: y
      ! Local variables
      INTEGER(SIK) :: N,i
      REAL(SRK) :: w, x, Pn, Pnm1, Pnm2

      w=TWO
      x=globX

      IF(PRESENT(h)) w=h
      IF(PRESENT(nodeX)) x=globX-nodeX

      N=SIZE(coef)

      !project into local coordinate system
      x=TWO*x/w
      y=coef(1)
      Pnm2=ONE
      IF(N > 1) THEN
        y=y+coef(2)*x
        Pnm1=x
        DO i=2,N-1
          Pn=(TWO*REAL(i,SRK)-ONE)/REAL(i,SRK)*x*Pnm1-(REAL(i,SRK)-ONE)/REAL(i,SRK)*Pnm2
          y=y+coef(i+1)*Pn
          Pnm2=Pnm1
          Pnm1=Pn
        ENDDO
      ENDIF
    ENDFUNCTION LPPointVal
!
!-------------------------------------------------------------------------------
!> @brief Returns value of the integral of an arbitrary order Legendre Polynomial with coefficients coef
!> @param coef  The coefficients of the Legendre expansion
!> @param a     The beginning of the integral range
!> @param b     The end of the integral range
!> @param h     The height of the node, OPTIONAL (if not provided a height of 2 is assumed)
!> @param nodeX The center of the node, OPTIONAL (if not provided a position of 0 is assumed)
!>
!> This routine returns the value of the integral of an arbitrary order Legendre expansion on a
!> specific range a to b for NEM and SANM nodal methods.
!>
    FUNCTION LPIntegral(coef,a,b,h,nodeX) RESULT(y)
      REAL(SRK),DIMENSION(:),INTENT(IN) :: coef
      REAL(SRK),INTENT(IN) :: a
      REAL(SRK),INTENT(IN) :: b
      REAL(SRK),INTENT(IN),OPTIONAL :: h
      REAL(SRK),INTENT(IN),OPTIONAL :: nodeX
      REAL(SRK) :: y
      ! Local variables
      INTEGER(SIK) :: N,m
      REAL(SRK) :: w, xa, xb, Pna, Pnm1a, Pnm2a, Pnb, Pnm1b, Pnm2b
      REAL(SRK),ALLOCATABLE :: c(:)

      w=TWO
      xa=a
      xb=b

      IF(PRESENT(h)) w=h
      IF(PRESENT(nodeX)) xa=a-nodeX
      IF(PRESENT(nodeX)) xb=b-nodeX

      REQUIRE(ABS(xb-xa) <= w)

      N=SIZE(coef)

      ALLOCATE(c(0:N-1))
      c(:)=0.0_SRK
      c(0:N-1)=coef(1:N)

      !project into local coordinate system
      xa=TWO*xa/w
      xb=TWO*xb/w
      IF(N > 1) THEN
        Pnm1a=xa
        Pnm1b=xb
        Pnm2a=ONE
        Pnm2b=ONE
        !this is m=1
        y=c(0)*(xb-xa)
        DO m=2,N
          Pna=(TWO*REAL(m,SRK)-ONE)/REAL(m,SRK)*xa*Pnm1a-(REAL(m,SRK)-ONE)/REAL(m,SRK)*Pnm2a
          Pnb=(TWO*REAL(m,SRK)-ONE)/REAL(m,SRK)*xb*Pnm1b-(REAL(m,SRK)-ONE)/REAL(m,SRK)*Pnm2b
          y=y+c(m-1)/(TWO*REAL(m,SRK)-ONE)*(Pnb-Pna-Pnm2b+Pnm2a)
          Pnm2a=Pnm1a
          Pnm2b=Pnm1b
          Pnm1a=Pna
          Pnm1b=Pnb
        ENDDO
      ENDIF
      y=y/(xb-xa)
      DEALLOCATE(c)
    ENDFUNCTION LPIntegral
!
!-------------------------------------------------------------------------------
!> @brief Returns value of an arbitrary order Zernike Polynomial with coefficients coef
!> @param coef  The coefficients of the Zernike expansion
!> @param r     The value of r in the global coordinate
!> @param h     The height of the node, OPTIONAL (if not provided a height of 1 is assumed)
!>
!> This routine returns the value of an arbitrary order Zernike expansion at a
!> specific point.
!>
    FUNCTION ZPPointVal(coef,r,h) RESULT(y)
      REAL(SRK),DIMENSION(:),INTENT(IN) :: coef
      REAL(SRK),INTENT(IN) :: r
      REAL(SRK),INTENT(IN),OPTIONAL :: h
      REAL(SRK) :: y
      ! Local variables
      REAL(SRK) :: w

      w=ONE

      IF(PRESENT(h)) w=h

      y=LPPointVal(coef,TWO*(r/w)*(r/w)-ONE)
    ENDFUNCTION ZPPointVal
!
!-------------------------------------------------------------------------------
!> @brief Returns value of an arbitrary order Zernike Polynomial with coefficients coef
!> @param coef  The coefficients of the Zernike expansion
!> @param a     The beginning of the integral range
!> @param b     The end of the integral range
!> @param h     The height of the node, OPTIONAL (if not provided a height of 1 is assumed)
!>
!> This routine returns the value of an arbitrary order Zernike expansion at a
!> specific point.
!>
    FUNCTION ZPIntegral(coef,a,b,h) RESULT(y)
      REAL(SRK),DIMENSION(:),INTENT(IN) :: coef
      REAL(SRK),INTENT(IN) :: a
      REAL(SRK),INTENT(IN) :: b
      REAL(SRK),INTENT(IN),OPTIONAL :: h
      REAL(SRK) :: y
      ! Local variables
      REAL(SRK) :: w

      w=ONE

      IF(PRESENT(h)) w=h
      REQUIRE(ABS(b-a) <= w)

      y=LPIntegral(coef,TWO*(a/w)*(a/w)-ONE,TWO*(b/w)*(b/w)-ONE)
    ENDFUNCTION ZPIntegral
!
!-------------------------------------------------------------------------------
!  Test Interfaces
!-------------------------------------------------------------------------------
    SUBROUTINE testsetupP2P(TM,mesh_in,mesh_out)
      REAL(SRK),ALLOCATABLE,INTENT(INOUT) :: TM(:,:)
      REAL(SRK),INTENT(IN) :: mesh_in(:)
      REAL(SRK),INTENT(IN) :: mesh_out(:)

      CALL setupP2P(TM,mesh_in,mesh_out)
    ENDSUBROUTINE testsetupP2P
!
    SUBROUTINE testsetupV2P(TM,mesh_in,mesh_out)
      REAL(SRK),ALLOCATABLE,INTENT(INOUT) :: TM(:,:)
      REAL(SRK),INTENT(IN) :: mesh_in(:)
      REAL(SRK),INTENT(IN) :: mesh_out(:)

      CALL setupV2P(TM,mesh_in,mesh_out)
    ENDSUBROUTINE testsetupV2P
!
    SUBROUTINE testsetupC2C(TM,nmesh_in,nmesh_out)
      REAL(SRK),ALLOCATABLE,INTENT(INOUT) :: TM(:,:)
      INTEGER(SIK),INTENT(IN) :: nmesh_in
      INTEGER(SIK),INTENT(IN) :: nmesh_out

      CALL setupC2C(TM,nmesh_in,nmesh_out)
    ENDSUBROUTINE testsetupC2C
!
    SUBROUTINE testsetupP2V_cart(TM,mesh_in,mesh_out)
      REAL(SRK),ALLOCATABLE,INTENT(INOUT) :: TM(:,:)
      REAL(SRK),INTENT(IN) :: mesh_in(:)
      REAL(SRK),INTENT(IN) :: mesh_out(:)

      CALL setupP2V_cart(TM,mesh_in,mesh_out)
    ENDSUBROUTINE testsetupP2V_cart
!
    SUBROUTINE testsetupP2C_cart(TLS,mesh_in,minX,maxX)
      TYPE(LinearSolverType_Direct),INTENT(INOUT) :: TLS
      REAL(SRK),INTENT(IN) :: mesh_in(:)
      REAL(SRK),INTENT(IN) :: minX
      REAL(SRK),INTENT(IN) :: maxX

      CALL setupP2C_cart(TLS,mesh_in,minX,maxX)
    ENDSUBROUTINE testsetupP2C_cart
!
    SUBROUTINE testsetupV2V_cart(TM,mesh_in,mesh_out)
      REAL(SRK),ALLOCATABLE,INTENT(INOUT) :: TM(:,:)
      REAL(SRK),INTENT(IN) :: mesh_in(:)
      REAL(SRK),INTENT(IN) :: mesh_out(:)

      CALL setupV2V_cart(TM,mesh_in,mesh_out)
    ENDSUBROUTINE testsetupV2V_cart
!
    SUBROUTINE testsetupV2C_cart(TLS,mesh_in,minX,maxX)
      TYPE(LinearSolverType_Direct),INTENT(INOUT) :: TLS
      REAL(SRK),INTENT(IN) :: mesh_in(:)
      REAL(SRK),INTENT(IN) :: minX
      REAL(SRK),INTENT(IN) :: maxX

      CALL setupV2C_cart(TLS,mesh_in,minX,maxX)
    ENDSUBROUTINE testsetupV2C_cart
!
    SUBROUTINE testsetupC2P_cart(TM,nmesh_in,mesh_out,minX,maxX)
      REAL(SRK),ALLOCATABLE,INTENT(INOUT) :: TM(:,:)
      INTEGER(SIK),INTENT(IN) :: nmesh_in
      REAL(SRK),INTENT(IN) :: mesh_out(:)
      REAL(SRK),INTENT(IN) :: minX
      REAL(SRK),INTENT(IN) :: maxX

      CALL setupC2P_cart(TM,nmesh_in,mesh_out,minX,maxX)
    ENDSUBROUTINE testsetupC2P_cart
!
    SUBROUTINE testsetupC2V_cart(TM,nmesh_in,mesh_out,minX,maxX)
      REAL(SRK),ALLOCATABLE,INTENT(INOUT) :: TM(:,:)
      INTEGER(SIK),INTENT(IN) :: nmesh_in
      REAL(SRK),INTENT(IN) :: mesh_out(:)
      REAL(SRK),INTENT(IN) :: minX
      REAL(SRK),INTENT(IN) :: maxX

      CALL setupC2V_cart(TM,nmesh_in,mesh_out,minX,maxX)
    ENDSUBROUTINE testsetupC2V_cart
!
    SUBROUTINE testsetupP2V_cyl(TM,mesh_in,mesh_out)
      REAL(SRK),ALLOCATABLE,INTENT(INOUT) :: TM(:,:)
      REAL(SRK),INTENT(IN) :: mesh_in(:)
      REAL(SRK),INTENT(IN) :: mesh_out(:)

      CALL setupP2V_cyl(TM,mesh_in,mesh_out)
    ENDSUBROUTINE testsetupP2V_cyl
!
    SUBROUTINE testsetupP2C_cyl(TLS,mesh_in,maxR)
      TYPE(LinearSolverType_Direct),INTENT(INOUT) :: TLS
      REAL(SRK),INTENT(IN) :: mesh_in(:)
      REAL(SRK),INTENT(IN) :: maxR

      CALL setupP2C_cyl(TLS,mesh_in,maxR)
    ENDSUBROUTINE testsetupP2C_cyl
!
    SUBROUTINE testsetupV2V_cyl(TM,mesh_in,mesh_out)
      REAL(SRK),ALLOCATABLE,INTENT(INOUT) :: TM(:,:)
      REAL(SRK),INTENT(IN) :: mesh_in(:)
      REAL(SRK),INTENT(IN) :: mesh_out(:)

      CALL setupV2V_cyl(TM,mesh_in,mesh_out)
    ENDSUBROUTINE testsetupV2V_cyl
!
    SUBROUTINE testsetupV2C_cyl(TLS,mesh_in,maxR)
      TYPE(LinearSolverType_Direct),INTENT(INOUT) :: TLS
      REAL(SRK),INTENT(IN) :: mesh_in(:)
      REAL(SRK),INTENT(IN) :: maxR

      CALL setupV2C_cyl(TLS,mesh_in,maxR)
    ENDSUBROUTINE testsetupV2C_cyl
!
    SUBROUTINE testsetupC2P_cyl(TM,nmesh_in,mesh_out,maxR)
      REAL(SRK),ALLOCATABLE,INTENT(INOUT) :: TM(:,:)
      INTEGER(SIK),INTENT(IN) :: nmesh_in
      REAL(SRK),INTENT(IN) :: mesh_out(:)
      REAL(SRK),INTENT(IN) :: maxR

      CALL setupC2P_cyl(TM,nmesh_in,mesh_out,maxR)
    ENDSUBROUTINE testsetupC2P_cyl
!
    SUBROUTINE testsetupC2V_cyl(TM,nmesh_in,mesh_out,maxR)
      REAL(SRK),ALLOCATABLE,INTENT(INOUT) :: TM(:,:)
      INTEGER(SIK),INTENT(IN) :: nmesh_in
      REAL(SRK),INTENT(IN) :: mesh_out(:)
      REAL(SRK),INTENT(IN) :: maxR

      CALL setupC2V_cyl(TM,nmesh_in,mesh_out,maxR)
    ENDSUBROUTINE testsetupC2V_cyl
!
ENDMODULE MeshTransfer
