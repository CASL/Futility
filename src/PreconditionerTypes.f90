
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
!> @brief Module provides a preconditioner type and methods to set up different
!> preconditioners
!>
!> The precontioner type owns its own matrix. It is initialized with a matrix. 
!> In general any third party library (TPL) that may be interfaced with this 
!> module should be implemented such that it's optional.
!>
!> Currently supported TPLs include:
!>  - 
!>
!>
!> @par Module Dependencies
!>  - @ref IntrType "IntrType": @copybrief IntrType
!>  - @ref BLAS "BLAS": @copybrief BLAS
!>  - @ref Times "Times": @copybrief Times
!>  - @ref ExceptionHandler "ExceptionHandler": @copybrief ExceptionHandler
!>  - @ref Allocs "Allocs": @copybrief Allocs
!>  - @ref ParameterLists "ParameterLists": @copybrief ParameterLists
!>  - @ref ParallelEnv "ParallelEnv": @copybrief ParallelEnv
!>  - @ref VectorTypes "VectorTypes": @copybrief VectorTypes
!>  - @ref MatrixTypes "MatrixTypes": @copybrief MatrixTypes
!>
!> @par EXAMPLES
!> @code
!> 
!> @endcode
!>
!> @author Aaron Graham, Andrew Gerlach
!>   @date 11/01/2013
!>
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE PreconditionerTypes

  USE IntrType
  USE BLAS
  USE Times
  USE ExceptionHandler
  USE Allocs
  USE ParameterLists
  USE ParallelEnv
  USE VectorTypes
  USE MatrixTypes

  IMPLICIT NONE

  PRIVATE

  !
  ! List of Public members
  PUBLIC :: PreconditionerType
  PUBLIC :: LU_PreCondType
!  PUBLIC :: Minv_PreCondType


  TYPE,ABSTRACT :: PreConditionerType
    LOGICAL(SBK) :: isInit=.FALSE.
    CLASS(MatrixType),POINTER :: A=>NULL()

    CONTAINS
      PROCEDURE(precond_init_absintfc),DEFERRED,PASS :: init
      PROCEDURE(precond_absintfc),DEFERRED,PASS :: clear
      PROCEDURE(precond_absintfc),DEFERRED,PASS :: setup
      PROCEDURE(precond_apply_absintfc),DEFERRED,PASS :: apply
  ENDTYPE PreConditionerType

  TYPE,ABSTRACT,EXTENDS(PreConditionerType) :: LU_PreCondType
    CLASS(MatrixType),POINTER :: L
    CLASS(MatrixType),POINTER :: U

    CONTAINS
      PROCEDURE,PASS :: init => init_LU_PreCondType
      PROCEDURE,PASS :: clear => clear_LU_PreCondType
      PROCEDURE(precond_LU_absintfc),DEFERRED,PASS :: setup
      PROCEDURE,PASS :: apply => apply_LU_PreCondType
  ENDTYPE LU_PreCondType

!  TYPE,EXTENDS(PreConditionerType) :: Minv_PreCondType
!    TYPE(MatrixType) :: Minv
!
!    CONTAINS
!      PROCEDURE,PASS :: init => init_Minv_PreCondType
!      PROCEDURE,PASS :: clear => clear_Minv_PreCondType
!      PROCEDURE,PASS :: setup => setup_Minv_PreCondType
!      PROCEDURE,PASS :: apply => apply_Minv_PreCondType
!  ENDTYPE Minv_PreCondType

  ABSTRACT INTERFACE
    SUBROUTINE precond_init_absintfc(PC,A)
      IMPORT :: PreconditionerType,Matrixtype
      CLASS(PreconditionerType),INTENT(INOUT) :: PC
      CLASS(MatrixType),INTENT(IN) :: A
    ENDSUBROUTINE precond_init_absintfc
  ENDINTERFACE

  ABSTRACT INTERFACE
    SUBROUTINE precond_apply_absintfc(PC,v)
      IMPORT :: PreconditionerType,VectorType
      CLASS(PreconditionerType),INTENT(INOUT) :: PC
      CLASS(VectorType),INTENT(IN) :: v
    ENDSUBROUTINE precond_apply_absintfc
  ENDINTERFACE

  ABSTRACT INTERFACE
    SUBROUTINE precond_absintfc(PC)
      IMPORT :: PreconditionerType
      CLASS(PreconditionerType),INTENT(INOUT) :: PC
    ENDSUBROUTINE precond_absintfc
  ENDINTERFACE

  ABSTRACT INTERFACE
    SUBROUTINE precond_LU_absintfc(PC)
      IMPORT :: LU_PrecondType
      CLASS(LU_PrecondType),INTENT(INOUT) :: PC
    ENDSUBROUTINE precond_LU_absintfc
  ENDINTERFACE

  CHARACTER(LEN=*),PARAMETER :: modName='PreconditionerTypes'
!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief Initializes the Linear Solver Type with a parameter list
!> @param pList the parameter list
!>
!> @param solver The linear solver to act on
    SUBROUTINE init_LU_PreCondtype(PC,A)
      CLASS(LU_PrecondType),INTENT(INOUT) :: PC
      CLASS(MatrixType),TARGET,INTENT(IN) :: A

      CHARACTER(LEN=*),PARAMETER :: myName='init_LU_PreCondType'
      INTEGER(SIK) :: row,col,col2,i,j,k
      REAL(SRK) :: val1,val2,val3


      IF(PC%isinit) THEN
        ! Throw and error (need to set up error handling
      ELSE
        IF(.NOT.(A%isInit)) THEN
          ! Throw another error
        ELSE
          PC%A => A

          ! This might not be necessary here, but not sure
          SELECTTYPE(mat => PC%A)
            CLASS IS(SparseMatrixType)
              ALLOCATE(SparseMatrixType :: PC%L)
              ALLOCATE(SparseMatrixType :: PC%U)
              SELECTTYPE(U => PC%U); TYPE IS(SparseMatrixType)
                SELECTTYPE(L => PC%L); TYPE IS(SparseMatrixType)
                  j=0
                  val1=0.0_SRK
                  val2=0.0_SRK
                  DO row=2,SIZE(mat%ia)
                    DO i=1,mat%ia(row)
                      j=j+1
                      col=mat%ja(j)
                      IF(col > row-1) EXIT
                      CALL mat%get(row,col,val1)
                      CALL mat%get(col,col,val2)
                      val2=val1/val2
                      CALL L%setShape(row,col,val2)
                      DO k=i+1,mat%ia(row)
                        col2=mat%ja(j-i+k)
                        CALL mat%get(row,col2,val1)
                        IF(.NOT.(val1 .APPROXEQA. 0.0_SRK)) THEN
                          IF(col2 < row) THEN
                            CALL L%get(col,col2,val3)
                            CALL L%setShape(row,col2,val1-val2*val3)
                          ELSE
                            CALL U%get(col,col2,val3)
                            CALL U%setShape(row,col2,val1-val2*val3)
                          ENDIF
                        ENDIF
                      ENDDO
                    ENDDO
                    CALL L%setShape(row,row,1.0_SRK)
                  ENDDO
                ENDSELECT
              ENDSELECT
            CLASS DEFAULT
              ! Throw an error or warning or something
          ENDSELECT 

        ENDIF
      ENDIF
    ENDSUBROUTINE init_LU_PreCondtype
!
!-------------------------------------------------------------------------------
!> @brief Initializes the Linear Solver Type with a parameter list
!> @param pList the parameter list
!>
!> @param solver The linear solver to act on
    SUBROUTINE clear_LU_PreCondtype(PC)
      CLASS(LU_PrecondType),INTENT(INOUT) :: PC
    ENDSUBROUTINE clear_LU_PreCondtype
!
!-------------------------------------------------------------------------------
!> @brief Initializes the Linear Solver Type with a parameter list
!> @param pList the parameter list
!>
!> @param solver The linear solver to act on
    SUBROUTINE apply_LU_PreCondtype(PC,v)
      CLASS(LU_PrecondType),INTENT(INOUT) :: PC
      CLASS(Vectortype),INTENT(INOUT) :: v
    ENDSUBROUTINE apply_LU_PreCondtype
!
!-------------------------------------------------------------------------------
END MODULE
