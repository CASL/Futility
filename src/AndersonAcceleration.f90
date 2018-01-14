!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!> @brief Module provides an interface to Anderson Acceleration
!>
!> Currently supported TPLs include:
!>  - Trilinos
!>
!> @par Module Dependencies
!>  - @ref IntrType "IntrType": @copybrief IntrType
!>  - @ref BLAS "BLAS": @copybrief BLAS
!>  - @ref Times "Times": @copybrief Times
!>  - @ref ExceptionHandler "ExceptionHandler": @copybrief ExceptionHandler
!>  - @ref ParameterLists "ParameterLists": @copybrief ParameterLists
!>  - @ref ParallelEnv "ParallelEnv": @copybrief ParallelEnv
!>  - @ref VectorTypes "VectorTypes": @copybrief VectorTypes
!>
!> @par EXAMPLES
!> @code
!>
!> @endcode
!>
!> @author Ben Collins
!>   @date 07/30/2016
!>
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE AndersonAccelerationTypes
  USE IntrType
  USE BLAS
  USE Times
  USE ExceptionHandler
  USE ParameterLists
  USE ParallelEnv
  USE VectorTypes
  IMPLICIT NONE

  PRIVATE
!
! List of public members
  PUBLIC :: eAndersonAccelerationType
  PUBLIC :: AndersonAccelerationType

  !> @brief the base eigenvalue solver type
  TYPE :: AndersonAccelerationType
    !> Initialization status
    LOGICAL(SBK) :: isInit=.FALSE.
    !> NOX Solver type
    INTEGER(SIK) :: id=-1
    !> Pointer to the distributed memory parallel environment
    TYPE(MPI_EnvType),POINTER :: MPIparallelEnv => NULL()
    !> Pointer to the shared memory parallel environment TODO: eventually
    TYPE(OMP_EnvType) :: OMPparallelEnv
    !> iteration count
    INTEGER(SIK) :: iter=0
    !> size of nonlinear system
    INTEGER(SIK) :: n=-1
    !> depth of anderson solver
    INTEGER(SIK) :: depth=-1
    !> value of mixing parameter
    REAL(SRK) :: beta=0.0_SRK
    !> Pointer to solution vector, x
    CLASS(VectorType),POINTER :: X=>NULL()
    !> Timer to measure solution time
    TYPE(TimerType) :: SolveTime
  !
  !List of Type Bound Procedures
    CONTAINS
      !> @copybrief AndersonAccelerationType::init_AndersonAccelerationType
      !> @copydetails AndersonAccelerationType::init_AndersonAccelerationType
      PROCEDURE,PASS :: init => init_AndersonAccelerationType
      !> @copybrief AndersonAccelerationType::clear_AndersonAccelerationType
      !> @copydetails AndersonAccelerationType::clear_AndersonAccelerationType
      PROCEDURE,PASS :: clear => clear_AndersonAccelerationType
      !> @copybrief AndersonAccelerationType::step_AndersonAccelerationType
      !> @copydetails AndersonAccelerationType::step_AndersonAccelerationType
      PROCEDURE,PASS :: step => step_AndersonAccelerationType
      !> @copybrief AndersonAccelerationType::reset_AndersonAccelerationType
      !> @copydetails AndersonAccelerationType::reset_AndersonAccelerationType
      PROCEDURE,PASS :: reset => reset_AndersonAccelerationType
  ENDTYPE AndersonAccelerationType

  !> Exception Handler for use in Anderson Acceleration
  TYPE(ExceptionHandlerType),SAVE :: eAndersonAccelerationType

  !> Name of module
  CHARACTER(LEN=*),PARAMETER :: modName='ANDERSONACCELERATIONTYPE'

!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief Initializes the Anderson Acceleration Type with a parameter list
!> @param pList the parameter list
!>
!> @param solver The anderson acceleration solver to act on
!> @param MPIEnv The MPI environment description
!> @param Params The aprameterlist
!>
!> This routine initializes the data spaces for the NOX Anderson Acceleration
!>
    SUBROUTINE init_AndersonAccelerationType(solver,MPIEnv,Params)
      CHARACTER(LEN=*),PARAMETER :: myName='init_AndersonAccelerationType'
      CLASS(AndersonAccelerationType),INTENT(INOUT) :: solver
      TYPE(MPI_EnvType),INTENT(IN),TARGET :: MPIEnv
      TYPE(ParamType),INTENT(IN) :: Params

      TYPE(ParamType) :: validParams
      INTEGER(SIK) :: n,nlocal,depth,start
      REAL(SRK) :: beta
#ifdef FUTILITY_HAVE_Trilinos_NOX
      TYPE(ParamType) :: tmpPL
#endif

      !Check to set up required and optional param lists.
      !IF(.NOT.AndersonType_Paramsflag) CALL AndersionType_Declare_ValidParams()
      !Validate against the reqParams and OptParams
      validParams=Params
      !CALL validParams%validate(AndersonType_reqParams)

      IF(.NOT. MPIEnv%isInit()) THEN
        CALL eAndersonAccelerationType%raiseError('Incorrect input to '// &
          modName//'::'//myName//' - MPI Environment is not initialized!')
      ELSE
        solver%MPIparallelEnv => MPIEnv
      ENDIF

      n=0
      nlocal=0
      depth=-1
      beta=0.0_SRK
      start=1
      !Pull Data from Parameter List
      CALL validParams%get('AndersonAccelerationType->n',n)
      CALL validParams%get('AndersonAccelerationType->nlocal',nlocal)
      CALL validParams%get('AndersonAccelerationType->depth',depth)
      CALL validParams%get('AndersonAccelerationType->beta',beta)
      CALL validParams%get('AndersonAccelerationType->start',start)

      IF(.NOT. solver%isInit) THEN
        IF(n < 1) THEN
          CALL eAndersonAccelerationType%raiseError('Incorrect input to '// &
            modName//'::'//myName//' - Number of values (n) must be '// &
              'greater than 0!')
        ELSE
          solver%n=n
        ENDIF

        IF((nlocal < 1) .AND. (nlocal > n)) THEN
          CALL eAndersonAccelerationType%raiseError('Incorrect input to '// &
            modName//'::'//myName//' - Number of values (nlocal) must be '// &
              'greater than 0 and less than or equal to (n)!')
        ENDIF

        IF(depth < 0) THEN
          CALL eAndersonAccelerationType%raiseError('Incorrect input to '// &
            modName//'::'//myName//' - Depth values (depth) must be '// &
              'greater than or equal to 0!')
        ELSE
          solver%depth=depth
        ENDIF

        IF(start <= 0) THEN
          CALL eAndersonAccelerationType%raiseError('Incorrect input to '// &
            modName//'::'//myName//' - Starting iteration must be '// &
              'greater than to 0!')
        ENDIF

        IF((beta<=0.0_SRK) .OR. (beta>1.0_SRK)) THEN
          CALL eAndersonAccelerationType%raiseError('Incorrect input to '// &
            modName//'::'//myName//' - Beta must be '// &
              'greater than 0 and less than or equal to 1!')
        ELSE
          solver%beta=beta
        ENDIF

#ifdef FUTILITY_HAVE_Trilinos_NOX
        CALL tmpPL%clear()
        CALL tmpPL%add('VectorType->n',n)
        CALL tmpPL%add('VectorType->MPI_Comm_ID',solver%MPIparallelEnv%comm)
        CALL tmpPL%add('VectorType->nlocal',nlocal)
        CALL tmpPL%add('VectorType->engine',VM_TRILINOS)
        CALL VectorFactory(solver%X,tmpPL)
        CALL solver%X%set(0.0_SRK)

        SELECTTYPE(x=>solver%X); TYPE IS(TrilinosVectorType)
          CALL Anderson_Init(solver%id,solver%depth,solver%beta,start,x%b)
        ENDSELECT
#else
        CALL eAndersonAccelerationType%raiseError(modName//'::'//myName// &
          ' - NOX (Trilinos) is not present in build')
#endif
        solver%isInit=.TRUE.
      ELSE
        CALL eAndersonAccelerationType%raiseError('Incorrect call to '// &
          modName//'::'//myName//' - AndersonAccelerationType already initialized')
      ENDIF

    ENDSUBROUTINE init_AndersonAccelerationType
!
!-------------------------------------------------------------------------------
!> @brief Clears the Anderson Acceleration Type
!> @param solver The solver to act on
!>
!> This routine clears the data spaces
!>
    SUBROUTINE clear_AndersonAccelerationType(solver)
      CLASS(AndersonAccelerationType),INTENT(INOUT) :: solver

      NULLIFY(solver%MPIparallelEnv)
      IF(solver%OMPparallelEnv%isInit()) CALL solver%OMPparallelEnv%clear
      solver%iter=0
      solver%n=-1
      solver%depth=-1
      solver%beta=0.0_SRK
      IF(ASSOCIATED(solver%X)) THEN
        CALL solver%X%clear()
        DEALLOCATE(solver%X)
        NULLIFY(solver%X)
      ENDIF
#ifdef FUTILITY_HAVE_Trilinos_NOX
      CALL Anderson_Destroy(solver%id)
#endif
      solver%isInit=.FALSE.
    ENDSUBROUTINE clear_AndersonAccelerationType
!
!-------------------------------------------------------------------------------
!> @brief Single step of the Anderson acceleration
!> @param solver The anderson solver to act on
!>
!> This routine takes a single anderson acceleration step
!>
    SUBROUTINE step_AndersonAccelerationType(solver)
      CLASS(AndersonAccelerationType),INTENT(INOUT) :: solver
#ifdef FUTILITY_HAVE_Trilinos_NOX
      IF(solver%iter==0) THEN
        CALL Anderson_Reset(solver%id)
      ELSE
        CALL Anderson_Update(solver%id)
      ENDIF

      solver%iter=solver%iter+1
#endif
    ENDSUBROUTINE step_AndersonAccelerationType
!
!-------------------------------------------------------------------------------
!> @brief Reset the Anderson acceleration solver
!> @param solver The anderson solver to act on
!>
!> This routine resets the counters for the anderson acceleration
!>
    SUBROUTINE reset_AndersonAccelerationType(solver)
      CLASS(AndersonAccelerationType),INTENT(INOUT) :: solver
#ifdef FUTILITY_HAVE_Trilinos_NOX
      CALL Anderson_Reset(solver%id)
      solver%iter=1
#endif
    ENDSUBROUTINE reset_AndersonAccelerationType
!
ENDMODULE AndersonAccelerationTypes
