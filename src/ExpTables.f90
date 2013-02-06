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
!> @brief Module defines the exponent table type objects and methods.
!>
!> For characteristics solvers that have to evaluate an exponential 
!> (e.g. EXP()) a lot, it is often more computationally efficient to tabulate
!> evaluations of the exponential function and then to query the tables when
!> evaluating the exponential. This is because the table can often fit in L1
!> cache and because the EXP() function can take up to ~100 more FLOPS to 
!> evaluate than multiply or add operation.
!>
!> The object is initialized with a parameter list. For valid reference lists
!> see @ref ExpTables::ExpTables_Declare_ValidParams
!> "ExpTables_Declare_ValidParams".
!>
!> This module is largely based on the following paper:
!>
!> A. Yamamoto, et al., "Computational efficiencies of approximated functions
!> for transport calculations of the characteristics method", Annals of Nucl.
!> Energy, @b 31, pp. 1027-1037, (2004).
!>
!> The module provides a Fortran 2003 derived type that represents the
!> exponential table. The types of tables available are:
!>  - @c EXACT_EXP_TABLE no approximation same as evaluating EXP()
!>  - @c SINGLE_LEVEL_TABLE table has no interpolation and only one level
!>  - @c TWO_LEVEL_EXP_TABLE table has no interpolation and uses two levels
!>       based on the identity exp(x)*exp(y) = exp(x+y)
!>  - @c LINEAR_EXP_TABLE table linear interpolates exp(x) as m*x+b
!>  - @c ORDER2_EXP_TABLE is table does second order interpolation of exp(x)
!>
!> The tables are generally set up for the interval x=[-10,0] and can be 
!> specified for a given number of points in the table or a desired 
!> accuracy for the table.
!>
!> According to the above paper the @c TWO_LEVEL_EXP_TABLE and 
!> @c LINEAR_EXP_TABLE should perform favorably. This module is tested by
!> @c testExpTables.f90 which also measures performance if TAU/PAPI are
!> available on the test machine.
!>
!> @par Module Dependencies
!>  - @ref IntrType "IntrType": @copybrief IntrType
!>  - @ref ExceptionHandler "ExceptionHandler": @copybrief ExceptionHandler
!>  - @ref Allocs "Allocs": @copybrief Allocs
!>  - @ref ParameterLists "ParameterLists": @copybrief ParameterLists
!>
!> @author Zhouyu Liu and Brendan Kochunas
!>   @date 01/13/2012
!>
!> @par Revisions:
!> (5/4/2012) - Dan Jabaay
!>   - Modified tables to calculate 1-exp(x)
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE ExpTables

  USE IntrType
  USE Allocs
  USE ExceptionHandler
  USE ParameterLists

  IMPLICIT NONE
  PRIVATE
  
  !List of public members
  PUBLIC :: EXACT_EXP_TABLE
  PUBLIC :: SINGLE_LEVEL_EXP_TABLE
  PUBLIC :: TWO_LEVEL_EXP_TABLE
  PUBLIC :: LINEAR_EXP_TABLE
  PUBLIC :: ORDER2_EXP_TABLE
  PUBLIC :: ExpTableType
  PUBLIC :: eExpTable
  PUBLIC :: exptTbl
  PUBLIC :: EXPT
  PUBLIC :: ExpTableType_reqParams,ExpTableType_optParams
  PUBLIC :: ExpTables_Declare_ValidParams
  PUBLIC :: ExpTables_Clear_ValidParams
  
!Enumerations for the different table types

  !> Value indicating an exact exponential table
  INTEGER(SIK),PARAMETER :: EXACT_EXP_TABLE=1
  !> Value indicating a single level exponential table
  INTEGER(SIK),PARAMETER :: SINGLE_LEVEL_EXP_TABLE=2
  !> Value indicating a two level exponential table
  INTEGER(SIK),PARAMETER :: TWO_LEVEL_EXP_TABLE=3
  !> Value indicating an exponential table with linear approximation
  INTEGER(SIK),PARAMETER :: LINEAR_EXP_TABLE=4
  !> Value indicating an exponential table with quadratic approximation
  INTEGER(SIK),PARAMETER :: ORDER2_EXP_TABLE=5

  !> @brief An exponential table type object
  !>
  !> It is table that has evaluations of EXP(x) at specific points
  !> that can be queried or evaluated in kind to obtain an approximation
  !> to EXP(x) in significantly less time.
  TYPE :: ExpTableType
    !> Logical indicating initialization status
    LOGICAL(SBK) :: isinit=.FALSE.
    !> The type of exponential table
    INTEGER(SIK) :: tableType=-1
    !> The number of intervals in the table
    INTEGER(SIK) :: nintervals=-1
    !> The value between two entries in the table (delta x)
    REAL(SRK) :: dx=0._SRK
    !> The reciprocal of dx
    REAL(SRK) :: rdx=0._SRK
    !> The square of dx
    REAL(SRK) :: dx2rd=0._SRK
    !> The square of rdx
    REAL(SRK) :: rdx2rd=0._SRK
    !> The minimum value of the table
    INTEGER(SIK) :: minVal=0
    !> The maximum value of the table
    INTEGER(SIK) :: maxVal=0
    !> An upper bound estimate of the error of the table
    REAL(SRK) :: tableErr=0._SRK
    !> The first column of the table
    REAL(SRK),ALLOCATABLE :: table(:)
    !> The second column of the table (sometimes used)
    REAL(SRK),ALLOCATABLE :: table2rd(:)
    !> The third column of the table (sometimes used)
    REAL(SRK),ALLOCATABLE :: table3rd(:)
    !> Two dimensional table for better cache coherency
    REAL(SRK),ALLOCATABLE :: table2D(:,:)
    !> Procedure pointer to selected expt routine
    PROCEDURE(expT_absintfc),POINTER :: ptrExpT => NULL()
!
!List of type bound prodedures
    CONTAINS
      !> @copybrief ExpTables::init_ExpTable
      !> @copydetails ExpTables::init_ExpTable
      PROCEDURE,PASS :: initialize => init_ExpTable
      !> @copybrief ExpTables::EXPT
      !> @copydetails ExpTables::EXPT
      PROCEDURE,PASS :: EXPT
      !> @copybrief ExpTables::clear_ExpTable
      !> @copydetails ExpTables::clear_ExpTable
      PROCEDURE,PASS :: clear => clear_ExpTable
  ENDTYPE ExpTableType
!
  ABSTRACT INTERFACE
    ELEMENTAL FUNCTION expt_absintfc(thisExpT,x) RESULT(ans)
      IMPORT :: SRK,ExpTableType
      CLASS(ExpTableType),INTENT(IN) :: thisExpT
      REAL(SRK),INTENT(IN) :: x
      REAL(SRK) :: ans
    ENDFUNCTION expt_absintfc
  ENDINTERFACE
  
  !> Module name
  CHARACTER(LEN=*),PARAMETER :: modName='EXPTABLES'
  
  !> Exponent table object used outside of this modular
  TYPE(ExpTableType),SAVE,TARGET :: exptTbl
  
  !> Exception handler for the module
  TYPE(ExceptionHandlerType),POINTER,SAVE :: eExpTable => NULL()
  
  !> Logical indicates whether or not the parameter lists for validation
  !> have been initialized (only needs to be done once per execution)
  LOGICAL(SBK),SAVE :: ExpTableType_Paramsflag=.FALSE.
  
  !> The parameter lists to use when validating a parameter list for
  !> initialization;
  TYPE(ParamType),PROTECTED,SAVE :: ExpTableType_reqParams,ExpTableType_optParams
!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief Exponential function
!> @param myET Exponential table type object
!> @param x The variable
!> @param ans The return value
!>
    ELEMENTAL FUNCTION EXPT(myET,x) RESULT(ans)
      CLASS(ExpTableType),INTENT(IN) :: myET
      REAL(SRK),INTENT(IN) :: x
      REAL(SRK) :: ans
      
      IF(myET%minVal <= x .AND. x <= myET%maxVal) THEN
        ans=myET%ptrExpT(x)
        !SELECTCASE(myET%tableType)
        !  CASE (SINGLE_LEVEL_EXP_TABLE)
        !    ans=EXPT_Single(myET,x)
        !  CASE (TWO_LEVEL_EXP_TABLE)
        !    ans=EXPT_TwoLevel(myET,x)
        !  CASE (LINEAR_EXP_TABLE)
        !    ans=EXPT_Linear(myET,x)
        !  CASE (ORDER2_EXP_TABLE)
        !    ans=EXPT_TwoOrder(myET,x)
        !  CASE DEFAULT
        !    ans=1._SRK-EXP(x)
        !  ENDSELECT
      ELSE
        ans=1._SRK-EXP(x)
      ENDIF
    ENDFUNCTION
!
!-------------------------------------------------------------------------------
!> @brief Clears a exponent table type object
!> @param myET Exponential table type object
!>
    SUBROUTINE clear_ExpTable(myET)
      CLASS(ExpTableType),INTENT(INOUT) :: myET
      IF(ALLOCATED(myET%table)) CALL demallocA(myET%table)
      IF(ALLOCATED(myET%table2rd)) CALL demallocA(myET%table2rd)
      IF(ALLOCATED(myET%table3rd)) CALL demallocA(myET%table3rd)
      IF(ALLOCATED(myET%table2D)) CALL demallocA(myET%table2D)
      myET%tableType=-1
      myET%nintervals=-1
      myET%dx=0._SRK
      myET%rdx=0._SRK
      myET%dx2rd=0._SRK
      myET%rdx2rd=0._SRK
      myET%minVal=0._SRK
      myET%maxVal=0._SRK
      myET%tableErr=0._SRK
      myET%isinit=.FALSE.
    ENDSUBROUTINE clear_ExpTable
!
!-------------------------------------------------------------------------------
!> @brief Initialization method for ExpTableType object
!> @param myET Exponential table type object
!> @param Params The parameter list used to initialize the ExpTable object.
!>
!> @page ParamList Description of Parameter List
!> @param reqParamsExpTables This parameter list is null.
!> @param ExpTableType_optParams
!>   - <TT><'ExpTables -> tabletype'></TT> - Takes an integer as an input. The 
!>   integer is an enumeration specifying which table will be generated.  The
!>   options are: EXACT_EXP_TABLE, SINGLE_LEVEL_EXP_TABLE, TWO_LEVEL_EXP_TABLE,
!>   LINEAR_EXP_TABLE, and ORDER2_EXP_TABLE.  The default value is LINEAR_EXP_TABLE.
!>   - <TT><'ExpTables -> minval'></TT> - 
!>   - <TT><'ExpTables -> maxval'></TT> - 
!>   - <TT><'ExpTables -> nintervals'></TT> - 
!>   - <TT><'ExpTables -> error'></TT> - 
!>   - <TT><'ExpTables -> errorflag'></TT> - 
!>
    SUBROUTINE init_ExpTable(myET,Params)
      CHARACTER(LEN=*),PARAMETER :: myName="init_ExpTable"
      CLASS(ExpTableType),INTENT(INOUT) :: myET
      TYPE(ParamType),OPTIONAL,INTENT(IN) :: Params
     
      LOGICAL(SBK) :: localalloc,ErrFlag
      INTEGER(SIK) :: nerror,i,tableType,nintervals
      REAL(SRK) :: x,x2rd,tableErr,x1,x2,x3,y1,y2,y3,rdx2
      REAL(SRK) :: minVal,maxVal
      INTEGER(SIK) :: minTable,maxTable
      TYPE(ParamType) :: tmpList
      
      localalloc=.FALSE.
      IF(.NOT.ASSOCIATED(eExpTable)) THEN
        ALLOCATE(eExpTable)
        localalloc=.TRUE.
      ENDIF
      
      !Initialize reference parameter lists
      IF(.NOT.ExpTableType_Paramsflag) CALL ExpTables_Declare_ValidParams()
      
      !Input checking
      nerror=eExpTable%getcounter(EXCEPTION_ERROR)
      IF(myET%isinit) THEN
        CALL eExpTable%raiseError(modName//'::'//myName// &
          ' - Exponent table is already initialized!')
      ELSE
        IF(PRESENT(Params)) tmpList=Params
        CALL tmpList%validate(ExpTableType_reqParams,ExpTableType_optParams)
        !
        !Default value for table type
        CALL tmpList%get('ExpTables -> tabletype',tableType)
        IF(tableType > 5 .OR. tableType < 1) THEN
          CALL eExpTable%raiseWarning(modName//'::'//myName// &
            ' - Exponent table type is not correct input!'// &
              ' Using default table type!')
          !Get Default param
          CALL ExpTableType_optParams%get('ExpTables -> tabletype',tableType)
        ENDIF
        !Default value for minVal and maxVal
        CALL tmpList%get('ExpTables -> minval',minVal)
        CALL tmpList%get('ExpTables -> maxval',maxVal)
        IF(minVal >= 0_SRK) THEN
          CALL eExpTable%raiseWarning(modName//'::'//myName// &
            ' - Minimum value of the range of the table is not negative!'// &
              ' Using default table type!')
          !Get Default param
          CALL ExpTableType_optParams%get('ExpTables -> minval',minVal)
        ENDIF
        IF(maxVal > 0_SRK) THEN
          CALL eExpTable%raiseWarning(modName//'::'//myName// &
            ' - Maximum value of the range of the table is positive!'// &
              ' Using default table type!')
          !Get Default param
          CALL ExpTableType_optParams%get('ExpTables -> maxval',maxVal)
        ENDIF
        IF(maxVal == minVal) &
          CALL eExpTable%raiseError(modName//'::'//myName// &
            ' - Maximum value of the range of the table!'// &
              ' is equal to the minimum value!')
        !Default value for ninterval
        CALL tmpList%get('ExpTables -> nintervals',nintervals)
        IF(nintervals <= 1) THEN
          CALL eExpTable%raiseWarning(modName//'::'//myName// &
            ' - Number of intervals is less than or equal to 1!'// &
              ' Using default value!')
          !Get Default param
          CALL ExpTableType_optParams%get('ExpTables -> nintervals',nintervals)
        ENDIF
        !Check the ErrFlag to see if we need to recalculate nintervals
        CALL tmpList%get('ExpTables -> errorflag',ErrFlag)
        !Overwrite nintervals using the error
        IF(ErrFlag) THEN
          !Get the error to recalculate the nintervals
          CALL tmpList%get('ExpTables -> error',tableErr)
          CALL eExpTable%raiseWarning(modName//'::'//myName// &
            ' - Number of intervals is overwritten by the value that'// &
              ' is determined from desired error!')
          SELECTCASE(tableType)
            CASE (EXACT_EXP_TABLE)
              !Do nothing!
            CASE (SINGLE_LEVEL_EXP_TABLE)
              nintervals=NINT(ABS(0.5_SRK/tableErr))
            CASE (TWO_LEVEL_EXP_TABLE)
              nintervals=NINT(SQRT(ABS(0.5_SRK/tableErr)))
            CASE (LINEAR_EXP_TABLE)
              nintervals=NINT(SQRT(ABS(0.125_SRK/tableErr)))
            CASE (ORDER2_EXP_TABLE)
              nintervals=NINT(ABS(9.630017699314371e-3_SRK/tableErr)**0.3333333333333333_SRK)
            CASE DEFAULT
              CALL eExpTable%raiseError(modName//'::'//myName// &
                ' - Exponent table type is incorrect!')
          ENDSELECT
        !Ignore the user error and keep the input nintervals 
        ELSE
          SELECTCASE(tableType)
            CASE (EXACT_EXP_TABLE)
              tableErr=0._SRK
            CASE (SINGLE_LEVEL_EXP_TABLE)
              tableErr=0.5_SRK/nintervals
            CASE (TWO_LEVEL_EXP_TABLE)
              tableErr=0.5_SRK/(nintervals*nintervals)
            CASE (LINEAR_EXP_TABLE)
              tableErr=0.125_SRK/(nintervals*nintervals)
            CASE (ORDER2_EXP_TABLE)
              tableErr=9.630017699314371e-3_SRK/(nintervals*nintervals*nintervals)
            CASE DEFAULT
              tableErr=0._SRK
          ENDSELECT
        ENDIF
        IF(nerror == eExpTable%getcounter(EXCEPTION_ERROR)) THEN
          minTable=minVal*nintervals
          maxTable=maxVal*nintervals
          SELECTCASE(tableType)
            CASE (EXACT_EXP_TABLE)
              myET%ptrExpT=>EXPT_Exact
              myET%tableType=EXACT_EXP_TABLE
            CASE(SINGLE_LEVEL_EXP_TABLE)
              myET%ptrExpT=>EXPT_Single
              CALL dmalloc0A(myET%table,minTable,maxTable)
              myET%nintervals=nintervals
              myET%tableType=tableType
              myET%maxVal=maxVal
              myET%minVal=minVal
              myET%rdx=REAL(nintervals,SRK)
              myET%dx=1._SRK/myET%rdx
              myET%tableErr=tableErr
              x=minVal
              DO i=minTable,maxTable
                myET%table(i)=1._SRK-EXP(x)
                x=x+myET%dx
              ENDDO
            CASE(TWO_LEVEL_EXP_TABLE)
              myET%ptrExpT=>EXPT_TwoLevel
              CALL dmalloc0A(myET%table,minTable,maxTable)
              CALL dmallocA(myET%table2rd,nintervals)
              myET%nintervals=nintervals
              myET%tableType=tableType
              myET%maxVal=maxVal
              myET%minVal=minVal
              myET%rdx=REAL(nintervals,SRK)
              myET%dx=1._SRK/myET%rdx
              myET%dx2rd=myET%dx*myET%dx
              myET%rdx2rd=myET%rdx*myET%rdx
              myET%tableErr=tableErr
              x=minVal
              !The two-level table still tabulates EXP(x), not 1-EXP(x)
              !because it makes use of the identity EXP(x+y)=EXP(x)*EXP(y)
              DO i=minTable,maxTable
                myET%table(i)=EXP(x)
                x=x+myET%dx
              ENDDO
              x2rd=0._SRK
              DO i=1,nintervals
                x2rd=x2rd+myET%dx2rd
                myET%table2rd(i)=EXP(x2rd)
              ENDDO
            CASE(LINEAR_EXP_TABLE)
              myET%ptrExpT=>EXPT_Linear
              CALL dmalloc0A(myET%table2D,1,2,minTable,maxTable)
              myET%nintervals=nintervals
              myET%tableType=tableType
              myET%maxVal=maxVal
              myET%minVal=minVal
              myET%rdx=REAL(nintervals,SRK)
              myET%dx=1._SRK/myET%rdx
              myET%tableErr=tableErr
              x1=minVal
              y1=1._SRK-EXP(x1)
              DO i=minTable,maxTable
                x2=x1+myET%dx
                y2=1._SRK-EXP(x2)
                myET%table2D(1,i)=(y2-y1)*myET%rdx
                myET%table2D(2,i)=y1-myET%table2D(1,i)*x1
                x1=x2
                y1=y2
              ENDDO
            CASE(ORDER2_EXP_TABLE)
              myET%ptrExpT=>EXPT_TwoOrder
              CALL dmalloc0A(myET%table,minTable,maxTable)
              CALL dmalloc0A(myET%table2rd,minTable,maxTable)
              CALL dmalloc0A(myET%table3rd,minTable,maxTable)
              !The interpolation formula
              !f(x)=f(x1)*(x-x2)*(x-x3)/((x1-x2)*(x1-x3))
              !    +f(x2)*(x-x1)*(x-x3)/((x2-x1)*(x2-x3))
              !    +f(x3)*(x-x1)*(x-x2)/((x3-x1)*(x3-x2))
              !==>
              !f(x)=a+b*x+c*x*x
              !and a=x2*x3*f(x1)/((x1-x2)*(x1-x3))+x1*x3*f(x2)/((x2-x1)*(x2-x3)) &
              !     +x1*x2*f(x3)/((x3-x1)*(x3-x2))
              !    b=-(x2+x3)*f(x1)/((x1-x2)*(x1-x3))-(x1+x3)*f(x2)/((x2-x1)*(x2-x3)) &
              !     -(x1+x2)*f(x3)/((x3-x1)*(x3-x2))
              !    c=*f(x1)/((x1-x2)*(x1-x3))+*f(x2)/((x2-x1)*(x2-x3)) &
              !     +*f(x3)/((x3-x1)*(x3-x2))
              myET%nintervals=nintervals
              myET%tableType=tableType
              myET%maxVal=maxVal
              myET%minVal=minVal
              myET%rdx=REAL(nintervals,SRK)
              myET%dx=1._SRK/myET%rdx
              myET%tableErr=tableErr
              x1=minVal
              y1=1._SRK-EXP(x1)
              x2=x1+myET%dx
              y2=1._SRK-EXP(x2)
              rdx2=myET%rdx*myET%rdx
              DO i=minTable,maxTable
                x3=x2+myET%dx
                y3=1._SRK-EXP(x3)
                myET%table(i)=((y1+y3)*0.5_SRK-y2)*rdx2
                myET%table2rd(i)=(-((x2+x3)*y1+(x1+x2)*y3)*0.5_SRK+(x1+x3)*y2)*rdx2
                myET%table3rd(i)=((x3*y1+x1*y3)*x2*0.5_SRK-x1*x3*y2)*rdx2
                x1=x2
                x2=x3
                y1=y2
                y2=y3
              ENDDO
          ENDSELECT
          myET%isinit=.TRUE.
          CALL tmpList%clear()
        ENDIF
      ENDIF
      IF(localalloc) DEALLOCATE(eExpTable)
    ENDSUBROUTINE init_ExpTable
!
!-------------------------------------------------------------------------------
!> @brief Exponential function evaluated exactly
!> @param myET Exponential table type object
!> @param x the value to evaluate
!> @return ans the return value equal to 1-EXP(x) 
!>
!> This routine returns the exact answer to 1-EXP(x)
!>
    ELEMENTAL FUNCTION EXPT_Exact(myET,x) RESULT(ans)
      CLASS(ExpTableType),INTENT(IN) :: myET
      REAL(SRK),INTENT(IN) :: x
      REAL(SRK) :: ans
      ans=1._SRK-EXP(x)
    ENDFUNCTION EXPT_Exact
!
!-------------------------------------------------------------------------------
!> @brief Exponential function evaluated by single-level table without 
!> interpolation
!> @param myET Exponential table type object
!> @param x the value to evaluate
!> @return ans the return value approximate to 1-EXP(x) 
!>
!> It is assumed x is in the interval [myET%minVal,myET%maxVal], which is
!> checked in the calling routine which is the only public interface to this
!> routine.
!>
    ELEMENTAL FUNCTION EXPT_Single(myET,x) RESULT(ans)
      CLASS(ExpTableType),INTENT(IN) :: myET
      REAL(SRK),INTENT(IN) :: x
      REAL(SRK) :: ans
      INTEGER(SIK) :: i
      i=NINT(x*myET%rdx)
      ans=myET%table(i)
    ENDFUNCTION EXPT_Single
!
!-------------------------------------------------------------------------------
!> @brief Exponential function evaluated by two-level table without
!> interpolation
!> @param myET Exponential table type object
!> @param x the value to evaluate
!> @return ans the return value approximate to 1-EXP(x) 
!>
!> It is assumed x is in the interval [myET%minVal,myET%maxVal], which is
!> checked in the calling routine which is the only public interface to this
!> routine.
!>
    ELEMENTAL FUNCTION EXPT_TwoLevel(myET,x) RESULT(ans)
      CLASS(ExpTableType),INTENT(IN) :: myET
      REAL(SRK),INTENT(IN) :: x
      REAL(SRK) :: ans
      
      INTEGER(SIK) :: i,i2rd
      REAL(SRK) :: mid
      !
      mid=x*myET%nintervals
      i=FLOOR(mid)
      i2rd=INT((mid-i)*myET%nintervals)
      ans=myET%table(i)
      IF(i2rd /= 0_SIK) ans=ans*myET%table2rd(i2rd)
      ans=1._SRK-ans
    ENDFUNCTION EXPT_TwoLevel
!
!-------------------------------------------------------------------------------
!> @brief Exponential function evaluated by table with linear interpolation
!> @param myET Exponential table type object
!> @param x the value to evaluate
!> @return ans the return value approximate to 1-EXP(x) 
!>
!> It is assumed x is in the interval [myET%minVal,myET%maxVal], which is
!> checked in the calling routine which is the only public interface to this
!> routine.
!>
    ELEMENTAL FUNCTION EXPT_Linear(myET,x) RESULT(ans)
      CLASS(ExpTableType),INTENT(IN) :: myET
      REAL(SRK),INTENT(IN) :: x
      REAL(SRK) :: ans
      INTEGER(SIK) :: i
      i=FLOOR(x*myET%rdx)
      !ans=myET%table(i)*x+myET%table2rd(i)
      ans=myET%table2D(1,i)*x+myET%table2D(2,i)
    ENDFUNCTION EXPT_Linear
!-------------------------------------------------------------------------------
!> @brief Exponential function evaluated by table with second order 
!> interpolation
!> @param myET Exponential table type object
!> @param x the value to evaluate
!> @return ans the return value approximate to 1-EXP(x) 
!>
!> It is assumed x is in the interval [myET%minVal,myET%maxVal], which is
!> checked in the calling routine which is the only public interface to this
!> routine.
!>
    ELEMENTAL FUNCTION EXPT_TwoOrder(myET,x) RESULT(ans)
      CLASS(ExpTableType),INTENT(IN) :: myET
      REAL(SRK),INTENT(IN) :: x      
      REAL(SRK) :: ans
      INTEGER(SIK) :: i
      i=FLOOR(x*myET%rdx)
      ans=(myET%table2rd(i)+myET%table(i)*x)*x+myET%table3rd(i)
    ENDFUNCTION EXPT_TwoOrder
!
!-------------------------------------------------------------------------------
!> @brief Subroutine that sets up the default parameter lists for the 
!>        ExpTableType.
!> The required parameters for the ExpTableType do not exist.
!> The optional parameters for the ExpTableType are:
!>        'ExpTableType->tabletype',SIK
!>        'ExpTableType->minval',SRK
!>        'ExpTableType->maxval',SRK
!>        'ExpTableType->nintervals',SIK
!>        'ExpTableType->error',SRK
!>        'ExpTableType->errorflag',SBK
!> 
    SUBROUTINE ExpTables_Declare_ValidParams()

      ExpTableType_Paramsflag=.TRUE.

      !Set names for required parameters
      !Set defaults for optional parameters
      CALL ExpTableType_optParams%add('ExpTables -> tabletype',LINEAR_EXP_TABLE, &
        'The default ExpTable is just a linear level lookup table.')
      CALL ExpTableType_optParams%add('ExpTables -> minval',-10._SRK, &
        'The default minimum value in the exponential table.')
      CALL ExpTableType_optParams%add('ExpTables -> maxval',0._SRK, &
        'The default maximum value in the exponential table.')
      CALL ExpTableType_optParams%add('ExpTables -> nintervals',1000, &
        'The default value for the number of intervals in the exponential table.')
      CALL ExpTableType_optParams%add('ExpTables -> error',0.0005_SRK, &
        'The default value for the error in the exponential table.')
      CALL ExpTableType_optParams%add('ExpTables -> errorflag',.FALSE., &
        'The default value for the error in the exponential table.')
      
    ENDSUBROUTINE ExpTables_Declare_ValidParams

!-------------------------------------------------------------------------------
!> @brief Subroutine that clears the default parameter lists for the 
!>        ExpTableType.
!> 
    SUBROUTINE ExpTables_Clear_ValidParams()

      ExpTableType_Paramsflag=.FALSE.

      !ExpTableType required parameters
      !ExpTableType optional parameters
      CALL ExpTableType_optParams%clear()
    ENDSUBROUTINE ExpTables_Clear_ValidParams
!
ENDMODULE ExpTables
