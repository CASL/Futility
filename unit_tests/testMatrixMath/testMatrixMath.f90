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
PROGRAM testMatrixMath
  
  USE IntrType
  USE ExceptionHandler
  USE BLAS
  USE VectorTypes
  IMPLICIT NONE
  
  TYPE(ExceptionHandlerType),POINTER :: e
  

  !Configure exception handler for test
  ALLOCATE(e)
  CALL e%setStopOnError(.FALSE.)
  CALL e%setQuietMode(.TRUE.)
  eMatrixType => e

  WRITE(*,*) '==================================================='
  WRITE(*,*) 'TESTING MATRIX MATH...'
  WRITE(*,*) '==================================================='

  WRITE(*,*) 'TESTING BLAS LEVEL 1'
  CALL testBLAS1()
  WRITE(*,*) '---------------------------------------------------'
  
  WRITE(*,*) 'TESTING BLAS LEVEL 2'
  CALL testBLAS2()
  WRITE(*,*) '---------------------------------------------------'
  
  WRITE(*,*) 'TESTING BLAS LEVEL 3'
  CALL testBLAS3()
  WRITE(*,*) '---------------------------------------------------'
  
  WRITE(*,*) 'TESTING MATRIX TYPES'
  CALL testMatrixTypes()
  
  WRITE(*,*) 'TESTING VECTOR TYPES'
  CALL testVectorTypes()
  
  WRITE(*,*) '==================================================='
  WRITE(*,*) 'TESTING MATRIX MATH PASSED!'
  WRITE(*,*) '==================================================='
  DEALLOCATE(e)
!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
    SUBROUTINE testVectorTypes()
      CLASS(VectorType),ALLOCATABLE :: thisVector

!Test for real vectors      
      ALLOCATE(RealVectorType :: thisVector)
      SELECTTYPE(thisVector)
        TYPE IS(RealVectorType)
          !test clear
          !make vector without using untested init
          thisVector%isInit=.TRUE.
          thisVector%n=100
          ALLOCATE(thisVector%b(100))
      ENDSELECT
        
      !clear it
      CALL thisVector%clear()
        
      SELECTTYPE(thisVector)
        TYPE IS(RealVectorType)
          !check for success
          IF((thisVector%isInit).OR.(thisVector%n /= 0)) THEN
            WRITE(*,*) 'CALL realvec%clear() FAILED!'
            STOP 666
          ENDIF
          IF(ALLOCATED(thisVector%b)) THEN
            WRITE(*,*) 'CALL realvec%clear() FAILED!'
            STOP 666
          ENDIF
          WRITE(*,*) '  Passed: CALL realvec%clear()'
      ENDSELECT
 
 
!Test for PETSc vectors
      ALLOCATE(PETScVectorType :: thisVector)
      CALL thisVector%clear()
      WRITE(*,*) '  Passed: CALL petscvec%clear(...)'
      
    ENDSUBROUTINE testVectorTypes
      
    
!
!-------------------------------------------------------------------------------
    SUBROUTINE testMatrixTypes()
      CLASS(MatrixType),ALLOCATABLE :: thisMatrix
      INTEGER(SIK) :: i
      INTEGER(SIK) :: ia_vals(4)
      INTEGER(SIK) :: ja_vals(6)
      REAL(SRK) :: a_vals(6),x(3),y(3)
      REAL(SRK) :: dummy
      
      ALLOCATE(SparseMatrixType :: thisMatrix)
      SELECTTYPE(thisMatrix)
        TYPE IS(SparseMatrixType)
!Test for sparse matrices
          !test clear
          !make matrix w/out using untested init
          thisMatrix%isInit=.TRUE.
          thisMatrix%n=100
          thisMatrix%nnz=100
          thisMatrix%jCount=100
          thisMatrix%iPrev=100
          thisMatrix%jPrev=100
          ALLOCATE(thisMatrix%ia(101))
          ALLOCATE(thisMatrix%a(100))
          ALLOCATE(thisMatrix%ja(100))
          thisMatrix%ia(101)=101
      ENDSELECT
        
      !clear it
      CALL thisMatrix%clear()
        
      SELECTTYPE(thisMatrix)
        TYPE IS(SparseMatrixType)
          !check for success
          IF(((thisMatrix%jPrev /= 0).OR.(thisMatrix%iPrev /= 0)) &
            .OR.(((thisMatrix%isInit).OR.(thisMatrix%jCount /= 0)) &
            .OR.((thisMatrix%nnz /= 0).OR.(thisMatrix%n /= 0)))) THEN
            WRITE(*,*) 'CALL sparse%clear() FAILED!'
            STOP 666
          ENDIF
          IF((ALLOCATED(thisMatrix%a).OR.ALLOCATED(thisMatrix%ja)) &
            .OR.ALLOCATED(thisMatrix%ia)) THEN
            WRITE(*,*) 'CALL sparse%clear() FAILED!'
            STOP 666
          ENDIF
          WRITE(*,*) '  Passed: CALL sparse%clear()'
      ENDSELECT
          
      !check init
      !first check intended init path (m provided)
      eMatrixType => NULL()
      CALL thisMatrix%init(10,10)
      eMatrixType => e
      SELECTTYPE(thisMatrix)
        TYPE IS(SparseMatrixType)
          !check for success
          IF((((.NOT.thisMatrix%isInit).AND.(thisMatrix%jCount /= 0)) &
            .AND.((thisMatrix%nnz /= 10).AND.(thisMatrix%n /= 10))) &
            .AND.((thisMatrix%jPrev /=0).AND.(thisMatrix%iPrev /=0))) THEN
            WRITE(*,*) 'CALL sparse%init(...) FAILED!'
            STOP 666
          ENDIF
          IF(((SIZE(thisMatrix%a) /= 10).AND.(SIZE(thisMatrix%ja) /= 10)) &
            .AND.((SIZE(thisMatrix%ia) /= 11).AND.(thisMatrix%ia(11) /= 11))) THEN
            WRITE(*,*) 'CALL sparse%init(...) FAILED!'
            STOP 666
          ENDIF
      ENDSELECT  
      CALL thisMatrix%clear()
        
      !now check init without m being provided
      CALL thisMatrix%init(10) !expect exception
      IF(thisMatrix%isInit) THEN
        WRITE(*,*) 'CALL sparse%init(...) FAILED!'
        STOP 666
      ENDIF
      CALL thisMatrix%clear()
        
      !init it twice so on 2nd init, isInit==.TRUE.
      CALL thisMatrix%init(10,10)
      SELECTTYPE(thisMatrix)
        TYPE IS(SparseMatrixType); thisMatrix%nnz=1
      ENDSELECT
      CALL thisMatrix%init(10,10)
      SELECTTYPE(thisMatrix)
        TYPE IS(SparseMatrixType)
          IF(thisMatrix%nnz/=1) THEN !nnz/=1 implies it was changed, and thus fail
            WRITE(*,*) 'CALL sparse%init(...) FAILED!' !expect exception
            STOP 666
          ENDIF
      ENDSELECT
      !init with n<1
      CALL thisMatrix%clear()
      CALL thisMatrix%init(-1,10) !expect exception
      IF(thisMatrix%isInit) THEN
        WRITE(*,*) 'CALL sparse%init(...) FAILED!'
        STOP 666
      ENDIF
      CALL thisMatrix%clear()
      !n<1, and m not provided
      CALL thisMatrix%init(-1) !expect exception
      IF(thisMatrix%isInit) THEN
        WRITE(*,*) 'CALL sparse%init(...) FAILED!'
        STOP 666
      ENDIF
      CALL thisMatrix%clear()
      !init with m<1
      CALL thisMatrix%clear()
      CALL thisMatrix%init(10,-10) !expect exception
      IF(thisMatrix%isInit) THEN
        WRITE(*,*) 'CALL sparse%init(...) FAILED!'
        STOP 666
      ENDIF
      CALL thisMatrix%clear()
      WRITE(*,*) '  Passed: CALL sparse%init(...)'
      !test setShape
      !intend to make: [1 0 2]
      !                [0 0 3]
      !                [4 5 6]
      DO i=1,6
        a_vals(i)=i
      ENDDO
      ja_vals(1)=1
      ja_vals(2)=3
      ja_vals(3)=3
      ja_vals(4)=1
      ja_vals(5)=2
      ja_vals(6)=3
      ia_vals(1)=1
      ia_vals(2)=3
      ia_vals(3)=4
      ia_vals(4)=7
      CALL thisMatrix%clear()
      CALL thisMatrix%init(3,6)
        
      SELECTTYPE(thisMatrix)
        TYPE IS(SparseMatrixType)
          CALL thisMatrix%setShape(1,1,1._SRK)
          CALL thisMatrix%setShape(1,3,2._SRK)
          CALL thisMatrix%setShape(2,3,3._SRK)
          CALL thisMatrix%setShape(3,1,4._SRK)
          CALL thisMatrix%setShape(3,2,5._SRK)
          CALL thisMatrix%setShape(3,3,6._SRK)
          !expect:
          !a:  [1 2 3 4 5 6]
          !ja: [1 3 3 1 2 3]
          !ia: [1 3 4 7]
          DO i=1,6
            IF((thisMatrix%a(i) /= a_vals(i)) &
              .OR. (thisMatrix%ja(i) /= ja_vals(i))) THEN
              WRITE(*,*) 'CALL sparse%setShape(...) FAILED!'
              STOP 666
            ENDIF
            IF(i < 5) THEN
              IF(thisMatrix%ia(i) /= ia_vals(i)) THEN
                WRITE(*,*) 'CALL sparse%setShape(...) FAILED!'
                STOP 666
              ENDIF
            ENDIF
          ENDDO
          IF((thisMatrix%jPrev /= 3).OR.(thisMatrix%iPrev /= 3)) THEN
            WRITE(*,*) 'CALL sparse%setShape(...) FAILED!'
            STOP 666
          ENDIF
          !repeat same as before, but dont provide set_val
          CALL thisMatrix%clear()
          CALL thisMatrix%init(3,6)
          CALL thisMatrix%setShape(1,1)
          CALL thisMatrix%setShape(1,3)
          CALL thisMatrix%setShape(2,3)
          CALL thisMatrix%setShape(3,1)
          CALL thisMatrix%setShape(3,2)
          CALL thisMatrix%setShape(3,3)
          !check to make sure a is all zeros
          DO i=1,6
            IF(thisMatrix%a(i) /= 0) THEN
              WRITE(*,*) 'CALL sparse%setShape(...) FAILED!'
              STOP 666
            ENDIF
          ENDDO
          CALL thisMatrix%clear()
          !off-nominal setShape cases
          !test matrix with isInit set to false artificially
          CALL thisMatrix%init(3,6)
          thisMatrix%isInit=.FALSE.
          CALL thisMatrix%SetShape(1,1,1._SRK)
          IF(thisMatrix%a(1) == 1._SRK) THEN
            WRITE(*,*) 'CALL sparse%setShape(...) FAILED!'
            STOP 666
          ENDIF
          CALL thisMatrix%clear()
          !test case where i and j are out of bounds (one at a time)
          CALL thisMatrix%init(3,6)
          CALL thisMatrix%setShape(-1,1,2._SRK) !i<1
          IF(thisMatrix%a(1) == 2._SRK) THEN
            WRITE(*,*) 'CALL sparse%setShape(...) FAILED!'
            STOP 666
          ENDIF
          CALL thisMatrix%clear
          CALL thisMatrix%init(3,6)
          CALL thisMatrix%setShape(1,-1,2._SRK) !j<1
          IF(thisMatrix%a(1) == 2._SRK) THEN
            WRITE(*,*) 'CALL sparse%setShape(...) FAILED!'
            STOP 666
          ENDIF
          CALL thisMatrix%clear
          CALL thisMatrix%init(3,6)
          CALL thisMatrix%setShape(4,1,2._SRK) !i>n
          IF(thisMatrix%a(1) == 2._SRK) THEN
            WRITE(*,*) 'CALL sparse%setShape(...) FAILED!'
            STOP 666
          ENDIF
          !test check to see if i,j of new entry are below and to the right 
          !of previous i.j
          CALL thisMatrix%clear()
          CALL thisMatrix%init(3,6)
          CALL thisMatrix%setShape(1,2,1._SRK)
          CALL thisMatrix%setShape(1,1,2._SRK)
          IF(thisMatrix%a(2) == 2._SRK) THEN
            WRITE(*,*) 'CALL sparse%setShape(...) FAILED!'
            STOP 666
          ENDIF
          CALL thisMatrix%clear
          CALL thisMatrix%init(3,6)
          CALL thisMatrix%setShape(1,1,1._SRK)
          CALL thisMatrix%setShape(2,2,2._SRK)
          CALL thisMatrix%setShape(1,3,3._SRK)
          IF(thisMatrix%a(3) == 3._SRK) THEN
            WRITE(*,*) 'CALL sparse%setShape(...) FAILED!'
            STOP 666
          ENDIF
          WRITE(*,*) '  Passed: CALL sparse%setShape(...)'
          !test set (first initialize the values w/ setShape)
          CALL thisMatrix%clear()
          CALL thisMatrix%init(3,6)
          CALL thisMatrix%setShape(1,1)
          CALL thisMatrix%setShape(1,3)
          CALL thisMatrix%setShape(2,3)
          CALL thisMatrix%setShape(3,1)
          CALL thisMatrix%setShape(3,2)
          CALL thisMatrix%setShape(3,3)
      ENDSELECT
      
      !use set to update the values
      CALL thisMatrix%set(1,1,1._SRK)
      CALL thisMatrix%set(1,3,2._SRK)
      CALL thisMatrix%set(2,3,3._SRK)
      CALL thisMatrix%set(3,1,4._SRK)
      CALL thisMatrix%set(3,2,5._SRK)
      CALL thisMatrix%set(3,3,6._SRK)
      SELECTTYPE(thisMatrix)
        TYPE IS(SparseMatrixType)
          !now compare actual values with expected
          DO i=1,6
            IF((thisMatrix%a(i) /= a_vals(i)) &
              .OR. (thisMatrix%ja(i) /= ja_vals(i))) THEN
              WRITE(*,*) 'CALL sparse%set(...) FAILED!'
              STOP 666
            ENDIF
            IF(i < 5) THEN
              IF(thisMatrix%ia(i) /= ia_vals(i)) THEN
                WRITE(*,*) 'CALL sparse%set(...) FAILED!'
                STOP 666
              ENDIF
            ENDIF
          ENDDO
      ENDSELECT
      
      !Test BLAS_matvec
      x=1.0_SRK
      y=1.0_SRK
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=x,Y=y)
      IF(ANY(.NOT.(y .APPROXEQ. (/4._SRK,4._SRK,16._SRK/)))) THEN
        WRITE(*,*) "CALL BLAS_matvec(THISMATRIX=thisMatrix,X=x,Y=y) -sparse FAILED!"
        STOP 666
      ENDIF
      y=1.0_SRK
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=x,BETA=2.0_SRK,Y=y)
      IF(ANY(.NOT.(y .APPROXEQ. (/5._SRK,5._SRK,17._SRK/)))) THEN
        WRITE(*,*) "CALL BLAS_matvec(THISMATRIX=thisMatrix,X=x,BETA=2.0_SRK,Y=y) -sparse FAILED!"
        STOP 666
      ENDIF
      y=1.0_SRK
      CALL BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=x,Y=y)
      IF(ANY(.NOT.(y .APPROXEQ. (/7._SRK,7._SRK,31._SRK/)))) THEN
        WRITE(*,*) "CALL BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=x,Y=y) -sparse FAILED!"
        STOP 666
      ENDIF
      y=1.0_SRK
      CALL BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=x,BETA=2.0_SRK,Y=y)
      IF(ANY(.NOT.(y .APPROXEQ. (/8._SRK,8._SRK,32._SRK/)))) THEN
        WRITE(*,*) "CALL BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=x,BETA=2.0_SRK,Y=y) -sparse FAILED!"
        STOP 666
      ENDIF
      WRITE(*,*) '  Passed: CALL BLAS_matvec(...) sparse-matrix'
      
      !Test BLAS_matmult when SparseMatType is supported.
      
      !off-nominal tests
      !pass matrix w/out setshape
      CALL thisMatrix%clear()
      CALL thisMatrix%init(3,6)
      CALL thisMatrix%set(1,1,1._SRK) !since jCount = 0, expect no change
      SELECTTYPE(thisMatrix)
        TYPE IS(SparseMatrixType)
          DO i=1,SIZE(thisMatrix%a)
            IF(thisMatrix%a(i) == 1._SRK) THEN
              WRITE(*,*) 'CALL sparse%set(...) FAILED!'
              STOP 666
            ENDIF
          ENDDO
      ENDSELECT
      !set uninit matrix.
      CALL thisMatrix%clear()
      CALL thisMatrix%set(1,1,1._SRK) !since isInit=.FALSE. expect no change
      !no crash? good.
      !pass out-of bounds i and j
      CALL thisMatrix%clear()
      CALL thisMatrix%init(3,6)
      SELECTTYPE(thisMatrix)
        TYPE IS(SparseMatrixType); CALL thisMatrix%setShape(1,1)
      ENDSELECT
          
      CALL thisMatrix%set(-1,1,1._SRK)
      CALL thisMatrix%set(1,-1,1._SRK)
      CALL thisMatrix%set(4,1,1._SRK)
      CALL thisMatrix%set(1,4,1._SRK)
      !no crash? good.
      !i,j not in pre-defined shape
      CALL thisMatrix%clear()
      CALL thisMatrix%init(3,6)
        
      SELECTTYPE(thisMatrix)
        TYPE IS(SparseMatrixType)
          CALL thisMatrix%setShape(1,1)
          CALL thisMatrix%setShape(2,2)
      ENDSELECT
      CALL thisMatrix%set(1,2,1._SRK)
        
      SELECTTYPE(thisMatrix)
        TYPE IS(SparseMatrixType)
          DO i=1,SIZE(thisMatrix%a)
            IF(thisMatrix%a(i) == 1._SRK) THEN
              WRITE(*,*) 'CALL sparse%set(...) FAILED!'
              STOP 666
            ENDIF
          ENDDO
          WRITE(*,*) '  Passed: CALL sparse%set(...)'
      ENDSELECT
      
      !Perform test of functionality of get function
      ![1 0 2]
      ![0 0 3]
      ![4 5 6]
      CALL thisMatrix%clear()
      CALL thisMatrix%init(3,6)
      SELECTTYPE(thisMatrix)
        TYPE IS(SparseMatrixType)
          CALL thisMatrix%setShape(1,1,1._SRK)
          CALL thisMatrix%setShape(1,3,2._SRK)
          CALL thisMatrix%setShape(2,3,3._SRK)
          CALL thisMatrix%setShape(3,1,4._SRK)
          CALL thisMatrix%setShape(3,2,5._SRK)
          CALL thisMatrix%setShape(3,3,6._SRK)
          IF(((thisMatrix%get(1,1) /= 1._SRK)  .OR. &
              (thisMatrix%get(2,1) /= 0._SRK)) .OR. &
              (thisMatrix%get(3,1) /= 4._SRK)) THEN
            WRITE(*,*) 'CALL sparse%get(...) FAILED!' !column one check
            STOP 666
          ELSEIF(((thisMatrix%get(1,2) /= 0._SRK).OR. &
                (thisMatrix%get(2,2) /= 0._SRK)) .OR. &
                (thisMatrix%get(3,2) /= 5._SRK)) THEN
            WRITE(*,*) 'CALL sparse%get(...) FAILED!' !column one check
            STOP 666
          ELSEIF(((thisMatrix%get(1,3) /= 2._SRK).OR. &
                (thisMatrix%get(2,3) /= 3._SRK)) .OR. &
                (thisMatrix%get(3,3) /= 6._SRK)) THEN
            WRITE(*,*) 'CALL sparse%get(...) FAILED!' !column one check
            STOP 666
          ENDIF
      ENDSELECT
      !test with out of bounds i,j, make sure no crash.
      SELECTTYPE(thisMatrix)
        TYPE IS(SparseMatrixType)
          dummy=thisMatrix%get(4,2)
          IF(dummy /= -1051._SRK) THEN
            WRITE(*,*) 'CALL sparse%get(...) FAILED!' !column one check
            STOP 666
          ENDIF
          dummy=thisMatrix%get(-1,2)
          IF(dummy/=-1051._SRK) THEN
            WRITE(*,*) 'CALL sparse%get(...) FAILED!' !column one check
            STOP 666
          ENDIF
          dummy=thisMatrix%get(2,-1)
          IF(dummy/=-1051._SRK) THEN
            WRITE(*,*) 'CALL sparse%get(...) FAILED!' !column one check
            STOP 666
          ENDIF
      ENDSELECT
      !test get with uninit, make sure no crash.
      CALL thisMatrix%clear()
      SELECTTYPE(thisMatrix)
        TYPE IS(SparseMatrixType)      
          dummy=thisMatrix%get(1,1)
          IF(dummy /= 0.0_SRK) THEN
            WRITE(*,*) 'CALL sparse%get(...) FAILED!' !column one check
            STOP 666
          ENDIF
      ENDSELECT
      CALL thisMatrix%clear()
      WRITE(*,*) '  Passed: CALL sparse%get(...)'
      
      
      DEALLOCATE(thisMatrix)
!
!Test for dense square matrices        
      ALLOCATE(DenseSquareMatrixType :: thisMatrix)
      SELECTTYPE(thisMatrix)
        TYPE IS(DenseSquareMatrixType)
          !test clear
          !make matrix w/out using untested init
          thisMatrix%isInit=.TRUE.
          thisMatrix%n=10
          thisMatrix%isSymmetric=.TRUE.
          ALLOCATE(thisMatrix%a(10,10))
      ENDSELECT
      CALL thisMatrix%clear()
      SELECTTYPE(thisMatrix)
        TYPE IS(DenseSquareMatrixType)
          IF(((thisMatrix%isInit).OR.(thisMatrix%n /= 0)) &
              .OR.((thisMatrix%isSymmetric) &
              .OR.(ALLOCATED(thisMatrix%a)))) THEN
            WRITE(*,*) 'CALL densesquare%clear() FAILED!'
            STOP 666
          ENDIF
          WRITE(*,*) '  Passed: CALL densesquare%clear()'
      ENDSELECT
      !check init
      !first check intended init paths
      eMatrixType => NULL()
      CALL thisMatrix%init(10,0) !n=10, not symmetric
      eMatrixType => e
      SELECTTYPE(thisMatrix)
        TYPE IS(DenseSquareMatrixType)
          IF(((.NOT. thisMatrix%isInit).OR.(thisMatrix%n /= 10)) &
              .OR.(thisMatrix%isSymmetric)) THEN
            WRITE(*,*) 'CALL densesquare%init(...) FAILED!'
            STOP 666
          ENDIF
          IF((SIZE(thisMatrix%a,1) /= 10) &
            .OR. (SIZE(thisMatrix%a,2) /= 10)) THEN
            WRITE(*,*) 'CALL densesquare%init(...) FAILED!'
            STOP 666
          ENDIF
      ENDSELECT  
      CALL thisMatrix%clear()
      CALL thisMatrix%init(10,10) !n=10, symmetric
      SELECTTYPE(thisMatrix)
        TYPE IS(DenseSquareMatrixType)
          IF(((.NOT. thisMatrix%isInit).OR.(thisMatrix%n /= 10)) &
              .OR.(.NOT. thisMatrix%isSymmetric)) THEN
            WRITE(*,*) 'CALL densesquare%init(...) FAILED!'
            STOP 666
          ENDIF
      ENDSELECT
      !test w/out 2nd param being provided
      CALL thisMatrix%clear()
      CALL thisMatrix%init(10) !expect exception
      IF(thisMatrix%isInit) THEN 
        WRITE(*,*) 'CALL densesquare%init(...) FAILED!'
        STOP 666
      ENDIF
      CALL thisMatrix%clear()
      !test with double init (isInit==true on 2nd try)
      CALL thisMatrix%init(10,10) !n=10, symmetric
      SELECTTYPE(thisMatrix)
        TYPE IS(DenseSquareMatrixType); thisMatrix%isSymmetric=.FALSE.
      ENDSELECT
      CALL thisMatrix%init(10,10) !n=10, symmetric
      SELECTTYPE(thisMatrix)
        TYPE IS(DenseSquareMatrixType)
          IF(thisMatrix%isSymmetric) THEN
            WRITE(*,*) 'CALL densesquare%init(...) FAILED!'
            STOP 666
          ENDIF
      ENDSELECT
      CALL thisMatrix%clear()
      !test with n<1
      CALL thisMatrix%init(-1,100) !expect exception
      IF(thisMatrix%isInit) THEN
        WRITE(*,*) 'CALL densesquare%init(...) FAILED!'
        STOP 666
      ENDIF
      CALL thisMatrix%clear()
      !test with n<1 and no second parameter
      CALL thisMatrix%init(-1) !expect exception
      IF(thisMatrix%isInit) THEN
        WRITE(*,*) 'CALL densesquare%init(...) FAILED!'
        STOP 666
      ENDIF
      CALL thisMatrix%clear()
      WRITE(*,*) '  Passed: CALL densesquare%init(...)'
      !check set
      !test normal use case (symmetric and nonsymmetric)
      !want to build:
      ![1 2]
      ![2 3]
      CALL thisMatrix%clear()
      CALL thisMatrix%init(2,1)  !symmetric
      CALL thisMatrix%set(1,1,1._SRK)
      CALL thisMatrix%set(1,2,2._SRK)
      CALL thisMatrix%set(2,2,3._SRK)
      SELECTTYPE(thisMatrix)
        TYPE IS(DenseSquareMatrixType)
          IF(((thisMatrix%a(1,1)/=1._SRK) &
              .OR.(thisMatrix%a(1,2)/=2._SRK)) &
              .OR.((thisMatrix%a(2,1)/=2._SRK) &
              .OR.(thisMatrix%a(2,2)/=3._SRK)))THEN
            WRITE(*,*) 'CALL densesquare%set(...) FAILED!'
            STOP 666
          ENDIF
      ENDSELECT
      
      !Test BLAS_matvec
      x=1.0_SRK
      y=1.0_SRK
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=x,Y=y)
      IF(ANY(.NOT.(y .APPROXEQ. (/4._SRK,6._SRK,1._SRK/)))) THEN
        WRITE(*,*) "CALL BLAS_matvec(THISMATRIX=thisMatrix,X=x,Y=y) -densesq FAILED!"
        STOP 666
      ENDIF
      y=1.0_SRK
      CALL BLAS_matvec(THISMATRIX=thisMatrix,trans='t',X=x,Y=y)
      IF(ANY(.NOT.(y .APPROXEQ. (/4._SRK,6._SRK,1._SRK/)))) THEN
        WRITE(*,*) "CALL BLAS_matvec(THISMATRIX=thisMatrix,X=x,Y=y) -densesq FAILED!"
        STOP 666
      ENDIF
      y=1.0_SRK
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=x,BETA=2.0_SRK,Y=y)
      IF(ANY(.NOT.(y .APPROXEQ. (/5._SRK,7._SRK,1._SRK/)))) THEN
        WRITE(*,*) "CALL BLAS_matvec(THISMATRIX=thisMatrix,X=x,BETA=2.0_SRK,Y=y) -densesq FAILED!"
        STOP 666
      ENDIF
      y=1.0_SRK
      CALL BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=x,Y=y)
      IF(ANY(.NOT.(y .APPROXEQ. (/7._SRK,11._SRK,1._SRK/)))) THEN
        WRITE(*,*) "CALL BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=x,Y=y) -densesq FAILED!"
        STOP 666
      ENDIF
      y=1.0_SRK
      CALL BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=x,BETA=2.0_SRK,Y=y)
      IF(ANY(.NOT.(y .APPROXEQ. (/8._SRK,12._SRK,1._SRK/)))) THEN
        WRITE(*,*) "CALL BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=x,BETA=2.0_SRK,Y=y) -densesq FAILED!"
        STOP 666
      ENDIF
      CALL thisMatrix%clear()
      y=2._SRK
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=x,BETA=2.0_SRK,Y=y) !Error check uninit
      IF(ANY(.NOT.(y .APPROXEQ. (/2._SRK,2._SRK,2._SRK/)))) THEN
        WRITE(*,*) "CALL BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=x,BETA=2.0_SRK,Y=y) -densesq FAILED!"
        STOP 666
      ENDIF
      WRITE(*,*) '  Passed: CALL BLAS_matvec(...) densesq-matrix'
      
      CALL testMatrixMultSquare()
      
      CALL thisMatrix%init(2,0)  !nonsymmetric
      CALL thisMatrix%set(1,1,1._SRK)
      CALL thisMatrix%set(1,2,2._SRK)
      CALL thisMatrix%set(2,1,2._SRK)
      CALL thisMatrix%set(2,2,3._SRK)
      SELECTTYPE(thisMatrix)
        TYPE IS(DenseSquareMatrixType)
          IF(((thisMatrix%a(1,1)/=1._SRK) &
              .OR.(thisMatrix%a(1,2)/=2._SRK)) &
              .OR.((thisMatrix%a(2,1)/=2._SRK) &
              .OR.(thisMatrix%a(2,2)/=3._SRK)))THEN
            WRITE(*,*) 'CALL densesquare%set(...) FAILED!'
            STOP 666
          ENDIF
      ENDSELECT    
      !check matrix that hasnt been init, i,j out of bounds
      CALL thisMatrix%clear()
      CALL thisMatrix%set(1,1,1._SRK) 
      CALL thisMatrix%clear()
      CALL thisMatrix%init(2,0)
      CALL thisMatrix%set(-1,1,1._SRK)
      CALL thisMatrix%set(1,-1,1._SRK)
      CALL thisMatrix%set(5,1,1._SRK)
      CALL thisMatrix%set(1,5,1._SRK)
      !no crash? good
      CALL thisMatrix%clear()
      WRITE(*,*) '  Passed: CALL densesquare%set(...)'
      DEALLOCATE(thisMatrix)
!
!Test for tri-diagonal matrices
      ALLOCATE(TriDiagMatrixType :: thisMatrix)
      SELECTTYPE(thisMatrix)
        TYPE IS(TriDiagMatrixType)
          !test clear
          !make matrix w/out using untested init
          thisMatrix%isInit=.TRUE.
          thisMatrix%n=10
          thisMatrix%isSymmetric=.TRUE.
          ALLOCATE(thisMatrix%a(10,10))
      ENDSELECT
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=x,Y=y) !Error check unsupported type
      IF(ANY(.NOT.(y .APPROXEQ. (/2._SRK,2._SRK,2._SRK/)))) THEN
        WRITE(*,*) "CALL BLAS_matvec(...) -tridiag FAILED!"
        STOP 666
      ENDIF
      
      CALL thisMatrix%clear()
      SELECTTYPE(thisMatrix)
        TYPE IS(TriDiagMatrixType)
          IF(((thisMatrix%isInit).OR.(thisMatrix%n /= 0)) &
              .OR.((thisMatrix%isSymmetric) &
              .OR.(ALLOCATED(thisMatrix%a)))) THEN
            WRITE(*,*) 'CALL tridiag%clear() FAILED!'
            STOP 666
          ENDIF
          WRITE(*,*) '  Passed: CALL tridiag%clear()'
      ENDSELECT
      !check init
      !first check intended init paths
      CALL thisMatrix%clear()
      eMatrixType => NULL()
      CALL thisMatrix%init(10,0) !n=10, not symmetric
      eMatrixType => e
      SELECTTYPE(thisMatrix)
        TYPE IS(TriDiagMatrixType)
          IF(((.NOT. thisMatrix%isInit).OR.(thisMatrix%n /= 10)) &
              .OR.(thisMatrix%isSymmetric)) THEN
            WRITE(*,*) 'CALL tridiag%init(...) FAILED!'
            STOP 666
          ENDIF
          IF((SIZE(thisMatrix%a,1) /= 3) &
            .OR. (SIZE(thisMatrix%a,2) /= 10)) THEN
            WRITE(*,*) 'CALL tridiag%init(...) FAILED!'
            STOP 666
          ENDIF
      ENDSELECT
      CALL thisMatrix%clear()
      CALL thisMatrix%init(10,10) !n=10, symmetric
      SELECTTYPE(thisMatrix)
        TYPE IS(TriDiagMatrixType)
          IF(((.NOT. thisMatrix%isInit).OR.(thisMatrix%n /= 10)) &
              .OR.(.NOT. thisMatrix%isSymmetric)) THEN
            WRITE(*,*) 'CALL tridiag%init(...) FAILED!'
            STOP 666
          ENDIF
      ENDSELECT    
      !test w/out 2nd param being provided
      CALL thisMatrix%clear()
      CALL thisMatrix%init(10) !expect exception
      IF(thisMatrix%isInit) THEN 
        WRITE(*,*) 'CALL tridiag%init(...) FAILED!'
        STOP 666
      ENDIF
      CALL thisMatrix%clear()
      !test with double init (isInit==true on 2nd try)
      CALL thisMatrix%init(10,10) !n=10, symmetric
      SELECTTYPE(thisMatrix)
        TYPE IS(TriDiagMatrixType); thisMatrix%isSymmetric=.FALSE.
      ENDSELECT
      CALL thisMatrix%init(10,10) !n=10, symmetric
      SELECTTYPE(thisMatrix)
        TYPE IS(TriDiagMatrixType)
          IF(thisMatrix%isSymmetric) THEN
            WRITE(*,*) 'CALL tridiag%init(...) FAILED!'
            STOP 666
          ENDIF
      ENDSELECT
      CALL thisMatrix%clear()
      !test with n<1
      CALL thisMatrix%init(-1,100) !expect exception
      IF(thisMatrix%isInit) THEN
        WRITE(*,*) 'CALL tridiag%init(...) FAILED!'
        STOP 666
      ENDIF
      CALL thisMatrix%clear()
      !test with n<1 and no second parameter
      CALL thisMatrix%init(-1) !expect exception
      IF(thisMatrix%isInit) THEN
        WRITE(*,*) 'CALL tridiag%init(...) FAILED!'
        STOP 666
      ENDIF
      CALL thisMatrix%clear()
      WRITE(*,*) '  Passed: CALL tridiag%init(...)'
      !check set
      !test normal use case (symmetric and nonsymmetric)
      !want to build:
      ![1 4 0]
      ![4 2 5]
      ![0 5 3]
      CALL thisMatrix%clear()
      CALL thisMatrix%init(3,1)  !symmetric
      CALL thisMatrix%set(1,1,1._SRK)
      CALL thisMatrix%set(1,2,4._SRK)
      CALL thisMatrix%set(2,2,2._SRK)
      CALL thisMatrix%set(2,3,5._SRK)
      CALL thisMatrix%set(3,3,3._SRK)
      SELECTTYPE(thisMatrix)
        TYPE IS(TriDiagMatrixType)
          DO i=1,3
            IF(thisMatrix%a(2,i) /= i) THEN !check diagonal
              WRITE(*,*) 'CALL tridiag%set(...) FAILED!'
              STOP 666
            ENDIF
          ENDDO
          IF(thisMatrix%a(1,1) /= 0) THEN !check sub-diag 1st entry
            WRITE(*,*) 'CALL tridiag%set(...) FAILED!'
            STOP 666
          ENDIF
          DO i=2,3
            IF(thisMatrix%a(1,i) /= i+2) THEN !check sub-diag
              WRITE(*,*) 'CALL tridiag%set(...) FAILED!'
              STOP 666
            ENDIF
          ENDDO
          IF(thisMatrix%a(3,3) /= 0) THEN !check super-diag last entry
            WRITE(*,*) 'CALL tridiag%set(...) FAILED!'
            STOP 666
          ENDIF
          DO i=1,2
            IF(thisMatrix%a(3,i) /= i+3) THEN !check super-diag
              WRITE(*,*) 'CALL tridiag%set(...) FAILED!'
              STOP 666
            ENDIF
          ENDDO
      ENDSELECT
      CALL thisMatrix%clear()
      CALL thisMatrix%init(3,0)  !nonsymmetric
      CALL thisMatrix%set(1,1,1._SRK)
      CALL thisMatrix%set(1,2,4._SRK)
      CALL thisMatrix%set(2,1,4._SRK)
      CALL thisMatrix%set(2,2,2._SRK)
      CALL thisMatrix%set(2,3,5._SRK)
      CALL thisMatrix%set(3,2,5._SRK)
      CALL thisMatrix%set(3,3,3._SRK)
      SELECTTYPE(thisMatrix)
        TYPE IS(TriDiagMatrixType)
          DO i=1,3
            IF(thisMatrix%a(2,i) /= i) THEN !check diagonal
              WRITE(*,*) 'CALL tridiag%set(...) FAILED!'
              STOP 666
            ENDIF
          ENDDO
          IF(thisMatrix%a(1,1) /= 0) THEN !check sub-diag 1st entry
            WRITE(*,*) 'CALL tridiag%set(...) FAILED!'
            STOP 666
          ENDIF
          DO i=2,3
            IF(thisMatrix%a(1,i) /= i+2) THEN !check sub-diag
              WRITE(*,*) 'CALL tridiag%set(...) FAILED!'
              STOP 666
            ENDIF
          ENDDO
          IF(thisMatrix%a(3,3) /= 0) THEN !check super-diag last entry
            WRITE(*,*) 'CALL tridiag%set(...) FAILED!'
            STOP 666
          ENDIF
          DO i=1,2
            IF(thisMatrix%a(3,i) /= i+3) THEN !check super-diag
              WRITE(*,*) 'CALL tridiag%set(...) FAILED!'
              STOP 666
            ENDIF
          ENDDO
      ENDSELECT
      !check matrix that hasnt been init, i,j out of bounds
      CALL thisMatrix%clear()
      CALL thisMatrix%set(1,1,1._SRK) 
      CALL thisMatrix%clear()
      CALL thisMatrix%init(4,0)
      CALL thisMatrix%set(-1,1,1._SRK)
      CALL thisMatrix%set(1,-1,1._SRK)
      CALL thisMatrix%set(5,1,1._SRK)
      CALL thisMatrix%set(1,5,1._SRK)
      !no crash? good
      CALL thisMatrix%clear()
      WRITE(*,*) '  Passed: CALL tridiag%set(...)'
      DEALLOCATE(thisMatrix)
!
!Test for dense rectangular matrices
      ALLOCATE(DenseRectMatrixType :: thisMatrix)
      SELECTTYPE(thisMatrix)
        TYPE IS(DenseRectMatrixType)
          !test clear
          !make matrix w/out using untested init
          thisMatrix%isInit=.TRUE.
          thisMatrix%n=10
          thisMatrix%m=15
          ALLOCATE(thisMatrix%a(10,15))
      ENDSELECT
      CALL thisMatrix%clear()
      SELECTTYPE(thisMatrix)
        TYPE IS(DenseRectMatrixType)
          IF(((thisMatrix%isInit).OR.(thisMatrix%n /= 0)) &
              .OR.((thisMatrix%m /= 0) &
              .OR.(ALLOCATED(thisMatrix%a)))) THEN
            WRITE(*,*) 'CALL denserect%clear() FAILED!'
            STOP 666
          ENDIF
          WRITE(*,*) '  Passed: CALL denserect%clear()'
      ENDSELECT
      !check init
      !first check intended init paths
      CALL thisMatrix%clear()
      CALL thisMatrix%init(10) !Error check
      eMatrixType => NULL()
      CALL thisMatrix%init(10,15)
      eMatrixType => e
      SELECTTYPE(thisMatrix)
        TYPE IS(DenseRectMatrixType)
          IF(((.NOT. thisMatrix%isInit).OR.(thisMatrix%n /= 10)) &
              .OR.((thisMatrix%m /= 15))) THEN
            WRITE(*,*) 'CALL denserect%init(...) FAILED!'
            STOP 666
          ENDIF
          IF((SIZE(thisMatrix%a,1) /= 10) &
            .OR. (SIZE(thisMatrix%a,2) /= 15)) THEN
            WRITE(*,*) 'CALL denserect%init(...) FAILED!'
            STOP 666
          ENDIF
      ENDSELECT
      CALL thisMatrix%clear()
      !test with double init (isInit==true on 2nd try)
      CALL thisMatrix%init(10,15)
      SELECTTYPE(thisMatrix)
        TYPE IS(DenseRectMatrixType); thisMatrix%m=1
      ENDSELECT
      CALL thisMatrix%init(10,15)
      SELECTTYPE(thisMatrix)
        TYPE IS(DenseRectMatrixType)
          IF(thisMatrix%m /= 1) THEN
            WRITE(*,*) 'CALL denserect%init(...) FAILED!'
            STOP 666
          ENDIF
      ENDSELECT
      CALL thisMatrix%clear()
      !test with n<1
      CALL thisMatrix%init(-1,10) !expect exception
      IF(thisMatrix%isInit) THEN
        WRITE(*,*) 'CALL denserect%init(...) FAILED!'
        STOP 666
      ENDIF
      CALL thisMatrix%clear()
      !test with m<1
      CALL thisMatrix%init(10,-1) !expect exception
      IF(thisMatrix%isInit) THEN
        WRITE(*,*) 'CALL denserect%init(...) FAILED!'
        STOP 666
      ENDIF
      CALL thisMatrix%clear()
      WRITE(*,*) '  Passed: CALL denserect%init(...)'
            
      !check set
      !test normal use case (symmetric and nonsymmetric)
      !want to build:
      ![1 2 3]
      ![4 5 6]
      CALL thisMatrix%clear()
      CALL thisMatrix%init(2,3)
      CALL thisMatrix%set(1,1,1._SRK)
      CALL thisMatrix%set(1,2,2._SRK)
      CALL thisMatrix%set(1,3,3._SRK)
      CALL thisMatrix%set(2,1,4._SRK)
      CALL thisMatrix%set(2,2,5._SRK)
      CALL thisMatrix%set(2,3,6._SRK)
      SELECTTYPE(thisMatrix)
        TYPE IS(DenseRectMatrixType)
          DO i=1,3
            IF((thisMatrix%a(1,i)/= i).OR.(thisMatrix%a(2,i)/= 3+i)) THEN
              WRITE(*,*) 'CALL denserect%set(...) FAILED!'
              STOP 666
            ENDIF
          ENDDO
      ENDSELECT
      
      !Test BLAS_matvec
      x=1.0_SRK
      y=1.0_SRK
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=x,Y=y)
      IF(ANY(.NOT.(y .APPROXEQ. (/7._SRK,16._SRK,1._SRK/)))) THEN
        WRITE(*,*) "CALL BLAS_matvec(THISMATRIX=thisMatrix,X=x,Y=y) -denserect FAILED!"
        STOP 666
      ENDIF
      y=1.0_SRK
      CALL BLAS_matvec(THISMATRIX=thisMatrix,trans='t',X=x,Y=y)
      IF(ANY(.NOT.(y .APPROXEQ. (/6._SRK,8._SRK,10._SRK/)))) THEN
        WRITE(*,*) "CALL BLAS_matvec(THISMATRIX=thisMatrix,X=x,Y=y) -denserect FAILED!"
        STOP 666
      ENDIF
      y=1.0_SRK
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=x,BETA=2.0_SRK,Y=y)
      IF(ANY(.NOT.(y .APPROXEQ. (/8._SRK,17._SRK,1._SRK/)))) THEN
        WRITE(*,*) "CALL BLAS_matvec(THISMATRIX=thisMatrix,X=x,BETA=2.0_SRK,Y=y) -denserect FAILED!"
        STOP 666
      ENDIF
      y=1.0_SRK
      CALL BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=x,Y=y)
      IF(ANY(.NOT.(y .APPROXEQ. (/13._SRK,31._SRK,1._SRK/)))) THEN
        WRITE(*,*) "CALL BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=x,Y=y) -denserect FAILED!"
        STOP 666
      ENDIF
      y=1.0_SRK
      CALL BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=x,BETA=2.0_SRK,Y=y)
      IF(ANY(.NOT.(y .APPROXEQ. (/14._SRK,32._SRK,1._SRK/)))) THEN
        WRITE(*,*) "CALL BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=x,BETA=2.0_SRK,Y=y) -denserect FAILED!"
        STOP 666
      ENDIF
      WRITE(*,*) '  Passed: CALL BLAS_matvec(...) denserect-matrix'
      
      CALL testMatrixMultRect()
      
      !check matrix that hasnt been init, i,j out of bounds
      CALL thisMatrix%clear()
      CALL thisMatrix%set(1,1,1._SRK) 
      CALL thisMatrix%clear()
      CALL thisMatrix%init(2,3)
      CALL thisMatrix%set(-1,1,1._SRK)
      CALL thisMatrix%set(1,-1,1._SRK)
      CALL thisMatrix%set(5,1,1._SRK)
      CALL thisMatrix%set(1,5,1._SRK)
      !no crash? good
      CALL thisMatrix%clear()
      WRITE(*,*) '  Passed: CALL denserect%set(...)'
    ENDSUBROUTINE testMatrixTypes
!
!-------------------------------------------------------------------------------
    SUBROUTINE testMatrixMultSquare()
      CLASS(MatrixType),ALLOCATABLE :: thisMtrx
      CLASS(MatrixType),ALLOCATABLE :: bmat
      CLASS(MatrixType),ALLOCATABLE :: cmat
      INTEGER(SIK) :: i,j
      REAL(SRK) :: ALPHA,BETA
      
      !Test BLAS_matmult
      !
      !1.) A: Square  B: Square  C: Square     ----------------------------
      !
      ALLOCATE(DenseSquareMatrixType :: thisMtrx)
      CALL thisMtrx%clear()
      CALL thisMtrx%init(3,1)  !symmetric
      CALL thisMtrx%init(3,1)  
      CALL thisMtrx%set(1,1,1._SRK)
      CALL thisMtrx%set(1,2,1._SRK)
      CALL thisMtrx%set(1,3,1._SRK)
      CALL thisMtrx%set(2,2,1._SRK)
      CALL thisMtrx%set(2,3,1._SRK)
      CALL thisMtrx%set(3,3,1._SRK)
      
      ALLOCATE(DenseSquareMatrixType :: bmat)
      ALLOCATE(DenseSquareMatrixType :: cmat)
      CALL bmat%init(3,1)  !symmetric
      CALL bmat%set(1,1,1._SRK)
      CALL bmat%set(1,2,0._SRK)
      CALL bmat%set(1,3,0._SRK)
      CALL bmat%set(2,2,1._SRK)
      CALL bmat%set(2,3,0._SRK)
      CALL bmat%set(3,3,1._SRK)
      CALL cmat%init(3,1)  !symmetric
      CALL cmat%set(1,1,3._SRK)
      CALL cmat%set(1,2,3._SRK)
      CALL cmat%set(1,3,3._SRK)
      CALL cmat%set(2,2,3._SRK)
      CALL cmat%set(2,3,3._SRK)
      CALL cmat%set(3,3,3._SRK)
      
      BETA=1.0_SRK
      ALPHA=1.0_SRK
      ! A: Square  B: Square  C: Square  alpha & beta
      CALL BLAS_matmult(A=thisMtrx,B=bmat,C=cmat,alpha=ALPHA,beta=BETA,transA='n')
      SELECTTYPE(cmat)
        TYPE IS(DenseSquareMatrixType)
          DO i=1,cmat%n
            DO j=1,cmat%n
              IF(.NOT.(cmat%a(i,j) .APPROXEQ. 4._SRK) ) THEN
                WRITE(*,*) "CALL BLAS_matmult(A=thisMtrx,B=bmat,C=cmat,alpha=ALPHA,beta=BETA) -DS DS DS FAILED!"
                STOP 666
              ENDIF
            ENDDO
          ENDDO
        CLASS DEFAULT
          WRITE(*,*) "CALL BLAS_matmult(A=thisMtrx,B=bmat,C=cmat,alpha=ALPHA,beta=BETA) FAILED!"
          WRITE(*,*) "    cmat returned as incorrect matrix type"
          STOP 666
      ENDSELECT
      ! A: Square  B: Square  C: Square  alpha, no beta
      CALL BLAS_matmult(A=thisMtrx,B=bmat,C=cmat,alpha=ALPHA)
      SELECTTYPE(cmat)
        TYPE IS(DenseSquareMatrixType)
          DO i=1,cmat%n
            DO j=1,cmat%n
              IF(.NOT.(cmat%a(i,j) .APPROXEQ. 5._SRK) ) THEN
                WRITE(*,*) "CALL BLAS_matmult(A=thisMtrx,B=bmat,C=cmat,alpha=ALPHA) -DS DS DS FAILED!"
                STOP 666
              ENDIF
            ENDDO
          ENDDO
        CLASS DEFAULT
          WRITE(*,*) "CALL BLAS_matmult(A=thisMtrx,B=bmat,C=cmat,alpha=ALPHA) FAILED!"
          WRITE(*,*) "    cmat returned as incorrect matrix type"
          STOP 666
      ENDSELECT
      ! A: Square  B: Square  C: Square no alpha, beta
      CALL BLAS_matmult(A=thisMtrx,B=bmat,C=cmat,beta=BETA)
      SELECTTYPE(cmat)
        TYPE IS(DenseSquareMatrixType)
          DO i=1,cmat%n
            DO j=1,cmat%n
              IF(.NOT.(cmat%a(i,j) .APPROXEQ. 6._SRK) ) THEN
                WRITE(*,*) "CALL BLAS_matmult(A=thisMtrx,B=bmat,C=cmat,beta=BETA) -DS DS DS FAILED!"
                STOP 666
              ENDIF
            ENDDO
          ENDDO
        CLASS DEFAULT
          WRITE(*,*) "CALL BLAS_matmult(A=thisMtrx,B=bmat,C=cmat,beta=BETA) FAILED!"
          WRITE(*,*) "    cmat returned as incorrect matrix type"
          STOP 666
      ENDSELECT
      ! A: Square  B: Square  C: Square no alpha, nobeta
      CALL BLAS_matmult(A=thisMtrx,B=bmat,C=cmat)
      SELECTTYPE(cmat)
        TYPE IS(DenseSquareMatrixType)
          DO i=1,cmat%n
            DO j=1,cmat%n
              IF(.NOT.(cmat%a(i,j) .APPROXEQ. 7._SRK) ) THEN
                WRITE(*,*) "CALL BLAS_matmult(A=thisMtrx,B=bmat,C=cmat) -DS DS DS FAILED!"
                STOP 666
              ENDIF
            ENDDO
          ENDDO
        CLASS DEFAULT
          WRITE(*,*) "CALL BLAS_matmult(A=thisMtrx,B=bmat,C=cmat) FAILED!"
          WRITE(*,*) "    cmat returned as incorrect matrix type"
          STOP 666
      ENDSELECT
      WRITE(*,*) '  Passed: CALL BLAS_matmult(...) A=DS B=DS C=DS'
      !
      !2.) A: Square  B: Square  C: Rect    ----------------------------
      !
      CALL cmat%clear()
      DEALLOCATE(cmat)
      ALLOCATE(DenseRectMatrixType :: cmat)
      CALL cmat%init(3,3)  
      CALL cmat%set(1,1,3._SRK)
      CALL cmat%set(1,2,3._SRK)
      CALL cmat%set(1,3,3._SRK)
      CALL cmat%set(2,1,3._SRK)
      CALL cmat%set(2,2,3._SRK)
      CALL cmat%set(2,3,3._SRK)
      CALL cmat%set(3,1,3._SRK)
      CALL cmat%set(3,2,3._SRK)
      CALL cmat%set(3,3,3._SRK)
      
      ! A: Square  B: Square  C: Rect  alpha & beta
      CALL BLAS_matmult(A=thisMtrx,B=bmat,C=cmat,alpha=ALPHA,beta=BETA)
      SELECTTYPE(cmat)
        TYPE IS(DenseRectMatrixType)
          DO i=1,cmat%n
            DO j=1,cmat%n
              IF(.NOT.(cmat%a(i,j) .APPROXEQ. 4._SRK) ) THEN
                WRITE(*,*) "CALL BLAS_matmult(A=thisMtrx,B=bmat,C=cmat,alpha=ALPHA,beta=BETA) -DS DS DR FAILED!"
                STOP 666
              ENDIF
            ENDDO
          ENDDO
        CLASS DEFAULT
          WRITE(*,*) "CALL BLAS_matmult(A=thisMtrx,B=bmat,C=cmat,alpha=ALPHA,beta=BETA) FAILED!"
          WRITE(*,*) "    cmat returned as incorrect matrix type"
          STOP 666
      ENDSELECT
      ! A: Square  B: Square  C: Rect  alpha, no beta
      CALL BLAS_matmult(A=thisMtrx,B=bmat,C=cmat,alpha=ALPHA)
      SELECTTYPE(cmat)
        TYPE IS(DenseRectMatrixType)
          DO i=1,cmat%n
            DO j=1,cmat%n
              IF(.NOT.(cmat%a(i,j) .APPROXEQ. 5._SRK) ) THEN
                WRITE(*,*) "CALL BLAS_matmult(A=thisMtrx,B=bmat,C=cmat,alpha=ALPHA) -DS DS DR FAILED!"
                STOP 666
              ENDIF
            ENDDO
          ENDDO
        CLASS DEFAULT
          WRITE(*,*) "CALL BLAS_matmult(A=thisMtrx,B=bmat,C=cmat,alpha=ALPHA) FAILED!"
          WRITE(*,*) "    cmat returned as incorrect matrix type"
          STOP 666
      ENDSELECT
      ! A: Square  B: Square  C: Rect no alpha, beta
      CALL BLAS_matmult(A=thisMtrx,B=bmat,C=cmat,beta=BETA)
      SELECTTYPE(cmat)
        TYPE IS(DenseRectMatrixType)
          DO i=1,cmat%n
            DO j=1,cmat%n
              IF(.NOT.(cmat%a(i,j) .APPROXEQ. 6._SRK) ) THEN
                WRITE(*,*) "CALL BLAS_matmult(A=thisMtrx,B=bmat,C=cmat,beta=BETA) -DS DS DR FAILED!"
                STOP 666
              ENDIF
            ENDDO
          ENDDO
        CLASS DEFAULT
          WRITE(*,*) "CALL BLAS_matmult(A=thisMtrx,B=bmat,C=cmat,beta=BETA) FAILED!"
          WRITE(*,*) "    cmat returned as incorrect matrix type"
          STOP 666
      ENDSELECT
      ! A: Square  B: Square  C: Rect no alpha, nobeta
      CALL BLAS_matmult(A=thisMtrx,B=bmat,C=cmat)
      SELECTTYPE(cmat)
        TYPE IS(DenseRectMatrixType)
          DO i=1,cmat%n
            DO j=1,cmat%n
              IF(.NOT.(cmat%a(i,j) .APPROXEQ. 7._SRK) ) THEN
                WRITE(*,*) "CALL BLAS_matmult(A=thisMtrx,B=bmat,C=cmat) -DS DS DR FAILED!"
                STOP 666
              ENDIF
            ENDDO
          ENDDO
        CLASS DEFAULT
          WRITE(*,*) "CALL BLAS_matmult(A=thisMtrx,B=bmat,C=cmat) FAILED!"
          WRITE(*,*) "    cmat returned as incorrect matrix type"
          STOP 666
      ENDSELECT
      WRITE(*,*) '  Passed: CALL BLAS_matmult(...) A=DS B=DS C=DR'
    
      !
      !3.) A: Square  B: Rect  C: Square    ----------------------------
      !
      CALL cmat%clear()
      DEALLOCATE(cmat)
      ALLOCATE(DenseSquareMatrixType :: cmat)
      CALL cmat%init(3,1)  !symmetric
      CALL cmat%set(1,1,3._SRK)
      CALL cmat%set(1,2,3._SRK)
      CALL cmat%set(1,3,3._SRK)
      CALL cmat%set(2,2,3._SRK)
      CALL cmat%set(2,3,3._SRK)
      CALL cmat%set(3,3,3._SRK)
      CALL bmat%clear()
      DEALLOCATE(bmat)
      ALLOCATE(DenseRectMatrixType :: bmat)
      CALL bmat%init(3,3)  
      CALL bmat%set(1,1,1._SRK)
      CALL bmat%set(1,2,0._SRK)
      CALL bmat%set(1,3,0._SRK)
      CALL bmat%set(2,1,0._SRK)
      CALL bmat%set(2,2,1._SRK)
      CALL bmat%set(2,3,0._SRK)
      CALL bmat%set(3,1,0._SRK)
      CALL bmat%set(3,2,0._SRK)
      CALL bmat%set(3,3,1._SRK)
      
      ! A: Square  B: Rect  C: Square  alpha & beta
      CALL BLAS_matmult(A=thisMtrx,B=bmat,C=cmat,alpha=ALPHA,beta=BETA)
      SELECTTYPE(cmat)
        TYPE IS(DenseSquareMatrixType)
          DO i=1,cmat%n
            DO j=1,cmat%n
              IF(.NOT.(cmat%a(i,j) .APPROXEQ. 4._SRK) ) THEN
                WRITE(*,*) "CALL BLAS_matmult(A=thisMtrx,B=bmat,C=cmat,alpha=ALPHA,beta=BETA) -DS DR DS FAILED!"
                STOP 666
              ENDIF
            ENDDO
          ENDDO
        CLASS DEFAULT
          WRITE(*,*) "CALL BLAS_matmult(A=thisMtrx,B=bmat,C=cmat,alpha=ALPHA,beta=BETA) FAILED!"
          WRITE(*,*) "    cmat returned as incorrect matrix type"
          STOP 666
      ENDSELECT
      ! A: Square  B: Rect  C: Square  alpha, no beta
      CALL BLAS_matmult(A=thisMtrx,B=bmat,C=cmat,alpha=ALPHA)
      SELECTTYPE(cmat)
        TYPE IS(DenseSquareMatrixType)
          DO i=1,cmat%n
            DO j=1,cmat%n
              IF(.NOT.(cmat%a(i,j) .APPROXEQ. 5._SRK) ) THEN
                WRITE(*,*) "CALL BLAS_matmult(A=thisMtrx,B=bmat,C=cmat,alpha=ALPHA) -DS DR DS FAILED!"
                STOP 666
              ENDIF
            ENDDO
          ENDDO
        CLASS DEFAULT
          WRITE(*,*) "CALL BLAS_matmult(A=thisMtrx,B=bmat,C=cmat,alpha=ALPHA) FAILED!"
          WRITE(*,*) "    cmat returned as incorrect matrix type"
          STOP 666
      ENDSELECT
      ! A: Square  B: Rect  C: Square no alpha, beta
      CALL BLAS_matmult(A=thisMtrx,B=bmat,C=cmat,beta=BETA)
      SELECTTYPE(cmat)
        TYPE IS(DenseSquareMatrixType)
          DO i=1,cmat%n
            DO j=1,cmat%n
              IF(.NOT.(cmat%a(i,j) .APPROXEQ. 6._SRK) ) THEN
                WRITE(*,*) "CALL BLAS_matmult(A=thisMtrx,B=bmat,C=cmat,beta=BETA) -DS DR DS FAILED!"
                STOP 666
              ENDIF
            ENDDO
          ENDDO
        CLASS DEFAULT
          WRITE(*,*) "CALL BLAS_matmult(A=thisMtrx,B=bmat,C=cmat,beta=BETA) FAILED!"
          WRITE(*,*) "    cmat returned as incorrect matrix type"
          STOP 666
      ENDSELECT
      ! A: Square  B: Rect  C: Square no alpha, nobeta
      CALL BLAS_matmult(A=thisMtrx,B=bmat,C=cmat)
      SELECTTYPE(cmat)
        TYPE IS(DenseSquareMatrixType)
          DO i=1,cmat%n
            DO j=1,cmat%n
              IF(.NOT.(cmat%a(i,j) .APPROXEQ. 7._SRK) ) THEN
                WRITE(*,*) "CALL BLAS_matmult(A=thisMtrx,B=bmat,C=cmat) -DS DR DS FAILED!"
                STOP 666
              ENDIF
            ENDDO
          ENDDO
        CLASS DEFAULT
          WRITE(*,*) "CALL BLAS_matmult(A=thisMtrx,B=bmat,C=cmat) FAILED!"
          WRITE(*,*) "    cmat returned as incorrect matrix type"
          STOP 666
      ENDSELECT
      WRITE(*,*) '  Passed: CALL BLAS_matmult(...) A=DS B=DR C=DS'
      !
      !4.) A: Square  B: Rect  C: Rect    ----------------------------
      !
      CALL cmat%clear()
      DEALLOCATE(cmat)
      ALLOCATE(DenseRectMatrixType :: cmat)
      CALL cmat%init(3,3)  
      CALL cmat%set(1,1,3._SRK)
      CALL cmat%set(1,2,3._SRK)
      CALL cmat%set(1,3,3._SRK)
      CALL cmat%set(2,1,3._SRK)
      CALL cmat%set(2,2,3._SRK)
      CALL cmat%set(2,3,3._SRK)
      CALL cmat%set(3,1,3._SRK)
      CALL cmat%set(3,2,3._SRK)
      CALL cmat%set(3,3,3._SRK)
      
      ! A: Square  B: Rect  C: Rect  alpha & beta
      CALL BLAS_matmult(A=thisMtrx,B=bmat,C=cmat,alpha=ALPHA,beta=BETA)
      SELECTTYPE(cmat)
        TYPE IS(DenseRectMatrixType)
          DO i=1,cmat%n
            DO j=1,cmat%n
              IF(.NOT.(cmat%a(i,j) .APPROXEQ. 4._SRK) ) THEN
                WRITE(*,*) "CALL BLAS_matmult(A=thisMtrx,B=bmat,C=cmat,alpha=ALPHA,beta=BETA) -DS DR DR FAILED!"
                STOP 666
              ENDIF
            ENDDO
          ENDDO
        CLASS DEFAULT
          WRITE(*,*) "CALL BLAS_matmult(A=thisMtrx,B=bmat,C=cmat,alpha=ALPHA,beta=BETA) FAILED!"
          WRITE(*,*) "    cmat returned as incorrect matrix type"
          STOP 666
      ENDSELECT
      ! A: Square  B: Rect  C: Rect  alpha, no beta
      CALL BLAS_matmult(A=thisMtrx,B=bmat,C=cmat,alpha=ALPHA)
      SELECTTYPE(cmat)
        TYPE IS(DenseRectMatrixType)
          DO i=1,cmat%n
            DO j=1,cmat%n
              IF(.NOT.(cmat%a(i,j) .APPROXEQ. 5._SRK) ) THEN
                WRITE(*,*) "CALL BLAS_matmult(A=thisMtrx,B=bmat,C=cmat,alpha=ALPHA) -DS DR DR FAILED!"
                STOP 666
              ENDIF
            ENDDO
          ENDDO
        CLASS DEFAULT
          WRITE(*,*) "CALL BLAS_matmult(A=thisMtrx,B=bmat,C=cmat,alpha=ALPHA) FAILED!"
          WRITE(*,*) "    cmat returned as incorrect matrix type"
          STOP 666
      ENDSELECT
      ! A: Square  B: Rect  C: Rect no alpha, beta
      CALL BLAS_matmult(A=thisMtrx,B=bmat,C=cmat,beta=BETA)
      SELECTTYPE(cmat)
        TYPE IS(DenseRectMatrixType)
          DO i=1,cmat%n
            DO j=1,cmat%n
              IF(.NOT.(cmat%a(i,j) .APPROXEQ. 6._SRK) ) THEN
                WRITE(*,*) "CALL BLAS_matmult(A=thisMtrx,B=bmat,C=cmat,beta=BETA) -DS DR DR FAILED!"
                STOP 666
              ENDIF
            ENDDO
          ENDDO
        CLASS DEFAULT
          WRITE(*,*) "CALL BLAS_matmult(A=thisMtrx,B=bmat,C=cmat,beta=BETA) FAILED!"
          WRITE(*,*) "    cmat returned as incorrect matrix type"
          STOP 666
      ENDSELECT
      ! A: Square  B: Rect  C: Rect no alpha, nobeta
      CALL BLAS_matmult(A=thisMtrx,B=bmat,C=cmat)
      SELECTTYPE(cmat)
        TYPE IS(DenseRectMatrixType)
          DO i=1,cmat%n
            DO j=1,cmat%n
              IF(.NOT.(cmat%a(i,j) .APPROXEQ. 7._SRK) ) THEN
                WRITE(*,*) "CALL BLAS_matmult(A=thisMtrx,B=bmat,C=cmat) -DS DR DR FAILED!"
                STOP 666
              ENDIF
            ENDDO
          ENDDO
        CLASS DEFAULT
          WRITE(*,*) "CALL BLAS_matmult(A=thisMtrx,B=bmat,C=cmat) FAILED!"
          WRITE(*,*) "    cmat returned as incorrect matrix type"
          STOP 666
      ENDSELECT
      WRITE(*,*) '  Passed: CALL BLAS_matmult(...) A=DS B=DR C=DR'
      CALL thisMtrx%clear()
      CALL bmat%clear()
      CALL cmat%clear()
      
    ENDSUBROUTINE  testMatrixMultSquare
    
!
!-------------------------------------------------------------------------------
    SUBROUTINE testMatrixMultRect()
      CLASS(MatrixType),ALLOCATABLE :: thisMtrx
      CLASS(MatrixType),ALLOCATABLE :: bmat
      CLASS(MatrixType),ALLOCATABLE :: cmat
      INTEGER(SIK) :: i,j
      REAL(SRK) :: ALPHA,BETA
    
      
      !Test BLAS_matmult
      !
      !1.) A: Rect  B: Square  C: Square     ----------------------------
      !
      ALLOCATE(DenseRectMatrixType :: thisMtrx)
      CALL thisMtrx%clear()
      CALL thisMtrx%init(3,3)  !symmetric
      CALL thisMtrx%set(1,1,1._SRK)
      CALL thisMtrx%set(1,2,1._SRK)
      CALL thisMtrx%set(1,3,1._SRK)
      CALL thisMtrx%set(2,1,1._SRK)
      CALL thisMtrx%set(2,2,1._SRK)
      CALL thisMtrx%set(2,3,1._SRK)
      CALL thisMtrx%set(3,1,1._SRK)
      CALL thisMtrx%set(3,2,1._SRK)
      CALL thisMtrx%set(3,3,1._SRK)
      
      ALLOCATE(DenseSquareMatrixType :: bmat)
      ALLOCATE(DenseSquareMatrixType :: cmat)
      CALL bmat%init(3,1)  !symmetric
      CALL bmat%set(1,1,1._SRK)
      CALL bmat%set(1,2,0._SRK)
      CALL bmat%set(1,3,0._SRK)
      CALL bmat%set(2,2,1._SRK)
      CALL bmat%set(2,3,0._SRK)
      CALL bmat%set(3,3,1._SRK)
      CALL cmat%init(3,1)  !symmetric
      CALL cmat%set(1,1,3._SRK)
      CALL cmat%set(1,2,3._SRK)
      CALL cmat%set(1,3,3._SRK)
      CALL cmat%set(2,2,3._SRK)
      CALL cmat%set(2,3,3._SRK)
      CALL cmat%set(3,3,3._SRK)
      
      BETA=1.0_SRK
      ALPHA=1.0_SRK
      ! A: Rect  B: Square  C: Square  alpha & beta
      CALL BLAS_matmult(A=thisMtrx,B=bmat,C=cmat,alpha=ALPHA,beta=BETA,transB='n')
      SELECTTYPE(cmat)
        TYPE IS(DenseSquareMatrixType)
          DO i=1,cmat%n
            DO j=1,cmat%n
              IF(.NOT.(cmat%a(i,j) .APPROXEQ. 4._SRK) ) THEN
                WRITE(*,*) "CALL BLAS_matmult(A=thisMtrx,B=bmat,C=cmat,alpha=ALPHA,beta=BETA) -DR DS DS FAILED!"
                STOP 666
              ENDIF
            ENDDO
          ENDDO
        CLASS DEFAULT
          WRITE(*,*) "CALL BLAS_matmult(A=thisMtrx,B=bmat,C=cmat,alpha=ALPHA,beta=BETA) FAILED!"
          WRITE(*,*) "    cmat returned as incorrect matrix type"
          STOP 666
      ENDSELECT
      ! A: Rect  B: Square  C: Square  alpha, no beta
      CALL BLAS_matmult(A=thisMtrx,B=bmat,C=cmat,alpha=ALPHA)
      SELECTTYPE(cmat)
        TYPE IS(DenseSquareMatrixType)
          DO i=1,cmat%n
            DO j=1,cmat%n
              IF(.NOT.(cmat%a(i,j) .APPROXEQ. 5._SRK) ) THEN
                WRITE(*,*) "CALL BLAS_matmult(A=thisMtrx,B=bmat,C=cmat,alpha=ALPHA) -DR DS DS FAILED!"
                STOP 666
              ENDIF
            ENDDO
          ENDDO
        CLASS DEFAULT
          WRITE(*,*) "CALL BLAS_matmult(A=thisMtrx,B=bmat,C=cmat,alpha=ALPHA) FAILED!"
          WRITE(*,*) "    cmat returned as incorrect matrix type"
          STOP 666
      ENDSELECT
      ! A: Rect  B: Square  C: Square no alpha, beta
      CALL BLAS_matmult(A=thisMtrx,B=bmat,C=cmat,beta=BETA)
      SELECTTYPE(cmat)
        TYPE IS(DenseSquareMatrixType)
          DO i=1,cmat%n
            DO j=1,cmat%n
              IF(.NOT.(cmat%a(i,j) .APPROXEQ. 6._SRK) ) THEN
                WRITE(*,*) "CALL BLAS_matmult(A=thisMtrx,B=bmat,C=cmat,beta=BETA) -DR DS DS FAILED!"
                STOP 666
              ENDIF
            ENDDO
          ENDDO
        CLASS DEFAULT
          WRITE(*,*) "CALL BLAS_matmult(A=thisMtrx,B=bmat,C=cmat,beta=BETA) FAILED!"
          WRITE(*,*) "    cmat returned as incorrect matrix type"
          STOP 666
      ENDSELECT
      ! A: Rect  B: Square  C: Square no alpha, nobeta
      CALL BLAS_matmult(A=thisMtrx,B=bmat,C=cmat)
      SELECTTYPE(cmat)
        TYPE IS(DenseSquareMatrixType)
          DO i=1,cmat%n
            DO j=1,cmat%n
              IF(.NOT.(cmat%a(i,j) .APPROXEQ. 7._SRK) ) THEN
                WRITE(*,*) "CALL BLAS_matmult(A=thisMtrx,B=bmat,C=cmat) -DR DS DS FAILED!"
                STOP 666
              ENDIF
            ENDDO
          ENDDO
        CLASS DEFAULT
          WRITE(*,*) "CALL BLAS_matmult(A=thisMtrx,B=bmat,C=cmat) FAILED!"
          WRITE(*,*) "    cmat returned as incorrect matrix type"
          STOP 666
      ENDSELECT
      WRITE(*,*) '  Passed: CALL BLAS_matmult(...) A=DR B=DS C=DS'
      !
      !2.) A: Rect  B: Square  C: Rect    ----------------------------
      !
      CALL cmat%clear()
      DEALLOCATE(cmat)
      ALLOCATE(DenseRectMatrixType :: cmat)
      CALL cmat%init(3,3)  
      CALL cmat%set(1,1,3._SRK)
      CALL cmat%set(1,2,3._SRK)
      CALL cmat%set(1,3,3._SRK)
      CALL cmat%set(2,1,3._SRK)
      CALL cmat%set(2,2,3._SRK)
      CALL cmat%set(2,3,3._SRK)
      CALL cmat%set(3,1,3._SRK)
      CALL cmat%set(3,2,3._SRK)
      CALL cmat%set(3,3,3._SRK)
      
      ! A: Rect  B: Square  C: Rect  alpha & beta
      CALL BLAS_matmult(A=thisMtrx,B=bmat,C=cmat,alpha=ALPHA,beta=BETA)
      SELECTTYPE(cmat)
        TYPE IS(DenseRectMatrixType)
          DO i=1,cmat%n
            DO j=1,cmat%n
              IF(.NOT.(cmat%a(i,j) .APPROXEQ. 4._SRK) ) THEN
                WRITE(*,*) "CALL BLAS_matmult(A=thisMtrx,B=bmat,C=cmat,alpha=ALPHA,beta=BETA) -DR DS DR FAILED!"
                STOP 666
              ENDIF
            ENDDO
          ENDDO
        CLASS DEFAULT
          WRITE(*,*) "CALL BLAS_matmult(A=thisMtrx,B=bmat,C=cmat,alpha=ALPHA,beta=BETA) FAILED!"
          WRITE(*,*) "    cmat returned as incorrect matrix type"
          STOP 666
      ENDSELECT
      ! A: Rect  B: Square  C: Rect  alpha, no beta
      CALL BLAS_matmult(A=thisMtrx,B=bmat,C=cmat,alpha=ALPHA)
      SELECTTYPE(cmat)
        TYPE IS(DenseRectMatrixType)
          DO i=1,cmat%n
            DO j=1,cmat%n
              IF(.NOT.(cmat%a(i,j) .APPROXEQ. 5._SRK) ) THEN
                WRITE(*,*) "CALL BLAS_matmult(A=thisMtrx,B=bmat,C=cmat,alpha=ALPHA) -DR DS DR FAILED!"
                STOP 666
              ENDIF
            ENDDO
          ENDDO
        CLASS DEFAULT
          WRITE(*,*) "CALL BLAS_matmult(A=thisMtrx,B=bmat,C=cmat,alpha=ALPHA) FAILED!"
          WRITE(*,*) "    cmat returned as incorrect matrix type"
          STOP 666
      ENDSELECT
      ! A: Rect  B: Square  C: Rect no alpha, beta
      CALL BLAS_matmult(A=thisMtrx,B=bmat,C=cmat,beta=BETA)
      SELECTTYPE(cmat)
        TYPE IS(DenseRectMatrixType)
          DO i=1,cmat%n
            DO j=1,cmat%n
              IF(.NOT.(cmat%a(i,j) .APPROXEQ. 6._SRK) ) THEN
                WRITE(*,*) "CALL BLAS_matmult(A=thisMtrx,B=bmat,C=cmat,beta=BETA) -DR DS DR FAILED!"
                STOP 666
              ENDIF
            ENDDO
          ENDDO
        CLASS DEFAULT
          WRITE(*,*) "CALL BLAS_matmult(A=thisMtrx,B=bmat,C=cmat,beta=BETA) FAILED!"
          WRITE(*,*) "    cmat returned as incorrect matrix type"
          STOP 666
      ENDSELECT
      ! A: Rect  B: Square  C: Rect no alpha, nobeta
      CALL BLAS_matmult(A=thisMtrx,B=bmat,C=cmat)
      SELECTTYPE(cmat)
        TYPE IS(DenseRectMatrixType)
          DO i=1,cmat%n
            DO j=1,cmat%n
              IF(.NOT.(cmat%a(i,j) .APPROXEQ. 7._SRK) ) THEN
                WRITE(*,*) "CALL BLAS_matmult(A=thisMtrx,B=bmat,C=cmat) -DR DS DR FAILED!"
                STOP 666
              ENDIF
            ENDDO
          ENDDO
        CLASS DEFAULT
          WRITE(*,*) "CALL BLAS_matmult(A=thisMtrx,B=bmat,C=cmat) FAILED!"
          WRITE(*,*) "    cmat returned as incorrect matrix type"
          STOP 666
      ENDSELECT
      WRITE(*,*) '  Passed: CALL BLAS_matmult(...) A=DR B=DS C=DR'
    
      !
      !3.) A: Rect  B: Rect  C: Square    ----------------------------
      !
      CALL cmat%clear()
      DEALLOCATE(cmat)
      ALLOCATE(DenseSquareMatrixType :: cmat)
      CALL cmat%init(3,1)  !symmetric
      CALL cmat%set(1,1,3._SRK)
      CALL cmat%set(1,2,3._SRK)
      CALL cmat%set(1,3,3._SRK)
      CALL cmat%set(2,2,3._SRK)
      CALL cmat%set(2,3,3._SRK)
      CALL cmat%set(3,3,3._SRK)
      CALL bmat%clear()
      DEALLOCATE(bmat)
      ALLOCATE(DenseRectMatrixType :: bmat)
      CALL bmat%init(3,3)  
      CALL bmat%set(1,1,1._SRK)
      CALL bmat%set(1,2,0._SRK)
      CALL bmat%set(1,3,0._SRK)
      CALL bmat%set(2,1,0._SRK)
      CALL bmat%set(2,2,1._SRK)
      CALL bmat%set(2,3,0._SRK)
      CALL bmat%set(3,1,0._SRK)
      CALL bmat%set(3,2,0._SRK)
      CALL bmat%set(3,3,1._SRK)
      
      ! A: Rect  B: Rect  C: Square  alpha & beta
      CALL BLAS_matmult(A=thisMtrx,B=bmat,C=cmat,alpha=ALPHA,beta=BETA)
      SELECTTYPE(cmat)
        TYPE IS(DenseSquareMatrixType)
          DO i=1,cmat%n
            DO j=1,cmat%n
              IF(.NOT.(cmat%a(i,j) .APPROXEQ. 4._SRK) ) THEN
                WRITE(*,*) "CALL BLAS_matmult(A=thisMtrx,B=bmat,C=cmat,alpha=ALPHA,beta=BETA) -DR DR DS FAILED!"
                STOP 666
              ENDIF
            ENDDO
          ENDDO
        CLASS DEFAULT
          WRITE(*,*) "CALL BLAS_matmult(A=thisMtrx,B=bmat,C=cmat,alpha=ALPHA,beta=BETA) FAILED!"
          WRITE(*,*) "    cmat returned as incorrect matrix type"
          STOP 666
      ENDSELECT
      ! A: Rect  B: Rect  C: Square  alpha, no beta
      CALL BLAS_matmult(A=thisMtrx,B=bmat,C=cmat,alpha=ALPHA)
      SELECTTYPE(cmat)
        TYPE IS(DenseSquareMatrixType)
          DO i=1,cmat%n
            DO j=1,cmat%n
              IF(.NOT.(cmat%a(i,j) .APPROXEQ. 5._SRK) ) THEN
                WRITE(*,*) "CALL BLAS_matmult(A=thisMtrx,B=bmat,C=cmat,alpha=ALPHA) -DR DR DS FAILED!"
                STOP 666
              ENDIF
            ENDDO
          ENDDO
        CLASS DEFAULT
          WRITE(*,*) "CALL BLAS_matmult(A=thisMtrx,B=bmat,C=cmat,alpha=ALPHA) FAILED!"
          WRITE(*,*) "    cmat returned as incorrect matrix type"
          STOP 666
      ENDSELECT
      ! A: Rect  B: Rect  C: Square no alpha, beta
      CALL BLAS_matmult(A=thisMtrx,B=bmat,C=cmat,beta=BETA)
      SELECTTYPE(cmat)
        TYPE IS(DenseSquareMatrixType)
          DO i=1,cmat%n
            DO j=1,cmat%n
              IF(.NOT.(cmat%a(i,j) .APPROXEQ. 6._SRK) ) THEN
                WRITE(*,*) "CALL BLAS_matmult(A=thisMtrx,B=bmat,C=cmat,beta=BETA) -DR DR DS FAILED!"
                STOP 666
              ENDIF
            ENDDO
          ENDDO
        CLASS DEFAULT
          WRITE(*,*) "CALL BLAS_matmult(A=thisMtrx,B=bmat,C=cmat,beta=BETA) FAILED!"
          WRITE(*,*) "    cmat returned as incorrect matrix type"
          STOP 666
      ENDSELECT
      ! A: Rect  B: Rect  C: Square no alpha, nobeta
      CALL BLAS_matmult(A=thisMtrx,B=bmat,C=cmat)
      SELECTTYPE(cmat)
        TYPE IS(DenseSquareMatrixType)
          DO i=1,cmat%n
            DO j=1,cmat%n
              IF(.NOT.(cmat%a(i,j) .APPROXEQ. 7._SRK) ) THEN
                WRITE(*,*) "CALL BLAS_matmult(A=thisMtrx,B=bmat,C=cmat) -DR DR DS FAILED!"
                STOP 666
              ENDIF
            ENDDO
          ENDDO
        CLASS DEFAULT
          WRITE(*,*) "CALL BLAS_matmult(A=thisMtrx,B=bmat,C=cmat) FAILED!"
          WRITE(*,*) "    cmat returned as incorrect matrix type"
          STOP 666
      ENDSELECT
      WRITE(*,*) '  Passed: CALL BLAS_matmult(...) A=DR B=DR C=DS'
      !
      !4.) A: Rect  B: Rect  C: Rect    ----------------------------
      !
      CALL cmat%clear()
      DEALLOCATE(cmat)
      ALLOCATE(DenseRectMatrixType :: cmat)
      CALL cmat%init(3,3)  
      CALL cmat%set(1,1,3._SRK)
      CALL cmat%set(1,2,3._SRK)
      CALL cmat%set(1,3,3._SRK)
      CALL cmat%set(2,1,3._SRK)
      CALL cmat%set(2,2,3._SRK)
      CALL cmat%set(2,3,3._SRK)
      CALL cmat%set(3,1,3._SRK)
      CALL cmat%set(3,2,3._SRK)
      CALL cmat%set(3,3,3._SRK)
      
      ! A: Rect  B: Rect  C: Rect  alpha & beta
      CALL BLAS_matmult(A=thisMtrx,B=bmat,C=cmat,alpha=ALPHA,beta=BETA)
      SELECTTYPE(cmat)
        TYPE IS(DenseRectMatrixType)
          DO i=1,cmat%n
            DO j=1,cmat%n
              IF(.NOT.(cmat%a(i,j) .APPROXEQ. 4._SRK) ) THEN
                WRITE(*,*) "CALL BLAS_matmult(A=thisMtrx,B=bmat,C=cmat,alpha=ALPHA,beta=BETA) -DR DR DR FAILED!"
                STOP 666
              ENDIF
            ENDDO
          ENDDO
        CLASS DEFAULT
          WRITE(*,*) "CALL BLAS_matmult(A=thisMtrx,B=bmat,C=cmat,alpha=ALPHA,beta=BETA) FAILED!"
          WRITE(*,*) "    cmat returned as incorrect matrix type"
          STOP 666
      ENDSELECT
      ! A: Rect  B: Rect  C: Rect  alpha, no beta
      CALL BLAS_matmult(A=thisMtrx,B=bmat,C=cmat,alpha=ALPHA)
      SELECTTYPE(cmat)
        TYPE IS(DenseRectMatrixType)
          DO i=1,cmat%n
            DO j=1,cmat%n
              IF(.NOT.(cmat%a(i,j) .APPROXEQ. 5._SRK) ) THEN
                WRITE(*,*) "CALL BLAS_matmult(A=thisMtrx,B=bmat,C=cmat,alpha=ALPHA) -DR DR DR FAILED!"
                STOP 666
              ENDIF
            ENDDO
          ENDDO
        CLASS DEFAULT
          WRITE(*,*) "CALL BLAS_matmult(A=thisMtrx,B=bmat,C=cmat,alpha=ALPHA) FAILED!"
          WRITE(*,*) "    cmat returned as incorrect matrix type"
          STOP 666
      ENDSELECT
      ! A: Rect  B: Rect  C: Rect no alpha, beta
      CALL BLAS_matmult(A=thisMtrx,B=bmat,C=cmat,beta=BETA)
      SELECTTYPE(cmat)
        TYPE IS(DenseRectMatrixType)
          DO i=1,cmat%n
            DO j=1,cmat%n
              IF(.NOT.(cmat%a(i,j) .APPROXEQ. 6._SRK) ) THEN
                WRITE(*,*) "CALL BLAS_matmult(A=thisMtrx,B=bmat,C=cmat,beta=BETA) -DR DR DR FAILED!"
                STOP 666
              ENDIF
            ENDDO
          ENDDO
        CLASS DEFAULT
          WRITE(*,*) "CALL BLAS_matmult(A=thisMtrx,B=bmat,C=cmat,beta=BETA) FAILED!"
          WRITE(*,*) "    cmat returned as incorrect matrix type"
          STOP 666
      ENDSELECT
      ! A: Rect  B: Rect  C: Rect no alpha, nobeta
      CALL BLAS_matmult(A=thisMtrx,B=bmat,C=cmat)
      SELECTTYPE(cmat)
        TYPE IS(DenseRectMatrixType)
          DO i=1,cmat%n
            DO j=1,cmat%n
              IF(.NOT.(cmat%a(i,j) .APPROXEQ. 7._SRK) ) THEN
                WRITE(*,*) "CALL BLAS_matmult(A=thisMtrx,B=bmat,C=cmat) -DR DR DR FAILED!"
                STOP 666
              ENDIF
            ENDDO
          ENDDO
        CLASS DEFAULT
          WRITE(*,*) "CALL BLAS_matmult(A=thisMtrx,B=bmat,C=cmat) FAILED!"
          WRITE(*,*) "    cmat returned as incorrect matrix type"
          STOP 666
      ENDSELECT
      WRITE(*,*) '  Passed: CALL BLAS_matmult(...) A=DR B=DR C=DR'
      CALL cmat%clear()
      DEALLOCATE(cmat)
      CALL bmat%clear()
      DEALLOCATE(bmat)
      CALL thisMtrx%clear()
      DEALLOCATE(thisMtrx)
    ENDSUBROUTINE  testMatrixMultRect
!
!-------------------------------------------------------------------------------
    SUBROUTINE testBLAS1()
      INTEGER(SIK) :: iresult
      REAL(SDK) :: da(200),da2(20,20),dresult,dalpha(200)
      REAL(SDK) :: db(200),db2(20,20)
      REAL(SSK) :: sa(200),sa2(20,20),sresult,salpha(200)
      REAL(SSK) :: sb(200),sb2(20,20)
  !
  !Test _swap
      da=1.0_SDK
      db=0.0_SDK
      CALL BLAS_swap(10,da,1,db,1)
      IF(ANY(da(1:10) /= 0.0_SDK) .OR. ANY(db(1:10) /= 1.0_SDK)) THEN
        WRITE(*,*) 'CALL BLAS_swap(10,da,1,db,1) FAILED!'
        STOP 666
      ENDIF
      CALL BLAS_swap(10,da(11:20),db(11:200))
      IF(ANY(da(1:20) /= 0.0_SDK) .OR. ANY(db(1:20) /= 1.0_SDK)) THEN
        WRITE(*,*) 'CALL BLAS_swap(10,da(11:20),db(11:200)) FAILED!'
        STOP 666
      ENDIF
      CALL BLAS_swap(da(21:30),1,db(21:30),1)
      IF(ANY(da(1:30) /= 0.0_SDK) .OR. ANY(db(1:30) /= 1.0_SDK)) THEN
        WRITE(*,*) 'CALL BLAS_swap(da(21:30),1,db(21:30),1) FAILED!'
        STOP 666
      ENDIF
      CALL BLAS_swap(da(31:200),db(31:200))
      IF(ANY(da /= 0.0_SDK) .OR. ANY(db /= 1.0_SDK)) THEN
        WRITE(*,*) 'CALL BLAS_swap(da(31:200),db(31:200)) FAILED!'
        STOP 666
      ENDIF
      !Test swap on "2-D vectors"
      da2=1.0_SDK
      db2=0.0_SDK
      CALL BLAS_swap(400,da2(:,1),1,db2(:,1),1) !swap the whole thing
      IF(ANY(da2 /= 0.0_SDK) .OR. ANY(db2 /= 1.0_SDK)) THEN
        WRITE(*,*) 'CALL BLAS_swap(400,da2(:,1),1,db2(:,1),1) FAILED!'
        STOP 666
      ENDIF
      CALL BLAS_swap(20,da2(:,1),20,db2(:,1),20) !swap first rows
      IF(ANY(da2(1,:) /= 1.0_SDK) .OR. ANY(db2(1,:) /= 0.0_SDK)) THEN
        WRITE(*,*) 'CALL BLAS_swap(20,da2(:,1),20,db2(:,1),20) FAILED!'
        STOP 666
      ENDIF
      CALL BLAS_swap(20,da2(:,1),da2(5:,1),20) !swap the 1st and 5th rows
      IF(ANY(da2(1,:) /= 0.0_SDK) .OR. ANY(da2(5,:) /= 1.0_SDK)) THEN
        WRITE(*,*) 'CALL BLAS_swap(20,da2(:,1),20,da2(5:,1),20) FAILED!'
        STOP 666
      ENDIF
      !By not including n the rows are only partly swapped because da2(5:,1) is
      !shorter than da2(:,1)
      CALL BLAS_swap(da2(:,1),da2(5:,1),20)
      IF(ANY(da2(1,1:16) /= 1.0_SDK) .OR. ANY(da2(1,17:20) /= 0.0_SDK) &
          .OR. ANY(da2(5,1:16) /= 0.0_SDK) .OR. ANY(da2(5,17:20) /= 1.0_SDK)) THEN
        WRITE(*,*) 'CALL BLAS_swap(da2(:,1),20,da2(5:,1),20) FAILED!'
        STOP 666
      ENDIF
      sa=1.0_SSK
      sb=0.0_SSK
      CALL BLAS_swap(10,sa,1,sb,1)
      IF(ANY(sa(1:10) /= 0.0_SSK) .OR. ANY(sb(1:10) /= 1.0_SSK)) THEN
        WRITE(*,*) 'CALL BLAS_swap(10,sa,1,sb,1) FAILED!'
        STOP 666
      ENDIF
      CALL BLAS_swap(10,sa(11:20),sb(11:200))
      IF(ANY(sa(1:20) /= 0.0_SSK) .OR. ANY(sb(1:20) /= 1.0_SSK)) THEN
        WRITE(*,*) 'CALL BLAS_swap(10,sa(11:20),sb(11:200)) FAILED!'
        STOP 666
      ENDIF
      CALL BLAS_swap(sa(21:30),1,sb(21:30),1)
      IF(ANY(sa(1:30) /= 0.0_SSK) .OR. ANY(sb(1:30) /= 1.0_SSK)) THEN
        WRITE(*,*) 'CALL BLAS_swap(sa(21:30),1,sb(21:30),1) FAILED!'
        STOP 666
      ENDIF
      CALL BLAS_swap(sa(31:200),sb(31:200))
      IF(ANY(sa /= 0.0_SSK) .OR. ANY(sb /= 1.0_SSK)) THEN
        WRITE(*,*) 'CALL BLAS_swap(sa(31:200),sb(31:200)) FAILED!'
        STOP 666
      ENDIF
      !Test swap on "2-D vectors"
      sa2=1.0_SSK
      sb2=0.0_SSK
      CALL BLAS_swap(400,sa2(:,1),1,sb2(:,1),1) !swap the whole thing
      IF(ANY(sa2 /= 0.0_SSK) .OR. ANY(sb2 /= 1.0_SSK)) THEN
        WRITE(*,*) 'CALL BLAS_swap(400,sa2(:,1),1,sb2(:,1),1) FAILED!'
        STOP 666
      ENDIF
      CALL BLAS_swap(20,sa2(:,1),20,sb2(:,1),20) !swap first rows
      IF(ANY(sa2(1,:) /= 1.0_SSK) .OR. ANY(sb2(1,:) /= 0.0_SSK)) THEN
        WRITE(*,*) 'CALL BLAS_swap(20,sa2(:,1),20,sb2(:,1),20) FAILED!'
        STOP 666
      ENDIF
      CALL BLAS_swap(20,sa2(:,1),sa2(5:,1),20) !swap the 1st and 5th rows
      IF(ANY(sa2(1,:) /= 0.0_SSK) .OR. ANY(sa2(5,:) /= 1.0_SSK)) THEN
        WRITE(*,*) 'CALL BLAS_swap(20,sa2(:,1),20,sa2(5:,1),20) FAILED!'
        STOP 666
      ENDIF
      !By not including n the rows are only partly swapped because sa2(5:,1) is
      !shorter than sa2(:,1)
      CALL BLAS_swap(sa2(:,1),sa2(5:,1),20)
      IF(ANY(sa2(1,1:16) /= 1.0_SSK) .OR. ANY(sa2(1,17:20) /= 0.0_SSK) &
          .OR. ANY(sa2(5,1:16) /= 0.0_SSK) .OR. ANY(sa2(5,17:20) /= 1.0_SSK)) THEN
        WRITE(*,*) 'CALL BLAS_swap(sa2(:,1),20,sa2(5:,1),20) FAILED!'
        STOP 666
      ENDIF    
      WRITE(*,*) '  Passed: CALL BLAS_swap(...)'
  !
  !Test _copy
      da=1.0_SDK
      db=0.0_SDK
      CALL BLAS_copy(10,da,1,db,1)
      IF(ANY(db(1:10) /= 1.0_SDK) .OR. ANY(db(11:200) /= 0.0_SDK)) THEN
        WRITE(*,*) 'CALL BLAS_copy(10,da,1,db,1) FAILED!'
        STOP 666
      ENDIF
      CALL BLAS_copy(10,da(11:20),db(11:200))
      IF(ANY(db(1:20) /= 1.0_SDK) .OR. ANY(db(21:200) /= 0.0_SDK)) THEN
        WRITE(*,*) 'CALL BLAS_copy(10,da(11:20),db(11:200)) FAILED!'
        STOP 666
      ENDIF
      CALL BLAS_copy(da(21:30),1,db(21:30),1)
      IF(ANY(db(1:30) /= 1.0_SDK) .OR. ANY(db(31:200) /= 0.0_SDK)) THEN
        WRITE(*,*) 'CALL BLAS_copy(da(21:30),1,db(21:30),1) FAILED!'
        STOP 666
      ENDIF
      CALL BLAS_copy(da(31:40),db(31:40),1)
      IF(ANY(db(1:40) /= 1.0_SDK) .OR. ANY(db(41:200) /= 0.0_SDK)) THEN
        WRITE(*,*) 'CALL BLAS_copy(da(31:40),db(31:40),1) FAILED!'
        STOP 666
      ENDIF
      CALL BLAS_copy(da(41:200),db(41:200))
      IF(ANY(db /= 1.0_SDK)) THEN
        WRITE(*,*) 'CALL BLAS_copy(da(41:200),db(41:200)) FAILED!'
        STOP 666
      ENDIF
      !Test swap on "2-D vectors"
      da2=1.0_SDK
      db2=0.0_SDK
      CALL BLAS_copy(400,da2(:,1),1,db2(:,1),1) !copy the whole thing
      IF(ANY(db2 /= 1.0_SDK)) THEN
        WRITE(*,*) 'CALL BLAS_copy(400,da2(:,1),1,db2(:,1),1) FAILED!'
        STOP 666
      ENDIF
      da2(1,:)=0.0_SDK
      CALL BLAS_copy(20,da2(:,1),db2(:,1),20) !copy first row
      IF(ANY(db2(1,:) /= 0.0_SDK) .OR. ANY(db2(2:20,:) /= 1.0_SDK)) THEN
        WRITE(*,*) 'CALL BLAS_copy(20,da2(:,1),20,db2(:,1),20) FAILED!'
        STOP 666
      ENDIF
      sa=1.0_SSK
      sb=0.0_SSK
      CALL BLAS_copy(10,sa,1,sb,1)
      IF(ANY(sb(1:10) /= 1.0_SSK) .OR. ANY(sb(11:200) /= 0.0_SSK)) THEN
        WRITE(*,*) 'CALL BLAS_copy(10,sa,1,sb,1) FAILED!'
        STOP 666
      ENDIF
      CALL BLAS_copy(10,sa(11:20),sb(11:200))
      IF(ANY(sb(1:20) /= 1.0_SSK) .OR. ANY(sb(21:200) /= 0.0_SSK)) THEN
        WRITE(*,*) 'CALL BLAS_copy(10,sa(11:20),sb(11:200)) FAILED!'
        STOP 666
      ENDIF
      CALL BLAS_copy(sa(21:30),1,sb(21:30),1)
      IF(ANY(sb(1:30) /= 1.0_SSK) .OR. ANY(sb(31:200) /= 0.0_SSK)) THEN
        WRITE(*,*) 'CALL BLAS_copy(sa(21:30),1,sb(21:30),1) FAILED!'
        STOP 666
      ENDIF
      CALL BLAS_copy(sa(31:40),sb(31:40),1)
      IF(ANY(sb(1:40) /= 1.0_SSK) .OR. ANY(sb(41:200) /= 0.0_SSK)) THEN
        WRITE(*,*) 'CALL BLAS_copy(sa(31:40),sb(31:40),1) FAILED!'
        STOP 666
      ENDIF
      CALL BLAS_copy(sa(41:200),sb(41:200))
      IF(ANY(sb /= 1.0_SSK)) THEN
        WRITE(*,*) 'CALL BLAS_copy(sa(41:200),sb(41:200)) FAILED!'
        STOP 666
      ENDIF
      !Test swap on "2-D vectors"
      sa2=1.0_SSK
      sb2=0.0_SSK
      CALL BLAS_copy(400,sa2(:,1),1,sb2(:,1),1) !copy the whole thing
      IF(ANY(sb2 /= 1.0_SSK)) THEN
        WRITE(*,*) 'CALL BLAS_copy(400,sa2(:,1),1,sb2(:,1),1) FAILED!'
        STOP 666
      ENDIF
      sa2(1,:)=0.0_SSK
      CALL BLAS_copy(20,sa2(:,1),sb2(:,1),20) !copy first row
      IF(ANY(sb2(1,:) /= 0.0_SSK) .OR. ANY(sb2(2:20,:) /= 1.0_SSK)) THEN
        WRITE(*,*) 'CALL BLAS_copy(20,sa2(:,1),20,sb2(:,1),20) FAILED!'
        STOP 666
      ENDIF
      WRITE(*,*) '  Passed: CALL BLAS_copy(...)'
  !
  !Test _nrm2
      da=0.5_SDK
      da(1)=-0.5_SDK
      dresult=BLAS_nrm2(da(1:169),1)
      IF(.NOT.(dresult .APPROXEQ. 6.5_SDK)) THEN
        WRITE(*,*) 'BLAS_nrm2(da) FAILED!'
        STOP 666
      ENDIF
      dresult=BLAS_nrm2(da)
      IF(.NOT.(dresult .APPROXEQ. SQRT(50.0_SDK))) THEN
        WRITE(*,*) 'BLAS_nrm2(da) FAILED!'
        STOP 666
      ENDIF
      dresult=BLAS_nrm2(1,da)
      IF(.NOT.(dresult .APPROXEQ. 0.5_SDK)) THEN
        WRITE(*,*) 'BLAS_nrm2(1,da) FAILED!'
        STOP 666
      ENDIF
      dresult=BLAS_nrm2(-10,da)
      IF(.NOT.(dresult .APPROXEQ. 0.0_SDK)) THEN
        WRITE(*,*) 'BLAS_nrm2(-10,da) FAILED!'
        STOP 666
      ENDIF
      da(1:20:2)=0.25_SDK
      da(5)=0.0_SDK
      da(7)=0.25_SDK*SQRT(2._SDK)
      dresult=BLAS_nrm2(10,da,2)
      IF(.NOT.(dresult .APPROXEQ. SQRT(0.625_SDK))) THEN
        WRITE(*,*) 'BLAS_nrm2(10,da,2) FAILED!'
        STOP 666
      ENDIF
      da=1.0_SDK
      da(1)=0.0_SDK
      dresult=BLAS_nrm2(10,da(1:10),1)
      IF(.NOT.(dresult .APPROXEQ. 3.0_SDK)) THEN
        WRITE(*,*) 'BLAS_nrm2(10,da(1:10),1) FAILED!'
        STOP 666
      ENDIF
      sa=0.5_SSK
      sa(1)=-0.5_SSK
      sresult=BLAS_nrm2(sa(1:169),1)
      IF(.NOT.(sresult .APPROXEQ. 6.5_SSK)) THEN
        WRITE(*,*) 'BLAS_nrm2(sa) FAILED!'
        STOP 666
      ENDIF
      sresult=BLAS_nrm2(sa)
      IF(.NOT.(sresult .APPROXEQ. SQRT(50.0_SSK))) THEN
        WRITE(*,*) 'BLAS_nrm2(sa) FAILED!'
        STOP 666
      ENDIF
      sresult=BLAS_nrm2(1,sa)
      IF(.NOT.(sresult .APPROXEQ. 0.5_SSK)) THEN
        WRITE(*,*) 'BLAS_nrm2(1,sa) FAILED!'
        STOP 666
      ENDIF
      sresult=BLAS_nrm2(-10,sa)
      IF(.NOT.(sresult .APPROXEQ. 0.0_SSK)) THEN
        WRITE(*,*) 'BLAS_nrm2(-10,sa) FAILED!'
        STOP 666
      ENDIF
      sa(1:20:2)=0.25_SSK
      sa(5)=0.0_SSK
      sa(7)=0.25_SSK*SQRT(2._SSK)
      sresult=BLAS_nrm2(10,sa,2)
      IF(.NOT.(sresult .APPROXEQ. SQRT(0.625_SSK))) THEN
        WRITE(*,*) 'BLAS_nrm2(10,sa,2) FAILED!'
        STOP 666
      ENDIF
      sa=1.0_SSK
      sa(1)=0.0_SSK
      sresult=BLAS_nrm2(10,sa(1:10),1)
      IF(.NOT.(sresult .APPROXEQ. 3.0_SSK)) THEN
        WRITE(*,*) 'BLAS_nrm2(10,sa(1:10),1) FAILED!'
        STOP 666
      ENDIF
      WRITE(*,*) '  Passed: BLAS_nrm2(...)'
  !
  !Test _asum
      da=0.5_SDK
      da(1)=-0.5
      dresult=BLAS_asum(da)
      IF(.NOT.(dresult .APPROXEQ. 100.0_SDK)) THEN
        WRITE(*,*) 'BLAS_asum(da) FAILED!'
        STOP 666
      ENDIF
      dresult=BLAS_asum(10,da)
      IF(.NOT.(dresult .APPROXEQ. 5.0_SDK)) THEN
        WRITE(*,*) 'BLAS_asum(10,da) FAILED!'
        STOP 666
      ENDIF
      dresult=BLAS_asum(0,da,1)
      IF(.NOT.(dresult .APPROXEQ. 0.0_SDK)) THEN
        WRITE(*,*) 'BLAS_asum(0,da,1) FAILED!'
        STOP 666
      ENDIF
      dresult=BLAS_asum(10,da,0)
      IF(.NOT.(dresult .APPROXEQ. 0.0_SDK)) THEN
        WRITE(*,*) 'BLAS_asum(10,da,0) FAILED!'
        STOP 666
      ENDIF
      da(1:20:2)=0.25_SDK
      dresult=BLAS_asum(da(1:10),2)
      IF(.NOT.(dresult .APPROXEQ. 2.5_SDK)) THEN
        WRITE(*,*) 'BLAS_asum(10,da) FAILED!'
        STOP 666
      ENDIF
      sa=0.5_SSK
      sa(1)=-0.5
      sresult=BLAS_asum(sa)
      IF(.NOT.(sresult .APPROXEQ. 100.0_SSK)) THEN
        WRITE(*,*) 'BLAS_asum(sa) FAILED!'
        STOP 666
      ENDIF
      sresult=BLAS_asum(10,sa)
      IF(.NOT.(sresult .APPROXEQ. 5.0_SSK)) THEN
        WRITE(*,*) 'BLAS_asum(10,sa) FAILED!'
        STOP 666
      ENDIF
      sresult=BLAS_asum(0,sa,1)
      IF(.NOT.(sresult .APPROXEQ. 0.0_SSK)) THEN
        WRITE(*,*) 'BLAS_asum(0,sa,1) FAILED!'
        STOP 666
      ENDIF
      sresult=BLAS_asum(10,sa,0)
      IF(.NOT.(sresult .APPROXEQ. 0.0_SSK)) THEN
        WRITE(*,*) 'BLAS_asum(10,sa,0) FAILED!'
        STOP 666
      ENDIF
      sa(1:20:2)=0.25_SSK
      sresult=BLAS_asum(sa(1:10),2)
      IF(.NOT.(sresult .APPROXEQ. 2.5_SSK)) THEN
        WRITE(*,*) 'BLAS_asum(10,sa) FAILED!'
        STOP 666
      ENDIF
      WRITE(*,*) '  Passed: BLAS_asum(...)'
  !
  !Test i_amin
      da=1.0_SDK
      da(100)=-5.0_SDK
      da(50)=0.5_SDK
      da(60)=0.5_SDK
      da(70)=5.0_SDK
      da(80)=5.0_SDK
      iresult=BLAS_iamin(da)
      IF(iresult /= 50) THEN
        WRITE(*,*) 'BLAS_iamin(da) FAILED!'
        STOP 666
      ENDIF
      iresult=BLAS_iamin(10,da(20:200),20)
      IF(iresult /= 3) THEN
        WRITE(*,*) 'BLAS_iamin(10,da(20:200),20) FAILED!'
        STOP 666
      ENDIF
      iresult=BLAS_iamin(da,0)
      IF(iresult /= 0) THEN
        WRITE(*,*) 'BLAS_iamin(da,0) FAILED!'
        STOP 666
      ENDIF
      iresult=BLAS_iamin(0,da(1:10))
      IF(iresult /= 0) THEN
        WRITE(*,*) 'BLAS_iamin(0,da(1:10)) FAILED!'
        STOP 666
      ENDIF
      iresult=BLAS_iamin(1,da)
      IF(iresult /= 1) THEN
        WRITE(*,*) 'BLAS_iamin(1,da) FAILED!'
        STOP 666
      ENDIF
      sa=1.0_SSK
      sa(100)=-5.0_SSK
      sa(50)=0.5_SSK
      sa(60)=0.5_SSK
      sa(70)=5.0_SSK
      sa(80)=5.0_SSK
      iresult=BLAS_iamin(sa)
      IF(iresult /= 50) THEN
        WRITE(*,*) 'BLAS_iamin(sa) FAILED!'
        STOP 666
      ENDIF
      iresult=BLAS_iamin(10,sa(20:200),20)
      IF(iresult /= 3) THEN
        WRITE(*,*) 'BLAS_iamin(10,sa(20:200),20) FAILED!'
        STOP 666
      ENDIF
      iresult=BLAS_iamin(sa,0)
      IF(iresult /= 0) THEN
        WRITE(*,*) 'BLAS_iamin(sa,0) FAILED!'
        STOP 666
      ENDIF
      iresult=BLAS_iamin(0,sa(1:10))
      IF(iresult /= 0) THEN
        WRITE(*,*) 'BLAS_iamin(0,sa(1:10)) FAILED!'
        STOP 666
      ENDIF
      iresult=BLAS_iamin(1,sa)
      IF(iresult /= 1) THEN
        WRITE(*,*) 'BLAS_iamin(1,sa) FAILED!'
        STOP 666
      ENDIF
      WRITE(*,*) '  Passed: BLAS_iamin(...)'
  !
  !Test i_amax
      iresult=BLAS_iamax(da)
      IF(iresult /= 70) THEN
        WRITE(*,*) 'BLAS_iamax(da) FAILED!'
        STOP 666
      ENDIF
      iresult=BLAS_iamax(10,da(20:200),20)
      IF(iresult /= 4) THEN
        WRITE(*,*) 'BLAS_iamax(10,da(20:200),20) FAILED!'
        STOP 666
      ENDIF
      iresult=BLAS_iamax(da,0)
      IF(iresult /= 0) THEN
        WRITE(*,*) 'BLAS_iamax(da,0) FAILED!'
        STOP 666
      ENDIF
      iresult=BLAS_iamax(0,da(1:10))
      IF(iresult /= 0) THEN
        WRITE(*,*) 'BLAS_iamax(0,da(1:10)) FAILED!'
        STOP 666
      ENDIF
      iresult=BLAS_iamax(1,da)
      IF(iresult /= 1) THEN
        WRITE(*,*) 'BLAS_iamax(1,da) FAILED!'
        STOP 666
      ENDIF
      iresult=BLAS_iamax(sa)
      IF(iresult /= 70) THEN
        WRITE(*,*) 'BLAS_iamax(sa) FAILED!'
        STOP 666
      ENDIF
      iresult=BLAS_iamax(10,sa(20:200),20)
      IF(iresult /= 4) THEN
        WRITE(*,*) 'BLAS_iamax(10,sa(20:200),20) FAILED!'
        STOP 666
      ENDIF
      iresult=BLAS_iamax(sa,0)
      IF(iresult /= 0) THEN
        WRITE(*,*) 'BLAS_iamax(sa,0) FAILED!'
        STOP 666
      ENDIF
      iresult=BLAS_iamax(0,sa(1:10))
      IF(iresult /= 0) THEN
        WRITE(*,*) 'BLAS_iamax(0,sa(1:10)) FAILED!'
        STOP 666
      ENDIF
      iresult=BLAS_iamax(1,sa)
      IF(iresult /= 1) THEN
        WRITE(*,*) 'BLAS_iamax(1,sa) FAILED!'
        STOP 666
      ENDIF
      WRITE(*,*) '  Passed: BLAS_iamax(...)'
  !
  !Test _dot
      da=2.0_SDK
      db=0.5_SDK
      dresult=BLAS_dot(da(1:199),db(1:199))
      IF(.NOT.(dresult .APPROXEQ. 199.0_SDK)) THEN
        WRITE(*,*) 'BLAS_dot(da,db) FAILED!'
        STOP 666
      ENDIF
      dresult=BLAS_dot(10,da,db,20)
      IF(.NOT.(dresult .APPROXEQ. 10.0_SDK)) THEN
        WRITE(*,*) 'BLAS_dot(10,da,db,20) FAILED!'
        STOP 666
      ENDIF
      dresult=BLAS_dot(5,da,10,db,20)
      IF(.NOT.(dresult .APPROXEQ. 5.0_SDK)) THEN
        WRITE(*,*) 'BLAS_dot(5,da,10,db,20) FAILED!'
        STOP 666
      ENDIF
      dresult=BLAS_dot(da(1:12),1,db(1:12),2)
      IF(.NOT.(dresult .APPROXEQ. 12.0_SDK)) THEN
        WRITE(*,*) 'BLAS_dot(da(1:12),1,db(1:12),2) FAILED!'
        STOP 666
      ENDIF
      dresult=BLAS_dot(da(51:61),db(51:56),-2)
      IF(.NOT.(dresult .APPROXEQ. 6.0_SDK)) THEN
        WRITE(*,*) 'BLAS_dot(da(51:61),1,db(51:60),-2) FAILED!'
        STOP 666
      ENDIF
      dresult=BLAS_dot(-10,da,db)
      IF(.NOT.(dresult .APPROXEQ. 0.0_SDK)) THEN
        WRITE(*,*) 'BLAS_dot(-10,da,db) FAILED!'
        STOP 666
      ENDIF
      sa=2.0_SSK
      sb=0.5_SSK
      sresult=BLAS_dot(sa(1:199),sb(1:199))
      IF(.NOT.(sresult .APPROXEQ. 199.0_SSK)) THEN
        WRITE(*,*) 'BLAS_dot(sa,sb) FAILED!'
        STOP 666
      ENDIF
      sresult=BLAS_dot(10,sa,sb,20)
      IF(.NOT.(sresult .APPROXEQ. 10.0_SSK)) THEN
        WRITE(*,*) 'BLAS_dot(10,sa,sb,20) FAILED!'
        STOP 666
      ENDIF
      sresult=BLAS_dot(5,sa,10,sb,20)
      IF(.NOT.(sresult .APPROXEQ. 5.0_SSK)) THEN
        WRITE(*,*) 'BLAS_dot(5,sa,10,sb,20) FAILED!'
        STOP 666
      ENDIF
      sresult=BLAS_dot(sa(1:12),1,sb(1:12),2)
      IF(.NOT.(sresult .APPROXEQ. 12.0_SSK)) THEN
        WRITE(*,*) 'BLAS_dot(sa(1:12),1,sb(1:12),2) FAILED!'
        STOP 666
      ENDIF
      sresult=BLAS_dot(sa(51:61),sb(51:56),-2)
      IF(.NOT.(sresult .APPROXEQ. 6.0_SSK)) THEN
        WRITE(*,*) 'BLAS_dot(sa(51:61),1,sb(51:60),-2) FAILED!'
        STOP 666
      ENDIF
      sresult=BLAS_dot(-10,sa,sb)
      IF(.NOT.(sresult .APPROXEQ. 0.0_SSK)) THEN
        WRITE(*,*) 'BLAS_dot(-10,sa,sb) FAILED!'
        STOP 666
      ENDIF
      WRITE(*,*) '  Passed: BLAS_dot(...)'
  !
  !Test _scal
      da=2.0_SDK
      da(200)=1.0_SDK
      CALL BLAS_scal(0.5_SDK,da(1:199))
      IF(ANY(.NOT.(da .APPROXEQ. 1.0_SDK))) THEN
        WRITE(*,*) 'CALL BLAS_scal(0.5_SDK,da) FAILED!'
        STOP 666
      ENDIF
      CALL BLAS_scal(0.5_SDK,da,-2)
      IF(ANY(.NOT.(da .APPROXEQ. 1.0_SDK))) THEN
        WRITE(*,*) 'CALL BLAS_scal(0.5_SDK,da,-2) FAILED!'
        STOP 666
      ENDIF
      CALL BLAS_scal(-5,0.5_SDK,da)
      IF(ANY(.NOT.(da .APPROXEQ. 1.0_SDK))) THEN
        WRITE(*,*) 'CALL BLAS_scal(-5,0.5_SDK,da) FAILED!'
        STOP 666
      ENDIF
      CALL BLAS_scal(100,0.5_SDK,da,2)
      IF(ANY(.NOT.(da(1:200:2) .APPROXEQ. 0.5_SDK)) .OR. &
        ANY(.NOT.(da(2:200:2) .APPROXEQ. 1.0_SDK))) THEN
        WRITE(*,*) 'CALL BLAS_scal(100,0.5_SDK,da,2) FAILED!'
        STOP 666
      ENDIF
      CALL BLAS_scal(50,0.5_SDK,da,1)
      IF(ANY(.NOT.(da(1:50:2) .APPROXEQ. 0.25_SDK)) .OR. &
        ANY(.NOT.(da(2:50:2) .APPROXEQ. 0.5_SDK))) THEN
        WRITE(*,*) 'CALL BLAS_scal(50,0.5_SDK,da,1) FAILED!'
        STOP 666
      ENDIF
      !Test vector extensions
      da=2.0_SDK
      db(1:200:2)=-0.5_SDK
      db(2:200:2)=0.5_SDK
      da(200)=1.0_SDK
      CALL BLAS_scal(db(1:199),da(1:199))
      IF(ANY(.NOT.(da(1:200:2) .APPROXEQ. -1.0_SDK)) .OR. &
        ANY(.NOT.(da(2:200:2) .APPROXEQ. 1.0_SDK))) THEN
        WRITE(*,*) 'CALL BLAS_scal(db,da) FAILED!'
        STOP 666
      ENDIF
      CALL BLAS_scal(db,da,-2)
      IF(ANY(.NOT.(da(1:200:2) .APPROXEQ. -1.0_SDK)) .OR. &
        ANY(.NOT.(da(2:200:2) .APPROXEQ. 1.0_SDK))) THEN
        WRITE(*,*) 'CALL BLAS_scal(db,da,-2) FAILED!'
        STOP 666
      ENDIF
      CALL BLAS_scal(0,db,da)
      IF(ANY(.NOT.(da(1:200:2) .APPROXEQ. -1.0_SDK)) .OR. &
        ANY(.NOT.(da(2:200:2) .APPROXEQ. 1.0_SDK))) THEN
        WRITE(*,*) 'CALL BLAS_scal(0,db,da) FAILED!'
        STOP 666
      ENDIF
      CALL BLAS_scal(db(1:10),da)
      IF(ANY(.NOT.(da(1:200:2) .APPROXEQ. -1.0_SDK)) .OR. &
        ANY(.NOT.(da(2:200:2) .APPROXEQ. 1.0_SDK))) THEN
        WRITE(*,*) 'CALL BLAS_scal(0,db,da) FAILED!'
        STOP 666
      ENDIF
      CALL BLAS_scal(100,db,da,2)
      IF(ANY(.NOT.(da(1:200:2) .APPROXEQ. 0.5_SDK)) .OR. &
        ANY(.NOT.(da(2:200:2) .APPROXEQ. 1.0_SDK))) THEN
        WRITE(*,*) 'CALL BLAS_scal(100,db,da,2) FAILED!'
        STOP 666
      ENDIF
      CALL BLAS_scal(50,db,da,1)
      IF(ANY(.NOT.(da(1:50:2) .APPROXEQ. -0.25_SDK)) .OR. &
        ANY(.NOT.(da(2:50:2) .APPROXEQ. 0.5_SDK))) THEN
        WRITE(*,*) 'CALL BLAS_scal(50,db,da,1) FAILED!'
        STOP 666
      ENDIF
      sa=2.0_SSK
      sa(200)=1.0_SSK
      CALL BLAS_scal(0.5_SSK,sa(1:199))
      IF(ANY(.NOT.(sa .APPROXEQ. 1.0_SSK))) THEN
        WRITE(*,*) 'CALL BLAS_scal(0.5_SSK,sa) FAILED!'
        STOP 666
      ENDIF
      CALL BLAS_scal(0.5_SSK,sa,-2)
      IF(ANY(.NOT.(sa .APPROXEQ. 1.0_SSK))) THEN
        WRITE(*,*) 'CALL BLAS_scal(0.5_SSK,sa,-2) FAILED!'
        STOP 666
      ENDIF
      CALL BLAS_scal(-5,0.5_SSK,sa)
      IF(ANY(.NOT.(sa .APPROXEQ. 1.0_SSK))) THEN
        WRITE(*,*) 'CALL BLAS_scal(-5,0.5_SSK,sa) FAILED!'
        STOP 666
      ENDIF
      CALL BLAS_scal(100,0.5_SSK,sa,2)
      IF(ANY(.NOT.(sa(1:200:2) .APPROXEQ. 0.5_SSK)) .OR. &
        ANY(.NOT.(sa(2:200:2) .APPROXEQ. 1.0_SSK))) THEN
        WRITE(*,*) 'CALL BLAS_scal(100,0.5_SSK,sa,2) FAILED!'
        STOP 666
      ENDIF
      CALL BLAS_scal(50,0.5_SSK,sa,1)
      IF(ANY(.NOT.(sa(1:50:2) .APPROXEQ. 0.25_SSK)) .OR. &
        ANY(.NOT.(sa(2:50:2) .APPROXEQ. 0.5_SSK))) THEN
        WRITE(*,*) 'CALL BLAS_scal(50,0.5_SSK,sa,1) FAILED!'
        STOP 666
      ENDIF
      !Test vector extensions
      sa=2.0_SSK
      sb(1:200:2)=-0.5_SSK
      sb(2:200:2)=0.5_SSK
      sa(200)=1.0_SSK
      CALL BLAS_scal(sb(1:199),sa(1:199))
      IF(ANY(.NOT.(sa(1:200:2) .APPROXEQ. -1.0_SSK)) .OR. &
        ANY(.NOT.(sa(2:200:2) .APPROXEQ. 1.0_SSK))) THEN
        WRITE(*,*) 'CALL BLAS_scal(sb,sa) FAILED!'
        STOP 666
      ENDIF
      CALL BLAS_scal(sb,sa,-2)
      IF(ANY(.NOT.(sa(1:200:2) .APPROXEQ. -1.0_SSK)) .OR. &
        ANY(.NOT.(sa(2:200:2) .APPROXEQ. 1.0_SSK))) THEN
        WRITE(*,*) 'CALL BLAS_scal(sb,sa,-2) FAILED!'
        STOP 666
      ENDIF
      CALL BLAS_scal(0,sb,sa)
      IF(ANY(.NOT.(sa(1:200:2) .APPROXEQ. -1.0_SSK)) .OR. &
        ANY(.NOT.(sa(2:200:2) .APPROXEQ. 1.0_SSK))) THEN
        WRITE(*,*) 'CALL BLAS_scal(0,sb,sa) FAILED!'
        STOP 666
      ENDIF
      CALL BLAS_scal(sb(1:10),sa)
      IF(ANY(.NOT.(sa(1:200:2) .APPROXEQ. -1.0_SSK)) .OR. &
        ANY(.NOT.(sa(2:200:2) .APPROXEQ. 1.0_SSK))) THEN
        WRITE(*,*) 'CALL BLAS_scal(0,sb,sa) FAILED!'
        STOP 666
      ENDIF
      CALL BLAS_scal(100,sb,sa,2)
      IF(ANY(.NOT.(sa(1:200:2) .APPROXEQ. 0.5_SSK)) .OR. &
        ANY(.NOT.(sa(2:200:2) .APPROXEQ. 1.0_SSK))) THEN
        WRITE(*,*) 'CALL BLAS_scal(100,sb,sa,2) FAILED!'
        STOP 666
      ENDIF
      CALL BLAS_scal(50,sb,sa,1)
      IF(ANY(.NOT.(sa(1:50:2) .APPROXEQ. -0.25_SSK)) .OR. &
        ANY(.NOT.(sa(2:50:2) .APPROXEQ. 0.5_SSK))) THEN
        WRITE(*,*) 'CALL BLAS_scal(50,sb,sa,1) FAILED!'
        STOP 666
      ENDIF
      WRITE(*,*) '  Passed: CALL BLAS_scal(...)'
  !
  !Test _axpy
      da=0.5_SDK
      db=1.0_SDK
      db(200)=2.0_SDK
      CALL BLAS_axpy(2.0_SDK,da(1:199),db(1:199))
      IF(ANY(.NOT.(db .APPROXEQ. 2.0_SDK))) THEN
        WRITE(*,*) 'CALL BLAS_axpy(2.0_SDK,da(1:199),db(1:199)) FAILED!'
        STOP 666
      ENDIF
      CALL BLAS_axpy(0.0_SDK,da,db,-2)
      IF(ANY(.NOT.(db .APPROXEQ. 2.0_SDK))) THEN
        WRITE(*,*) 'CALL BLAS_axpy(0.0_SDK,da,db,-2) FAILED!'
        STOP 666
      ENDIF
      CALL BLAS_axpy(-10,5.0_SDK,da,db)
      IF(ANY(.NOT.(db .APPROXEQ. 2.0_SDK))) THEN
        WRITE(*,*) 'CALL BLAS_axpy(-10,5.0_SDK,da,db) FAILED!'
        STOP 666
      ENDIF
      CALL BLAS_axpy(5,5.0_SDK,da(1:10),db(1:10),2)
      IF(ANY(.NOT.(db(1:10:2) .APPROXEQ. 4.5_SDK)) .OR. &
        ANY(.NOT.(db(2:10:2) .APPROXEQ. 2.0_SDK))) THEN
        WRITE(*,*) 'CALL BLAS_axpy(5,5.0_SDK,da,db,2) FAILED!'
        STOP 666
      ENDIF
      CALL BLAS_axpy(10,2.0_SDK,da(1:10),1,db(1:10),1)
      IF(ANY(.NOT.(db(1:10:2) .APPROXEQ. 5.5_SDK)) .OR. &
        ANY(.NOT.(db(2:10:2) .APPROXEQ. 3.0_SDK))) THEN
        WRITE(*,*) 'CALL BLAS_axpy(5,2.0_SDK,da(1:10),2,db(1:10),1) FAILED!'
        STOP 666
      ENDIF
      da=0.5_SDK
      db=1.0_SDK
      db(200)=1.5_SDK
      CALL BLAS_axpy(da(1:199),db(1:199))
      IF(ANY(.NOT.(db .APPROXEQ. 1.5_SDK))) THEN
        WRITE(*,*) 'CALL BLAS_axpy(da(1:199),db(1:199)) FAILED!'
        STOP 666
      ENDIF
      CALL BLAS_axpy(-2,da,db)
      IF(ANY(.NOT.(db .APPROXEQ. 1.5_SDK))) THEN
        WRITE(*,*) 'CALL BLAS_axpy(-2,da,db) FAILED!'
        STOP 666
      ENDIF
      CALL BLAS_axpy(5,da(1:10),db(1:10),2)
      IF(ANY(.NOT.(db(1:10:2) .APPROXEQ. 2.0_SDK)) .OR. &
        ANY(.NOT.(db(2:10:2) .APPROXEQ. 1.5_SDK))) THEN
        WRITE(*,*) 'CALL BLAS_axpy(5,da(1:10),db(1:10),2) FAILED!'
        STOP 666
      ENDIF
      CALL BLAS_axpy(10,da(1:10),1,db(1:10),1)
      IF(ANY(.NOT.(db(1:10:2) .APPROXEQ. 2.5_SDK)) .OR. &
        ANY(.NOT.(db(2:10:2) .APPROXEQ. 2.0_SDK))) THEN
        WRITE(*,*) 'CALL BLAS_axpy(10,da(1:10),1,db(1:10),1) FAILED!'
        STOP 666
      ENDIF
      da=0.5_SDK
      db=1.0_SDK
      dalpha=2.0_SDK
      dalpha(1)=0.0_SDK
      db(200)=2.0_SDK
      CALL BLAS_axpy(dalpha(1:199),da(1:199),db(1:199))
      IF(ANY(.NOT.(db(2:200) .APPROXEQ. 2.0_SDK)) .OR. &
        .NOT.(db(1) .APPROXEQ. 1.0_SDK)) THEN
        WRITE(*,*) 'CALL BLAS_axpy(dalpha(1:199),da(1:199),db(1:199)) FAILED!'
        STOP 666
      ENDIF
      dalpha=0.0_SDK  
      CALL BLAS_axpy(dalpha,da,db,-2)
      IF(ANY(.NOT.(db(2:200) .APPROXEQ. 2.0_SDK)) .OR. &
        .NOT.(db(1) .APPROXEQ. 1.0_SDK)) THEN
        WRITE(*,*) 'CALL BLAS_axpy(dalpha,da,db,-2) FAILED!'
        STOP 666
      ENDIF
      CALL BLAS_axpy(dalpha(1:10),da,db)
      IF(ANY(.NOT.(db(2:200) .APPROXEQ. 2.0_SDK)) .OR. &
        .NOT.(db(1) .APPROXEQ. 1.0_SDK)) THEN
        WRITE(*,*) 'CALL BLAS_axpy(dalpha(1:10),da,db) FAILED!'
        STOP 666
      ENDIF  
      CALL BLAS_axpy(-10,dalpha,da,db)
      IF(ANY(.NOT.(db(2:200) .APPROXEQ. 2.0_SDK)) .OR. &
        .NOT.(db(1) .APPROXEQ. 1.0_SDK)) THEN
        WRITE(*,*) 'CALL BLAS_axpy(-10,dalpha,da,db) FAILED!'
        STOP 666
      ENDIF
      db(1)=2.0_SDK
      dalpha(1:10:2)=5.0_SDK
      CALL BLAS_axpy(5,dalpha(1:10),da(1:10),db(1:10),2)
      IF(ANY(.NOT.(db(1:10:2) .APPROXEQ. 4.5_SDK)) .OR. &
        ANY(.NOT.(db(2:10:2) .APPROXEQ. 2.0_SDK))) THEN
        WRITE(*,*) 'CALL BLAS_axpy(5,dalpha(1:10),da(1:10),db(1:10),2) FAILED!'
        STOP 666
      ENDIF
      dalpha(1:10)=2.0_SDK
      CALL BLAS_axpy(10,dalpha(1:10),da(1:10),1,db(1:10),1)
      IF(ANY(.NOT.(db(1:10:2) .APPROXEQ. 5.5_SDK)) .OR. &
        ANY(.NOT.(db(2:10:2) .APPROXEQ. 3.0_SDK))) THEN
        WRITE(*,*) 'CALL BLAS_axpy(10,dalpha(1:10),da(1:10),1,db(1:10),1) FAILED!'
        STOP 666
      ENDIF
      sa=0.5_SSK
      sb=1.0_SSK
      sb(200)=2.0_SSK
      CALL BLAS_axpy(2.0_SSK,sa(1:199),sb(1:199))
      IF(ANY(.NOT.(sb .APPROXEQ. 2.0_SSK))) THEN
        WRITE(*,*) 'CALL BLAS_axpy(2.0_SSK,sa(1:199),sb(1:199)) FAILED!'
        STOP 666
      ENDIF
      CALL BLAS_axpy(0.0_SSK,sa,sb,-2)
      IF(ANY(.NOT.(sb .APPROXEQ. 2.0_SSK))) THEN
        WRITE(*,*) 'CALL BLAS_axpy(0.0_SSK,sa,sb,-2) FAILED!'
        STOP 666
      ENDIF
      CALL BLAS_axpy(-10,5.0_SSK,sa,sb)
      IF(ANY(.NOT.(sb .APPROXEQ. 2.0_SSK))) THEN
        WRITE(*,*) 'CALL BLAS_axpy(-10,5.0_SSK,sa,sb) FAILED!'
        STOP 666
      ENDIF
      CALL BLAS_axpy(5,5.0_SSK,sa(1:10),sb(1:10),2)
      IF(ANY(.NOT.(sb(1:10:2) .APPROXEQ. 4.5_SSK)) .OR. &
        ANY(.NOT.(sb(2:10:2) .APPROXEQ. 2.0_SSK))) THEN
        WRITE(*,*) 'CALL BLAS_axpy(5,5.0_SSK,sa,sb,2) FAILED!'
        STOP 666
      ENDIF
      CALL BLAS_axpy(10,2.0_SSK,sa(1:10),1,sb(1:10),1)
      IF(ANY(.NOT.(sb(1:10:2) .APPROXEQ. 5.5_SSK)) .OR. &
        ANY(.NOT.(sb(2:10:2) .APPROXEQ. 3.0_SSK))) THEN
        WRITE(*,*) 'CALL BLAS_axpy(5,2.0_SSK,sa(1:10),2,sb(1:10),1) FAILED!'
        STOP 666
      ENDIF
      sa=0.5_SSK
      sb=1.0_SSK
      sb(200)=1.5_SSK
      CALL BLAS_axpy(sa(1:199),sb(1:199))
      IF(ANY(.NOT.(sb .APPROXEQ. 1.5_SSK))) THEN
        WRITE(*,*) 'CALL BLAS_axpy(sa(1:199),sb(1:199)) FAILED!'
        STOP 666
      ENDIF
      CALL BLAS_axpy(-2,sa,sb)
      IF(ANY(.NOT.(sb .APPROXEQ. 1.5_SSK))) THEN
        WRITE(*,*) 'CALL BLAS_axpy(-2,sa,sb) FAILED!'
        STOP 666
      ENDIF
      CALL BLAS_axpy(5,sa(1:10),sb(1:10),2)
      IF(ANY(.NOT.(sb(1:10:2) .APPROXEQ. 2.0_SSK)) .OR. &
        ANY(.NOT.(sb(2:10:2) .APPROXEQ. 1.5_SSK))) THEN
        WRITE(*,*) 'CALL BLAS_axpy(5,sa(1:10),sb(1:10),2) FAILED!'
        STOP 666
      ENDIF
      CALL BLAS_axpy(10,sa(1:10),1,sb(1:10),1)
      IF(ANY(.NOT.(sb(1:10:2) .APPROXEQ. 2.5_SSK)) .OR. &
        ANY(.NOT.(sb(2:10:2) .APPROXEQ. 2.0_SSK))) THEN
        WRITE(*,*) 'CALL BLAS_axpy(10,sa(1:10),1,sb(1:10),1) FAILED!'
        STOP 666
      ENDIF
      sa=0.5_SSK
      sb=1.0_SSK
      salpha=2.0_SSK
      salpha(1)=0.0_SSK
      sb(200)=2.0_SSK
      CALL BLAS_axpy(salpha(1:199),sa(1:199),sb(1:199))
      IF(ANY(.NOT.(sb(2:200) .APPROXEQ. 2.0_SSK)) .OR. &
        .NOT.(sb(1) .APPROXEQ. 1.0_SSK)) THEN
        WRITE(*,*) 'CALL BLAS_axpy(salpha(1:199),sa(1:199),sb(1:199)) FAILED!'
        STOP 666
      ENDIF
      salpha=0.0_SSK  
      CALL BLAS_axpy(salpha,sa,sb,-2)
      IF(ANY(.NOT.(sb(2:200) .APPROXEQ. 2.0_SSK)) .OR. &
        .NOT.(sb(1) .APPROXEQ. 1.0_SSK)) THEN
        WRITE(*,*) 'CALL BLAS_axpy(salpha,sa,sb,-2) FAILED!'
        STOP 666
      ENDIF
      CALL BLAS_axpy(salpha(1:10),sa,sb)
      IF(ANY(.NOT.(sb(2:200) .APPROXEQ. 2.0_SSK)) .OR. &
        .NOT.(sb(1) .APPROXEQ. 1.0_SSK)) THEN
        WRITE(*,*) 'CALL BLAS_axpy(salpha(1:10),sa,sb) FAILED!'
        STOP 666
      ENDIF  
      CALL BLAS_axpy(-10,salpha,sa,sb)
      IF(ANY(.NOT.(sb(2:200) .APPROXEQ. 2.0_SSK)) .OR. &
        .NOT.(sb(1) .APPROXEQ. 1.0_SSK)) THEN
        WRITE(*,*) 'CALL BLAS_axpy(-10,salpha,sa,sb) FAILED!'
        STOP 666
      ENDIF
      sb(1)=2.0_SSK
      salpha(1:10:2)=5.0_SSK
      CALL BLAS_axpy(5,salpha(1:10),sa(1:10),sb(1:10),2)
      IF(ANY(.NOT.(sb(1:10:2) .APPROXEQ. 4.5_SSK)) .OR. &
        ANY(.NOT.(sb(2:10:2) .APPROXEQ. 2.0_SSK))) THEN
        WRITE(*,*) 'CALL BLAS_axpy(5,salpha(1:10),sa(1:10),sb(1:10),2) FAILED!'
        STOP 666
      ENDIF
      salpha(1:10)=2.0_SSK
      CALL BLAS_axpy(10,salpha(1:10),sa(1:10),1,sb(1:10),1)
      IF(ANY(.NOT.(sb(1:10:2) .APPROXEQ. 5.5_SSK)) .OR. &
        ANY(.NOT.(sb(2:10:2) .APPROXEQ. 3.0_SSK))) THEN
        WRITE(*,*) 'CALL BLAS_axpy(10,salpha(1:10),sa(1:10),1,sb(1:10),1) FAILED!'
        STOP 666
      ENDIF
      WRITE(*,*) '  Passed: CALL BLAS_axpy(...)'
    ENDSUBROUTINE testBLAS1
!
!-------------------------------------------------------------------------------
    SUBROUTINE testBLAS2()
      INTEGER(SIK) :: ia(5),ja(7)
      REAL(SSK) :: sa(128,128),sx(128),sy(128),salpha,sbeta,saa(7)
      REAL(SDK) :: da(128,128),dx(128),dy(128),dalpha,dbeta,daa(7)
!
!Test sgemv interfaces
      sa=2.0_SSK
      sx=0.5_SSK
      salpha=3.0_SSK
      sy=0.25_SSK
      sbeta=4.0_SSK
    
      sx(1)=0.0_SSK
      sa(9,1)=0.0_SSK
      !sgemv_all(trans,m,n,alpha,a,lda,x,incx,beta,y,incy)
      CALL BLAS_matvec('N',4,2,salpha,sa,8,sx,2,sbeta,sy,4)
      IF(.NOT.(sy(1) .APPROXEQ. 1.0_SSK) .OR. ANY(.NOT.(sy(2:4) .APPROXEQ. 0.25_SSK)) .OR. &
        ANY(.NOT.(sy(5:13:4) .APPROXEQ. 4.0_SSK)) .OR. ANY(.NOT.(sy(6:8) .APPROXEQ. 0.25_SSK)) .OR. &
          ANY(.NOT.(sy(10:12) .APPROXEQ. 0.25_SSK)) .OR. ANY(.NOT.(sy(14:128) .APPROXEQ. 0.25_SSK))) THEN
        WRITE(*,*) "CALL BLAS_matvec('N',4,2,salpha,sa,8,sx,1,sbeta,sy,4) FAILED!"
        STOP 666
      ENDIF
      sy=0.25_SSK
      CALL BLAS_matvec('t',4,2,salpha,sa,8,sx,2,sbeta,sy,5)
      IF(ANY(.NOT.((/sy(1),sy(6)/) .APPROXEQ. 10.0_SSK)) .OR. &
        ANY(.NOT.(sy(7:) .APPROXEQ. 0.25_SSK)) .OR. &
          ANY(.NOT.(sy(2:5) .APPROXEQ. 0.25_SSK))) THEN
        WRITE(*,*) "CALL BLAS_matvec('t',4,2,salpha,sa,8,sx,2,sbeta,sy,5) FAILED!"
        STOP 666
      ENDIF
      
      sx(1)=0.5_SSK
      sa(9,1)=2.0_SSK
      sy=0.25_SSK
      !sgemv_tmnaaxby(trans,m,n,alpha,a,x,beta,y)
      CALL BLAS_matvec('T',10,128,salpha,sa,sx,0.0_SSK,sy)
      IF(ANY(.NOT.(sy .APPROXEQ. 30._SSK))) THEN
        WRITE(*,*) "CALL BLAS_matvec('T',10,128,salpha,sa,sx,0.0_SSK,sy) FAILED!"
        STOP 666
      ENDIF
      sy=0.25_SSK
      CALL BLAS_matvec('N',10,128,salpha,sa,sx,1.0_SSK,sy)
      IF(ANY(.NOT.(sy(1:10) .APPROXEQ. 384.25_SSK)) .AND. &
        ANY(.NOT.(sy(11:) .APPROXEQ. 384.25_SSK))) THEN
        WRITE(*,*) "CALL BLAS_matvec('N',10,128,salpha,sa,sx,1.0_SSK,sy) FAILED!"
        STOP 666
      ENDIF
      
      sy=0.25_SSK
      !sgemv_mnaaxby(m,n,alpha,a,x,beta,y)
      CALL BLAS_matvec(128,128,0.0_SSK,sa,sx,sbeta,sy)
      IF(ANY(.NOT.(sy .APPROXEQ. 1.0_SSK))) THEN
        WRITE(*,*) "CALL BLAS_matvec(128,128,salpha,sa,sx,sbeta,sy) FAILED!"
        STOP 666
      ENDIF
      
      !sgemv_tnaaxby(trans,n,alpha,a,x,beta,y)
      CALL BLAS_matvec('n',32,0.0_SSK,sa,sx,0.0_SSK,sy)
      IF(ANY(.NOT.(sy(1:32) .APPROXEQ. 0.0_SSK)) .OR. &
        ANY(.NOT.(sy(33:128) .APPROXEQ. 1.0_SSK))) THEN
        WRITE(*,*) "CALL BLAS_matvec('n',32,0.0_SSK,sa,sx,0.0_SSK,sy) FAILED!"
        STOP 666
      ENDIF
      
      !sgemv_naaxby(n,alpha,a,x,beta,y)
      sy=0.5_SSK
      CALL BLAS_matvec(32,0.0_SSK,sa,sx,2.0_SSK,sy)
      IF(ANY(.NOT.(sy(1:32) .APPROXEQ. 1.0_SSK)) .OR. &
        ANY(.NOT.(sy(33:128) .APPROXEQ. 0.5_SSK))) THEN
        WRITE(*,*) "CALL BLAS_matvec(32,0.0_SSK,sa,sx,2.0_SSK,sy) FAILED!"
        STOP 666
      ENDIF
      
      !sgemv_aaxby(alpha,a,x,beta,y)
      sy=3.0_SSK
      CALL BLAS_matvec(0.0_SSK,sa,sx,1.0_SSK,sy)
      IF(ANY(.NOT.(sy .APPROXEQ. 3.0_SSK))) THEN
        WRITE(*,*) "CALL BLAS_matvec(0.0_SSK,sa,sx,1.0_SSK,sy) FAILED!"
        STOP 666
      ENDIF
!
      sa=2.0_SSK
      sx=0.5_SSK
      salpha=3.0_SSK
      sy=0.25_SSK
      sbeta=4.0_SSK
    
      sx(1)=0.0_SSK
      sa(9,1)=0.0_SSK
      !sgemv_noalpha(trans,m,n,a,lda,x,incx,beta,y,incy)
      CALL BLAS_matvec('N',4,2,sa,8,sx,2,sbeta,sy,4)
      IF(.NOT.(sy(1) .APPROXEQ. 1.0_SSK) .OR. ANY(.NOT.(sy(2:4) .APPROXEQ. 0.25_SSK)) .OR. &
        ANY(.NOT.(sy(5:13:4) .APPROXEQ. 2.0_SSK)) .OR. ANY(.NOT.(sy(6:8) .APPROXEQ. 0.25_SSK)) .OR. &
          ANY(.NOT.(sy(10:12) .APPROXEQ. 0.25_SSK)) .OR. ANY(.NOT.(sy(14:128) .APPROXEQ. 0.25_SSK))) THEN
        WRITE(*,*) "CALL BLAS_matvec('N',4,2,sa,8,sx,2,sbeta,sy,4) FAILED!"
        STOP 666
      ENDIF
      sy=0.25_SSK
      CALL BLAS_matvec('t',4,2,sa,8,sx,2,sbeta,sy,5)
      IF(ANY(.NOT.((/sy(1),sy(6)/) .APPROXEQ. 4.0_SSK)) .OR. &
        ANY(.NOT.(sy(7:) .APPROXEQ. 0.25_SSK)) .OR. &
          ANY(.NOT.(sy(2:5) .APPROXEQ. 0.25_SSK))) THEN
        WRITE(*,*) "CALL BLAS_matvec('t',4,2,sa,8,sx,2,sbeta,sy,5) FAILED!"
        STOP 666
      ENDIF
      
      sx(1)=0.5_SSK
      sa(9,1)=2.0_SSK
      sy=0.25_SSK
      !sgemv_tmnaxby(trans,m,n,a,x,beta,y)
      CALL BLAS_matvec('T',10,128,sa,sx,0.0_SSK,sy)
      IF(ANY(.NOT.(sy .APPROXEQ. 10._SSK))) THEN
        WRITE(*,*) "CALL BLAS_matvec('T',10,128,sa,sx,0.0_SSK,sy) FAILED!"
        STOP 666
      ENDIF
      sy=0.25_SSK
      CALL BLAS_matvec('N',10,128,sa,sx,1.0_SSK,sy)
      IF(ANY(.NOT.(sy(1:10) .APPROXEQ. 128.25_SSK)) .AND. &
        ANY(.NOT.(sy(11:) .APPROXEQ. 128.25_SSK))) THEN
        WRITE(*,*) "CALL BLAS_matvec('N',10,128,sa,sx,1.0_SSK,sy) FAILED!"
        STOP 666
      ENDIF
      
      sy=0.25_SSK
      sx=0.0_SSK
      !sgemv_mnaxby(m,n,a,x,beta,y)
      CALL BLAS_matvec(128,128,sa,sx,sbeta,sy)
      IF(ANY(.NOT.(sy .APPROXEQ. 1.0_SSK))) THEN
        WRITE(*,*) "CALL BLAS_matvec(128,128,sa,sx,sbeta,sy) FAILED!"
        STOP 666
      ENDIF
      
      !sgemv_tnaxby(trans,n,a,x,beta,y)
      CALL BLAS_matvec('n',32,sa,sx,0.0_SSK,sy)
      IF(ANY(.NOT.(sy(1:32) .APPROXEQ. 0.0_SSK)) .OR. &
        ANY(.NOT.(sy(33:128) .APPROXEQ. 1.0_SSK))) THEN
        WRITE(*,*) "CALL BLAS_matvec('n',32,sa,sx,0.0_SSK,sy) FAILED!"
        STOP 666
      ENDIF
      
      !sgemv_naxby(n,a,x,beta,y)
      sy=0.5_SSK
      CALL BLAS_matvec(32,sa,sx,2.0_SSK,sy)
      IF(ANY(.NOT.(sy(1:32) .APPROXEQ. 1.0_SSK)) .OR. &
        ANY(.NOT.(sy(33:128) .APPROXEQ. 0.5_SSK))) THEN
        WRITE(*,*) "CALL BLAS_matvec(32,sa,sx,2.0_SSK,sy) FAILED!"
        STOP 666
      ENDIF
      
      !sgemv_axby(a,x,beta,y)
      sy=3.0_SSK
      CALL BLAS_matvec(sa,sx,1.0_SSK,sy)
      IF(ANY(.NOT.(sy .APPROXEQ. 3.0_SSK))) THEN
        WRITE(*,*) "CALL BLAS_matvec(sa,sx,1.0_SSK,sy) FAILED!"
        STOP 666
      ENDIF
      
      sa=2.0_SSK
      sx=0.5_SSK
      sy=1.0_SSK
      salpha=3.0_SSK
    
      sx(1)=0.0_SSK
      sa(9,1)=0.0_SSK
      !sgemv_nobeta(trans,m,n,alpha,a,lda,x,incx,y,incy)
      CALL BLAS_matvec('N',4,2,salpha,sa,8,sx,2,sy,4)
      IF(.NOT.(sy(1) .APPROXEQ. 1.0_SSK) .OR. ANY(.NOT.(sy(2:4) .APPROXEQ. 1.0_SSK)) .OR. &
        ANY(.NOT.(sy(5:13:4) .APPROXEQ. 4.0_SSK)) .OR. ANY(.NOT.(sy(6:8) .APPROXEQ. 1.0_SSK)) .OR. &
          ANY(.NOT.(sy(10:12) .APPROXEQ. 1.0_SSK)) .OR. ANY(.NOT.(sy(14:128) .APPROXEQ. 1.0_SSK))) THEN
        WRITE(*,*) "CALL BLAS_matvec('N',4,2,salpha,sa,8,sx,2,sy,4) FAILED!"
        STOP 666
      ENDIF
      sy=1.0_SSK
      CALL BLAS_matvec('t',4,2,salpha,sa,8,sx,2,sy,5)
      IF(ANY(.NOT.((/sy(1),sy(6)/) .APPROXEQ. 10.0_SSK)) .OR. &
        ANY(.NOT.(sy(7:) .APPROXEQ. 1.0_SSK)) .OR. &
          ANY(.NOT.(sy(2:5) .APPROXEQ. 1.0_SSK))) THEN
        WRITE(*,*) "CALL BLAS_matvec('t',4,2,salpha,sa,8,sx,2,sy,5) FAILED!"
        STOP 666
      ENDIF
      
      sx(1)=0.5_SSK
      sa(9,1)=2.0_SSK
      sy=0.0_SSK
      !sgemv_tmnaaxy(trans,m,n,alpha,a,x,y)
      CALL BLAS_matvec('T',10,128,salpha,sa,sx,sy)
      IF(ANY(.NOT.(sy .APPROXEQ. 30._SSK))) THEN
        WRITE(*,*) "CALL BLAS_matvec('T',10,128,salpha,sa,sx,sy) FAILED!"
        STOP 666
      ENDIF
      sy=0.25_SSK
      CALL BLAS_matvec('N',10,128,salpha,sa,sx,sy)
      IF(ANY(.NOT.(sy(1:10) .APPROXEQ. 384.25_SSK)) .AND. &
        ANY(.NOT.(sy(11:) .APPROXEQ. 384.25_SSK))) THEN
        WRITE(*,*) "CALL BLAS_matvec('N',10,128,salpha,sa,sx,sy) FAILED!"
        STOP 666
      ENDIF
      
      sy=1.0_SSK
      !sgemv_mnaaxy(m,n,alpha,a,x,y)
      CALL BLAS_matvec(128,128,0.0_SSK,sa,sx,sy)
      IF(ANY(.NOT.(sy .APPROXEQ. 1.0_SSK))) THEN
        WRITE(*,*) "CALL BLAS_matvec(128,128,0.0_SSK,sa,sx,sy) FAILED!"
        STOP 666
      ENDIF
      
      !sgemv_tnaaxy(trans,n,alpha,a,x,y)
      CALL BLAS_matvec('n',32,0.0_SSK,sa,sx,sy)
      IF(ANY(.NOT.(sy .APPROXEQ. 1.0_SSK))) THEN
        WRITE(*,*) "CALL BLAS_matvec('n',32,0.0_SSK,sa,sx,sy) FAILED!"
        STOP 666
      ENDIF
      
      !sgemv_naaxy(n,alpha,a,x,y)
      sy=0.5_SSK
      CALL BLAS_matvec(32,1.0_SSK,sa,sx,sy)
      IF(ANY(.NOT.(sy(1:32) .APPROXEQ. 32.5_SSK)) .OR. &
        ANY(.NOT.(sy(33:128) .APPROXEQ. 0.5_SSK))) THEN
        WRITE(*,*) "CALL BLAS_matvec(32,0.0_SSK,sa,sx,sy) FAILED!"
        STOP 666
      ENDIF
      
      !sgemv_aaxy(alpha,a,x,y)
      sy=3.0_SSK
      CALL BLAS_matvec(0.0_SSK,sa,sx,sy)
      IF(ANY(.NOT.(sy .APPROXEQ. 3.0_SSK))) THEN
        WRITE(*,*) "CALL BLAS_matvec(0.0_SSK,sa,sx,sy) FAILED!"
        STOP 666
      ENDIF
      
      sa=2.0_SSK
      sx=0.5_SSK
      sy=1.0_SSK
    
      sx(1)=0.0_SSK
      sa(9,1)=0.0_SSK
      !sgemv_noalphabeta(trans,m,n,a,lda,x,incx,y,incy)
      CALL BLAS_matvec('N',4,2,sa,8,sx,2,sy,4)
      IF(.NOT.(sy(1) .APPROXEQ. 1.0_SSK) .OR. ANY(.NOT.(sy(2:4) .APPROXEQ. 1.0_SSK)) .OR. &
        ANY(.NOT.(sy(5:13:4) .APPROXEQ. 2.0_SSK)) .OR. ANY(.NOT.(sy(6:8) .APPROXEQ. 1.0_SSK)) .OR. &
          ANY(.NOT.(sy(10:12) .APPROXEQ. 1.0_SSK)) .OR. ANY(.NOT.(sy(14:128) .APPROXEQ. 1.0_SSK))) THEN
        WRITE(*,*) "CALL BLAS_matvec('N',4,2,sa,8,sx,2,sy,4) FAILED!"
        STOP 666
      ENDIF
      sy=1.0_SSK
      CALL BLAS_matvec('t',4,2,sa,8,sx,2,sy,5)
      IF(ANY(.NOT.((/sy(1),sy(6)/) .APPROXEQ. 4.0_SSK)) .OR. &
        ANY(.NOT.(sy(7:) .APPROXEQ. 1.0_SSK)) .OR. &
          ANY(.NOT.(sy(2:5) .APPROXEQ. 1.0_SSK))) THEN
        WRITE(*,*) "CALL BLAS_matvec('t',4,2,sa,8,sx,2,sy,5) FAILED!"
        STOP 666
      ENDIF
      
      sx(1)=0.5_SSK
      sa(9,1)=2.0_SSK
      sy=0.0_SSK
      !sgemv_tmnaxy(trans,m,n,alpha,a,x,y)
      CALL BLAS_matvec('T',10,128,sa,sx,sy)
      IF(ANY(.NOT.(sy .APPROXEQ. 10._SSK))) THEN
        WRITE(*,*) "CALL BLAS_matvec('T',10,128,sa,sx,sy) FAILED!"
        STOP 666
      ENDIF
      sy=0.25_SSK
      CALL BLAS_matvec('N',10,128,sa,sx,sy)
      IF(ANY(.NOT.(sy(1:10) .APPROXEQ. 128.25_SSK)) .AND. &
        ANY(.NOT.(sy(11:) .APPROXEQ. 0.25_SSK))) THEN
        WRITE(*,*) "CALL BLAS_matvec('N',10,128,sa,sx,sy) FAILED!"
        STOP 666
      ENDIF
      
      sy=1.0_SSK
      !sgemv_mnaxy(m,n,alpha,a,x,y)
      CALL BLAS_matvec(128,128,sa,sx,sy)
      IF(ANY(.NOT.(sy .APPROXEQ. 129.0_SSK))) THEN
        WRITE(*,*) "CALL BLAS_matvec(128,128,sa,sx,sy) FAILED!"
        STOP 666
      ENDIF
      
      sy=0.5_SSK
      !sgemv_tnaxy(trans,n,alpha,a,x,y)
      CALL BLAS_matvec('n',32,sa,sx,sy)
      IF(ANY(.NOT.(sy(1:32) .APPROXEQ. 32.5_SSK)) .OR. &
        ANY(.NOT.(sy(33:128) .APPROXEQ. 0.5_SSK))) THEN
        WRITE(*,*) "CALL BLAS_matvec('n',32,sa,sx,sy) FAILED!"
        STOP 666
      ENDIF
      
      !sgemv_naxy(n,alpha,a,x,y)
      sy=0.5_SSK
      CALL BLAS_matvec(32,sa,sx,sy)
      IF(ANY(.NOT.(sy(1:32) .APPROXEQ. 32.5_SSK)) .OR. &
        ANY(.NOT.(sy(33:128) .APPROXEQ. 0.5_SSK))) THEN
        WRITE(*,*) "CALL BLAS_matvec(32,sa,sx,sy) FAILED!"
        STOP 666
      ENDIF
      
      !sgemv_axy(alpha,a,x,y)
      sy=3.0_SSK
      sx=0.0_SSK
      CALL BLAS_matvec(sa,sx,sy)
      IF(ANY(.NOT.(sy .APPROXEQ. 3.0_SSK))) THEN
        WRITE(*,*) "CALL BLAS_matvec(sa,sx,sy) FAILED!"
        STOP 666
      ENDIF
      
      sx=0.5_SSK
      !sgemv_tax(trans,a,x)
      CALL BLAS_matvec('t',sa(1:121,1:121),sx(1:121))
      IF(ANY(.NOT.(sx(1:121) .APPROXEQ. 121.0_SSK))) THEN
        WRITE(*,*) "CALL BLAS_matvec('t',sa(1:121,1:121),sx(1:121)) FAILED!"
        STOP 666
      ENDIF
      CALL BLAS_matvec('T',sa(122:128,122:128),sx(122:128))
      IF(ANY(.NOT.(sx(122:128) .APPROXEQ. 7.0_SSK))) THEN
        WRITE(*,*) "CALL BLAS_matvec('T',sa(122:128,122:128),sx(122:128)) FAILED!"
        STOP 666
      ENDIF
      
      sx=0.5_SSK
      !sgemv_ax(a,x)
      CALL BLAS_matvec(sa,sx)
      IF(ANY(.NOT.(sx .APPROXEQ. 128.0_SSK))) THEN
        WRITE(*,*) "CALL BLAS_matvec('t',sa,sx) FAILED!"
        STOP 666
      ENDIF
      WRITE(*,*) '  Passed: CALL BLAS_matvec(...) sgemv'
!
!Test dgemv interfaces
      da=2.0_SDK
      dx=0.5_SDK
      dalpha=3.0_SDK
      dy=0.25_SDK
      dbeta=4.0_SDK
    
      dx(1)=0.0_SDK
      da(9,1)=0.0_SDK
      !dgemv_all(trans,m,n,alpha,a,lda,x,incx,beta,y,incy)
      CALL BLAS_matvec('N',4,2,dalpha,da,8,dx,2,dbeta,dy,4)
      IF(.NOT.(dy(1) .APPROXEQ. 1.0_SDK) .OR. ANY(.NOT.(dy(2:4) .APPROXEQ. 0.25_SDK)) .OR. &
        ANY(.NOT.(dy(5:13:4) .APPROXEQ. 4.0_SDK)) .OR. ANY(.NOT.(dy(6:8) .APPROXEQ. 0.25_SDK)) .OR. &
          ANY(.NOT.(dy(10:12) .APPROXEQ. 0.25_SDK)) .OR. ANY(.NOT.(dy(14:128) .APPROXEQ. 0.25_SDK))) THEN
        WRITE(*,*) "CALL BLAS_matvec('N',4,2,dalpha,da,8,dx,1,dbeta,dy,4) FAILED!"
        STOP 666
      ENDIF
      dy=0.25_SDK
      CALL BLAS_matvec('t',4,2,dalpha,da,8,dx,2,dbeta,dy,5)
      IF(ANY(.NOT.((/dy(1),dy(6)/) .APPROXEQ. 10.0_SDK)) .OR. &
        ANY(.NOT.(dy(7:) .APPROXEQ. 0.25_SDK)) .OR. &
          ANY(.NOT.(dy(2:5) .APPROXEQ. 0.25_SDK))) THEN
        WRITE(*,*) "CALL BLAS_matvec('t',4,2,dalpha,da,8,dx,2,dbeta,dy,5) FAILED!"
        STOP 666
      ENDIF
      
      dx(1)=0.5_SDK
      da(9,1)=2.0_SDK
      dy=0.25_SDK
      !dgemv_tmnaaxby(trans,m,n,alpha,a,x,beta,y)
      CALL BLAS_matvec('T',10,128,dalpha,da,dx,0.0_SDK,dy)
      IF(ANY(.NOT.(dy .APPROXEQ. 30._SDK))) THEN
        WRITE(*,*) "CALL BLAS_matvec('T',10,128,dalpha,da,dx,0.0_SDK,dy) FAILED!"
        STOP 666
      ENDIF
      dy=0.25_SDK
      CALL BLAS_matvec('N',10,128,dalpha,da,dx,1.0_SDK,dy)
      IF(ANY(.NOT.(dy(1:10) .APPROXEQ. 384.25_SDK)) .AND. &
        ANY(.NOT.(dy(11:) .APPROXEQ. 384.25_SDK))) THEN
        WRITE(*,*) "CALL BLAS_matvec('N',10,128,dalpha,da,dx,1.0_SDK,dy) FAILED!"
        STOP 666
      ENDIF
      
      dy=0.25_SDK
      !dgemv_mnaaxby(m,n,alpha,a,x,beta,y)
      CALL BLAS_matvec(128,128,0.0_SDK,da,dx,dbeta,dy)
      IF(ANY(.NOT.(dy .APPROXEQ. 1.0_SDK))) THEN
        WRITE(*,*) "CALL BLAS_matvec(128,128,dalpha,da,dx,dbeta,dy) FAILED!"
        STOP 666
      ENDIF
      
      !dgemv_tnaaxby(trans,n,alpha,a,x,beta,y)
      CALL BLAS_matvec('n',32,0.0_SDK,da,dx,0.0_SDK,dy)
      IF(ANY(.NOT.(dy(1:32) .APPROXEQ. 0.0_SDK)) .OR. &
        ANY(.NOT.(dy(33:128) .APPROXEQ. 1.0_SDK))) THEN
        WRITE(*,*) "CALL BLAS_matvec('n',32,0.0_SDK,da,dx,0.0_SDK,dy) FAILED!"
        STOP 666
      ENDIF
      
      !dgemv_naaxby(n,alpha,a,x,beta,y)
      dy=0.5_SDK
      CALL BLAS_matvec(32,0.0_SDK,da,dx,2.0_SDK,dy)
      IF(ANY(.NOT.(dy(1:32) .APPROXEQ. 1.0_SDK)) .OR. &
        ANY(.NOT.(dy(33:128) .APPROXEQ. 0.5_SDK))) THEN
        WRITE(*,*) "CALL BLAS_matvec(32,0.0_SDK,da,dx,2.0_SDK,dy) FAILED!"
        STOP 666
      ENDIF
      
      !dgemv_aaxby(alpha,a,x,beta,y)
      dy=3.0_SDK
      CALL BLAS_matvec(0.0_SDK,da,dx,1.0_SDK,dy)
      IF(ANY(.NOT.(dy .APPROXEQ. 3.0_SDK))) THEN
        WRITE(*,*) "CALL BLAS_matvec(0.0_SDK,da,dx,1.0_SDK,dy) FAILED!"
        STOP 666
      ENDIF
!
      da=2.0_SDK
      dx=0.5_SDK
      dalpha=3.0_SDK
      dy=0.25_SDK
      dbeta=4.0_SDK
    
      dx(1)=0.0_SDK
      da(9,1)=0.0_SDK
      !dgemv_noalpha(trans,m,n,a,lda,x,incx,beta,y,incy)
      CALL BLAS_matvec('N',4,2,da,8,dx,2,dbeta,dy,4)
      IF(.NOT.(dy(1) .APPROXEQ. 1.0_SDK) .OR. ANY(.NOT.(dy(2:4) .APPROXEQ. 0.25_SDK)) .OR. &
        ANY(.NOT.(dy(5:13:4) .APPROXEQ. 2.0_SDK)) .OR. ANY(.NOT.(dy(6:8) .APPROXEQ. 0.25_SDK)) .OR. &
          ANY(.NOT.(dy(10:12) .APPROXEQ. 0.25_SDK)) .OR. ANY(.NOT.(dy(14:128) .APPROXEQ. 0.25_SDK))) THEN
        WRITE(*,*) "CALL BLAS_matvec('N',4,2,da,8,dx,2,dbeta,dy,4) FAILED!"
        STOP 666
      ENDIF
      dy=0.25_SDK
      CALL BLAS_matvec('t',4,2,da,8,dx,2,dbeta,dy,5)
      IF(ANY(.NOT.((/dy(1),dy(6)/) .APPROXEQ. 4.0_SDK)) .OR. &
        ANY(.NOT.(dy(7:) .APPROXEQ. 0.25_SDK)) .OR. &
          ANY(.NOT.(dy(2:5) .APPROXEQ. 0.25_SDK))) THEN
        WRITE(*,*) "CALL BLAS_matvec('t',4,2,da,8,dx,2,dbeta,dy,5) FAILED!"
        STOP 666
      ENDIF
      
      dx(1)=0.5_SDK
      da(9,1)=2.0_SDK
      dy=0.25_SDK
      !dgemv_tmnaxby(trans,m,n,a,x,beta,y)
      CALL BLAS_matvec('T',10,128,da,dx,0.0_SDK,dy)
      IF(ANY(.NOT.(dy .APPROXEQ. 10._SDK))) THEN
        WRITE(*,*) "CALL BLAS_matvec('T',10,128,da,dx,0.0_SDK,dy) FAILED!"
        STOP 666
      ENDIF
      dy=0.25_SDK
      CALL BLAS_matvec('N',10,128,da,dx,1.0_SDK,dy)
      IF(ANY(.NOT.(dy(1:10) .APPROXEQ. 128.25_SDK)) .AND. &
        ANY(.NOT.(dy(11:) .APPROXEQ. 128.25_SDK))) THEN
        WRITE(*,*) "CALL BLAS_matvec('N',10,128,da,dx,1.0_SDK,dy) FAILED!"
        STOP 666
      ENDIF
      
      dy=0.25_SDK
      dx=0.0_SDK
      !dgemv_mnaxby(m,n,a,x,beta,y)
      CALL BLAS_matvec(128,128,da,dx,dbeta,dy)
      IF(ANY(.NOT.(dy .APPROXEQ. 1.0_SDK))) THEN
        WRITE(*,*) "CALL BLAS_matvec(128,128,da,dx,dbeta,dy) FAILED!"
        STOP 666
      ENDIF
      
      !dgemv_tnaxby(trans,n,a,x,beta,y)
      CALL BLAS_matvec('n',32,da,dx,0.0_SDK,dy)
      IF(ANY(.NOT.(dy(1:32) .APPROXEQ. 0.0_SDK)) .OR. &
        ANY(.NOT.(dy(33:128) .APPROXEQ. 1.0_SDK))) THEN
        WRITE(*,*) "CALL BLAS_matvec('n',32,da,dx,0.0_SDK,dy) FAILED!"
        STOP 666
      ENDIF
      
      !dgemv_naxby(n,a,x,beta,y)
      dy=0.5_SDK
      CALL BLAS_matvec(32,da,dx,2.0_SDK,dy)
      IF(ANY(.NOT.(dy(1:32) .APPROXEQ. 1.0_SDK)) .OR. &
        ANY(.NOT.(dy(33:128) .APPROXEQ. 0.5_SDK))) THEN
        WRITE(*,*) "CALL BLAS_matvec(32,da,dx,2.0_SDK,dy) FAILED!"
        STOP 666
      ENDIF
      
      !dgemv_axby(a,x,beta,y)
      dy=3.0_SDK
      CALL BLAS_matvec(da,dx,1.0_SDK,dy)
      IF(ANY(.NOT.(dy .APPROXEQ. 3.0_SDK))) THEN
        WRITE(*,*) "CALL BLAS_matvec(da,dx,1.0_SDK,dy) FAILED!"
        STOP 666
      ENDIF
      
      da=2.0_SDK
      dx=0.5_SDK
      dy=1.0_SDK
      dalpha=3.0_SDK
    
      dx(1)=0.0_SDK
      da(9,1)=0.0_SDK
      !dgemv_nobeta(trans,m,n,alpha,a,lda,x,incx,y,incy)
      CALL BLAS_matvec('N',4,2,dalpha,da,8,dx,2,dy,4)
      IF(.NOT.(dy(1) .APPROXEQ. 1.0_SDK) .OR. ANY(.NOT.(dy(2:4) .APPROXEQ. 1.0_SDK)) .OR. &
        ANY(.NOT.(dy(5:13:4) .APPROXEQ. 4.0_SDK)) .OR. ANY(.NOT.(dy(6:8) .APPROXEQ. 1.0_SDK)) .OR. &
          ANY(.NOT.(dy(10:12) .APPROXEQ. 1.0_SDK)) .OR. ANY(.NOT.(dy(14:128) .APPROXEQ. 1.0_SDK))) THEN
        WRITE(*,*) "CALL BLAS_matvec('N',4,2,dalpha,da,8,dx,2,dy,4) FAILED!"
        STOP 666
      ENDIF
      dy=1.0_SDK
      CALL BLAS_matvec('t',4,2,dalpha,da,8,dx,2,dy,5)
      IF(ANY(.NOT.((/dy(1),dy(6)/) .APPROXEQ. 10.0_SDK)) .OR. &
        ANY(.NOT.(dy(7:) .APPROXEQ. 1.0_SDK)) .OR. &
          ANY(.NOT.(dy(2:5) .APPROXEQ. 1.0_SDK))) THEN
        WRITE(*,*) "CALL BLAS_matvec('t',4,2,dalpha,da,8,dx,2,dy,5) FAILED!"
        STOP 666
      ENDIF
      
      dx(1)=0.5_SDK
      da(9,1)=2.0_SDK
      dy=0.0_SDK
      !dgemv_tmnaaxy(trans,m,n,alpha,a,x,y)
      CALL BLAS_matvec('T',10,128,dalpha,da,dx,dy)
      IF(ANY(.NOT.(dy .APPROXEQ. 30._SDK))) THEN
        WRITE(*,*) "CALL BLAS_matvec('T',10,128,dalpha,da,dx,dy) FAILED!"
        STOP 666
      ENDIF
      dy=0.25_SDK
      CALL BLAS_matvec('N',10,128,dalpha,da,dx,dy)
      IF(ANY(.NOT.(dy(1:10) .APPROXEQ. 384.25_SDK)) .AND. &
        ANY(.NOT.(dy(11:) .APPROXEQ. 384.25_SDK))) THEN
        WRITE(*,*) "CALL BLAS_matvec('N',10,128,dalpha,da,dx,dy) FAILED!"
        STOP 666
      ENDIF
      
      dy=1.0_SDK
      !dgemv_mnaaxy(m,n,alpha,a,x,y)
      CALL BLAS_matvec(128,128,0.0_SDK,da,dx,dy)
      IF(ANY(.NOT.(dy .APPROXEQ. 1.0_SDK))) THEN
        WRITE(*,*) "CALL BLAS_matvec(128,128,0.0_SDK,da,dx,dy) FAILED!"
        STOP 666
      ENDIF
      
      !dgemv_tnaaxy(trans,n,alpha,a,x,y)
      CALL BLAS_matvec('n',32,0.0_SDK,da,dx,dy)
      IF(ANY(.NOT.(dy .APPROXEQ. 1.0_SDK))) THEN
        WRITE(*,*) "CALL BLAS_matvec('n',32,0.0_SDK,da,dx,dy) FAILED!"
        STOP 666
      ENDIF
      
      !dgemv_naaxy(n,alpha,a,x,y)
      dy=0.5_SDK
      CALL BLAS_matvec(32,1.0_SDK,da,dx,dy)
      IF(ANY(.NOT.(dy(1:32) .APPROXEQ. 32.5_SDK)) .OR. &
        ANY(.NOT.(dy(33:128) .APPROXEQ. 0.5_SDK))) THEN
        WRITE(*,*) "CALL BLAS_matvec(32,0.0_SDK,da,dx,dy) FAILED!"
        STOP 666
      ENDIF
      
      !dgemv_aaxy(alpha,a,x,y)
      dy=3.0_SDK
      CALL BLAS_matvec(0.0_SDK,da,dx,dy)
      IF(ANY(.NOT.(dy .APPROXEQ. 3.0_SDK))) THEN
        WRITE(*,*) "CALL BLAS_matvec(0.0_SDK,da,dx,dy) FAILED!"
        STOP 666
      ENDIF
      
      da=2.0_SDK
      dx=0.5_SDK
      dy=1.0_SDK
    
      dx(1)=0.0_SDK
      da(9,1)=0.0_SDK
      !dgemv_noalphabeta(trans,m,n,a,lda,x,incx,y,incy)
      CALL BLAS_matvec('N',4,2,da,8,dx,2,dy,4)
      IF(.NOT.(dy(1) .APPROXEQ. 1.0_SDK) .OR. ANY(.NOT.(dy(2:4) .APPROXEQ. 1.0_SDK)) .OR. &
        ANY(.NOT.(dy(5:13:4) .APPROXEQ. 2.0_SDK)) .OR. ANY(.NOT.(dy(6:8) .APPROXEQ. 1.0_SDK)) .OR. &
          ANY(.NOT.(dy(10:12) .APPROXEQ. 1.0_SDK)) .OR. ANY(.NOT.(dy(14:128) .APPROXEQ. 1.0_SDK))) THEN
        WRITE(*,*) "CALL BLAS_matvec('N',4,2,da,8,dx,2,dy,4) FAILED!"
        STOP 666
      ENDIF
      dy=1.0_SDK
      CALL BLAS_matvec('t',4,2,da,8,dx,2,dy,5)
      IF(ANY(.NOT.((/dy(1),dy(6)/) .APPROXEQ. 4.0_SDK)) .OR. &
        ANY(.NOT.(dy(7:) .APPROXEQ. 1.0_SDK)) .OR. &
          ANY(.NOT.(dy(2:5) .APPROXEQ. 1.0_SDK))) THEN
        WRITE(*,*) "CALL BLAS_matvec('t',4,2,da,8,dx,2,dy,5) FAILED!"
        STOP 666
      ENDIF
      
      dx(1)=0.5_SDK
      da(9,1)=2.0_SDK
      dy=0.0_SDK
      !dgemv_tmnaxy(trans,m,n,alpha,a,x,y)
      CALL BLAS_matvec('T',10,128,da,dx,dy)
      IF(ANY(.NOT.(dy .APPROXEQ. 10._SDK))) THEN
        WRITE(*,*) "CALL BLAS_matvec('T',10,128,da,dx,dy) FAILED!"
        STOP 666
      ENDIF
      dy=0.25_SDK
      CALL BLAS_matvec('N',10,128,da,dx,dy)
      IF(ANY(.NOT.(dy(1:10) .APPROXEQ. 128.25_SDK)) .AND. &
        ANY(.NOT.(dy(11:) .APPROXEQ. 0.25_SDK))) THEN
        WRITE(*,*) "CALL BLAS_matvec('N',10,128,da,dx,dy) FAILED!"
        STOP 666
      ENDIF
      
      dy=1.0_SDK
      !dgemv_mnaxy(m,n,alpha,a,x,y)
      CALL BLAS_matvec(128,128,da,dx,dy)
      IF(ANY(.NOT.(dy .APPROXEQ. 129.0_SDK))) THEN
        WRITE(*,*) "CALL BLAS_matvec(128,128,da,dx,dy) FAILED!"
        STOP 666
      ENDIF
      
      dy=0.5_SDK
      !dgemv_tnaxy(trans,n,alpha,a,x,y)
      CALL BLAS_matvec('n',32,da,dx,dy)
      IF(ANY(.NOT.(dy(1:32) .APPROXEQ. 32.5_SDK)) .OR. &
        ANY(.NOT.(dy(33:128) .APPROXEQ. 0.5_SDK))) THEN
        WRITE(*,*) "CALL BLAS_matvec('n',32,da,dx,dy) FAILED!"
        STOP 666
      ENDIF
      
      !dgemv_naxy(n,alpha,a,x,y)
      dy=0.5_SDK
      CALL BLAS_matvec(32,da,dx,dy)
      IF(ANY(.NOT.(dy(1:32) .APPROXEQ. 32.5_SDK)) .OR. &
        ANY(.NOT.(dy(33:128) .APPROXEQ. 0.5_SDK))) THEN
        WRITE(*,*) "CALL BLAS_matvec(32,da,dx,dy) FAILED!"
        STOP 666
      ENDIF
      
      !dgemv_axy(alpha,a,x,y)
      dy=3.0_SDK
      dx=0.0_SDK
      CALL BLAS_matvec(da,dx,dy)
      IF(ANY(.NOT.(dy .APPROXEQ. 3.0_SDK))) THEN
        WRITE(*,*) "CALL BLAS_matvec(da,dx,dy) FAILED!"
        STOP 666
      ENDIF
      
      dx=0.5_SDK
      !dgemv_tax(trans,a,x)
      CALL BLAS_matvec('t',da(1:121,1:121),dx(1:121))
      IF(ANY(.NOT.(dx(1:121) .APPROXEQ. 121.0_SDK))) THEN
        WRITE(*,*) "CALL BLAS_matvec('t',da(1:121,1:121),dx(1:121)) FAILED!"
        STOP 666
      ENDIF
      CALL BLAS_matvec('T',da(122:128,122:128),dx(122:128))
      IF(ANY(.NOT.(dx(122:128) .APPROXEQ. 7.0_SDK))) THEN
        WRITE(*,*) "CALL BLAS_matvec('T',da(122:128,122:128),dx(122:128)) FAILED!"
        STOP 666
      ENDIF
      
      dx=0.5_SDK
      !dgemv_ax(a,x)
      CALL BLAS_matvec(da,dx)
      IF(ANY(.NOT.(dx .APPROXEQ. 128.0_SDK))) THEN
        WRITE(*,*) "CALL BLAS_matvec('t',da,dx) FAILED!"
        STOP 666
      ENDIF
      WRITE(*,*) '  Passed: CALL BLAS_matvec(...) dgemv'
!
!Test CSR matvec routines
      ia=(/1,3,4,6,8/)
      ja=(/1,3,2,3,4,1,4/)
      saa=(/1._SSK,6._SSK,2._SSK,3._SSK,5._SSK,7._SSK,4._SSK/)
      salpha=0.5_SSK
      sbeta=2._SSK
      sx=2.0_SSK
      sy=1.5_SSK
      
      !scsrmv_all
      CALL BLAS_matvec(-4,7,ia,ja,saa,salpha,sx,sbeta,sy)
      IF(ANY(sy /= 1.5_SSK)) THEN
        WRITE(*,*) "CALL BLAS_matvec(-4,7,ia,ja,saa,salpha,sx,sbeta,sy) FAILED!"
        STOP 666
      ENDIF
      sy=23.5_SSK
      CALL BLAS_matvec(4,2,ia,ja,saa,salpha,sx,sbeta,sy)
      IF(ANY(sy /= 23.5_SSK)) THEN
        WRITE(*,*) "CALL BLAS_matvec(4,2,ia,ja,saa,salpha,sx,sbeta,sy) FAILED!"
        STOP 666
      ENDIF
      sy=0.5_SSK
      CALL BLAS_matvec(4,7,ia,ja,saa,0.0_SSK,sx,1.0_SSK,sy)
      IF(ANY(sy /= 0.5_SSK)) THEN
        WRITE(*,*) "CALL BLAS_matvec(4,7,ia,ja,saa,0.0_SSK,sx,1.0_SSK,sy) FAILED!"
        STOP 666
      ENDIF
      CALL BLAS_matvec(4,7,ia,ja,saa,salpha,sx,sbeta,sy)
      IF(ANY(.NOT.(sy(1:4) .APPROXEQ. (/8._SSK,3._SSK,9._SSK,12._SSK/))) .OR. &
        ANY(sy(5:128) /= 0.5_SSK)) THEN
        WRITE(*,*) "CALL BLAS_matvec(4,7,ia,ja,saa,salpha,sx,sbeta,sy) FAILED!"
        STOP 666
      ENDIF
      
      !scsrmv_noNNZ
      sy=0.5_SSK
      CALL BLAS_matvec(4,ia,ja(1:2),saa,salpha,sx,sbeta,sy)
      IF(ANY(sy /= 0.5_SSK)) THEN
        WRITE(*,*) "CALL BLAS_matvec(4,ia,ja(1:2),saa,salpha,sx,sbeta,sy) FAILED!"
        STOP 666
      ENDIF
      CALL BLAS_matvec(4,ia,ja,saa,salpha,sx,sbeta,sy)
      IF(ANY(.NOT.(sy(1:4) .APPROXEQ. (/8._SSK,3._SSK,9._SSK,12._SSK/))) .OR. &
        ANY(sy(5:128) /= 0.5_SSK)) THEN
        WRITE(*,*) "CALL BLAS_matvec(4,ia,ja,saa,salpha,sx,sbeta,sy) FAILED!"
        STOP 666
      ENDIF
      
      !scsrmv_noNNNZ
      sy=1.0_SSK
      CALL BLAS_matvec(ia,ja(1:2),saa,salpha,sx(1:4),sbeta,sy(1:4))
      IF(ANY(sy /= 1.0_SSK)) THEN
        WRITE(*,*) "CALL BLAS_matvec(ia,ja(1:2),saa,salpha,sx(1:4),sbeta,sy(1:4)) FAILED!"
        STOP 666
      ENDIF
      CALL BLAS_matvec(ia,ja,saa,salpha,sx,sbeta,sy(1:4))
      IF(ANY(sy /= 1.0_SSK)) THEN
        WRITE(*,*) "CALL BLAS_matvec(ia,ja,saa,salpha,sx,sbeta,sy(1:4)) FAILED!"
        STOP 666
      ENDIF
      CALL BLAS_matvec(ia,ja,saa,salpha,sx,sbeta,sy)
      IF(ANY(sy /= 1.0_SSK)) THEN
        WRITE(*,*) "CALL BLAS_matvec(ia,ja,saa,salpha,sx,sbeta,sy) FAILED!"
        STOP 666
      ENDIF
      CALL BLAS_matvec(ia,ja,saa,salpha,sx(1:4),1.0_SSK,sy(1:4))
      IF(ANY(.NOT.(sy(1:4) .APPROXEQ. (/8._SSK,3._SSK,9._SSK,12._SSK/))) .OR. &
        ANY(sy(5:128) /= 1.0_SSK)) THEN
        WRITE(*,*) "CALL BLAS_matvec(ia,ja,saa,salpha,sx(1:4),sbeta,sy(1:4)) FAILED!"
        STOP 666
      ENDIF
      
      !scsrmv_noAlpha
      sy=0.5_SSK
      sx=1.0_SSK
      CALL BLAS_matvec(0,7,ia,ja,saa,sx,sbeta,sy)
      IF(ANY(sy /= 0.5_SSK)) THEN
        WRITE(*,*) "CALL BLAS_matvec(0,7,ia,ja,saa,sx,sbeta,sy) FAILED!"
        STOP 666
      ENDIF
      CALL BLAS_matvec(4,2,ia,ja,saa,sx,sbeta,sy)
      IF(ANY(sy /= 0.5_SSK)) THEN
        WRITE(*,*) "CALL BLAS_matvec(4,2,ia,ja,saa,sx,sbeta,sy) FAILED!"
        STOP 666
      ENDIF
      CALL BLAS_matvec(4,7,ia,ja,saa,sx,sbeta,sy)
      IF(ANY(.NOT.(sy(1:4) .APPROXEQ. (/8._SSK,3._SSK,9._SSK,12._SSK/))) .OR. &
        ANY(sy(5:128) /= 0.5_SSK)) THEN
        WRITE(*,*) "CALL BLAS_matvec(4,7,ia,ja,saa,sx,sbeta,sy) FAILED!"
        STOP 666
      ENDIF
      
      !scsrmv_noAlphaNNZ
      sy=0.5_SSK
      CALL BLAS_matvec(4,ia,ja(1:2),saa,sx,sbeta,sy)
      IF(ANY(sy /= 0.5_SSK)) THEN
        WRITE(*,*) "CALL BLAS_matvec(4,ia,ja(1:2),saa,sx,sbeta,sy) FAILED!"
        STOP 666
      ENDIF
      CALL BLAS_matvec(4,ia,ja,saa,sx,sbeta,sy)
      IF(ANY(.NOT.(sy(1:4) .APPROXEQ. (/8._SSK,3._SSK,9._SSK,12._SSK/))) .OR. &
        ANY(sy(5:128) /= 0.5_SSK)) THEN
        WRITE(*,*) "CALL BLAS_matvec(4,ia,ja,saa,sx,sbeta,sy) FAILED!"
        STOP 666
      ENDIF
      
      !scsrmv_noAlphaNNNZ
      sy=1.0_SSK
      CALL BLAS_matvec(ia,ja(1:2),saa,sx(1:4),sbeta,sy(1:4))
      IF(ANY(sy /= 1.0_SSK)) THEN
        WRITE(*,*) "CALL BLAS_matvec(ia,ja(1:2),saa,sx(1:4),sbeta,sy(1:4)) FAILED!"
        STOP 666
      ENDIF
      CALL BLAS_matvec(ia,ja,saa,sx,sbeta,sy(1:4))
      IF(ANY(sy /= 1.0_SSK)) THEN
        WRITE(*,*) "CALL BLAS_matvec(ia,ja,saa,sx,sbeta,sy(1:4)) FAILED!"
        STOP 666
      ENDIF
      CALL BLAS_matvec(ia,ja,saa,sx,sbeta,sy)
      IF(ANY(sy /= 1.0_SSK)) THEN
        WRITE(*,*) "CALL BLAS_matvec(ia,ja,saa,sx,sbeta,sy) FAILED!"
        STOP 666
      ENDIF
      CALL BLAS_matvec(ia,ja,saa,sx(1:4),1.0_SSK,sy(1:4))
      IF(ANY(.NOT.(sy(1:4) .APPROXEQ. (/8._SSK,3._SSK,9._SSK,12._SSK/))) .OR. &
        ANY(sy(5:128) /= 1.0_SSK)) THEN
        WRITE(*,*) "CALL BLAS_matvec(ia,ja,saa,sx(1:4),1.0_SSK,sy(1:4)) FAILED!"
        STOP 666
      ENDIF
      
      !scsrmv_noBeta
      sy=1.5_SSK
      sx=2.0_SSK
      CALL BLAS_matvec(-4,7,ia,ja,saa,salpha,sx,sy)
      IF(ANY(sy /= 1.5_SSK)) THEN
        WRITE(*,*) "CALL BLAS_matvec(-4,7,ia,ja,saa,salpha,sx,sy) FAILED!"
        STOP 666
      ENDIF
      sy=23.5_SSK
      CALL BLAS_matvec(4,2,ia,ja,saa,salpha,sx,sy)
      IF(ANY(sy /= 23.5_SSK)) THEN
        WRITE(*,*) "CALL BLAS_matvec(4,2,ia,ja,saa,salpha,sx,sy) FAILED!"
        STOP 666
      ENDIF
      sy=1.0_SSK
      CALL BLAS_matvec(4,7,ia,ja,saa,0.0_SSK,sx,sy)
      IF(ANY(sy /= 1.0_SSK)) THEN
        WRITE(*,*) "CALL BLAS_matvec(4,7,ia,ja,saa,0.0_SSK,sx,sy) FAILED!"
        STOP 666
      ENDIF
      CALL BLAS_matvec(4,7,ia,ja,saa,salpha,sx,sy)
      IF(ANY(.NOT.(sy(1:4) .APPROXEQ. (/8._SSK,3._SSK,9._SSK,12._SSK/))) .OR. &
        ANY(sy(5:128) /= 1.0_SSK)) THEN
        WRITE(*,*) "CALL BLAS_matvec(4,7,ia,ja,saa,salpha,sx,sy) FAILED!"
        STOP 666
      ENDIF
      
      !scsrmv_noBetaNNZ
      sy=1.0_SSK
      CALL BLAS_matvec(4,ia,ja(1:2),saa,salpha,sx,sy)
      IF(ANY(sy /= 1.0_SSK)) THEN
        WRITE(*,*) "CALL BLAS_matvec(4,ia,ja(1:2),saa,salpha,sx,sy) FAILED!"
        STOP 666
      ENDIF
      CALL BLAS_matvec(4,ia,ja,saa,salpha,sx,sy)
      IF(ANY(.NOT.(sy(1:4) .APPROXEQ. (/8._SSK,3._SSK,9._SSK,12._SSK/))) .OR. &
        ANY(sy(5:128) /= 1.0_SSK)) THEN
        WRITE(*,*) "CALL BLAS_matvec(4,ia,ja,saa,salpha,sx,sy) FAILED!"
        STOP 666
      ENDIF
      
      !scsrmv_noBetaNNNZ
      sy=1.0_SSK
      CALL BLAS_matvec(ia,ja(1:2),saa,salpha,sx(1:4),sy(1:4))
      IF(ANY(sy /= 1.0_SSK)) THEN
        WRITE(*,*) "CALL BLAS_matvec(ia,ja(1:2),saa,salpha,sx(1:4),sy(1:4)) FAILED!"
        STOP 666
      ENDIF
      CALL BLAS_matvec(ia,ja,saa,salpha,sx,sy(1:4))
      IF(ANY(sy /= 1.0_SSK)) THEN
        WRITE(*,*) "CALL BLAS_matvec(ia,ja,saa,salpha,sx,sy(1:4)) FAILED!"
        STOP 666
      ENDIF
      CALL BLAS_matvec(ia,ja,saa,salpha,sx,sy)
      IF(ANY(sy /= 1.0_SSK)) THEN
        WRITE(*,*) "CALL BLAS_matvec(ia,ja,saa,salpha,sx,sy) FAILED!"
        STOP 666
      ENDIF
      CALL BLAS_matvec(ia,ja,saa,salpha,sx(1:4),sy(1:4))
      IF(ANY(.NOT.(sy(1:4) .APPROXEQ. (/8._SSK,3._SSK,9._SSK,12._SSK/))) .OR. &
        ANY(sy(5:128) /= 1.0_SSK)) THEN
        WRITE(*,*) "CALL BLAS_matvec(ia,ja,saa,salpha,sx(1:4),sy(1:4)) FAILED!"
        STOP 666
      ENDIF
      
      !scsrmv_noAlphaBeta
      sy=1.5_SSK
      sx=1.0_SSK
      CALL BLAS_matvec(-4,7,ia,ja,saa,sx,sy)
      IF(ANY(sy /= 1.5_SSK)) THEN
        WRITE(*,*) "CALL BLAS_matvec(-4,7,ia,ja,saa,sx,sy) FAILED!"
        STOP 666
      ENDIF
      sy=23.5_SSK
      CALL BLAS_matvec(4,2,ia,ja,saa,sx,sy)
      IF(ANY(sy /= 23.5_SSK)) THEN
        WRITE(*,*) "CALL BLAS_matvec(4,2,ia,ja,saa,sx,sy) FAILED!"
        STOP 666
      ENDIF
      sy=1.0_SSK
      CALL BLAS_matvec(4,7,ia,ja,saa,sx,sy)
      IF(ANY(.NOT.(sy(1:4) .APPROXEQ. (/8._SSK,3._SSK,9._SSK,12._SSK/))) .OR. &
        ANY(sy(5:128) /= 1.0_SSK)) THEN
        WRITE(*,*) "CALL BLAS_matvec(4,7,ia,ja,saa,sx,sy) FAILED!"
        STOP 666
      ENDIF
      
      !scsrmv_noAlphaBetaNNZ
      sy=1.0_SSK
      CALL BLAS_matvec(4,ia,ja(1:2),saa,sx,sy)
      IF(ANY(sy /= 1.0_SSK)) THEN
        WRITE(*,*) "CALL BLAS_matvec(4,ia,ja(1:2),saa,sx,sy) FAILED!"
        STOP 666
      ENDIF
      CALL BLAS_matvec(4,ia,ja,saa,sx,sy)
      IF(ANY(.NOT.(sy(1:4) .APPROXEQ. (/8._SSK,3._SSK,9._SSK,12._SSK/))) .OR. &
        ANY(sy(5:128) /= 1.0_SSK)) THEN
        WRITE(*,*) "CALL BLAS_matvec(4,ia,ja,saa,sx,sy) FAILED!"
        STOP 666
      ENDIF
      
      !scsrmv_noAlphaBetaNNNZ
      sy=1.0_SSK
      CALL BLAS_matvec(ia,ja(1:2),saa,sx(1:4),sy(1:4))
      IF(ANY(sy /= 1.0_SSK)) THEN
        WRITE(*,*) "CALL BLAS_matvec(ia,ja(1:2),saa,sx(1:4),sy(1:4)) FAILED!"
        STOP 666
      ENDIF
      CALL BLAS_matvec(ia,ja,saa,sx,sy(1:4))
      IF(ANY(sy /= 1.0_SSK)) THEN
        WRITE(*,*) "CALL BLAS_matvec(ia,ja,saa,sx,sy(1:4)) FAILED!"
        STOP 666
      ENDIF
      CALL BLAS_matvec(ia,ja,saa,sx,sy)
      IF(ANY(sy /= 1.0_SSK)) THEN
        WRITE(*,*) "CALL BLAS_matvec(ia,ja,saa,sx,sy) FAILED!"
        STOP 666
      ENDIF
      CALL BLAS_matvec(ia,ja,saa,sx(1:4),sy(1:4))
      IF(ANY(.NOT.(sy(1:4) .APPROXEQ. (/8._SSK,3._SSK,9._SSK,12._SSK/))) .OR. &
        ANY(sy(5:128) /= 1.0_SSK)) THEN
        WRITE(*,*) "CALL BLAS_matvec(ia,ja,saa,sx(1:4),sy(1:4)) FAILED!"
        STOP 666
      ENDIF
      
      ia=(/1,3,4,6,8/)
      ja=(/1,3,2,3,4,1,4/)
      daa=(/1._SDK,6._SDK,2._SDK,3._SDK,5._SDK,7._SDK,4._SDK/)
      dalpha=0.5_SDK
      dbeta=2._SDK
      dx=2.0_SDK
      dy=1.5_SDK
      
      !dcsrmv_all
      CALL BLAS_matvec(-4,7,ia,ja,daa,dalpha,dx,dbeta,dy)
      IF(ANY(dy /= 1.5_SDK)) THEN
        WRITE(*,*) "CALL BLAS_matvec(-4,7,ia,ja,daa,dalpha,dx,dbeta,dy) FAILED!"
        STOP 666
      ENDIF
      dy=23.5_SDK
      CALL BLAS_matvec(4,2,ia,ja,daa,dalpha,dx,dbeta,dy)
      IF(ANY(dy /= 23.5_SDK)) THEN
        WRITE(*,*) "CALL BLAS_matvec(4,2,ia,ja,daa,dalpha,dx,dbeta,dy) FAILED!"
        STOP 666
      ENDIF
      dy=0.5_SDK
      CALL BLAS_matvec(4,7,ia,ja,daa,0.0_SDK,dx,1.0_SDK,dy)
      IF(ANY(dy /= 0.5_SDK)) THEN
        WRITE(*,*) "CALL BLAS_matvec(4,7,ia,ja,daa,0.0_SDK,dx,1.0_SDK,dy) FAILED!"
        STOP 666
      ENDIF
      CALL BLAS_matvec(4,7,ia,ja,daa,dalpha,dx,dbeta,dy)
      IF(ANY(.NOT.(dy(1:4) .APPROXEQ. (/8._SDK,3._SDK,9._SDK,12._SDK/))) .OR. &
        ANY(dy(5:128) /= 0.5_SDK)) THEN
        WRITE(*,*) "CALL BLAS_matvec(4,7,ia,ja,daa,dalpha,dx,dbeta,dy) FAILED!"
        STOP 666
      ENDIF
      
      !dcsrmv_noNNZ
      dy=0.5_SDK
      CALL BLAS_matvec(4,ia,ja(1:2),daa,dalpha,dx,dbeta,dy)
      IF(ANY(dy /= 0.5_SDK)) THEN
        WRITE(*,*) "CALL BLAS_matvec(4,ia,ja(1:2),daa,dalpha,dx,dbeta,dy) FAILED!"
        STOP 666
      ENDIF
      CALL BLAS_matvec(4,ia,ja,daa,dalpha,dx,dbeta,dy)
      IF(ANY(.NOT.(dy(1:4) .APPROXEQ. (/8._SDK,3._SDK,9._SDK,12._SDK/))) .OR. &
        ANY(dy(5:128) /= 0.5_SDK)) THEN
        WRITE(*,*) "CALL BLAS_matvec(4,ia,ja,daa,dalpha,dx,dbeta,dy) FAILED!"
        STOP 666
      ENDIF
      
      !dcsrmv_noNNNZ
      dy=1.0_SDK
      CALL BLAS_matvec(ia,ja(1:2),daa,dalpha,dx(1:4),dbeta,dy(1:4))
      IF(ANY(dy /= 1.0_SDK)) THEN
        WRITE(*,*) "CALL BLAS_matvec(ia,ja(1:2),daa,dalpha,dx(1:4),dbeta,dy(1:4)) FAILED!"
        STOP 666
      ENDIF
      CALL BLAS_matvec(ia,ja,daa,dalpha,dx,dbeta,dy(1:4))
      IF(ANY(dy /= 1.0_SDK)) THEN
        WRITE(*,*) "CALL BLAS_matvec(ia,ja,daa,dalpha,dx,dbeta,dy(1:4)) FAILED!"
        STOP 666
      ENDIF
      CALL BLAS_matvec(ia,ja,daa,dalpha,dx,dbeta,dy)
      IF(ANY(dy /= 1.0_SDK)) THEN
        WRITE(*,*) "CALL BLAS_matvec(ia,ja,daa,dalpha,dx,dbeta,dy) FAILED!"
        STOP 666
      ENDIF
      CALL BLAS_matvec(ia,ja,daa,dalpha,dx(1:4),1.0_SDK,dy(1:4))
      IF(ANY(.NOT.(dy(1:4) .APPROXEQ. (/8._SDK,3._SDK,9._SDK,12._SDK/))) .OR. &
        ANY(dy(5:128) /= 1.0_SDK)) THEN
        WRITE(*,*) "CALL BLAS_matvec(ia,ja,daa,dalpha,dx(1:4),dbeta,dy(1:4)) FAILED!"
        STOP 666
      ENDIF
      
      !dcsrmv_noAlpha
      dy=0.5_SDK
      dx=1.0_SDK
      CALL BLAS_matvec(0,7,ia,ja,daa,dx,dbeta,dy)
      IF(ANY(dy /= 0.5_SDK)) THEN
        WRITE(*,*) "CALL BLAS_matvec(0,7,ia,ja,daa,dx,dbeta,dy) FAILED!"
        STOP 666
      ENDIF
      CALL BLAS_matvec(4,2,ia,ja,daa,dx,dbeta,dy)
      IF(ANY(dy /= 0.5_SDK)) THEN
        WRITE(*,*) "CALL BLAS_matvec(4,2,ia,ja,daa,dx,dbeta,dy) FAILED!"
        STOP 666
      ENDIF
      CALL BLAS_matvec(4,7,ia,ja,daa,dx,dbeta,dy)
      IF(ANY(.NOT.(dy(1:4) .APPROXEQ. (/8._SDK,3._SDK,9._SDK,12._SDK/))) .OR. &
        ANY(dy(5:128) /= 0.5_SDK)) THEN
        WRITE(*,*) "CALL BLAS_matvec(4,7,ia,ja,daa,dx,dbeta,dy) FAILED!"
        STOP 666
      ENDIF
      
      !dcsrmv_noAlphaNNZ
      dy=0.5_SDK
      CALL BLAS_matvec(4,ia,ja(1:2),daa,dx,dbeta,dy)
      IF(ANY(dy /= 0.5_SDK)) THEN
        WRITE(*,*) "CALL BLAS_matvec(4,ia,ja(1:2),daa,dx,dbeta,dy) FAILED!"
        STOP 666
      ENDIF
      CALL BLAS_matvec(4,ia,ja,daa,dx,dbeta,dy)
      IF(ANY(.NOT.(dy(1:4) .APPROXEQ. (/8._SDK,3._SDK,9._SDK,12._SDK/))) .OR. &
        ANY(dy(5:128) /= 0.5_SDK)) THEN
        WRITE(*,*) "CALL BLAS_matvec(4,ia,ja,daa,dx,dbeta,dy) FAILED!"
        STOP 666
      ENDIF
      
      !dcsrmv_noAlphaNNNZ
      dy=1.0_SDK
      CALL BLAS_matvec(ia,ja(1:2),daa,dx(1:4),dbeta,dy(1:4))
      IF(ANY(dy /= 1.0_SDK)) THEN
        WRITE(*,*) "CALL BLAS_matvec(ia,ja(1:2),daa,dx(1:4),dbeta,dy(1:4)) FAILED!"
        STOP 666
      ENDIF
      CALL BLAS_matvec(ia,ja,daa,dx,dbeta,dy(1:4))
      IF(ANY(dy /= 1.0_SDK)) THEN
        WRITE(*,*) "CALL BLAS_matvec(ia,ja,daa,dx,dbeta,dy(1:4)) FAILED!"
        STOP 666
      ENDIF
      CALL BLAS_matvec(ia,ja,daa,dx,dbeta,dy)
      IF(ANY(dy /= 1.0_SDK)) THEN
        WRITE(*,*) "CALL BLAS_matvec(ia,ja,daa,dx,dbeta,dy) FAILED!"
        STOP 666
      ENDIF
      CALL BLAS_matvec(ia,ja,daa,dx(1:4),1.0_SDK,dy(1:4))
      IF(ANY(.NOT.(dy(1:4) .APPROXEQ. (/8._SDK,3._SDK,9._SDK,12._SDK/))) .OR. &
        ANY(dy(5:128) /= 1.0_SDK)) THEN
        WRITE(*,*) "CALL BLAS_matvec(ia,ja,daa,dx(1:4),1.0_SDK,dy(1:4)) FAILED!"
        STOP 666
      ENDIF
      
      !dcsrmv_noBeta
      dy=1.5_SDK
      dx=2.0_SDK
      CALL BLAS_matvec(-4,7,ia,ja,daa,dalpha,dx,dy)
      IF(ANY(dy /= 1.5_SDK)) THEN
        WRITE(*,*) "CALL BLAS_matvec(-4,7,ia,ja,daa,dalpha,dx,dy) FAILED!"
        STOP 666
      ENDIF
      dy=23.5_SDK
      CALL BLAS_matvec(4,2,ia,ja,daa,dalpha,dx,dy)
      IF(ANY(dy /= 23.5_SDK)) THEN
        WRITE(*,*) "CALL BLAS_matvec(4,2,ia,ja,daa,dalpha,dx,dy) FAILED!"
        STOP 666
      ENDIF
      dy=1.0_SDK
      CALL BLAS_matvec(4,7,ia,ja,daa,0.0_SDK,dx,dy)
      IF(ANY(dy /= 1.0_SDK)) THEN
        WRITE(*,*) "CALL BLAS_matvec(4,7,ia,ja,daa,0.0_SDK,dx,dy) FAILED!"
        STOP 666
      ENDIF
      CALL BLAS_matvec(4,7,ia,ja,daa,dalpha,dx,dy)
      IF(ANY(.NOT.(dy(1:4) .APPROXEQ. (/8._SDK,3._SDK,9._SDK,12._SDK/))) .OR. &
        ANY(dy(5:128) /= 1.0_SDK)) THEN
        WRITE(*,*) "CALL BLAS_matvec(4,7,ia,ja,daa,dalpha,dx,dy) FAILED!"
        STOP 666
      ENDIF
      
      !dcsrmv_noBetaNNZ
      dy=1.0_SDK
      CALL BLAS_matvec(4,ia,ja(1:2),daa,dalpha,dx,dy)
      IF(ANY(dy /= 1.0_SDK)) THEN
        WRITE(*,*) "CALL BLAS_matvec(4,ia,ja(1:2),daa,dalpha,dx,dy) FAILED!"
        STOP 666
      ENDIF
      CALL BLAS_matvec(4,ia,ja,daa,dalpha,dx,dy)
      IF(ANY(.NOT.(dy(1:4) .APPROXEQ. (/8._SDK,3._SDK,9._SDK,12._SDK/))) .OR. &
        ANY(dy(5:128) /= 1.0_SDK)) THEN
        WRITE(*,*) "CALL BLAS_matvec(4,ia,ja,daa,dalpha,dx,dy) FAILED!"
        STOP 666
      ENDIF
      
      !dcsrmv_noBetaNNNZ
      dy=1.0_SDK
      CALL BLAS_matvec(ia,ja(1:2),daa,dalpha,dx(1:4),dy(1:4))
      IF(ANY(dy /= 1.0_SDK)) THEN
        WRITE(*,*) "CALL BLAS_matvec(ia,ja(1:2),daa,dalpha,dx(1:4),dy(1:4)) FAILED!"
        STOP 666
      ENDIF
      CALL BLAS_matvec(ia,ja,daa,dalpha,dx,dy(1:4))
      IF(ANY(dy /= 1.0_SDK)) THEN
        WRITE(*,*) "CALL BLAS_matvec(ia,ja,daa,dalpha,dx,dy(1:4)) FAILED!"
        STOP 666
      ENDIF
      CALL BLAS_matvec(ia,ja,daa,dalpha,dx,dy)
      IF(ANY(dy /= 1.0_SDK)) THEN
        WRITE(*,*) "CALL BLAS_matvec(ia,ja,daa,dalpha,dx,dy) FAILED!"
        STOP 666
      ENDIF
      CALL BLAS_matvec(ia,ja,daa,dalpha,dx(1:4),dy(1:4))
      IF(ANY(.NOT.(dy(1:4) .APPROXEQ. (/8._SDK,3._SDK,9._SDK,12._SDK/))) .OR. &
        ANY(dy(5:128) /= 1.0_SDK)) THEN
        WRITE(*,*) "CALL BLAS_matvec(ia,ja,daa,dalpha,dx(1:4),dy(1:4)) FAILED!"
        STOP 666
      ENDIF
      
      !dcsrmv_noAlphaBeta
      dy=1.5_SDK
      dx=1.0_SDK
      CALL BLAS_matvec(-4,7,ia,ja,daa,dx,dy)
      IF(ANY(dy /= 1.5_SDK)) THEN
        WRITE(*,*) "CALL BLAS_matvec(-4,7,ia,ja,daa,dx,dy) FAILED!"
        STOP 666
      ENDIF
      dy=23.5_SDK
      CALL BLAS_matvec(4,2,ia,ja,daa,dx,dy)
      IF(ANY(dy /= 23.5_SDK)) THEN
        WRITE(*,*) "CALL BLAS_matvec(4,2,ia,ja,daa,dx,dy) FAILED!"
        STOP 666
      ENDIF
      dy=1.0_SDK
      CALL BLAS_matvec(4,7,ia,ja,daa,dx,dy)
      IF(ANY(.NOT.(dy(1:4) .APPROXEQ. (/8._SDK,3._SDK,9._SDK,12._SDK/))) .OR. &
        ANY(dy(5:128) /= 1.0_SDK)) THEN
        WRITE(*,*) "CALL BLAS_matvec(4,7,ia,ja,daa,dx,dy) FAILED!"
        STOP 666
      ENDIF
      
      !dcsrmv_noAlphaBetaNNZ
      dy=1.0_SDK
      CALL BLAS_matvec(4,ia,ja(1:2),daa,dx,dy)
      IF(ANY(dy /= 1.0_SDK)) THEN
        WRITE(*,*) "CALL BLAS_matvec(4,ia,ja(1:2),daa,dx,dy) FAILED!"
        STOP 666
      ENDIF
      CALL BLAS_matvec(4,ia,ja,daa,dx,dy)
      IF(ANY(.NOT.(dy(1:4) .APPROXEQ. (/8._SDK,3._SDK,9._SDK,12._SDK/))) .OR. &
        ANY(dy(5:128) /= 1.0_SDK)) THEN
        WRITE(*,*) "CALL BLAS_matvec(4,ia,ja,daa,dx,dy) FAILED!"
        STOP 666
      ENDIF
      
      !dcsrmv_noAlphaBetaNNNZ
      dy=1.0_SDK
      CALL BLAS_matvec(ia,ja(1:2),daa,dx(1:4),dy(1:4))
      IF(ANY(dy /= 1.0_SDK)) THEN
        WRITE(*,*) "CALL BLAS_matvec(ia,ja(1:2),daa,dx(1:4),dy(1:4)) FAILED!"
        STOP 666
      ENDIF
      CALL BLAS_matvec(ia,ja,daa,dx,dy(1:4))
      IF(ANY(dy /= 1.0_SDK)) THEN
        WRITE(*,*) "CALL BLAS_matvec(ia,ja,daa,dx,dy(1:4)) FAILED!"
        STOP 666
      ENDIF
      CALL BLAS_matvec(ia,ja,daa,dx,dy)
      IF(ANY(dy /= 1.0_SDK)) THEN
        WRITE(*,*) "CALL BLAS_matvec(ia,ja,daa,dx,dy) FAILED!"
        STOP 666
      ENDIF
      CALL BLAS_matvec(ia,ja,daa,dx(1:4),dy(1:4))
      IF(ANY(.NOT.(dy(1:4) .APPROXEQ. (/8._SDK,3._SDK,9._SDK,12._SDK/))) .OR. &
        ANY(dy(5:128) /= 1.0_SDK)) THEN
        WRITE(*,*) "CALL BLAS_matvec(ia,ja,daa,dx(1:4),dy(1:4)) FAILED!"
        STOP 666
      ENDIF
    ENDSUBROUTINE testBLAS2
!
!-------------------------------------------------------------------------------
    SUBROUTINE testBLAS3()
      REAL(SSK) :: sa(128,128),sb(128,128),sc(128,128),salpha,sbeta
      REAL(SDK) :: da(128,128),db(128,128),dc(128,128),dalpha,dbeta
      REAL(SSK) :: sa2(128,128),sb2(128,128),sc2(128,128),salpha2,sbeta2
      REAL(SDK) :: da2(128,128),db2(128,128),dc2(128,128),dalpha2,dbeta2
!      
!Test sgemm interfaces
      sa=2.0_SSK
      sb=3.0_SSK
      sc=4.0_SSK
      salpha=0.0_SSK
      sbeta=0.0_SSK
      
!Secondary Test Variables
      sa2=2.0_SSK
      sb2=3.0_SSK
      sc2=4.0_SSK
      salpha2=2.0_SSK
      sbeta2=2.5_SSK
    
      !sgemm_all(transa,transb,m,n,k,alpha,a,lda,b,ldb,beta,c,ldc)
      !Check beta=0 and alpha=0 returns all zeros for the bounds specified
      CALL BLAS_matmat('N','n',4,4,4,salpha,sa,4,sb,4,sbeta,sc,4)
      IF(ANY(.NOT.(sc(1:16,1) .APPROXEQ. 0.0_SSK)) .AND. &
           ANY(.NOT.(sc(17:128,1) .APPROXEQ. 4.0_SSK)) .AND. &
             ANY(.NOT.(sc(:,2:128) .APPROXEQ. 4.0_SSK))) THEN
        WRITE(*,*) "CALL BLAS_matmat('N','N',4,4,4,salpha,sa,4,sb,4,sbeta,sc,4) FAILED!"
        STOP 666
      ENDIF
      !Check beta=0 and alpha=0 returns all zeros for the bounds specified, n*m>10000
      CALL BLAS_matmat('n','N',101,101,101,salpha,sa,101,sb,101,sbeta,sc,101)
      IF(ANY(.NOT.(sc(1:128,1:79) .APPROXEQ. 0.0_SSK)) .AND. &
           ANY(.NOT.(sc(1:89,80) .APPROXEQ. 0.0_SSK)) .AND. &
             ANY(.NOT.(sc(90:128,80) .APPROXEQ. 4.0_SSK)) .AND. &
               ANY(.NOT.(sc(90:128,80) .APPROXEQ. 4.0_SSK)) ) THEN
        WRITE(*,*) "CALL BLAS_matmat('N','N',101,101,101,salpha,sa,101,sb,101,sbeta,sc,101)) FAILED!"
        STOP 666
      ENDIF
      !Check beta=4 and alpha=0 returns 16.0 for the bounds specified
      sc=4.0_SSK
      sbeta=4.0_SSK
      CALL BLAS_matmat('N','N',4,2,4,salpha,sa,4,sb,4,sbeta,sc,4)
      IF(ANY(.NOT.(sc(1:8,1) .APPROXEQ. 16.0_SSK)) .AND. &
           ANY(.NOT.(sc(17:128,1) .APPROXEQ. 4.0_SSK)) .AND. &
             ANY(.NOT.(sc(:,2:128) .APPROXEQ. 4.0_SSK))) THEN
        WRITE(*,*) "CALL BLAS_matmat('N','N',4,4,4,salpha,sa,4,sb,4,sbeta,sc,4) FAILED!"
        STOP 666
      ENDIF
      !Check beta=4 and alpha=0 returns 16.0 for the bounds specified, n*m>10000
      sc=4.0_SSK
      CALL BLAS_matmat('N','N',101,101,101,salpha,sa,101,sb,101,sbeta,sc,101)
      IF(ANY(.NOT.(sc(1:128,1:79) .APPROXEQ. 16.0_SSK)) .AND. &
           ANY(.NOT.(sc(1:89,80) .APPROXEQ. 16.0_SSK)) .AND. &
             ANY(.NOT.(sc(90:128,80) .APPROXEQ. 4.0_SSK)) .AND. &
               ANY(.NOT.(sc(90:128,80) .APPROXEQ. 4.0_SSK)) ) THEN
        WRITE(*,*) "CALL BLAS_matmat('N','N',101,101,101,salpha,sa,101,sb,101,sbeta,sc,101) FAILED!"
        STOP 666
      ENDIF
      !Check beta=0 and alpha=2 returns 36.0 for the bounds specified
      salpha=2.0_SSK
      sc=4.0_SSK
      sbeta=0.0_SSK
      CALL BLAS_matmat('N','N',3,5,3,salpha,sa,3,sb,5,sbeta,sc,3)
      IF(ANY(.NOT.(sc(1:15,1) .APPROXEQ. 36.0_SSK)) .AND. &
           ANY(.NOT.(sc(16:128,1) .APPROXEQ. 4.0_SSK)) .AND. &
             ANY(.NOT.(sc(:,2:128) .APPROXEQ. 4.0_SSK))) THEN
        WRITE(*,*) "CALL BLAS_matmat('N','N',3,5,3,salpha,sa,101,sb,5,sbeta,sc,3) FAILED!"
        STOP 666
      ENDIF
      !Check beta=2.5 and alpha=2 returns 46.0 for the bounds specified
      sc=4.0_SSK
      sbeta=2.5_SSK
      CALL BLAS_matmat('N','N',3,5,3,salpha,sa,3,sb,3,sbeta,sc,3)
      IF(ANY(.NOT.(sc(1:15,1) .APPROXEQ. 46.0_SSK)) .AND. &
           ANY(.NOT.(sc(16:128,1) .APPROXEQ. 4.0_SSK)) .AND. &
             ANY(.NOT.(sc(:,2:128) .APPROXEQ. 4.0_SSK))) THEN
        WRITE(*,*) "CALL BLAS_matmat('N','N',3,5,3,salpha,sa,3,sb,3,sbeta,sc,3) FAILED!"
        STOP 666
      ENDIF

      !Check Single Transpose
      sc=4.0_SSK
      sbeta=2.5_SSK
      CALL BLAS_matmat('t','N',3,3,3,salpha,sa,3,sb,3,sbeta,sc,3)
      IF(ANY(.NOT.(sc(1:15,1) .APPROXEQ. 46.0_SSK)) .AND. &
           ANY(.NOT.(sc(16:128,1) .APPROXEQ. 4.0_SSK)) .AND. &
             ANY(.NOT.(sc(:,2:128) .APPROXEQ. 4.0_SSK))) THEN
        WRITE(*,*) "BLAS_matmat('T','N',3,3,3,salpha,sa,3,sb,3,sbeta,sc,3) FAILED!"
        STOP 666
      ENDIF
      !Check Second Single Transpose
      sc=4.0_SSK
      sbeta=2.5_SSK
      CALL BLAS_matmat('N','T',3,3,3,salpha,sa,3,sb,3,sbeta,sc,3)
      IF(ANY(.NOT.(sc(1:15,1) .APPROXEQ. 46.0_SSK)) .AND. &
           ANY(.NOT.(sc(16:128,1) .APPROXEQ. 4.0_SSK)) .AND. &
             ANY(.NOT.(sc(:,2:128) .APPROXEQ. 4.0_SSK))) THEN
        WRITE(*,*) "CALL BLAS_matmat('N','T',3,3,3,salpha,sa,3,sb,3,sbeta,sc,3) FAILED!"
        STOP 666
      ENDIF
      !Check Both Transpose
      sc=4.0_SSK
      sbeta=2.5_SSK
      CALL BLAS_matmat('T','t',3,3,3,salpha,sa,3,sb,3,sbeta,sc,3)
      IF(ANY(.NOT.(sc(1:15,1) .APPROXEQ. 46.0_SSK)) .AND. &
           ANY(.NOT.(sc(16:128,1) .APPROXEQ. 4.0_SSK)) .AND. &
             ANY(.NOT.(sc(:,2:128) .APPROXEQ. 4.0_SSK))) THEN
        WRITE(*,*) "BLAS_matmat('T','T',3,3,3,salpha,sa,3,sb,3,sbeta,sc,3) FAILED!"
        STOP 666
      ENDIF
      
      !With Beta = 0
      !Check Single Transpose
      sc=4.0_SSK
      sbeta=0.0_SSK
      CALL BLAS_matmat('T','N',3,3,3,salpha,sa,3,sb,3,sbeta,sc,3)
      IF(ANY(.NOT.(sc(1:15,1) .APPROXEQ. 46.0_SSK)) .AND. &
           ANY(.NOT.(sc(16:128,1) .APPROXEQ. 4.0_SSK)) .AND. &
             ANY(.NOT.(sc(:,2:128) .APPROXEQ. 4.0_SSK))) THEN
        WRITE(*,*) "BLAS_matmat('T','N',3,3,3,salpha,sa,3,sb,3,sbeta,sc,3) FAILED!"
        STOP 666
      ENDIF
      !Check Second Single Transpose
      sc=4.0_SSK
      CALL BLAS_matmat('N','T',3,3,3,salpha,sa,3,sb,3,sbeta,sc,3)
      IF(ANY(.NOT.(sc(1:15,1) .APPROXEQ. 46.0_SSK)) .AND. &
           ANY(.NOT.(sc(16:128,1) .APPROXEQ. 4.0_SSK)) .AND. &
             ANY(.NOT.(sc(:,2:128) .APPROXEQ. 4.0_SSK))) THEN
        WRITE(*,*) "CALL BLAS_matmat('N','T',3,3,3,salpha,sa,3,sb,3,sbeta,sc,3) FAILED!"
        STOP 666
      ENDIF
      !Check Both Transpose
      sc=4.0_SSK
      CALL BLAS_matmat('T','T',3,3,3,salpha,sa,3,sb,3,sbeta,sc,3)
      IF(ANY(.NOT.(sc(1:15,1) .APPROXEQ. 46.0_SSK)) .AND. &
           ANY(.NOT.(sc(16:128,1) .APPROXEQ. 4.0_SSK)) .AND. &
             ANY(.NOT.(sc(:,2:128) .APPROXEQ. 4.0_SSK))) THEN
        WRITE(*,*) "BLAS_matmat('T','T',3,3,3,salpha,sa,3,sb,3,sbeta,sc,3) FAILED!"
        STOP 666
      ENDIF

      sc=4.0_SSK
      sbeta=2.5_SSK
      CALL BLAS_matmat('N','N',3,5,3,salpha,sa,3,sb,3,sbeta,sc,3)
      !Check sgemm_ttmnkaabbc
      CALL BLAS_matmat('N','N',3,5,3,salpha2,sa2,sb2,sbeta2,sc2)
      IF(ANY(.NOT.(sc(:,:) .APPROXEQ. sc2(:,:)))) THEN
        WRITE(*,*) "CALL BLAS_matmat('N','N',3,5,3,salpha2,sa2,sb2,sbeta2,sc2) FAILED!"
        STOP 666
      ENDIF
      
      !Check sgemm_ttmnkabbc
      sa= 2.0_SSK; sb= 3.0_SSK; sc= 4.0_SSK; salpha= 1.0_SSK; sbeta= 2.5_SSK
      sa2=2.0_SSK; sb2=3.0_SSK; sc2=4.0_SSK; salpha2=2.0_SSK; sbeta2=2.5_SSK
      
      CALL BLAS_matmat('N','N',3,5,3,salpha,sa,3,sb,3,sbeta,sc,3)
      
      CALL BLAS_matmat('N','N',3,5,3,sa2,sb2,sbeta2,sc2)
      IF(ANY(.NOT.(sc(:,:) .APPROXEQ. sc2(:,:)))) THEN
        WRITE(*,*) "CALL BLAS_matmat('N','N',3,5,3,sa2,sb2,sbeta2,sc2) FAILED!"
        STOP 666
      ENDIF
      
      !Check sgemm_ttmnkaabc
      sc= 4.0_SSK; salpha= 2.0_SSK; sbeta= 1.0_SSK
      sc2=4.0_SSK; salpha2=2.0_SSK; sbeta2=1.0_SSK
      
      CALL BLAS_matmat('N','N',3,5,3,salpha,sa,3,sb,3,sbeta,sc,3)
      
      CALL BLAS_matmat('N','N',3,5,3,salpha,sa2,sb2,sc2)
      IF(ANY(.NOT.(sc(:,:) .APPROXEQ. sc2(:,:)))) THEN
        WRITE(*,*) "CALL BLAS_matmat('N','N',3,5,3,salpha2,sa2,sb2,sc2) FAILED!"
        STOP 666
      ENDIF
      
      !Check sgemm_ttmnkabc
      sc= 4.0_SSK; salpha= 1.0_SSK; sbeta= 1.0_SSK
      sc2=4.0_SSK; salpha2=1.0_SSK; sbeta2=1.0_SSK
      
      CALL BLAS_matmat('n','N',3,5,3,salpha,sa,3,sb,3,sbeta,sc,3)
      
      CALL BLAS_matmat('N','n',3,5,3,sa2,sb2,sc2)
      IF(ANY(.NOT.(sc(:,:) .APPROXEQ. sc2(:,:)))) THEN
        WRITE(*,*) "CALL BLAS_matmat('N','N',3,5,3,sa2,sb2,sc2) FAILED!"
        STOP 666
      ENDIF
      
      !Check sgemm_mnkaabbc
      sc= 4.0_SSK; salpha= 2.0_SSK; sbeta= 2.5_SSK
      sc2=4.0_SSK; salpha2=2.0_SSK; sbeta2=2.5_SSK
      
      CALL BLAS_matmat('N','N',3,5,3,salpha,sa,3,sb,3,sbeta,sc,1)
      
      CALL BLAS_matmat(3,5,3,salpha2,sa2,sb2,sbeta2,sc2)
      IF(ANY(.NOT.(sc(:,:) .APPROXEQ. sc2(:,:)))) THEN
        WRITE(*,*) "CALL BLAS_matmat('N','N',3,5,3,sa2,sb2,sc2) FAILED!"
        STOP 666
      ENDIF
      
      !Check sgemm_ttaabbc
      sc= 4.0_SSK; salpha= 2.0_SSK; sbeta= 2.5_SSK
      sc2=4.0_SSK; salpha2=2.0_SSK; sbeta2=2.5_SSK
      
      CALL BLAS_matmat('N','N',SIZE(sc,DIM=1),SIZE(sc,DIM=2),SIZE(sb,DIM=1),salpha, &
        sa,SIZE(sc,DIM=1),sb,SIZE(sb,DIM=1),sbeta,sc,SIZE(sc,DIM=1))
      
      CALL BLAS_matmat('N','N',salpha2,sa2,sb2,sbeta2,sc2)
      IF(ANY(.NOT.(sc(:,:) .APPROXEQ. sc2(:,:)))) THEN
        WRITE(*,*) "CALL BLAS_matmat('N','N',salpha2,sa2,sb2,sbeta2,sc2) FAILED!"
        STOP 666
      ENDIF
      
      !Check sgemm_aabbc
      sc= 4.0_SSK; salpha= 2.0_SSK; sbeta= 2.5_SSK
      sc2=4.0_SSK; salpha2=2.0_SSK; sbeta2=2.5_SSK
      
      CALL BLAS_matmat('N','N',SIZE(sc,DIM=1),SIZE(sc,DIM=2),SIZE(sb,DIM=1),salpha, &
        sa,SIZE(sc,DIM=1),sb,SIZE(sb,DIM=1),sbeta,sc,1)
      
      CALL BLAS_matmat(salpha2,sa2,sb2,sbeta2,sc2)
      IF(ANY(.NOT.(sc(:,:) .APPROXEQ. sc2(:,:)))) THEN
        WRITE(*,*) "CALL BLAS_matmat(salpha2,sa2,sb2,sbeta2,sc2) FAILED!"
        STOP 666
      ENDIF
      
      !Check sgemm_abbc
      sc= 4.0_SSK; salpha= 1.0_SSK; sbeta= 2.5_SSK
      sc2=4.0_SSK; salpha2=1.0_SSK; sbeta2=2.5_SSK
      
      CALL BLAS_matmat('N','N',SIZE(sc,DIM=1),SIZE(sc,DIM=2),SIZE(sb,DIM=1),salpha, &
        sa,SIZE(sc,DIM=1),sb,SIZE(sb,DIM=1),sbeta,sc,1)
      
      CALL BLAS_matmat(sa2,sb2,sbeta2,sc2)
      IF(ANY(.NOT.(sc(:,:) .APPROXEQ. sc2(:,:)))) THEN
        WRITE(*,*) "CALL BLAS_matmat(salpha2,sa2,sb2,sbeta2,sc2) FAILED!"
        STOP 666
      ENDIF
      
      !Check sgemm_aabc
      sc= 4.0_SSK; salpha= 2.0_SSK; sbeta= 1.0_SSK
      sc2=4.0_SSK; salpha2=2.0_SSK; sbeta2=1.0_SSK
      
      CALL BLAS_matmat('N','N',SIZE(sc,DIM=1),SIZE(sc,DIM=2),SIZE(sb,DIM=1),salpha, &
        sa,SIZE(sc,DIM=1),sb,SIZE(sb,DIM=1),sbeta,sc,1)
      
      CALL BLAS_matmat(salpha2,sa2,sb2,sc2)
      IF(ANY(.NOT.(sc(:,:) .APPROXEQ. sc2(:,:)))) THEN
        WRITE(*,*) "CALL BLAS_matmat(salpha2,sa2,sb2,sbeta2,sc2) FAILED!"
        STOP 666
      ENDIF
      
      !Check sgemm_abc
      sc= 4.0_SSK; salpha= 1.0_SSK; sbeta= 1.0_SSK
      sc2=4.0_SSK; salpha2=1.0_SSK; sbeta2=1.0_SSK
      
      CALL BLAS_matmat('N','N',SIZE(sc,DIM=1),SIZE(sc,DIM=2),SIZE(sb,DIM=1),salpha, &
        sa,SIZE(sc,DIM=1),sb,SIZE(sb,DIM=1),sbeta,sc,1)
      
      CALL BLAS_matmat(sa2,sb2,sc2)
      IF(ANY(.NOT.(sc(:,:) .APPROXEQ. sc2(:,:)))) THEN
        WRITE(*,*) "CALL BLAS_matmat(salpha2,sa2,sb2,sbeta2,sc2) FAILED!"
        STOP 666
      ENDIF
      
      
!Test dgemm interfaces
      da=2.0_SDK
      db=3.0_SDK
      dc=4.0_SDK
      dalpha=0.0_SDK
      dbeta=0.0_SDK
      
!Secondary Test Variables
      da2=2.0_SDK
      db2=3.0_SDK
      dc2=4.0_SDK
      dalpha2=2.0_SDK
      dbeta2=2.5_SDK
    
      
      
      !sgemm_all(transa,transb,m,n,k,alpha,a,lda,b,ldb,beta,c,ldc)
      !Check beta=0 and alpha=0 returns all zeros for the bounds specified
      CALL BLAS_matmat('N','n',4,4,4,dalpha,da,4,db,4,dbeta,dc,4)
      IF(ANY(.NOT.(dc(1:16,1) .APPROXEQ. 0.0_SDK)) .AND. &
           ANY(.NOT.(dc(17:128,1) .APPROXEQ. 4.0_SDK)) .AND. &
             ANY(.NOT.(dc(:,2:128) .APPROXEQ. 4.0_SDK))) THEN
        WRITE(*,*) "CALL BLAS_matmat('N','N',4,4,4,dalpha,da,4,db,4,dbeta,dc,4) FAILED!"
        STOP 666
      ENDIF
      !Check beta=0 and alpha=0 returns all zeros for the bounds specified, n*m>10000
      CALL BLAS_matmat('n','N',101,101,101,dalpha,da,101,db,101,dbeta,dc,101)
      IF(ANY(.NOT.(dc(1:128,1:79) .APPROXEQ. 0.0_SDK)) .AND. &
           ANY(.NOT.(dc(1:89,80) .APPROXEQ. 0.0_SDK)) .AND. &
             ANY(.NOT.(dc(90:128,80) .APPROXEQ. 4.0_SDK)) .AND. &
               ANY(.NOT.(dc(90:128,80) .APPROXEQ. 4.0_SDK)) ) THEN
        WRITE(*,*) "CALL BLAS_matmat('N','N',101,101,101,dalpha,da,101,db,101,dbeta,dc,101)) FAILED!"
        STOP 666
      ENDIF
      !Check beta=4 and alpha=0 returns 16.0 for the bounds specified
      dc=4.0_SDK
      dbeta=4.0_SDK
      CALL BLAS_matmat('N','N',4,2,4,dalpha,da,4,db,4,dbeta,dc,4)
      IF(ANY(.NOT.(dc(1:8,1) .APPROXEQ. 16.0_SDK)) .AND. &
           ANY(.NOT.(dc(17:128,1) .APPROXEQ. 4.0_SDK)) .AND. &
             ANY(.NOT.(dc(:,2:128) .APPROXEQ. 4.0_SDK))) THEN
        WRITE(*,*) "CALL BLAS_matmat('N','N',4,4,4,dalpha,da,4,db,4,dbeta,dc,4) FAILED!"
        STOP 666
      ENDIF
      !Check beta=4 and alpha=0 returns 16.0 for the bounds specified, n*m>10000
      dc=4.0_SDK
      CALL BLAS_matmat('N','N',101,101,101,dalpha,da,101,db,101,dbeta,dc,101)
      IF(ANY(.NOT.(dc(1:128,1:79) .APPROXEQ. 16.0_SDK)) .AND. &
           ANY(.NOT.(dc(1:89,80) .APPROXEQ. 16.0_SDK)) .AND. &
             ANY(.NOT.(dc(90:128,80) .APPROXEQ. 4.0_SDK)) .AND. &
               ANY(.NOT.(dc(90:128,80) .APPROXEQ. 4.0_SDK)) ) THEN
        WRITE(*,*) "CALL BLAS_matmat('N','N',101,101,101,dalpha,da,101,db,101,dbeta,dc,101) FAILED!"
        STOP 666
      ENDIF
      !Check beta=0 and alpha=2 returns 36.0 for the bounds specified
      dalpha=2.0_SDK
      dc=4.0_SDK
      dbeta=0.0_SDK
      CALL BLAS_matmat('N','N',3,5,3,dalpha,da,3,db,5,dbeta,dc,3)
      IF(ANY(.NOT.(dc(1:15,1) .APPROXEQ. 36.0_SDK)) .AND. &
           ANY(.NOT.(dc(16:128,1) .APPROXEQ. 4.0_SDK)) .AND. &
             ANY(.NOT.(dc(:,2:128) .APPROXEQ. 4.0_SDK))) THEN
        WRITE(*,*) "CALL BLAS_matmat('N','N',101,101,101,dalpha,da,101,db,101,dbeta,dc,101) FAILED!"
        STOP 666
      ENDIF
      !Check beta=2.5 and alpha=2 returns 46.0 for the bounds specified
      dc=4.0_SDK
      dbeta=2.5_SDK
      CALL BLAS_matmat('N','N',3,5,3,dalpha,da,3,db,3,dbeta,dc,3)
      IF(ANY(.NOT.(dc(1:15,1) .APPROXEQ. 46.0_SDK)) .AND. &
           ANY(.NOT.(dc(16:128,1) .APPROXEQ. 4.0_SDK)) .AND. &
             ANY(.NOT.(dc(:,2:128) .APPROXEQ. 4.0_SDK))) THEN
        WRITE(*,*) "CALL BLAS_matmat('N','N',101,101,101,dalpha,da,101,db,101,dbeta,dc,101) FAILED!"
        STOP 666
             ENDIF

      !Check Single Transpose
      dc=4.0_SDK
      dbeta=2.5_SDK
      CALL BLAS_matmat('T','N',3,3,3,dalpha,da,3,db,3,dbeta,dc,3)
      IF(ANY(.NOT.(dc(1:15,1) .APPROXEQ. 46.0_SDK)) .AND. &
           ANY(.NOT.(dc(16:128,1) .APPROXEQ. 4.0_SDK)) .AND. &
             ANY(.NOT.(dc(:,2:128) .APPROXEQ. 4.0_SDK))) THEN
        WRITE(*,*) "CALL BLAS_matmat('T','N',3,3,3,dalpha,da,3,db,3,dbeta,dc,3) FAILED!"
        STOP 666
      ENDIF
      !Check Second Single Transpose
      dc=4.0_SDK
      dbeta=2.5_SDK
      CALL BLAS_matmat('N','t',3,3,3,dalpha,da,3,db,3,dbeta,dc,3)
      IF(ANY(.NOT.(dc(1:15,1) .APPROXEQ. 46.0_SDK)) .AND. &
           ANY(.NOT.(dc(16:128,1) .APPROXEQ. 4.0_SDK)) .AND. &
             ANY(.NOT.(dc(:,2:128) .APPROXEQ. 4.0_SDK))) THEN
        WRITE(*,*) "CALL BLAS_matmat('N','T',3,3,3,dalpha,da,3,db,3,dbeta,dc,3) FAILED!"
        STOP 666
      ENDIF
      !Check Both Transpose
      dc=4.0_SDK
      dbeta=2.5_SDK
      CALL BLAS_matmat('t','T',3,3,3,dalpha,da,3,db,3,dbeta,dc,3)
      IF(ANY(.NOT.(dc(1:15,1) .APPROXEQ. 46.0_SDK)) .AND. &
           ANY(.NOT.(dc(16:128,1) .APPROXEQ. 4.0_SDK)) .AND. &
             ANY(.NOT.(dc(:,2:128) .APPROXEQ. 4.0_SDK))) THEN
        WRITE(*,*) "CALL BLAS_matmat('T','T',3,3,3,dalpha,da,3,db,3,dbeta,dc,3) FAILED!"
        STOP 666
      ENDIF
      !With Beta = 0
      !Check Single Transpose
      dc=4.0_SDK
      dbeta=0.0_SDK
      CALL BLAS_matmat('T','N',3,3,3,dalpha,da,3,db,3,dbeta,dc,3)
      IF(ANY(.NOT.(dc(1:15,1) .APPROXEQ. 46.0_SDK)) .AND. &
           ANY(.NOT.(dc(16:128,1) .APPROXEQ. 4.0_SDK)) .AND. &
             ANY(.NOT.(dc(:,2:128) .APPROXEQ. 4.0_SDK))) THEN
        WRITE(*,*) "CALL BLAS_matmat('T','N',3,3,3,dalpha,da,3,db,3,dbeta,dc,3) FAILED!"
        STOP 666
      ENDIF
      !Check Second Single Transpose
      dc=4.0_SDK
      CALL BLAS_matmat('N','T',3,3,3,dalpha,da,3,db,3,dbeta,dc,3)
      IF(ANY(.NOT.(dc(1:15,1) .APPROXEQ. 46.0_SDK)) .AND. &
           ANY(.NOT.(dc(16:128,1) .APPROXEQ. 4.0_SDK)) .AND. &
             ANY(.NOT.(dc(:,2:128) .APPROXEQ. 4.0_SDK))) THEN
        WRITE(*,*) "CALL BLAS_matmat('N','T',3,3,3,dalpha,da,3,db,3,dbeta,dc,3) FAILED!"
        STOP 666
      ENDIF
      !Check Both Transpose
      dc=4.0_SDK
      CALL BLAS_matmat('T','T',3,3,3,dalpha,da,3,db,3,dbeta,dc,3)
      IF(ANY(.NOT.(dc(1:15,1) .APPROXEQ. 46.0_SDK)) .AND. &
           ANY(.NOT.(dc(16:128,1) .APPROXEQ. 4.0_SDK)) .AND. &
             ANY(.NOT.(dc(:,2:128) .APPROXEQ. 4.0_SDK))) THEN
        WRITE(*,*) "CALL BLAS_matmat('T','T',3,3,3,dalpha,da,3,db,3,dbeta,dc,3) FAILED!"
        STOP 666
      ENDIF
             
       
      dc=4.0_SDK 
      dbeta=2.5_SDK     
      CALL BLAS_matmat('N','N',3,5,3,dalpha,da,3,db,3,dbeta,dc,3)
      !Check sgemm_ttmnkaabbc
      CALL BLAS_matmat('N','N',3,5,3,dalpha2,da2,db2,dbeta2,dc2)
      IF(ANY(.NOT.(dc(:,:) .APPROXEQ. dc2(:,:)))) THEN
        WRITE(*,*) "CALL BLAS_matmat('N','N',3,5,3,dalpha2,da2,db2,dbeta2,dc2) FAILED!"
        STOP 666
      ENDIF
      
      !Check sgemm_ttmnkabbc
      da= 2.0_SDK; db= 3.0_SDK; dc= 4.0_SDK; dalpha= 1.0_SDK; dbeta= 2.5_SDK
      da2=2.0_SDK; db2=3.0_SDK; dc2=4.0_SDK; dalpha2=2.0_SDK; dbeta2=2.5_SDK
      
      CALL BLAS_matmat('N','N',3,5,3,dalpha,da,3,db,3,dbeta,dc,3)
      
      CALL BLAS_matmat('N','N',3,5,3,da2,db2,dbeta2,dc2)
      IF(ANY(.NOT.(dc(:,:) .APPROXEQ. dc2(:,:)))) THEN
        WRITE(*,*) "CALL BLAS_matmat('N','N',3,5,3,da2,db2,dbeta2,dc2) FAILED!"
        STOP 666
      ENDIF
      
      !Check sgemm_ttmnkaabc
      dc= 4.0_SDK; dalpha= 2.0_SDK; dbeta= 1.0_SDK
      dc2=4.0_SDK; dalpha2=2.0_SDK; dbeta2=1.0_SDK
      
      CALL BLAS_matmat('N','N',3,5,3,dalpha,da,3,db,3,dbeta,dc,3)
      
      CALL BLAS_matmat('N','N',3,5,3,dalpha,da2,db2,dc2)
      IF(ANY(.NOT.(dc(:,:) .APPROXEQ. dc2(:,:)))) THEN
        WRITE(*,*) "CALL BLAS_matmat('N','N',3,5,3,dalpha2,da2,db2,dc2) FAILED!"
        STOP 666
      ENDIF
      
      !Check sgemm_ttmnkabc
      dc= 4.0_SDK; dalpha= 1.0_SDK; dbeta= 1.0_SDK
      dc2=4.0_SDK; dalpha2=1.0_SDK; dbeta2=1.0_SDK
      
      CALL BLAS_matmat('N','N',3,5,3,dalpha,da,3,db,3,dbeta,dc,3)
      
      CALL BLAS_matmat('N','N',3,5,3,da2,db2,dc2)
      IF(ANY(.NOT.(dc(:,:) .APPROXEQ. dc2(:,:)))) THEN
        WRITE(*,*) "CALL BLAS_matmat('N','N',3,5,3,da2,db2,dc2) FAILED!"
        STOP 666
      ENDIF
      
      !Check sgemm_mnkaabbc
      dc= 4.0_SDK; dalpha= 2.0_SDK; dbeta= 2.5_SDK
      dc2=4.0_SDK; dalpha2=2.0_SDK; dbeta2=2.5_SDK
      
      CALL BLAS_matmat('N','N',3,5,3,dalpha,da,3,db,3,dbeta,dc,1)
      
      CALL BLAS_matmat(3,5,3,dalpha2,da2,db2,dbeta2,dc2)
      IF(ANY(.NOT.(dc(:,:) .APPROXEQ. dc2(:,:)))) THEN
        WRITE(*,*) "CALL BLAS_matmat('N','N',3,5,3,da2,db2,dc2) FAILED!"
        STOP 666
      ENDIF
      
      !Check sgemm_ttaabbc
      dc= 4.0_SDK; dalpha= 2.0_SDK; dbeta= 2.5_SDK
      dc2=4.0_SDK; dalpha2=2.0_SDK; dbeta2=2.5_SDK
      
      CALL BLAS_matmat('N','N',SIZE(dc,DIM=1),SIZE(dc,DIM=2),SIZE(db,DIM=1),dalpha, &
        da,SIZE(dc,DIM=1),db,SIZE(db,DIM=1),dbeta,dc,SIZE(dc,DIM=1))
      
      CALL BLAS_matmat('N','N',dalpha2,da2,db2,dbeta2,dc2)
      IF(ANY(.NOT.(dc(:,:) .APPROXEQ. dc2(:,:)))) THEN
        WRITE(*,*) "CALL BLAS_matmat('N','N',dalpha2,da2,db2,dbeta2,dc2) FAILED!"
        STOP 666
      ENDIF
      
      !Check sgemm_aabbc
      dc= 4.0_SDK; dalpha= 2.0_SDK; dbeta= 2.5_SDK
      dc2=4.0_SDK; dalpha2=2.0_SDK; dbeta2=2.5_SDK
      
      CALL BLAS_matmat('N','N',SIZE(dc,DIM=1),SIZE(dc,DIM=2),SIZE(db,DIM=1),dalpha, &
        da,SIZE(dc,DIM=1),db,SIZE(db,DIM=1),dbeta,dc,1)
      
      CALL BLAS_matmat(dalpha2,da2,db2,dbeta2,dc2)
      IF(ANY(.NOT.(dc(:,:) .APPROXEQ. dc2(:,:)))) THEN
        WRITE(*,*) "CALL BLAS_matmat(dalpha2,da2,db2,dbeta2,dc2) FAILED!"
        STOP 666
      ENDIF
      
      !Check sgemm_abbc
      dc= 4.0_SDK; dalpha= 1.0_SDK; dbeta= 2.5_SDK
      dc2=4.0_SDK; dalpha2=1.0_SDK; dbeta2=2.5_SDK
      
      CALL BLAS_matmat('N','N',SIZE(dc,DIM=1),SIZE(dc,DIM=2),SIZE(db,DIM=1),dalpha, &
        da,SIZE(dc,DIM=1),db,SIZE(db,DIM=1),dbeta,dc,1)
      
      CALL BLAS_matmat(da2,db2,dbeta2,dc2)
      IF(ANY(.NOT.(dc(:,:) .APPROXEQ. dc2(:,:)))) THEN
        WRITE(*,*) "CALL BLAS_matmat(dalpha2,da2,db2,dbeta2,dc2) FAILED!"
        STOP 666
      ENDIF
      
      !Check sgemm_aabc
      dc= 4.0_SDK; dalpha= 2.0_SDK; dbeta= 1.0_SDK
      dc2=4.0_SDK; dalpha2=2.0_SDK; dbeta2=1.0_SDK
      
      CALL BLAS_matmat('N','N',SIZE(dc,DIM=1),SIZE(dc,DIM=2),SIZE(db,DIM=1),dalpha, &
        da,SIZE(dc,DIM=1),db,SIZE(db,DIM=1),dbeta,dc,1)
      
      CALL BLAS_matmat(dalpha2,da2,db2,dc2)
      IF(ANY(.NOT.(dc(:,:) .APPROXEQ. dc2(:,:)))) THEN
        WRITE(*,*) "CALL BLAS_matmat(dalpha2,da2,db2,dbeta2,dc2) FAILED!"
        STOP 666
      ENDIF
      
      !Check sgemm_abc
      dc= 4.0_SDK; dalpha= 1.0_SDK; dbeta= 1.0_SDK
      dc2=4.0_SDK; dalpha2=1.0_SDK; dbeta2=1.0_SDK
      
      CALL BLAS_matmat('N','N',SIZE(dc,DIM=1),SIZE(dc,DIM=2),SIZE(db,DIM=1),dalpha, &
        da,SIZE(dc,DIM=1),db,SIZE(db,DIM=1),dbeta,dc,1)
      
      CALL BLAS_matmat(da2,db2,dc2)
      IF(ANY(.NOT.(dc(:,:) .APPROXEQ. dc2(:,:)))) THEN
        WRITE(*,*) "CALL BLAS_matmat(dalpha2,da2,db2,dbeta2,dc2) FAILED!"
        STOP 666
      ENDIF
    ENDSUBROUTINE testBLAS3
ENDPROGRAM testMatrixMath
