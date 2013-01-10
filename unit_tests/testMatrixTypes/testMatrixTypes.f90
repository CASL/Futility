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
PROGRAM testMatrixTypes
  
  USE ISO_FORTRAN_ENV
  USE IntrType
  USE ExceptionHandler
  USE BLAS
  USE ParameterLists
  USE ParallelEnv
  USE VectorTypes
  USE MatrixTypes
  IMPLICIT NONE
  
#ifdef HAVE_PETSC
#include <finclude/petsc.h>
#undef IS
  PetscErrorCode  :: ierr
#endif
  
  TYPE(ExceptionHandlerType),TARGET :: e
  TYPE(ParamType) :: pList,optListMat,vecPList
  
  !Configure exception handler for test
  CALL e%setStopOnError(.FALSE.)
  CALL e%setQuietMode(.TRUE.)
  eParams => e
  eMatrixType => e
  
  !Set up optional PL
  CALL optListMat%add('MatrixType->nnz',-1_SNK)
  CALL optListMat%add('MatrixType->isSym',.FALSE.)
  CALL optListMat%add('MatrixType->matType',SPARSE)
  CALL optListMat%add('MatrixType->MPI_Comm_ID',PE_COMM_SELF)
  
  !Set up vector PL
  CALL vecPList%add('VectorType -> n',1)
  CALL vecPList%add('VectorType -> MPI_Comm_ID',PE_COMM_SELF)
  
#ifdef HAVE_PETSC    
  CALL PetscInitialize(PETSC_NULL_CHARACTER,ierr)
#endif

  WRITE(*,*) '==================================================='
  WRITE(*,*) 'TESTING MATRIX TYPES...'
  WRITE(*,*) '==================================================='
  
  CALL testMatrix()
  
  WRITE(*,*) '==================================================='
  WRITE(*,*) 'TESTING MATRIX TYPES PASSED!'
  WRITE(*,*) '==================================================='
  CALL optListMat%clear()
  CALL vecPList%clear()
  CALL pList%clear()
  CALL MatrixTypes_Clear_ValidParams()
  CALL VectorType_Clear_ValidParams()
  
#ifdef HAVE_PETSC    
      CALL PetscFinalize(ierr)
#endif
!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
    SUBROUTINE testMatrix()
      CLASS(MatrixType),ALLOCATABLE :: thisMatrix
      CLASS(VectorType),ALLOCATABLE :: xRealVector,yRealVector
      CLASS(VectorType),ALLOCATABLE :: xPETScVector,yPETScVector
      INTEGER(SIK) :: i
      INTEGER(SIK) :: matsize1,matsize2
      INTEGER(SIK) :: ia_vals(4)
      INTEGER(SIK) :: ja_vals(6)
      REAL(SRK) :: a_vals(6),x(3),y(3)
      REAL(SRK) :: dummy
      REAL(SRK),ALLOCATABLE :: dummyvec(:)
#ifdef HAVE_PETSC
      PetscErrorCode  :: ierr
#endif
      CALL vecPList%set('VectorType -> n',3)
      ALLOCATE(SparseMatrixType :: thisMatrix)
      ALLOCATE(RealVectorType :: xRealVector)
      ALLOCATE(RealVectorType :: yRealVector)
      CALL xRealVector%init(vecPList)
      CALL yRealVector%init(vecPlist)
#ifdef HAVE_PETSC
      ALLOCATE(PETScVectorType :: xPETScVector)
      ALLOCATE(PETScVectorType :: yPETScVector)
      CALL xPETScVector%init(vecPList)
      CALL yPETScVector%init(vecPlist)
#endif
      
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
      
      ! build parameter list
      CALL pList%add('MatrixType->n',10_SNK)
      CALL pList%add('MatrixType->nnz',10_SNK)
      CALL pList%validate(pList,optListMat)
      eMatrixType => NULL()
      CALL thisMatrix%init(pList)
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
      CALL pList%clear()
        
      !now check init without m being provided
      CALL pList%add('MatrixType->n',10_SNK)
      CALL pList%validate(pList,optListMat)
      CALL thisMatrix%init(pList) !expect exception
      IF(thisMatrix%isInit) THEN
        WRITE(*,*) 'CALL sparse%init(...) FAILED!'
        STOP 666
      ENDIF
      CALL thisMatrix%clear()
        
      !init it twice so on 2nd init, isInit==.TRUE.
      CALL thisMatrix%init(pList)
      SELECTTYPE(thisMatrix)
        TYPE IS(SparseMatrixType); thisMatrix%nnz=1
      ENDSELECT
      CALL thisMatrix%init(pList)
      SELECTTYPE(thisMatrix)
        TYPE IS(SparseMatrixType)
          IF(thisMatrix%nnz/=1) THEN !nnz/=1 implies it was changed, and thus fail
            WRITE(*,*) 'CALL sparse%init(...) FAILED!' !expect exception
            STOP 666
          ENDIF
      ENDSELECT
      !init with n<1
      CALL thisMatrix%clear()
      CALL pList%clear()
      CALL pList%add('MatrixType->n',-1_SNK)
      CALL pList%add('MatrixType->nnz',10_SNK)
      CALL pList%validate(pList,optListMat)
      CALL thisMatrix%init(pList) !expect exception
      IF(thisMatrix%isInit) THEN
        WRITE(*,*) 'CALL sparse%init(...) FAILED!'
        STOP 666
      ENDIF
      CALL thisMatrix%clear()
      CALL pList%clear()
      !n<1, and m not provided
      CALL pList%add('MatrixType->n',-1_SNK)
      CALL pList%validate(pList,optListMat)
      CALL thisMatrix%init(pList) !expect exception
      IF(thisMatrix%isInit) THEN
        WRITE(*,*) 'CALL sparse%init(...) FAILED!'
        STOP 666
      ENDIF
      CALL thisMatrix%clear()
      CALL pList%clear()
      !init with m<1
      CALL pList%add('MatrixType->n',10_SNK)
      CALL pList%add('MatrixType->nnz',-10_SNK)
      CALL pList%validate(pList,optListMat)
      CALL thisMatrix%init(pList) !expect exception
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
      
      ! build parameter list
      CALL pList%clear()
      CALL pList%add('MatrixType->n',3_SNK)
      CALL pList%add('MatrixType->nnz',6_SNK)
      CALL pList%validate(pList,optListMat)
      CALL thisMatrix%clear()
      CALL thisMatrix%init(pList)
        
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
          CALL thisMatrix%init(pList)
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
          CALL thisMatrix%init(pList)
          thisMatrix%isInit=.FALSE.
          CALL thisMatrix%SetShape(1,1,1._SRK)
          IF(thisMatrix%a(1) == 1._SRK) THEN
            WRITE(*,*) 'CALL sparse%setShape(...) FAILED!'
            STOP 666
          ENDIF
          CALL thisMatrix%clear()
          !test case where i and j are out of bounds (one at a time)
          CALL thisMatrix%init(pList)
          CALL thisMatrix%setShape(-1,1,2._SRK) !i<1
          IF(thisMatrix%a(1) == 2._SRK) THEN
            WRITE(*,*) 'CALL sparse%setShape(...) FAILED!'
            STOP 666
          ENDIF
          CALL thisMatrix%clear
          CALL thisMatrix%init(pList)
          CALL thisMatrix%setShape(1,-1,2._SRK) !j<1
          IF(thisMatrix%a(1) == 2._SRK) THEN
            WRITE(*,*) 'CALL sparse%setShape(...) FAILED!'
            STOP 666
          ENDIF
          CALL thisMatrix%clear
          CALL thisMatrix%init(pList)
          CALL thisMatrix%setShape(4,1,2._SRK) !i>n
          IF(thisMatrix%a(1) == 2._SRK) THEN
            WRITE(*,*) 'CALL sparse%setShape(...) FAILED!'
            STOP 666
          ENDIF
          !test check to see if i,j of new entry are below and to the right 
          !of previous i.j
          CALL thisMatrix%clear()
          CALL thisMatrix%init(pList)
          CALL thisMatrix%setShape(1,2,1._SRK)
          CALL thisMatrix%setShape(1,1,2._SRK)
          IF(thisMatrix%a(2) == 2._SRK) THEN
            WRITE(*,*) 'CALL sparse%setShape(...) FAILED!'
            STOP 666
          ENDIF
          CALL thisMatrix%clear
          CALL thisMatrix%init(pList)
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
          CALL thisMatrix%init(pList)
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
      CALL xRealVector%set(1.0_SRK)
      CALL yRealVector%set(1.0_SRK)
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=xRealVector,Y=yRealVector)
      CALL yRealVector%get(y)
      IF(ANY(.NOT.(y .APPROXEQ. (/4._SRK,4._SRK,16._SRK/)))) THEN
        WRITE(*,*) "CALL BLAS_matvec(THISMATRIX=thisMatrix,X=xRealVector,Y=yRealVector) -sparse FAILED!"
        STOP 666
      ENDIF
      y=1.0_SRK
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=x,BETA=2.0_SRK,Y=y)
      IF(ANY(.NOT.(y .APPROXEQ. (/5._SRK,5._SRK,17._SRK/)))) THEN
        WRITE(*,*) "CALL BLAS_matvec(THISMATRIX=thisMatrix,X=x,BETA=2.0_SRK,Y=y) -sparse FAILED!"
        STOP 666
      ENDIF
      CALL yRealVector%set(1.0_SRK)
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=xRealVector,BETA=2.0_SRK,Y=yRealVector)
      CALL yRealVector%get(y)
      IF(ANY(.NOT.(y .APPROXEQ. (/5._SRK,5._SRK,17._SRK/)))) THEN
        WRITE(*,*) "CALL BLAS_matvec(THISMATRIX=thisMatrix,X=xRealVector,BETA=2.0_SRK,Y=yRealVector) -sparse FAILED!"
        STOP 666
      ENDIF
      y=1.0_SRK
      CALL BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=x,Y=y)
      IF(ANY(.NOT.(y .APPROXEQ. (/7._SRK,7._SRK,31._SRK/)))) THEN
        WRITE(*,*) "CALL BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=x,Y=y) -sparse FAILED!"
        STOP 666
      ENDIF
      CALL yRealVector%set(1.0_SRK)
      CALL BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=xRealVector,Y=yRealVector)
      CALL yRealVector%get(y)
      IF(ANY(.NOT.(y .APPROXEQ. (/7._SRK,7._SRK,31._SRK/)))) THEN
        WRITE(*,*) "CALL BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=xRealVector,Y=yRealVector) -sparse FAILED!"
        STOP 666
      ENDIF
      y=1.0_SRK
      CALL BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=x,BETA=2.0_SRK,Y=y)
      IF(ANY(.NOT.(y .APPROXEQ. (/8._SRK,8._SRK,32._SRK/)))) THEN
        WRITE(*,*) "CALL BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=x,BETA=2.0_SRK,Y=y) -sparse FAILED!"
        STOP 666
      ENDIF
      CALL yRealVector%set(1.0_SRK)
      CALL BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=xRealVector,BETA=2.0_SRK,Y=yRealVector)
      CALL yRealVector%get(y)
      IF(ANY(.NOT.(y .APPROXEQ. (/8._SRK,8._SRK,32._SRK/)))) THEN
        WRITE(*,*) "CALL BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=xRealVector,BETA=2.0_SRK,Y=yRealVector) -sparse FAILED!"
        STOP 666
      ENDIF
      WRITE(*,*) '  Passed: CALL BLAS_matvec(...) sparse-matrix'
      
      !Test BLAS_matmult when SparseMatType is supported.
      CALL testMatrixMultSparse()
      
      !off-nominal tests
      !pass matrix w/out setshape
      CALL thisMatrix%clear()
      CALL pList%clear()
      CALL pList%add('MatrixType->n',3)
      CALL pList%add('MatrixType->nnz',6)
      CALL thisMatrix%init(pList)
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
      CALL thisMatrix%init(pList)
      SELECTTYPE(thisMatrix)
        TYPE IS(SparseMatrixType); CALL thisMatrix%setShape(1,1)
      ENDSELECT

      CALL thisMatrix%set(-1,1,1._SRK)
      CALL thisMatrix%set(1,-1,1._SRK)
      CALL thisMatrix%set(4,1,1._SRK)
      CALL thisMatrix%set(1,4,1._SRK)
      !no crash? good.
      !i,j not in pre-defined shape
      CALL pList%clear()
      CALL pList%add('MatrixType->n',3_SNK)
      CALL pList%add('MatrixType->nnz',6_SNK)
      CALL thisMatrix%clear()
      CALL thisMatrix%init(pList)
        
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
      CALL thisMatrix%init(pList)
      SELECTTYPE(thisMatrix)
        TYPE IS(SparseMatrixType)
          CALL thisMatrix%setShape(1,1,1._SRK)
          CALL thisMatrix%setShape(1,3,2._SRK)
          CALL thisMatrix%setShape(2,3,3._SRK)
          CALL thisMatrix%setShape(3,1,4._SRK)
          CALL thisMatrix%setShape(3,2,5._SRK)
          CALL thisMatrix%setShape(3,3,6._SRK)
          IF(ALLOCATED(dummyvec)) DEALLOCATE(dummyvec)
          ALLOCATE(dummyvec(9))
          CALL thisMatrix%get(1,1,dummyvec(1))
          CALL thisMatrix%get(2,1,dummyvec(2))
          CALL thisMatrix%get(3,1,dummyvec(3))
          CALL thisMatrix%get(1,2,dummyvec(4))
          CALL thisMatrix%get(2,2,dummyvec(5))
          CALL thisMatrix%get(3,2,dummyvec(6))
          CALL thisMatrix%get(1,3,dummyvec(7))
          CALL thisMatrix%get(2,3,dummyvec(8))
          CALL thisMatrix%get(3,3,dummyvec(9))
          IF(((dummyvec(1) /= 1._SRK)  .OR. &
              (dummyvec(2) /= 0._SRK)) .OR. &
              (dummyvec(3) /= 4._SRK)) THEN
            WRITE(*,*) 'CALL sparse%get(...) FAILED!' !column one check
            STOP 666
          ELSEIF(((dummyvec(4) /= 0._SRK).OR. &
                  (dummyvec(5) /= 0._SRK)) .OR. &
                  (dummyvec(6) /= 5._SRK)) THEN
            WRITE(*,*) 'CALL sparse%get(...) FAILED!' !column two check
            STOP 666
          ELSEIF(((dummyvec(7) /= 2._SRK).OR. &
                  (dummyvec(8) /= 3._SRK)) .OR. &
                  (dummyvec(9) /= 6._SRK)) THEN
            WRITE(*,*) 'CALL sparse%get(...) FAILED!' !column three check
            STOP 666
          ENDIF
      ENDSELECT
      !test with out of bounds i,j, make sure no crash.
      SELECTTYPE(thisMatrix)
        TYPE IS(SparseMatrixType)
          CALL thisMatrix%get(4,2,dummy)
          IF(dummy /= -1051._SRK) THEN
            WRITE(*,*) 'CALL sparse%get(...) FAILED!' !column one check
            STOP 666
          ENDIF
          CALL thisMatrix%get(-1,2,dummy)
          IF(dummy/=-1051._SRK) THEN
            WRITE(*,*) 'CALL sparse%get(...) FAILED!' !column one check
            STOP 666
          ENDIF
          CALL thisMatrix%get(2,-1,dummy)
          IF(dummy/=-1051._SRK) THEN
            WRITE(*,*) 'CALL sparse%get(...) FAILED!' !column one check
            STOP 666
          ENDIF
      ENDSELECT
      !test get with uninit, make sure no crash.
      CALL thisMatrix%clear()
      SELECTTYPE(thisMatrix)
        TYPE IS(SparseMatrixType)      
          CALL thisMatrix%get(1,1,dummy)
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
      CALL pList%clear()
      CALL pList%add('MatrixType->n',10_SNK)
      CALL pList%add('MatrixType->isSym',.FALSE.)
      CALL pList%validate(pList,optListMat)
      eMatrixType => NULL()
      CALL thisMatrix%init(pList) !n=10, not symmetric
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
      CALL pList%clear()
      CALL pList%add('MatrixType->n',10_SNK)
      CALL pList%add('MatrixType->isSym',.TRUE.)
      CALL pList%validate(pList,optListMat)
      CALL thisMatrix%init(pList) !n=10, symmetric
      SELECTTYPE(thisMatrix)
        TYPE IS(DenseSquareMatrixType)
          IF(((.NOT. thisMatrix%isInit).OR.(thisMatrix%n /= 10)) &
              .OR.(.NOT. thisMatrix%isSymmetric)) THEN
            WRITE(*,*) 'CALL densesquare%init(...) FAILED!'
            STOP 666
          ENDIF
      ENDSELECT
      CALL thisMatrix%clear()
      !test with double init (isInit==true on 2nd try)
      CALL pList%clear()
      CALL pList%add('MatrixType->n',10_SNK)
      CALL pList%add('MatrixType->isSym',.TRUE.)
      CALL pList%validate(pList,optListMat)
      CALL thisMatrix%init(pList) !n=10, symmetric
      SELECTTYPE(thisMatrix)
        TYPE IS(DenseSquareMatrixType); thisMatrix%isSymmetric=.FALSE.
      ENDSELECT
      CALL thisMatrix%init(pList) !n=10, symmetric
      SELECTTYPE(thisMatrix)
        TYPE IS(DenseSquareMatrixType)
          IF(thisMatrix%isSymmetric) THEN
            WRITE(*,*) 'CALL densesquare%init(...) FAILED!'
            STOP 666
          ENDIF
      ENDSELECT
      CALL thisMatrix%clear()
      !test with n<1
      CALL pList%clear()
      CALL pList%add('MatrixType->n',-1_SNK)
      CALL pList%add('MatrixType->isSym',.TRUE.)
      CALL pList%validate(pList,optListMat)
      CALL thisMatrix%init(pList) !expect exception
      IF(thisMatrix%isInit) THEN
        WRITE(*,*) 'CALL densesquare%init(...) FAILED!'
        STOP 666
      ENDIF
      CALL thisMatrix%clear()
      !test with n<1 and no second parameter
      CALL pList%clear()
      CALL pList%add('MatrixType->n',-1_SNK)
      CALL pList%validate(pList,optListMat)
      CALL thisMatrix%init(pList) !expect exception
      IF(thisMatrix%isInit) THEN
        WRITE(*,*) 'CALL densesquare%init(...) FAILED!'
        STOP 666
      ENDIF
      WRITE(*,*) '  Passed: CALL densesquare%init(...)'
      !check set
      !test normal use case (symmetric and nonsymmetric)
      !want to build:
      ![1 2]
      ![2 3]
      CALL thisMatrix%clear()
      CALL pList%clear()
      CALL pList%add('MatrixType->n',2_SNK)
      CALL pList%add('MatrixType->isSym',.TRUE.)
      CALL thisMatrix%init(pList)  !symmetric
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
      SELECTTYPE(thisMatrix)
        TYPE IS(DenseSquareMatrixType)
      ENDSELECT
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=x,Y=y)
      IF(ANY(.NOT.(y .APPROXEQ. (/4._SRK,6._SRK,1._SRK/)))) THEN
        WRITE(*,*) "CALL BLAS_matvec(THISMATRIX=thisMatrix,X=x,Y=y) -densesq FAILED!"
        STOP 666
      ENDIF
      CALL xRealVector%set(1.0_SRK)
      CALL yRealVector%set(1.0_SRK)
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=xRealVector,Y=yRealVector)
      CALL yRealVector%get(y)
      IF(ANY(.NOT.(y .APPROXEQ. (/4._SRK,6._SRK,1._SRK/)))) THEN
        WRITE(*,*) "CALL BLAS_matvec(THISMATRIX=thisMatrix,X=xRealVector,Y=yRealVector) -densesq FAILED!"
        STOP 666
      ENDIF
      y=1.0_SRK
      CALL BLAS_matvec(THISMATRIX=thisMatrix,trans='t',X=x,Y=y)
      IF(ANY(.NOT.(y .APPROXEQ. (/4._SRK,6._SRK,1._SRK/)))) THEN
        WRITE(*,*) "CALL BLAS_matvec(THISMATRIX=thisMatrix,X=x,Y=y) -densesq FAILED!"
        STOP 666
      ENDIF
      CALL yRealVector%set(1.0_SRK)
      CALL BLAS_matvec(THISMATRIX=thisMatrix,trans='t',X=xRealVector,Y=yRealVector)
      CALL yRealVector%get(y)
      IF(ANY(.NOT.(y .APPROXEQ. (/4._SRK,6._SRK,1._SRK/)))) THEN
        WRITE(*,*) "CALL BLAS_matvec(THISMATRIX=thisMatrix,X=xRealVector,Y=yRealVector) -densesq FAILED!"
        STOP 666
      ENDIF
      y=1.0_SRK
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=x,BETA=2.0_SRK,Y=y)
      IF(ANY(.NOT.(y .APPROXEQ. (/5._SRK,7._SRK,1._SRK/)))) THEN
        WRITE(*,*) "CALL BLAS_matvec(THISMATRIX=thisMatrix,X=x,BETA=2.0_SRK,Y=y) -densesq FAILED!"
        STOP 666
      ENDIF
      CALL yRealVector%set(1.0_SRK)
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=xRealVector,BETA=2.0_SRK,Y=yRealVector)
      CALL yRealVector%get(y)
      IF(ANY(.NOT.(y .APPROXEQ. (/5._SRK,7._SRK,1._SRK/)))) THEN
        WRITE(*,*) "CALL BLAS_matvec(THISMATRIX=thisMatrix,X=xRealVector,BETA=2.0_SRK,Y=yRealVector) -densesq FAILED!"
        STOP 666
      ENDIF
      y=1.0_SRK
      CALL BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=x,Y=y)
      IF(ANY(.NOT.(y .APPROXEQ. (/7._SRK,11._SRK,1._SRK/)))) THEN
        WRITE(*,*) "CALL BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=x,Y=y) -densesq FAILED!"
        STOP 666
      ENDIF
      CALL yRealVector%set(1.0_SRK)
      CALL BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=xRealVector,Y=yRealVector)
      CALL yRealVector%get(y)
      IF(ANY(.NOT.(y .APPROXEQ. (/7._SRK,11._SRK,1._SRK/)))) THEN
        WRITE(*,*) "CALL BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=xRealVector,Y=yRealVector) -densesq FAILED!"
        STOP 666
      ENDIF
      y=1.0_SRK
      CALL BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=x,BETA=2.0_SRK,Y=y)
      IF(ANY(.NOT.(y .APPROXEQ. (/8._SRK,12._SRK,1._SRK/)))) THEN
        WRITE(*,*) "CALL BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=x,BETA=2.0_SRK,Y=y) -densesq FAILED!"
        STOP 666
      ENDIF
      CALL yRealVector%set(1.0_SRK)
      CALL BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=xRealVector,BETA=2.0_SRK,Y=yRealVector)
      CALL yRealVector%get(y)
      IF(ANY(.NOT.(y .APPROXEQ. (/8._SRK,12._SRK,1._SRK/)))) THEN
        WRITE(*,*) "CALL BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=xRealVector,"// &
          "BETA=2.0_SRK,Y=yRealVector) -densesq FAILED!"
        STOP 666
      ENDIF
      CALL thisMatrix%clear()
      y=2._SRK
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=x,BETA=2.0_SRK,Y=y) !Error check uninit
      IF(ANY(.NOT.(y .APPROXEQ. (/2._SRK,2._SRK,2._SRK/)))) THEN
        WRITE(*,*) "CALL BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=x,BETA=2.0_SRK,Y=y) -densesq FAILED!"
        STOP 666
      ENDIF
      CALL yRealVector%set(2._SRK)
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=xRealVector,BETA=2.0_SRK,Y=yRealVector) !Error check uninit
      CALL yRealVector%get(y)
      IF(ANY(.NOT.(y .APPROXEQ. (/2._SRK,2._SRK,2._SRK/)))) THEN
        WRITE(*,*) "CALL BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=xRealVector,"// &
          "BETA=2.0_SRK,Y=yRealVector) -densesq FAILED!"
        STOP 666
      ENDIF
      WRITE(*,*) '  Passed: CALL BLAS_matvec(...) densesq-matrix'
      
      CALL testMatrixMultSquare()
      
      CALL pList%clear()
      CALL pList%add('MatrixType->n',2_SNK)
      CALL pList%add('MatrixType->isSym',.FALSE.)
      CALL thisMatrix%init(pList)  !nonsymmetric
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
      CALL thisMatrix%init(pList)
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
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=xRealVector,Y=yRealVector) !Error check unsupported type
      CALL yRealVector%get(y)
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
      CALL pList%clear()
      CALL pList%add('MatrixType->n',10_SNK)
      CALL pList%add('MatrixType->isSym',.FALSE.)
      CALL pList%validate(pList,optListMat)
      eMatrixType => NULL()
      CALL thisMatrix%init(pList) !n=10, not symmetric
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
      CALL pList%clear()
      CALL pList%add('MatrixType->n',10_SNK)
      CALL pList%add('MatrixType->isSym',.TRUE.)
      CALL pList%validate(pList,optListMat)
      CALL thisMatrix%init(pList) !n=10, symmetric
      SELECTTYPE(thisMatrix)
        TYPE IS(TriDiagMatrixType)
          IF(((.NOT. thisMatrix%isInit).OR.(thisMatrix%n /= 10)) &
              .OR.(.NOT. thisMatrix%isSymmetric)) THEN
            WRITE(*,*) 'CALL tridiag%init(...) FAILED!'
            STOP 666
          ENDIF
      ENDSELECT    
      CALL thisMatrix%clear()
      !test with double init (isInit==true on 2nd try)
      CALL pList%clear()
      CALL pList%add('MatrixType->n',10_SNK)
      CALL pList%add('MatrixType->isSym',.TRUE.)
      CALL pList%validate(pList,optListMat)
      CALL thisMatrix%init(pList) !n=10, symmetric
      SELECTTYPE(thisMatrix)
        TYPE IS(TriDiagMatrixType); thisMatrix%isSymmetric=.FALSE.
      ENDSELECT
      CALL thisMatrix%init(pList) !n=10, symmetric
      SELECTTYPE(thisMatrix)
        TYPE IS(TriDiagMatrixType)
          IF(thisMatrix%isSymmetric) THEN
            WRITE(*,*) 'CALL tridiag%init(...) FAILED!'
            STOP 666
          ENDIF
      ENDSELECT
      CALL thisMatrix%clear()
      !test with n<1
      CALL pList%clear()
      CALL pList%add('MatrixType->n',-1_SNK)
      CALL pList%add('MatrixType->isSym',.TRUE.)
      CALL pList%validate(pList,optListMat)
      CALL thisMatrix%init(pList) !expect exception
      IF(thisMatrix%isInit) THEN
        WRITE(*,*) 'CALL tridiag%init(...) FAILED!'
        STOP 666
      ENDIF
      CALL thisMatrix%clear()
      !test with n<1 and no second parameter
      CALL pList%clear()
      CALL pList%add('MatrixType->n',-1_SNK)
      CALL pList%validate(pList,optListMat)
      CALL thisMatrix%init(pList) !expect exception
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
      CALL pList%clear()
      CALL pList%add('MatrixType->n',3_SNK)
      CALL pList%add('MatrixType->isSym',.TRUE.)
      CALL thisMatrix%init(pList)  !symmetric
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
      CALL pList%clear()
      CALL pList%add('MatrixType->n',3_SNK)
      CALL pList%add('MatrixType->isSym',.FALSE.)
      CALL thisMatrix%init(pList)  !nonsymmetric
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
      CALL pList%clear()
      CALL pList%add('MatrixType->n',4_SNK)
      CALL pList%add('MatrixType->isSym',.FALSE.)
      CALL thisMatrix%init(pList)
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
      CALL pList%clear()
      CALL pList%add('MatrixType->n',10_SNK)
      CALL pList%add('MatrixType->m',15_SNK)
      CALL pList%validate(pList,optListMat)
      eMatrixType => NULL()
      CALL thisMatrix%init(pList)
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
      CALL thisMatrix%init(pList)
      SELECTTYPE(thisMatrix)
        TYPE IS(DenseRectMatrixType); thisMatrix%m=1
      ENDSELECT
      CALL thisMatrix%init(pList)
      SELECTTYPE(thisMatrix)
        TYPE IS(DenseRectMatrixType)
          IF(thisMatrix%m /= 1) THEN
            WRITE(*,*) 'CALL denserect%init(...) FAILED!'
            STOP 666
          ENDIF
      ENDSELECT
      CALL thisMatrix%clear()
      !test with n<1
      CALL pList%clear()
      CALL pList%add('MatrixType->n',-1_SNK)
      CALL pList%add('MatrixType->m',10_SNK)
      CALL pList%validate(pList,optListMat)
      CALL thisMatrix%init(pList) !expect exception
      IF(thisMatrix%isInit) THEN
        WRITE(*,*) 'CALL denserect%init(...) FAILED!'
        STOP 666
      ENDIF
      CALL thisMatrix%clear()
      !test with m<1
      CALL pList%clear()
      CALL pList%add('MatrixType->n',10_SNK)
      CALL pList%add('MatrixType->m',-1_SNK)
      CALL pList%validate(pList,optListMat)
      CALL thisMatrix%init(pList) !expect exception
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
      CALL pList%clear()
      CALL pList%add('MatrixType->n',2_SNK)
      CALL pList%add('MatrixType->m',3_SNK)
      CALL thisMatrix%init(pList)
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
      CALL xRealVector%set(1.0_SRK)
      CALL yRealVector%set(1.0_SRK)
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=xRealVector,Y=yRealVector)
      CALL yRealVector%get(y)
      IF(ANY(.NOT.(y .APPROXEQ. (/7._SRK,16._SRK,1._SRK/)))) THEN
        WRITE(*,*) "CALL BLAS_matvec(THISMATRIX=thisMatrix,X=xRealVector,Y=yRealVector) -denserect FAILED!"
        STOP 666
      ENDIF
      y=1.0_SRK
      CALL BLAS_matvec(THISMATRIX=thisMatrix,trans='t',X=x,Y=y)
      IF(ANY(.NOT.(y .APPROXEQ. (/6._SRK,8._SRK,10._SRK/)))) THEN
        WRITE(*,*) "CALL BLAS_matvec(THISMATRIX=thisMatrix,X=x,Y=y) -denserect FAILED!"
        STOP 666
      ENDIF
      CALL yRealVector%set(1.0_SRK)
      CALL BLAS_matvec(THISMATRIX=thisMatrix,trans='t',X=xRealVector,Y=yRealVector)
      CALL yRealVector%get(y)
      IF(ANY(.NOT.(y .APPROXEQ. (/6._SRK,8._SRK,10._SRK/)))) THEN
        WRITE(*,*) "CALL BLAS_matvec(THISMATRIX=thisMatrix,X=xRealVector,Y=yRealVector) -denserect FAILED!"
        STOP 666
      ENDIF
      y=1.0_SRK
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=x,BETA=2.0_SRK,Y=y)
      IF(ANY(.NOT.(y .APPROXEQ. (/8._SRK,17._SRK,1._SRK/)))) THEN
        WRITE(*,*) "CALL BLAS_matvec(THISMATRIX=thisMatrix,X=x,BETA=2.0_SRK,Y=y) -denserect FAILED!"
        STOP 666
      ENDIF
      CALL yRealVector%set(1.0_SRK)
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=xRealVector,BETA=2.0_SRK,Y=yRealVector)
      CALL yRealVector%get(y)
      IF(ANY(.NOT.(y .APPROXEQ. (/8._SRK,17._SRK,1._SRK/)))) THEN
        WRITE(*,*) "CALL BLAS_matvec(THISMATRIX=thisMatrix,X=xRealVector,BETA=2.0_SRK,Y=yRealVector) -denserect FAILED!"
        STOP 666
      ENDIF
      y=1.0_SRK
      CALL BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=x,Y=y)
      IF(ANY(.NOT.(y .APPROXEQ. (/13._SRK,31._SRK,1._SRK/)))) THEN
        WRITE(*,*) "CALL BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=x,Y=y) -denserect FAILED!"
        STOP 666
      ENDIF
      CALL yRealVector%set(1.0_SRK)
      CALL BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=xRealVector,Y=yRealVector)
      CALL yRealVector%get(y)
      IF(ANY(.NOT.(y .APPROXEQ. (/13._SRK,31._SRK,1._SRK/)))) THEN
        WRITE(*,*) "CALL BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=xRealVector,Y=yRealVector) -denserect FAILED!"
        STOP 666
      ENDIF
      y=1.0_SRK
      CALL BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=x,BETA=2.0_SRK,Y=y)
      IF(ANY(.NOT.(y .APPROXEQ. (/14._SRK,32._SRK,1._SRK/)))) THEN
        WRITE(*,*) "CALL BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=x,BETA=2.0_SRK,Y=y) -denserect FAILED!"
        STOP 666
      ENDIF
      CALL yRealVector%set(1.0_SRK)
      CALL BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=xRealVector,BETA=2.0_SRK,Y=yRealVector)
      CALL yRealVector%get(y)
      IF(ANY(.NOT.(y .APPROXEQ. (/14._SRK,32._SRK,1._SRK/)))) THEN
        WRITE(*,*) "CALL BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=xRealVector,BETA=2.0_SRK,Y=yRealVector)", & 
                   " -denserect FAILED!"
        STOP 666
      ENDIF
      WRITE(*,*) '  Passed: CALL BLAS_matvec(...) denserect-matrix'
      
      CALL testMatrixMultRect()
      
      !check matrix that hasnt been init, i,j out of bounds
      CALL thisMatrix%clear()
      CALL thisMatrix%set(1,1,1._SRK) 
      CALL thisMatrix%clear()
      CALL pList%clear()
      CALL pList%add('MatrixType->n',2_SNK)
      CALL pList%add('MatrixType->m',3_SNK)
      CALL thisMatrix%init(pList)
      CALL thisMatrix%set(-1,1,1._SRK)
      CALL thisMatrix%set(1,-1,1._SRK)
      CALL thisMatrix%set(5,1,1._SRK)
      CALL thisMatrix%set(1,5,1._SRK)
      !no crash? good
      CALL thisMatrix%clear()
      WRITE(*,*) '  Passed: CALL denserect%set(...)'
      DEALLOCATE(thisMatrix)
      
!Test for PETSc matrices (if necessary)
#ifdef HAVE_PETSC  

!Test for PETSc sparsematrices 
      ALLOCATE(PETScMatrixType :: thisMatrix)
      SELECTTYPE(thisMatrix)
        TYPE IS(PETScMatrixType)
          !test clear
          !make matrix w/out using untested init
          thisMatrix%isInit=.TRUE.
          thisMatrix%n=10
          thisMatrix%isSymmetric=.TRUE.
          CALL MatCreate(MPI_COMM_WORLD,thisMatrix%a,ierr)
          CALL MatSetSizes(thisMatrix%a,PETSC_DECIDE,PETSC_DECIDE, &
                                        thisMatrix%n,thisMatrix%n,ierr)
          CALL MatSetType(thisMatrix%a,MATMPIAIJ,ierr)
          CALL MatSetUp(thisMatrix%a,ierr)
      ENDSELECT
      CALL thisMatrix%clear()
      SELECTTYPE(thisMatrix)
        TYPE IS(PETScMatrixType)
          IF(((thisMatrix%isInit).OR.(thisMatrix%n /= 0)) &
              .OR.((thisMatrix%isSymmetric))) THEN
            WRITE(*,*) 'CALL petscsparse%clear() FAILED!'
            STOP 666
          ENDIF
          IF(thisMatrix%a /= PETSC_NULL_REAL) THEN
            WRITE(*,*) 'CALL petscsparse%clear() FAILED!'
            STOP 666
          ENDIF
          WRITE(*,*) '  Passed: CALL petscsparse%clear()'
      ENDSELECT
      !check init
      CALL pList%clear()
      CALL pList%add('MatrixType->n',10_SNK)
      CALL pList%add('MatrixType->isSym',.FALSE.)
      CALL pList%add('MatrixType->matType',SPARSE)
      CALL pList%validate(pList,optListMat)
      eMatrixType => NULL()
      CALL thisMatrix%init(pList) !n=10, not symmetric (0), sparse (0)
      eMatrixType => e
      SELECTTYPE(thisMatrix)
        TYPE IS(PETScMatrixType)
          IF(((.NOT. thisMatrix%isInit).OR.(thisMatrix%n /= 10)) &
              .OR.(thisMatrix%isSymmetric)) THEN
            WRITE(*,*) 'CALL petscsparse%init(...) FAILED!'
            STOP 666
          ENDIF
          CALL MatGetSize(thisMatrix%a,matsize1,matsize2,ierr)
          IF((matsize1 /= 10) .OR. (matsize2 /= 10)) THEN
            WRITE(*,*) 'CALL petscsparse%init(...) FAILED!'
            STOP 666
          ENDIF
      ENDSELECT  
      CALL thisMatrix%clear()
      CALL pList%clear()
      CALL pList%add('MatrixType->n',10_SNK)
      CALL pList%add('MatrixType->isSym',.TRUE.)
      CALL pList%add('MatrixType->matType',SPARSE)
      CALL pList%validate(pList,optListMat)
      CALL thisMatrix%init(pList) !n=10, symmetric (1), sparse (0)
      SELECTTYPE(thisMatrix)
        TYPE IS(PETScMatrixType)
          IF(((.NOT. thisMatrix%isInit).OR.(thisMatrix%n /= 10)) &
              .OR.(.NOT. thisMatrix%isSymmetric)) THEN
            WRITE(*,*) 'CALL petscsparse%init(...) FAILED!'
            STOP 666
          ENDIF
      ENDSELECT
      CALL thisMatrix%clear()
      !test with double init (isInit==true on 2nd try)
      CALL pList%clear()
      CALL pList%add('MatrixType->n',10_SNK)
      CALL pList%add('MatrixType->isSym',.TRUE.)
      CALL pList%add('MatrixType->matType',SPARSE)
      CALL pList%validate(pList,optListMat)
      CALL thisMatrix%init(pList) !n=10, symmetric (1), sparse (0)
      SELECTTYPE(thisMatrix)
        TYPE IS(PETScMatrixType); thisMatrix%isSymmetric=.FALSE.
      ENDSELECT
      CALL thisMatrix%init(pList) !n=10, symmetric (1), sparse (0)
      SELECTTYPE(thisMatrix)
        TYPE IS(PETScMatrixType)
          IF(thisMatrix%isSymmetric) THEN
            WRITE(*,*) 'CALL petscsparse%init(...) FAILED!'
            STOP 666
          ENDIF
      ENDSELECT
      CALL thisMatrix%clear()
      !test with n<1
      CALL pList%clear()
      CALL pList%add('MatrixType->n',-1_SNK)
      CALL pList%add('MatrixType->isSym',.TRUE.)
      CALL pList%add('MatrixType->matType',SPARSE)
      CALL pList%validate(pList,optListMat)
      CALL thisMatrix%init(pList) !expect exception
      IF(thisMatrix%isInit) THEN
        WRITE(*,*) 'CALL petscsparse%init(...) FAILED!'
        STOP 666
      ENDIF
      CALL thisMatrix%clear()
      WRITE(*,*) '  Passed: CALL petscsparse%init(...)'
      
      !check set
      !test normal use case (symmetric and nonsymmetric)
      !want to build:
      ![1 2]
      ![2 3]
      CALL pList%clear()
      CALL pList%add('MatrixType->n',2_SNK)
      CALL pList%add('MatrixType->isSym',.TRUE.)
      CALL pList%add('MatrixType->matType',SPARSE)
      CALL pList%validate(pList,optListMat)
      CALL thisMatrix%init(pList)  ! symmetric (1), sparse (0)
      CALL thisMatrix%set(1,1,1._SRK)
      CALL thisMatrix%set(1,2,2._SRK)
      CALL thisMatrix%set(2,2,3._SRK)
      SELECTTYPE(thisMatrix)
        TYPE IS(PETScMatrixType)
          ! manually assemble
          CALL MatAssemblyBegin(thisMatrix%a,MAT_FINAL_ASSEMBLY,ierr)
          CALL MatAssemblyEnd(thisMatrix%a,MAT_FINAL_ASSEMBLY,ierr)
          thisMatrix%isAssembled=.TRUE.
          
          CALL MatGetValues(thisMatrix%a,1,0,1,0,dummy,ierr)
          IF(dummy/=1._SRK)THEN
            WRITE(*,*) 'CALL petscsparse%set(...) FAILED!'
            STOP 666
          ENDIF
          CALL MatGetValues(thisMatrix%a,1,0,1,1,dummy,ierr)
          IF(dummy/=2._SRK)THEN
            WRITE(*,*) 'CALL petscsparse%set(...) FAILED!'
            STOP 666
          ENDIF
          CALL MatGetValues(thisMatrix%a,1,1,1,1,dummy,ierr)
          IF(dummy/=3._SRK)THEN
            WRITE(*,*) 'CALL petscsparse%set(...) FAILED!'
            STOP 666
          ENDIF
      ENDSELECT
      CALL thisMatrix%clear()
     
      CALL pList%clear()
      CALL pList%add('MatrixType->n',2_SNK)
      CALL pList%add('MatrixType->isSym',.FALSE.)
      CALL pList%add('MatrixType->mattype',SPARSE)
      CALL pList%validate(pList,optListMat)
      CALL thisMatrix%init(pList)  ! nonsymmetric (0), sparse (0)
      CALL thisMatrix%set(1,1,1._SRK)
      CALL thisMatrix%set(1,2,2._SRK)
      CALL thisMatrix%set(2,1,2._SRK)
      CALL thisMatrix%set(2,2,3._SRK)
      SELECTTYPE(thisMatrix)
        TYPE IS(PETScMatrixType)
          ! manually assemble
          CALL MatAssemblyBegin(thisMatrix%a,MAT_FINAL_ASSEMBLY,ierr)
          CALL MatAssemblyEnd(thisMatrix%a,MAT_FINAL_ASSEMBLY,ierr)
          thisMatrix%isAssembled=.TRUE.
          
          CALL MatGetValues(thisMatrix%a,1,0,1,0,dummy,ierr)
          IF(dummy/=1._SRK)THEN
            WRITE(*,*) 'CALL petscsparse%set(...) FAILED!'
            STOP 666
          ENDIF
          CALL MatGetValues(thisMatrix%a,1,0,1,1,dummy,ierr)
          IF(dummy/=2._SRK)THEN
            WRITE(*,*) 'CALL petscsparse%set(...) FAILED!'
            STOP 666
          ENDIF
          CALL MatGetValues(thisMatrix%a,1,1,1,0,dummy,ierr)
          IF(dummy/=2._SRK)THEN
            WRITE(*,*) 'CALL petscsparse%set(...) FAILED!'
            STOP 666
          ENDIF
          CALL MatGetValues(thisMatrix%a,1,1,1,1,dummy,ierr)
          IF(dummy/=3._SRK)THEN
            WRITE(*,*) 'CALL petscsparse%set(...) FAILED!'
            STOP 666
          ENDIF
      ENDSELECT    
      !check matrix that hasnt been init, i,j out of bounds
      CALL thisMatrix%clear()
      CALL thisMatrix%set(1,1,1._SRK) 
      CALL thisMatrix%clear()
      CALL thisMatrix%init(pList)
      CALL thisMatrix%set(-1,1,1._SRK)
      CALL thisMatrix%set(1,-1,1._SRK)
      CALL thisMatrix%set(5,1,1._SRK)
      CALL thisMatrix%set(1,5,1._SRK)
      !no crash? good
      WRITE(*,*) '  Passed: CALL petscsparse%set(...)'
      
      CALL thisMatrix%clear()
      CALL pList%clear()
      CALL pList%add('MatrixType->n',3_SNK)
      CALL pList%add('MatrixType->isSym',.FALSE.)
      CALL pList%add('MatrixType->mattype',SPARSE)
      CALL pList%validate(pList,optListMat)
      CALL thisMatrix%init(pList)  !symmetric
      CALL thisMatrix%set(1,1,1._SRK)
      CALL thisMatrix%set(1,3,2._SRK)
      CALL thisMatrix%set(2,2,4._SRK)
      CALL thisMatrix%set(2,3,3._SRK)
      CALL thisMatrix%set(3,1,4._SRK)
      CALL thisMatrix%set(3,2,5._SRK)
      CALL thisMatrix%set(3,3,6._SRK)
      x=1.0_SRK
      y=1.0_SRK
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=x,Y=y)
      IF(ANY(.NOT.(y .APPROXEQ. (/4._SRK,8._SRK,16._SRK/)))) THEN
        WRITE(*,*) "CALL BLAS_matvec(THISMATRIX=thisMatrix,X=x,Y=y) -petscsparse FAILED!"
        STOP 666
      ENDIF
      CALL xRealVector%set(1.0_SRK)
      CALL yRealVector%set(1.0_SRK)
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=xRealVector,Y=yRealVector)
      CALL yRealVector%get(y)
      IF(ANY(.NOT.(y .APPROXEQ. (/4._SRK,8._SRK,16._SRK/)))) THEN
        WRITE(*,*) "CALL BLAS_matvec(THISMATRIX=thisMatrix,X=xRealVector,Y=yRealVector) -petscsparse FAILED!"
        STOP 666
      ENDIF
      CALL xPETScVector%set(1.0_SRK)
      CALL yPETScVector%set(1.0_SRK)
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=xPETScVector,Y=yPETScVector)
      CALL yRealVector%get(y)
      IF(ANY(.NOT.(y .APPROXEQ. (/4._SRK,8._SRK,16._SRK/)))) THEN
        WRITE(*,*) "CALL BLAS_matvec(THISMATRIX=thisMatrix,X=PETScVector,Y=yPETScVector) -petscsparse FAILED!"
        STOP 666
      ENDIF
      y=1.0_SRK
      CALL BLAS_matvec(THISMATRIX=thisMatrix,trans='t',X=x,Y=y)
      IF(ANY(.NOT.(y .APPROXEQ. (/6._SRK,10._SRK,12._SRK/)))) THEN
        WRITE(*,*) "CALL BLAS_matvec(THISMATRIX=thisMatrix,trans='t',X=x,Y=y) -petscsparse FAILED!"
        STOP 666
      ENDIF
      CALL yRealVector%set(1.0_SRK)
      CALL BLAS_matvec(THISMATRIX=thisMatrix,trans='t',X=xRealVector,Y=yRealVector)
      CALL yRealVector%get(y)
      IF(ANY(.NOT.(y .APPROXEQ. (/6._SRK,10._SRK,12._SRK/)))) THEN
        WRITE(*,*) "CALL BLAS_matvec(THISMATRIX=thisMatrix,X=xRealVector,Y=yRealVector) -petscsparse FAILED!"
        STOP 666
      ENDIF
      CALL yPETScVector%set(1.0_SRK)
      CALL BLAS_matvec(THISMATRIX=thisMatrix,trans='t',X=xPETScVector,Y=yPETScVector)
      CALL yPETScVector%get(y)
      IF(ANY(.NOT.(y .APPROXEQ. (/6._SRK,10._SRK,12._SRK/)))) THEN
        WRITE(*,*) "CALL BLAS_matvec(THISMATRIX=thisMatrix,trans='t',X=xPETScVector,Y=yPETScVector) -petscsparse FAILED!"
        STOP 666
      ENDIF
      y=1.0_SRK
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=x,BETA=2.0_SRK,Y=y)
      IF(ANY(.NOT.(y .APPROXEQ. (/5._SRK,9._SRK,17._SRK/)))) THEN
        WRITE(*,*) "CALL BLAS_matvec(THISMATRIX=thisMatrix,X=x,BETA=2.0_SRK,Y=y) -petscsparse FAILED!"
        STOP 666
      ENDIF
      CALL yRealVector%set(1.0_SRK)
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=xRealVector,BETA=2.0_SRK,Y=yRealVector)
      CALL yRealVector%get(y)
      IF(ANY(.NOT.(y .APPROXEQ. (/5._SRK,9._SRK,17._SRK/)))) THEN
        WRITE(*,*) "CALL BLAS_matvec(THISMATRIX=thisMatrix,X=xRealVector,BETA=2.0_SRK,Y=yRealVector) -petscsparse FAILED!"
        STOP 666
      ENDIF
      CALL yPETScVector%set(1.0_SRK)
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=xPETScVector,BETA=2.0_SRK,Y=yPETScVector)
      CALL yPETScVector%get(y)
      IF(ANY(.NOT.(y .APPROXEQ. (/5._SRK,9._SRK,17._SRK/)))) THEN
        WRITE(*,*) "CALL BLAS_matvec(THISMATRIX=thisMatrix,X=xPETScVector,BETA=2.0_SRK,Y=yPETScVector) -petscsparse FAILED!"
        STOP 666
      ENDIF
      y=1.0_SRK
      CALL BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=x,Y=y)
      IF(ANY(.NOT.(y .APPROXEQ. (/7._SRK,15._SRK,31._SRK/)))) THEN
        WRITE(*,*) "CALL BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=x,Y=y) -petscsparse FAILED!"
        STOP 666
      ENDIF
      CALL yRealVector%set(1.0_SRK)
      CALL BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=xRealVector,Y=yRealVector)
      CALL yRealVector%get(y)
      IF(ANY(.NOT.(y .APPROXEQ. (/7._SRK,15._SRK,31._SRK/)))) THEN
        WRITE(*,*) "CALL BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=xRealVector,Y=yRealVector) -petscsparse FAILED!"
        STOP 666
      ENDIF
      CALL yPETScVector%set(1.0_SRK)
      CALL BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=xPETScVector,Y=yPETScVector)
      CALL yPETScVector%get(y)
      IF(ANY(.NOT.(y .APPROXEQ. (/7._SRK,15._SRK,31._SRK/)))) THEN
        WRITE(*,*) "CALL BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=xPETScVector,Y=yPETScVector) -petscsparse FAILED!"
        STOP 666
      ENDIF
      y=1.0_SRK
      CALL BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=x,BETA=2.0_SRK,Y=y)
      IF(ANY(.NOT.(y .APPROXEQ. (/8._SRK,16._SRK,32._SRK/)))) THEN
        WRITE(*,*) "CALL BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=x,BETA=2.0_SRK,Y=y) -petscsparse FAILED!"
        STOP 666
      ENDIF
      CALL yRealVector%set(1.0_SRK)
      CALL BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=xRealVector,BETA=2.0_SRK,Y=yRealVector)
      CALL yRealVector%get(y)
      IF(ANY(.NOT.(y .APPROXEQ. (/8._SRK,16._SRK,32._SRK/)))) THEN
        WRITE(*,*) "CALL BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=xRealVector,BETA=2.0_SRK,Y=yRealVector)", &
                   "-petscsparse FAILED!"
        STOP 666
      ENDIF
      CALL yPETScVector%set(1.0_SRK)
      CALL BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=xPETScVector,BETA=2.0_SRK,Y=yPETScVector)
      CALL yPETScVector%get(y)
      IF(ANY(.NOT.(y .APPROXEQ. (/8._SRK,16._SRK,32._SRK/)))) THEN
        WRITE(*,*) "CALL BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=xPETScVector,BETA=2.0_SRK,Y=yPETScVector)", &
                   " -petscsparse FAILED!"
        STOP 666
      ENDIF
      CALL thisMatrix%clear()
      y=2._SRK
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=x,BETA=2.0_SRK,Y=y) !Error check uninit
      IF(ANY(.NOT.(y .APPROXEQ. (/2._SRK,2._SRK,2._SRK/)))) THEN
        WRITE(*,*) "CALL BLAS_matvec(THISMATRIX=thisMatrix,X=x,BETA=2.0_SRK,Y=y) -petscsparse FAILED!"
        STOP 666
      ENDIF
      CALL yRealVector%set(2._SRK)
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=xRealVector,BETA=2.0_SRK,Y=yRealVector) !Error check uninit
      CALL yRealVector%get(y)
      IF(ANY(.NOT.(y .APPROXEQ. (/2._SRK,2._SRK,2._SRK/)))) THEN
        WRITE(*,*) "CALL BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=xRealVector,BETA=2.0_SRK,Y=yRealVector)", &
                   " -petscsparse FAILED!"
        STOP 666
      ENDIF
      CALL yPETScVector%set(2._SRK)
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=xPETScVector,BETA=2.0_SRK,Y=yPETScVector) !Error check uninit
      CALL yPETScVector%get(y)
      IF(ANY(.NOT.(y .APPROXEQ. (/2._SRK,2._SRK,2._SRK/)))) THEN
        WRITE(*,*) "CALL BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=xPETScVector,BETA=2.0_SRK,Y=yPETScVector)", & 
                   " -petscsparse FAILED!"
        STOP 666
      ENDIF
      WRITE(*,*) '  Passed: CALL BLAS_matvec(...) petscsparse-matrix'
      
      !Perform test of functionality of get function
      ![1 0 2]
      ![0 0 3]
      ![4 5 6]
      CALL thisMatrix%clear()
      CALL pList%clear()
      CALL pList%add('MatrixType->n',3_SNK)
      CALL pList%add('MatrixType->isSym',.FALSE.)
      CALL pList%add('MatrixType->mattype',SPARSE)
      CALL pList%validate(pList,optListMat)
      CALL thisMatrix%init(pList) ! non-symmetric (0), sparse (0) 
      SELECTTYPE(thisMatrix)
        TYPE IS(PETScMatrixType)
          CALL thisMatrix%set(1,1,1._SRK)
          CALL thisMatrix%set(1,3,2._SRK)
          CALL thisMatrix%set(2,3,3._SRK)
          CALL thisMatrix%set(3,1,4._SRK)
          CALL thisMatrix%set(3,2,5._SRK)
          CALL thisMatrix%set(3,3,6._SRK)
          IF(ALLOCATED(dummyvec)) DEALLOCATE(dummyvec)
          ALLOCATE(dummyvec(9))
          CALL thisMatrix%get(1,1,dummyvec(1))
          CALL thisMatrix%get(2,1,dummyvec(2))
          CALL thisMatrix%get(3,1,dummyvec(3))
          CALL thisMatrix%get(1,2,dummyvec(4))
          CALL thisMatrix%get(2,2,dummyvec(5))
          CALL thisMatrix%get(3,2,dummyvec(6))
          CALL thisMatrix%get(1,3,dummyvec(7))
          CALL thisMatrix%get(2,3,dummyvec(8))
          CALL thisMatrix%get(3,3,dummyvec(9))
          IF(((dummyvec(1) /= 1._SRK)  .OR. &
              (dummyvec(2) /= 0._SRK)) .OR. &
              (dummyvec(3) /= 4._SRK)) THEN
            WRITE(*,*) 'CALL petscsparse%get(...) FAILED!' !column one check
            STOP 666
          ELSEIF(((dummyvec(4) /= 0._SRK)  .OR. &
                  (dummyvec(5) /= 0._SRK)) .OR. &
                  (dummyvec(6) /= 5._SRK)) THEN
            WRITE(*,*) 'CALL petscsparse%get(...) FAILED!' !column two check
            STOP 666
          ELSEIF(((dummyvec(7) /= 2._SRK)  .OR. &
                  (dummyvec(8) /= 3._SRK)) .OR. &
                  (dummyvec(9) /= 6._SRK)) THEN
            WRITE(*,*) 'CALL petscsparse%get(...) FAILED!' !column three check
            STOP 666
          ENDIF
      ENDSELECT
      !test with out of bounds i,j, make sure no crash.
      SELECTTYPE(thisMatrix)
        TYPE IS(PETScMatrixType)
          CALL thisMatrix%get(4,2,dummy)
          IF(dummy /= -1051._SRK) THEN
            WRITE(*,*) 'CALL petscsparse%get(...) FAILED!' 
            STOP 666
          ENDIF
          CALL thisMatrix%get(-1,2,dummy)
          IF(dummy/=-1051._SRK) THEN
            WRITE(*,*) 'CALL petscsparse%get(...) FAILED!' 
            STOP 666
          ENDIF
          CALL thisMatrix%get(2,-1,dummy)
          IF(dummy/=-1051._SRK) THEN
            WRITE(*,*) 'CALL petscsparse%get(...) FAILED!' 
            STOP 666
          ENDIF
      ENDSELECT
      !test get with uninit, make sure no crash.
      CALL thisMatrix%clear()
      SELECTTYPE(thisMatrix)
        TYPE IS(PETScMatrixType)      
          CALL thisMatrix%get(1,1,dummy)
          IF(dummy /= 0.0_SRK) THEN
            WRITE(*,*) 'CALL petscsparse%get(...) FAILED!'
            STOP 666
          ENDIF
      ENDSELECT
      CALL thisMatrix%clear()
      WRITE(*,*) '  Passed: CALL petscsparse%get(...)'
      
      DEALLOCATE(thisMatrix)

!Test for PETSc dense square matrices        
      ALLOCATE(PETScMatrixType :: thisMatrix)
      SELECTTYPE(thisMatrix)
        TYPE IS(PETScMatrixType)
          !test clear
          !make matrix w/out using untested init
          thisMatrix%isInit=.TRUE.
          thisMatrix%n=10
          thisMatrix%isSymmetric=.TRUE.
          CALL MatCreate(MPI_COMM_WORLD,thisMatrix%a,ierr)
          CALL MatSetSizes(thisMatrix%a,PETSC_DECIDE,PETSC_DECIDE, &
                                        thisMatrix%n,thisMatrix%n,ierr)
          CALL MatSetType(thisMatrix%a,MATMPIDENSE,ierr)
          CALL MatSetUp(thisMatrix%a,ierr)
      ENDSELECT
      CALL thisMatrix%clear()
      SELECTTYPE(thisMatrix)
        TYPE IS(PETScMatrixType)
          IF(((thisMatrix%isInit).OR.(thisMatrix%n /= 0)) &
              .OR.((thisMatrix%isSymmetric))) THEN
            WRITE(*,*) 'CALL petscdense%clear() FAILED!'
            STOP 666
          ENDIF
          IF(thisMatrix%a /= PETSC_NULL_REAL) THEN
            WRITE(*,*) 'CALL petscdense%clear() FAILED!'
            STOP 666
          ENDIF
          WRITE(*,*) '  Passed: CALL petscdense%clear()'
      ENDSELECT
      !check init
      CALL pList%clear()
      CALL pList%add('MatrixType->n',10_SNK)
      CALL pList%add('MatrixType->isSym',.FALSE.)
      CALL pList%add('MatrixType->mattype',DENSESQUARE) ! dense
      CALL pList%validate(pList,optListMat)
      eMatrixType => NULL()
      CALL thisMatrix%init(pList) !n=10, not symmetric (0), sparse (0)
      eMatrixType => e
      SELECTTYPE(thisMatrix)
        TYPE IS(PETScMatrixType)
          IF(((.NOT. thisMatrix%isInit).OR.(thisMatrix%n /= 10)) &
              .OR.(thisMatrix%isSymmetric)) THEN
            WRITE(*,*) 'CALL petscdense%init(...) FAILED!'
            STOP 666
          ENDIF
          CALL MatGetSize(thisMatrix%a,matsize1,matsize2,ierr)
          IF((matsize1 /= 10) .OR. (matsize2 /= 10)) THEN
            WRITE(*,*) 'CALL petscdense%init(...) FAILED!'
            STOP 666
          ENDIF
      ENDSELECT  
      CALL thisMatrix%clear()
      CALL pList%clear()
      CALL pList%add('MatrixType->n',10_SNK)
      CALL pList%add('MatrixType->isSym',.TRUE.)
      CALL pList%add('MatrixType->mattype',DENSESQUARE)
      CALL pList%validate(pList,optListMat)
      CALL thisMatrix%init(pList) !n=10, symmetric (1), sparse (0)
      SELECTTYPE(thisMatrix)
        TYPE IS(PETScMatrixType)
          IF(((.NOT. thisMatrix%isInit).OR.(thisMatrix%n /= 10)) &
              .OR.(.NOT. thisMatrix%isSymmetric)) THEN
            WRITE(*,*) 'CALL petscdense%init(...) FAILED!'
            STOP 666
          ENDIF
      ENDSELECT
      CALL thisMatrix%clear()
      !test with double init (isInit==true on 2nd try)
      CALL pList%clear()
      CALL pList%add('MatrixType->n',10_SNK)
      CALL pList%add('MatrixType->isSym',.TRUE.)
      CALL pList%add('MatrixType->mattype',DENSESQUARE)
      CALL pList%validate(pList,optListMat)
      CALL thisMatrix%init(pList) !n=10, symmetric (1), sparse (0)
      SELECTTYPE(thisMatrix)
        TYPE IS(PETScMatrixType); thisMatrix%isSymmetric=.FALSE.
      ENDSELECT
      CALL thisMatrix%init(pList) !n=10, symmetric (1), sparse (0)
      SELECTTYPE(thisMatrix)
        TYPE IS(PETScMatrixType)
          IF(thisMatrix%isSymmetric) THEN
            WRITE(*,*) 'CALL petscdense%init(...) FAILED!'
            STOP 666
          ENDIF
      ENDSELECT
      CALL thisMatrix%clear()
      WRITE(*,*) '  Passed: CALL petscdense%init(...)'
      
      !check set
      !test normal use case (symmetric and nonsymmetric)
      !want to build:
      ![1 2]
      ![2 3]
      CALL pList%clear()
      CALL pList%add('MatrixType->n',2_SNK)
      CALL pList%add('MatrixType->isSym',.TRUE.)
      CALL pList%add('MatrixType->mattype',DENSESQUARE)
      CALL pList%validate(pList,optListMat)
      CALL thisMatrix%init(pList)  !symmetric (1), dense (1)
      CALL thisMatrix%set(1,1,1._SRK)
      CALL thisMatrix%set(1,2,2._SRK)
      CALL thisMatrix%set(2,2,3._SRK)
      SELECTTYPE(thisMatrix)
        TYPE IS(PETScMatrixType)
          ! manually assemble
          CALL MatAssemblyBegin(thisMatrix%a,MAT_FINAL_ASSEMBLY,ierr)
          CALL MatAssemblyEnd(thisMatrix%a,MAT_FINAL_ASSEMBLY,ierr)
          thisMatrix%isAssembled=.TRUE.
          
          CALL MatGetValues(thisMatrix%a,1,0,1,0,dummy,ierr)
          IF(dummy/=1._SRK)THEN
            WRITE(*,*) 'CALL petscdense%set(...) FAILED!'
            STOP 666
          ENDIF
          CALL MatGetValues(thisMatrix%a,1,0,1,1,dummy,ierr)
          IF(dummy/=2._SRK)THEN
            WRITE(*,*) 'CALL petscdense%set(...) FAILED!'
            STOP 666
          ENDIF
          CALL MatGetValues(thisMatrix%a,1,1,1,1,dummy,ierr)
          IF(dummy/=3._SRK)THEN
            WRITE(*,*) 'CALL petscdense%set(...) FAILED!'
            STOP 666
          ENDIF
      ENDSELECT
      CALL thisMatrix%clear()
      
      CALL pList%clear()
      CALL pList%add('MatrixType->n',2_SNK)
      CALL pList%add('MatrixType->isSym',.FALSE.)
      CALL pList%add('MatrixType->mattype',DENSESQUARE)
      CALL pList%validate(pList,optListMat)
      CALL thisMatrix%init(pList)  !nonsymmetric (0), dense (1)
      CALL thisMatrix%set(1,1,1._SRK)
      CALL thisMatrix%set(1,2,2._SRK)
      CALL thisMatrix%set(2,1,2._SRK)
      CALL thisMatrix%set(2,2,3._SRK)
      SELECTTYPE(thisMatrix)
        TYPE IS(PETScMatrixType)
          ! manually assemble
          CALL MatAssemblyBegin(thisMatrix%a,MAT_FINAL_ASSEMBLY,ierr)
          CALL MatAssemblyEnd(thisMatrix%a,MAT_FINAL_ASSEMBLY,ierr)
          thisMatrix%isAssembled=.TRUE.
          
          CALL MatGetValues(thisMatrix%a,1,0,1,0,dummy,ierr)
          IF(dummy/=1._SRK)THEN
            WRITE(*,*) 'CALL petscdense%set(...) FAILED!'
            STOP 666
          ENDIF
          CALL MatGetValues(thisMatrix%a,1,0,1,1,dummy,ierr)
          IF(dummy/=2._SRK)THEN
            WRITE(*,*) 'CALL petscdense%set(...) FAILED!'
            STOP 666
          ENDIF
          CALL MatGetValues(thisMatrix%a,1,1,1,0,dummy,ierr)
          IF(dummy/=2._SRK)THEN
            WRITE(*,*) 'CALL petscdense%set(...) FAILED!'
            STOP 666
          ENDIF
          CALL MatGetValues(thisMatrix%a,1,1,1,1,dummy,ierr)
          IF(dummy/=3._SRK)THEN
            WRITE(*,*) 'CALL petscdense%set(...) FAILED!'
            STOP 666
          ENDIF
      ENDSELECT    
      !check matrix that hasnt been init, i,j out of bounds
      CALL thisMatrix%clear()
      CALL thisMatrix%set(1,1,1._SRK) 
      CALL thisMatrix%clear()
      CALL pList%clear()
      CALL pList%add('MatrixType->n',2_SNK)
      CALL pList%add('MatrixType->isSym',.FALSE.)
      CALL pList%add('MatrixType->mattype',DENSESQUARE)
      CALL pList%validate(pList,optListMat)
      CALL thisMatrix%init(pList)
      CALL thisMatrix%set(-1,1,1._SRK)
      CALL thisMatrix%set(1,-1,1._SRK)
      CALL thisMatrix%set(5,1,1._SRK)
      CALL thisMatrix%set(1,5,1._SRK)
      !no crash? good
      WRITE(*,*) '  Passed: CALL petscdense%set(...)'
      
      !Test BLAS_matvec
      CALL thisMatrix%clear()
      CALL pList%clear()
      CALL pList%add('MatrixType->n',3_SNK)
      CALL pList%add('MatrixType->isSym',.FALSE.)
      CALL pList%add('MatrixType->mattype',DENSESQUARE)
      CALL pList%validate(pList,optListMat)
      CALL thisMatrix%init(pList)  !symmetric
      CALL thisMatrix%set(1,1,1._SRK)
      CALL thisMatrix%set(1,3,2._SRK)
      CALL thisMatrix%set(2,2,4._SRK)
      CALL thisMatrix%set(2,3,3._SRK)
      CALL thisMatrix%set(3,1,4._SRK)
      CALL thisMatrix%set(3,2,5._SRK)
      CALL thisMatrix%set(3,3,6._SRK)
      x=1.0_SRK
      y=1.0_SRK
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=x,Y=y)
      IF(ANY(.NOT.(y .APPROXEQ. (/4._SRK,8._SRK,16._SRK/)))) THEN
        WRITE(*,*) "CALL BLAS_matvec(THISMATRIX=thisMatrix,X=x,Y=y) -petscdense FAILED!"
        STOP 666
      ENDIF
      CALL xRealVector%set(1.0_SRK)
      CALL yRealVector%set(1.0_SRK)
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=xRealVector,Y=yRealVector)
      CALL yRealVector%get(y)
      IF(ANY(.NOT.(y .APPROXEQ. (/4._SRK,8._SRK,16._SRK/)))) THEN
        WRITE(*,*) "CALL BLAS_matvec(THISMATRIX=thisMatrix,X=xRealVector,Y=yRealVector) -petscdense FAILED!"
        STOP 666
      ENDIF
      CALL xPETScVector%set(1.0_SRK)
      CALL yPETScVector%set(1.0_SRK)
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=xPETScVector,Y=yPETScVector)
      CALL yPETScVector%get(y)
      IF(ANY(.NOT.(y .APPROXEQ. (/4._SRK,8._SRK,16._SRK/)))) THEN
        WRITE(*,*) "CALL BLAS_matvec(THISMATRIX=thisMatrix,X=PETScVector,Y=yPETScVector) -petscdense FAILED!"
        STOP 666
      ENDIF
      y=1.0_SRK
      CALL BLAS_matvec(THISMATRIX=thisMatrix,trans='t',X=x,Y=y)
      IF(ANY(.NOT.(y .APPROXEQ. (/6._SRK,10._SRK,12._SRK/)))) THEN
        WRITE(*,*) "CALL BLAS_matvec(THISMATRIX=thisMatrix,trans='t',X=x,Y=y) -petscdense FAILED!"
        STOP 666
      ENDIF
      CALL yRealVector%set(1.0_SRK)
      CALL BLAS_matvec(THISMATRIX=thisMatrix,trans='t',X=xRealVector,Y=yRealVector)
      CALL yRealVector%get(y)
      IF(ANY(.NOT.(y .APPROXEQ. (/6._SRK,10._SRK,12._SRK/)))) THEN
        WRITE(*,*) "CALL BLAS_matvec(THISMATRIX=thisMatrix,X=xRealVector,Y=yRealVector) -petscdense FAILED!"
        STOP 666
      ENDIF
      CALL yPETScVector%set(1.0_SRK)
      CALL BLAS_matvec(THISMATRIX=thisMatrix,trans='t',X=xPETScVector,Y=yPETScVector)
      CALL yPETScVector%get(y)
      IF(ANY(.NOT.(y .APPROXEQ. (/6._SRK,10._SRK,12._SRK/)))) THEN
        WRITE(*,*) "CALL BLAS_matvec(THISMATRIX=thisMatrix,X=xPETScVector,Y=yPETScVector) -petscdense FAILED!"
        STOP 666
      ENDIF
      y=1.0_SRK
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=x,BETA=2.0_SRK,Y=y)
      IF(ANY(.NOT.(y .APPROXEQ. (/5._SRK,9._SRK,17._SRK/)))) THEN
        WRITE(*,*) "CALL BLAS_matvec(THISMATRIX=thisMatrix,X=x,BETA=2.0_SRK,Y=y) -petscdense FAILED!"
        STOP 666
      ENDIF
      CALL yRealVector%set(1.0_SRK)
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=xRealVector,BETA=2.0_SRK,Y=yRealVector)
      CALL yRealVector%get(y)
      IF(ANY(.NOT.(y .APPROXEQ. (/5._SRK,9._SRK,17._SRK/)))) THEN
        WRITE(*,*) "CALL BLAS_matvec(THISMATRIX=thisMatrix,X=xRealVector,BETA=2.0_SRK,Y=yRealVector) -petscdense FAILED!"
        STOP 666
      ENDIF
      CALL yPETScVector%set(1.0_SRK)
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=xPETScVector,BETA=2.0_SRK,Y=yPETScVector)
      CALL yPETScVector%get(y)
      IF(ANY(.NOT.(y .APPROXEQ. (/5._SRK,9._SRK,17._SRK/)))) THEN
        WRITE(*,*) "CALL BLAS_matvec(THISMATRIX=thisMatrix,X=xPETScVector,BETA=2.0_SRK,Y=yPETScVector) -petscdense FAILED!"
        STOP 666
      ENDIF
      y=1.0_SRK
      CALL BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=x,Y=y)
      IF(ANY(.NOT.(y .APPROXEQ. (/7._SRK,15._SRK,31._SRK/)))) THEN
        WRITE(*,*) "CALL BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=x,Y=y) -petscdense FAILED!"
        STOP 666
      ENDIF
      CALL yRealVector%set(1.0_SRK)
      CALL BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=xRealVector,Y=yRealVector)
      CALL yRealVector%get(y)
      IF(ANY(.NOT.(y .APPROXEQ. (/7._SRK,15._SRK,31._SRK/)))) THEN
        WRITE(*,*) "CALL BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=xRealVector,Y=yRealVector) -petscdense FAILED!"
        STOP 666
      ENDIF
      CALL yPETScVector%set(1.0_SRK)
      CALL BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=xPETScVector,Y=yPETScVector)
      CALL yPETScVector%get(y)
      IF(ANY(.NOT.(y .APPROXEQ. (/7._SRK,15._SRK,31._SRK/)))) THEN
        WRITE(*,*) "CALL BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=xPETScVector,Y=yPETScVector) -petscdense FAILED!"
        STOP 666
      ENDIF
      y=1.0_SRK
      CALL BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=x,BETA=2.0_SRK,Y=y)
      IF(ANY(.NOT.(y .APPROXEQ. (/8._SRK,16._SRK,32._SRK/)))) THEN
        WRITE(*,*) "CALL BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=x,BETA=2.0_SRK,Y=y) -petscdense FAILED!"
        STOP 666
      ENDIF
      CALL yRealVector%set(1.0_SRK)
      CALL BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=xRealVector,BETA=2.0_SRK,Y=yRealVector)
      CALL yRealVector%get(y)
      IF(ANY(.NOT.(y .APPROXEQ. (/8._SRK,16._SRK,32._SRK/)))) THEN
        WRITE(*,*) "CALL BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=xRealVector,BETA=2.0_SRK,Y=yRealVector)", &
                   " -petscdense FAILED!"
        STOP 666
      ENDIF
      CALL yPETScVector%set(1.0_SRK)
      CALL BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=xPETScVector,BETA=2.0_SRK,Y=yPETScVector)
      CALL yPETScVector%get(y)
      IF(ANY(.NOT.(y .APPROXEQ. (/8._SRK,16._SRK,32._SRK/)))) THEN
        WRITE(*,*) "CALL BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=xPETScVector,BETA=2.0_SRK,Y=yPETScVector)", &
                   " -petscdense FAILED!"
        STOP 666
      ENDIF
      CALL thisMatrix%clear()
      y=2._SRK
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=x,BETA=2.0_SRK,Y=y) !Error check uninit
      IF(ANY(.NOT.(y .APPROXEQ. (/2._SRK,2._SRK,2._SRK/)))) THEN
        WRITE(*,*) "CALL BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=x,BETA=2.0_SRK,Y=y) -petscdense FAILED!"
        STOP 666
      ENDIF
      CALL yRealVector%set(2._SRK)
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=xRealVector,BETA=2.0_SRK,Y=yRealVector) !Error check uninit
      CALL yRealVector%get(y)
      IF(ANY(.NOT.(y .APPROXEQ. (/2._SRK,2._SRK,2._SRK/)))) THEN
        WRITE(*,*) "CALL BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=xRealVector,BETA=2.0_SRK,Y=yRealVector)", &
                   " -petscdense FAILED!"
        STOP 666
      ENDIF
      CALL yPETScVector%set(2._SRK)
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=xPETScVector,BETA=2.0_SRK,Y=yPETScVector) !Error check uninit
      CALL yPETScVector%get(y)
      IF(ANY(.NOT.(y .APPROXEQ. (/2._SRK,2._SRK,2._SRK/)))) THEN
        WRITE(*,*) "CALL BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=xPETScVector,BETA=2.0_SRK,Y=yPETScVector)", &
                   " -petscdense FAILED!"
        STOP 666
      ENDIF
      WRITE(*,*) '  Passed: CALL BLAS_matvec(...) petscdense-matrix'
      
      !Perform test of functionality of get function
      ![1 0 2]
      ![0 0 3]
      ![4 5 6]
      CALL thisMatrix%clear()
      CALL pList%clear()
      CALL pList%add('MatrixType->n',3_SNK)
      CALL pList%add('MatrixType->isSym',.FALSE.)
      CALL pList%add('MatrixType->mattype',DENSESQUARE)
      CALL pList%validate(pList,optListMat)
      CALL thisMatrix%init(pList) ! non-symmetric (0), dense (1)
      SELECTTYPE(thisMatrix)
        TYPE IS(PETScMatrixType)
          CALL thisMatrix%set(1,1,1._SRK)
          CALL thisMatrix%set(1,3,2._SRK)
          CALL thisMatrix%set(2,3,3._SRK)
          CALL thisMatrix%set(3,1,4._SRK)
          CALL thisMatrix%set(3,2,5._SRK)
          CALL thisMatrix%set(3,3,6._SRK)
          IF(ALLOCATED(dummyvec)) DEALLOCATE(dummyvec)
          ALLOCATE(dummyvec(9))
          CALL thisMatrix%get(1,1,dummyvec(1))
          CALL thisMatrix%get(2,1,dummyvec(2))
          CALL thisMatrix%get(3,1,dummyvec(3))
          CALL thisMatrix%get(1,2,dummyvec(4))
          CALL thisMatrix%get(2,2,dummyvec(5))
          CALL thisMatrix%get(3,2,dummyvec(6))
          CALL thisMatrix%get(1,3,dummyvec(7))
          CALL thisMatrix%get(2,3,dummyvec(8))
          CALL thisMatrix%get(3,3,dummyvec(9))
          IF(((dummyvec(1) /= 1._SRK)  .OR. &
              (dummyvec(2) /= 0._SRK)) .OR. &
              (dummyvec(3) /= 4._SRK)) THEN
            WRITE(*,*) 'CALL petscdense%get(...) FAILED!' !column one check
            STOP 666
          ELSEIF(((dummyvec(4) /= 0._SRK)  .OR. &
                  (dummyvec(5) /= 0._SRK)) .OR. &
                  (dummyvec(6) /= 5._SRK)) THEN
            WRITE(*,*) 'CALL petscdense%get(...) FAILED!' !column two check
            STOP 666
          ELSEIF(((dummyvec(7) /= 2._SRK)  .OR. &
                  (dummyvec(8) /= 3._SRK)) .OR. &
                  (dummyvec(9) /= 6._SRK)) THEN
            WRITE(*,*) 'CALL petscdense%get(...) FAILED!' !column three check
            STOP 666
          ENDIF
      ENDSELECT
      !test with out of bounds i,j, make sure no crash.
      SELECTTYPE(thisMatrix)
        TYPE IS(PETScMatrixType)
          CALL thisMatrix%get(4,2,dummy)
          IF(dummy /= -1051._SRK) THEN
            WRITE(*,*) 'CALL petscdensesquare%get(...) FAILED!' 
            STOP 666
          ENDIF
          CALL thisMatrix%get(-1,2,dummy)
          IF(dummy/=-1051._SRK) THEN
            WRITE(*,*) 'CALL petscdensesquare%get(...) FAILED!' 
            STOP 666
          ENDIF
          CALL thisMatrix%get(2,-1,dummy)
          IF(dummy/=-1051._SRK) THEN
            WRITE(*,*) 'CALL petscdensesquare%get(...) FAILED!' 
            STOP 666
          ENDIF
      ENDSELECT
      !test get with uninit, make sure no crash.
      CALL thisMatrix%clear()
      SELECTTYPE(thisMatrix)
        TYPE IS(PETScMatrixType)      
          CALL thisMatrix%get(1,1,dummy)
          IF(dummy /= 0.0_SRK) THEN
            WRITE(*,*) 'CALL petscdensesquare%get(...) FAILED!'
            STOP 666
          ENDIF
      ENDSELECT
      CALL thisMatrix%clear()
      
      WRITE(*,*) '  Passed: CALL petscdensesquare%get(...)'
      
      DEALLOCATE(thisMatrix)
      CALL xPETScVector%clear()
      CALL yPETScVector%clear()
#endif
      CALL xRealVector%clear()
      CALL yRealVector%clear()
    ENDSUBROUTINE testMatrix
!
!-------------------------------------------------------------------------------
    SUBROUTINE testMatrixMultSparse()
      CLASS(MatrixType),ALLOCATABLE :: thisMtrx
      CLASS(MatrixType),ALLOCATABLE :: bmat
      CLASS(MatrixType),ALLOCATABLE :: cmat
      INTEGER(SIK) :: i,j
      REAL(SRK) :: ALPHA,BETA,dummy  

#ifdef HAVE_PETSC
      !
      !1.) A: PETSc  B: PETSc  C: PETSc     ----------------------------
      !
      ALLOCATE(PETScMatrixType :: thisMtrx)
      CALL pList%clear()
      CALL pList%add('MatrixType->n',3_SNK)
      CALL pList%add('MatrixType->isSym',.TRUE.)
      CALL pList%add('MatrixType->mattype',SPARSE)
      CALL pList%validate(pList,optListMat)
      CALL thisMtrx%clear()
      CALL thisMtrx%init(pList)  !symmetric
      CALL thisMtrx%set(1,1,1._SRK)
      CALL thisMtrx%set(1,2,1._SRK)
      CALL thisMtrx%set(1,3,1._SRK)
      CALL thisMtrx%set(2,2,1._SRK)
      CALL thisMtrx%set(2,3,1._SRK)
      CALL thisMtrx%set(3,3,1._SRK)
      
      ALLOCATE(PETScMatrixType :: bmat)
      ALLOCATE(PETScMatrixType :: cmat)
      CALL bmat%init(pList)  !symmetric
      CALL bmat%set(1,1,1._SRK)
      CALL bmat%set(1,2,0._SRK)
      CALL bmat%set(1,3,0._SRK)
      CALL bmat%set(2,2,1._SRK)
      CALL bmat%set(2,3,0._SRK)
      CALL bmat%set(3,3,1._SRK)
      CALL cmat%init(pList)  !symmetric
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
        TYPE IS(PETScMatrixType)
          DO i=1,cmat%n
            DO j=1,cmat%n
              CALL cmat%get(i,j,dummy)
              IF(.NOT.(dummy .APPROXEQ. 4._SRK) ) THEN
                WRITE(*,*) "CALL BLAS_matmult(A=thisMtrx,B=bmat,C=cmat,alpha=ALPHA,beta=BETA) -All PETSc FAILED!"
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
        TYPE IS(PETScMatrixType)
          DO i=1,cmat%n
            DO j=1,cmat%n
              CALL cmat%get(i,j,dummy)
              IF(.NOT.(dummy .APPROXEQ. 5._SRK) ) THEN
                WRITE(*,*) "CALL BLAS_matmult(A=thisMtrx,B=bmat,C=cmat,alpha=ALPHA) -All PETSc FAILED!"
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
        TYPE IS(PETScMatrixType)
          DO i=1,cmat%n
            DO j=1,cmat%n
              CALL cmat%get(i,j,dummy)
              IF(.NOT.(dummy .APPROXEQ. 6._SRK) ) THEN
                WRITE(*,*) "CALL BLAS_matmult(A=thisMtrx,B=bmat,C=cmat,beta=BETA) -All PETSc FAILED!"
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
        TYPE IS(PETScMatrixType)
          DO i=1,cmat%n
            DO j=1,cmat%n
              CALL cmat%get(i,j,dummy)
              IF(.NOT.(dummy .APPROXEQ. 7._SRK) ) THEN
                WRITE(*,*) "CALL BLAS_matmult(A=thisMtrx,B=bmat,C=cmat) -All PETSc FAILED!"
                STOP 666
              ENDIF
            ENDDO
          ENDDO
        CLASS DEFAULT
          WRITE(*,*) "CALL BLAS_matmult(A=thisMtrx,B=bmat,C=cmat) FAILED!"
          WRITE(*,*) "    cmat returned as incorrect matrix type"
          STOP 666
      ENDSELECT
      WRITE(*,*) '  Passed: CALL BLAS_matmult(...) A=PETSC B=PETSC C=PETSC'
#endif
    ENDSUBROUTINE  testMatrixMultSparse
!
!-------------------------------------------------------------------------------
    SUBROUTINE testMatrixMultSquare()
      CLASS(MatrixType),ALLOCATABLE :: thisMtrx
      CLASS(MatrixType),ALLOCATABLE :: bmat
      CLASS(MatrixType),ALLOCATABLE :: cmat
      INTEGER(SIK) :: i,j
      REAL(SRK) :: ALPHA,BETA,dummy
      
      !Test BLAS_matmult
      !
      !1.) A: Square  B: Square  C: Square     ----------------------------
      !
      ALLOCATE(DenseSquareMatrixType :: thisMtrx)
      CALL pList%clear()
      CALL pList%add('MatrixType->n',3_SNK)
      CALL pList%add('MatrixType->isSym',.TRUE.)
      CALL thisMtrx%clear()
      CALL thisMtrx%init(pList)  !symmetric
      CALL thisMtrx%set(1,1,1._SRK)
      CALL thisMtrx%set(1,2,1._SRK)
      CALL thisMtrx%set(1,3,1._SRK)
      CALL thisMtrx%set(2,2,1._SRK)
      CALL thisMtrx%set(2,3,1._SRK)
      CALL thisMtrx%set(3,3,1._SRK)
      
      ALLOCATE(DenseSquareMatrixType :: bmat)
      ALLOCATE(DenseSquareMatrixType :: cmat)
      CALL bmat%init(pList)  !symmetric
      CALL bmat%set(1,1,1._SRK)
      CALL bmat%set(1,2,0._SRK)
      CALL bmat%set(1,3,0._SRK)
      CALL bmat%set(2,2,1._SRK)
      CALL bmat%set(2,3,0._SRK)
      CALL bmat%set(3,3,1._SRK)
      CALL cmat%init(pList)  !symmetric
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
      CALL pList%clear()
      CALL pList%add('MatrixType->n',3_SNK)
      CALL pList%add('MatrixType->m',3_SNK)
      CALL cmat%init(pList)  
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
      CALL pList%clear()
      CALL pList%add('MatrixType->n',3_SNK)
      CALL pList%add('MatrixType->isSym',.TRUE.)
      CALL cmat%init(pList)  !symmetric
      CALL cmat%set(1,1,3._SRK)
      CALL cmat%set(1,2,3._SRK)
      CALL cmat%set(1,3,3._SRK)
      CALL cmat%set(2,2,3._SRK)
      CALL cmat%set(2,3,3._SRK)
      CALL cmat%set(3,3,3._SRK)
      CALL bmat%clear()
      DEALLOCATE(bmat)
      ALLOCATE(DenseRectMatrixType :: bmat)
      CALL pList%clear()
      CALL pList%add('MatrixType->n',3_SNK)
      CALL pList%add('MatrixType->m',3_SNK)
      CALL bmat%init(pList)  
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
      CALL pList%clear()
      CALL pList%add('MatrixType->n',3_SNK)
      CALL pList%add('MatrixType->m',3_SNK)
      CALL cmat%init(pList)  
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

#ifdef HAVE_PETSC
      !
      !5.) A: PETSc  B: PETSc  C: PETSc     ----------------------------
      !
      DEALLOCATE(thisMtrx,bmat,cmat)
      ALLOCATE(PETScMatrixType :: thisMtrx)
      CALL pList%clear()
      CALL pList%add('MatrixType->n',3_SNK)
      CALL pList%add('MatrixType->isSym',.TRUE.)
      CALL pList%add('MatrixType->mattype',DENSESQUARE)
      CALL pList%validate(pList,optListMat)
      CALL thisMtrx%clear()
      CALL thisMtrx%init(pList)  !symmetric
      CALL thisMtrx%set(1,1,1._SRK)
      CALL thisMtrx%set(1,2,1._SRK)
      CALL thisMtrx%set(1,3,1._SRK)
      CALL thisMtrx%set(2,2,1._SRK)
      CALL thisMtrx%set(2,3,1._SRK)
      CALL thisMtrx%set(3,3,1._SRK)
      
      ALLOCATE(PETScMatrixType :: bmat)
      ALLOCATE(PETScMatrixType :: cmat)
      CALL bmat%init(pList)  !symmetric
      CALL bmat%set(1,1,1._SRK)
      CALL bmat%set(1,2,0._SRK)
      CALL bmat%set(1,3,0._SRK)
      CALL bmat%set(2,2,1._SRK)
      CALL bmat%set(2,3,0._SRK)
      CALL bmat%set(3,3,1._SRK)
      CALL cmat%init(pList)  !symmetric
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
        TYPE IS(PETScMatrixType)
          DO i=1,cmat%n
            DO j=1,cmat%n
              CALL cmat%get(i,j,dummy)
              IF(.NOT.(dummy .APPROXEQ. 4._SRK) ) THEN
                WRITE(*,*) "CALL BLAS_matmult(A=thisMtrx,B=bmat,C=cmat,alpha=ALPHA,beta=BETA) -All PETSc FAILED!"
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
        TYPE IS(PETScMatrixType)
          DO i=1,cmat%n
            DO j=1,cmat%n
              CALL cmat%get(i,j,dummy)
              IF(.NOT.(dummy .APPROXEQ. 5._SRK) ) THEN
                WRITE(*,*) "CALL BLAS_matmult(A=thisMtrx,B=bmat,C=cmat,alpha=ALPHA) -All PETSc FAILED!"
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
        TYPE IS(PETScMatrixType)
          DO i=1,cmat%n
            DO j=1,cmat%n
              CALL cmat%get(i,j,dummy)
              IF(.NOT.(dummy .APPROXEQ. 6._SRK) ) THEN
                WRITE(*,*) "CALL BLAS_matmult(A=thisMtrx,B=bmat,C=cmat,beta=BETA) -All PETSc FAILED!"
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
        TYPE IS(PETScMatrixType)
          DO i=1,cmat%n
            DO j=1,cmat%n
              CALL cmat%get(i,j,dummy)
              IF(.NOT.(dummy .APPROXEQ. 7._SRK) ) THEN
                WRITE(*,*) "CALL BLAS_matmult(A=thisMtrx,B=bmat,C=cmat) -All PETSc FAILED!"
                STOP 666
              ENDIF
            ENDDO
          ENDDO
        CLASS DEFAULT
          WRITE(*,*) "CALL BLAS_matmult(A=thisMtrx,B=bmat,C=cmat) FAILED!"
          WRITE(*,*) "    cmat returned as incorrect matrix type"
          STOP 666
      ENDSELECT
      WRITE(*,*) '  Passed: CALL BLAS_matmult(...) A=PETSC B=PETSC C=PETSC'
#endif
      
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
      CALL pList%clear()
      CALL pList%add('MatrixType->n',3_SNK)
      CALL pList%add('MatrixType->m',3_SNK)
      CALL thisMtrx%init(pList)  !symmetric
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
      CALL pList%clear()
      CALL pList%add('MatrixType->n',3_SNK)
      CALL pList%add('MatrixType->isSym',.TRUE.)
      CALL bmat%init(pList)  !symmetric
      CALL bmat%set(1,1,1._SRK)
      CALL bmat%set(1,2,0._SRK)
      CALL bmat%set(1,3,0._SRK)
      CALL bmat%set(2,2,1._SRK)
      CALL bmat%set(2,3,0._SRK)
      CALL bmat%set(3,3,1._SRK)
      CALL pList%clear()
      CALL pList%add('MatrixType->n',3_SNK)
      CALL pList%add('MatrixType->isSym',.TRUE.) 
      CALL cmat%init(pList)  !symmetric
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
      CALL pList%clear()
      CALL pList%add('MatrixType->n',3_SNK)
      CALL pList%add('MatrixType->m',3_SNK)
      CALL cmat%init(pList)  
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
      CALL pList%clear()
      CALL pList%add('MatrixType->n',3_SNK)
      CALL pList%add('MatrixType->isSym',.TRUE.)
      CALL cmat%init(pList)  !symmetric
      CALL cmat%set(1,1,3._SRK)
      CALL cmat%set(1,2,3._SRK)
      CALL cmat%set(1,3,3._SRK)
      CALL cmat%set(2,2,3._SRK)
      CALL cmat%set(2,3,3._SRK)
      CALL cmat%set(3,3,3._SRK)
      CALL bmat%clear()
      DEALLOCATE(bmat)
      ALLOCATE(DenseRectMatrixType :: bmat)
      CALL pList%clear()
      CALL pList%add('MatrixType->n',3_SNK)
      CALL pList%add('MatrixType->m',3_SNK)
      CALL bmat%init(pList)  
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
      CALL pList%clear()
      CALL pList%add('MatrixType->n',3_SNK)
      CALL pList%add('MatrixType->m',3_SNK)
      CALL cmat%init(pList)  
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
ENDPROGRAM testMatrixTypes
