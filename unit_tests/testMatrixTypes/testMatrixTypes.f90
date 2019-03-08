!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
PROGRAM testMatrixTypes
#include "Futility_DBC.h"
#include "UnitTest.h"
  USE ISO_FORTRAN_ENV
  USE ISO_C_BINDING
  USE UnitTest
  USE IntrType
  USE ExceptionHandler
  USE Futility_DBC
  USE trilinos_interfaces
  USE ParameterLists
  USE ParallelEnv
  USE VectorTypes
  USE MatrixTypes

  IMPLICIT NONE

#ifdef FUTILITY_HAVE_PETSC
#include <petscversion.h>
#if ((PETSC_VERSION_MAJOR>=3) && (PETSC_VERSION_MINOR>=6))
#include <petsc/finclude/petsc.h>
#else
#include <finclude/petsc.h>
#endif
#undef IS
  PetscErrorCode  :: ierr
#else
#ifdef HAVE_MPI
#include <mpif.h>
  INTEGER :: ierr
#endif
#endif

  TYPE(ExceptionHandlerType),TARGET :: e
  TYPE(ParamType) :: pList,optListMat,vecPList

  !Configure exception handler for test
  CALL e%setStopOnError(.FALSE.)
  CALL e%setQuietMode(.TRUE.)
  CALL eParams%addSurrogate(e)
  CALL eMatrixType%addSurrogate(e)

  !Set up optional PL
  CALL optListMat%add('MatrixType->nnz',-1_SNK)
  CALL optListMat%add('MatrixType->isSym',.FALSE.)
  CALL optListMat%add('MatrixType->matType',SPARSE)
  CALL optListMat%add('MatrixType->MPI_Comm_ID',PE_COMM_SELF)

  !Set up vector PL
  CALL vecPList%add('VectorType -> n',1)
  CALL vecPList%add('VectorType -> nlocal',1)
  CALL vecPList%add('VectorType -> MPI_Comm_ID',PE_COMM_SELF)

#ifdef FUTILITY_HAVE_PETSC
  CALL PetscInitialize(PETSC_NULL_CHARACTER,ierr)
#else
#ifdef HAVE_MPI
  CALL MPI_Init(ierr)
#endif
#endif

  CREATE_TEST('Test Matrix Types')
  REGISTER_SUBTEST('TestMatrix',testMatrix)
  REGISTER_SUBTEST('Test Factories',testFactories)
  REGISTER_SUBTEST('TestTranspose',testTransposeMatrix)
  REGISTER_SUBTEST('Test Zeroentries',testzeroEntriesMatrix)
  FINALIZE_TEST()

  CALL optListMat%clear()
  CALL vecPList%clear()
  CALL pList%clear()
  CALL MatrixTypes_Clear_ValidParams()
  CALL VectorType_Clear_ValidParams()

#ifdef FUTILITY_HAVE_PETSC
  CALL PetscFinalize(ierr)
#endif

!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
    SUBROUTINE testMatrix()
      CLASS(MatrixType),ALLOCATABLE :: thisMatrix
      CLASS(RealVectorType),ALLOCATABLE :: xRealVector,yRealVector
      INTEGER(SIK) :: i, ncnt, ncnt2
      INTEGER(SIK) :: ia_vals(4)
      INTEGER(SIK) :: ja_vals(6)
      REAL(SRK) :: a_vals(6),x(3),y(3),val
      REAL(SRK) :: dummy
      REAL(SRK),ALLOCATABLE :: dummyvec(:), dummyvec2(:)
      LOGICAL(SBK) :: bool
#ifdef FUTILITY_HAVE_PETSC
      INTEGER(SIK) :: matsize1,matsize2
      CLASS(VectorType),ALLOCATABLE :: xPETScVector,yPETScVector
#endif
#ifdef FUTILITY_HAVE_Trilinos
      INTEGER(SIK),ALLOCATABLE :: dnnz(:)
      CLASS(VectorType),ALLOCATABLE :: xTrilinosVector,yTrilinosVector
#endif

#ifdef FUTILITY_HAVE_PETSC
      PetscErrorCode  :: ierr
#endif
      CALL vecPList%set('VectorType -> n',3)
      CALL vecPList%set('VectorType -> nlocal',3)
      ALLOCATE(SparseMatrixType :: thisMatrix)
      ALLOCATE(RealVectorType :: xRealVector)
      ALLOCATE(RealVectorType :: yRealVector)
      CALL xRealVector%init(vecPList)
      CALL yRealVector%init(vecPlist)
#ifdef FUTILITY_HAVE_PETSC
      ALLOCATE(PETScVectorType :: xPETScVector)
      ALLOCATE(PETScVectorType :: yPETScVector)
      CALL xPETScVector%init(vecPList)
      CALL yPETScVector%init(vecPlist)
#endif
#ifdef FUTILITY_HAVE_Trilinos
      ALLOCATE(TrilinosVectorType :: xTrilinosVector)
      ALLOCATE(TrilinosVectorType :: yTrilinosVector)
      CALL xTrilinosVector%init(vecPList)
      CALL yTrilinosVector%init(vecPlist)
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
          bool = ((thisMatrix%jPrev == 0).AND.(thisMatrix%iPrev == 0)) &
            .AND.(((.NOT.thisMatrix%isInit).AND.(thisMatrix%jCount == 0)) &
            .AND.((thisMatrix%nnz == 0).AND.(thisMatrix%n == 0)))
          ASSERT(bool, 'sparse%clear()')
          bool = (.NOT.ALLOCATED(thisMatrix%a) .AND. .NOT.ALLOCATED(thisMatrix%ja)) &
                  .AND. .NOT.ALLOCATED(thisMatrix%ia)
          ASSERT(bool, 'sparse%clear()')
          WRITE(*,*) '  Passed: CALL sparse%clear()'
      ENDSELECT

      !check init

      ! build parameter list
      CALL pList%add('MatrixType->n',10_SNK)
      CALL pList%add('MatrixType->nnz',10_SNK)
      CALL pList%validate(pList,optListMat)
      CALL thisMatrix%init(pList)
      SELECTTYPE(thisMatrix)
        TYPE IS(SparseMatrixType)
          !check for success
          bool = (((thisMatrix%isInit) .OR. (thisMatrix%jCount == 0)) &
            .OR. ((thisMatrix%nnz == 10) .OR. (thisMatrix%n == 10))) &
            .OR. ((thisMatrix%jPrev ==0) .OR. (thisMatrix%iPrev ==0))
          ASSERT(bool, 'sparse%init(...)')
          bool = ((SIZE(thisMatrix%a) == 10).OR.(SIZE(thisMatrix%ja) == 10)) &
            .OR.((SIZE(thisMatrix%ia) == 11).OR.(thisMatrix%ia(11) == 11))
          ASSERT(bool, 'sparse%init(...)')
      ENDSELECT
      CALL thisMatrix%clear()
      CALL pList%clear()

      !now check init without m being provided
      CALL pList%add('MatrixType->n',10_SNK)
      CALL pList%validate(pList,optListMat)
      CALL thisMatrix%init(pList) !expect exception
      bool = .NOT. thisMatrix%isInit
      ASSERT(bool, 'sparse%init(...)')
      CALL thisMatrix%clear()

      !init it twice so on 2nd init, isInit==.TRUE.
      CALL thisMatrix%init(pList)
      SELECTTYPE(thisMatrix)
        TYPE IS(SparseMatrixType); thisMatrix%nnz=1
      ENDSELECT
      CALL thisMatrix%init(pList)
      SELECTTYPE(thisMatrix)
        TYPE IS(SparseMatrixType)
          ASSERT(thisMatrix%nnz==1, 'sparse%init(...)')
      ENDSELECT
      !init with n<1
      CALL thisMatrix%clear()
      CALL pList%clear()
      CALL pList%add('MatrixType->n',-1_SNK)
      CALL pList%add('MatrixType->nnz',10_SNK)
      CALL pList%validate(pList,optListMat)
      CALL thisMatrix%init(pList) !expect exception
      ASSERT(.NOT.thisMatrix%isInit, 'sparse%init(...)')
      CALL thisMatrix%clear()
      CALL pList%clear()
      !n<1, and m not provided
      CALL pList%add('MatrixType->n',-1_SNK)
      CALL pList%validate(pList,optListMat)
      CALL thisMatrix%init(pList) !expect exception
      ASSERT(.NOT.thisMatrix%isInit, 'sparse%init(...)')
      CALL thisMatrix%clear()
      CALL pList%clear()
      !init with m<1
      CALL pList%add('MatrixType->n',10_SNK)
      CALL pList%add('MatrixType->nnz',-10_SNK)
      CALL pList%validate(pList,optListMat)
      CALL thisMatrix%init(pList) !expect exception
      ASSERT(.NOT.thisMatrix%isInit, 'sparse%init(...)')
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
            bool = (thisMatrix%a(i) == a_vals(i)) &
              .AND. (thisMatrix%ja(i) == ja_vals(i))
            ASSERT(bool, 'sparse%setShape(...)')
            IF(i < 5) THEN
              bool = thisMatrix%ia(i) == ia_vals(i)
              ASSERT(bool, 'sparse%setShape(...)')
            ENDIF
          ENDDO
          bool = (thisMatrix%jPrev == 3).AND.(thisMatrix%iPrev == 3)
          ASSERT(bool, 'sparse%setShape(...)')
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
            ASSERT(thisMatrix%a(i) == 0, 'sparse%setShape(...)')
          ENDDO
          CALL thisMatrix%clear()

          ! Perform a series of exception tests:
          ! These tests rely on DBC being hit, so if it's not available, don't do them
#ifdef FUTILITY_DBC
          DBC_STOP_ON_FAIL=.false.
          ASSERT(DBC_COUNTER==0, "")
          CALL thisMatrix%init(pList)

          ! 1. Try to set data before setShape is called
          ! Trips jcount>0 require
          CALL thisMatrix%set(1,1,1._SRK)
          ASSERT(DBC_COUNTER==1, "")

          ! 2. Pass in j's out of order in setShape
          ! Hits only ordering require
          CALL thisMatrix%setShape(1,1)
          CALL thisMatrix%setShape(1,4)
          CALL thisMatrix%setShape(1,3)
          ASSERT(DBC_COUNTER==2, "")

          CALL thisMatrix%clear()
          DBC_STOP_ON_FAIL=.true.
          DBC_COUNTER=0
#endif
          ! Exception tests done

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
            bool = (thisMatrix%a(i) == a_vals(i)) &
              .AND. (thisMatrix%ja(i) == ja_vals(i))
            ASSERT(bool, 'sparse%set(...)')
            IF(i < 5) THEN
              bool = thisMatrix%ia(i) == ia_vals(i)
              ASSERT(bool, 'sparse%set(...)')
            ENDIF
          ENDDO
      ENDSELECT


      !Test BLAS_matvec
      x=1.0_SRK
      y=1.0_SRK
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=x,Y=y)
      bool = ALL((y .APPROXEQ. (/4._SRK,4._SRK,16._SRK/)))
      ASSERT(bool, 'BLAS_matvec(THISMATRIX=thisMatrix,X=x,Y=y) -sparse')
      CALL xRealVector%set(1.0_SRK)
      CALL yRealVector%set(1.0_SRK)
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=xRealVector,Y=yRealVector)
      CALL yRealVector%get(y)
      bool = ALL((y .APPROXEQ. (/4._SRK,4._SRK,16._SRK/)))
      ASSERT(bool, 'BLAS_matvec(THISMATRIX=thisMatrix,X=xRealVector,Y=yRealVector) -sparse')
      y=1.0_SRK
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=x,BETA=2.0_SRK,Y=y)
      bool = ALL((y .APPROXEQ. (/5._SRK,5._SRK,17._SRK/)))
      ASSERT(bool, 'BLAS_matvec(THISMATRIX=thisMatrix,X=x,BETA=2.0_SRK,Y=y) -sparse')
      CALL yRealVector%set(1.0_SRK)
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=xRealVector,BETA=2.0_SRK,Y=yRealVector)
      CALL yRealVector%get(y)
      bool = ALL((y .APPROXEQ. (/5._SRK,5._SRK,17._SRK/)))
      ASSERT(bool, 'BLAS_matvec(THISMATRIX=thisMatrix,X=xRealVector,BETA=2.0_SRK,Y=yRealVector) -sparse')
      y=1.0_SRK
      CALL BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=x,Y=y)
      bool = ALL((y .APPROXEQ. (/7._SRK,7._SRK,31._SRK/)))
      ASSERT(bool, 'BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=x,Y=y) -sparse')
      CALL yRealVector%set(1.0_SRK)
      CALL BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=xRealVector,Y=yRealVector)
      CALL yRealVector%get(y)
      bool = ALL((y .APPROXEQ. (/7._SRK,7._SRK,31._SRK/)))
      ASSERT(bool, 'BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=xRealVector,Y=yRealVector) -sparse')
      y=1.0_SRK
      CALL BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=x,BETA=2.0_SRK,Y=y)
      bool = ALL((y .APPROXEQ. (/8._SRK,8._SRK,32._SRK/)))
      ASSERT(bool, 'BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=x,BETA=2.0_SRK,Y=y) -sparse')
      CALL yRealVector%set(1.0_SRK)
      CALL BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=xRealVector,BETA=2.0_SRK,Y=yRealVector)
      CALL yRealVector%get(y)
      bool = ALL((y .APPROXEQ. (/8._SRK,8._SRK,32._SRK/)))
      ASSERT(bool, ' ')
      FINFO() 'BLAS_matvec'
      FINFO() '(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=xRealVector,BETA=2.0_SRK,Y=yRealVector)'
      FINFO() '-sparse'

      ! Set row at a time
      SELECT TYPE (thisMatrix)
      CLASS IS (SparseMatrixType)
        CALL thisMatrix%setRow(1, [1, 3], [11._SRK, 12._SRK])
        CALL thisMatrix%setRow(2, [3], [13._SRK])
        CALL thisMatrix%setRow(3, [1, 2, 3], [15._SRK, 16._SRK, 17._SRK])
        CALL thisMatrix%get(1, 1, val)
        ASSERT(val .APPROXEQ. 11._SRK, "SparseMatrixTvalpe::setRow")
        CALL thisMatrix%get(1, 3, val)
        ASSERT(val .APPROXEQ. 12._SRK, "SparseMatrixTvalpe::setRow")
        CALL thisMatrix%get(2, 3, val)
        ASSERT(val .APPROXEQ. 13._SRK, "SparseMatrixTvalpe::setRow")
        CALL thisMatrix%get(3, 1, val)
        ASSERT(val .APPROXEQ. 15._SRK, "SparseMatrixTvalpe::setRow")
        CALL thisMatrix%get(3, 2, val)
        ASSERT(val .APPROXEQ. 16._SRK, "SparseMatrixTvalpe::setRow")
        CALL thisMatrix%get(3, 3, val)
        ASSERT(val .APPROXEQ. 17._SRK, "SparseMatrixTvalpe::setRow")
      ENDSELECT

! Check sparse triangular solvers
      CALL yRealVector%clear()
      CALL xRealVector%clear()
      CALL thisMatrix%clear()
      IF(ALLOCATED(thisMatrix)) DEALLOCATE(thisMatrix)
      ALLOCATE(SparseMatrixType :: thisMatrix)
      CALL Plist%clear()
      CALL vecPList%clear()
      CALL PList%add('MatrixType->n',4_SIK)
      CALL PList%add('MatrixType->matType',SPARSE)
      CALL Plist%add('MatrixType->nnz',16_SIK)
      CALL vecPList%add('VectorType->n',4_SIK)
      CALL xRealVector%init(vecPList)
      CALL yRealVector%init(vecPList)
      CALL thisMatrix%init(PList)
      SELECTTYPE(x => xRealVector); TYPE IS(RealVectorType)
        CALL x%set(1.0_SRK)
      ENDSELECT
      SELECTTYPE(mat => thisMatrix); TYPE IS(SparseMatrixType)
        CALL mat%setShape(1,1,1.0_SRK)
        CALL mat%setShape(1,2,2.0_SRK)
        CALL mat%setShape(1,3,3.0_SRK)
        CALL mat%setShape(1,4,4.0_SRK)
        CALL mat%setShape(2,1,2.0_SRK)
        CALL mat%setShape(2,2,2.0_SRK)
        CALL mat%setShape(2,3,3.0_SRK)
        CALL mat%setShape(2,4,4.0_SRK)
        CALL mat%setShape(3,1,3.0_SRK)
        CALL mat%setShape(3,2,3.0_SRK)
        CALL mat%setShape(3,3,3.0_SRK)
        CALL mat%setShape(3,4,4.0_SRK)
        CALL mat%setShape(4,1,4.0_SRK)
        CALL mat%setShape(4,2,4.0_SRK)
        CALL mat%setShape(4,3,4.0_SRK)
        CALL mat%setShape(4,4,4.0_SRK)
      ENDSELECT
      ! Test both the VectorType and native interface
      IF(ALLOCATED(dummyvec)) DEALLOCATE(dummyvec)
      ALLOCATE(dummyvec(4))
      dummyvec=(/1.0000000_SRK,-0.5000000_SRK,-0.1666666666666666_SRK,-0.083333333333333333_SRK/)
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=xRealVector,Y=yRealVector,TRANS='N',UPLO='L',DIAG='N')
      bool=ALL(yRealVector%b .APPROXEQA. dummyvec)
      ASSERT(bool,'BLAS_matvec(THISMATRIX,X,Y,TRANS=''N'',UPLO=''L'',DIAG=''N''')
      FINFO() 'Result:',yRealVector%b,'Solution:',dummyvec
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=xRealVector%b,Y=yRealVector%b,TRANS='N',UPLO='L',DIAG='N')
      bool=ALL(yRealVector%b .APPROXEQA. dummyvec)
      ASSERT(bool,'BLAS_matvec(THISMATRIX,X%b,Y%b,TRANS=''N'',UPLO=''L'',DIAG=''N''')
      FINFO() 'Result:',yRealVector%b,'Solution:',dummyvec
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=xRealVector,Y=yRealVector,TRANS='T',UPLO='U',DIAG='N')
      bool=ALL(yRealVector%b .APPROXEQA. dummyvec)
      ASSERT(bool,'BLAS_matvec(THISMATRIX,X,Y,TRANS=''T'',UPLO=''U'',DIAG=''N''')
      FINFO() 'Result:',yRealVector%b,'Solution:',dummyvec
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=xRealVector%b,Y=yRealVector%b,TRANS='T',UPLO='U',DIAG='N')
      bool=ALL(yRealVector%b .APPROXEQA. dummyvec)
      ASSERT(bool,'BLAS_matvec(THISMATRIX,X,Y,TRANS=''T'',UPLO=''U'',DIAG=''N''')
      FINFO() 'Result:',yRealVector%b,'Solution:',dummyvec
      dummyvec=(/0.00_SRK,0.00_SRK,0.00_SRK,0.250_SRK/)
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=xRealVector,Y=yRealVector,TRANS='N',UPLO='U',DIAG='N')
      bool=ALL(yRealVector%b .APPROXEQA. dummyvec)
      ASSERT(bool,'BLAS_matvec(THISMATRIX,X,Y,TRANS=''N'',UPLO=''U'',DIAG=''N''')
      FINFO() 'Result:',yRealVector%b,'Solution:',dummyvec
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=xRealVector%b,Y=yRealVector%b,TRANS='N',UPLO='U',DIAG='N')
      bool=ALL(yRealVector%b .APPROXEQA. dummyvec)
      ASSERT(bool,'BLAS_matvec(THISMATRIX,X%b,Y%b,TRANS=''N'',UPLO=''U'',DIAG=''N''')
      FINFO() 'Result:',yRealVector%b,'Solution:',dummyvec
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=xRealVector,Y=yRealVector,TRANS='T',UPLO='L',DIAG='N')
      bool=ALL(yRealVector%b .APPROXEQA. dummyvec)
      ASSERT(bool,'BLAS_matvec(THISMATRIX,X,Y,TRANS=''T'',UPLO=''L'',DIAG=''N''')
      FINFO() 'Result:',yRealVector%b,'Solution:',dummyvec
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=xRealVector%b,Y=yRealVector%b,TRANS='T',UPLO='L',DIAG='N')
      bool=ALL(yRealVector%b .APPROXEQA. dummyvec)
      ASSERT(bool,'BLAS_matvec(THISMATRIX,X%b,Y%b,TRANS=''T'',UPLO=''L'',DIAG=''N''')
      FINFO() 'Result:',yRealVector%b,'Solution:',dummyvec

      !Check increments /= 1
      CALL xRealVector%clear()
      CALL yRealVector%clear()
      CALL vecPlist%clear()
      CALL vecPList%add('VectorType->n',8_SIK)
      CALL xRealVector%init(vecPList)
      CALL yRealVector%init(vecPList)
      SELECTTYPE(x => xrealVector); TYPE IS(RealVectorType)
        CALL x%set(1.0_SRK)
      ENDSELECT
      dummyvec=(/1.0000000_SRK,-0.5000000_SRK,-0.1666666666666666_SRK,-0.083333333333333333_SRK/)
      ! Negative Increment
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=xRealVector,Y=yRealVector,TRANS='N',UPLO='L',DIAG='N',INCX_IN=-2_SIK)
      bool=ALL(yRealVector%b(7:1:-2) .APPROXEQA. dummyvec)
      ASSERT(bool,'BLAS_matvec(THISMATRIX,X,Y,TRANS=''N'',UPLO=''L'',DIAG=''N'',INXC_IN=-2_SIK')
      FINFO() 'Result:',yRealVector%b(7:1:-2),'Solution:',dummyvec
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=xRealVector%b,Y=yRealVector%b,TRANS='N',UPLO='L',DIAG='N',INCX_IN=-2_SIK)
      bool=ALL(yRealVector%b(7:1:-2) .APPROXEQA. dummyvec)
      ASSERT(bool,'BLAS_matvec(MATRIX,X%b,Y%b,TRANS=''N'',UPLO=''L'',DIAG=''N'',INXC_IN=-2_SIK')
      FINFO() 'Result:',yRealVector%b(7:1:-2),'Solution:',dummyvec
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=xRealVector,Y=yRealVector,TRANS='T',UPLO='U',DIAG='N',INCX_IN=-2_SIK)
      bool=ALL(yRealVector%b(7:1:-2) .APPROXEQA. dummyvec)
      ASSERT(bool,'BLAS_matvec(THISMATRIX,X,Y,TRANS=''T'',UPLO=''U'',DIAG=''N'',INXC_IN=-2_SIK')
      FINFO() 'Result:',yRealVector%b(7:1:-2),'Solution:',dummyvec
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=xRealVector%b,Y=yRealVector%b,TRANS='T',UPLO='U',DIAG='N',INCX_IN=-2_SIK)
      bool=ALL(yRealVector%b(7:1:-2) .APPROXEQA. dummyvec)
      ASSERT(bool,'BLAS_matvec(MATRIX,X%b,Y%b,TRANS=''T'',UPLO=''U'',DIAG=''N'',INXC_IN=-2_SIK')
      FINFO() 'Result:',yRealVector%b(7:1:-2),'Solution:',dummyvec
      ! Positive Increment
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=xRealVector,Y=yRealVector,TRANS='N',UPLO='L',DIAG='N',INCX_IN=2_SIK)
      bool=ALL(yRealVector%b(1:7:2) .APPROXEQA. dummyvec)
      ASSERT(bool,'BLAS_matvec(THISMATRIX,X,Y,TRANS=''N'',UPLO=''L'',DIAG=''N'',INXC_IN=2_SIK')
      FINFO() 'Result:',yRealVector%b(1:7:2),'Solution:',dummyvec
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=xRealVector%b,Y=yRealVector%b,TRANS='N',UPLO='L',DIAG='N',INCX_IN=2_SIK)
      bool=ALL(yRealVector%b(1:7:2) .APPROXEQA. dummyvec)
      ASSERT(bool,'BLAS_matvec(MATRIX,X%b,Y%b,TRANS=''N'',UPLO=''L'',DIAG=''N'',INXC_IN=2_SIK')
      FINFO() 'Result:',yRealVector%b(1:7:2),'Solution:',dummyvec
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=xRealVector,Y=yRealVector,TRANS='T',UPLO='U',DIAG='N',INCX_IN=2_SIK)
      bool=ALL(yRealVector%b(1:7:2) .APPROXEQA. dummyvec)
      ASSERT(bool,'BLAS_matvec(THISMATRIX,X,Y,TRANS=''T'',UPLO=''U'',DIAG=''N'',INXC_IN=2_SIK')
      FINFO() 'Result:',yRealVector%b(1:7:2),'Solution:',dummyvec
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=xRealVector%b,Y=yRealVector%b,TRANS='T',UPLO='U',DIAG='N',INCX_IN=2_SIK)
      bool=ALL(yRealVector%b(1:7:2) .APPROXEQA. dummyvec)
      ASSERT(bool,'BLAS_matvec(MATRIX,X%b,Y%b,TRANS=''T'',UPLO=''U'',DIAG=''N'',INXC_IN=2_SIK')
      FINFO() 'Result:',yRealVector%b(1:7:2),'Solution:',dummyvec

      dummyvec=(/0.00_SRK,0.00_SRK,0.00_SRK,0.250_SRK/)
      ! Negative Increment
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=xRealVector,Y=yRealVector,TRANS='N',UPLO='U',DIAG='N',INCX_IN=-2_SIK)
      bool=ALL(yRealVector%b(7:1:-2) .APPROXEQA. dummyvec)
      ASSERT(bool,'BLAS_matvec(THISMATRIX,X,Y,TRANS=''N'',UPLO=''U'',DIAG=''N'',INXC_IN=-2_SIK')
      FINFO() 'Result:',yRealVector%b(7:1:-2),'Solution:',dummyvec
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=xRealVector%b,Y=yRealVector%b,TRANS='N',UPLO='U',DIAG='N',INCX_IN=-2_SIK)
      bool=ALL(yRealVector%b(7:1:-2) .APPROXEQA. dummyvec)
      ASSERT(bool,'BLAS_matvec(MATRIX,X%b,Y%b,TRANS=''N'',UPLO=''U'',DIAG=''N'',INXC_IN=-2_SIK')
      FINFO() 'Result:',yRealVector%b(7:1:-2),'Solution:',dummyvec
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=xRealVector,Y=yRealVector,TRANS='T',UPLO='L',DIAG='N',INCX_IN=-2_SIK)
      bool=ALL(yRealVector%b(7:1:-2) .APPROXEQA. dummyvec)
      ASSERT(bool,'BLAS_matvec(THISMATRIX,X,Y,TRANS=''T'',UPLO=''L'',DIAG=''N'',INXC_IN=-2_SIK')
      FINFO() 'Result:',yRealVector%b(7:1:-2),'Solution:',dummyvec
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=xRealVector%b,Y=yRealVector%b,TRANS='T',UPLO='L',DIAG='N',INCX_IN=-2_SIK)
      bool=ALL(yRealVector%b(7:1:-2) .APPROXEQA. dummyvec)
      ASSERT(bool,'BLAS_matvec(MATRIX,X%b,Y%b,TRANS=''T'',UPLO=''L'',DIAG=''N'',INXC_IN=-2_SIK')
      FINFO() 'Result:',yRealVector%b(7:1:-2),'Solution:',dummyvec
      !Positive Increment
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=xRealVector,Y=yRealVector,TRANS='N',UPLO='U',DIAG='N',INCX_IN=2_SIK)
      bool=ALL(yRealVector%b(1:7:2) .APPROXEQA. dummyvec)
      ASSERT(bool,'BLAS_matvec(THISMATRIX,X,Y,TRANS=''N'',UPLO=''U'',DIAG=''N'',INXC_IN=2_SIK')
      FINFO() 'Result:',yRealVector%b(1:7:2),'Solution:',dummyvec
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=xRealVector%b,Y=yRealVector%b,TRANS='N',UPLO='U',DIAG='N',INCX_IN=2_SIK)
      bool=ALL(yRealVector%b(1:7:2) .APPROXEQA. dummyvec)
      ASSERT(bool,'BLAS_matvec(MATRIX,X%b,Y%b,TRANS=''N'',UPLO=''U'',DIAG=''N'',INXC_IN=2_SIK')
      FINFO() 'Result:',yRealVector%b(1:7:2),'Solution:',dummyvec
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=xRealVector,Y=yRealVector,TRANS='T',UPLO='L',DIAG='N',INCX_IN=2_SIK)
      bool=ALL(yRealVector%b(1:7:2) .APPROXEQA. dummyvec)
      ASSERT(bool,'BLAS_matvec(THISMATRIX,X,Y,TRANS=''T'',UPLO=''L'',DIAG=''N'',INXC_IN=2_SIK')
      FINFO() 'Result:',yRealVector%b(1:7:2),'Solution:',dummyvec
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=xRealVector%b,Y=yRealVector%b,TRANS='T',UPLO='L',DIAG='N',INCX_IN=2_SIK)
      bool=ALL(yRealVector%b(1:7:2) .APPROXEQA. dummyvec)
      ASSERT(bool,'BLAS_matvec(MATRIX,X%b,Y%b,TRANS=''T'',UPLO=''L'',DIAG=''N'',INXC_IN=2_SIK')
      FINFO() 'Result:',yRealVector%b(1:7:2),'Solution:',dummyvec

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
      SELECTTYPE(thisMatrix)
        TYPE IS(SparseMatrixType); CALL thisMatrix%setShape(1,1)
      ENDSELECT

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
            ASSERT(thisMatrix%a(i) /= 1._SRK, 'sparse%set(...)')
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
          bool = (dummyvec(1) == 1._SRK) .AND. &
                 (dummyvec(2) == 0._SRK) .AND. &
                 (dummyvec(3) == 4._SRK)
          ASSERT(bool, 'sparse%get(...)') !column one check
          bool = (dummyvec(4) == 0._SRK) .AND. &
                 (dummyvec(5) == 0._SRK) .AND. &
                 (dummyvec(6) == 5._SRK)
          ASSERT(bool, 'sparse%get(...)') !column two check
          bool = (dummyvec(7) == 2._SRK) .AND. &
                 (dummyvec(8) == 3._SRK) .AND. &
                 (dummyvec(9) == 6._SRK)
          ASSERT(bool, 'sparse%get(...)') !column three check
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
          bool = (.NOT.(thisMatrix%isInit) .AND. (thisMatrix%n == 0)) &
              .AND. (.NOT.(thisMatrix%isSymmetric) &
              .AND. (.NOT.ALLOCATED(thisMatrix%a)))
          ASSERT(bool, 'densesquare%clear()')
          WRITE(*,*) '  Passed: CALL densesquare%clear()'
      ENDSELECT

      !check init
      CALL pList%clear()
      CALL pList%add('MatrixType->n',10_SNK)
      CALL pList%add('MatrixType->isSym',.FALSE.)
      CALL pList%validate(pList,optListMat)
      CALL thisMatrix%init(pList) !n=10, not symmetric
      SELECTTYPE(thisMatrix)
        TYPE IS(DenseSquareMatrixType)
          bool = ((thisMatrix%isInit) .AND. (thisMatrix%n == 10)) &
              .AND. (.NOT.thisMatrix%isSymmetric)
          ASSERT(bool, 'densesquare%init(...)')
          bool = (SIZE(thisMatrix%a,1) == 10) .AND. &
                 (SIZE(thisMatrix%a,2) == 10)
          ASSERT(bool, 'densesquare%init(...)')
      ENDSELECT
      CALL thisMatrix%clear()
      CALL pList%clear()
      CALL pList%add('MatrixType->n',10_SNK)
      CALL pList%add('MatrixType->isSym',.TRUE.)
      CALL pList%validate(pList,optListMat)
      CALL thisMatrix%init(pList) !n=10, symmetric
      SELECTTYPE(thisMatrix)
        TYPE IS(DenseSquareMatrixType)
          bool = ((thisMatrix%isInit) .AND. (thisMatrix%n == 10)) &
                  .AND. (thisMatrix%isSymmetric)
          ASSERT(bool, 'densesquare%init(...)')
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
          bool = .NOT. thisMatrix%isSymmetric
          ASSERT(bool, 'densesquare%init(...)')
      ENDSELECT
      CALL thisMatrix%clear()
      !test with n<1
      CALL pList%clear()
      CALL pList%add('MatrixType->n',-1_SNK)
      CALL pList%add('MatrixType->isSym',.TRUE.)
      CALL pList%validate(pList,optListMat)
      CALL thisMatrix%init(pList) !expect exception
      bool = .NOT.thisMatrix%isInit
      ASSERT(bool, 'densesquare%init(...)')
      CALL thisMatrix%clear()
      !test with n<1 and no second parameter
      CALL pList%clear()
      CALL pList%add('MatrixType->n',-1_SNK)
      CALL pList%validate(pList,optListMat)
      CALL thisMatrix%init(pList) !expect exception
      bool = .NOT.thisMatrix%isInit
      ASSERT(bool, 'densesquare%init(...)')
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
          bool = ((thisMatrix%a(1,1)==1._SRK) &
              .AND.(thisMatrix%a(1,2)==2._SRK)) &
              .AND.((thisMatrix%a(2,1)==2._SRK) &
              .AND.(thisMatrix%a(2,2)==3._SRK))
          ASSERT(bool, 'densesquare%set(...)')
      ENDSELECT

      !Test BLAS_matvec
      x=1.0_SRK
      y=1.0_SRK
      CALL thisMatrix%clear()
      CALL pList%clear()
      CALL pList%add('MatrixType->n',2_SNK)
      CALL pList%add('MatrixType->isSym',.TRUE.)
      CALL thisMatrix%init(pList)  !symmetric
      CALL thisMatrix%set(1,1,1._SRK)
      CALL thisMatrix%set(1,2,2._SRK)
      CALL thisMatrix%set(2,2,3._SRK)
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=x,Y=y)
      bool = ALL((y .APPROXEQ. (/4._SRK,6._SRK,1._SRK/)))
      ASSERT(bool, 'BLAS_matvec(THISMATRIX=thisMatrix,X=x,Y=y) -densesq')
      CALL xRealVector%set(1.0_SRK)
      CALL yRealVector%set(1.0_SRK)
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=xRealVector,Y=yRealVector)
      CALL yRealVector%get(y)
      bool = ALL((y .APPROXEQ. (/4._SRK,6._SRK,1._SRK/)))
      ASSERT(bool, 'BLAS_matvec(THISMATRIX=thisMatrix,X=xRealVector,Y=yRealVector) -densesq')
      y=1.0_SRK
      CALL BLAS_matvec(THISMATRIX=thisMatrix,trans='t',X=x,Y=y)
      bool = ALL((y .APPROXEQ. (/4._SRK,6._SRK,1._SRK/)))
      ASSERT(bool, 'BLAS_matvec(THISMATRIX=thisMatrix,X=x,Y=y) -densesq')
      CALL yRealVector%set(1.0_SRK)
      CALL BLAS_matvec(THISMATRIX=thisMatrix,trans='t',X=xRealVector,Y=yRealVector)
      CALL yRealVector%get(y)
      bool = ALL((y .APPROXEQ. (/4._SRK,6._SRK,1._SRK/)))
      ASSERT(bool, 'BLAS_matvec(THISMATRIX=thisMatrix,X=xRealVector,Y=yRealVector) -densesq')
      y=1.0_SRK
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=x,BETA=2.0_SRK,Y=y)
      bool = ALL((y .APPROXEQ. (/5._SRK,7._SRK,1._SRK/)))
      ASSERT(bool, 'BLAS_matvec(THISMATRIX=thisMatrix,X=x,BETA=2.0_SRK,Y=y) -densesq')
      CALL yRealVector%set(1.0_SRK)
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=xRealVector,BETA=2.0_SRK,Y=yRealVector)
      CALL yRealVector%get(y)
      bool = ALL((y .APPROXEQ. (/5._SRK,7._SRK,1._SRK/)))
      ASSERT(bool, 'BLAS_matvec(THISMATRIX=thisMatrix,X=xRealVector,BETA=2.0_SRK,Y=yRealVector) -densesq')
      y=1.0_SRK
      CALL BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=x,Y=y)
      bool = ALL((y .APPROXEQ. (/7._SRK,11._SRK,1._SRK/)))
      ASSERT(bool, 'BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=x,Y=y) -densesq')
      CALL yRealVector%set(1.0_SRK)
      CALL BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=xRealVector,Y=yRealVector)
      CALL yRealVector%get(y)
      bool = ALL((y .APPROXEQ. (/7._SRK,11._SRK,1._SRK/)))
      ASSERT(bool, 'BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=xRealVector,Y=yRealVector) -densesq')
      y=1.0_SRK
      CALL BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=x,BETA=2.0_SRK,Y=y)
      bool = ALL((y .APPROXEQ. (/8._SRK,12._SRK,1._SRK/)))
      ASSERT(bool, 'BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=x,BETA=2.0_SRK,Y=y) -densesq')
      CALL yRealVector%set(1.0_SRK)
      CALL BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=xRealVector,BETA=2.0_SRK,Y=yRealVector)
      CALL yRealVector%get(y)
      bool = ALL((y .APPROXEQ. (/8._SRK,12._SRK,1._SRK/)))
      ASSERT(bool, ' ')
      FINFO() 'BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=xRealVector,'
      FINFO() 'BETA=2.0_SRK,Y=yRealVector) -densesq'
      CALL thisMatrix%clear()
      y=2._SRK
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=x,BETA=2.0_SRK,Y=y) !Error check uninit
      bool = ALL((y .APPROXEQ. (/2._SRK,2._SRK,2._SRK/)))
      ASSERT(bool, 'BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=x,BETA=2.0_SRK,Y=y) -densesq')
      CALL yRealVector%set(2._SRK)
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=xRealVector,BETA=2.0_SRK,Y=yRealVector) !Error check uninit
      CALL yRealVector%get(y)
      bool = ALL((y .APPROXEQ. (/2._SRK,2._SRK,2._SRK/)))
      ASSERT(bool, ' ')
      FINFO() 'BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=xRealVector,'
      FINFO() 'BETA=2.0_SRK,Y=yRealVector) -densesq'

      ! Check dense triangular solvers
      CALL yRealVector%clear()
      CALL xRealVector%clear()
      CALL thisMatrix%clear()
      IF(ALLOCATED(thisMatrix)) DEALLOCATE(thisMatrix)
      ALLOCATE(DenseSquareMatrixType :: thisMatrix)
      CALL Plist%clear()
      CALL vecPList%clear()
      CALL PList%add('MatrixType->n',4_SIK)
      CALL PList%add('MatrixType->matType',DENSESQUARE)
      CALL Plist%add('MatrixType->isSym',.FALSE.)
      CALL vecPList%add('VectorType->n',4_SIK)
      CALL xRealVector%init(vecPList)
      CALL yRealVector%init(vecPList)
      CALL thisMatrix%init(PList)
      SELECTTYPE(x => xRealVector); TYPE IS(RealVectorType)
        CALL x%set(1.0_SRK)
      ENDSELECT
      SELECTTYPE(mat => thisMatrix); TYPE IS(DenseSquareMatrixType)
        CALL mat%set(1,1,1.0_SRK)
        CALL mat%set(1,2,2.0_SRK)
        CALL mat%set(1,3,3.0_SRK)
        CALL mat%set(1,4,4.0_SRK)
        CALL mat%set(2,1,2.0_SRK)
        CALL mat%set(2,2,2.0_SRK)
        CALL mat%set(2,3,3.0_SRK)
        CALL mat%set(2,4,4.0_SRK)
        CALL mat%set(3,1,3.0_SRK)
        CALL mat%set(3,2,3.0_SRK)
        CALL mat%set(3,3,3.0_SRK)
        CALL mat%set(3,4,4.0_SRK)
        CALL mat%set(4,1,4.0_SRK)
        CALL mat%set(4,2,4.0_SRK)
        CALL mat%set(4,3,4.0_SRK)
        CALL mat%set(4,4,4.0_SRK)
      ENDSELECT
      ! Test both the VectorType and native interfaces
      IF(ALLOCATED(dummyvec)) DEALLOCATE(dummyvec)
      ALLOCATE(dummyvec(4))
      dummyvec=(/1.0000000_SRK,-0.5000000_SRK,-0.1666666666666666_SRK,-0.083333333333333333_SRK/)
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=xRealVector,Y=yRealVector,TRANS='N',UPLO='L',DIAG='N')
      bool=ALL(yRealVector%b .APPROXEQA. dummyvec)
      ASSERT(bool,'BLAS_matvec(THISMATRIX,X,Y,TRANS=''N'',UPLO=''L'',DIAG=''N''')
      FINFO() 'Result:',yRealVector%b,'Solution:',dummyvec
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=xRealVector%b,Y=yRealVector%b,TRANS='N',UPLO='L',DIAG='N')
      bool=ALL(yRealVector%b .APPROXEQA. dummyvec)
      ASSERT(bool,'BLAS_matvec(THISMATRIX,X%b,Y%b,TRANS=''N'',UPLO=''L'',DIAG=''N''')
      FINFO() 'Result:',yRealVector%b,'Solution:',dummyvec
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=xRealVector,Y=yRealVector,TRANS='T',UPLO='U',DIAG='N')
      bool=ALL(yRealVector%b .APPROXEQA. dummyvec)
      ASSERT(bool,'BLAS_matvec(THISMATRIX,X,Y,TRANS=''T'',UPLO=''U'',DIAG=''N''')
      FINFO() 'Result:',yRealVector%b,'Solution:',dummyvec
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=xRealVector%b,Y=yRealVector%b,TRANS='T',UPLO='U',DIAG='N')
      bool=ALL(yRealVector%b .APPROXEQA. dummyvec)
      ASSERT(bool,'BLAS_matvec(THISMATRIX,X%b,Y%b,TRANS=''T'',UPLO=''U'',DIAG=''N''')
      FINFO() 'Result:',yRealVector%b,'Solution:',dummyvec
      dummyvec=(/0.00_SRK,0.00_SRK,0.00_SRK,0.250_SRK/)
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=xRealVector,Y=yRealVector,TRANS='N',UPLO='U',DIAG='N')
      bool=ALL(yRealVector%b .APPROXEQA. dummyvec)
      ASSERT(bool,'BLAS_matvec(THISMATRIX,X,Y,TRANS=''N'',UPLO=''U'',DIAG=''N''')
      FINFO() 'Result:',yRealVector%b,'Solution:',dummyvec
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=xRealVector%b,Y=yRealVector%b,TRANS='N',UPLO='U',DIAG='N')
      bool=ALL(yRealVector%b .APPROXEQA. dummyvec)
      ASSERT(bool,'BLAS_matvec(THISMATRIX,X%b,Y%b,TRANS=''N'',UPLO=''U'',DIAG=''N''')
      FINFO() 'Result:',yRealVector%b,'Solution:',dummyvec
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=xRealVector,Y=yRealVector,TRANS='T',UPLO='L',DIAG='N')
      bool=ALL(yRealVector%b .APPROXEQA. dummyvec)
      ASSERT(bool,'BLAS_matvec(THISMATRIX,X,Y,TRANS=''T'',UPLO=''L'',DIAG=''N''')
      FINFO() 'Result:',yRealVector%b,'Solution:',dummyvec
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=xRealVector%b,Y=yRealVector%b,TRANS='T',UPLO='L',DIAG='N')
      bool=ALL(yRealVector%b .APPROXEQA. dummyvec)
      ASSERT(bool,'BLAS_matvec(THISMATRIX,X%b,Y%b,TRANS=''T'',UPLO=''L'',DIAG=''N''')
      FINFO() 'Result:',yRealVector%b,'Solution:',dummyvec

      !Check increments /= 1
      CALL xRealVector%clear()
      CALL yRealVector%clear()
      CALL vecPlist%clear()
      CALL vecPList%add('VectorType->n',8_SIK)
      CALL xRealVector%init(vecPList)
      CALL yRealVector%init(vecPList)
      SELECTTYPE(x => xrealVector); TYPE IS(RealVectorType)
        CALL x%set(1.0_SRK)
      ENDSELECT
      dummyvec=(/1.0000000_SRK,-0.5000000_SRK,-0.1666666666666666_SRK,-0.083333333333333333_SRK/)
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=xRealVector,Y=yRealVector,TRANS='N',UPLO='L',DIAG='N',INCX_IN=-2_SIK)
      bool=ALL(yRealVector%b(7:1:-2) .APPROXEQA. dummyvec)
      ASSERT(bool,'BLAS_matvec(THISMATRIX,X,Y,TRANS=''N'',UPLO=''L'',DIAG=''N'',INCX_IN=-2_SIK')
      FINFO() 'Result:',yRealVector%b(7:1:-2),'Solution:',dummyvec
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=xRealVector%b,Y=yRealVector%b,TRANS='N',UPLO='L',DIAG='N',INCX_IN=-2_SIK)
      bool=ALL(yRealVector%b(7:1:-2) .APPROXEQA. dummyvec)
      ASSERT(bool,'BLAS_matvec(THISMATRIX,X%b,Y%b,TRANS=''N'',UPLO=''L'',DIAG=''N'',INCX_IN=-2_SIK')
      FINFO() 'Result:',yRealVector%b(7:1:-2),'Solution:',dummyvec
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=xRealVector,Y=yRealVector,TRANS='T',UPLO='U',DIAG='N',INCX_IN=-2_SIK)
      bool=ALL(yRealVector%b(7:1:-2) .APPROXEQA. dummyvec)
      ASSERT(bool,'BLAS_matvec(THISMATRIX,X,Y,TRANS=''T'',UPLO=''U'',DIAG=''N'',INCX_IN=-2_SIK')
      FINFO() 'Result:',yRealVector%b(7:1:-2),'Solution:',dummyvec
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=xRealVector%b,Y=yRealVector%b,TRANS='T',UPLO='U',DIAG='N',INCX_IN=-2_SIK)
      bool=ALL(yRealVector%b(7:1:-2) .APPROXEQA. dummyvec)
      ASSERT(bool,'BLAS_matvec(THISMATRIX,X%b,Y%b,TRANS=''T'',UPLO=''U'',DIAG=''N'',INCX_IN=-2_SIK')
      FINFO() 'Result:',yRealVector%b(7:1:-2),'Solution:',dummyvec
      dummyvec=(/0.00_SRK,0.00_SRK,0.00_SRK,0.250_SRK/)
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=xRealVector,Y=yRealVector,TRANS='N',UPLO='U',DIAG='N',INCX_IN=-2_SIK)
      bool=ALL(yRealVector%b(7:1:-2) .APPROXEQA. dummyvec)
      ASSERT(bool,'BLAS_matvec(THISMATRIX,X,Y,TRANS=''N'',UPLO=''U'',DIAG=''N'',INCX_IN=-2_SIK')
      FINFO() 'Result:',yRealVector%b(7:1:-2),'Solution:',dummyvec
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=xRealVector%b,Y=yRealVector%b,TRANS='N',UPLO='U',DIAG='N',INCX_IN=-2_SIK)
      bool=ALL(yRealVector%b(7:1:-2) .APPROXEQA. dummyvec)
      ASSERT(bool,'BLAS_matvec(THISMATRIX,X%b,Y%b,TRANS=''N'',UPLO=''U'',DIAG=''N'',INCX_IN=-2_SIK')
      FINFO() 'Result:',yRealVector%b(7:1:-2),'Solution:',dummyvec
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=xRealVector,Y=yRealVector,TRANS='T',UPLO='L',DIAG='N',INCX_IN=-2_SIK)
      bool=ALL(yRealVector%b(7:1:-2) .APPROXEQA. dummyvec)
      ASSERT(bool,'BLAS_matvec(THISMATRIX,X,Y,TRANS=''T'',UPLO=''L'',DIAG=''N'',INCX_IN=-2_SIK')
      FINFO() 'Result:',yRealVector%b(7:1:-2),'Solution:',dummyvec
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=xRealVector%b,Y=yRealVector%b,TRANS='T',UPLO='L',DIAG='N',INCX_IN=-2_SIK)
      bool=ALL(yRealVector%b(7:1:-2) .APPROXEQA. dummyvec)
      ASSERT(bool,'BLAS_matvec(THISMATRIX,X%b,Y%b,TRANS=''T'',UPLO=''L'',DIAG=''N'',INCX_IN=-2_SIK')
      FINFO() 'Result:',yRealVector%b(7:1:-2),'Solution:',dummyvec

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
          bool = ((thisMatrix%a(1,1)==1._SRK) &
              .AND.(thisMatrix%a(1,2)==2._SRK)) &
              .AND.((thisMatrix%a(2,1)==2._SRK) &
              .AND.(thisMatrix%a(2,2)==3._SRK))
          ASSERT(bool, 'densesquare%set(...)')
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

      !Perform test of functionality of get function
      ![1 0 2]
      ![0 0 3]
      ![4 5 6]
      CALL thisMatrix%clear()
      CALL pList%clear()
      CALL pList%add('MatrixType->n',3_SNK)
      CALL pList%add('MatrixType->isSym',.FALSE.)
      CALL thisMatrix%init(pList)
      SELECTTYPE(thisMatrix)
        TYPE IS(DenseSquareMatrixType)
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
          bool = (dummyvec(1) == 1._SRK) .AND. &
                 (dummyvec(2) == 0._SRK) .AND. &
                 (dummyvec(3) == 4._SRK)
          ASSERT(bool, 'densesquare%get(...)')
          FINFO() dummyvec
          bool = (dummyvec(4) == 0._SRK) .AND. &
                 (dummyvec(5) == 0._SRK) .AND. &
                 (dummyvec(6) == 5._SRK)
          ASSERT(bool, 'densesquare%get(...)')
          bool = (dummyvec(7) == 2._SRK) .AND. &
                 (dummyvec(8) == 3._SRK) .AND. &
                 (dummyvec(9) == 6._SRK)
          ASSERT(bool, 'densesquare%get(...)')
      ENDSELECT
      !test with out of bounds i,j, make sure no crash.
      SELECTTYPE(thisMatrix)
        TYPE IS(DenseSquareMatrixType)
          CALL thisMatrix%get(4,2,dummy)
          bool = dummy == -1051._SRK
          ASSERT(bool, 'densesquare%get(...)')
          CALL thisMatrix%get(-1,2,dummy)
          bool = dummy==-1051._SRK
          ASSERT(bool, 'densesquare%get(...)')
          CALL thisMatrix%get(2,-1,dummy)
          bool = dummy==-1051._SRK
          ASSERT(bool, 'densesquare%get(...)')
      ENDSELECT
      !test get with uninit, make sure no crash.
      CALL thisMatrix%clear()
      SELECTTYPE(thisMatrix)
        TYPE IS(DenseSquareMatrixType)
          CALL thisMatrix%get(1,1,dummy)
          bool = dummy == 0.0_SRK
          ASSERT(bool, 'densesquare%get(...)')
      ENDSELECT
      CALL thisMatrix%clear()
      WRITE(*,*) '  Passed: CALL densesquare%get(...)'
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
!  These are unsuported matvecs.  I added an error message in order to catch unsported types so these don't run anymore without throwing an error
      ncnt=e%getCounter(EXCEPTION_ERROR)
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=x,Y=y) !Error check unsupported type
      bool = ALL((y .APPROXEQ. (/2._SRK,2._SRK,2._SRK/)))
      ASSERT(bool, 'BLAS_matvec(...) -tridiag')
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=xRealVector,Y=yRealVector) !Error check unsupported type
      CALL yRealVector%get(y)
      bool = ALL((y .APPROXEQ. (/2._SRK,2._SRK,2._SRK/)))
      ASSERT(bool, 'BLAS_matvec(...) -tridiag')
      ncnt2=e%getCounter(EXCEPTION_ERROR)
      ASSERT(ncnt2-ncnt==2, 'BLAS_matvec(...) -tridiag expected failures')


      CALL thisMatrix%clear()
      SELECTTYPE(thisMatrix)
        TYPE IS(TriDiagMatrixType)
          bool = (.NOT.(thisMatrix%isInit).AND.(thisMatrix%n == 0)) &
              .AND.((.NOT.thisMatrix%isSymmetric) &
              .AND.(.NOT.ALLOCATED(thisMatrix%a)))
          ASSERT(bool, 'tridiag%clear()')
          WRITE(*,*) '  Passed: CALL tridiag%clear()'
      ENDSELECT
      !check init
      CALL pList%clear()
      CALL pList%add('MatrixType->n',10_SNK)
      CALL pList%add('MatrixType->isSym',.FALSE.)
      CALL pList%validate(pList,optListMat)
      CALL thisMatrix%init(pList) !n=10, not symmetric
      SELECTTYPE(thisMatrix)
        TYPE IS(TriDiagMatrixType)
          bool = ((thisMatrix%isInit).AND.(thisMatrix%n == 10)) &
              .AND.(.NOT.thisMatrix%isSymmetric)
          ASSERT(bool, 'tridiag%init(...)')
          bool = (SIZE(thisMatrix%a,1) == 3) .AND. (SIZE(thisMatrix%a,2) == 10)
          ASSERT(bool, 'tridiag%init(...)')
      ENDSELECT
      CALL thisMatrix%clear()
      CALL pList%clear()
      CALL pList%add('MatrixType->n',10_SNK)
      CALL pList%add('MatrixType->isSym',.TRUE.)
      CALL pList%validate(pList,optListMat)
      CALL thisMatrix%init(pList) !n=10, symmetric
      SELECTTYPE(thisMatrix)
        TYPE IS(TriDiagMatrixType)
          bool = ((thisMatrix%isInit).AND.(thisMatrix%n == 10)) &
              .AND.(thisMatrix%isSymmetric)
          ASSERT(bool, 'tridiag%init(...)')
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
          bool = .NOT.thisMatrix%isSymmetric
          ASSERT(bool, 'tridiag%init(...)')
      ENDSELECT
      CALL thisMatrix%clear()
      !test with n<1
      CALL pList%clear()
      CALL pList%add('MatrixType->n',-1_SNK)
      CALL pList%add('MatrixType->isSym',.TRUE.)
      CALL pList%validate(pList,optListMat)
      CALL thisMatrix%init(pList) !expect exception
      bool = .NOT.thisMatrix%isInit
      ASSERT(bool, 'tridiag%init(...)')
      CALL thisMatrix%clear()
      !test with n<1 and no second parameter
      CALL pList%clear()
      CALL pList%add('MatrixType->n',-1_SNK)
      CALL pList%validate(pList,optListMat)
      CALL thisMatrix%init(pList) !expect exception
      bool = .NOT.thisMatrix%isInit
      ASSERT(bool, 'tridiag%init(...)')
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
            bool = thisMatrix%a(2,i) == i
            ASSERT(bool, 'tridiag%set(...)')
          ENDDO
          bool = thisMatrix%a(1,1) == 0
          ASSERT(bool, 'tridiag%set(...)')
          DO i=2,3
            bool = thisMatrix%a(1,i) == i+2
            ASSERT(bool, 'tridiag%set(...)')
          ENDDO
          bool = thisMatrix%a(3,3) == 0
          ASSERT(bool, 'tridiag%set(...)')
          DO i=1,2
            bool = thisMatrix%a(3,i) == i+3
            ASSERT(bool, 'tridiag%set(...)')
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
            bool = thisMatrix%a(2,i) == i
            ASSERT(bool, 'tridiag%set(...)')
          ENDDO
          bool = thisMatrix%a(1,1) == 0
          ASSERT(bool, 'tridiag%set(...)')
          DO i=2,3
            bool = thisMatrix%a(1,i) == i+2
            ASSERT(bool, 'tridiag%set(...)')
          ENDDO
          bool = thisMatrix%a(3,3) == 0
          ASSERT(bool, 'tridiag%set(...)')
          DO i=1,2
            bool = thisMatrix%a(3,i) == i+3
            ASSERT(bool, 'tridiag%set(...)')
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

      !Perform test of functionality of get function
      ![1 2 0]
      ![4 0 3]
      ![0 5 6]
      CALL thisMatrix%clear()
      CALL pList%clear()
      CALL pList%add('MatrixType->n',3_SNK)
      CALL pList%add('MatrixType->isSym',.FALSE.)
      CALL thisMatrix%init(pList)
      SELECTTYPE(thisMatrix)
        TYPE IS(TriDiagMatrixType)
          CALL thisMatrix%set(1,1,1._SRK)
          CALL thisMatrix%set(1,2,2._SRK)
          CALL thisMatrix%set(2,1,4._SRK)
          CALL thisMatrix%set(2,3,3._SRK)
          CALL thisMatrix%set(3,2,5._SRK)
          CALL thisMatrix%set(3,3,6._SRK)
          IF(ALLOCATED(dummyvec)) DEALLOCATE(dummyvec)
          ALLOCATE(dummyvec(9))
          dummyvec=0
          CALL thisMatrix%get(1,1,dummyvec(1))
          CALL thisMatrix%get(2,1,dummyvec(2))
          CALL thisMatrix%get(1,2,dummyvec(4))
          CALL thisMatrix%get(3,2,dummyvec(6))
          CALL thisMatrix%get(2,3,dummyvec(8))
          CALL thisMatrix%get(3,3,dummyvec(9))
          bool = (dummyvec(1) == 1._SRK) .AND. &
                 (dummyvec(2) == 4._SRK) .AND. &
                 (dummyvec(3) == 0._SRK)
          ASSERT(bool, 'tridiag%get(...)') !column one check
          bool = (dummyvec(4) == 2._SRK) .AND. &
                 (dummyvec(5) == 0._SRK) .AND. &
                 (dummyvec(6) == 5._SRK)
          ASSERT(bool, 'tridiag%get(...)') !column two check
          bool = (dummyvec(7) == 0._SRK) .AND. &
                 (dummyvec(8) == 3._SRK) .AND. &
                 (dummyvec(9) == 6._SRK)
          ASSERT(bool, 'tridiag%get(...)') !column three check
      ENDSELECT
      !test with out of bounds i,j, make sure no crash.
      SELECTTYPE(thisMatrix)
        TYPE IS(TriDiagMatrixType)
          CALL thisMatrix%get(4,2,dummy)
          bool = dummy == -1051._SRK
          ASSERT(bool, 'tridiag%get(...)')
          CALL thisMatrix%get(-1,2,dummy)
          bool = dummy == -1051._SRK
          ASSERT(bool, 'tridiag%get(...)')
          CALL thisMatrix%get(2,-1,dummy)
          bool = dummy == -1051._SRK
          ASSERT(bool, 'tridiag%get(...)')
      ENDSELECT
      !test get with uninit, make sure no crash.
      CALL thisMatrix%clear()
      SELECTTYPE(thisMatrix)
        TYPE IS(TriDiagMatrixType)
          dummy=0.0_SRK
          CALL thisMatrix%get(1,1,dummy)
          bool = dummy == 0.0_SRK
          ASSERT(bool, 'tridiag%get(...)')
      ENDSELECT
      CALL thisMatrix%clear()
      WRITE(*,*) '  Passed: CALL tridiag%get(...)'
      DEALLOCATE(thisMatrix)
!
!Test for banded matrices
      ALLOCATE(BandedMatrixType :: thisMatrix)
      SELECT TYPE(thisMatrix)
        TYPE IS(BandedMatrixType)
          !test clear
          !make matrix w/out using untested init
          thisMatrix%isInit=.TRUE.
          thisMatrix%n=10
          thisMatrix%m=15
          thisMatrix%nnz=16
          ALLOCATE(thisMatrix%bands(4))
          ALLOCATE(thisMatrix%bandIdx(4))
          ALLOCATE(thisMatrix%bands(2)%elem(5))
      END SELECT
      CALL thisMatrix%clear()
      SELECT TYPE(thisMatrix)
        TYPE IS(BandedMatrixType)
          bool = (.NOT.(thisMatrix%isInit).AND.(thisMatrix%n == 0)) &
              .AND.((thisMatrix%m == 0) &
              .AND.(thisMatrix%nnz == 0) &
              .AND.(.NOT.ALLOCATED(thisMatrix%bands)))
          ASSERT(bool, 'banded%clear()')
          WRITE(*,*) '  Passed: CALL banded%clear()'
      END SELECT
      !check init
      CALL pList%clear()
      CALL pList%add('MatrixType->n',10_SNK)
      CALL pList%add('MatrixType->m',15_SNK)
      CALL pList%add('MatrixType->nnz',12_SNK)
      CALL pList%validate(pList,optListMat)
      CALL thisMatrix%init(pList)
      SELECTTYPE(thisMatrix)
        TYPE IS(BandedMatrixType)
          bool = (( thisMatrix%isInit).AND.(thisMatrix%n == 10)) &
              .AND.((thisMatrix%m == 15).AND.(thisMatrix%nnz == 12))
          ASSERT(bool, 'banded%init(...)')
      ENDSELECT
      CALL thisMatrix%clear()
      !test with double init (isInit==true on 2nd try)

      CALL thisMatrix%init(pList)
      SELECTTYPE(thisMatrix)
        TYPE IS(BandedMatrixType); thisMatrix%m=1
      ENDSELECT
      CALL thisMatrix%init(pList)
      SELECTTYPE(thisMatrix)
        TYPE IS(BandedMatrixType)
          bool = thisMatrix%m == 1
          ASSERT(bool, 'banded%init(...)')
      ENDSELECT
      CALL thisMatrix%clear()
      !test with n<1
      CALL pList%clear()
      CALL pList%add('MatrixType->n',-1_SNK)
      CALL pList%add('MatrixType->m',10_SNK)
      CALL pList%add('MatrixType->nnz',12_SNK)
      CALL pList%validate(pList,optListMat)
      CALL thisMatrix%init(pList) !expect exception
      bool = .NOT.thisMatrix%isInit
      ASSERT(bool, 'banded%init(...)')
      CALL thisMatrix%clear()
      !test with m<1
      CALL pList%clear()
      CALL pList%add('MatrixType->n',10_SNK)
      CALL pList%add('MatrixType->m',-1_SNK)
      CALL pList%add('MatrixType->nnz',12_SNK)
      CALL pList%validate(pList,optListMat)
      CALL thisMatrix%init(pList) !expect exception
      bool = .NOT.thisMatrix%isInit
      ASSERT(bool, 'banded%init(...)')
      CALL thisMatrix%clear()
      !test with nnz<1
      CALL pList%clear()
      CALL pList%add('MatrixType->n',10_SNK)
      CALL pList%add('MatrixType->m',15_SNK)
      CALL pList%add('MatrixType->nnz',-1_SNK)
      CALL pList%validate(pList,optListMat)
      CALL thisMatrix%init(pList) !expect exception
      bool = .NOT.thisMatrix%isInit
      ASSERT(bool, 'banded%init(...)')
      CALL thisMatrix%clear()
      WRITE(*,*) '  Passed: CALL banded%init(...)'

      CALL thisMatrix%clear()
      CALL pList%clear()

      !check set
      !test normal diag use case
      !want to build:
      ![1 2 0 0]
      ![0 3 0 0]
      ![0 0 5 6]
      ![0 9 0 7]

      CALL pList%add('MatrixType->n',4_SNK)
      CALL pList%add('MatrixType->m',4_SNK)
      CALL pList%add('MatrixType->nnz',7_SNK)
      CALL pList%validate(pList,optListMat)
      CALL thisMatrix%init(pList)
      CALL thisMatrix%set(1,1,1._SRK)
      CALL thisMatrix%set(1,2,2._SRK)
      CALL thisMatrix%set(2,2,3._SRK)
      CALL thisMatrix%set(3,3,5._SRK)
      CALL thisMatrix%set(3,4,6._SRK)
      CALL thisMatrix%set(4,4,7._SRK)
      CALL thisMatrix%set(4,2,9._SRK)
      SELECTTYPE(thisMatrix)
        TYPE IS(BandedMatrixType)
          CALL thisMatrix%assemble()
          bool = .TRUE.
          bool = bool .AND. thisMatrix%bands(2)%elem(1) == 1
          bool = bool .AND. thisMatrix%bands(2)%elem(2) == 3
          bool = bool .AND. thisMatrix%bands(2)%elem(3) == 5
          bool = bool .AND. thisMatrix%bands(2)%elem(4) == 7
          bool = bool .AND. thisMatrix%bands(3)%elem(1) == 2
          bool = bool .AND. thisMatrix%bands(3)%elem(2) == 6
          bool = bool .AND. thisMatrix%bands(1)%elem(1) == 9
          ASSERT(bool, 'banded%set(...)')
      ENDSELECT
      WRITE(*,*) '  Passed: CALL banded%set(...)'
      CALL thisMatrix%clear()
      CALL pList%clear()

      !check get functionality
      ![1 2 0 0]
      ![0 3 0 0]
      ![0 0 5 6]
      ![0 9 0 7]
      !with main diagonal split [1,3],[5,7]
      CALL pList%add('MatrixType->n',4_SNK)
      CALL pList%add('MatrixType->m',4_SNK)
      CALL pList%add('MatrixType->nnz',7_SNK)
      CALL pList%validate(pList,optListMat)
      CALL thisMatrix%init(pList)
      CALL thisMatrix%set(1,1,1._SRK)
      CALL thisMatrix%set(1,2,2._SRK)
      CALL thisMatrix%set(2,2,3._SRK)
      CALL thisMatrix%set(3,3,5._SRK)
      CALL thisMatrix%set(3,4,6._SRK)
      CALL thisMatrix%set(4,4,7._SRK)
      CALL thisMatrix%set(4,2,9._SRK)

      SELECT TYPE(thisMatrix)
      TYPE IS(BandedMatrixType)
        CALL thisMatrix%assemble()
      END SELECT

      IF(ALLOCATED(dummyvec)) DEALLOCATE(dummyvec)
      ALLOCATE(dummyvec(10))
      dummyvec=0
      CALL thisMatrix%get(1,1,dummyvec(1))
      CALL thisMatrix%get(1,2,dummyvec(2))
      CALL thisMatrix%get(2,2,dummyvec(3))
      CALL thisMatrix%get(2,3,dummyvec(4))
      CALL thisMatrix%get(3,3,dummyvec(5))
      CALL thisMatrix%get(3,4,dummyvec(6))
      CALL thisMatrix%get(4,4,dummyvec(7))
      CALL thisMatrix%get(3,1,dummyvec(8))
      CALL thisMatrix%get(4,2,dummyvec(9))
      CALL thisMatrix%get(1,4,dummyvec(10))

      bool = .TRUE.
      bool = bool .AND. dummyvec(1) == 1
      bool = bool .AND. dummyvec(2) == 2
      bool = bool .AND. dummyvec(3) == 3
      bool = bool .AND. dummyvec(4) == 0
      bool = bool .AND. dummyvec(5) == 5
      bool = bool .AND. dummyvec(6) == 6
      bool = bool .AND. dummyvec(7) == 7
      bool = bool .AND. dummyvec(8) == 0
      bool = bool .AND. dummyvec(9) == 9
      bool = bool .AND. dummyvec(10) == 0
      ASSERT(bool, 'banded%get(...)')

      !test get with uninit, make sure no crash.
      CALL thisMatrix%clear()
      SELECTTYPE(thisMatrix)
        TYPE IS(BandedMatrixType)
          dummy=0.0_SRK
          CALL thisMatrix%get(1,1,dummy)
      ENDSELECT
      IF(ALLOCATED(dummyvec)) DEALLOCATE(dummyvec)
      !check matrix that hasnt been init, i,j out of bounds
      CALL thisMatrix%clear()
      CALL pList%add('MatrixType->n',4_SNK)
      CALL pList%add('MatrixType->m',4_SNK)
      CALL pList%add('MatrixType->nnz',4_SNK)
      CALL thisMatrix%init(pList)
      SELECTTYPE(thisMatrix)
        TYPE IS(BandedMatrixType)
          CALL thisMatrix%get(-1,1,dummy)
          CALL thisMatrix%get(1,-1,dummy)
          CALL thisMatrix%get(5,1,dummy)
          CALL thisMatrix%get(1,5,dummy)
      ENDSELECT
      WRITE(*,*) '  Passed: CALL banded%get(...)'
      !check matvec functionality
      ![1 2 0 0]
      ![0 3 0 0]
      ![0 0 5 6]
      ![0 9 0 7]
      !with main diagonal split [1,3],[5,7]
      CALL thisMatrix%clear()
      CALL pList%clear()
      CALL pList%add('MatrixType->n',4_SNK)
      CALL pList%add('MatrixType->m',4_SNK)
      CALL pList%add('MatrixType->nnz',7_SNK)
      CALL pList%validate(pList,optListMat)
      CALL thisMatrix%init(pList)
      CALL thisMatrix%set(1,1,1._SRK)
      CALL thisMatrix%set(1,2,2._SRK)
      CALL thisMatrix%set(2,2,3._SRK)
      CALL thisMatrix%set(3,3,5._SRK)
      CALL thisMatrix%set(3,4,6._SRK)
      CALL thisMatrix%set(4,4,7._SRK)
      CALL thisMatrix%set(4,2,9._SRK)
      IF(ALLOCATED(dummyvec)) DEALLOCATE(dummyvec)
      IF(ALLOCATED(dummyvec2)) DEALLOCATE(dummyvec2)
      ALLOCATE(dummyvec(4))
      ALLOCATE(dummyvec2(4))
      ! Check zero vector
      dummyvec=0
      dummyvec2=1
      SELECTTYPE(thisMatrix)
        TYPE IS(BandedMatrixType)
          CALL thisMatrix%assemble()
          CALL BLAS_matvec(THISMATRIX=thisMatrix,X=dummyvec,Y=dummyvec2)
          DO i=1,4
            bool = ABS(dummyvec2(i)) < 1E-6
            ASSERT(bool, 'banded%matvec(...)')
          ENDDO
      ENDSELECT
      ! Check for non-trivial vector
      dummyvec=(/1._SRK,2._SRK,3._SRK,4._SRK/)
      SELECTTYPE(thisMatrix)
        TYPE IS(BandedMatrixType)
        CALL BLAS_matvec(THISMATRIX=thisMatrix,X=dummyvec,Y=dummyvec2)
        bool = dummyvec2(1) == 5._SRK
        ASSERT(bool, 'banded%matvec(...)')
        bool = dummyvec2(2) == 6._SRK
        ASSERT(bool, 'banded%matvec(...)')
        bool = dummyvec2(3) == 39._SRK
        ASSERT(bool, 'banded%matvec(...)')
        bool = dummyvec2(4) == 46._SRK
        ASSERT(bool, 'banded%matvec(...)')
      ENDSELECT
      WRITE(*,*) '  Passed: CALL banded%matvec(...)'
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
          bool = ((.NOT.thisMatrix%isInit).AND.(thisMatrix%n == 0)) &
              .AND.((thisMatrix%m == 0) &
              .AND.(.NOT.ALLOCATED(thisMatrix%a)))
          ASSERT(bool, 'denserect%clear()')
          WRITE(*,*) '  Passed: CALL denserect%clear()'
      ENDSELECT
      !check init
      CALL pList%clear()
      CALL pList%add('MatrixType->n',10_SNK)
      CALL pList%add('MatrixType->m',15_SNK)
      CALL pList%validate(pList,optListMat)
      CALL thisMatrix%init(pList)
      SELECTTYPE(thisMatrix)
        TYPE IS(DenseRectMatrixType)
          bool = (( thisMatrix%isInit).AND.(thisMatrix%n == 10)) &
              .AND.((thisMatrix%m == 15))
          ASSERT(bool, 'denserect%init(...)')
          bool = (SIZE(thisMatrix%a,1) == 10) &
            .AND.(SIZE(thisMatrix%a,2) == 15)
          ASSERT(bool, 'denserect%init(...)')
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
          bool = thisMatrix%m == 1
          ASSERT(bool, 'denserect%init(...)')
      ENDSELECT
      CALL thisMatrix%clear()
      !test with n<1
      CALL pList%clear()
      CALL pList%add('MatrixType->n',-1_SNK)
      CALL pList%add('MatrixType->m',10_SNK)
      CALL pList%validate(pList,optListMat)
      CALL thisMatrix%init(pList) !expect exception
      bool = .NOT.thisMatrix%isInit
      ASSERT(bool, 'denserect%init(...)')
      CALL thisMatrix%clear()
      !test with m<1
      CALL pList%clear()
      CALL pList%add('MatrixType->n',10_SNK)
      CALL pList%add('MatrixType->m',-1_SNK)
      CALL pList%validate(pList,optListMat)
      CALL thisMatrix%init(pList) !expect exception
      bool = .NOT.thisMatrix%isInit
      ASSERT(bool, 'denserect%init(...)')
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
            bool = (thisMatrix%a(1,i)== i).AND.(thisMatrix%a(2,i)== 3+i)
            ASSERT(bool, 'denserect%set(...)')
          ENDDO
      ENDSELECT

      !Test BLAS_matvec
      x=1.0_SRK
      y=1.0_SRK
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=x,Y=y)
      bool = ALL((y .APPROXEQ. (/7._SRK,16._SRK,1._SRK/)))
      ASSERT(bool, 'BLAS_matvec(THISMATRIX=thisMatrix,X=x,Y=y) -denserect')
      CALL xRealVector%set(1.0_SRK)
      CALL yRealVector%set(1.0_SRK)
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=xRealVector,Y=yRealVector)
      CALL yRealVector%get(y)
      bool = ALL((y .APPROXEQ. (/7._SRK,16._SRK,1._SRK/)))
      ASSERT(bool, 'BLAS_matvec(THISMATRIX=thisMatrix,X=xRealVector,Y=yRealVector) -denserect')
      y=1.0_SRK
      CALL BLAS_matvec(THISMATRIX=thisMatrix,trans='t',X=x,Y=y)
      bool = ALL((y .APPROXEQ. (/6._SRK,8._SRK,10._SRK/)))
      ASSERT(bool, 'BLAS_matvec(THISMATRIX=thisMatrix,X=x,Y=y) -denserect')
      CALL yRealVector%set(1.0_SRK)
      CALL BLAS_matvec(THISMATRIX=thisMatrix,trans='t',X=xRealVector,Y=yRealVector)
      CALL yRealVector%get(y)
      bool = ALL((y .APPROXEQ. (/6._SRK,8._SRK,10._SRK/)))
      ASSERT(bool, 'BLAS_matvec(THISMATRIX=thisMatrix,X=xRealVector,Y=yRealVector) -denserect')
      y=1.0_SRK
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=x,BETA=2.0_SRK,Y=y)
      bool = ALL((y .APPROXEQ. (/8._SRK,17._SRK,1._SRK/)))
      ASSERT(bool, 'BLAS_matvec(THISMATRIX=thisMatrix,X=x,BETA=2.0_SRK,Y=y) -denserect')
      CALL yRealVector%set(1.0_SRK)
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=xRealVector,BETA=2.0_SRK,Y=yRealVector)
      CALL yRealVector%get(y)
      bool = ALL((y .APPROXEQ. (/8._SRK,17._SRK,1._SRK/)))
      ASSERT(bool, 'BLAS_matvec(THISMATRIX=thisMatrix,X=xRealVector,BETA=2.0_SRK,Y=yRealVector) -denserect')
      y=1.0_SRK
      CALL BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=x,Y=y)
      bool = ALL((y .APPROXEQ. (/13._SRK,31._SRK,1._SRK/)))
      ASSERT(bool, 'BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=x,Y=y) -denserect')
      CALL yRealVector%set(1.0_SRK)
      CALL BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=xRealVector,Y=yRealVector)
      CALL yRealVector%get(y)
      bool = ALL((y .APPROXEQ. (/13._SRK,31._SRK,1._SRK/)))
      ASSERT(bool, 'BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=xRealVector,Y=yRealVector) -denserect')
      y=1.0_SRK
      CALL BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=x,BETA=2.0_SRK,Y=y)
      bool = ALL((y .APPROXEQ. (/14._SRK,32._SRK,1._SRK/)))
      ASSERT(bool, 'BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=x,BETA=2.0_SRK,Y=y) -denserect')
      CALL yRealVector%set(1.0_SRK)
      CALL BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=xRealVector,BETA=2.0_SRK,Y=yRealVector)
      CALL yRealVector%get(y)
      bool = ALL((y .APPROXEQ. (/14._SRK,32._SRK,1._SRK/)))
      ASSERT(bool, ' ')
      FINFO() 'BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=xRealVector,BETA=2.0_SRK,Y=yRealVector)",'
      FINFO() '-denserect'
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

      !Perform test of functionality of get function
      ![1 0 2]
      ![4 5 3]
      CALL thisMatrix%clear()
      CALL pList%clear()
      CALL pList%add('MatrixType->n',2_SNK)
      CALL pList%add('MatrixType->m',3_SNK)
      CALL thisMatrix%init(pList)
      SELECTTYPE(thisMatrix)
        TYPE IS(DenseRectMatrixType)
          CALL thisMatrix%set(1,1,1._SRK)
          CALL thisMatrix%set(1,3,2._SRK)
          CALL thisMatrix%set(2,1,4._SRK)
          CALL thisMatrix%set(2,2,5._SRK)
          CALL thisMatrix%set(2,3,3._SRK)
          IF(ALLOCATED(dummyvec)) DEALLOCATE(dummyvec)
          ALLOCATE(dummyvec(6))
          CALL thisMatrix%get(1,1,dummyvec(1))
          CALL thisMatrix%get(2,1,dummyvec(2))
          CALL thisMatrix%get(1,2,dummyvec(3))
          CALL thisMatrix%get(2,2,dummyvec(4))
          CALL thisMatrix%get(1,3,dummyvec(5))
          CALL thisMatrix%get(2,3,dummyvec(6))
          bool = (dummyvec(1) == 1._SRK)  .AND. &
              (dummyvec(2) == 4._SRK)
          ASSERT(bool, 'densesquare%get(...)') !column one check
          bool = (dummyvec(3) == 0._SRK).AND. &
                  (dummyvec(4) == 5._SRK)
          ASSERT(bool, 'densesquare%get(...)')
          bool = (dummyvec(5) == 2._SRK).AND. &
                  (dummyvec(6) == 3._SRK)
          ASSERT(bool, 'densesquare%get(...)')
      ENDSELECT
      !test with out of bounds i,j, make sure no crash.
      SELECTTYPE(thisMatrix)
        TYPE IS(DenseRectMatrixType)
          CALL thisMatrix%get(4,2,dummy)
          bool = dummy == -1051._SRK
          ASSERT(bool, 'densesquare%get(...)')
          CALL thisMatrix%get(-1,2,dummy)
          bool = dummy == -1051._SRK
          ASSERT(bool, 'densesquare%get(...)')
          CALL thisMatrix%get(2,-1,dummy)
          bool = dummy == -1051._SRK
          ASSERT(bool, 'densesquare%get(...)')
      ENDSELECT
      !test get with uninit, make sure no crash.
      CALL thisMatrix%clear()
      SELECTTYPE(thisMatrix)
        TYPE IS(DenseRectMatrixType)
          dummy=0.0_SRK
          CALL thisMatrix%get(1,1,dummy)
          bool = dummy == 0.0_SRK
          ASSERT(bool, 'densesquare%get(...)')
      ENDSELECT
      CALL thisMatrix%clear()
      WRITE(*,*) '  Passed: CALL densesquare%get(...)'
      DEALLOCATE(thisMatrix)

!Test for PETSc matrices (if necessary)
#ifdef FUTILITY_HAVE_PETSC

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
          bool = ((.NOT.thisMatrix%isInit).AND.(thisMatrix%n == 0)) &
              .AND.((.NOT.thisMatrix%isSymmetric))
          ASSERT(bool, 'petscsparse%clear()')
          !check if pointer fo a is null (not supported till 3.3)
      ENDSELECT
      !check init
      CALL pList%clear()
      CALL pList%add('MatrixType->n',10_SNK)
      CALL pList%add('MatrixType->isSym',.FALSE.)
      CALL pList%add('MatrixType->matType',SPARSE)
      CALL pList%validate(pList,optListMat)
      CALL thisMatrix%init(pList) !n=10, not symmetric (0), sparse (0)
      SELECTTYPE(thisMatrix)
        TYPE IS(PETScMatrixType)
          bool = ((thisMatrix%isInit).AND.(thisMatrix%n == 10)) &
              .AND.(.NOT.thisMatrix%isSymmetric)
          ASSERT(bool, 'petscsparse%init(...)')
          CALL MatGetSize(thisMatrix%a,matsize1,matsize2,ierr)
          bool = (matsize1 == 10) .AND. (matsize2 == 10)
          ASSERT(bool, 'petscsparse%init(...)')
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
          bool = ((thisMatrix%isInit).AND.(thisMatrix%n == 10)) &
              .AND.(thisMatrix%isSymmetric)
          ASSERT(bool, 'petscsparse%init(...)')
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
          bool = .NOT.thisMatrix%isSymmetric
          ASSERT(bool, 'petscsparse%init(...)')
      ENDSELECT
      CALL thisMatrix%clear()
      !test with n<1
      CALL pList%clear()
      CALL pList%add('MatrixType->n',-1_SNK)
      CALL pList%add('MatrixType->isSym',.TRUE.)
      CALL pList%add('MatrixType->matType',SPARSE)
      CALL pList%validate(pList,optListMat)
      CALL thisMatrix%init(pList) !expect exception
      bool = .NOT.thisMatrix%isInit
      ASSERT(bool, 'petscsparse%init(...)')
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
          bool = dummy==1._SRK
          ASSERT(bool, 'petscsparse%set(...)')
          CALL MatGetValues(thisMatrix%a,1,0,1,1,dummy,ierr)
          bool = dummy==2._SRK
          ASSERT(bool, 'petscsparse%set(...)')
          CALL MatGetValues(thisMatrix%a,1,1,1,1,dummy,ierr)
          bool = dummy==3._SRK
          ASSERT(bool, 'petscsparse%set(...)')
      ENDSELECT

      SELECT TYPE (thisMatrix)
      CLASS IS (DistributedMatrixType)
        ! Overwrite with setRow
        CALL thisMatrix%setRow(1,[1, 2],[10._SRK, 11._SRK])
        CALL thisMatrix%setRow(2,[2],[13._SRK])
        CALL thisMatrix%get(1, 1, val)
        ASSERT(val .APPROXEQ. 10._SRK, "PETScMatrixTvalpe::setRow")
        CALL thisMatrix%get(1, 2, val)
        ASSERT(val .APPROXEQ. 11._SRK, "PETScMatrixTvalpe::setRow")
        CALL thisMatrix%get(2, 2, val)
        ASSERT(val .APPROXEQ. 13._SRK, "PETScMatrixTvalpe::setRow")
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
          bool = dummy==1._SRK
          ASSERT(bool, 'petscsparse%set(...)')
          CALL MatGetValues(thisMatrix%a,1,0,1,1,dummy,ierr)
          bool = dummy==2._SRK
          ASSERT(bool, 'petscsparse%set(...)')
          CALL MatGetValues(thisMatrix%a,1,1,1,0,dummy,ierr)
          bool = dummy==2._SRK
          ASSERT(bool, 'petscsparse%set(...)')
          CALL MatGetValues(thisMatrix%a,1,1,1,1,dummy,ierr)
          bool = dummy==3._SRK
          ASSERT(bool, 'petscsparse%set(...)')
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
      bool = ALL((y .APPROXEQ. (/4._SRK,8._SRK,16._SRK/)))
      ASSERT(bool, 'BLAS_matvec(THISMATRIX=thisMatrix,X=x,Y=y) -petscsparse')
      CALL xRealVector%set(1.0_SRK)
      CALL yRealVector%set(1.0_SRK)
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=xRealVector,Y=yRealVector)
      CALL yRealVector%get(y)
      bool = ALL((y .APPROXEQ. (/4._SRK,8._SRK,16._SRK/)))
      ASSERT(bool, 'BLAS_matvec(THISMATRIX=thisMatrix,X=xRealVector,Y=yRealVector) -petscsparse')
      CALL xPETScVector%set(1.0_SRK)
      CALL yPETScVector%set(1.0_SRK)
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=xPETScVector,Y=yPETScVector)
      CALL yRealVector%get(y)
      bool = ALL((y .APPROXEQ. (/4._SRK,8._SRK,16._SRK/)))
      ASSERT(bool, 'BLAS_matvec(THISMATRIX=thisMatrix,X=PETScVector,Y=yPETScVector) -petscsparse')
      y=1.0_SRK
      CALL BLAS_matvec(THISMATRIX=thisMatrix,trans='t',X=x,Y=y)
      bool = ALL((y .APPROXEQ. (/6._SRK,10._SRK,12._SRK/)))
      ASSERT(bool, "BLAS_matvec(THISMATRIX=thisMatrix,trans='t',X=x,Y=y) -petscsparse")
      CALL yRealVector%set(1.0_SRK)
      CALL BLAS_matvec(THISMATRIX=thisMatrix,trans='t',X=xRealVector,Y=yRealVector)
      CALL yRealVector%get(y)
      bool = ALL((y .APPROXEQ. (/6._SRK,10._SRK,12._SRK/)))
      ASSERT(bool, 'BLAS_matvec(THISMATRIX=thisMatrix,X=xRealVector,Y=yRealVector) -petscsparse')
      CALL yPETScVector%set(1.0_SRK)
      CALL BLAS_matvec(THISMATRIX=thisMatrix,trans='t',X=xPETScVector,Y=yPETScVector)
      CALL yPETScVector%get(y)
      bool = ALL((y .APPROXEQ. (/6._SRK,10._SRK,12._SRK/)))
      ASSERT(bool, "BLAS_matvec(THISMATRIX=thisMatrix,trans='t',X=xPETScVector,Y=yPETScVector) -petscsparse")
      y=1.0_SRK
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=x,BETA=2.0_SRK,Y=y)
      bool = ALL((y .APPROXEQ. (/5._SRK,9._SRK,17._SRK/)))
      ASSERT(bool, 'BLAS_matvec(THISMATRIX=thisMatrix,X=x,BETA=2.0_SRK,Y=y) -petscsparse')
      CALL yRealVector%set(1.0_SRK)
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=xRealVector,BETA=2.0_SRK,Y=yRealVector)
      CALL yRealVector%get(y)
      bool = ALL((y .APPROXEQ. (/5._SRK,9._SRK,17._SRK/)))
      ASSERT(bool, 'BLAS_matvec(THISMATRIX=thisMatrix,X=xRealVector,BETA=2.0_SRK,Y=yRealVector) -petscsparse')
      CALL yPETScVector%set(1.0_SRK)
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=xPETScVector,BETA=2.0_SRK,Y=yPETScVector)
      CALL yPETScVector%get(y)
      bool = ALL((y .APPROXEQ. (/5._SRK,9._SRK,17._SRK/)))
      ASSERT(bool, 'BLAS_matvec(THISMATRIX=thisMatrix,X=xPETScVector,BETA=2.0_SRK,Y=yPETScVector) -petscsparse')
      y=1.0_SRK
      CALL BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=x,Y=y)
      bool = ALL((y .APPROXEQ. (/7._SRK,15._SRK,31._SRK/)))
      ASSERT(bool, 'BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=x,Y=y) -petscsparse')
      CALL yRealVector%set(1.0_SRK)
      CALL BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=xRealVector,Y=yRealVector)
      CALL yRealVector%get(y)
      bool = ALL((y .APPROXEQ. (/7._SRK,15._SRK,31._SRK/)))
      ASSERT(bool, 'BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=xRealVector,Y=yRealVector) -petscsparse')
      CALL yPETScVector%set(1.0_SRK)
      CALL BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=xPETScVector,Y=yPETScVector)
      CALL yPETScVector%get(y)
      bool = ALL((y .APPROXEQ. (/7._SRK,15._SRK,31._SRK/)))
      ASSERT(bool, 'BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=xPETScVector,Y=yPETScVector) -petscsparse')
      y=1.0_SRK
      CALL BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=x,BETA=2.0_SRK,Y=y)
      bool = ALL((y .APPROXEQ. (/8._SRK,16._SRK,32._SRK/)))
      ASSERT(bool, 'BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=x,BETA=2.0_SRK,Y=y) -petscsparse')
      CALL yRealVector%set(1.0_SRK)
      CALL BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=xRealVector,BETA=2.0_SRK,Y=yRealVector)
      CALL yRealVector%get(y)
      bool = ALL((y .APPROXEQ. (/8._SRK,16._SRK,32._SRK/)))
      ASSERT(bool, 'BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=xRealVector,BETA=2.0_SRK,Y=yRealVector)')
      FINFO() '-petscsparse'
      CALL yPETScVector%set(1.0_SRK)
      CALL BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=xPETScVector,BETA=2.0_SRK,Y=yPETScVector)
      CALL yPETScVector%get(y)
      bool = ALL((y .APPROXEQ. (/8._SRK,16._SRK,32._SRK/)))
      ASSERT(bool, 'BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=xPETScVector,BETA=2.0_SRK,Y=yPETScVector)')
      FINFO() '-petscsparse'
      CALL thisMatrix%clear()
      y=2._SRK
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=x,BETA=2.0_SRK,Y=y) !Error check uninit
      bool = ALL((y .APPROXEQ. (/2._SRK,2._SRK,2._SRK/)))
      ASSERT(bool, 'BLAS_matvec(THISMATRIX=thisMatrix,X=x,BETA=2.0_SRK,Y=y) -petscsparse')
      CALL yRealVector%set(2._SRK)
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=xRealVector,BETA=2.0_SRK,Y=yRealVector) !Error check uninit
      CALL yRealVector%get(y)
      bool = ALL((y .APPROXEQ. (/2._SRK,2._SRK,2._SRK/)))
      ASSERT(bool, 'BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=xRealVector,BETA=2.0_SRK,Y=yRealVector)')
      FINFO() '-petscsparse'
      CALL yPETScVector%set(2._SRK)
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=xPETScVector,BETA=2.0_SRK,Y=yPETScVector) !Error check uninit
      CALL yPETScVector%get(y)
      bool = ALL((y .APPROXEQ. (/2._SRK,2._SRK,2._SRK/)))
      ASSERT(bool, 'BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=xPETScVector,BETA=2.0_SRK,Y=yPETScVector)')
      FINFO() '-petscsparse'
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
          bool = (dummyvec(1) == 1._SRK) .AND. &
                 (dummyvec(2) == 0._SRK) .AND. &
                 (dummyvec(3) == 4._SRK)
          ASSERT(bool, 'petscsparse%get(...)')
          bool = (dummyvec(4) == 0._SRK) .AND. &
                 (dummyvec(5) == 0._SRK) .AND. &
                 (dummyvec(6) == 5._SRK)
          ASSERT(bool, 'petscsparse%get(...)')
          FINFO() dummyvec(4:6)
          bool = (dummyvec(7) == 2._SRK) .AND. &
                 (dummyvec(8) == 3._SRK) .AND. &
                 (dummyvec(9) == 6._SRK)
          ASSERT(bool, 'petscsparse%get(...)')
          FINFO() dummyvec(7:9)
      ENDSELECT
      !test with out of bounds i,j, make sure no crash.
      SELECTTYPE(thisMatrix)
        TYPE IS(PETScMatrixType)
          CALL thisMatrix%get(4,2,dummy)
          bool = dummy == -1051._SRK
          ASSERT(bool, 'petscsparse%get(...)')
          CALL thisMatrix%get(-1,2,dummy)
          bool = dummy==-1051._SRK
          ASSERT(bool, 'petscsparse%get(...)')
          CALL thisMatrix%get(2,-1,dummy)
          bool = dummy==-1051._SRK
          ASSERT(bool, 'petscsparse%get(...)')
      ENDSELECT
      !test get with uninit, make sure no crash.
      CALL thisMatrix%clear()
      SELECTTYPE(thisMatrix)
        TYPE IS(PETScMatrixType)
          CALL thisMatrix%get(1,1,dummy)
          bool = dummy == 0.0_SRK
          ASSERT(bool, 'petscsparse%get(...)')
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
          bool = ((.NOT.thisMatrix%isInit).AND.(thisMatrix%n == 0)) &
              .AND.((.NOT.thisMatrix%isSymmetric))
          ASSERT(bool, 'petscdense%clear()')
           !check if pointer fo a is null (not supported till 3.3)
          WRITE(*,*) '  Passed: CALL petscdense%clear()'
      ENDSELECT
      !check init
      CALL pList%clear()
      CALL pList%add('MatrixType->n',10_SNK)
      CALL pList%add('MatrixType->isSym',.FALSE.)
      CALL pList%add('MatrixType->mattype',DENSESQUARE) ! dense
      CALL pList%validate(pList,optListMat)
      CALL thisMatrix%init(pList) !n=10, not symmetric (0), sparse (0)
      SELECTTYPE(thisMatrix)
        TYPE IS(PETScMatrixType)
          bool = ((thisMatrix%isInit).AND.(thisMatrix%n == 10)) &
              .AND.(.NOT.thisMatrix%isSymmetric)
          ASSERT(bool, 'petscdense%init(...)')
          CALL MatGetSize(thisMatrix%a,matsize1,matsize2,ierr)
          bool = (matsize1 == 10) .AND. (matsize2 == 10)
          ASSERT(bool, 'petscdense%init(...)')
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
          bool = ((thisMatrix%isInit).AND.(thisMatrix%n == 10)) &
              .AND.(thisMatrix%isSymmetric)
          ASSERT(bool, 'petscdense%init(...)')
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
          bool = .NOT.thisMatrix%isSymmetric
          ASSERT(bool, 'petscdense%init(...)')
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
          bool = dummy==1._SRK
          ASSERT(bool, 'petscdense%set(...)')
          CALL MatGetValues(thisMatrix%a,1,0,1,1,dummy,ierr)
          bool = dummy==2._SRK
          ASSERT(bool, 'petscdense%set(...)')
          CALL MatGetValues(thisMatrix%a,1,1,1,1,dummy,ierr)
          bool = dummy==3._SRK
          ASSERT(bool, 'petscdense%set(...)')
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
          bool = dummy==1._SRK
          ASSERT(bool, 'petscdense%set(...)')
          CALL MatGetValues(thisMatrix%a,1,0,1,1,dummy,ierr)
          bool = dummy==2._SRK
          ASSERT(bool, 'petscdense%set(...)')
          CALL MatGetValues(thisMatrix%a,1,1,1,0,dummy,ierr)
          bool = dummy==2._SRK
          ASSERT(bool, 'petscdense%set(...)')
          CALL MatGetValues(thisMatrix%a,1,1,1,1,dummy,ierr)
          bool = dummy==3._SRK
          ASSERT(bool, 'petscdense%set(...)')
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
      bool = ALL((y .APPROXEQ. (/4._SRK,8._SRK,16._SRK/)))
      ASSERT(bool, 'BLAS_matvec(THISMATRIX=thisMatrix,X=x,Y=y) -petscdense')
      CALL xRealVector%set(1.0_SRK)
      CALL yRealVector%set(1.0_SRK)
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=xRealVector,Y=yRealVector)
      CALL yRealVector%get(y)
      bool = ALL((y .APPROXEQ. (/4._SRK,8._SRK,16._SRK/)))
      ASSERT(bool, 'BLAS_matvec(THISMATRIX=thisMatrix,X=xRealVector,Y=yRealVector) -petscdense')
      CALL xPETScVector%set(1.0_SRK)
      CALL yPETScVector%set(1.0_SRK)
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=xPETScVector,Y=yPETScVector)
      CALL yPETScVector%get(y)
      bool = ALL((y .APPROXEQ. (/4._SRK,8._SRK,16._SRK/)))
      ASSERT(bool, 'BLAS_matvec(THISMATRIX=thisMatrix,X=PETScVector,Y=yPETScVector) -petscdense')
      y=1.0_SRK
      CALL BLAS_matvec(THISMATRIX=thisMatrix,trans='t',X=x,Y=y)
      bool = ALL((y .APPROXEQ. (/6._SRK,10._SRK,12._SRK/)))
      ASSERT(bool, "BLAS_matvec(THISMATRIX=thisMatrix,trans='t',X=x,Y=y) -petscdense")
      CALL yRealVector%set(1.0_SRK)
      CALL BLAS_matvec(THISMATRIX=thisMatrix,trans='t',X=xRealVector,Y=yRealVector)
      CALL yRealVector%get(y)
      bool = ALL((y .APPROXEQ. (/6._SRK,10._SRK,12._SRK/)))
      ASSERT(bool, 'BLAS_matvec(THISMATRIX=thisMatrix,X=xRealVector,Y=yRealVector) -petscdense')
      CALL yPETScVector%set(1.0_SRK)
      CALL BLAS_matvec(THISMATRIX=thisMatrix,trans='t',X=xPETScVector,Y=yPETScVector)
      CALL yPETScVector%get(y)
      bool = ALL((y .APPROXEQ. (/6._SRK,10._SRK,12._SRK/)))
      ASSERT(bool, 'BLAS_matvec(THISMATRIX=thisMatrix,X=xPETScVector,Y=yPETScVector) -petscdense')
      y=1.0_SRK
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=x,BETA=2.0_SRK,Y=y)
      bool = ALL((y .APPROXEQ. (/5._SRK,9._SRK,17._SRK/)))
      ASSERT(bool, 'BLAS_matvec(THISMATRIX=thisMatrix,X=x,BETA=2.0_SRK,Y=y) -petscdense')
      CALL yRealVector%set(1.0_SRK)
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=xRealVector,BETA=2.0_SRK,Y=yRealVector)
      CALL yRealVector%get(y)
      bool = ALL((y .APPROXEQ. (/5._SRK,9._SRK,17._SRK/)))
      ASSERT(bool, 'BLAS_matvec(THISMATRIX=thisMatrix,X=xRealVector,BETA=2.0_SRK,Y=yRealVector) -petscdense')
      CALL yPETScVector%set(1.0_SRK)
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=xPETScVector,BETA=2.0_SRK,Y=yPETScVector)
      CALL yPETScVector%get(y)
      bool = ALL((y .APPROXEQ. (/5._SRK,9._SRK,17._SRK/)))
      ASSERT(bool, 'BLAS_matvec(THISMATRIX=thisMatrix,X=xPETScVector,BETA=2.0_SRK,Y=yPETScVector) -petscdense')
      y=1.0_SRK
      CALL BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=x,Y=y)
      bool = ALL((y .APPROXEQ. (/7._SRK,15._SRK,31._SRK/)))
      ASSERT(bool, 'BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=x,Y=y) -petscdense')
      CALL yRealVector%set(1.0_SRK)
      CALL BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=xRealVector,Y=yRealVector)
      CALL yRealVector%get(y)
      bool = ALL((y .APPROXEQ. (/7._SRK,15._SRK,31._SRK/)))
      ASSERT(bool, 'BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=xRealVector,Y=yRealVector) -petscdense')
      CALL yPETScVector%set(1.0_SRK)
      CALL BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=xPETScVector,Y=yPETScVector)
      CALL yPETScVector%get(y)
      bool = ALL((y .APPROXEQ. (/7._SRK,15._SRK,31._SRK/)))
      ASSERT(bool, 'BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=xPETScVector,Y=yPETScVector) -petscdense')
      y=1.0_SRK
      CALL BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=x,BETA=2.0_SRK,Y=y)
      bool = ALL((y .APPROXEQ. (/8._SRK,16._SRK,32._SRK/)))
      ASSERT(bool, 'BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=x,BETA=2.0_SRK,Y=y) -petscdense')
      CALL yRealVector%set(1.0_SRK)
      CALL BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=xRealVector,BETA=2.0_SRK,Y=yRealVector)
      CALL yRealVector%get(y)
      bool = ALL((y .APPROXEQ. (/8._SRK,16._SRK,32._SRK/)))
      ASSERT(bool, 'BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=xRealVector,BETA=2.0_SRK,Y=yRealVector)')
      FINFO() '-petscdense'
      CALL yPETScVector%set(1.0_SRK)
      CALL BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=xPETScVector,BETA=2.0_SRK,Y=yPETScVector)
      CALL yPETScVector%get(y)
      bool = ALL((y .APPROXEQ. (/8._SRK,16._SRK,32._SRK/)))
      ASSERT(bool, 'BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=xPETScVector,BETA=2.0_SRK,Y=yPETScVector)')
      FINFO() '-petscdense'
      CALL thisMatrix%clear()
      y=2._SRK
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=x,BETA=2.0_SRK,Y=y) !Error check uninit
      bool = ALL((y .APPROXEQ. (/2._SRK,2._SRK,2._SRK/)))
      ASSERT(bool, 'BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=x,BETA=2.0_SRK,Y=y) -petscdense')
      CALL yRealVector%set(2._SRK)
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=xRealVector,BETA=2.0_SRK,Y=yRealVector) !Error check uninit
      CALL yRealVector%get(y)
      bool = ALL((y .APPROXEQ. (/2._SRK,2._SRK,2._SRK/)))
      ASSERT(bool, 'BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=xRealVector,BETA=2.0_SRK,Y=yRealVector)')
      FINFO() '-petscdense'
      CALL yPETScVector%set(2._SRK)
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=xPETScVector,BETA=2.0_SRK,Y=yPETScVector) !Error check uninit
      CALL yPETScVector%get(y)
      bool = ALL((y .APPROXEQ. (/2._SRK,2._SRK,2._SRK/)))
      ASSERT(bool, 'BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=xPETScVector,BETA=2.0_SRK,Y=yPETScVector)')
      FINFO() '-petscdense'
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
          bool = (dummyvec(1) == 1._SRK) .AND. &
                 (dummyvec(2) == 0._SRK) .AND. &
                 (dummyvec(3) == 4._SRK)
          ASSERT(bool, 'petscdense%get(...)') !column one check
          bool = (dummyvec(4) == 0._SRK) .AND. &
                 (dummyvec(5) == 0._SRK) .AND. &
                 (dummyvec(6) == 5._SRK)
          ASSERT(bool, 'petscdense%get(...)')
          bool = (dummyvec(7) == 2._SRK) .AND. &
                 (dummyvec(8) == 3._SRK) .AND. &
                 (dummyvec(9) == 6._SRK)
          ASSERT(bool, 'petscdense%get(...)')
      ENDSELECT
      !test with out of bounds i,j, make sure no crash.
      SELECTTYPE(thisMatrix)
        TYPE IS(PETScMatrixType)
          CALL thisMatrix%get(4,2,dummy)
          bool = dummy == -1051._SRK
          ASSERT(bool, 'petscdensesquare%get(...)')
          CALL thisMatrix%get(-1,2,dummy)
          bool = dummy == -1051._SRK
          ASSERT(bool, 'petscdensesquare%get(...)')
          CALL thisMatrix%get(2,-1,dummy)
          bool = dummy == -1051._SRK
          ASSERT(bool, 'petscdensesquare%get(...)')
      ENDSELECT
      !test get with uninit, make sure no crash.
      CALL thisMatrix%clear()
      SELECTTYPE(thisMatrix)
        TYPE IS(PETScMatrixType)
          CALL thisMatrix%get(1,1,dummy)
          bool = dummy == 0.0_SRK
          ASSERT(bool, 'petscdensesquare%get(...)')
      ENDSELECT
      CALL thisMatrix%clear()

      WRITE(*,*) '  Passed: CALL petscdensesquare%get(...)'

      DEALLOCATE(thisMatrix)
      CALL xPETScVector%clear()
      CALL yPETScVector%clear()
#endif

!Test for Trilinos matrices (if necessary)
#ifdef FUTILITY_HAVE_Trilinos

!Test for Trilinos sparsematrices
      ALLOCATE(TrilinosMatrixType :: thisMatrix)
      !check init
      CALL pList%clear()
      CALL pList%add('MatrixType->n',10_SNK)
      CALL pList%add('MatrixType->nlocal',10_SNK)
      CALL pList%add('MatrixType->isSym',.FALSE.)
      CALL pList%add('MatrixType->matType',SPARSE)
      ALLOCATE(dnnz(10))
      dnnz=10
      CALL pList%add('MatrixType->dnnz',dnnz)
      dnnz=0
      CALL pList%add('MatrixType->onnz',dnnz)
      CALL pList%validate(pList,optListMat)
      CALL thisMatrix%init(pList) !n=10, not symmetric (0), sparse (0)
      SELECTTYPE(thisMatrix)
        TYPE IS(TrilinosMatrixType)
          bool = ((thisMatrix%isInit).AND.(thisMatrix%n == 10)) &
              .AND.(.NOT.thisMatrix%isSymmetric)
          ASSERT(bool, 'Trilinos%init(...)')
      ENDSELECT
      CALL thisMatrix%clear()
      CALL pList%clear()
!NOTE that symmetric matrices can't be supported since the matrix needs to be loaded row by row
      CALL pList%add('MatrixType->n',10_SNK)
      CALL pList%add('MatrixType->nlocal',10_SNK)
      CALL pList%add('MatrixType->isSym',.TRUE.)
      CALL pList%add('MatrixType->matType',SPARSE)
      dnnz=10
      CALL pList%add('MatrixType->dnnz',dnnz)
      dnnz=0
      CALL pList%add('MatrixType->onnz',dnnz)
      CALL pList%validate(pList,optListMat)
      CALL thisMatrix%init(pList) !n=10, symmetric (1), sparse (0)
      SELECTTYPE(thisMatrix)
        TYPE IS(TrilinosMatrixType)
          bool = .NOT. thisMatrix%isInit
          ASSERT(bool, 'Trilinossparse%init(...)')
      ENDSELECT
      CALL thisMatrix%clear()
      !test with n<1
      CALL pList%clear()
      CALL pList%add('MatrixType->n',-1_SNK)
      CALL pList%add('MatrixType->isSym',.TRUE.)
      CALL pList%add('MatrixType->matType',SPARSE)
      CALL pList%validate(pList,optListMat)
      CALL thisMatrix%init(pList) !expect exception
      bool = .NOT.thisMatrix%isInit
      ASSERT(bool, 'Trilinossparse%init(...)')
      CALL thisMatrix%clear()
      WRITE(*,*) '  Passed: CALL Trilinossparse%init(...)'

      !check set
      !test normal use case (unsymmetric and nonsymmetric)
      !want to build:
      ![1 2]
      ![0 3]
      CALL pList%clear()
      CALL pList%add('MatrixType->n',2_SNK)
      CALL pList%add('MatrixType->nlocal',2_SNK)
      CALL pList%add('MatrixType->isSym',.FALSE.)
      CALL pList%add('MatrixType->matType',SPARSE)
      dnnz=2
      CALL pList%add('MatrixType->dnnz',dnnz)
      dnnz=0
      CALL pList%add('MatrixType->onnz',dnnz)
      CALL pList%validate(pList,optListMat)
      CALL thisMatrix%init(pList)
      CALL thisMatrix%set(1,1,1._SRK)
      CALL thisMatrix%set(1,2,2._SRK)
      CALL thisMatrix%set(2,2,3._SRK)
      SELECTTYPE(thisMatrix)
        TYPE IS(TrilinosMatrixType)
          CALL thisMatrix%assemble()
          CALL thisMatrix%get(1,1,dummy)
          bool = dummy==1._SRK
          ASSERT(bool, 'Trilinossparse%set(...)')
          CALL thisMatrix%get(1,2,dummy)
          bool = dummy==2._SRK
          ASSERT(bool, 'Trilinossparse%set(...)')
          CALL thisMatrix%get(2,2,dummy)
          bool = dummy==3._SRK
          ASSERT(bool, 'Trilinossparse%set(...)')
      ENDSELECT
      CALL thisMatrix%clear()
      CALL pList%clear()
      CALL pList%add('MatrixType->n',2_SNK)
      CALL pList%add('MatrixType->nlocal',2_SNK)
      CALL pList%add('MatrixType->isSym',.FALSE.)
      CALL pList%add('MatrixType->mattype',SPARSE)
      dnnz=2
      CALL pList%add('MatrixType->dnnz',dnnz)
      dnnz=0
      CALL pList%add('MatrixType->onnz',dnnz)
      CALL pList%validate(pList,optListMat)
      CALL thisMatrix%init(pList)  ! nonsymmetric (0), sparse (0)
      CALL thisMatrix%set(1,1,1._SRK)
      CALL thisMatrix%set(1,2,2._SRK)
      CALL thisMatrix%set(2,1,2._SRK)
      CALL thisMatrix%set(2,2,3._SRK)
      SELECTTYPE(thisMatrix)
        TYPE IS(TrilinosMatrixType)
          ! manually assemble
          CALL thisMatrix%assemble()

          CALL thisMatrix%get(1,1,dummy)
          bool = dummy==1._SRK
          ASSERT(bool, 'Trilinossparse%set(...)')
          CALL thisMatrix%get(1,2,dummy)
          bool = dummy==2._SRK
          ASSERT(bool, 'Trilinossparse%set(...)')
          CALL thisMatrix%get(2,1,dummy)
          bool = dummy==2._SRK
          ASSERT(bool, 'Trilinossparse%set(...)')
          CALL thisMatrix%get(2,2,dummy)
          bool = dummy==3._SRK
          ASSERT(bool, 'Trilinossparse%set(...)')
      ENDSELECT

      CALL thisMatrix%clear()
      CALL thisMatrix%init(pList)  ! nonsymmetric (0), sparse (0)

      SELECT TYPE (thisMatrix)
      CLASS IS (DistributedMatrixType)
        ! Overwrite with setRow
        CALL thisMatrix%setRow(1,[1, 2],[10._SRK, 11._SRK])
        CALL thisMatrix%setRow(2,[1, 2],[12._SRK, 13._SRK])
        CALL thisMatrix%assemble()
        CALL thisMatrix%get(1, 1, val)
        ASSERT(val .APPROXEQ. 10._SRK, "TrilinosMatrixType::setRow")
        CALL thisMatrix%get(1, 2, val)
        ASSERT(val .APPROXEQ. 11._SRK, "TrilinosMatrixType::setRow")
        CALL thisMatrix%get(2, 1, val)
        ASSERT(val .APPROXEQ. 12._SRK, "TrilinosMatrixType::setRow")
        CALL thisMatrix%get(2, 2, val)
        ASSERT(val .APPROXEQ. 13._SRK, "TrilinosMatrixType::setRow")
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
      WRITE(*,*) '  Passed: CALL Trilinossparse%set(...)'

      CALL thisMatrix%clear()
      CALL pList%clear()
      CALL pList%add('MatrixType->n',3_SNK)
      CALL pList%add('MatrixType->nlocal',3_SNK)
      CALL pList%add('MatrixType->isSym',.FALSE.)
      CALL pList%add('MatrixType->mattype',SPARSE)
      dnnz=3
      CALL pList%add('MatrixType->dnnz',dnnz)
      dnnz=0
      CALL pList%add('MatrixType->onnz',dnnz)

      CALL pList%validate(pList,optListMat)
      CALL thisMatrix%init(pList)  !symmetric
      CALL thisMatrix%set(1,1,1._SRK)
      CALL thisMatrix%set(1,3,2._SRK)
      CALL thisMatrix%set(2,2,4._SRK)
      CALL thisMatrix%set(2,3,3._SRK)
      CALL thisMatrix%set(3,1,4._SRK)
      CALL thisMatrix%set(3,2,5._SRK)
      CALL thisMatrix%set(3,3,6._SRK)
!TODO: these interfaces are not implemented
!      x=1.0_SRK
!      y=1.0_SRK
!      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=x,Y=y)
!      bool = ALL((y .APPROXEQ. (/4._SRK,8._SRK,16._SRK/)))
!      ASSERT(bool, 'BLAS_matvec(THISMATRIX=thisMatrix,X=x,Y=y) -Trilsparse')
!      CALL xRealVector%set(1.0_SRK)
!      CALL yRealVector%set(1.0_SRK)
!      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=xRealVector,Y=yRealVector)
!      CALL yRealVector%get(y)
!      bool = ALL((y .APPROXEQ. (/4._SRK,8._SRK,16._SRK/)))
!      ASSERT(bool, 'BLAS_matvec(THISMATRIX=thisMatrix,X=xRealVector,Y=yRealVector) -Trilsparse')
      CALL xTrilinosVector%set(1.0_SRK)
      CALL yTrilinosVector%set(1.0_SRK)
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=xTrilinosVector,Y=yTrilinosVector)
      CALL yTrilinosVector%get(1,y(1))
      CALL yTrilinosVector%get(2,y(2))
      CALL yTrilinosVector%get(3,y(3))
      bool = ALL((y .APPROXEQ. (/4._SRK,8._SRK,16._SRK/)))
      ASSERT(bool, 'BLAS_matvec(THISMATRIX=thisMatrix,X=TrilVector,Y=yTrilVector) -Trilsparse')
!TODO: these interfaces are not implemented
!      y=1.0_SRK
!      CALL BLAS_matvec(THISMATRIX=thisMatrix,trans='t',X=x,Y=y)
!      bool = ALL((y .APPROXEQ. (/6._SRK,10._SRK,12._SRK/)))
!      ASSERT(bool, "BLAS_matvec(THISMATRIX=thisMatrix,trans='t',X=x,Y=y) -Trilsparse")
!      CALL yRealVector%set(1.0_SRK)
!      CALL BLAS_matvec(THISMATRIX=thisMatrix,trans='t',X=xRealVector,Y=yRealVector)
!      CALL yRealVector%get(y)
!      bool = ALL((y .APPROXEQ. (/6._SRK,10._SRK,12._SRK/)))
!      ASSERT(bool, 'BLAS_matvec(THISMATRIX=thisMatrix,X=xRealVector,Y=yRealVector) -Trilsparse')
      CALL yTrilinosVector%set(1.0_SRK)
      CALL BLAS_matvec(THISMATRIX=thisMatrix,trans='t',X=xTrilinosVector,Y=yTrilinosVector)
      CALL yTrilinosVector%get(1,y(1))
      CALL yTrilinosVector%get(2,y(2))
      CALL yTrilinosVector%get(3,y(3))
      bool = ALL((y .APPROXEQ. (/6._SRK,10._SRK,12._SRK/)))
      ASSERT(bool, "BLAS_matvec(THISMATRIX=thisMatrix,trans='t',X=xTrilVector,Y=yTrilVector) -Trilsparse")
!TODO: these interfaces are not implemented
!      y=1.0_SRK
!      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=x,BETA=2.0_SRK,Y=y)
!      bool = ALL((y .APPROXEQ. (/5._SRK,9._SRK,17._SRK/)))
!      ASSERT(bool, 'BLAS_matvec(THISMATRIX=thisMatrix,X=x,BETA=2.0_SRK,Y=y) -Trilsparse')
!      CALL yRealVector%set(1.0_SRK)
!      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=xRealVector,BETA=2.0_SRK,Y=yRealVector)
!      CALL yRealVector%get(y)
!      bool = ALL((y .APPROXEQ. (/5._SRK,9._SRK,17._SRK/)))
!      ASSERT(bool, 'BLAS_matvec(THISMATRIX=thisMatrix,X=xRealVector,BETA=2.0_SRK,Y=yRealVector) -Trilsparse')
      CALL yTrilinosVector%set(1.0_SRK)
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=xTrilinosVector,BETA=2.0_SRK,Y=yTrilinosVector)
      CALL yTrilinosVector%get(1,y(1))
      CALL yTrilinosVector%get(2,y(2))
      CALL yTrilinosVector%get(3,y(3))
      bool = ALL((y .APPROXEQ. (/5._SRK,9._SRK,17._SRK/)))
      ASSERT(bool, 'BLAS_matvec(THISMATRIX=thisMatrix,X=xTrilVector,BETA=2.0_SRK,Y=yTrilVector) -Trilsparse')
!TODO: these interfaces are not implemented
!      y=1.0_SRK
!      CALL BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=x,Y=y)
!      bool = ALL((y .APPROXEQ. (/7._SRK,15._SRK,31._SRK/)))
!      ASSERT(bool, 'BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=x,Y=y) -Trilsparse')
!      CALL yRealVector%set(1.0_SRK)
!      CALL BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=xRealVector,Y=yRealVector)
!      CALL yRealVector%get(y)
!      bool = ALL((y .APPROXEQ. (/7._SRK,15._SRK,31._SRK/)))
!      ASSERT(bool, 'BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=xRealVector,Y=yRealVector) -Trilsparse')
      CALL yTrilinosVector%set(1.0_SRK)
      CALL BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=xTrilinosVector,Y=yTrilinosVector)
      CALL yTrilinosVector%get(1,y(1))
      CALL yTrilinosVector%get(2,y(2))
      CALL yTrilinosVector%get(3,y(3))
      bool = ALL((y .APPROXEQ. (/7._SRK,15._SRK,31._SRK/)))
      ASSERT(bool, 'BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=xTrilVector,Y=yTrilVector) -Trilsparse')
!TODO: these interfaces are not implemented
!      y=1.0_SRK
!      CALL BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=x,BETA=2.0_SRK,Y=y)
!      bool = ALL((y .APPROXEQ. (/8._SRK,16._SRK,32._SRK/)))
!      ASSERT(bool, 'BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=x,BETA=2.0_SRK,Y=y) -Trilsparse')
!      CALL yRealVector%set(1.0_SRK)
!      CALL BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=xRealVector,BETA=2.0_SRK,Y=yRealVector)
!      CALL yRealVector%get(y)
!      bool = ALL((y .APPROXEQ. (/8._SRK,16._SRK,32._SRK/)))
!      ASSERT(bool, 'BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=xRealVector,BETA=2.0_SRK,Y=yRealVector)')
!      FINFO() '-Trilsparse'
      CALL yTrilinosVector%set(1.0_SRK)
      CALL BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=xTrilinosVector,BETA=2.0_SRK,Y=yTrilinosVector)
      CALL yTrilinosVector%get(1,y(1))
      CALL yTrilinosVector%get(2,y(2))
      CALL yTrilinosVector%get(3,y(3))
      bool = ALL((y .APPROXEQ. (/8._SRK,16._SRK,32._SRK/)))
      ASSERT(bool, 'BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=xTrilVector,BETA=2.0_SRK,Y=yTrilVector)')
      FINFO() '-Trilsparse'
      CALL thisMatrix%clear()
!TODO: these interfaces are not implemented
!      y=2._SRK
!      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=x,BETA=2.0_SRK,Y=y) !Error check uninit
!      bool = ALL((y .APPROXEQ. (/2._SRK,2._SRK,2._SRK/)))
!      ASSERT(bool, 'BLAS_matvec(THISMATRIX=thisMatrix,X=x,BETA=2.0_SRK,Y=y) -Trilsparse')
!      CALL yRealVector%set(2._SRK)
!      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=xRealVector,BETA=2.0_SRK,Y=yRealVector) !Error check uninit
!      CALL yRealVector%get(y)
!      bool = ALL((y .APPROXEQ. (/2._SRK,2._SRK,2._SRK/)))
!      ASSERT(bool, 'BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=xRealVector,BETA=2.0_SRK,Y=yRealVector)')
!      FINFO() '-Trilsparse'
      CALL yTrilinosVector%set(2._SRK)
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=xTrilinosVector,BETA=2.0_SRK,Y=yTrilinosVector) !Error check uninit
      CALL yTrilinosVector%get(1,y(1))
      CALL yTrilinosVector%get(2,y(2))
      CALL yTrilinosVector%get(3,y(3))
      bool = ALL((y .APPROXEQ. (/2._SRK,2._SRK,2._SRK/)))
      ASSERT(bool, 'BLAS_matvec(THISMATRIX=thisMatrix,ALPHA=2.0_SRK,X=xTrilVector,BETA=2.0_SRK,Y=yTrilVector)')
      FINFO() '-Trilsparse'
      WRITE(*,*) '  Passed: CALL BLAS_matvec(...) Trilinossparse-matrix'

      !Perform test of functionality of get function
      ![1 0 2]
      ![0 0 3]
      ![4 5 6]
      CALL thisMatrix%clear()
      CALL pList%clear()
      CALL pList%add('MatrixType->n',3_SNK)
      CALL pList%add('MatrixType->nlocal',3_SNK)
      CALL pList%add('MatrixType->isSym',.FALSE.)
      CALL pList%add('MatrixType->mattype',SPARSE)
      dnnz=3
      CALL pList%add('MatrixType->dnnz',dnnz)
      dnnz=0
      CALL pList%add('MatrixType->onnz',dnnz)

      CALL pList%validate(pList,optListMat)
      CALL thisMatrix%init(pList) ! non-symmetric (0), sparse (0)
      SELECTTYPE(thisMatrix)
        TYPE IS(TrilinosMatrixType)
          CALL thisMatrix%set(1,1,1._SRK)
          CALL thisMatrix%set(1,3,2._SRK)
          CALL thisMatrix%set(2,3,3._SRK)
          CALL thisMatrix%set(3,1,4._SRK)
          CALL thisMatrix%set(3,2,5._SRK)
          CALL thisMatrix%set(3,3,6._SRK)
          CALL thisMatrix%assemble()
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
          bool = (dummyvec(1) == 1._SRK) .AND. &
                 (dummyvec(2) == 0._SRK) .AND. &
                 (dummyvec(3) == 4._SRK)
          ASSERT(bool, 'Trilinossparse%get(...)')
          bool = (dummyvec(4) == 0._SRK) .AND. &
                 (dummyvec(5) == 0._SRK) .AND. &
                 (dummyvec(6) == 5._SRK)
          ASSERT(bool, 'Trilinossparse%get(...)')
          FINFO() dummyvec(4:6)
          bool = (dummyvec(7) == 2._SRK) .AND. &
                 (dummyvec(8) == 3._SRK) .AND. &
                 (dummyvec(9) == 6._SRK)
          ASSERT(bool, 'Trilinossparse%get(...)')
          FINFO() dummyvec(7:9)
      ENDSELECT
      !test with out of bounds i,j, make sure no crash.
      SELECTTYPE(thisMatrix)
        TYPE IS(TrilinosMatrixType)
          CALL thisMatrix%get(4,2,dummy)
          bool = dummy == -1051._SRK
          ASSERT(bool, 'Trilinossparse%get(...)')
          CALL thisMatrix%get(-1,2,dummy)
          bool = dummy==-1051._SRK
          ASSERT(bool, 'Trilinossparse%get(...)')
          CALL thisMatrix%get(2,-1,dummy)
          bool = dummy==-1051._SRK
          ASSERT(bool, 'Trilinossparse%get(...)')
      ENDSELECT
      !test get with uninit, make sure no crash.
      CALL thisMatrix%clear()
      SELECTTYPE(thisMatrix)
        TYPE IS(TrilinosMatrixType)
          CALL thisMatrix%get(1,1,dummy)
          bool = dummy == 0.0_SRK
          ASSERT(bool, 'Trilinossparse%get(...)')
      ENDSELECT
      CALL thisMatrix%clear()
      WRITE(*,*) '  Passed: CALL Trilinossparse%get(...)'
      DEALLOCATE(thisMatrix)

      CALL xTrilinosVector%clear()
      CALL yTrilinosVector%clear()
#endif

      CALL xRealVector%clear()
      CALL yRealVector%clear()
    ENDSUBROUTINE testMatrix
!
!-------------------------------------------------------------------------------
    SUBROUTINE testMatrixMultSparse()
#ifdef FUTILITY_HAVE_PETSC
      CLASS(MatrixType),ALLOCATABLE :: thisMtrx
      CLASS(MatrixType),ALLOCATABLE :: bmat
      CLASS(MatrixType),ALLOCATABLE :: cmat
      INTEGER(SIK) :: i,j
      REAL(SRK) :: ALPHA,BETA,dummy
      LOGICAL(SBK) :: bool

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
              bool = (dummy .APPROXEQ. 4._SRK)
              ASSERT(bool, 'BLAS_matmult(A=thisMtrx,B=bmat,C=cmat,alpha=ALPHA,beta=BETA) -All PETSc')
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
              bool = (dummy .APPROXEQ. 5._SRK)
              ASSERT(bool, 'BLAS_matmult(A=thisMtrx,B=bmat,C=cmat,alpha=ALPHA) -All PETSc')
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
              bool = (dummy .APPROXEQ. 6._SRK)
              ASSERT(bool, 'BLAS_matmult(A=thisMtrx,B=bmat,C=cmat,beta=BETA) -All PETSc')
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
              bool = (dummy .APPROXEQ. 7._SRK)
              ASSERT(bool, 'BLAS_matmult(A=thisMtrx,B=bmat,C=cmat) -All PETSc')
            ENDDO
          ENDDO
        CLASS DEFAULT
          WRITE(*,*) "CALL BLAS_matmult(A=thisMtrx,B=bmat,C=cmat) FAILED!"
          WRITE(*,*) "    cmat returned as incorrect matrix type"
          STOP 666
      ENDSELECT
      CALL thisMtrx%clear()
      CALL bmat%clear()
      CALL cmat%clear()
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
      REAL(SRK) :: ALPHA,BETA
      LOGICAL(SBK) :: bool
#ifdef FUTILITY_HAVE_PETSC
     REAL(SRK) :: dummy
#endif

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
              bool = (cmat%a(i,j) .APPROXEQ. 4._SRK)
              ASSERT(bool, 'BLAS_matmult(A=thisMtrx,B=bmat,C=cmat,alpha=ALPHA,beta=BETA) -DS DS DS')
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
              bool = (cmat%a(i,j) .APPROXEQ. 5._SRK)
              ASSERT(bool, 'BLAS_matmult(A=thisMtrx,B=bmat,C=cmat,alpha=ALPHA) -DS DS DS')
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
              bool = (cmat%a(i,j) .APPROXEQ. 6._SRK)
              ASSERT(bool, 'BLAS_matmult(A=thisMtrx,B=bmat,C=cmat,beta=BETA) -DS DS DS')
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
              bool = (cmat%a(i,j) .APPROXEQ. 7._SRK)
              ASSERT(bool, 'BLAS_matmult(A=thisMtrx,B=bmat,C=cmat) -DS DS DS')
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
              bool = (cmat%a(i,j) .APPROXEQ. 4._SRK)
              ASSERT(bool, 'BLAS_matmult(A=thisMtrx,B=bmat,C=cmat,alpha=ALPHA,beta=BETA) -DS DS DR')
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
              bool = (cmat%a(i,j) .APPROXEQ. 5._SRK)
              ASSERT(bool, 'BLAS_matmult(A=thisMtrx,B=bmat,C=cmat,alpha=ALPHA) -DS DS DR')
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
              bool = (cmat%a(i,j) .APPROXEQ. 6._SRK)
              ASSERT(bool, 'BLAS_matmult(A=thisMtrx,B=bmat,C=cmat,beta=BETA) -DS DS DR')
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
              bool = (cmat%a(i,j) .APPROXEQ. 7._SRK)
              ASSERT(bool, 'BLAS_matmult(A=thisMtrx,B=bmat,C=cmat) -DS DS DR')
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
              bool = (cmat%a(i,j) .APPROXEQ. 4._SRK)
              ASSERT(bool, 'BLAS_matmult(A=thisMtrx,B=bmat,C=cmat,alpha=ALPHA,beta=BETA) -DS DR DS')
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
              bool = (cmat%a(i,j) .APPROXEQ. 5._SRK)
              ASSERT(bool, 'BLAS_matmult(A=thisMtrx,B=bmat,C=cmat,alpha=ALPHA) -DS DR DS')
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
              bool = (cmat%a(i,j) .APPROXEQ. 6._SRK)
              ASSERT(bool, 'BLAS_matmult(A=thisMtrx,B=bmat,C=cmat,beta=BETA) -DS DR DS')
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
              bool = (cmat%a(i,j) .APPROXEQ. 7._SRK)
              ASSERT(bool, 'BLAS_matmult(A=thisMtrx,B=bmat,C=cmat) -DS DR DS')
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
              bool = (cmat%a(i,j) .APPROXEQ. 4._SRK)
              ASSERT(bool, 'BLAS_matmult(A=thisMtrx,B=bmat,C=cmat,alpha=ALPHA,beta=BETA) -DS DR DR')
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
              bool = (cmat%a(i,j) .APPROXEQ. 5._SRK)
              ASSERT(bool, 'BLAS_matmult(A=thisMtrx,B=bmat,C=cmat,alpha=ALPHA) -DS DR DR')
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
              bool = (cmat%a(i,j) .APPROXEQ. 6._SRK)
              ASSERT(bool, 'BLAS_matmult(A=thisMtrx,B=bmat,C=cmat,beta=BETA) -DS DR DR')
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
              bool = (cmat%a(i,j) .APPROXEQ. 7._SRK)
              ASSERT(bool, 'BLAS_matmult(A=thisMtrx,B=bmat,C=cmat) -DS DR DR')
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

#ifdef FUTILITY_HAVE_PETSC
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
              bool = (dummy .APPROXEQ. 4._SRK)
              ASSERT(bool, 'BLAS_matmult(A=thisMtrx,B=bmat,C=cmat,alpha=ALPHA,beta=BETA) -All PETSc')
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
              bool = (dummy .APPROXEQ. 5._SRK)
              ASSERT(bool, 'BLAS_matmult(A=thisMtrx,B=bmat,C=cmat,alpha=ALPHA) -All PETSc')
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
              bool = (dummy .APPROXEQ. 6._SRK)
              ASSERT(bool, 'BLAS_matmult(A=thisMtrx,B=bmat,C=cmat,beta=BETA) -All PETSc')
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
              bool = (dummy .APPROXEQ. 7._SRK)
              ASSERT(bool, 'BLAS_matmult(A=thisMtrx,B=bmat,C=cmat) -All PETSc')
            ENDDO
          ENDDO
        CLASS DEFAULT
          WRITE(*,*) "CALL BLAS_matmult(A=thisMtrx,B=bmat,C=cmat) FAILED!"
          WRITE(*,*) "    cmat returned as incorrect matrix type"
          STOP 666
      ENDSELECT
      CALL thisMtrx%clear()
      CALL bmat%clear()
      CALL cmat%clear()
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
      LOGICAL(SBK) :: bool

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
              bool = (cmat%a(i,j) .APPROXEQ. 4._SRK)
              ASSERT(bool, 'BLAS_matmult(A=thisMtrx,B=bmat,C=cmat,alpha=ALPHA,beta=BETA) -DR DS DS')
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
              bool = (cmat%a(i,j) .APPROXEQ. 5._SRK)
              ASSERT(bool, 'BLAS_matmult(A=thisMtrx,B=bmat,C=cmat,alpha=ALPHA) -DR DS DS')
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
              bool = (cmat%a(i,j) .APPROXEQ. 6._SRK)
              ASSERT(bool, 'BLAS_matmult(A=thisMtrx,B=bmat,C=cmat,beta=BETA) -DR DS DS')
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
              bool = (cmat%a(i,j) .APPROXEQ. 7._SRK)
              ASSERT(bool, 'BLAS_matmult(A=thisMtrx,B=bmat,C=cmat) -DR DS DS')
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
              bool = (cmat%a(i,j) .APPROXEQ. 4._SRK)
              ASSERT(bool, 'BLAS_matmult(A=thisMtrx,B=bmat,C=cmat,alpha=ALPHA,beta=BETA) -DR DS DR')
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
              bool = (cmat%a(i,j) .APPROXEQ. 5._SRK)
              ASSERT(bool, 'BLAS_matmult(A=thisMtrx,B=bmat,C=cmat,alpha=ALPHA) -DR DS DR')
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
              bool = (cmat%a(i,j) .APPROXEQ. 6._SRK)
              ASSERT(bool, 'BLAS_matmult(A=thisMtrx,B=bmat,C=cmat,beta=BETA) -DR DS DR')
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
              bool = (cmat%a(i,j) .APPROXEQ. 7._SRK)
              ASSERT(bool, 'BLAS_matmult(A=thisMtrx,B=bmat,C=cmat) -DR DS DR')
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
              bool = (cmat%a(i,j) .APPROXEQ. 4._SRK)
              ASSERT(bool, 'BLAS_matmult(A=thisMtrx,B=bmat,C=cmat,alpha=ALPHA,beta=BETA) -DR DR DS')
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
              bool = (cmat%a(i,j) .APPROXEQ. 5._SRK)
              ASSERT(bool, 'BLAS_matmult(A=thisMtrx,B=bmat,C=cmat,alpha=ALPHA) -DR DR DS')
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
              bool = (cmat%a(i,j) .APPROXEQ. 6._SRK)
              ASSERT(bool, 'BLAS_matmult(A=thisMtrx,B=bmat,C=cmat,beta=BETA) -DR DR DS')
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
              ASSERT(bool, 'BLAS_matmult(A=thisMtrx,B=bmat,C=cmat) -DR DR DS')
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
              bool = (cmat%a(i,j) .APPROXEQ. 4._SRK)
              ASSERT(bool, 'BLAS_matmult(A=thisMtrx,B=bmat,C=cmat,alpha=ALPHA,beta=BETA) -DR DR DR')
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
              bool = (cmat%a(i,j) .APPROXEQ. 5._SRK)
              ASSERT(bool, 'BLAS_matmult(A=thisMtrx,B=bmat,C=cmat,alpha=ALPHA) -DR DR DR')
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
              bool = (cmat%a(i,j) .APPROXEQ. 6._SRK)
              ASSERT(bool, 'BLAS_matmult(A=thisMtrx,B=bmat,C=cmat,beta=BETA) -DR DR DR')
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
              bool = (cmat%a(i,j) .APPROXEQ. 7._SRK)
              ASSERT(bool, 'BLAS_matmult(A=thisMtrx,B=bmat,C=cmat) -DR DR DR')
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
    SUBROUTINE testTransposeMatrix()
      CLASS(SparseMatrixType),ALLOCATABLE :: testA
      CLASS(MatrixType),POINTER :: mat_p
      LOGICAL(SBK) :: bool
      TYPE(ParamType) :: tmpPlist
      REAL(SRK) :: dummy
#ifdef FUTILITY_HAVE_PETSC
      CLASS(DistributedMatrixType),POINTER :: dmat_p
      REAL(SRK) :: aij
#endif
      ALLOCATE(SparseMatrixType :: testA)


      COMPONENT_TEST('Sparse')
      CALL Plist%clear()
      CALL tmpPlist%add('MatrixType->n',4_SIK)
      CALL tmpPlist%add('MatrixType->matType',SPARSE)
      CALL tmpPlist%add('MatrixType->nnz',6_SIK)

      CALL testA%init(tmpPlist)
      CALL testA%setShape(1,2,1.0_SRK)
      CALL testA%setShape(2,1,2.0_SRK)
      CALL testA%setShape(2,4,3.0_SRK)
      CALL testA%setShape(3,4,4.0_SRK)
      CALL testA%setShape(4,1,5.0_SRK)
      CALL testA%setShape(4,4,6.0_SRK)

      CALL testA%transpose()
      bool=ALL(testA%ia(:) == (/1,3,4,4,7/))
      ASSERT(bool,"wrong ia")
      bool=ALL(testA%ja(:) == (/2,4,1,2,3,4/))
      ASSERT(bool,"wrong ja")
      bool=ALL(testA%a(:) .APPROXEQ. (/2.0_SRK,5.0_SRK,1.0_SRK,3.0_SRK,4.0_SRK,6.0_SRK/))
      ASSERT(bool,"wrong a")
      CALL tmpPlist%clear()

      ! Banded Matrix
      !want to build:
      ![1 2 0 0 0]
      ![0 3 4 0 0]
      ![8 0 5 6 0]
      ![0 9 0 7 0]
      !transpose to
      ![1 0 8 0]
      ![2 3 0 9]
      ![0 4 5 0]
      ![0 0 6 7]
      ![0 0 0 0]
      ALLOCATE(BandedMatrixType :: mat_p)

      COMPONENT_TEST('Banded')
      CALL tmpPlist%add('MatrixType->n',4)
      CALL tmpPlist%add('MatrixType->m',5)
      CALL tmpPlist%add('MatrixType->nnz',9)
      CALL mat_p%init(tmpPlist)
      CALL mat_p%set(1,1,1._SRK)
      CALL mat_p%set(1,2,2._SRK)
      CALL mat_p%set(2,2,3._SRK)
      CALL mat_p%set(2,3,4._SRK)
      CALL mat_p%set(3,3,5._SRK)
      CALL mat_p%set(3,4,6._SRK)
      CALL mat_p%set(4,4,7._SRK)
      CALL mat_p%set(3,1,8._SRK)
      CALL mat_p%set(4,2,9._SRK)

      SELECTTYPE(mat_p)
        TYPE IS(BandedMatrixType)
          CALL mat_p%assemble()
          CALL mat_p%transpose()
          bool = (mat_p%n == 5).AND.(mat_p%m == 4)
          ASSERT(bool,"banded%transpose()")
          bool = .TRUE.
          CALL mat_p%get(1,1,dummy)
          bool = bool .AND. dummy == 1.0_SRK
          CALL mat_p%get(1,3,dummy)
          bool = bool .AND. dummy == 8.0_SRK
          CALL mat_p%get(2,1,dummy)
          bool = bool .AND. dummy == 2.0_SRK
          CALL mat_p%get(2,2,dummy)
          bool = bool .AND. dummy == 3.0_SRK
          CALL mat_p%get(2,4,dummy)
          bool = bool .AND. dummy == 9.0_SRK
          CALL mat_p%get(3,2,dummy)
          bool = bool .AND. dummy == 4.0_SRK
          CALL mat_p%get(3,3,dummy)
          bool = bool .AND. dummy == 5.0_SRK
          CALL mat_p%get(4,3,dummy)
          bool = bool .AND. dummy == 6.0_SRK
          CALL mat_p%get(4,4,dummy)
          bool = bool .AND. dummy == 7.0_SRK
          CALL mat_p%get(1,2,dummy)
          bool = bool .AND. dummy == 0.0_SRK
          ASSERT(bool,"banded%transpose()")
      ENDSELECT
      CALL mat_p%clear()
      DEALLOCATE(mat_p)
      NULLIFY(mat_p)
      CALL tmpPlist%clear()


#ifdef FUTILITY_HAVE_PETSC
      COMPONENT_TEST('PETSc')
      dmat_p => NULL()
      CALL tmpPlist%add("MatrixType->n",4)
      CALL tmpPlist%add("MatrixType->isSym",.FALSE.)
      CALL tmpPlist%add("MatrixType->matType",SPARSE)
      CALL tmpPlist%add("MatrixType->engine",VM_PETSC)
      CALL tmpPlist%add("MatrixType->MPI_COMM_ID",PE_COMM_SELF)
      CALL DistributedMatrixFactory(dmat_p,tmpPlist)

      CALL dmat_p%set(1,2,1.0_SRK)
      CALL dmat_p%set(2,1,2.0_SRK)
      CALL dmat_p%set(2,4,3.0_SRK)
      CALL dmat_p%set(3,4,4.0_SRK)
      CALL dmat_p%set(4,1,5.0_SRK)
      CALL dmat_p%set(4,4,6.0_SRK)

      CALL dmat_p%assemble()

      CALL dmat_p%transpose()

      CALL dmat_p%get(2,1,aij)
      ASSERT(aij == 1.0_SRK,'(1,2)->(2,1)')
      CALL dmat_p%get(1,2,aij)
      ASSERT(aij == 2.0_SRK,'(2,1)->(1,2)')
      CALL dmat_p%get(4,2,aij)
      ASSERT(aij == 3.0_SRK,'(2,4)->(4,2)')
      CALL dmat_p%get(4,3,aij)
      ASSERT(aij == 4.0_SRK,'(3,4)->(4,3)')
      CALL dmat_p%get(1,4,aij)
      ASSERT(aij == 5.0_SRK,'(4,1)->(1,4)')
      CALL dmat_p%get(4,4,aij)
      ASSERT(aij == 6.0_SRK,'(4,4)->(4,4)')

      CALL dmat_p%clear()
      DEALLOCATE(dmat_p)
      CALL tmpPlist%clear()
#endif
    ENDSUBROUTINE testTransposeMatrix
!
!-------------------------------------------------------------------------------
    SUBROUTINE testzeroEntriesMatrix()
      CLASS(MatrixType),POINTER :: mat_p
      CLASS(SparseMatrixType),ALLOCATABLE :: testA
      LOGICAL(SBK) :: bool
      TYPE(ParamType) :: tmpPlist,params
      INTEGER(SIK) :: i,j
#ifdef FUTILITY_HAVE_PETSC
      CLASS(DistributedMatrixType),POINTER :: dmat_p
      REAL(SRK) :: aij
#endif
      ALLOCATE(SparseMatrixType :: testA)

      COMPONENT_TEST("Matrix ZeroEntries")
      ALLOCATE(DenseSquareMatrixType :: mat_p)
      ! Dense Square matrix
      CALL params%add("MatrixType->n",4)
      CALL params%add("MatrixType->isSym",.FALSE.)
      CALL params%add("MatrixType->matType",DENSESQUARE)
      CALL params%add("MatrixType->engine",VM_NATIVE)
      CALL mat_p%init(params)
      CALL mat_p%zeroentries()
      SELECTTYPE(mat_p)
        TYPE IS(DenseSquareMatrixType)
          DO i=1,mat_p%n
            DO j=1,mat_p%n
              bool=(mat_p%a(i,j) .APPROXEQ. 0.0_SRK)
              ASSERT(bool,"wrong zeroentries")
            ENDDO
          ENDDO
      ENDSELECT
      CALL mat_p%clear()
      DEALLOCATE(mat_p)
      NULLIFY(mat_p)
      CALL params%clear()
      ALLOCATE(DenseRectMatrixType :: mat_p)
      ! Dense Square matrix
      CALL params%add("MatrixType->n",4)
      CALL params%add("MatrixType->m",4)
      CALL params%add("MatrixType->isSym",.FALSE.)
      CALL params%add("MatrixType->matType",DENSERECT)
      CALL params%add("MatrixType->engine",VM_NATIVE)
      CALL mat_p%init(params)
      CALL mat_p%zeroentries()
      SELECTTYPE(mat_p)
        TYPE IS(DenseRectMatrixType)
          DO i=1,mat_p%n
            DO j=1,mat_p%n
              bool=(mat_p%a(i,j) .APPROXEQ. 0.0_SRK)
              ASSERT(bool,"wrong zeroentries")
            ENDDO
          ENDDO
      ENDSELECT
      CALL mat_p%clear()
      DEALLOCATE(mat_p)
      NULLIFY(mat_p)
      CALL params%clear()

      ! TriDiag matrix
      ALLOCATE(TridiagMatrixType :: mat_p)
      CALL params%add("MatrixType->n",4)
      CALL params%add("MatrixType->isSym",.FALSE.)
      CALL params%add("MatrixType->matType",TRIDIAG)
      CALL params%add("MatrixType->engine",VM_NATIVE)
      CALL mat_p%init(params)
      CALL mat_p%zeroentries()
      SELECTTYPE(mat_p)
        TYPE IS(TridiagMatrixType)
          DO i=1,SIZE(mat_p%a,DIM=1)
            DO j=1,mat_p%n
              bool=(mat_p%a(i,j) .APPROXEQ. 0.0_SRK)
              ASSERT(bool,"wrong zeroentries")
            ENDDO
          ENDDO
      ENDSELECT
      CALL mat_p%clear()
      DEALLOCATE(mat_p)
      NULLIFY(mat_p)
      CALL params%clear()

      ! Banded Matrix
      ALLOCATE(BandedMatrixType :: mat_p)
      CALL params%add('MatrixType->n',4)
      CALL params%add('MatrixType->m',4)
      CALL params%add('MatrixType->nnz',9)
      CALL mat_p%init(params)
      CALL mat_p%set(1,1,1._SRK)
      CALL mat_p%set(1,2,2._SRK)
      CALL mat_p%set(2,2,3._SRK)
      CALL mat_p%set(2,3,4._SRK)
      CALL mat_p%set(3,3,5._SRK)
      CALL mat_p%set(3,4,6._SRK)
      CALL mat_p%set(4,4,7._SRK)
      CALL mat_p%set(3,1,8._SRK)
      CALL mat_p%set(4,2,9._SRK)
      SELECTTYPE(mat_p)
        TYPE IS(BandedMatrixType)
          CALL mat_p%assemble()
          CALL mat_p%zeroentries()

          DO i=1,SIZE(mat_p%bands)
            DO j=1,SIZE(mat_p%bands(i)%elem)
              bool=(mat_p%bands(i)%elem(j) .APPROXEQ. 0.0_SRK)
              ASSERT(bool,"banded%zeroentries()")
            ENDDO
          ENDDO
      ENDSELECT
      CALL mat_p%clear()
      DEALLOCATE(mat_p)
      NULLIFY(mat_p)
      CALL params%clear()


      COMPONENT_TEST('Sparse')
      CALL Plist%clear()
      CALL tmpPlist%add('MatrixType->n',4_SIK)
      CALL tmpPlist%add('MatrixType->matType',SPARSE)
      CALL tmpPlist%add('MatrixType->nnz',6_SIK)

      CALL testA%init(tmpPlist)
      CALL testA%setShape(1,2,1.0_SRK)
      CALL testA%setShape(2,1,2.0_SRK)
      CALL testA%setShape(2,4,3.0_SRK)
      CALL testA%setShape(3,4,4.0_SRK)
      CALL testA%setShape(4,1,5.0_SRK)
      CALL testA%setShape(4,4,6.0_SRK)

      CALL testA%zeroentries()
      SELECTTYPE(mat_p)
        TYPE IS(SparseMatrixType)
          bool=ALL(testA%a(:) .APPROXEQ. (/0.0_SRK,0.0_SRK,0.0_SRK,0.0_SRK,0.0_SRK,0.0_SRK/))
      ENDSELECT
      ASSERT(bool,"wrong a")
      CALL tmpPlist%clear()


#ifdef FUTILITY_HAVE_PETSC
      COMPONENT_TEST('PETSc')
      dmat_p => NULL()
      CALL tmpPlist%add("MatrixType->n",4)
      CALL tmpPlist%add("MatrixType->isSym",.FALSE.)
      CALL tmpPlist%add("MatrixType->matType",SPARSE)
      CALL tmpPlist%add("MatrixType->engine",VM_PETSC)
      CALL tmpPlist%add("MatrixType->MPI_COMM_ID",PE_COMM_SELF)
      CALL DistributedMatrixFactory(dmat_p,tmpPlist)

      CALL dmat_p%set(1,2,1.0_SRK)
      CALL dmat_p%set(2,1,2.0_SRK)
      CALL dmat_p%set(2,4,3.0_SRK)
      CALL dmat_p%set(3,4,4.0_SRK)
      CALL dmat_p%set(4,1,5.0_SRK)
      CALL dmat_p%set(4,4,6.0_SRK)

      CALL dmat_p%assemble()

      CALL dmat_p%zeroentries()

      CALL dmat_p%get(2,1,aij)
      ASSERT(aij == 0.0_SRK,'(1,2)->(2,1)')
      CALL dmat_p%get(1,2,aij)
      ASSERT(aij == 0.0_SRK,'(2,1)->(1,2)')
      CALL dmat_p%get(4,2,aij)
      ASSERT(aij == 0.0_SRK,'(2,4)->(4,2)')
      CALL dmat_p%get(4,3,aij)
      ASSERT(aij == 0.0_SRK,'(3,4)->(4,3)')
      CALL dmat_p%get(1,4,aij)
      ASSERT(aij == 0.0_SRK,'(4,1)->(1,4)')
      CALL dmat_p%get(4,4,aij)
      ASSERT(aij == 0.0_SRK,'(4,4)->(4,4)')

      CALL dmat_p%clear()
      DEALLOCATE(dmat_p)
      CALL tmpPlist%clear()
#endif
    ENDSUBROUTINE testzeroEntriesMatrix
!
!
!-------------------------------------------------------------------------------
    SUBROUTINE testFactories()
      CLASS(MatrixType),POINTER :: mat_p
      CLASS(MatrixType),POINTER :: other_mat_p
      CLASS(DistributedMatrixType),POINTER :: dmat_p
      TYPE(ParamType) :: params
#ifdef FUTILITY_HAVE_Trilinos
      INTEGER(SIK),ALLOCATABLE :: dnnz(:)
#endif

      mat_p => NULL()
      other_mat_p => NULL()
      dmat_p => NULL()

      COMPONENT_TEST("Matrix Factory")

      ! Dense Square matrix
      CALL params%add("MatrixType->n",10)
      CALL params%add("MatrixType->isSym",.FALSE.)
      CALL params%add("MatrixType->matType",DENSESQUARE)
      CALL params%add("MatrixType->engine",VM_NATIVE)
      CALL MatrixFactory(mat_p,params)
      ASSERT(ASSOCIATED(mat_p), "dense square matrix not allocated")
      ASSERT(mat_p%isInit, "dense square matrix not initialized")
      SELECTTYPE(mat_p); TYPE IS(DenseSquareMatrixType)
        ASSERT(.TRUE., "yay")
      CLASS DEFAULT
        ASSERT(.FALSE., "Wrong TYPE ALLOCATED for dense square matrix")
      ENDSELECT
      CALL MatrixResemble(other_mat_p,mat_p,params)
      SELECTTYPE(other_mat_p); TYPE IS(DenseSquareMatrixType)
        ASSERT(.TRUE., "yay")
      CLASS DEFAULT
        ASSERT(.FALSE., "Wrong TYPE ALLOCATED for cloned matrix")
      ENDSELECT
      CALL other_mat_p%clear()
      DEALLOCATE(other_mat_p)
      NULLIFY(other_mat_p)
      CALL mat_p%clear()
      DEALLOCATE(mat_p)
      NULLIFY(mat_p)
      CALL params%clear()

      ! Dense rect matrix
      CALL params%add("MatrixType->n",10)
      CALL params%add("MatrixType->m",12)
      CALL params%add("MatrixType->isSym",.FALSE.)
      CALL params%add("MatrixType->matType",DENSERECT)
      CALL params%add("MatrixType->engine",VM_NATIVE)
      CALL MatrixFactory(mat_p,params)
      ASSERT(ASSOCIATED(mat_p), "dense rect matrix not allocated")
      ASSERT(mat_p%isInit, "dense rect matrix not initialized")
      SELECTTYPE(mat_p); TYPE IS(DenseRectMatrixType)
        ASSERT(.TRUE., "yay")
      CLASS DEFAULT
        ASSERT(.FALSE., "Wrong TYPE ALLOCATED for dense rect matrix")
      ENDSELECT
      CALL MatrixResemble(other_mat_p,mat_p,params)
      SELECTTYPE(other_mat_p); TYPE IS(DenseRectMatrixType)
        ASSERT(.TRUE., "yay")
      CLASS DEFAULT
        ASSERT(.FALSE., "Wrong TYPE ALLOCATED for cloned matrix")
      ENDSELECT
      CALL other_mat_p%clear()
      DEALLOCATE(other_mat_p)
      NULLIFY(other_mat_p)
      CALL mat_p%clear()
      DEALLOCATE(mat_p)
      NULLIFY(mat_p)
      CALL params%clear()

      ! Native, sparse matrix
      CALL params%add("MatrixType->n",10)
      CALL params%add("MatrixType->nnz",25)
      CALL params%add("MatrixType->matType",SPARSE)
      CALL params%add("MatrixType->engine",VM_NATIVE)
      CALL MatrixFactory(mat_p,params)
      ASSERT(ASSOCIATED(mat_p), "sparse matrix not allocated")
      ASSERT(mat_p%isInit, "sparse matrix not initialized")
      SELECTTYPE(mat_p); TYPE IS(SparseMatrixType)
        ASSERT(.TRUE., "yay")
      CLASS DEFAULT
        ASSERT(.FALSE., "Wrong TYPE ALLOCATED for sparse matrix")
      ENDSELECT
      CALL MatrixResemble(other_mat_p,mat_p,params)
      SELECTTYPE(other_mat_p); TYPE IS(SparseMatrixType)
        ASSERT(.TRUE., "yay")
      CLASS DEFAULT
        ASSERT(.FALSE., "Wrong TYPE ALLOCATED for cloned matrix")
      ENDSELECT
      CALL other_mat_p%clear()
      DEALLOCATE(other_mat_p)
      NULLIFY(other_mat_p)
      CALL mat_p%clear()
      DEALLOCATE(mat_p)
      NULLIFY(mat_p)
      CALL params%clear()

      ! Tridiag matrix
      CALL params%add("MatrixType->n",10)
      CALL params%add("MatrixType->isSym",.FALSE.)
      CALL params%add("MatrixType->matType",TRIDIAG)
      CALL params%add("MatrixType->engine",VM_NATIVE)
      CALL MatrixFactory(mat_p,params)
      ASSERT(ASSOCIATED(mat_p), "tridiag matrix not allocated")
      ASSERT(mat_p%isInit, "tridiag matrix not initialized")
      SELECTTYPE(mat_p); TYPE IS(TridiagMatrixType)
        ASSERT(.TRUE., "yay")
      CLASS DEFAULT
        ASSERT(.FALSE., "Wrong TYPE ALLOCATED for tridiag matrix")
      ENDSELECT
      CALL MatrixResemble(other_mat_p,mat_p,params)
      SELECTTYPE(other_mat_p); TYPE IS(TridiagMatrixType)
        ASSERT(.TRUE., "yay")
      CLASS DEFAULT
        ASSERT(.FALSE., "Wrong TYPE ALLOCATED for cloned matrix")
      ENDSELECT
      CALL other_mat_p%clear()
      DEALLOCATE(other_mat_p)
      NULLIFY(other_mat_p)
      CALL mat_p%clear()
      DEALLOCATE(mat_p)
      NULLIFY(mat_p)
      CALL params%clear()

      !TPL-backed engines
#ifdef FUTILITY_HAVE_PETSC
      ! PETSc Matrix
      CALL params%add("MatrixType->n",10)
      CALL params%add("MatrixType->isSym",.FALSE.)
      CALL params%add("MatrixType->matType",SPARSE)
      CALL params%add("MatrixType->engine",VM_PETSC)
      CALL params%add("MatrixType->MPI_COMM_ID",PE_COMM_SELF)
      CALL MatrixFactory(mat_p,params)
      ASSERT(ASSOCIATED(mat_p), "PETSc matrix not allocated")
      ASSERT(mat_p%isInit, "PETSc matrix not initialized")
      SELECTTYPE(mat_p); TYPE IS(PETScMatrixType)
        ASSERT(.TRUE., "yay")
      CLASS DEFAULT
        ASSERT(.FALSE., "Wrong TYPE ALLOCATED for PETSc matrix")
      ENDSELECT
      CALL MatrixResemble(other_mat_p,mat_p,params)
      SELECTTYPE(other_mat_p); TYPE IS(PETScMatrixType)
        ASSERT(.TRUE., "yay")
      CLASS DEFAULT
        ASSERT(.FALSE., "Wrong TYPE ALLOCATED for cloned matrix")
      ENDSELECT
      CALL other_mat_p%clear()
      DEALLOCATE(other_mat_p)
      NULLIFY(other_mat_p)
      CALL mat_p%clear()
      DEALLOCATE(mat_p)
      NULLIFY(mat_p)
      CALL params%clear()
#endif

#ifdef FUTILITY_HAVE_Trilinos
      ! PETSc Matrix
      CALL params%add('MatrixType->n',10_SNK)
      CALL params%add('MatrixType->nlocal',10_SNK)
      CALL params%add('MatrixType->isSym',.FALSE.)
      CALL params%add('MatrixType->matType',SPARSE)
      ALLOCATE(dnnz(10))
      dnnz=10
      CALL params%add('MatrixType->dnnz',dnnz)
      dnnz=0
      CALL params%add('MatrixType->onnz',dnnz)
      CALL params%add("MatrixType->engine",VM_TRILINOS)
      CALL params%add("MatrixType->MPI_COMM_ID",PE_COMM_SELF)
      CALL MatrixFactory(mat_p,params)
      ASSERT(ASSOCIATED(mat_p), "Trilinos matrix not allocated")
      ASSERT(mat_p%isInit, "Trilinos matrix not initialized")
      SELECTTYPE(mat_p); TYPE IS(TrilinosMatrixType)
        ASSERT(.TRUE., "yay")
      CLASS DEFAULT
        ASSERT(.FALSE., "Wrong TYPE ALLOCATED for Trilinos matrix")
      ENDSELECT
      CALL MatrixResemble(other_mat_p,mat_p,params)
      SELECTTYPE(other_mat_p); TYPE IS(TrilinosMatrixType)
        ASSERT(.TRUE., "yay")
      CLASS DEFAULT
        ASSERT(.FALSE., "Wrong TYPE ALLOCATED for cloned matrix")
      ENDSELECT
      CALL other_mat_p%clear()
      DEALLOCATE(other_mat_p)
      NULLIFY(other_mat_p)
      CALL mat_p%clear()
      DEALLOCATE(mat_p)
      NULLIFY(mat_p)
      CALL params%clear()
#endif

      COMPONENT_TEST("Distributed Matrix Factory")
#ifdef FUTILITY_HAVE_PETSC
      ! PETSc Matrix
      CALL params%add("MatrixType->n",10)
      CALL params%add("MatrixType->isSym",.FALSE.)
      CALL params%add("MatrixType->matType",SPARSE)
      CALL params%add("MatrixType->engine",VM_PETSC)
      CALL params%add("MatrixType->MPI_COMM_ID",PE_COMM_SELF)
      CALL DistributedMatrixFactory(dmat_p,params)
      ASSERT(ASSOCIATED(dmat_p), "PETSc matrix not allocated")
      ASSERT(dmat_p%isInit, "PETSc matrix not initialized")
      SELECTTYPE(dmat_p); TYPE IS(PETScMatrixType)
        ASSERT(.TRUE., "yay")
      CLASS DEFAULT
        ASSERT(.FALSE., "Wrong TYPE ALLOCATED for PETSc matrix")
      ENDSELECT
      CALL dmat_p%clear()
      DEALLOCATE(dmat_p)
      NULLIFY(dmat_p)
      CALL params%clear()
#endif

#ifdef FUTILITY_HAVE_Trilinos
      ! PETSc Matrix
      CALL params%add('MatrixType->n',10_SNK)
      CALL params%add('MatrixType->nlocal',10_SNK)
      CALL params%add('MatrixType->isSym',.FALSE.)
      CALL params%add('MatrixType->matType',SPARSE)
      DEALLOCATE(dnnz)
      ALLOCATE(dnnz(10))
      dnnz=10
      CALL params%add('MatrixType->dnnz',dnnz)
      dnnz=0
      CALL params%add('MatrixType->onnz',dnnz)
      CALL params%add("MatrixType->engine",VM_TRILINOS)
      CALL params%add("MatrixType->MPI_COMM_ID",PE_COMM_SELF)
      CALL DistributedMatrixFactory(dmat_p,params)
      ASSERT(ASSOCIATED(dmat_p), "Trilinos matrix not allocated")
      ASSERT(dmat_p%isInit, "Trilinos matrix not initialized")
      SELECTTYPE(dmat_p); TYPE IS(TrilinosMatrixType)
        ASSERT(.TRUE., "yay")
      CLASS DEFAULT
        ASSERT(.FALSE., "Wrong TYPE ALLOCATED for Trilinos matrix")
      ENDSELECT
      CALL dmat_p%clear()
      DEALLOCATE(dmat_p)
      NULLIFY(dmat_p)
      CALL params%clear()
#endif
    ENDSUBROUTINE testFactories
!
ENDPROGRAM testMatrixTypes
