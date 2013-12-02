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
PROGRAM testPreconditionerTypes
#include "UnitTest.h"

  USE ISO_FORTRAN_ENV
  USE UnitTest
  USE IntrType
  USE ExceptionHandler
  USE BLAS
  USE ParameterLists
  USE ParallelEnv
  USE VectorTypes
  USE MatrixTypes
  USE PreconditionerTypes
  IMPLICIT NONE

#ifdef MPACT_HAVE_PETSC
#include <finclude/petsc.h>
#undef IS
  PetscErrorCode  :: ierr
#else
#ifdef HAVE_MPI
  INCLUDE 'mpif.h'
#endif
#endif
  
  TYPE(ExceptionHandlerType),TARGET :: e
  TYPE(ParamType) :: PListMat,PListVec
  CLASS(MatrixType),ALLOCATABLE :: testSparseMatrix,testDenseMatrix
  CLASS(MatrixType),ALLOCATABLE :: testBILU_1g,testBILU_mg
  CLASS(VectorType),ALLOCATABLE :: testVector
  CLASS(VectorType),ALLOCATABLE :: testVec_1g,testVec_mg

#ifdef HAVE_MPI
  INTEGER :: mpierr
  CALL MPI_Init(mpierr)
#else
  INTEGER :: MPI_COMM_WORLD=0
#endif
  !Configure exception handler for test
  CALL e%setStopOnError(.FALSE.)
  CALL e%setQuietMode(.TRUE.)
  eParams => e
  ePreCondType => e
  
  
#ifdef MPACT_HAVE_PETSC    
  CALL PetscInitialize(PETSC_NULL_CHARACTER,ierr)
#endif

  CREATE_TEST('Test Preconditioner Types')

  CALL setupILUTest()
  REGISTER_SUBTEST('Test ILU Preconditioner Type',testILU_PreCondType)
  CALL clearTest()

#ifdef MPACT_HAVE_PETSC
  CALL setupBILUTest()
  REGISTER_SUBTEST('Test BILU Preconditioner Type',testBILU_PreCondType)
  CALL clearTest() 
#endif 
  
  FINALIZE_TEST()
  
#ifdef MPACT_HAVE_PETSC    
  CALL PetscFinalize(ierr)
#endif
#ifdef HAVE_MPI
  CALL MPI_Finalize(mpierr)
#endif
!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
    SUBROUTINE setupILUTest()
      INTEGER(SIK) :: i
      REAL(SRK) :: tmpreal

      !Set up Vector
      CALL PListVec%add('VectorType->n',10_SIK)
      ALLOCATE(RealVectorType :: testVector)
      CALL testVector%init(PListVec)      

      !Set up Matrices
      CALL PListMat%add('MatrixType->nnz',40) ! Will be a 10x10, 5-stripe matrix
      CALL PListMat%add('MatrixType->n',10)
      CALL PListMat%add('MatrixType->isSym',.FALSE.)
      ALLOCATE(SparseMatrixType :: testSparseMatrix)
      ALLOCATE(DenseSquareMatrixType :: testDenseMatrix)
      CALL testSparseMatrix%init(PListMat)
      CALL testDenseMatrix%init(PListMat)

      ! Fill Matrix
      SELECTTYPE(testSparseMatrix); TYPE IS(SparseMatrixType)
        tmpreal=0.0_SRK
        DO i=1,10
          IF(i >= 5) THEN
            tmpreal=tmpreal+1.0_SRK
            CALL testSparseMatrix%setShape(i,i-4,tmpreal)
            CALL testDenseMatrix%set(i,i-4,tmpreal)
          ENDIF
          IF(i >= 2) THEN
            tmpreal=tmpreal+1.0_SRK
            CALL testSparseMatrix%setShape(i,i-1,tmpreal)
            CALL testDenseMatrix%set(i,i-1,tmpreal)
          ENDIF
          tmpreal=tmpreal+1.0_SRK
          CALL testSparseMatrix%setShape(i,i,tmpreal)
          CALL testDenseMatrix%set(i,i,tmpreal)
          IF(i <= 9) THEN
            tmpreal=tmpreal+1.0_SRK
            CALL testSparseMatrix%setShape(i,i+1,tmpreal)
            CALL testDenseMatrix%set(i,i+1,tmpreal)
          ENDIF
          IF(i <= 6) THEN
            tmpreal=tmpreal+1.0_SRK
            CALL testSparseMatrix%setShape(i,i+4,tmpreal)
            CALL testDenseMatrix%set(i,i+4,tmpreal)
          ENDIF
          CALL testVector%set(i,REAL(i*1.0_SRK,SRK))
        ENDDO
      CLASS DEFAULT
        ASSERT(.FALSE.,'ALLOCATE(SparseMatrixType :: testSparseMatrix)')
      ENDSELECT

    ENDSUBROUTINE setupILUTest
!
!-------------------------------------------------------------------------------
    SUBROUTINE testILU_PreCondType()
      CLASS(LU_PreCondType),ALLOCATABLE :: testLU
      TYPE(RealVectorType) :: tempVector
      LOGICAL(SBK) :: bool
      REAL(SRK) :: tmpreal(25),tmpreal2(10)

      !This is used to check %apply() routines
      tmpreal2=(/81.828106457936357_SRK,-32.910224240167040_SRK,-8.6840659884980056_SRK,12.521242974252598_SRK, &
        -5.0025526592007603_SRK,-6.9466109699304406_SRK,8.1414442396900757_SRK,-1.8345636773373761_SRK, &
        -9.2450823045000341_SRK,13.515984012597492_SRK/)

      COMPONENT_TEST('ILU Preconditioner Type, DenseMatrixType')
      IF(testDenseMatrix%isInit .AND. testVector%isInit) THEN
        ALLOCATE(ILU_PreCondType :: testLU)
        
        ! Check %init
        CALL testLU%init(testDenseMatrix)
        ASSERT(testLU%isInit,'DenseSquareMatrixType ILU%isInit')
        ASSERT(ASSOCIATED(testLU%A),'DenseSquareMatrixType ASSOCIATED(ILU%LU%A)')
        ASSERT(testLU%L%isInit,'DenseSquareMatrixType ILU%L%isInit')
        ASSERT(testLU%U%isInit,'DenseSquareMatrixType ILU%U%isInit')
        SELECTTYPE(L => testLU%L); TYPE IS(SparseMatrixType)
          bool=ALL(L%ia == (/1,2,4,6,8,11,14,17,20,23,26/))
          ASSERT(bool,'DenseSquareMatrixType ILU%L%ia')
          bool=ALL(L%ja == (/1,1,2,2,3,3,4,1,4,5,2,5,6,3,6,7,4,7,8,5,8,9,6,9,10/))
          ASSERT(bool,'DenseSquareMatrixType ILU%L%ja')
        CLASS DEFAULT
          ASSERT(.FALSE.,'DenseSquareMatrixType ILU%L TYPE IS(SparseMatrixType)')
        ENDSELECT
        SELECTTYPE(u => testLU%U); TYPE IS(SparseMatrixType)
          bool=ALL(U%ia == (/1,4,7,10,13,16,19,21,23,25,26/))
          ASSERT(bool,'DenseSquareMatrixType ILU%U%ia')
          bool=ALL(U%ja == (/1,2,5,2,3,6,3,4,7,4,5,8,5,6,9,6,7,10,7,8,8,9,9,10,10/))
          ASSERT(bool,'DenseSquareMatrixType ILU%U%ja')
        CLASS DEFAULT
          ASSERT(.FALSE.,'DenseSquareMatrixType ILU%U TYPE IS(SparseMatrixType)')
        ENDSELECT

        ! Check %setup
        CALL testLU%setup()
        SELECTTYPE(L => testLU%L); TYPE IS(SparseMatrixType)
          tmpreal=(/1.0000000000000000_SRK,4.0000000000000000_SRK,1.0000000000000000_SRK, &
            -2.6666666666666665_SRK,1.0000000000000000_SRK,0.47999999999999998_SRK,1.0000000000000000_SRK, &
            16.000000000000000_SRK,2.0731707317073171_SRK,1.0000000000000000_SRK,-7.0000000000000000_SRK, &
            -0.37272727272727274_SRK,1.0000000000000000_SRK,1.0400000000000000_SRK,0.34141855385676517_SRK, &
            1.0000000000000000_SRK,3.6585365853658538_SRK,3.7054946009258081_SRK,1.0000000000000000_SRK,&
            -0.57603305785123970_SRK,-0.26853383673906256_SRK,1.0000000000000000_SRK,0.48051500172433614_SRK,&
            0.69170671251519822_SRK,1.0000000000000000_SRK/)
          ASSERT(ALL(L%a .APPROXEQA. tmpreal),'DenseSquareMatrixtype ILU%L%a')
        ENDSELECT
        SELECTTYPE(U => testLU%U); TYPE IS(SparseMatrixType)
          tmpreal=(/1.0000000000000000_SRK,2.0000000000000000_SRK,3.0000000000000000_SRK, &
            -3.0000000000000000_SRK,6.0000000000000000_SRK,7.0000000000000000_SRK,25.000000000000000_SRK, &
            10.000000000000000_SRK,11.000000000000000_SRK,8.1999999999999993_SRK,14.000000000000000_SRK, &
            15.000000000000000_SRK,-59.024390243902438_SRK,19.000000000000000_SRK,20.000000000000000_SRK, &
            79.081818181818178_SRK,24.000000000000000_SRK,25.000000000000000_SRK,8.3659547074376341_SRK, &
            29.000000000000000_SRK,-130.33739220733625_SRK,33.000000000000000_SRK,56.382277769413854_SRK, &
            37.000000000000000_SRK,2.3939765938292652_SRK/)
          ASSERT(ALL(U%a .APPROXEQA. tmpreal),'DenseSquareMatrixType ILU%U%a')
        ENDSELECT

        ! Check %apply
        SELECTTYPE(testVector); TYPE IS(RealVectorType)
          tempVector=testVector
        ENDSELECT
        CALL testLU%apply(tempVector)
        ASSERT(ALL(tempVector%b .APPROXEQA. tmpreal2),'DenseSquareMatrixType ILU%apply(vector)')
        

        ! Check %clear
        CALL testLU%clear()
      ELSE
        ASSERT(testSparseMatrix%isInit,'TestDenseMatrix Initialization')
        ASSERT(testVector%isInit,'TestVector Initialization')
      ENDIF
        
      COMPONENT_TEST('ILU Preconditioner Type, SparseMatrixType')
      ! Check %init
      CALL testLU%init(testSparseMatrix)
      IF(testSparseMatrix%isInit .AND. testVector%isInit) THEN
        ASSERT(testLU%isInit,'SparseMatrixType ILU%isInit')
        ASSERT(ASSOCIATED(testLU%A),'SparseMatrixType ASSOCIATED(ILU%LU%A)')
        ASSERT(testLU%L%isInit,'SparseMatrixType ILU%L%isInit')
        ASSERT(testLU%U%isInit,'SparseMatrixType ILU%U%isInit')
        SELECTTYPE(L => testLU%L); TYPE IS(SparseMatrixType)
          bool=ALL(L%ia == (/1,2,4,6,8,11,14,17,20,23,26/))
          ASSERT(bool,'SparseMatrixType ILU%L%ia')
          bool=ALL(L%ja == (/1,1,2,2,3,3,4,1,4,5,2,5,6,3,6,7,4,7,8,5,8,9,6,9,10/))
          ASSERT(bool,'SparseMatrixType ILU%L%ja')
        CLASS DEFAULT
          ASSERT(.FALSE.,'SparseMatrixType ILU%L TYPE IS(SparseMatrixType)')
        ENDSELECT
        SELECTTYPE(u => testLU%U); TYPE IS(SparseMatrixType)
          bool=ALL(U%ia == (/1,4,7,10,13,16,19,21,23,25,26/))
          ASSERT(bool,'SparseMatrixType ILU%U%ia')
          bool=ALL(U%ja == (/1,2,5,2,3,6,3,4,7,4,5,8,5,6,9,6,7,10,7,8,8,9,9,10,10/))
          ASSERT(bool,'SparseMatrixType ILU%U%ja')
        CLASS DEFAULT
          ASSERT(.FALSE.,'SparseMatrixType ILU%U TYPE IS(SparseMatrixType)')
        ENDSELECT

        ! Check %setup
        CALL testLU%setup()
        SELECTTYPE(L => testLU%L); TYPE IS(SparseMatrixType)
          tmpreal=(/1.0000000000000000_SRK,4.0000000000000000_SRK,1.0000000000000000_SRK, &
            -2.6666666666666665_SRK,1.0000000000000000_SRK,0.47999999999999998_SRK,1.0000000000000000_SRK, &
            16.000000000000000_SRK,2.0731707317073171_SRK,1.0000000000000000_SRK,-7.0000000000000000_SRK, &
            -0.37272727272727274_SRK,1.0000000000000000_SRK,1.0400000000000000_SRK,0.34141855385676517_SRK, &
            1.0000000000000000_SRK,3.6585365853658538_SRK,3.7054946009258081_SRK,1.0000000000000000_SRK,&
            -0.57603305785123970_SRK,-0.26853383673906256_SRK,1.0000000000000000_SRK,0.48051500172433614_SRK,&
            0.69170671251519822_SRK,1.0000000000000000_SRK/)
          ASSERT(ALL(L%a .APPROXEQA. tmpreal),'SparseMatrixtype ILU%L%a')
        ENDSELECT
        SELECTTYPE(U => testLU%U); TYPE IS(SparseMatrixType)
          tmpreal=(/1.0000000000000000_SRK,2.0000000000000000_SRK,3.0000000000000000_SRK, &
            -3.0000000000000000_SRK,6.0000000000000000_SRK,7.0000000000000000_SRK,25.000000000000000_SRK, &
            10.000000000000000_SRK,11.000000000000000_SRK,8.1999999999999993_SRK,14.000000000000000_SRK, &
            15.000000000000000_SRK,-59.024390243902438_SRK,19.000000000000000_SRK,20.000000000000000_SRK, &
            79.081818181818178_SRK,24.000000000000000_SRK,25.000000000000000_SRK,8.3659547074376341_SRK, &
            29.000000000000000_SRK,-130.33739220733625_SRK,33.000000000000000_SRK,56.382277769413854_SRK, &
            37.000000000000000_SRK,2.3939765938292652_SRK/)
          ASSERT(ALL(U%a .APPROXEQA. tmpreal),'SparseMatrixType ILU%U%a')
        ENDSELECT

        ! Check %apply
        SELECTTYPE(testVector); TYPE IS(RealVectorType)
          tempVector=testVector
        ENDSELECT
        CALL testLU%apply(tempVector)
        ASSERT(ALL(tempVector%b .APPROXEQA. tmpreal2),'SparseMatrixType ILU%apply(vector)')

        ! Check %clear
        CALL testLU%clear()
        ASSERT(.NOT.(testLU%isInit),'SparseMatrixType .NOT.(ILU%LU%isInit)')
        ASSERT(.NOT.(ASSOCIATED(testLU%A)),'SparseMatrixType .NOT.(ASSOCIATED(ILU%LU%A))')
        ASSERT(.NOT.(ASSOCIATED(testLU%L)),'SparseMatrixType .NOT.(ASSOCIATED(ILU%LU%L))')
        ASSERT(.NOT.(ASSOCIATED(testLU%U)),'SparseMatrixType .NOT.(ASSOCIATED(ILU%LU%U))')
      ELSE
        ASSERT(testSparseMatrix%isInit,'TestSparseMatrix Initialization')
        ASSERT(testVector%isInit,'TestVector Initialization')
      ENDIF

      DEALLOCATE(testLU)

    ENDSUBROUTINE testILU_PreCondtype
!
!-------------------------------------------------------------------------------
    SUBROUTINE setupBILUTest()
      INTEGER(SIK) :: i,j,iostatus,ierr
      INTEGER(SIK) :: nPlane,nPin,nGrp,N
      REAL(SRK) :: tmpreal,val

#ifdef HAVE_MPI
      !!!setup 1g stuff      
      nPlane=3
      nPin=9
      nGrp=1
      N=nPlane*nPin*nGrp


      !Set up Vector
      CALL PListVec%clear()
      CALL PListVec%add('VectorType->n',N)
      CALL PListVec%add('VectorType->MPI_Comm_ID',MPI_COMM_WORLD)    
      ALLOCATE(PETScVectorType :: testVec_1g)
      CALL testVec_1g%init(PListVec) 

      !Set up Matrix
      CALL PListMat%clear()
      CALL PListMat%add('MatrixType->matType',SPARSE)
      CALL PListMat%add('MatrixType->n',N)
      CALL PListMat%add('MatrixType->isSym',.FALSE.)
      CALL PListMat%add('MatrixType->MPI_Comm_ID',MPI_COMM_WORLD)
      ALLOCATE(PETScMatrixType :: testBILU_1g)
      CALL testBILU_1g%init(PListMat)

      SELECTTYPE(testBILU_1g); TYPE IS(PETScMatrixType)
        
        ! Fill Matrix from File
        OPEN(unit=111,file="matrices/1g_matrix.txt",status='old')
        READ(111,*,iostat=iostatus)
        DO WHILE(iostatus==0)
          READ(111,*,iostat=iostatus) i,j,val
          CALL testBILU_1g%set(i,j,val)
        ENDDO
        CLOSE(111)
        CALL testBILU_1g%assemble()
!        CALL MatView(testSparseMatrix%A,PETSC_VIEWER_STDOUT_SELF,ierr)
        
      CLASS DEFAULT
        ASSERT(.FALSE.,'ALLOCATE(SparseMatrixType :: testSparseMatrix)')
      ENDSELECT

      !!!setup mg stuff      
      nPlane=3
      nPin=9
      nGrp=56
      N=nPlane*nPin*nGrp

      !Set up Vector
      CALL PListVec%clear()
      CALL PListVec%add('VectorType->n',N)
      CALL PListVec%add('VectorType->MPI_Comm_ID',MPI_COMM_WORLD)    
      ALLOCATE(PETScVectorType :: testVec_mg)
      CALL testVec_mg%init(PListVec) 

      !Set up Matrix
      CALL PListMat%clear()
      CALL PListMat%add('MatrixType->matType',SPARSE)
      CALL PListMat%add('MatrixType->n',N)
      CALL PListMat%add('MatrixType->isSym',.FALSE.)
      CALL PListMat%add('MatrixType->MPI_Comm_ID',MPI_COMM_WORLD)
      ALLOCATE(PETScMatrixType :: testBILU_mg)
      CALL testBILU_mg%init(PListMat)

      SELECTTYPE(testBILU_mg); TYPE IS(PETScMatrixType)
        
        ! Fill Matrix from File
        OPEN(unit=111,file="matrices/mg_matrix.txt",status='old')
        READ(111,*,iostat=iostatus)
        DO WHILE(iostatus==0)
          READ(111,*,iostat=iostatus) i,j,val
          CALL testBILU_mg%set(i,j,val)
        ENDDO
        CLOSE(111)
        CALL testBILU_mg%assemble()
        
      CLASS DEFAULT
        ASSERT(.FALSE.,'ALLOCATE(SparseMatrixType :: testSparseMatrix)')
      ENDSELECT
#endif

    ENDSUBROUTINE setupBILUTest
!
!-------------------------------------------------------------------------------
    SUBROUTINE testBILU_PreCondType()
      CLASS(LU_PreCondType),ALLOCATABLE :: testLU

      INTEGER(SIK) :: nPlane,nPin,nGrp
      INTEGER(SIK) :: row,col,i,j,iostatus
      REAL(SRK) :: tmpval,reftmpval,val
      TYPE(ParamType) :: pList
      CLASS(MatrixType),ALLOCATABLE :: refBILU_L,refBILU_U
      
      COMPONENT_TEST('BILU Preconditioner Type (1g)')
#ifdef HAVE_MPI
      IF(ALLOCATED(testBILU_1g) .AND. ALLOCATED(testVec_1g)) THEN
        IF(testBILU_1g%isInit .AND. testVec_1g%isInit) THEN
          ALLOCATE(BILU_PreCondType :: testLU)
          
          nPlane=3
          nPin=9
          nGrp=1
  
          !initialize        
          CALL testLU%init(testBILU_1g)
          !set some values (to be moved into init at some point)
          SELECTTYPE(pc => testLU); TYPE IS(BILU_PreCondType)
            pc%nPlane=nPlane
            pc%nPin=nPin
            pc%nGrp=nGrp    
          ENDSELECT
          ! Check %init
          ASSERT(testLU%isInit,'BILU Preconditioner %isInit')
          ASSERT(ASSOCIATED(testLU%A),'BILU Preconditioner ASSOCIATED(LU%A)')
          ASSERT(testLU%L%isInit,'BILU Preconditioner %L%isInit')
          ASSERT(testLU%U%isInit,'BILU Preconditioner %U%isInit')
  
          ! Check %setup
          CALL testLU%setup()
          
!          !write some matrices out
!          SELECTTYPE(A => testLU%A); TYPE IS(PETScMatrixType)
!            WRITE(667,*) "A: "
!            DO row=1,A%n
!              WRITE(667,'(I4)',ADVANCE='NO') row
!              DO col=1,A%n
!                CALL A%get(row,col,tmpval)
!                WRITE(667,'(ES20.10)',ADVANCE='NO') tmpval
!              ENDDO   
!              WRITE(667,*)            
!            ENDDO
!            WRITE(667,*)  
!          ENDSELECT
!          SELECTTYPE(A => testLU%L); TYPE IS(PETScMatrixType)
!            WRITE(667,*) "L: "
!            DO row=1,A%n
!              WRITE(667,'(I4)',ADVANCE='NO') row
!              DO col=1,A%n
!                CALL A%get(row,col,tmpval)
!                WRITE(667,'(ES20.10)',ADVANCE='NO') tmpval
!              ENDDO   
!              WRITE(667,*)            
!            ENDDO
!            WRITE(667,*)  
!          ENDSELECT
!          SELECTTYPE(A => testLU%L); TYPE IS(PETScMatrixType)
!            DO row=1,A%n
!              DO col=1,A%n
!                CALL A%get(row,col,tmpval)
!                IF(tmpval /= 0.0_SRK) WRITE(670,'(2I7,ES25.15)') row,col,tmpval
!              ENDDO         
!            ENDDO
!          ENDSELECT
!          SELECTTYPE(A => testLU%U); TYPE IS(PETScMatrixType)
!            WRITE(667,*) "U: "
!            DO row=1,A%n
!              WRITE(667,'(I4)',ADVANCE='NO') row
!              DO col=1,A%n
!                CALL A%get(row,col,tmpval)
!                WRITE(667,'(ES20.10)',ADVANCE='NO') tmpval
!              ENDDO   
!              WRITE(667,*)            
!            ENDDO
!            WRITE(667,*)  
!          ENDSELECT
!          SELECTTYPE(A => testLU%U); TYPE IS(PETScMatrixType)
!            DO row=1,A%n
!              DO col=1,A%n
!                CALL A%get(row,col,tmpval)
!                IF(tmpval /= 0.0_SRK) WRITE(671,'(2I7,ES20.10)') row,col,tmpval
!              ENDDO         
!            ENDDO
!          ENDSELECT
          
          !setup ref L
          CALL PListMat%add('MatrixType->matType',SPARSE)
          CALL PListMat%add('MatrixType->n',testLU%L%N)
          CALL PListMat%add('MatrixType->isSym',.FALSE.)
          CALL PListMat%add('MatrixType->MPI_Comm_ID',MPI_COMM_WORLD)
          ALLOCATE(PETScMatrixType :: refBILU_L)
          CALL refBILU_L%init(PListMat)
          OPEN(unit=111,file="matrices/refBILU_1gL.txt",status='old')
          iostatus=0
          DO WHILE(iostatus==0)
            READ(111,*,iostat=iostatus) i,j,val
            CALL refBILU_L%set(i,j,val)
          ENDDO
          CLOSE(111)
          !check L
          SELECTTYPE(L => testLU%L); TYPE IS(PETScMatrixType)
            SELECTTYPE(refL => refBILU_L); TYPE IS(PETScMatrixType)
              DO row=1,L%n
                DO col=1,L%n
                  CALL L%get(row,col,tmpval)
                  CALL refL%get(row,col,reftmpval)
                  IF(tmpval /= 0.0_SRK .AND. reftmpval /= 0.0_SRK) THEN
                    ASSERT( ABS(tmpval-reftmpval) < 1E-12_SRK,"BILU_L failed")
                    FINFO() row,col,tmpval,reftmpval,ABS(tmpval-reftmpval)
                  ENDIF
                ENDDO         
              ENDDO
            ENDSELECT
          ENDSELECT
          CALL refBILU_L%clear()
          DEALLOCATE(refBILU_L)
  
          !setup ref U
          CALL PListMat%add('MatrixType->matType',SPARSE)
          CALL PListMat%add('MatrixType->n',testLU%U%N)
          CALL PListMat%add('MatrixType->isSym',.FALSE.)
          CALL PListMat%add('MatrixType->MPI_Comm_ID',MPI_COMM_WORLD)
          ALLOCATE(PETScMatrixType :: refBILU_U)
          CALL refBILU_U%init(PListMat)
          OPEN(unit=111,file="matrices/refBILU_1gU.txt",status='old')
          iostatus=0
          DO WHILE(iostatus==0)
            READ(111,*,iostat=iostatus) i,j,val
            CALL refBILU_U%set(i,j,val)
          ENDDO
          CLOSE(111)
          !check U
          SELECTTYPE(U => testLU%U); TYPE IS(PETScMatrixType)
            SELECTTYPE(refU => refBILU_U); TYPE IS(PETScMatrixType)
              DO row=1,U%n
                DO col=1,U%n
                  CALL U%get(row,col,tmpval)
                  CALL refU%get(row,col,reftmpval)
                  IF(tmpval /= 0.0_SRK .AND. reftmpval /= 0.0_SRK) THEN
                    ASSERT( ABS(tmpval-reftmpval) < 1E-12_SRK,"BILU_U failed")
                    FINFO() row,col,tmpval,reftmpval,ABS(tmpval-reftmpval)
                  ENDIF
                ENDDO         
              ENDDO
            ENDSELECT
          ENDSELECT
          CALL refBILU_U%clear()
          DEALLOCATE(refBILU_U)
  
          ! Check %apply
          CALL testLU%apply(testVec_1g)
  
          ! Check %clear
          CALL testLU%clear()
          ASSERT(.NOT.(testLU%isInit),'BILU Preconditioner .NOT.(lu%isInit)')
          ASSERT(.NOT.(ASSOCIATED(testLU%A)),'BILU Preconditioner .NOT.(ASSOCIATED(LU%A))')
          ASSERT(.NOT.(ASSOCIATED(testLU%L)),'BILU Preconditioner .NOT.(ASSOCIATED(LU%L))')
          ASSERT(.NOT.(ASSOCIATED(testLU%U)),'BILU Preconditioner .NOT.(ASSOCIATED(LU%U))')
          DEALLOCATE(testLU)
        ELSE
          ASSERT(testBILU_1g%isInit,'TestMatrix Initialization')
          ASSERT(testVec_1g%isInit,'TestVector Initialization')
        ENDIF
      ELSE
        ASSERT(ALLOCATED(testBILU_1g),'TestMatrix Allocation')
        ASSERT(ALLOCATED(testVec_1g),'TestVector Allocation')
      ENDIF

      
      COMPONENT_TEST('BILU Preconditioner Type (mg)')
      IF(ALLOCATED(testBILU_mg) .AND. ALLOCATED(testVec_mg)) THEN
        IF(testBILU_mg%isInit .AND. testVec_mg%isInit) THEN
          ALLOCATE(BILU_PreCondType :: testLU)
          
          nPlane=3
          nPin=9
          nGrp=56
  
          !initialize        
          CALL testLU%init(testBILU_mg)
          !set some values (to be moved into init at some point)
          SELECTTYPE(pc => testLU); TYPE IS(BILU_PreCondType)
            pc%nPlane=nPlane
            pc%nPin=nPin
            pc%nGrp=nGrp    
          ENDSELECT
          ! Check %init
          ASSERT(testLU%isInit,'BILU Preconditioner %isInit')
          ASSERT(ASSOCIATED(testLU%A),'BILU Preconditioner ASSOCIATED(LU%A)')
          ASSERT(testLU%L%isInit,'BILU Preconditioner %L%isInit')
          ASSERT(testLU%U%isInit,'BILU Preconditioner %U%isInit')
  
          ! Check %setup
          CALL testLU%setup()
          
!          !write some matrices out
!          SELECTTYPE(A => testLU%A); TYPE IS(PETScMatrixType)
!            WRITE(667,*) "A: "
!            WRITE(*,*) "A%n: ",a%n
!            DO row=1,A%n
!              WRITE(667,'(I4)',ADVANCE='NO') row
!              DO col=1,A%n
!                CALL A%get(row,col,tmpval)
!                WRITE(667,'(ES20.10)',ADVANCE='NO') tmpval
!              ENDDO   
!              WRITE(667,*)            
!            ENDDO
!            WRITE(667,*)  
!          ENDSELECT
!          SELECTTYPE(A => testLU%L); TYPE IS(PETScMatrixType)
!            WRITE(667,*) "L: "
!            DO row=1,A%n
!              WRITE(667,'(I4)',ADVANCE='NO') row
!              DO col=1,A%n
!                CALL A%get(row,col,tmpval)
!                WRITE(667,'(ES20.10)',ADVANCE='NO') tmpval
!              ENDDO   
!              WRITE(667,*)            
!            ENDDO
!            WRITE(667,*)  
!          ENDSELECT
!          SELECTTYPE(A => testLU%L); TYPE IS(PETScMatrixType)
!            DO row=1,A%n
!              DO col=1,A%n
!                CALL A%get(row,col,tmpval)
!                IF(tmpval /= 0.0_SRK) WRITE(670,'(2I7,ES25.15)') row,col,tmpval
!              ENDDO         
!            ENDDO
!          ENDSELECT
!          SELECTTYPE(A => testLU%U); TYPE IS(PETScMatrixType)
!            WRITE(667,*) "U: "
!            DO row=1,A%n
!              WRITE(667,'(I4)',ADVANCE='NO') row
!              DO col=1,A%n
!                CALL A%get(row,col,tmpval)
!                WRITE(667,'(ES20.10)',ADVANCE='NO') tmpval
!              ENDDO   
!              WRITE(667,*)            
!            ENDDO
!            WRITE(667,*)  
!          ENDSELECT
!          SELECTTYPE(A => testLU%U); TYPE IS(PETScMatrixType)
!            DO row=1,A%n
!              DO col=1,A%n
!                CALL A%get(row,col,tmpval)
!                IF(tmpval /= 0.0_SRK) WRITE(671,'(2I7,ES20.10)') row,col,tmpval
!              ENDDO         
!            ENDDO
!          ENDSELECT
          
          !setup ref L
          CALL PListMat%add('MatrixType->matType',SPARSE)
          CALL PListMat%add('MatrixType->n',testLU%L%N)
          CALL PListMat%add('MatrixType->isSym',.FALSE.)
          CALL PListMat%add('MatrixType->MPI_Comm_ID',MPI_COMM_WORLD)
          ALLOCATE(PETScMatrixType :: refBILU_L)
          CALL refBILU_L%init(PListMat)
          OPEN(unit=111,file="matrices/refBILU_mgL.txt",status='old')
          iostatus=0
          DO WHILE(iostatus==0)
            READ(111,*,iostat=iostatus) i,j,val
            CALL refBILU_L%set(i,j,val)
          ENDDO
          CLOSE(111)
          !check L
          SELECTTYPE(L => testLU%L); TYPE IS(PETScMatrixType)
            SELECTTYPE(refL => refBILU_L); TYPE IS(PETScMatrixType)
              DO row=1,L%n
                DO col=1,L%n
                  CALL L%get(row,col,tmpval)
                  CALL refL%get(row,col,reftmpval)
                  IF(tmpval /= 0.0_SRK .AND. reftmpval /= 0.0_SRK) THEN
                    ASSERT( ABS(tmpval-reftmpval) < 1E-12_SRK,"BILU_L failed")
                    FINFO() row,col,tmpval,reftmpval,ABS(tmpval-reftmpval)
                  ENDIF
                ENDDO         
              ENDDO
            ENDSELECT
          ENDSELECT
          CALL refBILU_L%clear()
          DEALLOCATE(refBILU_L)
  
          !setup ref U
          CALL PListMat%add('MatrixType->matType',SPARSE)
          CALL PListMat%add('MatrixType->n',testLU%U%N)
          CALL PListMat%add('MatrixType->isSym',.FALSE.)
          CALL PListMat%add('MatrixType->MPI_Comm_ID',MPI_COMM_WORLD)
          ALLOCATE(PETScMatrixType :: refBILU_U)
          CALL refBILU_U%init(PListMat)
          OPEN(unit=111,file="matrices/refBILU_mgU.txt",status='old')
          iostatus=0
          DO WHILE(iostatus==0)
            READ(111,*,iostat=iostatus) i,j,val
            CALL refBILU_U%set(i,j,val)
          ENDDO
          CLOSE(111)
          !check U
          SELECTTYPE(U => testLU%U); TYPE IS(PETScMatrixType)
            SELECTTYPE(refU => refBILU_U); TYPE IS(PETScMatrixType)
              DO row=1,U%n
                DO col=1,U%n
                  CALL U%get(row,col,tmpval)
                  CALL refU%get(row,col,reftmpval)
                  IF(tmpval /= 0.0_SRK .AND. reftmpval /= 0.0_SRK) THEN
                    ASSERT( ABS(tmpval-reftmpval) < 1E-12_SRK,"BILU_U failed")
                    FINFO() row,col,tmpval,reftmpval,ABS(tmpval-reftmpval)
                  ENDIF
                ENDDO         
              ENDDO
            ENDSELECT
          ENDSELECT
          CALL refBILU_U%clear()
          DEALLOCATE(refBILU_U)
  
          ! Check %apply
          CALL testLU%apply(testVec_mg)
  
          ! Check %clear
          CALL testLU%clear()
          ASSERT(.NOT.(testLU%isInit),'BILU Preconditioner .NOT.(lu%isInit)')
          ASSERT(.NOT.(ASSOCIATED(testLU%A)),'BILU Preconditioner .NOT.(ASSOCIATED(LU%A))')
          ASSERT(.NOT.(ASSOCIATED(testLU%L)),'BILU Preconditioner .NOT.(ASSOCIATED(LU%L))')
          ASSERT(.NOT.(ASSOCIATED(testLU%U)),'BILU Preconditioner .NOT.(ASSOCIATED(LU%U))')
          DEALLOCATE(testLU)
        ELSE
          ASSERT(testBILU_mg%isInit,'TestMatrix Initialization')
          ASSERT(testVec_mg%isInit,'TestVector Initialization')
        ENDIF
      ELSE
        ASSERT(ALLOCATED(testBILU_mg),'TestMatrix Allocation')
        ASSERT(ALLOCATED(testVec_mg),'TestVector Allocation')
      ENDIF
#endif
      

    ENDSUBROUTINE testBILU_PreCondtype
!
!-------------------------------------------------------------------------------
    SUBROUTINE clearTest()

      CALL PListMat%clear()
      CALL PListVec%clear()
      CALL MatrixTypes_Clear_ValidParams()
      IF(ALLOCATED(testSparseMatrix)) CALL testSparseMatrix%clear()
      IF(ALLOCATED(testBILU_1g)) CALL testBILU_1g%clear()
      IF(ALLOCATED(testBILU_mg)) CALL testBILU_mg%clear()
      IF(ALLOCATED(testVector)) CALL testVector%clear()
       
      IF(ALLOCATED(testSparseMatrix)) DEALLOCATE(testSparseMatrix)
      IF(ALLOCATED(testBILU_1g)) DEALLOCATE(testBILU_1g)
      IF(ALLOCATED(testBILU_mg)) DEALLOCATE(testBILU_mg)
      IF(ALLOCATED(testDenseMatrix)) DEALLOCATE(testDenseMatrix)
      IF(ALLOCATED(testVector)) DEALLOCATE(testVector)
     

    ENDSUBROUTINE clearTest
!
ENDPROGRAM testPreconditionerTypes
