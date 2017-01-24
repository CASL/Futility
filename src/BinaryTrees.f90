!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!> @brief Utility module for supplying basic binary tree data types
!>
!> @par Module Dependencies
!>  - @ref IntrType "IntrType": @copybrief IntrType
!>
!> @author Brendan Kochunas
!>    @date 08/05/2011
!>
!> @todo
!>  - Create equivalent non-recursive subroutines
!>  - Add other types of binary trees as needed
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE BinaryTrees

  USE IntrType
  IMPLICIT NONE
  PRIVATE

  PUBLIC :: BinaryTreeType_Integer
  PUBLIC :: BinaryTreeType_Index
  PUBLIC :: CreateBinaryTree
  PUBLIC :: CreateBinaryTreeRmDup
  PUBLIC :: BurnBinaryTree
  PUBLIC :: SortedBinaryTree

  !> Basic binary tree type that has an integer value on each node/leaf
  TYPE :: BinaryTreeType_Integer
    !> Value of node
    INTEGER(SIK) :: val
    !> Node that is one level down and to the left of current node
    TYPE(BinaryTreeType_Integer),POINTER :: left => NULL()
    !> Node that is one level down and to the right of current node
    TYPE(BinaryTreeType_Integer),POINTER :: right => NULL()
!
!List of type bound procedures (methods) for the Exception Handler object
    CONTAINS
      !> @copybrief BinaryTrees::SearchBinaryTree_Integer
      !> @copydetails BinaryTrees::SearchBinaryTree_Integer
      PROCEDURE,PASS :: match => SearchBinaryTree_Integer
  ENDTYPE BinaryTreeType_Integer

  !> Basic binary tree type that has an integer value on each node/leaf
  TYPE :: BinaryTreeType_Index
    !> Value of node
    INTEGER(SIK) :: val
    !> Return Value of node
    INTEGER(SIK) :: index
    !> Node that is one level down and to the left of current node
    TYPE(BinaryTreeType_Index),POINTER :: left => NULL()
    !> Node that is one level down and to the right of current node
    TYPE(BinaryTreeType_Index),POINTER :: right => NULL()
!
!List of type bound procedures (methods) for the Exception Handler object
    CONTAINS
      !> @copybrief BinaryTrees::SearchBinaryTreeIndex_Index
      !> @copydetails BinaryTrees::SearchBinaryTreeIndex_Index
      PROCEDURE,PASS :: match=> SearchBinaryTree_Index
      !> @copybrief BinaryTrees::getBinaryTreeIndex_Index
      !> @copydetails BinaryTrees::getBinaryTreeIndex_Index
      PROCEDURE,PASS :: findIndex => getBinaryTree_Index
  ENDTYPE BinaryTreeType_Index

  !> Generic interface to creating a binary tree data type for
  !> different types of binary trees and input data
  INTERFACE CreateBinaryTree
    !> @copybrief BinaryTrees::CreateBinaryTreeIndex_Integer
    !> @copydetails BinaryTrees::CreateBinaryTreeIndex_Integer
    MODULE PROCEDURE CreateBinaryTree_Integer
    !> @copybrief BinaryTrees::CreateBinaryTreeIndex_Index
    !> @copydetails BinaryTrees::CreateBinaryTreeIndex_Index
    MODULE PROCEDURE CreateBinaryTree_Index
  ENDINTERFACE CreateBinaryTree

  !> Generic interface to creating a binary tree data type for
  !> different types of binary trees and input data
  INTERFACE CreateBinaryTreeRmDup
    !> @copybrief BinaryTrees::CreateBinaryTreeRmDup_Integer
    !> @copydetails BinaryTrees::CreateBinaryTreeRmDup_Integer
    MODULE PROCEDURE CreateBinaryTreeRmDup_Integer
    !> @copybrief BinaryTrees::CreateBinaryTreeRmDup_Index
    !> @copydetails BinaryTrees::CreateBinaryTreeRmDup_Index
    MODULE PROCEDURE CreateBinaryTreeRmDup_Index
  ENDINTERFACE CreateBinaryTreeRmDup

  !> Generic interface for destroying a binary tree
  INTERFACE BurnBinaryTree
    !> @copybrief BinaryTrees::BurnBinaryTreeIndex_Integer
    !> @copydetails BinaryTrees::BurnBinaryTreeIndex_Integer
    MODULE PROCEDURE BurnBinaryTree_Integer
    !> @copybrief BinaryTrees::BurnBinaryTreeIndex_Index
    !> @copydetails BinaryTrees::BurnBinaryTreeIndex_Index
    MODULE PROCEDURE BurnBinaryTree_Index
  ENDINTERFACE BurnBinaryTree

  !> Generif interface for creating a binary list from a sorted list
  INTERFACE SortedBinaryTree
    !> @copybrief BinaryTrees::SortedBinaryTreeIndex_Index
    !> @copydetails BinaryTrees::SortedBinaryTreeIndex_Index
    MODULE PROCEDURE SortedBinaryTree_Index
  ENDINTERFACE SortedBinaryTree
!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief Creates a dense binary tree from a sorted list.
!> @param t an unallocated binary tree
!> @param sorted_list a sorted list of integers
!>
!> This routine is recursive.
   RECURSIVE SUBROUTINE SortedBinaryTree_Index(t,stt,sorted_list)
     TYPE(BinaryTreeType_Index),POINTER :: t
     INTEGER(SIK),INTENT(IN) :: stt
     INTEGER(SIK),INTENT(IN) :: sorted_list(:)
     INTEGER(SIK) :: p,n
     IF(.NOT.ASSOCIATED(t)) THEN
       ALLOCATE(t)
       n=SIZE(sorted_list)
       p=n/2+1
       t%index=stt+p-1
       t%val=sorted_list(p)
       IF(p > 1) CALL SortedBinaryTree_Index(t%left,stt,sorted_list(1:p-1))
       IF(p+1 <= n) CALL SortedBinaryTree_Index(t%right,t%index+1, &
         sorted_list(p+1:SIZE(sorted_list)))
     ENDIF
   ENDSUBROUTINE SortedBinaryTree_Index
!
!-------------------------------------------------------------------------------
!> @brief Constructs a binary tree from a list of integers
!> @param t the binary tree to be created
!> @param list the list of integers to put into the tree
!>
!>
    SUBROUTINE CreateBinaryTree_Integer(t,list)
      TYPE(BinaryTreeType_Integer),POINTER :: t
      INTEGER(SIK),INTENT(IN) :: list(:)
      INTEGER(SIK) :: i

      IF(.NOT.ASSOCIATED(t)) THEN
        DO i=1,SIZE(list)
          CALL InsertBinaryTreeNode_Integer(t,list(i))
        ENDDO
      ENDIF
    ENDSUBROUTINE CreateBinaryTree_Integer
!
!-------------------------------------------------------------------------------
!> @brief Constructs a binary tree from a list of integers
!> @param t the binary tree to be created
!> @param list the list of integers to put into the tree
!>
!>
    SUBROUTINE CreateBinaryTreeRmDup_Integer(t,list)
      TYPE(BinaryTreeType_Integer),POINTER :: t
      INTEGER(SIK),INTENT(IN) :: list(:)
      INTEGER(SIK) :: i

      IF(.NOT.ASSOCIATED(t)) THEN
        DO i=1,SIZE(list)
          CALL InsertBinaryTreeNodeRmDup_Integer(t,list(i))
        ENDDO
      ENDIF
    ENDSUBROUTINE CreateBinaryTreeRmDup_Integer
!
!-------------------------------------------------------------------------------
!> @brief Recursive subroutine inserts a new node into the binary tree.
!> @param t the current node in the binary tree
!> @param number the number be inserted in the node
!>
!> Recursive subroutines are really bad for trees with a very high depth because
!> a new stack is allocated each level down the tree. This creates all kinds of
!> performance issues and so it should be replaced by a non-recursive routine
!> to traverse the tree. However, it should be ok for initial use.
    RECURSIVE SUBROUTINE InsertBinaryTreeNode_Integer(t,number)
      TYPE(BinaryTreeType_Integer),POINTER :: t
      INTEGER(SIK),INTENT(IN) :: number

      !If (sub)tree is empty, put number at root
      IF(.NOT.ASSOCIATED(t)) THEN
        ALLOCATE(t)
        t%val=number
      !otherwise, insert into correct subtree
      ELSEIF(number < t%val) THEN
        CALL InsertBinaryTreeNode_Integer(t%left,number)
      ELSE
        CALL InsertBinaryTreeNode_Integer(t%right,number)
      ENDIF
    ENDSUBROUTINE InsertBinaryTreeNode_Integer
!
!-------------------------------------------------------------------------------
!> @brief Recursive subroutine inserts a new node into the binary tree.
!> @param t the current node in the binary tree
!> @param number the number be inserted in the node
!>
!> Recursive subroutines are really bad for trees with a very high depth because
!> a new stack is allocated each level down the tree. This creates all kinds of
!> performance issues and so it should be replaced by a non-recursive routine
!> to traverse the tree. However, it should be ok for initial use.
    RECURSIVE SUBROUTINE InsertBinaryTreeNodeRmDup_Integer(t,number)
      TYPE(BinaryTreeType_Integer),POINTER :: t
      INTEGER(SIK),INTENT(IN) :: number

      !If (sub)tree is empty, put number at root
      IF(.NOT.ASSOCIATED(t)) THEN
        ALLOCATE(t)
        t%val=number
      ELSEIF(number == t%val) THEN
      !Abandon it if the same
      !otherwise, insert into correct subtree
      ELSEIF(number < t%val) THEN
        CALL InsertBinaryTreeNodeRmDup_Integer(t%left,number)
      ELSE
        CALL InsertBinaryTreeNodeRmDup_Integer(t%right,number)
      ENDIF
    ENDSUBROUTINE InsertBinaryTreeNodeRmDup_Integer
!
!-------------------------------------------------------------------------------
!> @brief Recursive function returns if a number is found in the tree
!> @param t the binary to be searched
!> @param val the value to search for in the tree
!> @returns match logical whether or not the value was found in the tree
    PURE RECURSIVE FUNCTION SearchBinaryTree_Integer(t,val) RESULT(match)
      CLASS(BinaryTreeType_Integer),INTENT(IN) :: t
      INTEGER(SIK),INTENT(IN) :: val
      LOGICAL(SBK) :: match

      match=.FALSE.
      IF(val == t%val) THEN
        match=.TRUE.
      ELSEIF(val < t%val) THEN
        IF(ASSOCIATED(t%left)) match=SearchBinaryTree_Integer(t%left,val)
      ELSEIF(val > t%val) THEN
        IF(ASSOCIATED(t%right)) match=SearchBinaryTree_Integer(t%right,val)
      ENDIF
    ENDFUNCTION SearchBinaryTree_Integer
!
!-------------------------------------------------------------------------------
!> @brief Recursive subroutine deallocates a binary tree
!> @param t the binary tree to be deallocated
    RECURSIVE SUBROUTINE BurnBinaryTree_Integer(t)
      TYPE(BinaryTreeType_Integer),POINTER :: t
      IF(ASSOCIATED(t)) THEN
        IF(ASSOCIATED(t%left)) CALL BurnBinaryTree_Integer(t%left)
        IF(ASSOCIATED(t%right)) CALL BurnBinaryTree_Integer(t%right)
        DEALLOCATE(t)
      ENDIF
    ENDSUBROUTINE BurnBinaryTree_Integer
!
!-------------------------------------------------------------------------------
!> @brief Constructs a binary tree from a list of integers and indices
!> @param t the binary tree to be created
!> @param list the list of integers to put into the tree
!> @param n1 optional, the starting index
!> @param n2 optional, the ending index
!>
!> This is useful for searching lists with non-sequential, and possibly
!> unordered values.
    SUBROUTINE CreateBinaryTree_Index(t,list,n1,n2)
      TYPE(BinaryTreeType_Index),POINTER :: t
      INTEGER(SIK),INTENT(IN) :: list(:)
      INTEGER(SIK),INTENT(IN),OPTIONAL :: n1,n2
      INTEGER(SIK) :: i,j,stt,stp

      IF(PRESENT(n1)) THEN
        stt=n1
      ELSE
        stt=1
      ENDIF
      IF(PRESENT(n2)) THEN
        stp=n2
      ELSE
        stp=SIZE(list)
      ENDIF
      IF(stp-stt+1 == SIZE(list) .AND. .NOT.ASSOCIATED(t)) THEN
        j=0
        DO i=stt,stp
          j=j+1
          CALL InsertBinaryTreeNode_Index(t,list(j),i)
        ENDDO
      ENDIF
    ENDSUBROUTINE CreateBinaryTree_Index
!
!-------------------------------------------------------------------------------
!> @brief Constructs a binary tree from a list of integers and indices
!> @param t the binary tree to be created
!> @param list the list of integers to put into the tree
!> @param n1 optional, the starting index
!> @param n2 optional, the ending index
!>
!> This is useful for searching lists with non-sequential, and possibly
!> unordered values.
    SUBROUTINE CreateBinaryTreeRmDup_Index(t,list,n1,n2)
      TYPE(BinaryTreeType_Index),POINTER :: t
      INTEGER(SIK),INTENT(IN) :: list(:)
      INTEGER(SIK),INTENT(IN),OPTIONAL :: n1,n2
      INTEGER(SIK) :: i,j,stt,stp

      IF(PRESENT(n1)) THEN
        stt=n1
      ELSE
        stt=1
      ENDIF
      IF(PRESENT(n2)) THEN
        stp=n2
      ELSE
        stp=SIZE(list)
      ENDIF
      IF(stp-stt+1 == SIZE(list) .AND. .NOT.ASSOCIATED(t)) THEN
        j=0
        DO i=stt,stp
          j=j+1
          CALL InsertBinaryTreeNodeRmDup_Index(t,list(j),i)
        ENDDO
      ENDIF
    ENDSUBROUTINE CreateBinaryTreeRmDup_Index
!
!-------------------------------------------------------------------------------
!> @brief Recursive subroutine inserts a new node into the binary tree.
!> @param t the current node in the binary tree
!> @param number the number be inserted in the node
!> @param index the index of the number to be inserted
!>
!> Recursive subroutines are really bad for trees with a very high depth because
!> a new stack is allocated each level down the tree. This creates all kinds of
!> performance issues and so it should be replaced by a non-recursive routine
!> to traverse the tree. However, it should be ok for initial use.
    RECURSIVE SUBROUTINE InsertBinaryTreeNode_Index(t,number,index)
      TYPE(BinaryTreeType_Index),POINTER :: t
      INTEGER(SIK),INTENT(IN) :: number,index

      !If (sub)tree is empty, put number at root
      IF(.NOT.ASSOCIATED(t)) THEN
        ALLOCATE(t)
        t%val=number
        t%index=index
      !otherwise, insert into correct subtree
      ELSEIF(number < t%val) THEN
        CALL InsertBinaryTreeNode_Index(t%left,number,index)
      ELSE
        CALL InsertBinaryTreeNode_Index(t%right,number,index)
      ENDIF
    ENDSUBROUTINE InsertBinaryTreeNode_Index
!
!-------------------------------------------------------------------------------
!> @brief Recursive subroutine inserts a new node into the binary tree.
!> @param t the current node in the binary tree
!> @param number the number be inserted in the node
!> @param index the index of the number to be inserted
!>
!> Recursive subroutines are really bad for trees with a very high depth because
!> a new stack is allocated each level down the tree. This creates all kinds of
!> performance issues and so it should be replaced by a non-recursive routine
!> to traverse the tree. However, it should be ok for initial use.
    RECURSIVE SUBROUTINE InsertBinaryTreeNodeRmDup_Index(t,number,index)
      TYPE(BinaryTreeType_Index),POINTER :: t
      INTEGER(SIK),INTENT(IN) :: number,index

      !If (sub)tree is empty, put number at root
      IF(.NOT.ASSOCIATED(t)) THEN
        ALLOCATE(t)
        t%val=number
        t%index=index
      ELSEIF(number == t%val) THEN
      !Abandon it if it is the same
      !otherwise, insert into correct subtree
      ELSEIF(number < t%val) THEN
        CALL InsertBinaryTreeNodeRmDup_Index(t%left,number,index)
      ELSE
        CALL InsertBinaryTreeNodeRmDup_Index(t%right,number,index)
      ENDIF
    ENDSUBROUTINE InsertBinaryTreeNodeRmDup_Index
!
!-------------------------------------------------------------------------------
!> @brief Recursive function returns if a number is found in the tree
!> @param t the binary to be searched
!> @param val the value to search for in the tree
!> @returns match logical whether or not the value was found in the tree
    PURE RECURSIVE FUNCTION SearchBinaryTree_Index(t,val) RESULT(match)
      CLASS(BinaryTreeType_Index),INTENT(IN) :: t
      INTEGER(SIK),INTENT(IN) :: val
      LOGICAL(SBK) :: match

      match=.FALSE.
      IF(val == t%val) THEN
        match=.TRUE.
      ELSEIF(val < t%val) THEN
        IF(ASSOCIATED(t%left)) match=SearchBinaryTree_Index(t%left,val)
      ELSEIF(val > t%val) THEN
        IF(ASSOCIATED(t%right)) match=SearchBinaryTree_Index(t%right,val)
      ENDIF
    ENDFUNCTION SearchBinaryTree_Index
!
!-------------------------------------------------------------------------------
!> @brief Recursive function returns the index of an array if a value is found
!> @param t the binary to be searched
!> @param val the value to search for in the tree
!> @returns index of value in some other array
    PURE RECURSIVE FUNCTION getBinaryTree_Index(t,val) RESULT(index)
      CLASS(BinaryTreeType_Index),INTENT(IN) :: t
      INTEGER(SIK),INTENT(IN) :: val
      INTEGER(SIK) :: index

      index=-1
      IF(val == t%val) THEN
        index=t%index
      ELSEIF(val < t%val) THEN
        IF(ASSOCIATED(t%left)) index=getBinaryTree_Index(t%left,val)
      ELSEIF(val > t%val) THEN
        IF(ASSOCIATED(t%right)) index=getBinaryTree_Index(t%right,val)
      ENDIF
    ENDFUNCTION getBinaryTree_Index
!
!-------------------------------------------------------------------------------
!> @brief Recursive subroutine deallocates a binary tree with indices
!> @param t the binary tree to be deallocated
    RECURSIVE SUBROUTINE BurnBinaryTree_Index(t)
      TYPE(BinaryTreeType_Index),POINTER :: t
      IF(ASSOCIATED(t)) THEN
        IF(ASSOCIATED(t%left)) CALL BurnBinaryTree_Index(t%left)
        IF(ASSOCIATED(t%right)) CALL BurnBinaryTree_Index(t%right)
        DEALLOCATE(t)
      ENDIF
    ENDSUBROUTINE BurnBinaryTree_Index
!
ENDMODULE BinaryTrees
