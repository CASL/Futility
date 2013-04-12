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
!> @brief Provides Morton order (also known as Z-order) space filling curve 
!> functions and data structures.
!> 
!> Space filling curves are useful in computer science for converting a 
!> multi-dimensional array into a 1-D array. There are several types of space
!> filling curves (e.g. Peano, Sirpinksy, Hilbert, Z-order). One space filling
!> curve comes from Morton ordering or Z-ordering or bit flipping. This is 
!> the type of space filling curve provided by this module.
!>
!> It provides a generic interface to functions that return the Morton index
!> in 2-D or 3-D. It also provides a tree like data structure called a "Z"-Tree
!> which can be used to recursively partition a cuboid domain defined by
!> starting and ending indices in the x-, y-, and z-dimension.
!>
!> For integer coordinates X and Y if X and Y are represented in binary form
!> e.g. for 2-bits (x2,x1) and (y2,y1) then the binary form of the Morton index
!> is computed as (y2,x2,y1,x1).
!>
!> @par Module Dependencies
!>  - @ref IntrType "IntrType": @copybrief IntrType
!> 
!> @author Brendan Kochunas
!>   @date 11/10/2010
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE MortonOrdering

  USE IntrType
  IMPLICIT NONE
  PRIVATE

  PUBLIC :: MortonIndex
  PUBLIC :: ZTreeNodeType
  
  !> Generic interface for computing the Morton index 
  INTERFACE MortonIndex
    !> @copybrief MortonOrdering::Morton2D
    !> @copydetails MortonOrdering::Morton2D
    MODULE PROCEDURE Morton2D
    !> @copybrief MortonOrdering::Morton3D
    !> @copydetails MortonOrdering::Morton3D
    MODULE PROCEDURE Morton3D
  ENDINTERFACE
  
  !> A tree data structure that can be used to recursively setup and partition
  !> a grid with Morton ordering (or near morton ordering).
  !>
  !> It is useful for partitioning since each subdomain will have contiguous
  !> numbering. For domains with uniform dimensions of sizes 2^n the ordering
  !> produced is identical to Morton Ordering. For other domain sizes it will
  !> attempt to divide the domain in 2 along each dimension, giving preference
  !> to the dimensions with longer aspect ratios.
  !>
  TYPE :: ZTreeNodeType
    !> The starting and stopping indices for the x-dimension for this node.
    INTEGER(SIK),DIMENSION(2) :: x=0
    !> The starting and stopping indices for the y-dimension for this node.
    INTEGER(SIK),DIMENSION(2) :: y=0
    !> The starting and stopping indices for the z-dimension for this node.
    INTEGER(SIK),DIMENSION(2) :: z=0
    !> The starting 1-D index for the domain defined on this node
    INTEGER(SIK) :: istt=-1
    !> The stopping 1-D index for the domain defined on this node
    INTEGER(SIK) :: istp=-1
    !> The number of subdomains this node has been divided into
    INTEGER(SIK) :: nsubdomains=0
    !> The subdomains for the domain defined by this node
    TYPE(ZTreeNodeType),POINTER :: subdomains(:) => NULL()
    CONTAINS
      !> @copybrief MortonOrdering::ZTree_Create
      !> @copydetails MortonOrdering::ZTree_Create
      PROCEDURE,PASS :: init => ZTree_Create
      !> @copybrief MortonOrdering::ZTree_Burn
      !> @copydetails MortonOrdering::ZTree_Burn
      PROCEDURE,PASS :: clear => ZTree_Burn
      !> @copybrief MortonOrdering::ZTree_istpMax
      !> @copydetails MortonOrdering::ZTree_istpMax
      PROCEDURE,PASS :: istpMax => ZTree_istpMax
      !> @copybrief MortonOrdering::ZTree_ijk_to_1D
      !> @copydetails MortonOrdering::ZTree_ijk_to_1D
      PROCEDURE,PASS :: ijk2oneD => ZTree_ijk_to_1D
      !> @copybrief MortonOrdering::ZTree_1D_to_ijk
      !> @copydetails MortonOrdering::ZTree_1D_to_ijk
      PROCEDURE,PASS :: oneD2ijk => ZTree_1D_to_ijk
      !> @copybrief MortonOrdering::ZTree_getMaxLevels
      !> @copydetails MortonOrdering::ZTree_getMaxLevels
      PROCEDURE,PASS :: getMaxLevels => ZTree_getMaxLevels
      !> @copybrief MortonOrdering::ZTree_getNDomains
      !> @copydetails MortonOrdering::ZTree_getNDomains
      PROCEDURE,PASS :: getNDomains => ZTree_getNDomains
      !> @copybrief MortonOrdering::ZTree_getSubNodePointer
      !> @copydetails MortonOrdering::ZTree_getSubNodePointer
      PROCEDURE,PASS :: getSubNodePointer => ZTree_getSubNodePointer
      !> @copybrief MortonOrdering::ZTree_getLeafNodePointer
      !> @copydetails MortonOrdering::ZTree_getLeafNodePointer
      PROCEDURE,PASS :: getLeafNodePointer => ZTree_getLeafNodePointer
      !> @copybrief MortonOrdering::ZTree_flattenLeafs
      !> @copydetails MortonOrdering::ZTree_flattenLeafs
      PROCEDURE,PASS :: flattenLeafs => ZTree_flattenLeafs
      !> @copybrief MortonOrdering::ZTree_addToLeafs
      !> @copydetails MortonOrdering::ZTree_addToLeafs
      PROCEDURE,PASS :: addToLeafs => ZTree_addToLeafs
      !> @copybrief MortonOrdering::ZTree_getSubNodeBounds
      !> @copydetails MortonOrdering::ZTree_getSubNodeBounds
      PROCEDURE,PASS :: getSubNodeBounds => ZTree_getSubNodeBounds
      !> @copybrief MortonOrdering::ZTree_Partition
      !> @copydetails MortonOrdering::ZTree_Partition
      PROCEDURE,PASS :: partition => ZTree_Partition
      !> @copybrief MortonOrdering::ZTree_Renumber
      !> @copydetails MortonOrdering::ZTree_Renumber
      PROCEDURE,PASS :: renumber => ZTree_Renumber
      !> @copybrief MortonOrdering::ZTree_Shave
      !> @copydetails MortonOrdering::ZTree_Shave
      PROCEDURE,PASS :: shave => ZTree_Shave
  ENDTYPE ZTreeNodeType
!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief Computes the 1-D Morton index for integer coordinates x and y in 2-D
!> space.
!> @param x the x-coordinate (integer)
!> @param y the y-coordinate (integer)
!> @returns im the morton index
!>
    ELEMENTAL FUNCTION Morton2D(x,y) RESULT(im)
      INTEGER(SIK),INTENT(IN) :: x,y
      INTEGER(SIK) :: im
      INTEGER(SIK) :: i,j
      INTEGER(SIK) :: xbin(32),ybin(32),indexbin(64)
      
      xbin=int2bin(x)
      ybin=int2bin(y)
      j=1_SIK
      DO i=1_SIK,32_SIK
        indexbin(j)=xbin(i)
        indexbin(j+1_SIK)=ybin(i)
        j=j+2_SIK
      ENDDO
      im=bin2int(indexbin)
    ENDFUNCTION Morton2D
!
!-------------------------------------------------------------------------------
!> @brief Computes the 1-D Morton index for integer coordinates x, y, and z in
!> 3-D space.
!> @param x the x-coordinate (integer)
!> @param y the y-coordinate (integer)
!> @param z the z-coordinate (integer)
!> @returns im the morton index
!>
    ELEMENTAL FUNCTION Morton3D(x,y,z) RESULT(im)
      INTEGER(SIK),INTENT(IN) :: x,y,z
      INTEGER(SIK) :: im
      INTEGER(SIK) :: i,j
      INTEGER(SIK) :: xbin(32),ybin(32),zbin(32),indexbin(96)
      
      xbin=int2bin(x)
      ybin=int2bin(y)
      zbin=int2bin(z)
      j=1_SIK
      DO i=1_SIK,32_SIK
        indexbin(j)=xbin(i)
        indexbin(j+1_SIK)=ybin(i)
        indexbin(j+2_SIK)=zbin(i)
        j=j+3_SIK
      ENDDO
      im=bin2int(indexbin)
    ENDFUNCTION Morton3D
!
!-------------------------------------------------------------------------------
!> Converts a 32-bit integer into binary as an array of integers.
!> @param int the integer to convert to binary
!> @returns bin an array of 0's and 1's which is the binary form of int
!>
    PURE FUNCTION int2bin(int) RESULT(bin)
      INTEGER(SIK),INTENT(IN) :: int
      INTEGER(SIK) :: bin(32)
      INTEGER(SIK) :: i,iint
      
      bin=0_SIK
      iint=int
      DO i=1,32
        bin(i)=MOD(iint,2_SIK)
        iint=iint/2_SIK
      ENDDO
    ENDFUNCTION int2bin
!
!-------------------------------------------------------------------------------
!> Converts a binary number into an integer
!> @param bin the binary representation of an integer (array of 0's and 1's)
!> @returns int the integer in base 10 numbering
!>
    PURE FUNCTION bin2int(bin) RESULT(int)
      INTEGER(SIK),INTENT(IN) :: bin(:)
      INTEGER(SIK) :: int
      INTEGER(SIK) :: i,iord
      
      int=0_SIK
      iord=1_SIK
      DO i=1,SIZE(bin)
        int=int+bin(i)*iord
        iord=iord+iord
      ENDDO
    ENDFUNCTION bin2int
!
!-------------------------------------------------------------------------------
!> @brief Creates a "Z"-Tree using the bounds of a rectilinear grid
!> @param thisZTreeNode a "Z"-Tree node object to initialize
!> @param x1 the starting x index for the domain on this node (x1 > 0)
!> @param x2 the stopping x index for the domain on this node (x2 >= x1)
!> @param y1 the starting y index for the domain on this node (y1 > 0)
!> @param y2 the stopping y index for the domain on this node (y2 >= y1)
!> @param z1 the starting z index for the domain on this node (z1 > 0)
!> @param z2 the stopping z index for the domain on this node (z2 >= z1)
!> @param istt the starting global 1-D index for this node (istt >= 0)
!>
!> This routine sets all the attributes for the given node and will then try to
!> subdivide the domain on this node into subdomains (and recursively initialize
!> the subdomains). When splitting a domain it first finds the dimension with
!> the least extent and computes the aspect ratios relative to this. If any 
!> aspect ratios are greater than 2 (there is at most 2 and at least 0), then
!> only these domains will be split. This is so the tree preferentially splits
!> towards cubic domains. If no aspect ratios are above 2 then all dimensions
!> are split. Only dimensions with size greater than 1 are split because 1
!> cannot be split.
!> 
    PURE RECURSIVE SUBROUTINE ZTree_Create(thisZTreeNode,x1,x2,y1,y2,z1,z2,istt)
      CLASS(ZTreeNodeType),INTENT(INOUT) :: thisZTreeNode
      INTEGER(SIK),INTENT(IN) :: x1,x2
      INTEGER(SIK),INTENT(IN) :: y1,y2
      INTEGER(SIK),INTENT(IN) :: z1,z2
      INTEGER(SIK),INTENT(IN) :: istt
      LOGICAL(SBK) :: splitX,splitY,splitZ
      INTEGER(SIK) :: nx,ny,nz
      INTEGER(SIK) :: id,idstt,ix,iy,iz,ndx,ndy,ndz,nsmall
      INTEGER(SIK),DIMENSION(2) :: nxdstt,nxdstp,nydstt,nydstp,nzdstt,nzdstp
      REAL(SRK) :: rx,ry,rz
      
      !Check for valid input
      IF(.NOT.(istt < 0 .OR. x2 < x1 .OR. y2 < y1 .OR. z2 < z1 .OR. &
        x1 < 1 .OR. y1 < 1 .OR. z1 < 1 .OR. thisZTreeNode%istt /= -1)) THEN
      
        !Assign values to this node based on inputs
        thisZTreeNode%istt=istt
        thisZTreeNode%x(1)=x1
        thisZTreeNode%x(2)=x2
        thisZTreeNode%y(1)=y1
        thisZTreeNode%y(2)=y2
        thisZTreeNode%z(1)=z1
        thisZTreeNode%z(2)=z2
        nx=thisZTreeNode%x(2)-thisZTreeNode%x(1)+1
        ny=thisZTreeNode%y(2)-thisZTreeNode%y(1)+1
        nz=thisZTreeNode%z(2)-thisZTreeNode%z(1)+1
      
        !Determine along which dimensions to split the domain based on aspect ratio
        nsmall=MIN(nx,ny)
        nsmall=MIN(nsmall,nz)
        rx=REAL(nx,SRK)/REAL(nsmall,SRK)
        ry=REAL(ny,SRK)/REAL(nsmall,SRK)
        rz=REAL(nz,SRK)/REAL(nsmall,SRK)
        splitX=(rx > 2._SRK-EPSREAL)
        splitY=(ry > 2._SRK-EPSREAL)
        splitZ=(rz > 2._SRK-EPSREAL)
      
        IF(.NOT.(splitX .OR. splitY .OR. splitZ)) THEN
          !Aspect ratio is good for all dimensions, so split in all 3 dimensions if possible
          splitX=(nx > 1)
          splitY=(ny > 1)
          splitZ=(nz > 1)
        ENDIF
      
        !Determine the number of subdomains to divide this domain into
        !and the sizes of each subdomain
        nxdstt(1)=thisZTreeNode%x(1)
        nydstt(1)=thisZTreeNode%y(1)
        nzdstt(1)=thisZTreeNode%z(1)
        IF(splitX) THEN
          !Divide x into 2 subdomains
          ndx=2
          nxdstp(1)=nxdstt(1)+nx/2-1
          nxdstt(2)=nxdstp(1)+1
          nxdstp(2)=thisZTreeNode%x(2)
        ELSE
          !Do not divide domain along x
          ndx=1
          nxdstp(1)=thisZTreeNode%x(2)
          nxdstt(2)=0
          nxdstp(2)=0
        ENDIF
        IF(splitY) THEN
          !Divide y into 2 subdomains
          ndy=2
          nydstp(1)=nydstt(1)+ny/2-1
          nydstt(2)=nydstp(1)+1
          nydstp(2)=thisZTreeNode%y(2)
        ELSE
          !Do not divide domain along y
          ndy=1
          nydstp(1)=thisZTreeNode%y(2)
          nydstt(2)=0
          nydstp(2)=0
        ENDIF
        IF(splitZ) THEN
          !Divide z into 2 subdomains
          ndz=2
          nzdstp(1)=nzdstt(1)+nz/2-1
          nzdstt(2)=nzdstp(1)+1
          nzdstp(2)=thisZTreeNode%z(2)
        ELSE
          !Do not divide domain along z
          ndz=1
          nzdstp(1)=thisZTreeNode%z(2)
          nzdstt(2)=0
          nzdstp(2)=0
        ENDIF
        thisZTreeNode%nsubdomains=ndx*ndy*ndz
        
        idstt=thisZTreeNode%istt
        IF(thisZTreeNode%nsubdomains > 1) THEN
          !Allocate the subdomains
          ALLOCATE(thisZTreeNode%subdomains(thisZTreeNode%nsubdomains))
        
          !Assign sizes and indices to each subdomain
          id=0
          DO iz=1,ndz
            DO iy=1,ndy
              DO ix=1,ndx
                id=id+1
                CALL thisZTreeNode%subdomains(id)%init(nxdstt(ix),nxdstp(ix), &
                  nydstt(iy),nydstp(iy),nzdstt(iz),nzdstp(iz),idstt)
                idstt=thisZTreeNode%subdomains(id)%istpMax()+1
              ENDDO
            ENDDO
          ENDDO
          idstt=idstt-1
        ELSE
          !Domain is 1x1x1 and cannot be further sub-divided
          thisZTreeNode%nsubdomains=0
        ENDIF
        thisZTreeNode%istp=idstt
      ENDIF
    ENDSUBROUTINE ZTree_Create
!
!-------------------------------------------------------------------------------
!> @brief Clears a "Z"-Tree object
!> @param thisZTreeNode a "Z"-Tree node object to clear
!>
!> If @c thisZTreeNode has subdomains then those subdomains are cleared 
!> by recursively calling this routine.
!> 
    PURE RECURSIVE SUBROUTINE ZTree_Burn(thisZTreeNode)
      CLASS(ZTreeNodeType),INTENT(INOUT) :: thisZTreeNode
      INTEGER(SIK) :: i
      
      IF(ASSOCIATED(thisZTreeNode%subdomains)) THEN
        DO i=thisZTreeNode%nsubdomains,1,-1
          CALL thisZTreeNode%subdomains(i)%clear()
        ENDDO
        DEALLOCATE(thisZTreeNode%subdomains)
      ENDIF
      thisZTreeNode%x=0
      thisZTreeNode%y=0
      thisZTreeNode%z=0
      thisZTreeNode%istt=-1
      thisZTreeNode%istp=-1
      thisZTreeNode%nsubdomains=0
    ENDSUBROUTINE ZTree_Burn
!
!-------------------------------------------------------------------------------
!> @brief Computes the stopping index for the given domain defined on a "Z"-tree
!> node.
!> @param thisZTreeNode a "Z"-Tree node object to get the stopping index for
!> @returns istp the stopping index for the node
!>
    PURE FUNCTION ZTree_istpMax(thisZTreeNode) RESULT(istp)
      CLASS(ZTreeNodeType),INTENT(IN) :: thisZTreeNode
      INTEGER(SIK) :: istp
      istp=-1
      IF(.NOT.thisZTreeNode%istt == -1) istp=thisZTreeNode%istt-1+ &
        (thisZTreeNode%x(2)-thisZTreeNode%x(1)+1)* &
          (thisZTreeNode%y(2)-thisZTreeNode%y(1)+1)* &
            (thisZTreeNode%z(2)-thisZTreeNode%z(1)+1)
    ENDFUNCTION ZTree_istpMax
!
!-------------------------------------------------------------------------------
!> @brief Returns the global index within the tree for a 1x1x1 domain defined by
!> its @c i, @c j, @c k coordinates in the grid.
!> @param thisZTreeNode a "Z"-Tree node object to the index for
!> @param i the x-coordinate in the grid
!> @param j the y-coordinate in the grid
!> @param k the z-coordinate in the grid
!> @returns index the global index within the grid
!> 
    PURE RECURSIVE FUNCTION ZTree_ijk_to_1D(thisZTreeNode,i,j,k) RESULT(index)
      CLASS(ZTreeNodeType),INTENT(IN) :: thisZTreeNode
      INTEGER(SIK),INTENT(IN) :: i
      INTEGER(SIK),INTENT(IN) :: j
      INTEGER(SIK),INTENT(IN) :: k
      INTEGER(SIK) :: index
      INTEGER(SIK) :: id
      
      index=-1
      IF(thisZTreeNode%x(1) <= i .AND. i <= thisZTreeNode%x(2) .AND. &
         thisZTreeNode%y(1) <= j .AND. j <= thisZTreeNode%y(2) .AND. &
         thisZTreeNode%z(1) <= k .AND. k <= thisZTreeNode%z(2)) THEN
        IF(thisZTreeNode%nsubdomains == 0) THEN
          index=thisZTreeNode%istt
        ELSE
          DO id=1,thisZTreeNode%nsubdomains
            IF(thisZTreeNode%subdomains(id)%x(1) <= i .AND. &
              i <= thisZTreeNode%subdomains(id)%x(2) .AND. &
                thisZTreeNode%subdomains(id)%y(1) <= j .AND. &
                  j <= thisZTreeNode%subdomains(id)%y(2) .AND. &
                    thisZTreeNode%subdomains(id)%z(1) <= k .AND. &
                      k <= thisZTreeNode%subdomains(id)%z(2)) THEN
              index=thisZTreeNode%subdomains(id)%ijk2oneD(i,j,k)
              EXIT
            ENDIF
          ENDDO
        ENDIF
      ENDIF
    ENDFUNCTION ZTree_ijk_to_1D
!
!-------------------------------------------------------------------------------
!> @brief Returns the @c i, @c j, @c k coordinates of a node given its
!> global index within the grid.
!> @param thisZTreeNode a Z-Tree node object to search for @c i, @c j, @c k in
!> @param index the global index within the grid to use to obtain @c i, @c j, 
!>        @c k
!> @param i (output) the x-coordinate in the grid
!> @param j (output) the y-coordinate in the grid
!> @param k (output) the z-coordinate in the grid
!>
    PURE RECURSIVE SUBROUTINE ZTree_1D_to_ijk(thisZTreeNode,index,i,j,k)
      CLASS(ZTreeNodeType),INTENT(IN) :: thisZTreeNode
      INTEGER(SIK),INTENT(IN) :: index
      INTEGER(SIK),INTENT(OUT) :: i
      INTEGER(SIK),INTENT(OUT) :: j
      INTEGER(SIK),INTENT(OUT) :: k
      INTEGER(SIK) :: id
      i=-1
      j=-1
      k=-1
      IF(thisZTreeNode%istt <= index .AND. index <= thisZTreeNode%istp) THEN
        IF(thisZTreeNode%nsubdomains == 0 .AND. thisZTreeNode%istt >= 0) THEN
          i=thisZTreeNode%x(1)
          j=thisZTreeNode%y(1)
          k=thisZTreeNode%z(1)
        ELSE
          DO id=1,thisZTreeNode%nsubdomains
            IF(thisZTreeNode%subdomains(id)%istt <= index .AND. &
                index <= thisZTreeNode%subdomains(id)%istp) THEN
              CALL thisZTreeNode%subdomains(id)%oneD2ijk(index,i,j,k)
              EXIT
            ENDIF
          ENDDO
        ENDIF
      ENDIF
    ENDSUBROUTINE ZTree_1D_to_ijk
!
!-------------------------------------------------------------------------------
!> @brief Returns the maximum number of levels in the Z-Tree
!> @param thisZTreeNode the maximum number of levels in the tree below this node
!> @param il the starting level index
!> @returns nl the maximum number of levels below this node
!>
!> There is probably an alternative way to compute this parameter that does not
!> involve a recursive procedure. It would likely be based on taking the
!> log-base 8 of total number of nodes in the tree, but I'm not sure if this
!> will work in all cases because the splitting at each level may be any number
!> between 2 and 8. I'm also not sure what happens if we start to shave parts
!> of the tree.
!>
    PURE RECURSIVE FUNCTION ZTree_getMaxLevels(thisZTreeNode,il) RESULT(nl)
      CLASS(ZTreeNodeType),INTENT(IN) :: thisZTreeNode
      INTEGER(SIK),INTENT(IN) :: il
      INTEGER(SIK) :: nl
      INTEGER(SIK) :: id,nd
      
      nl=-1
      IF(thisZTreeNode%istt /= -1 .AND. il >= 0) THEN
        nl=il
        IF(thisZTreeNode%nsubdomains > 0) THEN
          DO id=1,thisZTreeNode%nsubdomains
            nd=thisZTreeNode%subdomains(id)%getMaxLevels(il+1)
            nl=MAX(nl,nd)
          ENDDO
        ENDIF
      ENDIF
    ENDFUNCTION ZTree_getMaxLevels
!
!-------------------------------------------------------------------------------
!> @brief Returns the number of nodes at a given level of the tree.
!> @param thisZTreeNode the "Z"-Tree to be queried
!> @param il the level of the tree for which we want to count the domains
!> @returns nd the number of nodes or domains at the given level
!>
!> Note that the deepest level of the tree may not be fully filled, so 
!> frequently it will have less domains than the second deepest level.
!> For @c il=0 it refers to the root, which is counted as 1 domain/node.
!> If @c il is larger than the deepest level of the tree a 0 will be returned.
!> If the tree is fully dense and partitioned at each level then @c nd will
!> be @c 8^il, however in general the fully dense and partitioned tree is not
!> expected.
!>
    PURE RECURSIVE FUNCTION ZTree_getNDomains(thisZTreeNode,il) RESULT(nd)
      CLASS(ZTreeNodeType),INTENT(IN) :: thisZTreeNode
      INTEGER(SIK),INTENT(IN) :: il
      INTEGER(SIK) :: nd
      INTEGER(SIK) :: id
      
      nd=0
      IF(il > 0) THEN
        DO id=1,thisZTreeNode%nsubdomains
          nd=nd+thisZTreeNode%subdomains(id)%getNDomains(il-1)
        ENDDO
      ENDIF
      IF(il == 0 .AND. thisZTreeNode%istt /= -1) nd=nd+1
    ENDFUNCTION ZTree_getNDomains
!
!-------------------------------------------------------------------------------
!> @brief Returns the starting and stopping indices for a node located at 
!> coordinates specified by the level and node within level.
!> @param thisZTreeNode the node to get the subnode bounds from
!> @param il the ith level below this node
!> @param in the ith node within the level @c il
!> @param istt (output) the starting index for the subdomain
!> @param istp (output) the stopping index for the subdomain
!>
!> If the index is not valid within the level or the level is not valid or
!> the node is not initialized then @c istt and @c istp have the value -1.
!>
    PURE RECURSIVE SUBROUTINE ZTree_getSubNodeBounds(thisZTreeNode,il,in,istt,istp)
      CLASS(ZTreeNodeType),INTENT(IN) :: thisZTreeNode
      INTEGER(SIK),INTENT(IN) :: il
      INTEGER(SIK),INTENT(IN) :: in
      INTEGER(SIK),INTENT(OUT) :: istt
      INTEGER(SIK),INTENT(OUT) :: istp
      INTEGER(SIK) :: id,ndstt,ndstp
      
      istt=-1
      istp=-1
      IF(il > 0) THEN
        !Determine which subdomain to enter
        ndstt=1
        DO id=1,thisZTreeNode%nsubdomains
          ndstp=ndstt+thisZTreeNode%subdomains(id)%getNDomains(il-1)-1
          IF(ndstt <= in .AND. in <= ndstp) THEN
            !The node index lies within this subdomain
            CALL thisZTreeNode%subdomains(id)% &
              getSubNodeBounds(il-1,in-ndstt+1,istt,istp)
            EXIT
          ENDIF
          ndstt=ndstp+1
        ENDDO
      ENDIF
      IF(il == 0 .AND. in == 1) THEN
        istt=thisZTreeNode%istt
        istp=thisZTreeNode%istp
      ENDIF
  ENDSUBROUTINE ZTree_getSubNodeBounds
!
!-------------------------------------------------------------------------------
!> @brief Returns a pointer to a node located at coordinates specified by the
!> level and node within level.
!> @param thisZTreeNode the node to get the subnode bounds from
!> @param il the ith level below this node
!> @param in the ith node within the level @c il
!> @param subnode (output) the pointer to the node
!>
!> If the index is not valid within the level or the level is not valid then
!> the pointer is returned as null.
!>
!> Not actually sure if we need this, getLeafNode is really more useful I think.
!>
    RECURSIVE SUBROUTINE ZTree_getSubNodePointer(thisZTreeNode,il,in,subnode)
      CLASS(ZTreeNodeType),TARGET,INTENT(IN) :: thisZTreeNode
      INTEGER(SIK),INTENT(IN) :: il
      INTEGER(SIK),INTENT(IN) :: in
      TYPE(ZTreeNodeType),POINTER,INTENT(OUT) :: subnode
      INTEGER(SIK) :: id,ndstt,ndstp
      
      subnode => NULL()
      IF(il > 0) THEN
        !Determine which subdomain to enter
        ndstt=1
        DO id=1,thisZTreeNode%nsubdomains
          ndstp=ndstt+thisZTreeNode%subdomains(id)%getNDomains(il-1)-1
          IF(ndstt <= in .AND. in <= ndstp) THEN
            !The node index lies within this subdomain
            CALL thisZTreeNode%subdomains(id)% &
              getSubNodePointer(il-1,in-ndstt+1,subnode)
            EXIT
          ENDIF
          ndstt=ndstp+1
        ENDDO
      ENDIF
      IF(il == 0 .AND. in == 1) THEN
        SELECTTYPE(thisZTreeNode); TYPE IS(ZTreeNodeType)
          subnode => thisZTreeNode
        ENDSELECT
      ENDIF
    ENDSUBROUTINE ZTree_getSubNodePointer
!
!-------------------------------------------------------------------------------
!> @brief Returns a pointer to a leaf node specified by its global index.
!> @param thisZTreeNode the node to get the subnode bounds from
!> @param idx the leaf node's global index within the tree
!> @param leafnode (output) the pointer to the node
!>
!> If the index is not positive or within the range of the tree then the pointer
!> is returned as null.
!>
    RECURSIVE SUBROUTINE ZTree_getLeafNodePointer(thisZTreeNode,idx,leafnode)
      CLASS(ZTreeNodeType),TARGET,INTENT(IN) :: thisZTreeNode
      INTEGER(SIK),INTENT(IN) :: idx
      TYPE(ZTreeNodeType),POINTER,INTENT(OUT) :: leafnode
      INTEGER(SIK) :: id,ndstt,ndstp
      
      leafnode => NULL()
      IF(thisZTreeNode%istt <= idx .AND. idx <= thisZTreeNode%istp &
         .AND. idx > 0) THEN
        IF(idx == thisZTreeNode%istt .AND. idx == thisZTreeNode%istp) THEN
          SELECTTYPE(thisZTreeNode); TYPE IS(ZTreeNodeType)
            leafnode => thisZTreeNode
          ENDSELECT
        ELSE
          !Determine which subdomain to enter
          
          DO id=1,thisZTreeNode%nsubdomains
            ndstt=thisZTreeNode%subdomains(id)%istt
            ndstp=thisZTreeNode%subdomains(id)%istp
            IF(ndstt <= idx .AND. idx <= ndstp) THEN
              !The node index lies within this subdomain
              CALL thisZTreeNode%subdomains(id)% &
                getLeafNodePointer(idx,leafnode)
              EXIT
            ENDIF
          ENDDO
        ENDIF
      ENDIF
    ENDSUBROUTINE ZTree_getLeafNodePointer
!
!-------------------------------------------------------------------------------
!> @brief If the leafs on the tree are not all on the deepest level of the tree
!> then the last two levels of the tree are restructured so that the deepest
!> level of the tree is completely filled.
!> @param thisZTreeNode
!>
!> This only happens in grids that have prime factors other than 2.
!>
    SUBROUTINE ZTree_flattenLeafs(thisZTreeNode)
      CLASS(ZTreeNodeType),INTENT(INOUT) :: thisZTreeNode
      INTEGER(SIK) :: nlevels,id,id0,id2,idp,idshift,ip,newnd,nsubd
      INTEGER(SIK) :: x(2),y(2),z(2),istt
      TYPE(ZTreeNodeType),POINTER :: pZTree,pZTreeParent,tmpSubDomains(:)
      
      nlevels=thisZTreeNode%getMaxLevels(0)
      IF(nlevels > 0) THEN
        IF(thisZTreeNode%getNDomains(nlevels) < &
            thisZTreeNode%istpMax()-thisZTreeNode%istt+1) THEN
        
          !Restructure the last two levels of the Z-Tree
          idshift=0
          nsubd=thisZTreeNode%getNDomains(nlevels-1)
          DO id0=1,nsubd
            id=id0+idshift
            CALL thisZTreeNode%getSubNodePointer(nlevels-1,id,pZTree)
          
            IF(pZTree%nsubdomains > 0) THEN
              !This node needs to be deleted, move children to parent
              !Start by finding it's parent.
              FindParent: DO ip=1,thisZTreeNode%getNDomains(nlevels-2)
                CALL thisZTreeNode%getSubNodePointer(nlevels-2,ip,pZTreeParent)
                DO idp=1,pZTreeParent%nsubdomains
                  IF(pZTreeParent%subdomains(idp)%istt == pZTree%istt .AND. &
                    pZTreeParent%subdomains(idp)%istp == pZTree%istp) &
                    EXIT FindParent
                ENDDO
              ENDDO FindParent
            
              !Move children up to parent, preserve ordering
              newnd=pZTreeParent%nsubdomains+pZTree%nsubdomains-1
              ALLOCATE(tmpSubDomains(newnd))
            
              !From one to the this child subdomain on the parent
              DO id2=1,idp-1
                tmpSubDomains(id2)=pZTreeParent%subdomains(id2)
                CALL pZTreeParent%subdomains(id2)%clear()
              ENDDO
            
              !Then all the subdomains on the child
              DO id2=idp,idp+pZTree%nsubdomains-1
                tmpSubDomains(id2)=pZTree%subdomains(id2-idp+1)
              ENDDO
            
              !All the subdomains on the parent that come after the child
              DO id2=idp+pZTree%nsubdomains,newnd
                x=pZTreeParent%subdomains(id2-pZTree%nsubdomains+1)%x
                y=pZTreeParent%subdomains(id2-pZTree%nsubdomains+1)%y
                z=pZTreeParent%subdomains(id2-pZTree%nsubdomains+1)%z
                istt=pZTreeParent%subdomains(id2-pZTree%nsubdomains+1)%istt
                
                !Use init because these subdomains may have multiple levels
                CALL tmpSubDomains(id2)%init(x(1),x(2),y(1),y(2),z(1),z(2),istt)
                CALL pZTreeParent%subdomains(id2-pZTree%nsubdomains+1)%clear()
              ENDDO
              
              !The number of domains on level nlevel-1 has changed, so on
              !the next iteration of id we need to update the index passed to
              !getSubNodePointer on this level to account for this change.
              idshift=idshift+pZTree%nsubdomains-1
            
              !Clean up
              CALL pZTree%clear()
              DEALLOCATE(pZTreeParent%subdomains)
              pZTreeParent%nsubdomains=newnd
              pZTreeParent%subdomains => tmpSubDomains
              tmpSubDomains => NULL()
            ENDIF
          ENDDO
        ENDIF
      ENDIF
    ENDSUBROUTINE ZTree_flattenLeafs
!
!-------------------------------------------------------------------------------
!> @brief Adds a fixed block size to all leaf nodes in the Z-tree
!> @param thisZTreeNode the "Z"-Tree node to start renumbering from 
!>        (must be initialized)
!> @param xdim the x-dimension of the block to add (xdim > 0)
!> @param ydim the y-dimension of the block to add (ydim > 0)
!> @param zdim the z-dimension of the block to add (zdim > 0)
!> 
    PURE RECURSIVE SUBROUTINE ZTree_addToLeafs(thisZTreeNode,xdim,ydim,zdim)
      CLASS(ZTreeNodeType),INTENT(INOUT) :: thisZTreeNode
      INTEGER(SIK),INTENT(IN) :: xdim
      INTEGER(SIK),INTENT(IN) :: ydim
      INTEGER(SIK),INTENT(IN) :: zdim
      INTEGER(SIK) :: id,istt,isttd,xstt,ystt,zstt
      
      IF(thisZTreeNode%istt /= -1 .AND. ALL((/xdim,ydim,zdim/) > 0)) THEN
        
        !Update starting dimensions
        thisZTreeNode%x(1)=(thisZTreeNode%x(1)-1)*xdim+1
        thisZTreeNode%y(1)=(thisZTreeNode%y(1)-1)*ydim+1
        thisZTreeNode%z(1)=(thisZTreeNode%z(1)-1)*zdim+1
        
        IF(thisZTreeNode%nsubdomains == 0) THEN
          !This is a leaf node insert the new block on this leaf
          istt=thisZTreeNode%istt
          xstt=thisZTreeNode%x(1)
          ystt=thisZTreeNode%y(1)
          zstt=thisZTreeNode%z(1)
          CALL thisZTreeNode%clear()
          CALL thisZTreeNode%init(xstt,xstt+xdim-1,ystt,ystt+ydim-1, &
            zstt,zstt+zdim-1,istt)
        ELSE
          !This is an intermediate level, so modify the dimensions
          !and update istp then the subdomains. The starting index
          !of the subdomains is also updated.
          thisZTreeNode%x(2)=thisZTreeNode%x(2)*xdim
          thisZTreeNode%y(2)=thisZTreeNode%y(2)*ydim
          thisZTreeNode%z(2)=thisZTreeNode%z(2)*zdim
        
          thisZTreeNode%istp=thisZTreeNode%istt-1+ &
          (thisZTreeNode%x(2)-thisZTreeNode%x(1)+1)* &
            (thisZTreeNode%y(2)-thisZTreeNode%y(1)+1)* &
              (thisZTreeNode%z(2)-thisZTreeNode%z(1)+1)
          
          !Update subdomains
          isttd=thisZTreeNode%istt
          xstt=thisZTreeNode%x(1)
          ystt=thisZTreeNode%y(1)
          zstt=thisZTreeNode%z(1)
          DO id=1,thisZTreeNode%nsubdomains
            thisZTreeNode%subdomains(id)%istt=isttd
            CALL thisZTreeNode%subdomains(id)%addToleafs(xdim,ydim,zdim)
            isttd=thisZTreeNode%subdomains(id)%istp+1
          ENDDO
        ENDIF
      ENDIF
    ENDSUBROUTINE ZTree_addToLeafs
!
!-------------------------------------------------------------------------------
!> @brief Renumbers a "Z-tree" node with a new starting index and stopping index
!> for the domain defined on the node and all subdomains.
!> @param thisZTreeNode the "Z"-Tree node to start renumbering from 
!>        (must be initialized)
!> @param istt the new starting index to use (0 < istt)
!> 
    PURE RECURSIVE SUBROUTINE ZTree_Renumber(thisZTreeNode,istt)
      CLASS(ZTreeNodeType),INTENT(INOUT) :: thisZTreeNode
      INTEGER(SIK),INTENT(IN) :: istt
      INTEGER(SIK) :: id,isttd,d
      
      IF(thisZTreeNode%istt /= -1 .AND. istt > 0) THEN
        d=thisZTreeNode%istp-thisZTreeNode%istt
        thisZTreeNode%istt=istt
        thisZTreeNode%istp=istt+d
        isttd=thisZTreeNode%istt
        DO id=1,thisZTreeNode%nsubdomains
          CALL thisZTreeNode%subdomains(id)%renumber(isttd)
          isttd=thisZTreeNode%subdomains(id)%istp+1
        ENDDO
      ENDIF
    ENDSUBROUTINE ZTree_Renumber
!
!-------------------------------------------------------------------------------
!> @brief Removes (shaves off) a range of the "Z"-Tree and renumbers the rest of
!> nodes in the tree as necessary.
!> @param thisZTreeNode the "Z"-Tree node to shave from
!> @param x the range in the x-dimension to shave (x(1) < x(2))
!> @param y the range in the y-dimension to shave (y(1) < y(2))
!> @param z the range in the z-dimension to shave (z(1) < z(2))
!>
!> This method can also be thought of as a partial deletion or removal of nodes
!> from the tree. Any nodes within the range can be deleted which destroys
!> convexity of the grid. So in general this method does not enforce
!> that the grid be convex after it is shaved.
!>
    PURE RECURSIVE SUBROUTINE ZTree_Shave(thisZTreeNode,x,y,z)
      CLASS(ZTreeNodeType),INTENT(INOUT) :: thisZTreeNode
      INTEGER(SIK),INTENT(IN) :: x(2)
      INTEGER(SIK),INTENT(IN) :: y(2)
      INTEGER(SIK),INTENT(IN) :: z(2)
      
      INTEGER(SIK) :: id,id2,idstt,idstp,sdx(2),sdy(2),sdz(2)
      TYPE(ZTreeNodeType),POINTER :: tmpSubDomains(:)
      
      IF(thisZTreeNode%istt /= -1) THEN
!
!This node is entirely within the range, so clear the node and all subnodes
        IF(x(1) <= thisZTreeNode%x(1) .AND. thisZTreeNode%x(2) <= x(2) .AND. &
           y(1) <= thisZTreeNode%y(1) .AND. thisZTreeNode%y(2) <= y(2) .AND. &
           z(1) <= thisZTreeNode%z(1) .AND. thisZTreeNode%z(2) <= z(2)) THEN
          
          CALL thisZTreeNode%clear()
!
!Some part of the shave range is within the domain
        ELSEIF((thisZTreeNode%x(1) <= x(1) .AND. x(1) <= thisZTreeNode%x(2)) &
          .OR. (thisZTreeNode%x(1) <= x(2) .AND. x(2) <= thisZTreeNode%x(2)) &
          .OR. (thisZTreeNode%y(1) <= y(1) .AND. y(1) <= thisZTreeNode%y(2)) &
          .OR. (thisZTreeNode%y(1) <= y(2) .AND. y(2) <= thisZTreeNode%y(2)) &
          .OR. (thisZTreeNode%z(1) <= z(1) .AND. z(1) <= thisZTreeNode%z(2)) &
          .OR. (thisZTreeNode%z(1) <= z(2) .AND. z(2) <= thisZTreeNode%z(2))) THEN
          
          !Loop over the subdomains, find those that are also partially 
          !within the shave range and shave them
          DO id=thisZTreeNode%nsubdomains,1,-1
            
            !Determine ranges within each subdomain
            sdx(1)=MAX(thisZTreeNode%subdomains(id)%x(1),x(1))
            sdx(2)=MIN(thisZTreeNode%subdomains(id)%x(2),x(2))
            sdy(1)=MAX(thisZTreeNode%subdomains(id)%y(1),y(1))
            sdy(2)=MIN(thisZTreeNode%subdomains(id)%y(2),y(2))
            sdz(1)=MAX(thisZTreeNode%subdomains(id)%z(1),z(1))
            sdz(2)=MIN(thisZTreeNode%subdomains(id)%z(2),z(2))
            
            !Shave the subdomain
            idstp=thisZTreeNode%subdomains(id)%istp
            IF(x(1) <= x(2) .AND. y(1) <= y(2) .AND. z(1) <= z(2)) &
              CALL thisZTreeNode%subdomains(id)%shave(sdx,sdy,sdz)
            
            IF(thisZTreeNode%subdomains(id)%istt == -1) THEN
              IF(thisZTreeNode%nsubdomains == 1) THEN
                CALL thisZTreeNode%clear()
              ELSE
                !The entire subdomain was cleared, so reallocate the subdomains
                tmpSubDomains => thisZTreeNode%subdomains
                NULLIFY(thisZTreeNode%subdomains)
                thisZTreeNode%nsubdomains=thisZTreeNode%nsubdomains-1
                ALLOCATE(thisZTreeNode%subdomains(thisZTreeNode%nsubdomains))
            
                !Re-initialize the subdomains with a new starting index
                idstt=thisZTreeNode%istt
                DO id2=1,id-1
                  thisZTreeNode%subdomains(id2)%x=tmpSubDomains(id2)%x
                  thisZTreeNode%subdomains(id2)%y=tmpSubDomains(id2)%y
                  thisZTreeNode%subdomains(id2)%z=tmpSubDomains(id2)%z
                  thisZTreeNode%subdomains(id2)%istt=tmpSubDomains(id2)%istt
                  thisZTreeNode%subdomains(id2)%istp=tmpSubDomains(id2)%istp
                  thisZTreeNode%subdomains(id2)%nsubdomains= &
                    tmpSubDomains(id2)%nsubdomains
                  thisZTreeNode%subdomains(id2)%subdomains => &
                    tmpSubDomains(id2)%subdomains
                  idstt=thisZTreeNode%subdomains(id2)%istp+1
                ENDDO
                DO id2=id+1,thisZTreeNode%nsubdomains+1
                  thisZTreeNode%subdomains(id2-1)%x=tmpSubDomains(id2)%x
                  thisZTreeNode%subdomains(id2-1)%y=tmpSubDomains(id2)%y
                  thisZTreeNode%subdomains(id2-1)%z=tmpSubDomains(id2)%z
                  thisZTreeNode%subdomains(id2-1)%istt=tmpSubDomains(id2)%istt
                  thisZTreeNode%subdomains(id2-1)%istp=tmpSubDomains(id2)%istp
                  thisZTreeNode%subdomains(id2-1)%nsubdomains= &
                    tmpSubDomains(id2)%nsubdomains
                  thisZTreeNode%subdomains(id2-1)%subdomains => &
                    tmpSubDomains(id2)%subdomains
                  CALL thisZTreeNode%subdomains(id2-1)%renumber(idstt)
                  idstt=thisZTreeNode%subdomains(id2-1)%istp+1
                ENDDO
              
                !Clear temporary
                DEALLOCATE(tmpSubDomains)
            
                !Change the stopping index for this node
                thisZTreeNode%istp=idstt-1
              ENDIF
            ELSEIF(thisZTreeNode%subdomains(id)%istp < idstp) THEN
              !Part of this subdomain was removed, so renumber all the
              !subdomains downstream of this subdomains.
              DO id2=id+1,thisZTreeNode%nsubdomains
                CALL thisZTreeNode%subdomains(id2)% &
                  renumber(thisZTreeNode%subdomains(id2-1)%istp+1)
              ENDDO
              !Update the stopping index for this domain
              thisZTreeNode%istp= &
                thisZTreeNode%subdomains(thisZTreeNode%nsubdomains)%istp
            ENDIF
          ENDDO
        ENDIF
      ENDIF
    ENDSUBROUTINE ZTree_Shave
!
!-------------------------------------------------------------------------------
!> @brief Partitions the global indexing within a "Z"-Tree. It returns the 
!> starting and stopping indices for the ith partition given a desired number
!> of partitions.
!> @param thisZTreeNode the "Z"-Tree node whose indexing will be partitioned
!> @param npart the number of partitions for the "Z"-tree
!> @param ipart the ith partition to get the bounding indices for
!> @param istt (output) the starting index for the partition
!> @param istp (output) the stopping index for the partition
!> 
    PURE SUBROUTINE ZTree_Partition(thisZTreeNode,npart,ipart,istt,istp)
      CLASS(ZTreeNodeType),INTENT(IN) :: thisZTreeNode
      INTEGER(SIK),INTENT(IN) :: npart
      INTEGER(SIK),INTENT(IN) :: ipart
      INTEGER(SIK),INTENT(OUT) :: istt
      INTEGER(SIK),INTENT(OUT) :: istp
      
      LOGICAL(SBK) :: lenoughWork
      INTEGER(SIK) :: id,ilevel,maxLevels,ndlevel,idpart,idstt,ipstt,ipstp
      INTEGER(SIK) :: npart_rem,nd_rem,nwork
      REAL(SRK) :: avgNperP
      
      istt=-1
      istp=-1
      IF(thisZTreeNode%istt /= -1 .AND. npart > 0 .AND. 0 <= ipart .AND. &
        ipart <= npart-1) THEN
        
        !Check for trivial case of 1-to-1 mapping
        IF(npart == thisZTreeNode%istp-thisZTreeNode%istt+1) THEN
          istt=ipart+1
          istp=istt
        ELSE
          !Get maximum levels in tree
          maxLevels=thisZTreeNode%getMaxLevels(0)
        
          !Determine the level in the tree with at least npart domains
          !Skip the bottom-most level because this is level is likely
          !incomplete and is handled by the 1-to-1 mapping check previously.
          ndlevel=0
          DO ilevel=1,maxLevels-1
            !Get the total number of domains at each level
            ndlevel=thisZTreeNode%getNDomains(ilevel)
            IF(ndlevel >= npart) EXIT
          ENDDO
          
          IF(ndlevel < npart) THEN
            !A sufficient number of domains has not been found,
            !Try the last level of the tree, only if its completely filled.
            IF(thisZTreeNode%getNDomains(maxLevels) == &
              thisZTreeNode%istp-thisZTreeNode%istt+1) &
                ndlevel=thisZTreeNode%istp-thisZTreeNode%istt+1
          ENDIF
          
          IF(ndlevel == npart) THEN
            !There is a 1-to-1 correspondence between Z-tree nodes at
            !the given level and the number of partitions. So,
            !get the starting and stopping index at the given
            !node within the given level
            CALL thisZTreeNode%getSubNodeBounds(ilevel,ipart+1,istt,istp)
          ELSEIF(ndlevel > npart) THEN
            !Try to evenly distribute the Z-Tree nodes at the given level 
            !among the partitions
            
            !Compute the average number of nodes per partition
            nwork=thisZTreeNode%istp-thisZTreeNode%istt+1
            avgNperP=REAL(nwork,SRK)/REAL(npart,SRK)
            
            !Loop up to this partition to determine it's starting/stopping
            !indices.
            idpart=-1
            ipstt=1
            npart_rem=npart
            nd_rem=ndlevel
            DO id=1,ndlevel
              CALL thisZTreeNode%getSubNodeBounds(ilevel,id,idstt,ipstp)
              
              !Check if there is sufficient work yet over the given domains
              lenoughWork=(avgNperP < REAL(ipstp-ipstt+1,SRK) .OR. &
                (avgNperP .APPROXEQA. REAL(ipstp-ipstt+1,SRK)))
              
              !Increment to the next partition if we have enough work for it,
              !or if there are an equal # of partitions and nodes remaining
              IF(lenoughWork .OR. npart_rem == nd_rem) THEN
                idpart=idpart+1
                npart_rem=npart_rem-1
                IF(idpart == ipart) THEN
                  !Found the starting/stopping indices for the
                  !desired partition, so assign output and exit
                  istt=ipstt
                  istp=ipstp
                  EXIT
                ELSE
                  nwork=nwork-(ipstp-ipstt+1)
                  avgNperP=REAL(nwork,SRK)/REAL(npart_rem,SRK)
                  ipstt=ipstp+1
                ENDIF
              ENDIF
              nd_rem=nd_rem-1  
            ENDDO
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE ZTree_Partition
!
ENDMODULE MortonOrdering
