/*Empty Since Template declaration&implementation must be put together*/
#include <tree.h>

// The k is key, b is a bit string which only has some/one bit(s) set like 0000 0100
#define BIT_SET(k,b)            ((k) |= (b))
#define BIT_RESET(k,b)          ((k) &= ~(b))
#define BIT_FLIP(k,b)           ((k) ^= (b))
#define BIT_TEST(k,b)           ((k) & (b))
#define BIT_ISSET(k,b)          (BIT_TEST(k,b) != 0)
#define BIT_MATCH(k,b)          (BIT_TEST(k,b) == (b))

// Flags for (I)nternal (N)ode use
#define FLAG_IN_ATTACHED    0x01
#define FLAG_IN_L_EXT       0x02
#define FLAG_IN_L_ATTACHED  0x04
#define FLAG_IN_LEFT        (FLAG_IN_L_EXT|FLAG_IN_L_ATTACHED)
#define FLAG_IN_R_EXT       0x08
#define FLAG_IN_R_ATTACHED  0x10
#define FLAG_IN_RIGHT       (FLAG_IN_R_EXT|FLAG_IN_R_ATTACHED)



/*
 * This Matrix is used for quick searching of the 
 * first different bit of given two ints in U8 bits
 * format. e.g :
 * 0000 0011
 * RadixBitIndexMatrix[0x1 ^ 0x2] is 6, the 6 bit is
 * most significant bit 
 */
const u_int8_t RadixBitIndexMatrix[256] = {
    8, 7, 6, 6, 5, 5, 5, 5, 4, 4, 4, 4, 4, 4, 4, 4,
    3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
    2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
    2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
};

//       0123 4567      --- index start from 0
// key = 0110 0000 0000 0111, bits = 2
// To see the specified bit of give key is set or not 
static inline U8 RadixBitTest(const U8 * const key, U16 bits)
{
    //  key[0] = 0110 0000, 1000 0000
    //                  , >> 2:0010 0000

    // * key[bits/8] & 0010 0000
    return (key[bits >> 3]) & (0x80 >> (bits & 0x07) );
}

// Find the index of first different bit between two key  
static inline U16 RadixBitFindDiff(const U8 * p1, const U8 * p2, U16 bits)
{
    U16 diffBit;

    for (diffBit = 0; diffBit < bits; diffBit += CHAR_BIT) {
        if (*p1 != *p2) {
            diffBit += RadixBitIndexMatrix[*p1 ^ *p2];
            // has already found it, return;
            break;
        }
        p1++, p2++;
    }

    // diff bit should not bigger than given bits
    if (diffBit > bits) diffBit = bits;

    return diffBit;
}

// Link the node to given parent(iNode)
/*
static void RadixNodeLink(RadixInterNode * parent, RadixNode * origChild, RadixNode * child)
{
    // the bits'th has be set in rNode prefix, rNode should on right side.
    // otherwise, on the left side.
    if (RadixBitTest(getNodePrefix(child), parent->bits)) {
        parent->right.rNode = child;
        parent->left.rNode = origChild;
    } else {
        parent->left.rNode = child;
        parent->right.rNode = origChild;
    }
}
*/

// Insert between external node and its internal node parent
bool RadixTree::updateSubTree(RadixInterNode * insert,
        RadixNode * found)
{
    RadixInterNode * parent = found->parent;
    insert->parent = parent;
    // which means insertion happends at root node
    if (parent == nullptr) {
        root.iNode = insert;
    } else if (parent->right.rNode == found) {
        // insertion happends on right side, replace right side
        // external node with this internal node. reset flag
        parent->right.iNode = insert;
        BIT_RESET(parent->flags, FLAG_IN_RIGHT);
    } else {
        // insertion happends on left side
        parent->left.iNode = insert;
        BIT_RESET(parent->flags, FLAG_IN_LEFT);
    }

    found->parent = insert;

}

// Insert between internal node and its internal node parent
bool RadixTree::updateSubTree(RadixInterNode * insert,
        RadixInterNode * found)
{
    RadixInterNode * parent = found->parent;
    insert->parent = parent;
    // which means insertion happends at root node
    if (parent == nullptr) {
        root.iNode = insert;
    } else if (parent->right.iNode == found) {
        // insertion happends on right side, replace right side
        // internal node with this internal node.
        parent->right.iNode = insert;
    } else {
        // insertion happends on left side
        parent->left.iNode = insert;
    }

    if (insert->bits == found->bits) {
        // the left/right children of 'found' are re-parent to this 'insert'
        // free it and return 
        delete found;
        return true;
    }
    
    found->parent = insert;
    return true;
}

// These 'link' functions are straightforward
void RadixTree::link(RadixInterNode * internal, RadixInterNode * node, RadixInterNode *found)
{

}

/*              iNode[bits:x]                     iNode[bits:x]                       
 *                   / \                               / \                
 *           rNode  /   \             OR       rNode  /   \               
 *         rNode(key)  iNode[bits:x]       iNode[bits:x]  rNode(key)    
 */
void RadixTree::link(RadixInterNode * internal, RadixNode * node, RadixInterNode *found)
{
    if (internal==nullptr) return;

    // If user forget to set flag, set it here
    if (internal->attached != nullptr) 
        internal->flags = BIT_SET(internal->flags, FLAG_IN_ATTACHED);

    // If the internal node has an attached node which means it's an 
    // 'route' internal node
    if (internal->flags && BIT_ISSET(internal->flags, FLAG_IN_ATTACHED)) {
        // In this routine, 'node' is used to pass the external child of
        // 'found', by doing this we know the 'found' is on which path.
        if (found->bits == internal->bits) {
            // [@Case4-2-1]: Replace 'found' with 'internal'
            internal->left = found->left;
            internal->right = found->right;
            internal->flags = found->flags | FLAG_IN_ATTACHED;
        } else if (RadixBitTest(getNodePrefix(node), internal->bits)) {
            // [@Case4-2-2]: 'found' is on the right path of 'internal'
            // that means its parent:found is the right child of 'internal'
            // but the problem is who shall be the left child? we use attched one
            internal->right.iNode = found;
            internal->left.rNode = internal->attached;
            internal->flags = internal->flags | FLAG_IN_L_ATTACHED | FLAG_IN_R_EXT;

        } else {
            // [@Case4-2-3]: 'Found' is on the left path of 'internal'
            // that means its parent:found is the left child of 'internal'
            internal->left.iNode = found;
            internal->right.rNode = internal->attached;
            internal->flags = internal->flags | FLAG_IN_R_ATTACHED | FLAG_IN_L_EXT;
        }

    // It's a 'pure' internal node
    } else {
        // the bits'th has be set in 'node' prefix, 'node' should on right side.
        // otherwise, on the left side.
        if (RadixBitTest(getNodePrefix(node), internal->bits)) {
            internal->right.rNode = node;
            internal->left.iNode = found;
            // this internal node has an external node on right
            internal->flags = FLAG_IN_R_EXT;
        } else {
            internal->left.rNode = node;
            internal->right.iNode = found;
            // this internal node has an external node on left
            internal->flags = FLAG_IN_L_EXT;
            // After linking these nodes together shall update the subtree
            // The parent of 'found' should be the parent of this internal node
            // and this internal node should be the new parent of 'found'
            updateSubTree(internal,found);
        }
    }
}

/*              iNode[bits:x]  
 *                   / \
 *           rNode  /   \ rNode
 *        *rNode(key1) rNode(key2)
 */
void RadixTree::link(RadixInterNode * internal, RadixNode * node, RadixNode *found)
{
    if (internal==nullptr) return;

    // If user forget to set flag, set it here
    if (internal->attached != nullptr) 
        internal->flags = BIT_SET(internal->flags, FLAG_IN_ATTACHED);

    // If the internal node has an attached node which means it's an 
    // 'route' internal node
    if (internal->flags && BIT_ISSET(internal->flags, FLAG_IN_ATTACHED)) {
        // In this routine, 'node' is useless since we can fetch it
        // through internal->attached
        if (RadixBitTest(getNodePrefix(found), internal->bits)) {
            internal->right.rNode = found;
            internal->left.rNode = internal->attached;
            internal->flags = internal->flags | FLAG_IN_L_ATTACHED | FLAG_IN_R_EXT;
        } else {
            internal->left.rNode = found;
            internal->right.rNode = internal->attached;
            internal->flags = internal->flags | FLAG_IN_R_ATTACHED | FLAG_IN_L_EXT;
        }
    
    // It's a 'pure' internal node
    } else {
        // this internal node has external nodes on both right/left side
        internal->flags = FLAG_IN_L_EXT | FLAG_IN_R_EXT;
        // the bits'th has be set in 'node' prefix, 'node' should on right side.
        // otherwise, on the left side.
        if (RadixBitTest(getNodePrefix(node), internal->bits)) {
            internal->right.rNode = node;
            internal->left.rNode = found;
        } else {
            internal->left.rNode = node;
            internal->right.rNode = found;
        }
    }

    // After linking these nodes together shall update the subtree
    // The parent of 'found' should be the parent of this internal node
    // and this internal node should be the new parent of 'found'
    updateSubTree(internal,found);
}

/*
 * Give these 7 keys:
 *              0123 4567
 *
 *      key1    0100 0001 / 8
 *      key2    0100 1001 / 8
 *      key3    0100 1010 / 8
 *      key4    0100 0011 / 8
 *      key5    0010 0001 / 8
 *      key6    0100 1111 / 4
 *      key7    0100 0001 / 5
 *      key8    0110 1111 / 3
 *  
 *  (1)Add key1, the easiest case:
 *                  root.rNode = RadixNode(key1)
 *
 *  (2)Add key2, refers to [@Case1-1] :
 *                  root
 *                    | 
 *              iNode[bits:4]  
 *                   / \
 *           rNode  /   \ rNode
 *        *rNode(key1) rNode(key2)
 *
 *  (3)Add key3, refers to [@Case1-1]:
 *                  root
 *                    | 
 *              iNode[bits:4]  
 *                   / \
 *           rNode  /   \ 
 *          rNode(key1) iNode[bits:6]
 *                          /\
 *                         /  \
 *              *rNode(key2)  rNode(key3)
 *
 *  (4)Add key4, refers to [@Case1-1]:
 *                  root
 *                    | 
 *              iNode[bits:4]  
 *                 0 / \ 1
 *                  /   \ 
 *      iNode[bits:6] iNode[bits:6]
 *         0 /\ 1         0 /\ 1
 *          /  \           /  \
 *rNode(key1) rNode(key4) /    \
 *                rNode(key2) rNode(key3)
 *
 *  (5)Add key5, refers to [@Case1-2]:
 *                  root
 *                    |
 *              iNode[bits:1]
 *               0 /     \ 1
 *          rNode(key5)   \ 
 *                    iNode[bits:4]  
 *                       0 / \ 1
 *                        /   \ 
 *            iNode[bits:6] iNode[bits:6]
 *               0 /\ 1         0 /\ 1
 *                /  \           /  \
 *     *rNode(key1) rNode(key4) /    \
 *                      rNode(key2) rNode(key3)
 *
 *  (6)Add key6, refers to [@Case4-2]:
 *                  root
 *                    |
 *              iNode[bits:1]
 *               0 /     \ 1
 *          rNode(key5)   \ 
 *                    iNode[bits:4]--attached=rNode(key6)  
 *                       0 / \ 1
 *                        /   \ 
 *            iNode[bits:6] iNode[bits:6]
 *               0 /\ 1         0 /\ 1
 *                /  \           /  \
 *     *rNode(key1) rNode(key4) /    \
 *                      rNode(key2) rNode(key3)
 *
 *  (7)Add key7, refers to [@Case4-1]:
 *                  root
 *                    |
 *              iNode[bits:1]
 *               0 /     \ 1
 *          rNode(key5)   \ 
 *                    iNode[bits:4]--attached=rNode(key6)  
 *                       0 / \ 1
 *                        /   \ 
 *            iNode[bits:5] iNode[bits:6]
 *               0 /\ 1 \       0 /\ 1
 *                /  \  /attach  /  \
 *     iNode[bits:6]  \/        /    \
 *           /\        \rNode(key2) rNode(key3)
 *          /  \        \
 *         /    \    rNode(key7)   
 * *rNode(key1) rNode(key4)
 */
bool RadixTree::add(RadixNode * rNode)
{
    RadixInterNode * currNode;
    RadixNode  * found;
    U16 minPrefixLen, diffBit;
    const U8 * thePrefix;

    if (rNode == nullptr) return false;

    // At the very beginning, no nodes at all, attach this node on root
    if (root.iNode == nullptr) {
        rNode->parent = nullptr;
        root.rNode = rNode;
        eNodeCount ++ ;    
    }

    thePrefix = getNodePrefix(rNode);

    if (iNodeCount == 0) {
        currNode = nullptr;
        found = root.rNode;
    } else {
        currNode = root.iNode;
        while(true) {

            // Bits of prefix of node we're adding is less than current node
            if (rNode->prefixLength <= currNode->bits) {
                if (currNode->flags) {
                    if (BIT_TEST(currNode->flags, FLAG_IN_ATTACHED)) {
                        found = currNode->attached;
                    } else if (BIT_TEST(currNode->flags, FLAG_IN_R_EXT)) {
                        found = currNode->right.rNode;
                    } else {
                        found = currNode->left.rNode;
                    }
                    break;
                }
                // no flags set means it's an internal node with internal nodes 
                // on right and left, go further to find leaf node
                currNode = currNode->left.iNode;
            } else if (RadixBitTest(getNodePrefix(rNode),currNode->bits)) {
                // the prefix of adding node fits the currNode and the currNode
                // has leaf node, we find it.
                if (BIT_ISSET(currNode->flags, FLAG_IN_RIGHT)) {
                    found = currNode->right.rNode;
                    break;
                }
                // go on searching on right side tree
                currNode = currNode->right.iNode;
            } else {
                // go on searching on left side tree(default)
                // which means if there is no matchable prefix in current tree
                // always use the leftmost leaf node as found node 
                if (BIT_ISSET(currNode->flags, FLAG_IN_LEFT)) {
                    found = currNode->left.rNode;
                    break;
                }
                currNode = currNode->left.iNode;
            }
        }
    }

    minPrefixLen = (rNode->prefixLength <= found->prefixLength) ?
        rNode->prefixLength : found->prefixLength;
    diffBit = RadixBitFindDiff(getNodePrefix(rNode), getNodePrefix(found), minPrefixLen);

    /*
     * [@Case1]They have different prefix:
     * The first different bit index is lower than prefix length, what does it mean?
     * suppose we have a key=0111 0011, prefix length is 4, the node we have found has
     * key=0110 0011, prefix length is 5, first different index is 2, that means these 
     * two nodes should not be on same branch since 2 is less than 4, they should fork
     * from "01"
     *
     * diffBit can never bigger than minPrefixLen:
     */
    if (diffBit < minPrefixLen) {
        // A new internal node should be created to link tree
        RadixInterNode * newInode = new RadixInterNode();
        newInode->bits = diffBit;
        rNode->parent = newInode;

        if (currNode == nullptr || newInode->bits > currNode->bits) {
            /*
             * [@Case1-1]:
             * If newly created node is between found node(a external node) 
             * and its parent(currNode, a internal node)
             */
            link(newInode, rNode, found);
        } else {
            // [@Case1-2] : Insertion happends up the tree(currNode)
            RadixInterNode * parent = currNode->parent;
            // Search back up until we find node whose bits fit this diffBit
            while (parent && ( newInode->bits < parent->bits)) {
                currNode = parent;
                parent = currNode->parent;
            }
            // if ( (!parent || ( newInode->bits != parent->bits )))
            
            // Node to insert found, the node to be added is between this parent and
            // currNode 
            link(newInode, rNode, currNode);
        }

    /*
     * [@Case2]They have same minimum prefix and same prefix length
     * The 'rNode' shall be linked behind 'found'
     *
     * minPrefixLen == rNode->prefixLength == found->prefixLength == diffBit
     */
    } else if (rNode->prefixLength == found->prefixLength) {
        // Not supported yet
    
    /*
     * [@Case3]They have same minimum prefix and prefix length of node to be added is longer
     * That means 'rNode' is more specific and 'found' would be attached on that internal 
     * node whose 'bits' fits 'found's prefix length
     *
     * minPrefixLen == found->prefixLength == diffBit
     */
    } else if (rNode->prefixLength > found->prefixLength) {
        if (currNode &&(found->prefixLength == currNode->bits)) {
            /*
             * The external we found is an attached route.  
             * There is guaranteed to be an invalid child
             * pointer in the `curr' internal node that we can
             * just point at our route.
             */
            rNode->parent = currNode;
            if (RadixBitTest(getNodePrefix(rNode),currNode->bits)) {
                currNode->right.rNode = rNode;
                BIT_RESET(currNode->flags,FLAG_IN_R_ATTACHED);
                BIT_SET(currNode->flags,FLAG_IN_R_EXT);
            } else {
                currNode->left.rNode = rNode;
                BIT_RESET(currNode->flags,FLAG_IN_L_ATTACHED);
                BIT_SET(currNode->flags,FLAG_IN_L_EXT);
            }
        } else {
            /*
             * The external we found is a child route.  Here
             * we'll need to acquire a new internal node with space
             * for an attached external, attach `found' to it and
             * point one of the child pointers at `new', leaving the
             * other side invalid.
             */
            RadixInterNode * newInode = new RadixInterNode();
            newInode->bits = found->prefixLength;
            newInode->attached = found;
            newInode->flags = FLAG_IN_ATTACHED;

            if (RadixBitTest(getNodePrefix(rNode), newInode->bits)) {
                newInode->right.rNode = rNode;
                newInode->left.rNode = newInode->attached;
                newInode->flags = newInode->flags | FLAG_IN_L_ATTACHED | FLAG_IN_R_EXT;
            } else {
                newInode->left.rNode = rNode;
                newInode->right.rNode = newInode->attached;
                newInode->flags = newInode->flags | FLAG_IN_R_ATTACHED | FLAG_IN_L_EXT;
            }

            updateSubTree(newInode,found);
        }
    
    /*
     * [@Case4]They have same minimum prefix and prefix length of node to be added is shorter
     * The 'rNode' should be attached to a internal node above 'found'
     *
     * minPrefixLen == rNode->prefixLength == diffBit
     */
    } else { 
        RadixInterNode * newInode = new RadixInterNode();
        newInode->bits = rNode->prefixLength;
        newInode->attached = rNode;
        newInode->flags = FLAG_IN_ATTACHED;
        rNode->parent = newInode;

        if (currNode == nullptr || newInode->bits > currNode->bits) {
            /*
             * [@Case4-1]:
             * If newly created node is between found node(a external node) 
             * and its parent(currNode, a internal node)
             */
            link(newInode, rNode, found);
            // Last we should update/adjust the sub tree which currNode points to
            // updateSubTree(currNode, found, newInode);
        } else { 
            // [@Case4-2] : Insertion happends up the tree(currNode)
            RadixInterNode * parent = currNode->parent;
            // Search back up until we find node whose bits fit this diffBit
            while (parent && ( newInode->bits < parent->bits)) {
                currNode = parent;
                parent = currNode->parent;
            }
            // if ( (!parent || ( newInode->bits != parent->bits )))
            
            link(newInode, found, currNode);

            // Last we should update/adjust the sub tree which currNode points to
            // updateSubTree(parent, currNode, newInode);
        }
    }
}

// Introduction version with more details
//  bool RadixTree::add(RadixNode * rNode)
//  {
//      RadixInterNode * currNode;
//      RadixNode  * found;
//      U16 minPrefixLen, diffBit;
//      const U8 * thePrefix;
//  
//      if (rNode == nullptr) return false;
//  
//      // At the very beginning, no nodes at all, attach this node on root
//      if (root.iNode == nullptr) {
//          rNode->parent = nullptr;
//          root.rNode = rNode;
//          eNodeCount ++ ;    
//      }
//  
//      thePrefix = getNodePrefix(rNode);
//  
//      if (iNodeCount == 0) {
//          currNode = nullptr;
//          found = root.rNode;
//      } else {
//          currNode = root.iNode;
//          while(true) {
//  
//              // Bits of prefix of node we're adding is less than current node
//              if (rNode->prefixLength <= currNode->bits) {
//                  if (currNode->flags) {
//                      if (BIT_TEST(currNode->flags, FLAG_IN_ATTACHED)) {
//                          found = currNode->attached;
//                      } else if (BIT_TEST(currNode->flags, FLAG_IN_R_EXT)) {
//                          found = currNode->right.rNode;
//                      } else {
//                          found = currNode->left.rNode;
//                      }
//                      break;
//                  }
//                  // no flags set means it's an internal node with internal nodes 
//                  // on right and left, go further to find leaf node
//                  currNode = currNode->left.iNode;
//              } else if (RadixBitTest(getNodePrefix(rNode),currNode->bits)) {
//                  // the prefix of adding node fits the currNode and the currNode
//                  // has leaf node, we find it.
//                  if (BIT_ISSET(currNode->flags, FLAG_IN_RIGHT)) {
//                      found = currNode->right.rNode;
//                      break;
//                  }
//                  // go on searching on right side tree
//                  currNode = currNode->right.iNode;
//              } else {
//                  // go on searching on left side tree(default)
//                  // which means if there is no matchable prefix in current tree
//                  // always use the leftmost leaf node as found node 
//                  if (BIT_ISSET(currNode->flags, FLAG_IN_LEFT)) {
//                      found = currNode->left.rNode;
//                      break;
//                  }
//                  currNode = currNode->left.iNode;
//              }
//          }
//      }
//  
//      minPrefixLen = (rNode->prefixLength <= found->prefixLength) ?
//          rNode->prefixLength : found->prefixLength;
//      diffBit = RadixBitFindDiff(getNodePrefix(rNode), getNodePrefix(found), minPrefixLen);
//  
//      /*
//       * [@Case1]They have different prefix:
//       * The first different bit index is lower than prefix length, what does it mean?
//       * suppose we have a key=0111 0011, prefix length is 4, the node we have found has
//       * key=0110 0011, prefix length is 5, first different index is 2, that means these 
//       * two nodes should not be on same branch since 2 is less than 4, they should fork
//       * from "01"
//       *
//       */
//      if (diffBit < minPrefixLen) {
//          // A new internal node should be created to link tree
//          RadixInterNode * newInode = new RadixInterNode();
//          newInode->bits = diffBit;
//          rNode->parent = newInode;
//  
//          if (currNode == nullptr || newInode->bits > currNode->bits) {
//              /*
//               * If newly created node is between found node(a external node) 
//               * and its parent(currNode, a internal node)
//               */
//              newInode->parent = currNode;
//              // this internal node has external nodes on both right/left side
//              newInode->flags = FLAG_IN_L_EXT | FLAG_IN_R_EXT;
//  
//              // the bits'th has be set in rNode prefix, rNode should on right side.
//              // otherwise, on the left side.
//              if (RadixBitTest(getNodePrefix(rNode), newInode->bits)) {
//                  newInode->right.rNode = rNode;
//                  newInode->left.rNode = found;
//              } else {
//                  newInode->left.rNode = rNode;
//                  newInode->right.rNode = found;
//              }
//  
//              // Last we should update/adjust the sub tree which currNode points to
//              updateSubTree(currNode, found, newInode);
//          } else {
//              // Insertion happends up the tree(currNode)
//              RadixInterNode * parent = currNode->parent;
//              // Search back up until we find node whose bits fit this diffBit
//              while (parent && ( newInode->bits < parent->bits)) {
//                  currNode = parent;
//                  parent = currNode->parent;
//              }
//              // if ( (!parent || ( newInode->bits != parent->bits )))
//              
//              // Node to insert found, the node to be added is between this parent and
//              // currNode 
//              newInode->parent = parent;
//              if (RadixBitTest(getNodePrefix(rNode), newInode->bits)) {
//                  newInode->right.rNode = rNode;
//                  newInode->left.iNode = currNode;
//                  // this internal node has an external node on right
//                  newInode->flags = FLAG_IN_R_EXT;
//              } else {
//                  newInode->left.rNode = rNode;
//                  newInode->right.iNode = currNode;
//                  // this internal node has an external node on left
//                  newInode->flags = FLAG_IN_L_EXT;
//              }
//  
//              // Last we should update/adjust the sub tree which currNode points to
//              //updateSubTree(parent, found, newInode);
//          }
//  
//      /*
//       * [@Case2]They have same minimum prefix and same prefix length
//       * 
//       *
//       */
//      } else if (rNode->prefixLength == found->prefixLength) {
//      
//      /*
//       * [@Case3]They have same minimum prefix and prefix length of node to be added is longer
//       * 
//       *
//       */
//      } else if (rNode->prefixLength > found->prefixLength) {
//      
//      /*
//       * [@Case4]They have same minimum prefix and prefix length of node to be added is shorter
//       * 
//       *
//       */
//      } else {
//      }
//  }
