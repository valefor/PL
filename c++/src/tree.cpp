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
bool RadixTree::updateSubTree(RadixInterNode * parent,
        RadixNode * child, RadixInterNode * insert)
{
    // which means insertion happends at root node
    if (parent == nullptr) {
        root.iNode = insert;
    } else if (parent->right.rNode == child) {
        // insertion happends on right side, replace right side
        // node with this internal node. reset flag
        parent->right.iNode = insert;
        BIT_RESET(parent->flags, FLAG_IN_RIGHT);
    } else {
        // insertion happends on left side
        parent->left.iNode = insert;
        BIT_RESET(parent->flags, FLAG_IN_LEFT);
    }
}

// Insert between internal node and its internal node parent
bool RadixTree::updateSubTree(RadixInterNode * parent,
        RadixInterNode * child, RadixInterNode * insert)
{
    // which means insertion happends at root node
    if (parent == nullptr) {
        root.iNode = insert;
    } else if (parent->right.iNode == child) {
        // insertion happends on right side, replace right side
        // node with this internal node. reset flag
        parent->right.iNode = insert;
        BIT_RESET(parent->flags, FLAG_IN_RIGHT);
    } else {
        // insertion happends on left side
        parent->left.iNode = insert;
        BIT_RESET(parent->flags, FLAG_IN_LEFT);
    }
}

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
            /*
             * Bits of prefix of node we're adding is less than current node
             */
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
                // go on searching on left side tree
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
     * The first different bit index is lower than prefix length, what does it mean?
     * suppose we have a key=0111 0011, prefix length is 4, the node we have found has
     * key=0110 0011, prefix length is 5, first different index is 2, that means these 
     * two nodes should not be on same branch since 2 is less than 4, they should fork
     * from "01"
     *
     */
    if (diffBit < minPrefixLen) {
        // A new internal node should be created to link tree
        RadixInterNode * newInode = new RadixInterNode();
        newInode->bits = diffBit;
        rNode->parent = newInode;

        if (currNode == nullptr || newInode->bits > currNode->bits) {
            /*
             * If newly created node is between found node(a external node) 
             * and its parent(currNode, a internal node)
             */
            newInode->parent = currNode;
            // this internal node has external nodes on both right/left side
            newInode->flags = FLAG_IN_L_EXT | FLAG_IN_R_EXT;

            // the bits'th has be set in rNode prefix, rNode should on right side.
            // otherwise, on the left side.
            if (RadixBitTest(getNodePrefix(rNode), newInode->bits)) {
                newInode->right.rNode = rNode;
                newInode->left.rNode = found;
            } else {
                newInode->left.rNode = rNode;
                newInode->right.rNode = found;
            }

            // Last we should update/adjust the sub tree which currNode points to
            updateSubTree(currNode, found, newInode);
        } else {
            // Insertion happends up the tree(currNode)
            RadixInterNode * parent = currNode->parent;
            // Search back up until we find node whose bits fit this diffBit
            while (parent && ( newInode->bits < parent->bits)) {
                currNode = parent;
                parent = currNode->parent;
            }
            // if ( (!parent || ( newInode->bits != parent->bits )))
            
            // Node to insert found, the node to be added is between this parent and
            // currNode 
            newInode->parent = parent;
            if (RadixBitTest(getNodePrefix(rNode), newInode->bits)) {
                newInode->right.rNode = rNode;
                newInode->left.iNode = currNode;
                // this internal node has an external node on right
                newInode->flags = FLAG_IN_R_EXT;
            } else {
                newInode->left.rNode = rNode;
                newInode->right.iNode = currNode;
                // this internal node has an external node on left
                newInode->flags = FLAG_IN_L_EXT;
            }

            // Last we should update/adjust the sub tree which currNode points to
            //updateSubTree(parent, found, newInode);
        }

    }
}
