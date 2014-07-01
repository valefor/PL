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
static inline U8 RadixBitTest(const U8 * const key, U16 bits) {
    //  key[0] = 0110 0000, 1000 0000
    //                  , >> 2:0010 0000

    // * key[bits/8] & 0010 0000
    return (key[bits >> 3]) & (0x80 >> (bits & 0x07) );
}

// Find the index of first different bit between two key  
static inline U16 RadixBitFindDiff(const U8 * const p1, const U8 * const p1, U16 bits) {
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

bool RadixTree::add(RadixNode * rNode)
{
    RadixInterNode * currNode;
    RadixNode  * found;
    U16 minPrefixLen, diffBit;
    const U8 * thePrefix;

    if (rNode == nullptr) return false;

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
                currNode = currNode->left.iNode;
            }
        
        }
    }

    minPrefixLen = (rNode->prefixLength <= found->prefixLength) ?
        rNode->prefixLength : found->prefixLength;
    diffBit = RadixBitFindDiff(getNodePrefix(rNode), getNodePrefix(found), minPrefixLen);

    if (diffBit < minPrefixLen) {
    
    }
}
