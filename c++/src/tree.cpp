/*Empty Since Template declaration&implementation must be put together*/
#include <tree.h>

bool RadixTree::add(RadixNode * rNode)
{
    RadixNode * currNode, * found;

    if (rNode == nullptr) return false;

    if (root.iNode == nulltr) {
        rNode->parent = nullptr;
        root.rNode = rNode;
        eNodeCount ++ ;    
    }

}
