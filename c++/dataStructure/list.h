/*
 * Data Structure - List
 *
 */

#include <queue>

template<typename Type>
class Node {
    Type data;
    Node<Type> * next;
};

// Bi-direction linked node
template<typename Type>
class NBode {
    Type data;
    NBode * next, * prev;
};

// The basic linked list
template<typename Type>
class List{

    int data;
    Node<Type> * head ,* tail;

    public:
    List() : head(0),tail(0){}
    List<Type> * reverse();
    Node<Type> * reverse(Node<Type>* cur);
};



// Tree Node
template<typename Type>
class TNode {
    Type* data;
    TNode<Type> * left, * right;
};

// Breadth Fisrt Search Binary Tree
// To implement BFS, we need a queue, well, C++ STL provide one for us ;-)
template<typename Type>
class BST {
    TNode<Type> * root; 

    public:
    void traversal();
};



