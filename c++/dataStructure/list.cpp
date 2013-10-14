/*
 * Data Structure - List
 *
 */

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

template<typename Type>
List<Type> * List<Type>::reverse() {
    if (!head) return this;

    Node<Type> * temp,* current;
    current = head->next;
    head->next = 0;
    while (current) {
        temp = current->next;
        current->next = head;
        head = current;
        current = temp;       
    }

    return this;

}

// recursive version
template<typename Type>
Node<Type>* List<Type>::reverse(Node<Type>* current) {
    if(!current || !current->next) 
    {
        return current;
    }

    Node<Type> * temp = reverse(current->next);
    temp -> next = current;
    current->next = 0;
}


// Tree Node
template<typename Type>
class TNode {
    Type data;
    TNode<Type> * left, * right;
};

// Breadth Fisrt Search Binary Tree
template<typename Type>
class BST {
    TNode<Type> * root; 
};
