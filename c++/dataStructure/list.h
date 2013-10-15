/*
 * Data Structure - List
 *
 */

#include <queue>
#include <iostream>

template<typename Type>
class Node {
    public:
    Type data;
    Node<Type> * next;
    Node(Type t) : data(t), next(0) {} 
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

    int cnt;
    Node<Type> * head ,* tail;

    public:
    void print();
    inline List() :cnt(0), head(0),tail(0){}
    inline List<Type> & append(Type t) {
        Node<Type> * temp = new Node<Type>(t); 
        if (cnt ==0) head = tail = temp;
        else tail->next = temp, tail = temp;

        cnt++;
        return *this;
    }

    inline int size() {
        return cnt;
    }
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
template<typename Type>
class BST {
    TNode<Type> * root; 

    public:
    void traversal();
};


// Template Function Mebmber Implemetation
/*
 * List 
 *
 */
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

template<typename Type>
void List<Type>::print() {
    Node<Type> * p = head;

    do {
        std::cout << p->data << " ";
    } while(p = p->next);
    std::cout << std::endl;
}

/*
 * BST
 *
 */
// To implement BFS, we need a queue, well, C++ STL provide one for us ;-)
template<typename Type>
void BST<Type>::traversal() {
    if (!root) return;

    std::queue<Type*> q;
    q.push(root);
    TNode<Type> * temp;
    while (temp = q.pop()) {
        // do something
        // temp->data = new Type();
        if (temp->left) q.push(temp->left);
        if (temp->right) q.push(temp->right);
    }
}
 


