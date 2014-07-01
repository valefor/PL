#include <iostream>
#include <memory>
#include <cassert>
#include <functional>
#include <stddef.h>

#include "stdtypes.h"

/* -----------------------RB(red-black) Tree Implementation----------------- */

/* ----------------------------Radix Tree Implementation-------------------- */
class RadixNode;
class RadixInterNode;

union RadixNodePtr {
    RadixInterNode* iNode;
    RadixNode* rNode;

};

// Sizeof(RadixNodePrefix) is 0 since it uses c empty array trick
// This union was designed as a placeholder for later use
union RadixNodePrefix {
    U8 value[0] ;
    const U8 * pValue[0];
};

// Internal node
class RadixInterNode {
    public:
    RadixInterNode* parent;
    RadixNodePtr left;
    RadixNodePtr right;
    U16 bits;
    U16 flags;
    RadixNode * attached;
};

class RadixNode {
    public:
    U16 prefixLength; // Length of prefix, from 1 to 2^16-1
    RadixInterNode * parent;
    RadixNode(U16 pl, RadixInterNode * p = nullptr): prefixLength(pl), parent(p){} 
};

// Invasion Radix Tree version which means Radix doesn't manage user defined
// data, using invasion ways like pointer+offset to access user define data
class RadixTree {
    RadixNodePtr root;
    U16 prefixOffset;

    U32 iNodeCount; // Internal nodes count
    U32 eNodeCount; // External nodes count

    public:
    RadixTree(U16 pos): prefixOffset(pos), iNodeCount(0), eNodeCount(0){}
    bool add(RadixNode * rNode);
    const U8 * getNodePrefix(RadixNode* rNode) {
        RadixNodePrefix * prefix = (RadixNodePrefix*) (rNode + 1);
        return prefix->value + prefixOffset;
    }
};

/* ------------------------------AVL Tree Implementation-------------------- */
class AVLTraits {
    public:
    enum CmpResult {
        LT = -1,
        EQ = 0,
        BT = 1
    };
    class RotDir {
        public:
        enum value{
        LEFT    = 0,
        RIGHT   = 1
        };
    };
};

template <typename T>
class AVLNode 
{
    public:
    T data;       // user defined data
    int balancor;   // balance factor
    AVLNode<T> * c[2]; // c[0] - left sub tree, c[1] - right sub tree;

    // For iteration use
    AVLNode<T> * prev;
    AVLNode<T> * next;

    // The c({}) is the new Syntactic sugar of c++0x
    // T must support copy operation
    AVLNode(T t): data(t), balancor(0),c{nullptr,nullptr},prev(nullptr),next(nullptr) {}

    AVLNode * rotate(AVLTraits::RotDir::value dir);
};

template <typename T>
AVLNode<T> * AVLNode<T>::rotate(AVLTraits::RotDir::value dir=AVLTraits::RotDir::LEFT)
{
    AVLNode<T> * childr,*childl,*temp,*parent=this; 
    childl = parent->c[0];
    childr = parent->c[1];
    if (dir == AVLTraits::RotDir::LEFT) {
        if (childr != nullptr) {
            temp = childr->c[0];
            childr->c[0] = parent;
            parent->c[1] = temp;
            parent = childr;
        }
    } else {
        if (childl != nullptr) {
            temp = childl->c[1];
            childl->c[1] = parent;
            parent->c[0] = temp;
            parent = childl;
        }
    }
    return parent;
}

template <typename T> class AVLTreeIter;

template <typename T, typename A= std::allocator<T> > 
class AVLTree
{
    friend class AVLTreeIter<T>;

    public:
    typedef T value_type;
    // The typedefs have been defined in std::Allocator, just copy it here, or you can define them you self
    typedef typename A::reference       reference;
    typedef typename A::const_reference const_reference;
    typedef typename A::size_type       size_type;
    typedef typename A::difference_type difference_type;
    typedef A allocator_type;
    typedef const AVLTreeIter<T>        const_iterator;
    typedef AVLTreeIter<T>              iterator;

    typedef std::function<const void * (const void * obj)> AVLKeyFn;
    typedef std::function<int (const T& obj, const T& other)> AVLCmpFn;
    AVLTree(AVLKeyFn key, AVLCmpFn cmp, const A& a = A()): root(nullptr), keyFn(key), cmpFn(cmp), count(0), first(nullptr){ last = new AVLNode<T>(0);}

    ~AVLTree();

    bool insert(T t);
    void remove(T& t);
    bool exist(T& t);
    T& find(T t);

    iterator begin();
    const_iterator begin() const;
    const_iterator cbegin() const;
    iterator end();
    const_iterator end() const;
    const_iterator cend() const;

    void swap(const AVLTree&);
    size_type size();
    size_type max_size();
    bool empty();

    private:
    AVLNode<T> * root;              // user defined data
    AVLNode<T> * first, * last;          // will be used by interator class, last is the sentinal
    AVLKeyFn    keyFn;
    AVLCmpFn    cmpFn;
    int         count;

    AVLNode<T> * findInternal(T& t);
    // Think about how to support iterator
};

template <typename T, typename A>
typename AVLTree<T,A>::iterator AVLTree<T, A>::begin() {
    return AVLTree::iterator(first);
}

template <typename T, typename A>
typename AVLTree<T,A>::iterator AVLTree<T, A>::end() {
    return AVLTree::iterator(last);
}

template <typename T, typename A>
AVLTree<T, A>::~AVLTree() {
    delete last;
}

// !Note, each time inserting one node will lead a balance-broking of only one parent node 
template <typename T, typename A>
bool AVLTree<T, A>::insert(T t)
{
    if (root == nullptr) {
        first = root = new AVLNode<T>(t);
        root->next = last;
        last->prev = root;
        first->prev = last;
    }
    AVLNode<T> * reb = root;
    AVLNode<T> * parent, * child;
    int cmpResult, direction;

    // First, we need to find out the nearest node which was banlance-broken by inserting node
    for (reb = parent = root; ; parent = child) {
        if ( (cmpResult = cmpFn(parent->data, t)) == 0 ) return false;
        direction = cmpResult > 0 ? 0 : 1;

        // Here find insert point
        if ( (child=parent->c[direction]) == nullptr ) break;

        // Save the node which needs to be rebalanced
        if ( child->balancor != 0 ) {
            reb = child;
        }
    }

    // Create new node for management
    parent->c[direction] = child = new AVLNode<T>(t);
    // Add new node to iteration list
    if (direction == 0) {
        AVLNode<T> * temp = parent->prev;
        child->next = parent;
        parent->prev = child;
        child->prev = temp;
        if (temp != last) temp->next = child;
        else first = child;
    } else {
        AVLNode<T> * temp = parent->next;
        child->prev = parent;
        parent->next = child;
        child->next = temp;
        temp->prev = child;
    }
    count ++;

    // Update balance factor of all nodes of sub tree which is to be rebalanced 
    for (parent = reb; ; parent = parent->c[direction]) {
        cmpResult = cmpFn(parent->data, t);
        direction = cmpResult > 0 ? 0 : 1;
        parent->balancor -= cmpResult;

        if (parent->c[direction] == child)  break;
    }

    // Adjust sub tree
    if ( reb->balancor >= -1 && reb->balancor <= 1) return true;
    // RR/LR , rotate to right
    if ( reb->balancor < -1 ) {
        // LR, rotation+1
        if ( reb->c[0]->balancor > 0 ) reb->c[0] = reb->c[0]->rotate(AVLTraits::RotDir::LEFT);
        reb = reb->rotate(AVLTraits::RotDir::RIGHT);
    }
    // LL/RL , rotate to left
    if ( reb->balancor > 1) {
        // RL, rotation+1
        if ( reb->c[1]->balancor < 0 ) reb->c[1] = reb->c[1]->rotate(AVLTraits::RotDir::RIGHT);
        reb = reb->rotate(AVLTraits::RotDir::LEFT);
    }
    return true;
}



template <typename T, typename A>
AVLNode<T> * AVLTree<T,A>::findInternal(T& t){
    AVLNode<T> * temp = root;
    for (int direction = cmpFn(temp->data, t); temp !=nullptr ; temp = temp->c[direction] ) {
        if (direction == 0) return temp;
    }
    return nullptr;
}

/*
template <typename T, typename A>
T& AVLTree<T,A>::find(T& t){
    AVLNode<T> * temp = findInternal(t);
    if ( temp != nullptr)
    return nullptr;
}
*/

template <typename T, typename A>
void AVLTree<T,A>::remove(T& t)
{
    
}

/* -------------------------------------Iterator--------------------------------------------- */

// Can simply think iterator as a delegate class
template <typename T> 
class AVLTreeIter
{
    private:
    AVLNode<T>* node;

    public:
    // typedefs
    typedef T                 value_type;
    typedef value_type*       pointer;
    typedef const value_type* const_pointer;
    typedef value_type&       reference;
    typedef const value_type& const_reference;
    typedef std::size_t       size_type;
    typedef std::ptrdiff_t    difference_type;
    typedef std::random_access_iterator_tag iterator_category;

    AVLTreeIter(AVLNode<T>* n):node(n) {}
    // Copy constructor
    AVLTreeIter(const AVLTreeIter& it):node(it.node) {  }

    // Destructor
    // ~AVLTreeIter();

    AVLTreeIter& operator=(const AVLTreeIter& it) { node = it.node; }
    bool operator==(const AVLTreeIter& it) const { return node == it.node; }
    bool operator!=(const AVLTreeIter& it) const { return node != it.node; }
    
    AVLTreeIter& operator++();
    AVLTreeIter operator++(int);
    AVLTreeIter& operator--();
    AVLTreeIter operator--(int);

    reference operator*() const { return node->data;}
    pointer operator->() const { return &(node->data);}

    //reference operator[](size_type) const;
    
};

template <typename T>
AVLTreeIter<T>& AVLTreeIter<T>::operator++() {
    assert(this->node!=0);
    this->node = this->node->next;
    return *this;
}

// Note the return value is RValue
template <typename T>
AVLTreeIter<T> AVLTreeIter<T>::operator++(int) {
    AVLTreeIter<T> copy = *this;
    ++(*this);
    return copy;
}

template <typename T>
AVLTreeIter<T>& AVLTreeIter<T>::operator--() {
    assert(this->node!=0);
    this->node = this->node->prev;
    return *this;
}

