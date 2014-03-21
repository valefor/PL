#include "tree.h"
#include <functional>

class AVLTraits {
    public:
    enum CmpResult {
        LT = -1,
        EQ = 0,
        BT = 1
    };
    class RotDir {
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
    AVLNode * c[2]; // c[0] - left sub tree, c[1] - right sub tree;

    // The c({}) is the new Syntactic sugar of c++0x
    // T must support copy operation
    AVLNode(T t): data(t), balancor(0),c({nullptr,nullptr}) {}

    AVLNode * rotate(AVLTraits::RotDir::value dir);
};

template <typename T>
AVLNode<T> * AVLNode<T>::rotate(AVLTraits::RotDir::value dir=AVLTraits::RotDir::LEFT)
{
    AVLNode<T> * childr,childl,parent=this; 
    if (dir == AVLTraits::RotDir::LEFT) {
        childl = parent->c[0];
        childr = parent->c[1];
        childr->c[0] = parent;
        if (childr != nullptr) parent->c[1] = childr->c[0];
        return childr;

    } else {
        childl = parent->c[0];
        childr = parent->c[1];
        childl->c[1] = parent;
        if (childl != nullptr) parent->c[0] = childl->c[1];
        return childl;
    }
}

template <typename T>
class AVLTree 
{
    public:
    
    typedef std::function<const void * (const void * obj)> AVLKeyFn;
    typedef std::function<int (const T& obj, const T& other)> AVLCmpFn;
    AVLTree(AVLKeyFn key, AVLCmpFn cmp): root(nullptr), keyFn(key), cmpFn(cmp), count(0) {}

    bool insert(T t);
    T& find(T t);
    void remove(T& t);
    bool exist(T& t);

    private:
    AVLNode<T> * root;       // user defined data
    AVLKeyFn    keyFn;
    AVLCmpFn    cmpFn;
    int         count;

    AVLNode<T> * findInternal(T& t);
    // Think about how to support iterator
};

// !Note, each time inserting one node will lead a balance-broking of only one parent node 
template <typename T>
bool AVLTree<T>::insert(T t)
{
    if (root == nullptr) {
        root = new AVLNode<T>();
    }
    AVLNode<T> * reb = root;
    AVLNode<T> * parent, child;
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

    parent->c[direction] = child = new AVLNode<T>(t);
    count ++;

    // Update balance factor of all nodes of sub tree which is to be rebalanced 
    for (parent = reb; parent->c[direction] != child ; parent = parent->c[direction]) {
        cmpResult = cmpFn(parent->data, t);
        direction = cmpResult > 0 ? 0 : 1;
        parent->balancor -= cmpResult;
    }

    // Adjust sub tree
    if ( reb->balancor >= -1 && reb->balancor <= 1) return true;
    // RR/LR , rotate to right
    if ( reb->balancor < -1) {
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
template <typename T>
AVLNode<T> * AVLTree<T>::findInternal(T& t){
    AVLNode<T> * temp = root;
    for (int direction = cmpFn(temp->data, t); temp !=nullptr ; temp = temp->c[direction] ) {
        if (direction == 0) return temp;
    }
    return nullptr;
}

template <typename T>
T& AVLTree<T>::find(T& t){
    AVLNode<T> * temp = findInternal(t);
    if ( temp != nullptr)
    return nullptr;
}

template <typename T>
void AVLTree<T>::remove(T& t)
{
    
}
