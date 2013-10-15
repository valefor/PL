#include "list.h"

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


/*
 * BST
 *
 */
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
