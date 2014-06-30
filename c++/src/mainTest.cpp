/*
 * The main test routine
 *
 */
#include "list.h"
#include "mutex.h"
#include "ptr.h"
#include "sem.h"
#include "tree.h"
#include "thread.h"
#include "stdio.h"
#include <iostream>
#include <semaphore.h>
#include "util.h"

#define MAXSIZE 100

Mutex mt;
Semaphore sem;
int stack[MAXSIZE][2];
int pos = 0;

const void* intKey(const void* i)
{
    return i;
}

int intCmp(const int& i, const int& j)
{
    if (i==j) return 0;
    return i>j? 1 : -1;
}

class Producer: public Runable
{
    static int count;
    int id;
    public: 
    Producer():id(count){
        ++count;
        std::cout << "this is producer:" << id << std::endl; 
    }

    void run();
    
};

void Producer::run()
{
    // Lock lock(mt);
    for (int i = 0; i < 100; ++i ) {
        // std::cout << "[" << id  <<"]produce:" << i << std::endl;
        printf("[%d]produce:%d\n", id ,i);
        stack[pos][0] = i++;
        stack[pos][1] = i;
        sem.post();
        ++pos;
    }
}

class ConsumerA: public Runable
{
    public:
    void run() {
        while (1) {
        sem.wait();
        // std::cout << stack[pos][0] + stack[pos][1] << std::endl;
        printf("Plus: %d+%d=%d\n",stack[pos][0],stack[pos][1], stack[pos][0] + stack[pos][1] );
        --pos;
        }
    }
};

class ConsumerB: public Runable
{
    public:
    void run() {
        while (1) {
        sem.wait();
        // std::cout << stack[pos][0] * stack[pos][1] << std::endl;
        printf("Multiply: %d*%d=%d\n",stack[pos][0],stack[pos][1], stack[pos][0] * stack[pos][1] );
        --pos;
        }
    }
};


// Static variable definitions
int Producer::count = 0;

int main(int argn, char ** argv) {

    //List Test
    List<int> l;
    l.append(1).append(2).append(3).append(4);
    l.reverse();
    l.print();

    // AVL Tree Test
    AVLTree<int> t(intKey,intCmp);
    t.insert(5);
    t.insert(1);
    t.insert(2);
    t.insert(3);

    AVLTree<int>::iterator it = t.begin();
    while ( it != t.end()) {
        std::cout << *it << std::endl;
        ++it;
    }

    // Threading Test
    // sem_init(&sem,0,0);
    Thread<Producer> pt1(false),pt2(false),pt3(false);
    Thread<ConsumerA> cta(false);
    Thread<ConsumerB> ctb(false);
    pt1.start();
    pt2.start();
    pt3.start();

    cta.start();
    ctb.start();

    pt1.join();
    //pt2.join();
    //pt3.join();

    cta.join();
    ctb.join();

    class A : NonNewable {
    };

    // A a = new A; error, operator new is private
    A a; // fine
}
