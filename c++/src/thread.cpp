#include "thread.h"

class Runable 
{
    virtual run() = 0;
};

template <typename T>
class Thread : public T
{
    size_t stackSize;

    public:
    Thread() {}

    virtual ~Thread();

};

Thread::~Thread() {
    // Nothing todo
}
