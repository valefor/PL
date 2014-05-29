#include <pthread.h>
#include <iostream>
#include <limits.h>

class Runable 
{
    public:
    virtual void run() = 0;
};

template <typename T>
class Thread : public T
{
    // Default stack size is PTHREAD_STACK_MIN=65536
    size_t stackSize_;
    bool createDetached_;
    pthread_t tid_;
    pthread_attr_t attr_;

    void spawnNewThread();

    // Thread entrance
    static void* threadStarter(void* threadPtr);
    public:
    Thread(): stackSize_(1024*10*64),createDetached_(false) {
        std::cout << "Thread inited!" << std::endl;
    };
    Thread(bool detacted);
    Thread(size_t sz, bool detacted): stackSize_(sz),createDetached_(detacted) {
        std::cout << "Thread inited!" << std::endl;
    };
    void start();
    void join();

    //virtual ~Thread();
};

template <typename T>
Thread<T>::Thread(bool detacted):stackSize_(1024*10*64), createDetached_(detacted) {
    int ret;
    if ((ret = pthread_attr_init(&attr_) != 0)) {
        std::cerr << "Failed to init pthread attributes, error code:" << ret << std::endl;
    }

    // Set thread's stack size
    if ((ret = pthread_attr_setstacksize(&attr_, stackSize_) != 0))
    {
        std::cerr << "Failed to set pthread stack size:" << stackSize_ <<",error code:" << ret << std::endl;
    }

    // If the thread is created un-joinable, set 'PTHREAD_CREATE_DETACHED' attribute
    if (createDetached_)
    {
        if ((ret = pthread_attr_setdetachstate(&attr_, PTHREAD_CREATE_DETACHED) != 0))
        {
            std::cerr << "Failed to set PTHREAD_CREATE_DETACHED, error code:" << ret << std::endl;
        }
    }
    std::cout << "Thread inited!" << std::endl;
};

/*
template <typename T>
Thread::~Thread() {
    // Nothing todo
}
*/

template <typename T>
void* Thread<T>::threadStarter(void* threadPtr) {
    T * ra = static_cast<T*>(threadPtr);
    std::cout << "Thread start!" << std::endl;
    ra->run();
    std::cout << "Thread finished!" << std::endl;
}

template <typename T>
void Thread<T>::spawnNewThread() {
    pthread_attr_t attr;
    int ret;
    /*

    if ((ret = pthread_attr_init(&attr) != 0)) {
        std::cerr << "Failed to init pthread attributes, error code:" << ret << std::endl;
    }

    //std::cerr << "Minimum pthread stack size:" << PTHREAD_STACK_MIN << std::endl;

    // Set thread's stack size
    if ((ret = pthread_attr_setstacksize(&attr, stackSize_) != 0))
    {
        std::cerr << "Failed to set pthread stack size:" << stackSize_ <<",error code:" << ret << std::endl;
    }

    // If the thread is created un-joinable, set 'PTHREAD_CREATE_DETACHED' attribute
    if (createDetached_)
    {
        if ((ret = pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED) != 0))
        {
            std::cerr << "Failed to set PTHREAD_CREATE_DETACHED, error code:" << ret << std::endl;
        }
    }
    */

    // Create thread
    if ((ret = pthread_create(&tid_, &attr_, &threadStarter, static_cast<void*>(this))) != 0)
    {
        std::cerr << "Failed to start thread, error code:" << ret << std::endl;
    }
        
}

template <typename T>
void Thread<T>::start() {
    spawnNewThread();
}

template <typename T>
void Thread<T>::join() {
    int ret;
    if ((ret = pthread_join(tid_, NULL)) != 0)
    {
        std::cerr << "Failed to join pthread, error code:" << ret << std::endl;
    }
}

