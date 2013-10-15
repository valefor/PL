// wrap standard POSIX mutex(pthread_mutex_t)
#include <pthread.h>
#include <iostream>
#include <cerrno>
#include <string.h>

class ILockable {

    public:
    virtual bool lock() = 0;
    virtual bool unlock() = 0;
    virtual ~ILockable() = 0;
};

class Lock: public ILockable{
    public:

    explicit Lock(ILockable& lockable);
    virtual ~Lock();

    private:
    // disallowed dynamic allocation
    void * operator new(size_t size);
    void * operator new[](size_t size);

    bool isLocked_;
    ILockable& itsLockable;

};

class Mutex : public ILockable{
    private:

    pthread_mutex_t mutex_;

    public:

    explicit Mutex();
    ~Mutex();

    inline bool lock() {
        const int res = pthread_mutex_lock(&mutex_);
        
        switch (res) {
            
        case 0: 
            return true;
        case EDEADLK :
            return false;
        default:
            std::cerr << "Failed to lock mutex, res: " << res << std::endl;
            return false;
        }
    
    }

    inline bool unlock() {
        int res;
        
        if ((res = pthread_mutex_unlock(&mutex_)) != 0)
        {
            std::cerr << "Failed to unlock mutex, res: " << res << std::endl;
        }

    }

    pthread_mutex_t * getMutex();
};
