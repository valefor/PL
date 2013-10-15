#include "mutex.h"

Lock::~Lock() {

    if (isLocked_) {
        itsLockable.unlock();
    }
}

Lock::Lock(ILockable& lockable) :itsLockable(lockable) {
    isLocked_ = itsLockable.lock();
}


Mutex::Mutex() {

    memset(&mutex_, 0, sizeof(pthread_mutex_t));

    int ret;
    ret = pthread_mutex_init(&mutex_, 0);                                           
    if (ret != 0)
    {
        std::cerr << "Failed to init mutex, error: " << ret << std::endl;
    }

}

Mutex::~Mutex() {

    memset(&mutex_, 0, sizeof(pthread_mutex_t));

    int ret;

    if( (ret = pthread_mutex_destroy(&mutex_)) != 0 ) {
        std::cerr << "Failed to destroy mutex, error: " << ret << std::endl;
    }

}

pthread_mutex_t * Mutex::getMutex() {
    return &mutex_;
}
