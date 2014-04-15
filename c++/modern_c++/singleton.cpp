#include <mutex>

#include "singleton.h"


// Definition of static variable members here
// Because static members belong to class instead of instance
// they are very like function members
bool Singleton::destroyed_ = false;
Singleton * Singleton::instance_  = nullptr;

std::mutex mutex_;

// In c++11, can also use std::lock_guard
class Lock {
    
    std::mutex * mt_;
    bool isLocked;

    public:
    Lock(std::mutex * mt) : mt_(mt) {
        while( !(isLocked = mt_->try_lock()) ){}
    }

    ~Lock() {
        if (isLocked) {
            mt_->unlock();
            isLocked = false;    
        }
    }
};

Singleton & Singleton::getInstance() {
    if(!instance_) {
        // Double-checked locking
        std::lock_guard<std::mutex> lock(mutex_);
        // From now it's thread-safe.
        if (!instance_) {
            if (destroyed_) {
                onDeadReference();
            } else {
                createInstance();
            }
        }
    }

    return *instance_;
}

int main() {

    {
        Singleton& st = Singleton::getInstance();
    }
}
