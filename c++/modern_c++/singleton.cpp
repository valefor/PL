#include "singleton.h"

// Definition of static variable members here
// Because static members belong to class instead of instance
// they are very like function members
bool Singleton::destroyed_ = false;
Singleton * Singleton::instance_  = nullptr;

Singleton & Singleton::getInstance() {
    if(!instance_) {
        if (destroyed_) {
            onDeadReference();
        } else {
            createInstance();
        }
    }

    return *instance_;
}

int main() {

    {
        Singleton& st = Singleton::getInstance();
    }
}
