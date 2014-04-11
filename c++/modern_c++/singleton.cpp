#include "singleton.h"

// definition of static member variable here
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

    Singleton& st = Singleton::getInstance();
}
