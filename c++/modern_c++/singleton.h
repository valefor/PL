#include <memory>
#include <iostream>
#include <cstdlib>

class Singleton {
    
    private:
    // static variable members declaration
    // the definition must be put outside class
    static bool destroyed_ ;
    static Singleton * instance_ ;
    
    // instance_ may not be initialied first
    // static Singleton instance_;

    // Disable default constructors
    Singleton() {}
    Singleton(const Singleton&);
    Singleton& operator=(const Singleton&);
    virtual ~Singleton() {
        instance_ = nullptr;
        destroyed_= true;
        std::cout << "destory instance\n";
    }

    static void createInstance() { 
        // instance_ = new Singleton();
        // Meyers singleton
        // Static object in function only being initialized at it's first time being called
        static Singleton obj;
        instance_ = &obj;
    }
    static void destoryInstance() {
        // Call the destructor manually
        instance_->~Singleton();
    }
    static void onDeadReference() {
        //! Note implementation details of createInstance, it uses static local var
        //  Every time you call it, 'instance_' is always assigned with the address
        //  of that static local var, that's elegant since we don't waste any memory
        //  when we want to reuse/nirvana 'instance_'
        std::cout << "reborn instance\n";
        createInstance();
        new (instance_) Singleton;
        std::atexit(destoryInstance);
        destroyed_=false;
    }

    public:
    static Singleton & getInstance();
};


