
class Singleton {
    
    private:
    // static member variable declaration
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
    }

    static void createInstance() { 
        // instance_ = new Singleton();
        // Meyers singleton
        // Static object in function only being initialized at it's first time being called
        static Singleton obj;
        instance_ = &obj;
    }
    static void onDeadReference() { /*Some warning*/ }

    public:
    static Singleton & getInstance();
};


