// smart pointer

// simulate auto_ptr's functionality
template<typename T>
class SmartPtr {
    public:
    typedef T* PtrType;
    typedef T ValueType;
    typedef T& RefType;
    explicit SmartPtr(T * obj);
    ~SmartPtr();

    private:
    // Disable dynamic allocation
    void* operator new(size_t size);
    void* operator new[] (size_t size);

    PtrType obj_;
};

template<typename T>
class SharedPtr {

    public:
    explicit SharedPtr(T *obj);
    SharedPtr(SharedPtr& r);
};
