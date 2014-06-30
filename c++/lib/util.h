#ifndef _H_UTIL__
#define _H_UTIL__

// Disable copy constructor and assignment operator
class NonCopyable
{
    protected:
    NonCopyable();
    ~NonCopyable();

    private:
    NonCopyable(const NonCopyable& nc);
    NonCopyable& operator=(const NonCopyable& other);
};

// Must disable new/new[]
class NonNewable
{
    private:
    void * operator new(size_t size);
    void * operator new[] (size_t size);
};

// Disable default constructor and new operators
// Don't think you can call constructor manually.it's impossible
class NonStackObj
{
    private:
    NonStackObj() {}
    void * operator new(size_t size) {
        return ::operator new(size);
    }
    void * operator new[] (size_t size);

    public:
    static NonStackObj* createInstance(){
        return new NonStackObj;
    }
    void deleteInstance() {delete this;}
};
#endif
