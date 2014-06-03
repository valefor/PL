#ifndef _H_UTIL__
#define _H_UTIL__
class NonCopyable
{
    protected:
    NonCopyable();
    ~NonCopyable();

    private:
    NonCopyable(const NonCopyable& nc);
    NonCopyable& operator=(const NonCopyable& other);
};

class NonNewable
{
};

class NonStackObj
{
};
#endif
