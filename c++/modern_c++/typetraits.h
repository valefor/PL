class NullType;
struct EmptyType {};

template <typename T>
class TypeTraits
{
    private:
    template <class U> struct PointerTraits
    {
        enum { result = false };
        typedef NullType PointeeType;
    };

    template <class U> struct PointerTraits<U*>
    {
        enum { result = true };
        typedef U PointeeType;
    };


    template <class U> struct ClassMemberTraits
    {
        enum { result = false };  
    };

    template <class U, class V> struct ClassMemberTraits<U V::*>
    {
        enum { result = true };  
    };

    public:

    enum { 
        isPointer = PointerTraits<T>::result,
        isMemberPointer = ClassMemberTraits<T>::result
    };
    typedef typename PointerTraits<T>::PointeeType PointeeType;

};
