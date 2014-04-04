// TypeList
#include "typetraits.h"

namespace TL
{

template <class T, class U> struct TypeList;

template <class NullType>
struct TypeListLength
{
    enum { value = 0 };
};

template <class T, class U>
struct TypeListLength< TypeList<T,U> >
{
    enum { value = 1 + TypeListLength<U>::value };
};

template <class T, class U, unsigned int i> struct At< TypeList<T,U>, 0 >
{
    typedef typename TypeList::Head Result;
};

template <typename TypeList, unsigned int i> struct At< TypeList, i >
{
    typedef typename At<TypeList<Tail>,i-1>::Result Result;
};

template <class T, class U>
class TypeList
{
    public:
    
    typedef T Head;
    typedef U Tail;

    enum { length = TypeListLength< TypeList<T,U> >::value };
};

#define TYPELIST1(T1) TypeList<T1, NullType >
#define TYPELIST2(T1,T2) TypeList<T1, TYPELIST1(T2) >
#define TYPELIST3(T1,T2,T3) TypeList<T1, TYPELIST2(T2,T3) >
#define TYPELIST4(T1,T2,T3,T4) TypeList<T1, TYPELIST3(T2,T3,T4) >

}
