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

/* 
 * TypeList<T,U> is also a type 
 *  Types of Type<0> and Type<1> are different 
 */

template <class TList, unsigned int i> struct TypeAt;

template <unsigned int i>
struct TypeAt< NullType, i >
{
    typedef NullType result;
};

template <class T, class U>
struct TypeAt< TypeList<T,U>, 0 >
{
    typedef T result;
};

template <class T, class U, unsigned int i>
struct TypeAt< TypeList<T,U>, i >
{
    typedef typename TypeAt<U,i-1>::result result;
};

template <class T, class U>
class TypeList
{
    public:
    
    typedef T head;
    typedef U tail;
    
    template<unsigned int i>
    struct At
    {
        typedef typename TypeAt<TypeList<T,U>,i>::result type;
    };

    enum { length = TypeListLength< TypeList<T,U> >::value };
};

#define TYPELIST1(T1) TypeList<T1, NullType >
#define TYPELIST2(T1,T2) TypeList<T1, TYPELIST1(T2) >
#define TYPELIST3(T1,T2,T3) TypeList<T1, TYPELIST2(T2,T3) >
#define TYPELIST4(T1,T2,T3,T4) TypeList<T1, TYPELIST3(T2,T3,T4) >

}
