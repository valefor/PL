#include <iostream>
#include <vector>
#include "typelist.h"


class A {
    public:
    int i,j;
    int f() { return 1;}
    
};

/* Template should be defined as stateless,
 *  that means all data members shall be static const

*/
template <unsigned long N>
struct Binary
{
    // The value on right side is evaluate before compile phase
    static unsigned const value = Binary<N/10>::value * 2 + N%10;
};

template <>
struct Binary<0>
{
    static unsigned const value = 0;
};

using TL::TypeList;

int main()
{
    A a();
    typedef int (A::* p2mAf)();
    p2mAf p = &A::f;
    const bool iterIsPtr = TypeTraits<std::vector<int>::iterator>::isPointer;
    const bool isMemberPtr = TypeTraits<p2mAf>::isMemberPointer;

    //typedef TypeList< int, TypeList<char,NullType> > simpleList;
    typedef TYPELIST2(int,char) simpleList;

    std::cout << "vector<int>::iterator is " << (iterIsPtr? "fast":"smart") << std::endl;
    std::cout << "A::f * is " << (isMemberPtr? "":"not") << " member poiter" << std::endl;
    std::cout << "The length of simpleList is " << simpleList::length << std::endl;

    std::cout << "The size of simpleList[0] is " << sizeof(simpleList::At<0>::type) << std::endl;
    std::cout << "The size of simpleList[1] is " << sizeof(simpleList::At<1>::type) << std::endl;

    simpleList::At<0>::type intVar = 100;
}
