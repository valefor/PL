#include <iostream>
#include <vector>
#include "typelist.h"


class A {
    public:
    int i,j;
    int f() { return 1;}
    
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
}
