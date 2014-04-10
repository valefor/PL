#include <iostream>
#include "functor.h"


using TL::TypeList;

struct TestFunctor 
{
    void operator() (int i, double d) {
        std::cout << "TestFunctor::operator(" 
                << i << "," << d << ") called" << std::endl; 
    }
};

int main()
{
    TestFunctor t;
    Functor<void, TYPELIST2(int,double)> myFunctor(t);
    myFunctor(4,6.5);
}
