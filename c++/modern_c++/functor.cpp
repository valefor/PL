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

class Cat
{
    public:
    void eat() {
        std::cout << "Moamu,Moamu...\n";
    }

    void speak() {
        std::cout << "Miaou,Miaou...\n";
    }
    
};

int main()
{
    TestFunctor t;
    Cat miao;

    Functor<void, TYPELIST2(int,double)> myFunctor(t);
    Functor<void, NullType> catFunctor(&miao, &Cat::eat);
    myFunctor(4,6.5);
    catFunctor();
}
