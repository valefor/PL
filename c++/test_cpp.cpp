#include <iostream>
#include <vector>
#include "stdio.h"

class A {
    public:
    A() {
        std::cout << "Class A" << std::endl; 
    }
};

class B: public A {
    int e;
    public:
    B() {
        std::cout << "Class B" << std::endl; 
    }
};

class C: public A {
    public:
    C() {
        std::cout << "Class C" << std::endl; 
    }
};

class E :public A{
    int e;
};

class F :public A{
};

class G :public A{
};

class H :public A{
};

class I :public A{
};

class J :public A{
};

class D: public A,public B,public C,public F,public G,H {

    public:
    mutable int d;
    D() {
        std::cout << "Class D" << std::endl; 
    }
    
    void setD(int newD) const { d = newD; }
};

class Base {
    public:
    int i,j,k;

    Base() : i(0),j(0),k(0) {}

    virtual void func() {
        std::cout << "vfunction in Base" << std::endl; 
    }
};

class Derived : public Base{
    public:
    virtual void func() {
        std::cout << "vfunction in Derived" << std::endl; 
    }
};

template<class classType,
        class dataType1,
        class dataType2>
const char *
accessOrder(
    dataType1 classType::*member1,
    dataType2 classType::*member2) {
    classType ct;
    printf("& member1 = %p\n", &(ct.*member1));
    printf("& member2 = %p\n", &(ct.*member2));

    return 
         &(ct.*member1) < &(ct.*member2)
        ?"member 1 occurs first"
        :"member 2 occurs first";
}

class X { };
class X_D1 : public virtual X { public: int j; };
class X_D2 : public virtual X { public: double d; };
class X_DD : public X_D1,public X_D2 { public: int k; };

//void foo (const A *pa) { pa->i = 1024; }

/*
typedef struct desc_struct {
	unsigned long a,b;
} desc_table[256];

desc_table idt,gdt;

int funca()
{
    static int counter_a = 0;
    printf("This is function [a]\n");
    printf("Counter [a] value:%d\n",counter_a ++);
    
}

int funcb()
{
    int counter_b =0;
    printf("This is function [b]\n");
    printf("Counter [b] value:%d\n",counter_b ++);
}

int funcc()
{
    printf("This is function [c]\n");
}

int funcd(int counter)
{
    printf("This is function [d]\n");
    printf("Counter [d] value:%d\n",counter);
}

int funce(int x,int y)
{
    printf("Value of X:%d\n",x);
    printf("Value of Y:%d\n",y);
}

void funcf(int & x)
{
    printf("Value of X:%d\n",x);
}

fn_ptr function_table [] = {&funca,&funcb,&funcc};

void hello(char * msg )
{
    printf("%s adrian.\n",msg);
}

int main()
{
    struct desc_struct ldt[3] = {{0,0},{0x9f,0xc0fa00},{0x9f,0xc0f200},};
    int integer = 1;
    int (*f)();
    f = funcc;
    printf("%d\n",sizeof(int));
    printf("%d\n",sizeof(struct desc_struct));
    printf("%d\n",sizeof(desc_table));
    printf("%d\n",sizeof(ldt));
    printf("%d\n",sizeof(idt));
    hello("hello");
    function_table[0]();
    function_table[0]();
    function_table[1]();
    function_table[1]();
    funcd(integer);
    funce(integer,++integer);
    funcf(integer);
    f();
}
*/

template <typename type>
void
printVector(std::vector<type> vec) {

    // !!! 'typename'
    // you must use 'typename' to tell compiler to treat 
    // 'std::vector<type>::iterator' as a type
    typename std::vector<type>::iterator it;

    std::cout << '[';
    for ( it = vec.begin(); it < vec.end(); it++ ) {
        std::cout << ' ' << *it;
    }
    std::cout << ']' << std::endl;
}

void stl_vector_test() {
    std::vector<int> myVector(1,100);

    std::vector<int> emptyVector;
    std::cout << "****** testcase:stl_vector_test <> START ******" << std::endl;
    printVector(emptyVector);
    printVector(myVector);
    myVector.insert(myVector.end(), emptyVector.begin(), emptyVector.end());
    emptyVector.insert(emptyVector.end(), myVector.begin(), myVector.end());
    printVector(emptyVector);
    printVector(myVector);
    std::cout << "****** testcase:stl_vector_test <> END ******" << std::endl;
}

int main() {
    D dd;
    D* d = new D();
    std::cout << "sizeof A: " << sizeof(A) << std::endl;
    std::cout << "sizeof B: " << sizeof(B) << std::endl;
    std::cout << "sizeof C: " << sizeof(C) << std::endl;
    std::cout << "sizeof E: " << sizeof(E) << std::endl;
    std::cout << "sizeof F: " << sizeof(F) << std::endl;
    std::cout << "sizeof D: " << sizeof(D) << std::endl;
    std::cout << "& D::d = " << &D::d << std::endl;
    std::cout << "sizeof dd: " << sizeof(dd) << std::endl;
    std::cout << "sizeof d: " << sizeof(*d) << std::endl;

    Base b;
    Derived de;
    Base* bp = new Base(); 
    std::cout << "sizeof Base: " << sizeof(b) << std::endl;
    std::cout << "sizeof Derived: " << sizeof(d) << std::endl;
    std::cout << "sizeof poiter of Base: " << sizeof(*bp) << std::endl;
    std::cout << "& Base::i = " << &Base::i << std::endl;
    std::cout << "& Base::j = " << &Base::j << std::endl;
    printf("& Base::i = %p\n", &Base::i);
    printf("& Base::j = %p\n", &Base::j);
    std::cout << "& Base::func = " << &Base::func << std::endl;

    std::cout << "sizeof X: " << sizeof(X) << std::endl;
    std::cout << "sizeof X_D1: " << sizeof(X_D1) << std::endl;
    std::cout << "& X_D1::j = " << &X_D1::j << std::endl;
    std::cout << "sizeof X_D2: " << sizeof(X_D2) << std::endl;

    std::cout << accessOrder(&Base::j, &Base::k) << std::endl;

    stl_vector_test();
}
