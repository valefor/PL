#include <iostream>

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
    int d;
    public:
    D() {
        std::cout << "Class D" << std::endl; 
    }
};

class Base {
    public:
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

class X { public : int i; };
class X_D1 : public virtual X { public: int j; };
class X_D2 : public virtual X { public: double d; };
class X_DD : public X_D1,public X_D2 { public: int k; };

void foo (const A *pa) { pa->i = 1024; }

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

int main() {
    D dd;
    D* d = new D();
    std::cout << "sizeof A: " << sizeof(A) << std::endl;
    std::cout << "sizeof B: " << sizeof(B) << std::endl;
    std::cout << "sizeof C: " << sizeof(C) << std::endl;
    std::cout << "sizeof E: " << sizeof(E) << std::endl;
    std::cout << "sizeof F: " << sizeof(F) << std::endl;
    std::cout << "sizeof D: " << sizeof(D) << std::endl;
    std::cout << "sizeof dd: " << sizeof(dd) << std::endl;
    std::cout << "sizeof d: " << sizeof(*d) << std::endl;

    Base b;
    Derived de;
    Base* bp = new Base(); 
    std::cout << "sizeof : " << sizeof(b) << std::endl;
    std::cout << "sizeof : " << sizeof(d) << std::endl;
    std::cout << "sizeof : " << sizeof(*bp) << std::endl;
}
