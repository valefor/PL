/*
    Notes:
    <1>
    Q: HOW TO DETERMINE WHETHER AN EXPRESSION IS AN LVALUE/RVALUE?
    A: Ask "can I take its address?",
        If you can, it's an lvalue.  If you can't, it's an rvalue.
        lvalue: &obj, &*ptr? it's ok
        rvalue: &123, &(x+y)? it's bad

    <2>
    ++x is an lvalue and x++ is an rvalue:
        '++x' modifies and then names the persistent object. However, 
        the expression x++ is an rvalue.  It copies the original 
        value of the persistent object, modifies the persistent object, 
        and then returns the copy.  This copy is a temporary. 
        
    
    <3>
    I,Type& can not bind to rvalue, const lvalue
    II,const Type& can bind to everything, because we don't need to modify it, just observe it
    III,The named rvalue would be treated as lvalue, 
        A reference is a name, so a reference bound to an rvalue is itself an lvalue
    VI,What's the difference between modifiable rvalues and const rvalues?
    Some examples:
    int &a = 0; //error: Type& can not bind to rvalue, accidentally modifying temporaries, 
                //only to have the temporaries evaporate along with your modifications, 
                //would lead to subtle and obnoxious bugs, when temporaries disappear,
                // &a points to hell.
    const int &a = 0; // ok: & can
    int && a = 0; // a is lvalue, 0 is rvalue

    int && foo();// function declaration
    int && a = foo();// The returned valus is rvalue

    
    *****************************************************
    <4>Template argument deduction and reference collapsing:
    I, Argument deduction rule:
    when the function parameter type is of the form T&& where T is a template parameter, and the function argument is an lvalue of type A, the type A& is used for template argument deduction.  (This special rule doesn't apply to function parameter types of the form T& or const T& , which behave as they did in C++98/03, nor does it apply to const T&& .) 
template <typename T>
void tempFunc(T && t) // Notice: here the 't' is always lvalue
{
    // ...
};
    int &i = 2;
    int &&j = 3; // j will be treat as lvalue
    tempFunc(i); // i is int ref,template argument T will be deduced to int&
    tempFunc(j); // j is int ref,template argument T will be deduced to int&
    tempFunc(4); // 4 is int rvalue,template argument T will be deduced to int
    
    ! How to disable template arguments deduction, declare template function like this:
    template <typename T> void tempFunc(templateClass<t> t)

    II, Reference collasping rule:
    T   +   &*   =>  T&*
    X&      &       X&
    X&&     &       X&
    X&      &&      X&
    X/X&&   &&      X&&
    For example:
    T <=> X& , TypeName<T&&> => TypeName<X&>
    T <=> X&& , TypeName<T&&> => TypeName<X&&>


*/
#include <string>
#include <iostream>

// Test - Reference collapsing in templates
template <typename T> struct TypeName;

// template specialization
template < > 
struct TypeName<std::string> 
{
    static const char * get() {
        return "string";
    }
};

template < > 
struct TypeName<const std::string> 
{
    static const char * get() {
        return "const string";
    }
};

template < > 
struct TypeName<std::string&> 
{
    static const char * get() {
        return "string&";
    }
};

template < > 
struct TypeName<const std::string&> 
{
    static const char * get() {
        return "const string&";
    }
};


template < > 
struct TypeName<std::string&&> 
{
    static const char * get() {
        return "string&&";
    }
};

template < > 
struct TypeName<const std::string&&> 
{
    static const char * get() {
        return "const string&&";
    }
};

// Class template
template <typename T>
struct MyRemoveRef
{
    typedef T type; 
};

template <typename T>
struct MyRemoveRef<T&>
{
    typedef T type; 
};

template <typename T>
struct MyRemoveRef<T&&>
{
    typedef T type; 
};

// Function template
template <typename T>
void typeTest(T&& t){
    std::cout << "t: " << t 
        << ", Type of T:" << TypeName<T>::get()
        //<< ", Type of T&:" << TypeName<T&>::get() 
        << ", Type of T&&:" << TypeName<T&&>::get()
        << std::endl;
}

template <typename T>
typename MyRemoveRef<T>::type&& myMove(T && t)
{
    return static_cast<typename MyRemoveRef<T>::type&&>(t);
}

template <typename T>
T&& myForward(typename MyRemoveRef<T>::type& t)
{
    return static_cast<T&&>(t);
}

template <typename T>
T&& myForward(typename MyRemoveRef<T>::type&& t)
{
    return static_cast<T&&>(t);
}

template <typename T>
void f(T&& t);  // The type of t will be deduced as reference(T &) or rvalue(T &&)
                // This hack applies only to T&& parameters, not const T&& parameters
template <typename T>
void cf(const T&& t);

//int && foo();

std::string rvalueString() {
    return std::string("A rvalue string");
}

const std::string constRvalueString() {
    return std::string("A const rvalue string");
}

int main() 
{
    // 
    
    //int  i;
    //int && b= foo();// foo() is rvalue, but b is lvalue
    //myMove(i);// type of 'i' is int
    //myMove(2);
    //int && c = myMove(b);   // myMove now returns int object(rvalue) referred by b(int &)
    //int r = int();          // Actually here is a copy
    //const int &cr = int();
  
    //f(i);   // Type of i will be deduced as 'int &'
    //f(2);   // Type of 2 will be deduced as 'int &&'

    //cf(i);// can not bind 'int' lvalue to 'const int&&'
    std::string &&r1="one";
    std::string r2;
    {
        r2 = myMove(r1);
    }
    std::cout << r2 << std::endl;

    std::string &&ls("A lvalue string");
    const std::string &&cls("A const lvalue string");
    typeTest(ls);
    typeTest(cls);
    typeTest(rvalueString());
    typeTest(constRvalueString());
    myForward<std::string>(ls);
    /* Out put:
        t: A lvalue string, Type of T:string&, Type of T&&:string&
        t: A const lvalue string, Type of T:const string&, Type of T&&:const string&
        t: A rvalue string, Type of T:string, Type of T&&:string&&
        t: A const rvalue string, Type of T:const string, Type of T&&:const string&&
    */
}
