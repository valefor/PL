/*
    Notes:
    The named rvalue would be treated as lvalue
    int && a = 0; // a is lvalue, 0 is rvalue

    int && foo();// function declaration
    int && a = foo();// The returned valus is rvalue

    Reference collapsing in templates:

template <typename T>
void tempFunc(T && t) // Notice: here the 't' is always lvalue
{
    // ...
};

    // P = parameter declaration
    // A = argument

    T   +   P   =>  A
    U       &       U&
    U       &&      U&&
    U&      &&      U&
    U&&     &&      U&&
    

    P   +   A   =>  T
    T&      U&      U&
    T&&     U&      U&
    T&      U&&     U&
    T&&     U&&     U

*/

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

int && foo();

int main() 
{
    // error: int &a = 0;
    const int &a = 0;
    int  i;
    int && b= foo();// foo() is rvalue, but b is lvalue
    //myMove(i);// type of 'i' is int
    myMove(2);
    int && c = myMove(b);   // myMove now returns int object(rvalue) referred by b(int &)
    int r = int();          // Actually here is a copy
    const int &cr = int();
  
    f(i);   // Type of i will be deduced as 'int &'
    f(2);   // Type of 2 will be deduced as 'int &&'

    //cf(i);// can not bind 'int' lvalue to 'const int&&'
}
