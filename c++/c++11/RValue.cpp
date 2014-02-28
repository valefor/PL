/*
    Notes:
    The named rvalue would be treated as lvalue
    int && a = 0; // a is lvalue, 0 is rvalue

    int && foo();// function declaration
    int && a = foo();// The returned valus is rvalue

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
typename MyRemoveRef<T>::type myMove(T && t)
{
    return t;
}


int && foo();

int main() 
{
    // error: int &a = 0;
    const int &a = 0;

    int && b= foo();// foo() is rvalue, but b is lvalue
    int && c = myMove(b); // myMove now returns int object(rvalue) referred by b
    int r = int(); // Actually here is a copy
    const int &cr = int();
}
