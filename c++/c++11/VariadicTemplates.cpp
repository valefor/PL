// Variadic Templates
#include <cstddef>
#include <iostream>
#include <stdexcept>

template <typename ... Args> class Tuple;

template <std::size_t ... Entries> 
class StaticArray {
    // '...' on the right means unpack
    // '...' on the left means pack
    std::size_t array[] = {Entries...};
    
    public:
    StaticArray();
};

// template function
template <typename T>
void output(T&& t) {
    std::cout << t;
}

template <typename First, typename ... Args>
void output(First&& f, Args&&... args) {
    std::cout << f << " ";
    // !Note, 'std::forward<Args>(args)...'
    output(std::forward<Args>(args)...);
}

void printf(const char * s) {
    while(*s) {
        if(*s == '%') {
            if(*(s+1) == '%') ++s;
            else throw std::runtime_error("invalid format string:missing argument");
        }
        std::cout << *s++;
    }
}

template <typename T, typename ... Args>
void printf(const char * s, T value, Args ... args) {
    while(*s) {
        if(*s == '%') {
            if(*(s+1) == '%') {
                ++s;
            } else {
                std::cout << value;
                printf(s+1, args...);
                return;
            }
        }
        std::cout << *s++;
    }
    throw std::logic_error("extra argument provided to printf");
}

int main() {
    //StaticArray<1,2,3> a3;
    printf("Hello");
    printf("%s"," world");

}
