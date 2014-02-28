#include <iostream>
#include <memory>

std::weak_ptr<int> wp;
/* Notes *
    * std::weak_ptr *
    expired - checks whether the referenced object was already deleted
    lock    - creates a shared_ptr that manages the referenced object 

    * std::unique_ptr * ,using this instead of auto_ptr considering:
    1 - operators supported(==, !=, < ...),so they can be used/stored in STL
        which uses/relies on a lot of these operations
    2 - array supported, can point to an array
*/
void f()
{
    if ( !wp.expired()) {
        auto spt = wp.lock();
        std::cout << *spt << "\n";
        std::cout << wp.use_count() << "\n";
    }
    else {
        std::cout << "wp is expired\n";
    }
}

int main() 
{
    {
        auto sp = std::make_shared<int>(42);
        wp = sp;
        f();
    }
    f();
}
