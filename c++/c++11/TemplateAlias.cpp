#include <vector>
#include <array>

using MyAllocator = int;

template<typename T>
using MyAllocVec = std::vector<T,MyAllocator>;

template<std::size_t N>
using StringArray = std::array<std::string, N>;
StringArray<15> sa;
