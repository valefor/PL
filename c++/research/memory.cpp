#include <stdio.h>
#include <string.h>
#include <dmalloc.h>

// Basically, memory problems are all in these classes
// 1,memory being allocated but not be freed
// 2,free memory that's not allocated
// 3,fence-post, aka overruns the bundaries of a malloc()memory allocation
//  or touches a memory allocation that has been released by free().

// test dmalloc library
// using "g++ -o test memory.cpp -ldmalloc -DDMALLOC -DDMALLOC_FUNC_CHECK"
int main() {
    int *p = (int *)malloc(sizeof(int));

    char *pc =(char*)malloc(5);

    //strcpy(pc,"fency post!");
    
    return 0;
}
