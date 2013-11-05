#include <stdio.h>
#include <string.h>
#include <dmalloc.h>

/* 
 * Basically, memory problems are all in these classes
 *  1,memory being allocated but not be freed
 *  2,free memory that's not allocated
 *  3,fence-post, aka overruns the bundaries of a malloc() memory allocation
 *  or touches a memory allocation that has been released by free().
 *
 */

/* Think about how to do memory leak check?
 *  Almost all memo-leak check tools work by replacing malloc, free and other memery
 *  management calls, each tools has code to intercept calls to malloc and set up tracking
 *  information for each memeory request.
 *
 */

// Here are simple implementation prototype
 
/* replace malloc/free by macro*/
//#define malloc() _xmalloc()

/* 
 * Build up your wrapped memory management functions
 *
 *  For example
 */
// Define header which will be put before the allocated memory.


#define app_malloc(size) app_mallocFct(size,__FILE__, __LINE__)

void * app_mallocFct(size_t size, const char* filename, int line_no) {

    
}

// disable default memory management functions
// NOTE! the following macro must be defined just before your code and 
//  after includes and pre-defined memory management functions
#ifdef malloc
#define APP_MALLOC_ERR  err_malloc_disabled_use_app_malloc_instead
#define malloc(x) APP_MALLOC_ERR
#endif

#ifdef free
#define APP_FREE_ERR  err_free_disabled_use_app_malloc_instead
#define free(x) APP_FREE_ERR
#endif

// test dmalloc library
// using "g++ -o test memory.cpp -ldmalloc -DDMALLOC -DDMALLOC_FUNC_CHECK"
int main() {
    int *p = (int *)malloc(sizeof(int));

    char *pc =(char*)malloc(5);

    //strcpy(pc,"fency post!");
    
    return 0;
}
