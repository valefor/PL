#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <sched.h>
#include <pthread.h>

//#include <dmalloc.h>

/* 
 * Basically, memory problems are all in these classes
 *  1,memory being allocated but not be freed
 *  2,free memory that's not allocated
 *  3,fence-post, aka overruns the bundaries of a malloc() memory allocation
 *  or touches a memory allocation that has been released by free().
 *
 */

/* Think about how to do memory leak check?
 *  Almost all memo-leak check tools work by replacing malloc, free and other memory
 *  management calls, each tools has code to intercept calls to malloc and set up tracking
 *  information for each memeory request.
 *
 */

// Here is a simple implementation prototype
//  No fence-post check, not support for multi-thread
 
/* replace malloc/free by macro*/
//#define malloc() _xmalloc()
#ifdef DERR
#error "This is an error message, you would see this when you open DERR flag at compile time..."
#endif

/* 
 * Build up your wrapped memory management functions
 *
 *
 *  For example:
 */

// How to support multi-thread 
#define SPINLOCK_INIT_VALUE 0
#define SPINLOCK_SET_VALUE 1

typedef volatile uint32_t spinlock_t;
static void lock(spinlock_t* lock) {
    // GNU CAS implemetation
    while (__sync_val_compare_and_swap(lock, SPINLOCK_INIT_VALUE, SPINLOCK_SET_VALUE) == 1)
        sched_yield();
}

static void unlock(spinlock_t* lock) {
    *lock = SPINLOCK_INIT_VALUE;
}

#define SLOT_MAX 257
#define BUCKET_MAX 1023
// Define header which will be put before the allocated memory.
typedef struct memHeader_s {
    struct memHeader_s * next;

    size_t size;
    const char * file;
    int lineno;
} memHeader_t;

typedef struct memBucket_s{
    spinlock_t lock;
    uint32_t nbOfAlloc;
    memHeader_t * table[BUCKET_MAX];
} memBucket_t;

// global variables will be initialized by compiler
memBucket_t mem_table[SLOT_MAX];
bool isRegistered = false;

void mem_check() {

    memHeader_t * p ;
    for (int i = 0; i < SLOT_MAX; i++) {
        for (int j = 0; j < BUCKET_MAX; j++) {
            p = mem_table[i].table[j];
            while (p) {
                printf("[ERROR]Unfreed memory:%p(%d bytes), at %s:%d\n",
                    p, p->size, p->file, p->lineno);
                p = p->next;
            }
        }
    }
}
#define app_malloc(size) app_mallocFct(size,__FILE__, __LINE__)
#define app_free(size) app_freeFct(size,__FILE__, __LINE__)

void * app_mallocFct(size_t size, const char* filename, int lineno) {

    if (!isRegistered) {
    
        //for (int i = 0; i < BUCKET_MAX; i++) {
        //    mem_table[i] = NULL;
        //}

        // register memory check function to atexit()
        atexit(mem_check);
        isRegistered = true;
    }

    memHeader_t * header = (memHeader_t *)malloc(sizeof(memHeader_t)+size);
    header->size = size;
    header->file = filename;
    header->lineno = lineno;
    header->next = NULL;
    unsigned int slot = (unsigned int)header%SLOT_MAX;
    unsigned int bucket = (unsigned int)header%BUCKET_MAX;
    lock(&mem_table[slot].lock);
    header->next = mem_table[slot].table[bucket];
    mem_table[slot].table[bucket] = header;
    mem_table[slot].nbOfAlloc++;
    unlock(&mem_table[slot].lock);

    header ++;
    return (void*)header;
}

void app_freeFct(void * p, const char* filename, int line_no) {
    if (!p) {
        printf("[ERROR]Try to free a null poiter at %s:%d\n",filename,line_no);
        return ;
    }

    p = (memHeader_t *)p - 1;
    if ((unsigned int)p <=0 ) {
        printf("[ERROR]Try to free a invalid poiter(%p) at %s:%d\n", p,filename,line_no);
        return;
    }

    unsigned int slot = ((unsigned int)p)%SLOT_MAX;
    unsigned int bucket = ((unsigned int)p)%BUCKET_MAX;

    lock(&mem_table[slot].lock);
    memHeader_t * tmp = mem_table[slot].table[bucket];

    if (tmp!=NULL) {

        if (tmp==p) {
            mem_table[slot].table[bucket] = tmp->next;
            free(p);
            // Don't forget to unlock before return
            mem_table[slot].nbOfAlloc--;
            unlock(&mem_table[slot].lock);
            return;
        }

        memHeader_t * prev = tmp;
        tmp = prev->next;
        while (tmp) {
            if (tmp == p) {
                prev->next = tmp->next;
                free(p);
                // Don't forget to unlock before return
                mem_table[slot].nbOfAlloc--;
                unlock(&mem_table[slot].lock);
                return;
            }
            prev = tmp;
            tmp = prev->next;
        }
    }
    unlock(&mem_table[slot].lock);

    printf("[ERROR]Try to free memory(%p) which has been freed at %s:%d\n",p,filename,line_no);
    return;
}



// disable default memory management functions
// NOTE! the following macro must be defined just before your code and 
//  after includes and pre-defined memory management functions
//#ifdef malloc
#define APP_MALLOC_ERR  err_malloc_disabled_use_app_malloc_instead
#define malloc(x) APP_MALLOC_ERR
//#endif

//#ifdef free
#define APP_FREE_ERR  err_free_disabled_use_app_malloc_instead
#define free(x) APP_FREE_ERR
//#endif

void * run(void *) {
    int * p = (int *)app_malloc(sizeof(int));
}

#define NUM_THREAD 5
// test dmalloc library
// using "g++ -o test memory.cpp -ldmalloc -DDMALLOC -DDMALLOC_FUNC_CHECK"
int main() {
    //int *p = (int *)malloc(sizeof(int));

    //char *pc =(char*)malloc(5);

    //strcpy(pc,"fency post!");
    int *p = (int *)app_malloc(sizeof(int));
    //p = (int *)app_malloc(sizeof(int));
    app_free(p);
    app_free(p);
    app_free(0);

    // Here we fork some new threads
    pthread_t theads[NUM_THREAD];
    int rc;
    long t;
    for (t = 0; i < NUM_THREAD; t++) {
        printf("In main thread, creating thread %ld\n", t);
        //rc = pthread_create
    }

    pthread_exit(NULL);
    exit(0);
}
