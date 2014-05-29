/* POSIX semaphore wrapper */
#include <semaphore.h>
#include <iostream>

class Semaphore
{
    sem_t sem_;

    public:
    Semaphore();
    ~Semaphore();

    void wait();
    void post();
};
