#include "sem.h"

Semaphore::Semaphore()
{
    const int noshared=0;// Do not share with processes
    if ( sem_init(&sem_,noshared,0)== -1 ) {
        std::cerr << "Failed to init semaphore!\n";
    }
}

Semaphore::~Semaphore()
{
    if (sem_destroy(&sem_)== -1) {
        std::cerr << "Failed to destory semaphore!\n";
    }
}

void Semaphore::wait()
{
    sem_wait(&sem_)== -1;
}

void Semaphore::post()
{
    if (sem_post(&sem_)== -1) {
        std::cerr << "Failed to post semaphore!\n";
    }
}
