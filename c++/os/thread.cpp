#include <stdio.h>
#include <iostream>
#include <pthread.h>

void* run(void*) {
    for (int i = 0; i < 3; i++) {
        std::cout << "This is pthread[" << i << "]" << std::endl; 
    }
}

// g++ -o test thread.cpp -lpthread
int main(void) {
    pthread_t id;
    int i, ret;
    ret = pthread_create(&id,NULL,run, NULL);
    if (ret != 0) {
        std::cout << "Create pthread failed!" << std::endl;
        return 1;
    }

    for (int i = 0; i < 3; i++) {
        std::cout << "This is main process[" << i << "]" << std::endl; 
    }
    pthread_join(id,NULL);
    return 0;
}
