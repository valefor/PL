#include <iostream>
#include <condition_variable>
#include <thread>
#include <atomic>
#include <chrono>

std::condition_variable cv;
std::mutex cv_m;
std::atomic<int> i = ATOMIC_VAR_INIT(0);

/*
  void wait( std::unique_lock<std::mutex>& lock );
    Atomically releases lock, blocks the current executing thread, and adds it to the list of threads waiting on *this. The thread will be unblocked when notify_all() or notify_one() is executed. It may also be unblocked spuriously. When unblocked, regardless of the reason, lock is reacquired and wait() exits. If this function exits via exception, lock is also reacquired.


    bool wait_for( std::unique_lock<mutex>& lock,
               const std::chrono::duration<Rep, Period>& rel_time,Predicate pred);
Equivalent to:

    while (!pred())
        if (wait_for(lock, rel_time) == std::cv_status::timeout)
            return pred();
    return true; 

    

*/

void waits(int idx)
{
    std::unique_lock<std::mutex> lk(cv_m);
    if (cv.wait_for(lk, std::chrono::milliseconds(idx*100),[](){return i==1;}) )
        std::cerr << "Thread " << idx << " finished waiting. i == " << i << '\n';
    else
        std::cerr << "Thread " << idx << " timeout. i == " << i << '\n';
}

void signals()
{
    std::this_thread::sleep_for(std::chrono::milliseconds(120));
    std::cerr <<  "Notifying...\n";
    cv.notify_all();
    std::this_thread::sleep_for(std::chrono::milliseconds(100));
    i = 1;
    std::cerr <<  "Notifying again...\n";
    cv.notify_all();
}

int main()
{
    std::thread t1(waits, 1), t2(waits, 2), t3(waits, 3), t4(signals);
    t1.join(); t2.join(); t3.join();t4.join();
}
