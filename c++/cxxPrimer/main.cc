#include "CxxPrimer.h"
#include "Console.h"
#include <iostream>


int main(int argc, char ** argv)
{
    Book cxxPrimer = Book("C++ Primer");

    CxxPrimer_Init(cxxPrimer);

    Console csl = Console(cxxPrimer);
    csl.start();
}


