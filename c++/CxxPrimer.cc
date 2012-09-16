#include "CxxPrimer.h"
#include <iostream>

CxxPrimer::CxxPrimer(std::string name)
{
    bookName = name; 
}

void CxxPrimer::show()
{
    std::string upBar = "****************************************";
    std::string leftBar = "* Book Name: ";
    std::cout << upBar << "\n"
              << leftBar << bookName << "\n"
              << upBar << std::endl;
}

int main(int argc, char ** argv)
{
    CxxPrimer cxxPrimer = CxxPrimer("C++ Primer");

    cxxPrimer.show();
}
