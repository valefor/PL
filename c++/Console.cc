#include "Console.h"

Console::Console(Operable & opra){
    oprb = &opra;
}  

void Console::start() {

    std::string cmd = "";
    std::string arg = "";

    while (true)
    {
        std::cout << "(csl):";
        std::cin >> cmd >> arg;

        if (!dispatch(cmd,arg)) break;
    }
}

int Console::dispatch(std::string cmd,std::string arg) {

    if ("quit" == cmd)
    return 0;

    return 1;
}

void Console::helper() {
    std::cout << ":cd,ls,ex" << std::endl;
}
