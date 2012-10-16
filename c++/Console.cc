#include "Console.h"
#include <string>
//#include <boost/regex.hpp>

Console::Console(Operable & opra){
    oprb = &opra;
}  

void Console::start() {

    oprb->show();

    std::string cmd = "";
    std::string arg = "";

    while (true)
    {
        std::cout << "(csl):";
        std::getline(std::cin,cmd);

        if (!dispatch(cmdFilter(cmd),arg)) break;
    }
}

Console::CMD_TYPE Console::cmdFilter(std::string cmd) {
    //boost::regex lsRegex("(ls|show|s)");
    //boost::regex cdRegex("(goto|g|cd)");
    //boost::regex qtRegex("(quit|q|exit)");

    //if(boost::regex_match(cmd,lsRegex)) return CT_LS;
    //if(boost::regex_match(cmd,cdRegex)) return CT_CD;
    //if(boost::regex_match(cmd,qtRegex)) return CT_QT;
    //if("ls" == cmd || "show"==cmd || "s" == cmd) return CT_LS;
    //if("goto" == cmd || "g"==cmd || "cd" == cmd) return CT_CD;
    //if("quit" == cmd || "q"==cmd || "exit" == cmd) return CT_QT;

    return CT_UNDEF;
}

bool Console::dispatch(CMD_TYPE cmdType,std::string arg) {

    //std::cout << "command:" << cmd << std::endl;
    //if ( CT_QT == cmdType) return false;
    switch(cmdType) {
        case CT_QT: 
            return false;
        case CT_CD:
            ;
        case CT_LS:
            ;
        default:
            ;
        
    }

    return true;
}

void Console::helper() {
    std::cout << ":cd,ls,ex" << std::endl;
}
